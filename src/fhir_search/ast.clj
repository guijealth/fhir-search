(ns fhir-search.ast
  (:require
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.string :as string]
   [fhir-search.config :as config])
  (:import
   (java.time LocalDate LocalDateTime OffsetDateTime)))

(defn load-params
  "Load search parameters from the active configuration.
   
   With no args, loads params from the currently active alias.
   With cfg, uses the active alias from that config.
   With cfg and alias, loads params for the specific alias."
  ([]
   (load-params (config/load-config) (:alias (config/active-params))))

  ([cfg]
   (load-params cfg (:alias (config/active-params cfg))))

  ([cfg alias]
   (let [path (:path (config/resolve-params cfg alias))
         file (io/file path)]
     (when (and (.exists file) (pos? (.length file)))
       (edn/read-string (slurp file))))))

(defn- format-basic-data [restype type expression]
  {:type (keyword type)
   :path (->> (string/split expression #"\|")
              (map string/trim)
              (filterv #(re-find (re-pattern (str "^\\(?" restype)) %)))})

(defn extract-data
  "Extract type, path and component (if exists) information for a search parameter."
  [{:keys [restype code url]} params-data]
  (let [matches? (if code
                   #(and (= code (:code %))
                         (some #{restype} (:base %)))

                   #(and (= url (:url %))
                         (some #{restype} (:base %))))]
    (when-some [{:keys [type expression component]} (first (filter matches? params-data))]
      (if (= "composite" type)

        {:component (mapv #(extract-data {:url (:definition %) :restype restype} params-data) component)}

        (format-basic-data restype type expression)))))

(defn parse-token-value
  "Parse a token value into a map with :system and/or :code keys.
    
    Handles formats: 'system|code', '|code', 'system|', or plain values.
    Works with both single values and vectors of values."
  [value]
  (let [parser
        (fn [v]

          (let [[part1 part2] (remove string/blank? (string/split v #"\|"))]

            (cond
              part2
              {:system part1
               :code part2}

              (string/starts-with? v "|")
              {:code part1}

              (string/ends-with? v "|")
              {:system part1}

              :else v)))]

    (if (vector? value)
      (mapv #(update % :value parser) value)
      (parser value))))

(defn parse-date-value [value]
  (let [parser (fn [v]
                 (cond
                   (not (string/includes? v "T"))
                   (LocalDate/parse v)

                   (re-find #"[Z+\-]" (subs v 10))
                   (OffsetDateTime/parse v)

                   :else
                   (LocalDateTime/parse v)))]
    (if (vector? value)
      (mapv #(update % :value parser) value)
      (parser value))))

(defn parse-number-value [value]
  (if (vector? value)
    (mapv #(update % :value parse-double) value)
    (parse-double value)))

(defn parse-quantity-value [value]
  (let [parser (fn [v]
                 (let [[part1 part2 part3] (remove string/blank? (string/split v #"\|"))]

                   (cond
                     part3
                     {:value part1
                      :system part2
                      :code part3}

                     part2
                     (cond

                       (string/starts-with? v "|")
                       {:system part1
                        :code part2}

                       (string/ends-with? v "|")
                       {:value part1
                        :system part2}

                       :else
                       {:value part1
                        :code part2})

                     (and (string/starts-with? v "|")
                          (string/ends-with? v "|"))
                     {:system part1}

                     (string/starts-with? v "|")
                     {:code part1}

                     (string/ends-with? v "|")
                     {:value part1}

                     :else v)))]
    (if (vector? value)
      (mapv #(update % :value parser) value)
      (parser value))))

(def supported-value-types
  {:token parse-token-value
   :string identity
   :date parse-date-value
   :number parse-number-value
   :reference identity
   :quantity parse-quantity-value
   :uri identity})

(defn format-value
  "Format a search parameter value according to its type.
    
   Dispatches to the appropriate formatter based on the parameter type.
   Throws ex-info if the type is not supported."
  [type value]
  (if-let [formatter (get supported-value-types type)]
    (formatter value)
    (throw (ex-info "Unsupported search parameter type."
                    {:current-type type
                     :support-types (vec (keys supported-value-types))}))))

(defn process-composite
  "Process a composite search parameter by merging component metadata.
     
   Takes a param with :composite true and its component definitions.
   Validates that param has the expected number of components,
   then enriches each sub-param with its type and formats its value.
     
   Throws ex-info if component count doesn't match."
  [composite-param components]
  (let [count-p (count (:params composite-param))
        count-c (count components)]
    (when (not= count-p count-c)
      (throw
       (ex-info "Incorrect composite parameter."
                {:search-param (:name composite-param)
                 :description
                 (format
                  "This composite parameter requires %d components. Look at %s for more information."
                  count-c
                  (-> (config/active-params) :url))}))))
  
  (let [updater (fn [param {:keys [type] :as component}]
                  (-> (merge param component)
                      (update :value (partial format-value type))))]
    (-> composite-param
        (dissoc :composite)
        (assoc :type :composite)
        (update :params #(mapv updater % components)))))

(defn enrich
  "Enrich an AST with search parameter metadata.
    
   Adds :type and :path information to each parameter in the AST,
   and formats parameter values according to their type.
    
   With one arg, uses loaded params from active config.
   With two args, uses provided params-data."
  ([ast]
   (enrich (load-params) ast))

  ([params-data ast]
   (let [restype (:type ast)
         updater (fn [m]
                   (if-let [{:keys [type] :as data} (extract-data {:restype restype :code (:name m)} params-data)]

                     (let [formatter (partial format-value type)

                           base (merge m data)]
                       (cond
                         (:composite m)
                         (process-composite m (:component data))

                         (:value m)
                         (update base :value formatter)

                         (:params m)
                         (update base :params formatter)))

                     (throw (ex-info "Unsupported search parameter."
                                     {:search-param (:name m)
                                      :description "The current search parameter is not defined at specifications that currently active."}))))]

     (update ast :params (partial mapv updater)))))


