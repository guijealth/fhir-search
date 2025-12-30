(ns fhir-search.compiler
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as string]
            [fhir-search.config :as config])
  (:import [java.time LocalDate LocalDateTime OffsetDateTime]))

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

(declare extract-data)

(defn- format-basic-data [restype type expression]
  {:type (keyword type)
   :path (->> (string/split expression #"\|")
              (map string/trim)
              (filterv #(re-find (re-pattern (str "^\\(?" restype)) %)))})

(defn- build-result [restype {:keys [type expression component]} params-data]
  (let [data (format-basic-data restype type expression)]
    (if (= "composite" type)
      (assoc data :component
             (mapv #(extract-data (:definition %) params-data) component))
      data)))

(defn extract-data
  "Extract type, path and component (if exists) information for a search parameter."
  ([url params-data]
   (when-some [match (first (filter #(= url (:url %)) params-data))]
     (build-result (first (:base match)) match params-data)))

  ([restype param-code params-data]
   (let [matches? #(and (= param-code (:code %))
                        (some #{restype} (:base %)))]
     (when-some [match (first (filter matches? params-data))]
       (build-result restype match params-data)))))

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

(defn process-composite [param]
  param)

(def supported-param-types
  {:token parse-token-value
   :string identity
   :date parse-date-value
   :number parse-number-value
   :reference identity
   :quantity parse-quantity-value
   :uri identity
   :composite identity})

(defn format-value
  "Format a search parameter value according to its type.
    
    Dispatches to the appropriate formatter based on the parameter type.
    Throws ex-info if the type is not supported."
  [type value]
  (if-let [formatter (get supported-param-types type)]
    (formatter value)
    (throw (ex-info "Unsupported search parameter type"
                    {:current-type type
                     :support-types (vec (keys supported-param-types))}))))


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
                   (if-let [{:keys [type] :as data} (extract-data restype (:name m) params-data)]

                     (let [formatter (partial format-value type)
                           base (merge m data)]
                       (cond 
                         (= :composite type)
                         (process-composite base)
                         
                         (:value m)
                         (update base :value formatter)

                         (:params m)
                         (update base :params formatter)))
                     (throw (ex-info "Unsupported search parameter"
                                     {:search-param (:name m)
                                      :description "The current search parameter is not defined at specifications that currently active."}))))]

     (update ast :params (partial mapv updater)))))

(comment

  (extract-data "Observation" "code-value-quantity" (load-params))

  (def ex {:type "DiagnosticReport",
   :join :fhir.search.join/and,
   :params
   [{:name "result",
     :join :fhir.search.join/and,
     :chained true,
     :params
     [{:name "code-value-quantity",
       :composite true,
       :params
       [{:name "code", :value "http://loinc.org|2823-3"}
        {:name "value", :value "5.4|http://unitsofmeasure.org|mmol/L", :prefix :fhir.search.prefix/gt}]}]}]})

  (enrich ex)
(extract-data "DiagnosticReport" "code-value-quantity" (load-params))
  (parse "/DiagnosticReport?result.code-value-quantity=code%24http%3A%2F%2Floinc.org%7C2823-3,value%24gt5.4%7Chttp%3A%2F%2Funitsofmeasure.org%7Cmmol%2FL")
  :.)
