(ns fhir-search.compiler
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as string]
            [fhir-search.config :as config]))

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

(defn extract-data
  "Extract type and path information for a search parameter.
    
    Returns a map with :type and :path keys if the parameter is found,
    nil otherwise."
  [restype param-code params-data]
  (let [matches? #(and (= param-code (:code %)) (some #{restype} (:base %)))]
    (when-some [{:keys [type expression]} (first (filter matches? params-data))]
      {:type (keyword type)
       :path (->> (string/split expression #"\|")
                  (map string/trim)
                  (filterv #(re-find (re-pattern (str "^\\(?" restype)) %)))})))

(defn parse-token-value
  "Parse a token value into a map with :system and/or :code keys.
    
    Handles formats: 'system|code', '|code', 'system|', or plain values.
    Works with both single values and vectors of values."
  [value]
  (let [parser
        (fn [v]

          (let [[part1 part2] (remove string/blank? (string/split v #"\|"))

                clean (string/replace v #"\|" "")]

            (cond
              part2
              {:system part1
               :code part2}

              (string/starts-with? v "|")
              {:code clean}

              (string/ends-with? v "|")
              {:system clean}

              :else v)))]

    (if (vector? value)

      (mapv #(update % :value parser) value)

      (parser value))))

(def supported-param-types
  {:token (partial parse-token-value)
   :string identity
   :date identity
   :number identity
   :reference identity
   :quantity identity
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

                     (let [formatter (partial format-value type)]
                       (cond-> (merge m data)

                         (:value m)
                         (update :value formatter)

                         (:params m)
                         (update :params formatter)))
                     (throw (ex-info "Unsupported search parameter"
                                     {:search-param (:name m)
                                      :description "The current search parameter is not defined at specifications that currently active."}))))]

     (update ast :params (partial mapv updater)))))

