(ns fhir-search.compiler
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as string]
            [fhir-search.config :as config]))

(defn load-params
  ([]
   (load-params (config/load-config) (:alias (config/active-params))))

  ([cfg]
   (load-params cfg (:alias (config/active-params cfg))))

  ([cfg alias]
   (let [path (:path (config/resolve-params cfg alias))
         file (io/file path)]
     (when (and (.exists file) (pos? (.length file)))
       (edn/read-string (slurp file))))))

(defn extract-data [restype param-code params-data]
  (let [matches? #(and (= param-code (:code %)) (some #{restype} (:base %)))]
    (when-some [{:keys [type expression]} (first (filter matches? params-data))]
      {:type (keyword type)
       :path (->> (string/split expression #"\|")
                  (map string/trim)
                  (filterv #(re-find (re-pattern (str "^\\(?" restype)) %)))})))

(defn parse-token-value [value]
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

(def support-param-types
  {:token (partial parse-token-value)
   :string identity
   :date identity
   :number identity
   :reference identity
   :quantity identity
   :uri identity
   :composite identity})

(defn format-value [type value]
  (if-let [formatter (get support-param-types type)]
    (formatter value)
    (throw (ex-info "Unsupported search parameter type"
                    {:curretn-type type
                     :support-types (vec (keys support-param-types))}))))


(defn enrich
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
                     m))]

     (update ast :params (partial mapv updater)))))

