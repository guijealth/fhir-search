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
      {:type type
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

              :else {:value v})))]

    (if (vector? value)

      (mapv #(update % :value parser) value)

      (parser value))))

(defn format-value [type value]
  (case type
    "token" (parse-token-value value)
    "string" value
    "date" value
    "number" value
    "reference" value
    "quantity" value
    "uri" value
    "composite" value
    (throw (ex-info "Unsupported search parameter type"
                    {:curretn-type type
                     :support-types ["token" "string" "date" "number" "reference" "quantity" "uri" "composite"]}))))


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

