(ns fhir-search.compiler
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as string]
            [fhir-search.config :as cfg]))

(comment
  (defn load-params [alias]
    (let [path (:path (cfg/resolve-params alias))
          file (io/file path)]
      (when (and (.exists file) (pos? (.length file)))
        (edn/read-string (slurp file)))))


  (defn extract-data [restype param-code params]
    (let [condition #(and (= param-code (:code %)) (contains? (set %) restype))

          {:keys [type expression]} (some #(when (condition %) %) params)

          path (->> (string/split expression #"\|")
                    (mapv string/trim)
                    (filterv #(string/starts-with? % restype)))]
      {:type type
       :path path}))

  (defn parse-token-value [value]
    (let [parser
          (fn [v]
            (let [[part1 part2] (remove string/blank? (string/split v #"\|"))
                  clean (string/replace v #"\|" "")]
              (cond
                part2 {:system part1
                       :code part2}

                (string/starts-with? v "|")
                {:code clean}

                (string/ends-with? v "|")
                {:system clean}

                :else {:value v})))]
      (if (coll? value)
        (mapv #(update % :value parser) value)
        (parser value))))

  (defn format-value [type value]
    (case type
      "token" (parse-token-value value)
      "string" []
      "date" []
      "number" []
      "reference" []
      "quantity" []
      "uri" []
      "composite" []
      (throw (ex-info "Unsupported search parameter type"
                      {:curretn-type type
                       :support-types ["token" "string" "date" "number" "reference" "quantity" "uri" "composite"]}))))

  (defn enrich [ast])
  :.)