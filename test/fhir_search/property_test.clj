(ns fhir-search.property-test
  (:require
   [clojure.test.check.generators :as gen]
   [clojure.test.check.properties :as prop]
   [clojure.test.check.clojure-test :refer [defspec]]
   [fhir-search.uri-query :as fq]
   [fhir-search.complex :refer [clean]]
   [clojure.edn :as edn]))

(def simple-resource-types
  (edn/read-string (slurp "resources/test/fhir_resource_types_samples.edn")))

(def simple-search-params-values
  (edn/read-string (slurp "resources/test/fhir_params_values_samples.edn")))

(def params-modifiers-and-prefixes
  (edn/read-string (slurp "resources/test/fhir_params_modifiers_and_prefixes.edn")))

(def gen-url-ast
  (gen/let [restype (gen/elements (keys simple-resource-types))
            n-params (gen/choose 1 10)
            params (gen/vector-distinct
                    (gen/let [param-name (gen/elements (get simple-resource-types restype))
                              modifier (if-let [mods (seq (get-in params-modifiers-and-prefixes [param-name :modifiers]))]
                                         (gen/elements mods)
                                         (gen/return nil))
                              prefix (if-let [prefixes (seq (get-in params-modifiers-and-prefixes [param-name :prefixes]))]
                                       (gen/elements prefixes)
                                       (gen/return nil))
                              value (gen/elements (get simple-search-params-values param-name))]
                      (clean {:name param-name
                              :modifier (when modifier (keyword "fhir.search.modifier" modifier))
                              :prefix (when prefix (keyword "fhir.search.prefix" prefix))
                              :value value}))
                    {:num-elements n-params
                     :max-tries 100})]
    {:type restype
     :join :fhir.search.join/and
     :params (->> params
                  (group-by :name)
                  (mapv (fn [[name maps]]
                          (if (= 1 (count maps))
                            (first maps)
                            (let [common-mod (:modifier (rand-nth maps))]
                              {:name name
                               :join :fhir.search.join/or
                               :params (mapv #(-> % (dissoc :name)
                                                  (assoc :modifier common-mod)) maps)}))))
                  clean)}))

(def gen-url
  (gen/fmap fq/to-url gen-url-ast))

(def parsed-url-with-params-property
  (prop/for-all [url gen-url]
                (let [parsed (fq/parse url)]
                  (and (map? parsed)
                       (contains? parsed :type)
                       (string? (:type parsed))
                       (contains? parsed :join)
                       (= :fhir.search.join/and (:join parsed))
                       (contains? parsed :params)
                       (vector? (:params parsed))
                       (not-empty (:params parsed))
                       (every? map? (:params parsed))
                       (every? #(:name %) (:params parsed))))))

(def round-trip-property
  (prop/for-all [ast gen-url-ast]
                (= ast (fq/parse (fq/to-url ast)))))

(defspec parsed-url-has-valid-structure-test 1000
  parsed-url-with-params-property)

(defspec roundtrip-test 1000
  round-trip-property)
