(ns fhir-search.property-test
  (:require [clojure.test :refer :all]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.clojure-test :refer [defspec]]
            [fhir-search.uri-query :as fq]
            [fhir-search.complex :refer [clean]]
            [clojure.edn :as edn]
            [clojure.string :as str])
  (:import [java.net URLEncoder]))

(def simple-resource-types
  (edn/read-string (slurp "resources/fhir_resource_types_samples.edn")))

(def simple-search-params-values
  (edn/read-string (slurp "resources/fhir_params_values_samples.edn")))

(def params-modifiers-and-prefixes
  (edn/read-string (slurp "resources/fhir_params_modifiers_and_prefixes.edn")))

(def gen-single-param-url-ast
  (gen/let [restype (gen/elements (keys simple-resource-types))
            param (gen/elements (get simple-resource-types restype))
            value (gen/elements (get simple-search-params-values param))]
    {:type restype
     :join :fhir.search.join/and
     :params [{:name param :value value}]}))

(def gen-single-param-url-with-mod-and-prefix-ast
  (gen/let [restype (gen/elements (keys simple-resource-types))
            param (gen/elements (get simple-resource-types restype))
            modifier (if-let [mods (seq (get-in params-modifiers-and-prefixes [param :modifiers]))]
                       (gen/elements mods)
                       (gen/return nil))
            prefix (if-let [prefixes (seq (get-in params-modifiers-and-prefixes [param :prefixes]))]
                     (gen/elements prefixes)
                     (gen/return nil))
            value (gen/elements (get simple-search-params-values param))]
    (clean {:type restype
            :join :fhir.search.join/and
            :params [{:name param
                      :modifier (when modifier (keyword "fhir.search.modifier" modifier))
                      :prefix (when prefix (keyword "fhir.search.prefix" prefix))
                      :value value}]})))



;; Only supports ASTs up to 3.1 spec
(defn stringify-param [{:keys [name modifier prefix join value params]}]
  (let [param-fn #(str %1
                       (when %2 (str ":" (clojure.core/name %2)))
                       "%3D");; %3D is "=" encoded
        value-fn (fn [n p v] 
                    (str 
                     (when n (str n "%24"));;%24 is "$" encoded
                     (when p (clojure.core/name p))
                       (URLEncoder/encode v "UTF-8")))]
    (cond
      (some? value)
      ;;
      (str (param-fn name modifier)
           (value-fn nil prefix value))
      ;;
      (= :fhir.search.join/or join)
      ;;
      (->> (reduce (fn [o {:keys [name prefix value]}]
                     (conj o (value-fn name prefix value)))
                   [] params)
           (str/join "%2C")
           (str (param-fn name (-> params first :modifier)))))))


(defn ast-to-url [{:keys [type params]}]
  (->> (map stringify-param params)
       (reduce (fn [o param]
                 (str o "%26" param)));; %26 is "&" encoded
       (str "/" type "?")))

(def gen-url
  (gen/fmap ast-to-url (gen/one-of [gen-single-param-url-with-mod-and-prefix-ast
                                    gen-single-param-url-ast])))

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
                       (not-empty (:params parsed))))))

(def round-trip
  (prop/for-all [ast gen-single-param-url-ast]
                (= ast (fq/parse (ast-to-url ast)))))

(comment
  (tc/quick-check 100000 parsed-url-with-params-property)
  (tc/quick-check 100000 round-trip) 
  
  (ast-to-url {:type "Observation"
   :join :fhir.search.join/and
   :params [{:name "code-value-quantity"
             :join :fhir.search.join/or
             :composite true
             :params [{:name "code"
                       :value "loinc|12907-2"}
                      {:name "value"
                       :prefix :fhir.search.prefix/ge
                       :value "150|http://unitsofmeasure.org|mmol/L"}]}
            {:name "based-on"
             :value "ServiceRequest/f8d0ee15-43dc-4090-a2d5-379d247672eb"}]})
  

   :.)