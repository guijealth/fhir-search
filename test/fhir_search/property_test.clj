(ns fhir-search.property-test
  (:require [clojure.test :refer :all]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.clojure-test :refer [defspec]]
            [fhir-search.uri-query :as fq]
            [clojure.edn :as edn])
  (:import [java.net URLEncoder]))

(def simple-resource-types 
  (edn/read-string (slurp "resources/fhir_resource_types_samples.edn")))

(def simple-search-params-values
  (edn/read-string (slurp "resources/fhir_params_values_samples.edn")))

(def gen-smart-param
  (gen/fmap
   (fn [[param values]]
     (str param "=" (URLEncoder/encode (rand-nth values) "UTF-8")))
   (gen/elements (seq simple-search-params-values))))

(def gen-simple-url
  (gen/fmap (fn [resource-type]
              (str "/" resource-type))
            (gen/elements simple-resource-types)))

(def gen-full-url-with-param
  (gen/fmap (fn [[path param]]
              (str path "?" param))
            (gen/tuple gen-simple-url gen-smart-param)))

(def parsed-url-property
  (prop/for-all [url gen-simple-url]
                (let [parsed (fq/parse url)]
                  (and (map? parsed)
                       (contains? parsed :type)
                       (string? (:type parsed))))))

(def parsed-url-with-params-property
  (prop/for-all [url gen-full-url-with-param]
                (let [parsed (fq/parse url)]
                  (and (map? parsed)
                       (contains? parsed :type)
                       (string? (:type parsed))
                       (contains? parsed :join)
                       (= :fhir.search.join/and (:join parsed))
                       (contains? parsed :params)
                       (vector? (:params parsed))
                       (not-empty (:params parsed))))))


(comment
(tc/quick-check 10 parsed-url-with-params-property) 
  (gen/sample gen-full-url-with-param)
  
  :.)