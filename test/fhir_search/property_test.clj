(ns fhir-search.property-test
  (:require [clojure.test :refer :all]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.clojure-test :refer [defspec]]
            [fhir-search.uri-query :as fq]
            [clojure.edn :as edn]
            [clojure.string :as str])
  (:import [java.net URLEncoder URLDecoder]))

(def simple-resource-types
  (edn/read-string (slurp "resources/fhir_resource_types_samples.edn")))

(def simple-search-params-values
  (edn/read-string (slurp "resources/fhir_params_values_samples.edn")))

(def gen-single-param-url-ast
  (gen/let [restype (gen/elements (keys simple-resource-types))
            param (gen/elements (get simple-resource-types restype))
            value (gen/elements (get simple-search-params-values param))]
    {:type restype
     :join :fhir.search.join/and
     :params [{:name param :value value}]}))

;; Only supports ASTs up to 2.3 spec
(defn stringify-param [{:keys [name modifier prefix join value params]}]
  (let [param-fn #(str %1
                       (when %2 (str ":" (clojure.core/name %2)))
                       "=")
        value-fn #(str (when %1 (clojure.core/name %1))
                       (URLEncoder/encode %2 "UTF-8"))]
    (cond
      (some? value)
      ;;
      (str (param-fn name modifier)
           (value-fn prefix value))
      ;;
      (= :fhir.search.join/or join)
      ;;
      (->> (reduce (fn [o {:keys [prefix value]}]
                     (conj o (value-fn prefix value)))
                   [] params)
           (str/join ",")
           (str (param-fn name (-> params first :modifier)))))))


(defn ast-to-url [{:keys [type params]}]
  (->> (map stringify-param params)
       (reduce (fn [o param]
                 (str o "&" param)))
       (str "/" type "?")))

(def gen-url
  (gen/fmap ast-to-url gen-single-param-url-ast))

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
  (tc/quick-check 1000 parsed-url-with-params-property)
  (tc/quick-check 100000 round-trip) 

  :.)