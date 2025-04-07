(ns fhir-search.uri-query
  (:require [clojure.string :as str]
            [clojure.walk :refer [postwalk]]
            [fhir-search.complex :refer [clean]])
  (:import [java.net URI]))

(defn prefix-finder
  [element]
  (when-let [[_ prefix value] (first (re-seq #"(eq|ne|gt|lt|ge|le|sa|eb|ap)(\d.*)" element))]
    [prefix value]))

(defn param-name [string]
  (-> (first (str/split string #"="))
      (str/split #":")))

(defn param-value [string]
  (->
   (second (str/split string #"="))
   (str/split #",")))

;;Composite Parameters section
(defn composite-param? [string]
  (when (re-find #"\$" string) true))

(defn comp-group [string]
  (str/split string #"\$"))

;;Chained parameters section
(defn chained-params? [string]
  (when (re-find #"\." string) true))

;; Hay que dividir la query por donde están los ".", 
;; luego se pasaría eso a param-name fn para asignar nombre y tipo.
;; Cuando se halla parseado la cadena, hay que tener en cuenta que 
;; si existe algún modificador siempre será el último elemento
;; del vector.


(defn params-values [query]
  (->> (str/split query #"&")
       (reduce (fn [result element]
                 (let [param-name (param-name element)
                       param-values (param-value element)]
                   (if (> (count param-values) 1)
                     (conj result
                           (->> (reduce (fn [values item]
                                          (let [pf-group (prefix-finder item) comp-group (comp-group item)]
                                            (conj values {:name (when (composite-param? item) (first comp-group))
                                                          ;;
                                                          :modifier (when-let [modif (second param-name)]
                                                                      (keyword "fhir.search.modifier" modif))
                                                          ;;
                                                          :prefix (when-let [prefix (first pf-group)]
                                                                    (keyword "fhir.search.prefix" prefix))
                                                          ;;
                                                          :value (cond
                                                                   (first pf-group) (second pf-group)
                                                                   (composite-param? item) (second comp-group)
                                                                   :else item)})))
                                        [] param-values)
                                (assoc {:join :fhir.search.join/or
                                        :name (first param-name)
                                        :composite (when (some composite-param? param-values) true)} :params)))
                     ;;
                     (conj result (let [pf-group (prefix-finder (first param-values)) comp-group (comp-group (first param-values))]
                                    {:name (first param-name)
                                     ;;
                                     :modifiers (when-let [modif (second param-name)]
                                                  (keyword "fhir.search.modifier" modif))
                                     ;;
                                     :composite (when (composite-param? (first param-values)) true)
                                     ;;
                                     :prefix (when-let [prefix (first pf-group)]
                                               (keyword "fhir.search.prefix" prefix))
                                     ;;
                                     ;;
                                     :value (cond
                                              (first pf-group) (second pf-group)
                                              (composite-param? (first param-values)) [{:name (first comp-group)
                                                                                        :value (second comp-group)}]
                                              :else (first param-values))}))))) [])))

(defn path [string]
  (let [path (remove empty? (str/split string #"/"))
        structure {:type (last path)}]
    (cond-> structure
      (= 3 (count path)) (assoc :compartment {:type (first path) :id (second path)})
      (= 2 (count path)) (assoc :component {:id (first path)}))))

(defn params [query]
  (when query
    (hash-map :params [{:join :fhir.search.join/and
                        :params (params-values query)}])))

(defn uri-parse [url]
  (let [uri (URI. url)]
    (postwalk clean (into (path (.getPath uri)) (params (.getQuery uri))))))

(comment

  (uri-parse "/Patient/p123/Condition")
  ;;  {:type "Condition", :compartment {:type "Patient", :id "p123"}}

  (uri-parse "/Patient/p123/Condition?code:in=http%3A%2F%2Fhspc.org%2FValueSet%2Facute-concerns")
  ;; {:type "Condition"
  ;;  :compartment {:type "Patient", :id "p123"}
  ;;  :params [{:join :fhir.search.join/and
  ;;            :params [{:name "code"
  ;;            :modifiers :fhir.search.modifier/in
  ;;            :value "http://hspc.org/ValueSet/acute-concerns"}]}]}
  (uri-parse "/Patient?given:exact=GivenA,GivenB")
  ;; {:type "Patient"
  ;;  :params [{:join :fhir.search.join/and
  ;;            :params [{:join :fhir.search.join/or
  ;;                     :name "given"
  ;;                     :params [{:modifier :fhir.search.modifier/exact
  ;;                               :value "GivenA"}
  ;;                              {:modifier :fhir.search.modifier/exact
  ;;                               :value "GivenB"}]}]}]}

  (uri-parse "/Observation?code:in=http%3A%2F%2Floinc.org%7C8867-4&value-quantity=lt60%2Cgt100")
  ;; {:type "Observation"
  ;;  :params [{:join :fhir.search.join/and
  ;;            :params [{:name "code"
  ;;                      :modifiers :fhir.search.modifier/in
  ;;                      :value "http://loinc.org|8867-4"}
  ;;                     {:join :fhir.search.join/or
  ;;                      :name "value-quantity"
  ;;                      :params [{:prefix :fhir.search.prefix/lt
  ;;                                :value "60"}
  ;;                               {:prefix :fhir.search.prefix/gt
  ;;                                :value "100"}]}]}]}
  (uri-parse "/DiagnosticReport?result=http://loinc.org%7C2823-3$gt5.4%7Chttp://unitsofmeasure.org%7Cmmol/L")
  ;; {:type "DiagnosticReport"
  ;;  :params [{:join :fhir.search.join/and
  ;;            :params [{:name "result"
  ;;                      :composite true
  ;;                      :prefix :fhir.search.prefix/gt
  ;;                      :value "5.4|http://unitsofmeasure.org|mmol/L"}]}]}
  (uri-parse "/Observation?code-value-quantity=code$loinc%7C12907-2,value$ge150%7Chttp://unitsofmeasure.org%7Cmmol/L&based-on=ServiceRequest/f8d0ee15-43dc-4090-a2d5-379d247672eb")
  ;; {:type "Observation"
  ;;  :params [{:join :fhir.search.join/and
  ;;            :params [{:join :fhir.search.join/or
  ;;                      :name "code-value-quantity"
  ;;                      :composite true
  ;;                      :params [{:name "code"
  ;;                                :value "loinc|12907-2"}
  ;;                               {:name "value"
  ;;                                :prefix :fhir.search.prefix/ge
  ;;                                :value "150|http://unitsofmeasure.org|mmol/L"}]}
  ;;                     {:name "based-on"
  ;;                      :value "ServiceRequest/f8d0ee15-43dc-4090-a2d5-379d247672eb"}]}]}
  )

