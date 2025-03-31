(ns fhir-search.uri-query
  (:require [clojure.string :as str]
            [clojure.walk :refer [postwalk]]
            [fhir-search.complex :refer [clean]])
  (:import [java.net URI]))

(defn prefix-finder
  [element]
  (when-let [[_ prefix value] (first (re-seq #"(eq|ne|gt|lt|ge|le|sa|eb|ap)?(\d.*)" element))]
    [prefix value]))

(defn param-name [string]
  (->
   (first (str/split string #"="))
   (str/split #":")))

(defn param-value [string]
  (->
   (second (str/split string #"="))
   (str/split #",")))

(defn values [query]
  (->> (str/split query #"&")
       (reduce (fn [result element]
                 (let [param-name (param-name element)
                       param-values (param-value element)]
                   (prn (count param-values))
                   (if (> (count param-values) 1)
                     (conj result 
                           (->> (reduce (fn [values item] 
                                          (let [pf-group (prefix-finder item)]
                                            (conj values {:modifier (when-let [modif (second param-name)] 
                                                       (keyword "fhir.search.modifier" modif))
                                           :prefix (when-let [prefix (first pf-group)]
                                                     (keyword "fhir.search.prefix" prefix))
                                           :value (if (first pf-group)
                                                    (second pf-group)
                                                    item)})))
                                        [] param-values)
                                (assoc {:join :fhir.search.join/or
                                        :name (first param-name)} :values))) 
                     (conj result (let [pf-group (prefix-finder (first param-values))]
                                    {:name (first param-name)
                                     :modifiers (when-let [modif (second param-name)]
                                                  (keyword "fhir.search.modifier" modif))
                                     :prefix (when-let [prefix (first pf-group)]
                                               (keyword "fhir.search.prefix" prefix))
                                     :value (if (first pf-group)
                                              (second pf-group)
                                              (first param-values))}))))) [])))

(defn path [string]
  (let [path (remove empty? (str/split string #"/"))
        structure {:type (last path)}]
    (cond-> structure
      (= 3 (count path)) (assoc :compartment {:type (first path) :id (second path)})
      (= 2 (count path)) (assoc :component {:id (first path)}))))

(defn params [query]
  (when query
    (hash-map :params {:join :fhir.search.join/and
                       :values (values query)})))

(defn uri-parse [url]
  (let [uri (URI. url)]
    (postwalk clean (into (path (.getPath uri)) (params (.getQuery uri))))))


(comment
  (uri-parse "/Patient/p123/Condition")
  ;;  {:type "Condition", :compartment {:type "Patient", :id "p123"}}

  (uri-parse "/Patient/p123/Condition?code:in=http%3A%2F%2Fhspc.org%2FValueSet%2Facute-concerns")
  ;;  {:type "Condition",
  ;;    :compartment {:type "Patient"
  ;;                  :id "p123"}
  ;;    :params {:join :fhir.search.join/and 
  ;;             :values [{:name "code"
  ;;                    :modifiers :fhir.search.modifier/in
  ;;                    :value "http://hspc.org/ValueSet/acute-concerns"}]}}

  (uri-parse "/Patient?given:exact=GivenA,GivenB")
  ;; {:type "Patient",
  ;;  :params
  ;;  {:join :fhir.search.join/and
  ;;   :values
  ;;   [{:join :fhir.search.join/or
  ;;     :name "given"
  ;;     :values
  ;;     [{:modifiers :fhir.search.modifier/exact
  ;;       :value "GivenA"}
  ;;      {:modifiers :fhir.search.modifier/exact
  ;;       :value "GivenB"}]}]}}

  (uri-parse "/Observation?code:in=http%3A%2F%2Floinc.org%7C8867-4&value-quantity=lt60%2Cgt100")
  ;; {:type "Observation"
  ;;  :params
  ;;  {:join :fhir.search.join/and
  ;;   :values
  ;;   [{:name "code"
  ;;     :modifiers :fhir.search.modifier/in
  ;;     :value "http://loinc.org|8867-4"}
  ;;    {:join :fhir.search.join/or
  ;;     :name "value-quantity"
  ;;     :values [{:prefix :fhir.search.prefix/lt
  ;;               :value "60"} 
  ;;             {:prefix :fhir.search.prefix/gt
  ;;              :value "100"}]}]}} 
  )

