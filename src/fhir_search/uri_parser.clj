(ns fhir-search.uri-parser 
  (:require [clojure.string :as str])
  (:require [clojure.walk :refer [postwalk]])
  (:import [java.net URI]))

(defn gen-values [query]
  (->> 
   (str/split query #"&")
   (reduce (fn [result element]
             (let [first-group (first (str/split element #"=")) second-group (second (str/split element #"="))]
               (conj result {:name (first (str/split first-group #":"))
                             :modifiers (when (not (nil? (second (str/split first-group #":")))) 
                                          (keyword (str "fhir.search.modifier/" (second (str/split first-group #":")))))
                             :value second-group}))) [])))

(defn get-path [string]
  (let [path (remove empty? (-> (.getPath (URI. string)) (str/split #"/"))) structure {:type (last path)}]
    (cond-> structure
      (= 3 (count path)) (assoc :compartment {:type (first path) :id (second path)})
      (= 2 (count path)) (assoc :component {:id (first path)}))))

(defn get-params [string]
  (let [query (.getQuery (URI. string)) structure {:params nil}]
    (when (not (nil? query))
      (assoc structure :params {:join :fhir.search.join/and
                                :values (gen-values query)}))))

(defn clean [m]
  (postwalk (fn [v]
              (cond
                (instance? clojure.lang.MapEntry v) (when-not (nil? (val v)) v)
                (map? v) (when-let [entries (seq (remove #(-> % second nil?) v))] (into {} entries))
                (vector? v) (when-let [coll (seq (remove nil? v))] (into [] coll))
                (seq? v) (remove nil? v)
                :else v)) m))

(defn uri-parser [url] (postwalk clean (into (get-path url) (get-params url))))


(comment

  (get-path "/Condition")
  ;;  {:type "Condition"}

  (get-path "/Patient/p123/Condition")
  ;;  {:type "Condition" 
  ;;   :compartment {:type "Patient"
  ;;                 :id "p123"}}

  (get-params "/Patient/p123/Condition?code:in=http%3A%2F%2Fhspc.org%2FValueSet%2Facute-concerns")
  ;;  {:params {:join :fhir.search.join/and
  ;;             :values [{:name "code"
  ;;                       :modifiers :fhir.search.modifier/in
  ;;                       :value "http://hspc.org/ValueSet/acute-concerns"}]}}

  
  
  (uri-parser "/Patient/p123/Condition")
  ;;  {:type "Condition", :compartment {:type "Patient", :id "p123"}}
  
  (uri-parser "/Patient/p123/Condition?code:in=http%3A%2F%2Fhspc.org%2FValueSet%2Facute-concerns")
  ;;  {:type "Condition",
  ;;    :compartment {:type "Patient"
  ;;                  :id "p123"}
  ;;    :params {:join :fhir.search.join/and 
  ;;             :values [{:name "code"
  ;;                    :modifiers :fhir.search.modifier/in
  ;;                    :value "http://hspc.org/ValueSet/acute-concerns"}]}}
  )

