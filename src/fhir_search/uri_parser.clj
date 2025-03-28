(ns fhir-search.uri-parser
  (:require [clojure.string :as str])
  (:require [clojure.walk :refer [postwalk]])
  (:import [java.net URI]))



(defn url-encoding [url]
  (let [uri (URI. url)
        scheme (.getScheme uri)
        authority (.getAuthority uri)
        path (.getPath uri)
        query (.getQuery uri)
        fragment (.getFragment uri)]
    (when uri
      (str (when scheme (str scheme "://"))
           authority path
           (when query (str "?" query))
           (when fragment (str "#" fragment))))))

(defn values [query]
  (->>
   (str/split query #"&")
   (reduce (fn [result element]
             (let [group (str/split element #"=")
                   first-group (first group)
                   second-group (second group)]
               (if (re-find #"," second-group)
                 (conj result (->> 
                               (reduce (fn [value item]
                                         (conj value {:modifiers (when (second (str/split first-group #":"))
                                                                   (keyword "fhir.search.modifier" (second (str/split first-group #":"))))
                                                      :value item}))
                                      [] (str/split second-group #","))
                               (assoc {:join :fhir.search.join/or
                                      :name (first (str/split first-group #":"))} :values)))
                 
                 (conj result {:name (first (str/split first-group #":"))
                               :modifiers (when (second (str/split first-group #":"))
                                            (keyword "fhir.search.modifier" (second (str/split first-group #":"))))
                               :value second-group})))) [])))

(defn path [string]
  (let [path (remove empty? (-> (.getPath (URI. string)) (str/split #"/")))
        structure {:type (last path)}]
    (cond-> structure
      (= 3 (count path)) (assoc :compartment {:type (first path) :id (second path)})
      (= 2 (count path)) (assoc :component {:id (first path)}))))

(defn params [string]
  (let [query (.getQuery (URI. string))]
    (when query
      (hash-map :params {:join :fhir.search.join/and
                         :values (values query)}))))

(defn clean [m]
  (postwalk (fn [v]
              (cond
                (instance? clojure.lang.MapEntry v) (when-not (nil? (val v)) v)
                (map? v) (when-let [entries (seq (remove #(-> % second nil?) v))] (into {} entries))
                (vector? v) (when-let [coll (seq (remove nil? v))] (into [] coll))
                (seq? v) (remove nil? v)
                :else v)) m))

(defn uri-parser [url] (postwalk clean (into (path url) (params url))))



(comment
  (url-encoding "/Observation?code=http%3A%2F%2Floinc.org%7C8867-4&value-quantity=lt60%2Cgt100")
  ;; "/Observation?code=http://loinc.org|8867-4&value-quantity=lt60,gt100"

  (path "/Condition")
  ;;  {:type "Condition"}

  (path "/Patient/p123/Condition")
  ;;  {:type "Condition" 
  ;;   :compartment {:type "Patient"
  ;;                 :id "p123"}}

  (params "/Patient/p123/Condition?code:in=http%3A%2F%2Fhspc.org%2FValueSet%2Facute-concerns")
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

  (uri-parser "/Patient?given:exact=GivenA,GivenB")
  ;; {:type "Patient",
  ;;  :params
  ;;  {:join :fhir.search.join/and,
  ;;   :values
  ;;   [{:join :fhir.search.join/or,
  ;;     :name "given",
  ;;     :values
  ;;     [{:modifiers :fhir.search.modifier/exact, :value "GivenA"}
  ;;      {:modifiers :fhir.search.modifier/exact, :value "GivenB"}]}]}}

  (uri-parser "/Observation?code:in=http%3A%2F%2Floinc.org%7C8867-4&value-quantity=lt60%2Cgt100")


  )

