(ns fhir-search.uri-parser
  (:require [clojure.string :as str])
  (:require [clojure.walk :refer [postwalk]])
  (:import [java.net URI]))

(def prefixes ["eq" "ne" "gt" "lt" "ge" "le" "sa" "eb" "ap"])

(defn prefix-find "Find the prefix of a number value. If there are more than two prefix or the value is not the correct returns nil."
  [pf-list element]
  (when-let [prefix (first (filter (and #(re-find (re-pattern %) element)
                                        #(not (re-find #"\p{L}" (str/replace element (re-pattern %) "")))) pf-list))]
    prefix))


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
                                                      :prefix (when (prefix-find prefixes item) (keyword "fhir.search.prefix" (prefix-find prefixes item)))
                                                      :value (if (prefix-find prefixes item)
                                                               (str/replace item (re-pattern (prefix-find prefixes item)) "")
                                                               item)}))
                                       [] (str/split second-group #","))
                               (assoc {:join :fhir.search.join/or
                                       :name (first (str/split first-group #":"))} :values)))

                 (conj result {:name (first (str/split first-group #":"))
                               :modifiers (when (second (str/split first-group #":"))
                                            (keyword "fhir.search.modifier" (second (str/split first-group #":"))))
                               :prefix (when (prefix-find prefixes second-group) (keyword "fhir.search.prefix" (prefix-find prefixes second-group)))
                               :value (if (prefix-find prefixes second-group)
                                        (str/replace second-group (re-pattern (prefix-find prefixes second-group)) "")
                                        second-group)})))) [])))

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
  ;;  {:join :fhir.search.join/and
  ;;   :values
  ;;   [{:join :fhir.search.join/or
  ;;     :name "given"
  ;;     :values
  ;;     [{:modifiers :fhir.search.modifier/exact
  ;;       :value "GivenA"}
  ;;      {:modifiers :fhir.search.modifier/exact
  ;;       :value "GivenB"}]}]}}

  (uri-parser "/Observation?code:in=http%3A%2F%2Floinc.org%7C8867-4&value-quantity=lt60%2Cgt100")
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

