(ns fhir-search.uri-query
  (:require [clojure.string :as str]
            [fhir-search.complex :refer [clean update-specific-val seq-nest search-key-val]])
  (:import [java.net URI]))

(defn main-layer
  "Return the main layer of the map tree"
  [uri-path]
  (let [path (remove empty? (str/split uri-path #"/")) structure {}]
    (cond-> (assoc structure :type (last path) :join :fhir.search.join/and)
      (= 3 (count path)) (assoc :compartment {:type (first path)
                                              :id (second path)})
      (= 2 (count path)) (assoc :component {:id (first path)}))))

;; Parameter Types detectors
(defn chained-params?
  "Recieve a parameter or query string and detects if it's a chained parameter."
  [string]
  (cond
    (re-find #"=" string)
    (when-let [g (first (str/split string #"="))]
      (when (re-find #"\." g) true))
    :else (when (re-find #"\." string) true)))

(defn composite-params?
  [x]
  (if-not (or (map? x) (coll? x))
    (cond
      (re-find #"=" x)
      (when-let [g (second (str/split x #"="))]
        (when (re-find #"\$" g) true))
      :else (when (re-find #"\$" x) true))
    (when (seq (search-key-val x :name)) true)))

;;Parse functions
(defn prefix-finder
  [element]
  (when-let [[_ prefix value] (first (re-seq #"^(eq|ne|gt|lt|ge|le|sa|eb|ap)(\d.*)" element))]
    [prefix value]))

(defn comp-group [string]
  (str/split string #"\$"))

;;Prefixes and modifiers function
(defn prefix?
  "Returns true if there is a valid fhir prefix into arg"
  [x] (when (first (prefix-finder x)) true))

(defn prefix
  "Returns the current fhir prefix"
  [p] (keyword "fhir.search.prefix" p))

(defn modifier
  "Returns the current fhir modifier"
  [m] (keyword "fhir.search.modifier" m))

;; Layers functions
(defn param-layer [string]
  (cond
    (chained-params? string)
    (let [group (str/split string #"\.")
          coll (reduce (fn [out in]
                         (let [name-type (str/split in #":")]
                           (conj out [{:name (if-not (= (last group) in)
                                               (when-let [name (first name-type)] name)
                                               (first name-type))
                                       :type (when-not (= (last group) in)
                                               (when-let [type (second name-type)] type))
                                       :join (when-not (= (last group) in) :fhir.search.join/and)
                                       :modifier (when (and (= (last group) in) (re-find #":" in))
                                                   (when-let [mod (last name-type)]
                                                     (modifier mod)))
                                       :params nil}]))) [] group)
          mod-coll (reduce (fn [o i]
                             (conj o (cond
                                       (= i (last coll)) [(assoc (first i) :value nil :composite nil)]
                                       (= i (first coll)) [(assoc (first i) :chain true)]
                                       :else i)))
                           [] coll)]
      (seq-nest (first (first mod-coll)) :params nil (rest mod-coll)))
      ;;
    :else (let [group (str/split string #":")]
            {:name (when-let [name (first group)]
                     name)
             :join nil
             :modifier (when-let [mod (second group)]
                         (modifier mod))
             :composite nil
             :value nil
             :params nil})))

(defn value-layer [string]
  (let [group (str/split string #",")]
    (reduce (fn [out in]
              (let [comp-g (comp-group in)
                    n (first comp-g)
                    v (second comp-g)
                    pfx-group (prefix-finder in)
                    pfx (first pfx-group)
                    value (second pfx-group)]
                (conj out {:name (when (composite-params? in) n)
                           :value (cond
                                    (composite-params? in)
                                    (if (prefix? v) (second (prefix-finder v)) v)
                                    (prefix? in) value
                                    :else in)
                           :prefix (cond
                                     (and (composite-params? in) (prefix? v))
                                     (prefix (first (prefix-finder v)))
                                     (prefix? in)
                                     (prefix pfx))}))) [] group)))

(defn value-ensambler [param-layer value-layer]
  (let [modifier (first (search-key-val param-layer :modifier))]
    (-> (if (= 1 (count value-layer))
          (let [vl (first value-layer)]
            (if (or (:prefix vl) (:name vl))
              (-> (if (nil? (second modifier))
                    param-layer
                    (update-specific-val param-layer :modifier (second modifier) nil))
                  (update-specific-val :params nil [(conj vl modifier)]))
        ;;
              (update-specific-val param-layer :value nil (:value vl))))
      ;;
          (-> (if (nil? (second modifier))
                param-layer
                (update-specific-val param-layer :modifier (second modifier) nil))
              (update-specific-val :params nil (reduce (fn [o i]
                                                         (conj o (conj i modifier)))
                                                       [] value-layer))
              (update-specific-val :join nil :fhir.search.join/or)))
        (update-specific-val :composite nil (composite-params? value-layer)))))

(defn module-creator [param]
  (let [group (str/split param #"=")
        pl (first group)
        vl (second group)]
    (value-ensambler (param-layer pl) (value-layer vl))))

(defn uri-manager [path query]
  (if (and path query)
    (let [params (str/split query #"&")
          values (reduce (fn [o i]
                           (conj o (module-creator i)))
                         [] params)]
      (assoc (main-layer path) :params (when (seq values) values)))
    (dissoc (main-layer path) :join)))

(defn uri-parse [url]
  (let [uri (URI. url)
        path (.getPath uri)
        query (.getQuery uri)]
    (clean (uri-manager path query))))

(comment
  (uri-parse "/Patient/p123/Condition?code:in=http%3A%2F%2Fhspc.org%2FValueSet%2Facute-concerns")
  ;; {:type "Condition"
  ;;  :join :fhir.search.join/and
  ;;  :compartment {:type "Patient" :id "p123"}
  ;;  :params [{:name "code"
  ;;            :modifier :fhir.search.modifier/in
  ;;            :value "http://hspc.org/ValueSet/acute-concerns"}]}
  (uri-parse "/Patient?given:exact=GivenA,GivenB")
  ;; {:type "Patient",
  ;;  :join :fhir.search.join/and,
  ;;  :params [{:name "given",
  ;;            :join :fhir.search.join/or,
  ;;            :params [{:value "GivenA"
  ;;                      :modifier :fhir.search.modifier/exact}
  ;;          {:value "GivenB"
  ;;           :modifier :fhir.search.modifier/exact}]}]}
  (uri-parse "/Observation?code:in=http%3A%2F%2Floinc.org%7C8867-4&value-quantity=lt60%2Cgt100")
  ;; {:type "Observation",
  ;;  :join :fhir.search.join/and,
  ;;  :params [{:name "code", 
  ;;            :modifier :fhir.search.modifier/in, 
  ;;            :value "http://loinc.org|8867-4"}
  ;;           {:name "value-quantity",
  ;;            :join :fhir.search.join/or,
  ;;            :params [{:value "60", 
  ;;                      :prefix :fhir.search.prefix/lt} 
  ;;                     {:value "100", 
  ;;                      :prefix :fhir.search.prefix/gt}]}]}
  (uri-parse "/DiagnosticReport?result=http://loinc.org%7C2823-3$gt5.4%7Chttp://unitsofmeasure.org%7Cmmol/L")
  ;; {:type "DiagnosticReport",
  ;;  :join :fhir.search.join/and,
  ;;  :params
  ;;  [{:name "result",
  ;;    :composite true,
  ;;    :params
  ;;    [{:name "http://loinc.org|2823-3", :value "5.4|http://unitsofmeasure.org|mmol/L", :prefix :fhir.search.prefix/gt}]}]}
  (uri-parse "/Observation?code-value-quantity=code$loinc%7C12907-2,value$ge150%7Chttp://unitsofmeasure.org%7Cmmol/L&based-on=ServiceRequest/f8d0ee15-43dc-4090-a2d5-379d247672eb")
  ;; {:type "Observation",
  ;;  :join :fhir.search.join/and,
  ;;  :params
  ;;  [{:name "code-value-quantity",
  ;;    :join :fhir.search.join/or,
  ;;    :composite true,
  ;;    :params
  ;;    [{:name "code", :value "loinc|12907-2"}
  ;;     {:name "value", :value "150|http://unitsofmeasure.org|mmol/L", :prefix :fhir.search.prefix/ge}]}
  ;;   {:name "based-on", :value "ServiceRequest/f8d0ee15-43dc-4090-a2d5-379d247672eb"}]}
  (uri-parse "/Patient?general-practitioner.name=Joe&general-practitioner.address-state=MN")
  ;; {:type "Patient",
  ;;  :join :fhir.search.join/and,
  ;;  :params
  ;;  [[{:name "general-practitioner", :join :fhir.search.join/and, :params [{:name "name", :value "Joe"}], :chain true}]
  ;;   [{:name "general-practitioner",
  ;;     :join :fhir.search.join/and,
  ;;     :params [{:name "address-state", :value "MN"}],
  ;;     :chain true}]]}
  (uri-parse "/DiagnisticReport?subject.name=peter")
  ;; {:type "DiagnisticReport",
  ;;  :join :fhir.search.join/and,
  ;;  :params [[{:name "subject", :join :fhir.search.join/and, :params [{:name "name", :value "peter"}], :chain true}]]}
  (uri-parse "/Patient?general-practitioner:PractitionerRole.practitioner:Practitioner.name:contains=John&organization=Organization/909823472760")
  ;; {:type "Patient",
  ;;  :join :fhir.search.join/and,
  ;;  :params
  ;;  [{:name "general-practitioner",
  ;;    :type "PractitionerRole",
  ;;    :join :fhir.search.join/and,
  ;;    :params
  ;;    [{:name "practitioner",
  ;;      :type "Practitioner",
  ;;      :join :fhir.search.join/and,
  ;;      :params [{:name "name", :modifier :fhir.search.modifier/contains, :value "John"}]}],
  ;;    :chain true}
  ;;   {:name "organization", :value "Organization/909823472760"}]} 
  )