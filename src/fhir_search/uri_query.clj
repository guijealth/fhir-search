(ns fhir-search.uri-query
   (:require [clojure.string :as str]
             [fhir-search.complex :refer [clean update-specific-val
                                          seq-nest
                                          search-key-val
                                          delete-specific-val]])
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
   "Evals if a parameter orquery string it's a chained parameter."
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

 (defn has-param?
   [s]
   (cond
     (re-find #"=" s)
     (when-let [g (first (str/split s #"="))]
       (when (re-find #"\_has:" g) true))
     :else (when (re-find #"\_has:" s) true)))

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

;;Important functions
 (defn coll-keys-add [coll param]
   (reduce (fn [o i]
             (let [layer (cond
                           (= i (last coll))
                           (assoc (first i) :composite nil)
                           (and (= i (first coll))
                                (chained-params? param))
                           (assoc (first i) :chain true)
                           :else (first i))]
               (conj o [layer])))
           [] coll))



 (defn reduce-has-chain [group]
   (reduce (fn [o i]
             (let [element {:name (if (= 1 (count i))
                                    (first i)
                                    (second i))
                            :type (if (= 1 (count group))
                                    (first i)
                                    (when-not  (= 1 (count i))
                                      (first i)))
                            :join (when-not (= i (last group))
                                    :fhir.search.join/and)
                            :reverse (if (= 1 (count group)) true
                                         (when-not (= i (last group)) true))
                            :params nil}]
               (if (= i (last group))
                 (conj o [(assoc element :value nil)])
                 (conj o [element]))))
   [] group))

(defn has-partition [string]
  (let [has-partition (->> (remove empty? (str/split string #"\_has:"))
                           (map #(str/split % #":"))
                           (flatten))]
    (partition-all 2 has-partition)))

(defn has-param
  [string]
  (let [coll (coll-keys-add (reduce-has-chain (has-partition string)) string)]
    (seq-nest (first (first coll)) :params nil (rest coll))))


(defn reduce-chain [g]
  (reduce (fn [out in]
            (if (has-param? in)
              (if (= in (last g)) 
                (conj out [(has-param in)])
                           (conj out [(delete-specific-val (has-param in) :value nil)]))
              (let [name-type (str/split in #":")
                    element {:name (if-not (= (last g) in)
                                     (when-let [name (first name-type)] name)
                                     (first name-type))
                             :type (when-not (= (last g) in)
                                     (when-let [type (second name-type)] type))
                             :join (when-not (= (last g) in) :fhir.search.join/and)
                             :modifier (when (and (= (last g) in) (re-find #":" in))
                                         (when-let [mod (last name-type)]
                                           (modifier mod)))
                             :params nil}]
                (if (= in (last g))
                  (conj out [(assoc element :value nil)])
                  (conj out [element]))))) [] g))

(defn chained-params
  [string]
  (let [group (str/split string #"\.")
        coll (coll-keys-add (reduce-chain group) string)]
    (seq-nest (first (first coll)) :params nil (rest coll))))

;; Layers functions
(defn param-layer [string]
  (cond
    (chained-params? string)
    (chained-params string)
    ;;
    (has-param? string)
    (has-param string)
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
