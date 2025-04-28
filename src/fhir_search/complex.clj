(ns fhir-search.complex
  (:require [clojure.walk :refer [postwalk]]))

(defn clean [m]
  (postwalk (fn [v]
              (cond
                (map-entry? v)
                (when-not (nil? (val v))
                  v)
                ;;
                (map? v)
                (when-let [entries (seq (remove #(-> % second nil?) v))]
                  (into {} entries))
                ;;
                (vector? v)
                (when-let [coll (seq (remove nil? v))]
                  (into [] coll))
                ;;
                (seq? v)
                (remove nil? v)
                ;;
                :else v)) m))

(defn update-specific-val [coll key origin-val new-val]
  (postwalk (fn [x]
              (if (and (map? x)
                       (contains? x key)
                       (= origin-val (key x)))
                (assoc x key new-val)
                x))
            coll))

(defn delete-specific-val [coll key value]
  (postwalk (fn [x]
              (if (and (map? x)
                       (contains? x key)
                       (= value (key x)))
                (dissoc x key value)
                x))
            coll))

(defn seq-nest
  "Returns a map tree of nestings made in a main layer associated to a keyword with a given value."
  [layer  key value colls]
  (reduce (fn [o i]
            (update-specific-val o key value i))
          layer colls))

(defn search-key-val
  "Return a vector with the key-values pairs finded" 
  ([coll key]
  (let [result (atom [])]
    (postwalk (fn [x]
                (when (and (map? x) (contains? x key))
                  (when-let [v (key x)]
                    (swap! result conj [key v]))) x)
              coll)
    @result))
  ([coll key value]
   (let [result (atom [])]
     (postwalk (fn [x]
                 (when (and (map? x) (contains? x key))
                   (when (= value (key x))
                     (let[v (key x)] (swap! result conj [key v])))) x)
               coll)
     @result)))
