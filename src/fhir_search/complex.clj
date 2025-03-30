(ns fhir-search.complex
  (:require
   [clojure.walk :refer [postwalk]]))

(defn clean [m]
  (postwalk (fn [x]
              (cond
                (map-entry? x)
                (when-not (nil? (val x))
                  x)
                ;;
                (map? x)
                (when-let [entries (seq (remove (comp nil? val) x))]
                  (into {} entries))
                ;;
                (vector? x)
                (when-let [coll (seq (remove nil? x))]
                  (into [] coll))
                ;;
                (seq? x)
                (remove nil? x)
                ;;
                :else x)) m))