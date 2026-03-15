(ns fhir-search.complex
  (:require [clojure.walk :refer [postwalk]]))

(defn clean
  "Recursively removes nil values from a nested data structure (bottom-up).
  
    - Map entries with nil values are removed. If all entries are removed,
      the map itself becomes nil.
    - Vectors with nil elements are compacted. If all elements are removed,
      the vector itself becomes nil.
    - Seqs have nil elements removed, but an all-nil seq returns ()
      rather than nil (unlike maps and vectors).
  
    Because traversal is bottom-up (postwalk), nil propagates upward:
    a nil-cleaned child can cause its parent map entry to be removed in turn.
  
    Examples:
      (clean {:a 1 :b nil :c {:d nil :e 2}})
      => {:a 1 :c {:e 2}}
  
      (clean {:a nil :b {:c nil}})
      => nil
  
      (clean [nil nil])
      => nil
  
      (clean '(nil nil))
      => ()"
  [m]
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

