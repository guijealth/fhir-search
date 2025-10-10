(ns fhir-search.uri-query
  (:require
   [clojure.string :as str]
   [fhir-search.complex :refer [clean]])
  (:import
   [java.net URI URLDecoder]))

(def modifier-pattern (re-pattern "(.+):(above|below|code-text|contains|exact|identifier|in|iterate|missing|not|not-in|of-type|text|text-advaced)?$"))
(def prefix-pattern (re-pattern #"^(eq|ne|gt|lt|ge|le|sa|eb|ap)(\d.*)"))
(def component-pattern (re-pattern #"^(.*)\$(.*)$"))

(defn parse-path [path]
  (let [[part1 part2 part3 :as parts] (->> (str/split path #"/")
                                           (remove str/blank?))]
    (case (count parts)
      1 {:type part1}
      2 {:type part1, :id part2}
      3 {:type part3, :compartment {:type part1, :id part2}})))

(defn parse-value [value modifier]
  (->> (str/split value #",")
       (map (fn [v] (let [safe-v (str/replace v #"%(?![0-9A-Fa-f]{2})" "%25")]
                      (URLDecoder/decode safe-v "UTF-8"))))
       (mapv (fn [v]
               (let [[_ cmp v1] (re-find component-pattern v)
                     [_ prefix v2] (re-find prefix-pattern (or v1 v))]
                 {:modifier modifier
                  :name cmp
                  :value (or v2 v1 v)
                  :prefix (when prefix (keyword "fhir.search.prefix" prefix))})))))

(defn build-chain "Builds a nested structure from pre-parsed segments.
 - segments: sequence of maps representing chain links.
 - keys-to-first: map containing :modifier, :value, :params, :composite for the first link.
 - every-chained?: boolean value to specify the chained status for every segment."

  [segments keys-to-first every-chained?]
  (when (seq segments)
    (let [seg (reverse segments)
          {:keys [modifier value params composite]} keys-to-first]
      (->> seg
           (map-indexed (fn [idx item]
                          (cond-> item
                            (zero? idx) (assoc :modifier modifier
                                               :value value
                                               :params params
                                               :composite composite)
                            (pos? idx) (assoc :join :fhir.search.join/and))))
           (reduce (fn [acc curr]
                     (let [base (assoc curr :params [acc])]
                       (if every-chained?
                         (assoc base :chained true)
                         base))))))))

(defn parse-has [param]
  (let [{:keys [name modifier value params composite]} param
        
        split-name (str/split name #"\.")

        forward-part (drop-last split-name)

        reverse-part (let [s (last split-name)]
                       (->> (re-seq #"(?<=:)(?!_has\b)[^:.]+" s)
                            (partition-all 2)))
        
        forward-segments (->> forward-part
                              (mapv (fn [part]
                                      (let [[name type] (str/split part #":")]
                                        {:name name
                                         :chained true
                                         :type type}))))
        
        reverse-part-count (count reverse-part)

        reverse-segments (->> reverse-part
                              (map-indexed (fn [idx [part1 part2 :as curr]]
                                             (cond-> (case (count curr)
                                                       1 {:name part1}
                                                       2 {:name part2
                                                          :type part1})
                                               (not= idx (dec reverse-part-count))
                                               (assoc :reverse true)))))
        
        all-segments (concat forward-segments reverse-segments)

        keys-to-first {:modifier modifier
                       :value value
                       :params params
                       :composite composite}]
    
    (build-chain all-segments keys-to-first false)))

(defn parse-chain [param]
  (let [{:keys [name modifier value params composite]} param]
    (if (re-find #"\." name)
      (let [chain (seq (str/split name #"\."))
            segments (->> chain
                          (map (fn [part]
                                 (let [[name type] (str/split part #":")]
                                   {:name name
                                    :type type}))))
            keys-to-first {:modifier modifier
                           :value value
                           :params params
                           :composite composite}]
        (build-chain segments keys-to-first true))
      param)))

(defn parse-query [query]
  (when-not (str/blank? query)
    (->> (str/split query #"&")
         (map (fn [param]
                (let [[key value] (str/split param #"=")
                      [_ name mod] (re-find modifier-pattern key)
                      modifier (when mod
                                 (keyword "fhir.search.modifier" mod))
                      params (parse-value value modifier)
                      params-c (count params)]
                  (cond-> {:name (or name key)}
                    (= 1 params-c) (merge (-> params first clean))
                    (> params-c 1) (assoc :join :fhir.search.join/or
                                          :params params)
                    (seq (filter :name params)) (assoc :composite true)))))
         (mapv #(if (re-find #"_has:" (:name %))
                  (parse-has %)
                  (parse-chain %))))))

(defn parse [fhir-query]
  (let [url ^URI (URI. fhir-query)
        path (.getPath url)
        query (.getQuery url)]
    (-> (parse-path path)
        (assoc :join (when-not (str/blank? query)
                       :fhir.search.join/and)
               :params (parse-query query))
        (clean))))
