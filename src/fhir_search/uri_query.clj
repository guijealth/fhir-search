(ns fhir-search.uri-query
  (:require
   [clojure.string :as str]
   [fhir-search.complex :refer [clean]])
  (:import
   [java.net URI]))

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
       (mapv (fn [v]
               (let [[_ cmp v1] (re-find component-pattern v)
                     [_ prefix v2] (re-find prefix-pattern (or v1 v))]
                 {:modifier modifier
                  :name cmp
                  :value (or v2 v1 v)
                  :prefix prefix})))))

(defn build-chain "Builds a nested structure from pre-parsed segments.
 - segments: sequence of maps representing chain links.
 - keys-to-first: map containing :modifier, :value, :params, :composite for the first link.
 - addit-keys: map with additional keys such as :chained, :reverse."

  [segments keys-to-first addit-keys]
  (when (seq segments)
    (let [seg (reverse segments)
          {:keys [modifier value params composite]} keys-to-first
          {:keys [chained reverse]} addit-keys]
      (->> seg
           (map-indexed (fn [idx item]
                          (cond-> item
                            (zero? idx) (assoc :modifier modifier
                                               :value value
                                               :params params
                                               :composite composite)
                            (pos? idx) (assoc :join :fhir.search.join/and))))
           (reduce (fn [acc curr]
                     (assoc curr
                            :params [acc]
                            :reverse reverse
                            :chained chained)))))))

(defn parse-has [param]
  (let [{:keys [name modifier value params composite]} param
        segments (->> (str/split name #"\.")
                      (map #(re-seq #"(?<=:)[^:._]+" %))
                      (flatten)
                      (partition-all 2)
                      (reduce (fn [acc curr]
                                (conj acc (let [[part1 part2] curr]
                                            (case (count curr)
                                              1 {:name part1}
                                              2 {:name part2
                                                 :type part1}))))
                              []))
        keys-to-first {:modifier modifier
                       :value value
                       :params params
                       :composite composite}
        addit-keys {:reverse true
                    :chained (when (re-find #"\." name) true)}]
    (build-chain segments keys-to-first addit-keys)))

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
                           :composite composite}
            addit-keys {:chained true}]
        (build-chain segments keys-to-first addit-keys))
      param)))

(defn parse-query [query]
  (when query
    (->> (str/split query #"&")
         (map (fn [param]
                (let [[key value] (str/split param #"=")
                      [_ name mod] (re-find modifier-pattern key)
                      modifier (when mod
                                 (keyword "fhir.search.modifier" mod))
                      params (parse-value value modifier)
                      params-c (count params)]
                  (cond-> {:name (or name key)}
                    (= 1 params-c) (assoc :modifier modifier
                                          :value value)
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
        (assoc :join (when query :fhir.search.join/and)
               :params (parse-query query))
        (clean))))
