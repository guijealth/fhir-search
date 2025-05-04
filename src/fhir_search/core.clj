(ns fhir-search.core
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

(defn parse-chain [param]
  (let [{:keys [name modifier value params composite]} param]
    (if-let [chain (seq (str/split name #"\."))]
      (->> chain
           (map (fn [part]
                  (let [[name type] (str/split part #":")]
                    {:name name
                     :type type})))
           (reverse)
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
                            :chained true))))
      param)))

(defn parse-query [query]
  (->> (str/split query #"&")
       (map (fn [param]
              (let [[key value] (str/split param #"=")
                    [_ name mod] (re-find modifier-pattern key)
                    modifier (when mod
                               (keyword "fhir.search.modier" mod))
                    params (parse-value value modifier)
                    params-c (count params)]
                (cond-> {:name (or name key)}
                  (= 1 params-c) (assoc :modifier modifier
                                        :value value)
                  (> params-c 1) (assoc :params params)
                  (seq (filter :name params)) (assoc :composite true)))))
       (mapv parse-chain)))

(comment

  (let [fhir-query "/DiagnosticReport?subject.name=peter"
        url ^URI (URI. fhir-query)]
    (-> (parse-path (.getPath url))
        (assoc :join :fhir.search.join/and
               :params (parse-query (.getQuery url)))
        (clean)))



  :.)