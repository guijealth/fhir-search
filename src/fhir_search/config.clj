(ns fhir-search.config
  (:require
   [clojure.java.io :as io]
   [clojure.edn :as edn]))

(defn load-config []
  (if (.exists (io/file "config.edn"))
    (-> "config.edn" slurp edn/read-string)
    (let [default-cfg {:search-params
                       {:active :r4
                        :registry {:r4 "resources/search_params/default.edn"}}}]
      (spit "config.edn" default-cfg)
      default-cfg)))

(defn get-path [alias]
  (get-in (load-config) [:search-params :registry alias]))

(defn activate-search-params 
  "Activate an specific search-params config by it's alias."
  [alias]
  {:pre [(keyword? alias)]}
  (when (get-path alias)
    (try
      (let [new-cfg (assoc-in (load-config) [:search-params :active] alias)]
        (spit "config.edn" new-cfg))
      {:active alias}
      (catch Exception e
        (throw
         (ex-info "Set up failed"
                  {:type :setup
                   :message (.getMessage e)}))))))

