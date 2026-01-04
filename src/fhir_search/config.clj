(ns fhir-search.config
  (:require
   [clojure.java.io :as io]
   [clojure.edn :as edn]
   [clojure.pprint :as pprint]))

(defn spit-config! [cfg]
  (spit "config.edn" (with-out-str (pprint/pprint cfg))))

(defn load-config
  ([] (load-config "config.edn"))
  ([path]
   (let [file (io/file path)]
     (if (and (.exists file) (pos? (.length file)))
       (-> file slurp edn/read-string)
       (do (spit-config! {})
           {})))))

(defn resolve-params
  ([alias]
   (resolve-params (load-config) alias))
  ([cfg alias]
   {:pre [(keyword? alias)]}
   (get-in cfg [:search-params :registry alias])))

(defn active-params
  "Return a map with `:alias`, `:path` and `:url` of the active params"
  ([]
   (active-params (load-config)))
  ([cfg] 
   (let [active-alias (get-in cfg [:search-params :active])]
     (->
      (get-in cfg [:search-params :registry active-alias])
      (assoc :alias active-alias)))))

(defn use-params!
  "Activate an specific search-params configuration to use."
  ([alias] (use-params! (load-config) alias))
  ([cfg alias]
   {:pre [(keyword? alias)]}
   (when (resolve-params cfg alias)
     (try
       (let [new-cfg (assoc-in cfg [:search-params :active] alias)]
         (spit-config! new-cfg)
         (active-params new-cfg))
       (catch Exception e
         (throw
          (ex-info "Activation failed"
                   {:description (str "The activation of the alias " alias " search-params has failed.")
                    :message (.getMessage e)})))))))

(comment
  ;;Si se activa con éxito devuelve la info del archivo search-params en uso, de lo contrario devuelve nil
  (use-params! :r4)
  (resolve-params :r4)
  :.)