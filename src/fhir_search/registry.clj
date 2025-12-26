(ns fhir-search.registry
  (:require [clj-http.client :as http]
            [fhir-search.config :as cfg]
            [clojure.pprint :as pprint]
            [clojure.tools.logging :as log]))

(defn download-search-params!
  "Download search-parameters form URL."
  [url opts]
  (let [{:keys [alias activate rewrite]} opts
        config (cfg/load-config)]
    (when (and (cfg/resolve-params config alias) (not rewrite))
      (throw (ex-info "Alias in use"
                      {:alias alias
                       :suggestion "Use :rewrite true to overwrite"})))
    
    (log/info "Starting download" {:url url :alias alias})
    
    (let [response (->> (http/get url {:as :json})
                        :body
                        :entry
                        (mapv :resource)
                        (filterv #(= "SearchParameter" (:resourceType %))))
          path (str "resources/search_parameters/" (name alias) ".edn")]
    
      (log/info "Retrieved search parameters" {:count (count response)})
      (log/debug "Saving to path" {:path path})

      (spit path (with-out-str (pprint/pprint response))) 
      (cfg/spit-config! (assoc-in config [:search-params :registry alias] {:path path :url url}))
    
      (when activate 
        (log/info "Activating search parameters" {:alias alias})
        (cfg/use-params config alias))
    
      (log/info "Download completed" {:alias alias :activated (boolean activate)})

      (cfg/resolve-params alias))))

(comment
  (download-search-params! "https://hl7.org/fhir/R4/search-parameters.json" {:alias :r4, :activate true :rewrite true})

  :.)