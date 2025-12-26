(ns fhir-search.registry
  (:require [clj-http.client :as http]))

(defn download-search-params
  "Download search-parameters form URL."
  [url opts]
  (let [response (->> (http/get url {:as :json})
                      :body
                      :entry
                      (filterv #(= "SearchParameter" (:resourceType %))))
        {:keys [alias default]} opts
        path (str "resorces/search_parameters/" alias ".edn")]
    (spit path response)))