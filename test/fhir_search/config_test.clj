(ns fhir-search.config-test
  (:require [clojure.test :refer [deftest testing is]]
            [fhir-search.config :as config]))

(def test-config {:search-params
                  {:active :test
                   :registry
                   {:test
                    {:path "path/example"
                     :url "https://url.example"}
                    :other
                    {:path "other/path/example"
                     :url "https://other.url.example"}}}})

(deftest load-config-test
  (testing "Load config by path"
    (let [cfg (config/load-config "test/resources/test_config.edn")]
      (is (= test-config cfg)))))

(deftest resolve-params-test
  (testing "Resolve registred search-param"
    (is (= {:path "path/example" :url "https://url.example"} (config/resolve-params test-config :test))))

  (testing "Resolve unregistred search-param"
    (is (nil? (config/resolve-params test-config :foo)))))

(deftest get-active-params-data-test
  (is (= {:alias :test :path "path/example" :url "https://url.example"} (config/active-params test-config))))

