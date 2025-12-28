(ns fhir-search.compiler-test
  (:require [clojure.test :refer [deftest testing is]]
            [fhir-search.compiler :as compiler]))

(deftest parse-token-value-test
  (testing "Single value"
    (is (= {:system "loinc" :code "12907-2"} (compiler/parse-token-value "loinc|12907-2")))
    (is (= {:system "loinc"} (compiler/parse-token-value "loinc|")))
    (is (= {:code "12907-2"} (compiler/parse-token-value "|12907-2")))
    (is (= "male" (compiler/parse-token-value "male"))))

  (testing "multiple values (OR)"
    (let [value [{:value "loinc|12907-2"}
                 {:value "12907-2"}]
          expected [{:value {:system "loinc" :code "12907-2"}}
                    {:value "12907-2"}]]
      (is (= expected (compiler/parse-token-value value))))))

(deftest format-value-test
  (testing "Token param value"
    (let [value [{:value "http://hospital.org/mrn|12345"}
                 {:value "http://hospital.org/mrn|67890"}]
          expected [{:value {:system "http://hospital.org/mrn"
                             :code "12345"}}
                    {:value {:system "http://hospital.org/mrn"
                             :code "67890"}}]]
      (is (= expected (compiler/format-value :token value)))))
  
  (testing "Unsupported param type"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"Unsupported search parameter type"
         (compiler/format-value :foo "value-example")))))

(deftest enrich-ast-test
  (testing "Patient AST with identifier (OR)"
    (let [ast {:type "Patient",
               :join :fhir.search.join/and,
               :params
               [{:name "identifier",
                 :join :fhir.search.join/or,
                 :params [{:value "http://hospital.org/mrn|12345"} 
                          {:value "http://hospital.org/mrn|67890"}]}]}
          
          expected {:type "Patient"
                    :join :fhir.search.join/and
                    :params [{:name "identifier"
                              :join :fhir.search.join/or
                              :type :token
                              :path ["Patient.identifier"]
                              :params [{:value {:system "http://hospital.org/mrn"
                                                :code "12345"}}
                                       {:value {:system "http://hospital.org/mrn"
                                                :code "67890"}}]}]}]
      (is (= expected (compiler/enrich ast))))))

(comment 

   

  
  :.)