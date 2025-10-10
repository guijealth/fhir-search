(ns fhir-search.uri-query-test
  (:require [clojure.test :refer [deftest testing is]]
            [fhir-search.uri-query :as fq]))

(deftest context-parsing-tests
  (testing "1.1 Simple resource type /Condition"
    (is (= {:type "Condition"}
                (fq/parse "/Condition"))))

  (testing "1.1 Resource with id - /Condition/c123"
    (is (= {:type "Condition"
                 :id "c123"}
                (fq/parse "/Condition/c123"))))

  (testing "1.1 Compartment context - /Patient/p123/Condition"
    (is (= {:type "Condition"
                 :compartment {:type "Patient"
                               :id "p123"}}
                (fq/parse "/Patient/p123/Condition"))))

  (testing "1.2 Compartment with parameters"
    (let [input "/Patient/p123/Condition?code:in=http%3A%2F%2Fhspc.org%2FValueSet%2Facute-concerns"
          expected {:type "Condition"
                    :compartment {:type "Patient"
                                  :id "p123"}
                    :join :fhir.search.join/and
                    :params [{:name "code"
                              :modifier :fhir.search.modifier/in
                              :value "http://hspc.org/ValueSet/acute-concerns"}]}]
      (is (= expected (fq/parse input))))))

(deftest multiple-values-parsing-tests
  (testing "2.1 Multiple parameters with AND - /Patient?given=John&family=Doe"
    (let [input "/Patient?given=John&family=Doe"
          expected {:type "Patient"
                    :join :fhir.search.join/and
                    :params [{:name "given"
                              :value "John"}
                             {:name "family"
                              :value "Doe"}]}]
      (is (= expected (fq/parse input)))))

  (testing "2.2 Multiple values with OR - /Patient?given:exact=GivenA,GivenB"
    (let [input "/Patient?given:exact=GivenA,GivenB"
          expected {:type "Patient"
                    :join :fhir.search.join/and
                    :params [{:join :fhir.search.join/or
                              :name "given"
                              :params [{:modifier :fhir.search.modifier/exact
                                        :value "GivenA"}
                                       {:modifier :fhir.search.modifier/exact
                                        :value "GivenB"}]}]}]
      (is (= expected (fq/parse input)))))

  (testing "2.3 Mixed parameters with prefixes"
    (let [input "/Observation?code:in=http%3A%2F%2Floinc.org%7C8867-4&value-quantity=lt60%2Cgt100"
          expected {:type "Observation"
                    :join :fhir.search.join/and
                    :params [{:name "code"
                              :modifier :fhir.search.modifier/in
                              :value "http://loinc.org|8867-4"}
                             {:join :fhir.search.join/or
                              :name "value-quantity"
                              :params [{:prefix :fhir.search.prefix/lt
                                        :value "60"}
                                       {:prefix :fhir.search.prefix/gt
                                        :value "100"}]}]}]
      (is (= expected (fq/parse input))))))

(deftest composite-param-parsing-tests
  
  (testing "3.1 Composite parameter with multiple components"
    (let [input "/Observation?code-value-quantity%3Dcode%24loinc%7C12907-2%2Cvalue%24ge150%7Chttp%3A%2F%2Funitsofmeasure.org%7Cmmol%2FL%26based-on%3DServiceRequest%2Ff8d0ee15-43dc-4090-a2d5-379d247672eb"
          expected {:type "Observation"
                    :join :fhir.search.join/and
                    :params [{:name "code-value-quantity"
                              :join :fhir.search.join/or
                              :composite true
                              :params [{:name "code"
                                        :value "loinc|12907-2"}
                                       {:name "value"
                                        :prefix :fhir.search.prefix/ge
                                        :value "150|http://unitsofmeasure.org|mmol/L"}]}
                             {:name "based-on"
                              :value "ServiceRequest/f8d0ee15-43dc-4090-a2d5-379d247672eb"}]}]
      (is (= expected (fq/parse input))))))

(deftest chained-params-parsing-tests
  
  (testing "4.1 Deep chaining with types and modifiers"
    (let [input "/Patient?general-practitioner%3APractitionerRole.practitioner%3APractitioner.name%3Acontains%3DJohn%26organization%3DOrganization%2F909823472760"
          expected {:type "Patient"
                    :join :fhir.search.join/and
                    :params [{:name "general-practitioner"
                              :type "PractitionerRole"
                              :join :fhir.search.join/and
                              :chained true
                              :params [{:name "practitioner"
                                        :type "Practitioner"
                                        :join :fhir.search.join/and
                                        :chained true
                                        :params [{:name "name"
                                                  :modifier :fhir.search.modifier/contains
                                                  :value "John"}]}]}
                             {:name "organization"
                              :value "Organization/909823472760"}]}]
      (is (= expected (fq/parse input)))))
  
  (testing "4.2 Simple chaining without type - /DiagnosticReport?subject.name=peter"
    (let [input "/DiagnosticReport?subject.name=peter"
          expected {:type "DiagnosticReport"
                    :join :fhir.search.join/and
                    :params [{:name "subject"
                              :join :fhir.search.join/and
                              :chained true
                              :params [{:name "name"
                                        :value "peter"}]}]}]
      (is (= expected (fq/parse input)))))
  
  (testing "4.3 Chaining with explicit type - /DiagnosticReport?subject:Patient.name=peter"
    (let [input "/DiagnosticReport?subject:Patient.name=peter"
          expected {:type "DiagnosticReport"
                    :join :fhir.search.join/and
                    :params [{:name "subject"
                              :type "Patient"
                              :join :fhir.search.join/and
                              :chained true
                              :params [{:name "name"
                                        :value "peter"}]}]}]
      (is (= expected (fq/parse input)))))
  
  (testing "4.4 Multiple chained parameters on same resource"
    (let [input "/Patient?general-practitioner.name=Joe&general-practitioner.address-state=MN"
          expected {:type "Patient"
                    :join :fhir.search.join/and
                    :params [{:name "general-practitioner"
                              :join :fhir.search.join/and
                              :chained true
                              :params [{:name "name"
                                        :value "Joe"}]}
                             {:name "general-practitioner"
                              :join :fhir.search.join/and
                              :chained true
                              :params [{:name "address-state"
                                        :value "MN"}]}]}]
      (is (= expected (fq/parse input))))))

(deftest reverse-chaining-parsing-tests
  
  (testing "5.1 Simple reverse chaining with _has"
    (let [input "/Patient?_has:Observation:patient:code=1234-5"
          expected {:type "Patient"
                    :join :fhir.search.join/and
                    :params [{:name "patient"
                              :type "Observation"
                              :reverse true
                              :join :fhir.search.join/and
                              :params [{:name "code"
                                        :value "1234-5"}]}]}]
      (is (= expected (fq/parse input)))))
  
  (testing "5.2 Nested reverse chaining with _has"
    (let [input "/Patient?_has:Observation:patient:_has:AuditEvent:entity:agent=MyUserId&name:contains=Joe"
          expected {:type "Patient"
                    :join :fhir.search.join/and
                    :params [{:name "patient"
                              :type "Observation"
                              :join :fhir.search.join/and 
                              :reverse true
                              :params [{:name "entity"
                                        :type "AuditEvent"
                                        :join :fhir.search.join/and 
                                        :reverse true
                                        :params [{:name "agent"
                                                  :value "MyUserId"}]}]}
                             {:name "name"
                              :modifier :fhir.search.modifier/contains
                              :value "Joe"}]}]
      (is (= expected (fq/parse input)))))
  (testing "5.3 Mixted reverse chaining with forward and reverse segments."
    (let [input "/Encounter?patient._has:Group:member:_id=102"
          expected {:type "Encounter"
                    :join :fhir.search.join/and
                    :params [{:name "patient"
                              :chained true
                              :join :fhir.search.join/and
                              :params [{:name "member"
                                        :type "Group"
                                        :reverse true
                                        :join :fhir.search.join/and
                                        :params [{:name "_id"
                                                  :value "102"}]}]}]}]
      (is (= expected (fq/parse input))))))

(deftest edge-cases-tests 
  (testing "Query with empty parameters"
    (is (= {:type "Patient"}
           (fq/parse "/Patient?")))))


