(ns fhir-search.roundtrip-test
  (:require [clojure.test :refer [deftest testing is]]
            [fhir-search.uri-query :refer [parse to-url]]))

(deftest context-parsing-tests
  (testing "1.1 Simple resource type /Condition"
    (let [ast {:type "Condition"}
          url (to-url ast)
          parsed (parse url)]
      (is (= ast parsed))))

  (testing "1.1 Resource with id - /Condition/c123"
    (let [ast {:type "Condition"
               :id "c123"}
          url (to-url ast)
          parsed (parse url)]
      (is (= ast parsed))))

  (testing "1.1 Compartment context - /Patient/p123/Condition"
    (let [ast {:type "Condition"
               :compartment {:type "Patient"
                             :id "p123"}}
          url (to-url ast)
          parsed (parse url)]
      (is (= ast parsed))))

  (testing "1.2 Compartment with parameters"
    (let [ast {:type "Condition"
               :compartment {:type "Patient"
                             :id "p123"}
               :join :fhir.search.join/and
               :params [{:name "code"
                         :modifier :fhir.search.modifier/in
                         :value "http://hspc.org/ValueSet/acute-concerns"}]}
          url (to-url ast)
          parsed (parse url)]
      (is (= ast parsed)))))

(deftest multiple-values-parsing-tests
  (testing "2.1 Multiple parameters with AND - /Patient?given=John&family=Doe"
    (let [ast {:type "Patient"
               :join :fhir.search.join/and
               :params [{:name "given"
                         :value "John"}
                        {:name "family"
                         :value "Doe"}]}
          url (to-url ast)
          parsed (parse url)]
      (is (= ast parsed))))

  (testing "2.2 Multiple values with OR - /Patient?given:exact=GivenA,GivenB"
    (let [ast {:type "Patient"
               :join :fhir.search.join/and
               :params [{:join :fhir.search.join/or
                         :name "given"
                         :params [{:modifier :fhir.search.modifier/exact
                                   :value "GivenA"}
                                  {:modifier :fhir.search.modifier/exact
                                   :value "GivenB"}]}]}
          url (to-url ast)
          parsed (parse url)]
      (is (= ast parsed))))

  (testing "2.3 Mixed parameters with prefixes"
    (let [ast {:type "Observation"
               :join :fhir.search.join/and
               :params [{:name "code"
                         :modifier :fhir.search.modifier/in
                         :value "http://loinc.org|8867-4"}
                        {:join :fhir.search.join/or
                         :name "value-quantity"
                         :params [{:prefix :fhir.search.prefix/lt
                                   :value "60"}
                                  {:prefix :fhir.search.prefix/gt
                                   :value "100"}]}]}
          url (to-url ast)
          parsed (parse url)]
      (is (= ast parsed)))))

(deftest composite-param-parsing-tests

  (testing "3.1 Composite parameter with multiple components"
    (let [ast {:type "Observation"
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
                         :value "ServiceRequest/f8d0ee15-43dc-4090-a2d5-379d247672eb"}]}
          url (to-url ast)
          parsed (parse url)]
      (is (= ast parsed)))))

(deftest chained-params-parsing-tests

  (testing "4.1 Deep chaining with types and modifiers"
    (let [ast {:type "Patient"
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
                              :value "Organization/909823472760"}]}
          url (to-url ast)
          parsed (parse url)]
      (is (= ast parsed))))

  (testing "4.2 Simple chaining without type - /DiagnosticReport?subject.name=peter"
    (let [ast {:type "DiagnosticReport"
                    :join :fhir.search.join/and
                    :params [{:name "subject"
                              :join :fhir.search.join/and
                              :chained true
                              :params [{:name "name"
                                        :value "peter"}]}]}
          url (to-url ast)
          parsed (parse url)]
      (is (= ast parsed))))

  (testing "4.3 Chaining with explicit type - /DiagnosticReport?subject:Patient.name=peter"
    (let [ast {:type "DiagnosticReport"
                    :join :fhir.search.join/and
                    :params [{:name "subject"
                              :type "Patient"
                              :join :fhir.search.join/and
                              :chained true
                              :params [{:name "name"
                                        :value "peter"}]}]}
          url (to-url ast)
          parsed (parse  url)]
      (is (= ast parsed))))

  (testing "4.4 Multiple chained parameters on same resource"
    (let [ast {:type "Patient"
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
                                        :value "MN"}]}]}
          url (to-url ast)
          parsed (parse url)]
      (is (= ast parsed)))))

(deftest reverse-chaining-parsing-tests

  (testing "5.1 Simple reverse chaining with _has"
    (let [ast {:type "Patient"
                    :join :fhir.search.join/and
                    :params [{:name "patient"
                              :type "Observation"
                              :reverse true
                              :join :fhir.search.join/and
                              :params [{:name "code"
                                        :value "1234-5"}]}]}
          url (to-url ast)
          parsed (parse url)]
      (is (= ast parsed))))

  (testing "5.2 Nested reverse chaining with _has"
    (let [ast {:type "Patient"
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
                              :value "Joe"}]}
          url (to-url ast)
          parsed (parse url)]
      (is (= ast parsed))))
  (testing "5.3 Mixted reverse chaining with forward and reverse segments."
    (let [ast {:type "Encounter"
                    :join :fhir.search.join/and
                    :params [{:name "patient"
                              :chained true
                              :join :fhir.search.join/and
                              :params [{:name "member"
                                        :type "Group"
                                        :reverse true
                                        :join :fhir.search.join/and
                                        :params [{:name "_id"
                                                  :value "102"}]}]}]}
          url (to-url ast)
          parsed (parse url)]
      (is (= ast parsed)))))