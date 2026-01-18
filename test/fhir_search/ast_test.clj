(ns fhir-search.ast-test
  (:require
   [clojure.test :as test :refer [deftest testing is are]]
   [fhir-search.ast :as ast])
  (:import
   (java.time LocalDate LocalDateTime OffsetDateTime)))

(def test-params-data
  [{:code "identifier"
    :base ["Patient"]
    :type "token"
    :expression "Patient.identifier"}

   {:code "birthdate"
    :base ["Patient"]
    :type "date"
    :expression "Patient.birthDate"}

   {:code "component-code-value-quantity"
    :base ["Observation"]
    :type "composite"
    :expression "Observation"
    :component [{:definition "http://hl7.org/fhir/SearchParameter/Observation-code"}
                {:definition "http://hl7.org/fhir/SearchParameter/Observation-value-quantity"}]}

   {:code "code"
    :url "http://hl7.org/fhir/SearchParameter/Observation-code"
    :base ["Observation"]
    :type "token"
    :expression "Observation.code"}

   {:code "value-quantity"
    :url "http://hl7.org/fhir/SearchParameter/Observation-value-quantity"
    :base ["Observation"]
    :type "quantity"
    :expression "Observation.value.ofType(Quantity)"}])

(deftest parse-token-value-test
  (testing "system|code format"
    (are [input expected] (= expected (ast/parse-token-value input))
      "loinc|12907-2"  {:system "loinc" :code "12907-2"}
      "http://loinc.org|12907-2"  {:system "http://loinc.org" :code "12907-2"}))

  (testing "only system (ends with |)"
    (is (= {:system "loinc"}
           (ast/parse-token-value "loinc|"))))

  (testing "only code (starts with |)"
    (is (= {:code "12907-2"}
           (ast/parse-token-value "|12907-2"))))

  (testing "plain value (no pipes)"
    (is (= "male" (ast/parse-token-value "male"))))

  (testing "multiple values (vector)"
    (let [input [{:value "loinc|12907-2"}
                 {:value "|12907-2"}
                 {:value "male"}]
          expected [{:value {:system "loinc" :code "12907-2"}}
                    {:value {:code "12907-2"}}
                    {:value "male"}]]
      (is (= expected (ast/parse-token-value input))))))

;;

(deftest parse-date-value-test
  (testing "LocalDate - date only (no T)"
    (are [input] (instance? LocalDate (ast/parse-date-value input))
      "2010-01-01"
      "2024-12-31"
      "1900-01-01"))

  (testing "OffsetDateTime - with timezone indicator"
    (are [input] (instance? OffsetDateTime (ast/parse-date-value input))
      "2010-01-01T10:30:00Z"
      "2010-01-01T10:30:00+05:00"
      "2010-01-01T10:30:00-03:00"))

  (testing "LocalDateTime - time but no timezone"
    (are [input] (instance? LocalDateTime (ast/parse-date-value input))
      "2010-01-01T10:30:00"
      "2024-12-31T23:59:59"))

  (testing "multiple values (vector)"
    (let [input [{:value "2010-01-01"}
                 {:value "2010-01-01T10:30:00Z"}
                 {:value "2010-01-01T10:30:00"}]
          result (ast/parse-date-value input)]
      (is (instance? LocalDate (:value (nth result 0))))
      (is (instance? OffsetDateTime (:value (nth result 1))))
      (is (instance? LocalDateTime (:value (nth result 2)))))))

;;

(deftest parse-number-value-test
  (testing "single number value"
    (are [input expected] (= expected (ast/parse-number-value input))
      "42"      42.0
      "3.14"    3.14
      "0"       0.0
      "-10.5"   -10.5))

  (testing "multiple values (vector)"
    (let [input [{:value "42"} {:value "3.14"}]
          expected [{:value 42.0} {:value 3.14}]]
      (is (= expected (ast/parse-number-value input))))))

(deftest parse-quantity-value-test
  (testing "value|system|code - all three parts"
    (is (= {:value "5.4" :system "http://unitsofmeasure.org" :code "mg"}
           (ast/parse-quantity-value "5.4|http://unitsofmeasure.org|mg"))))

  (testing "|system|code - no value"
    (is (= {:system "http://unitsofmeasure.org" :code "mg"}
           (ast/parse-quantity-value "|http://unitsofmeasure.org|mg"))))

  (testing "value|system| - no code"
    (is (= {:value "5.4" :system "http://unitsofmeasure.org"}
           (ast/parse-quantity-value "5.4|http://unitsofmeasure.org|"))))

  (testing "value|code - no system (implicit)"
    (is (= {:value "5.4" :code "mg"}
           (ast/parse-quantity-value "5.4|mg"))))

  (testing "||system|| - only system with pipes"
    (is (= {:system "http://unitsofmeasure.org"}
           (ast/parse-quantity-value "|http://unitsofmeasure.org|"))))

  (testing "|code - only code"
    (is (= {:code "mg"}
           (ast/parse-quantity-value "|mg"))))

  (testing "value| - only value"
    (is (= {:value "5.4"}
           (ast/parse-quantity-value "5.4|"))))

  (testing "plain value - no pipes"
    (is (= "5.4" (ast/parse-quantity-value "5.4"))))

  (testing "multiple values (vector)"
    (let [input [{:value "5.4|http://unitsofmeasure.org|mg"}
                 {:value "10|mg"}]
          expected [{:value {:value "5.4" :system "http://unitsofmeasure.org" :code "mg"}}
                    {:value {:value "10" :code "mg"}}]]
      (is (= expected (ast/parse-quantity-value input))))))

;;

(deftest format-value-test
  (testing "token type"
    (is (= {:system "loinc" :code "12907-2"}
           (ast/format-value :token "loinc|12907-2"))))

  (testing "date type"
    (is (instance? LocalDate
                   (ast/format-value :date "2010-01-01"))))

  (testing "number type"
    (is (= 42.0 (ast/format-value :number "42"))))

  (testing "quantity type"
    (is (= {:value "5.4" :system "http://unitsofmeasure.org" :code "mg"}
           (ast/format-value :quantity "5.4|http://unitsofmeasure.org|mg"))))

  (testing "string type (identity)"
    (is (= "test" (ast/format-value :string "test"))))

  (testing "reference type (identity)"
    (is (= "Patient/123" (ast/format-value :reference "Patient/123"))))

  (testing "uri type (identity)"
    (is (= "http://example.com" (ast/format-value :uri "http://example.com"))))

  (testing "unsupported type throws exception"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"Unsupported search parameter type"
         (ast/format-value :unsupported "value")))))

;;

(deftest extract-data-test
  (testing "extract token by code"
    (let [result (ast/extract-data
                  {:restype "Patient" :code "identifier"}
                  test-params-data)]
      (is (= :token (:type result)))
      (is (= ["Patient.identifier"] (:path result)))))

  (testing "extract by url (for composite resolution)"
    (let [result (ast/extract-data
                  {:restype "Observation"
                   :url "http://hl7.org/fhir/SearchParameter/Observation-code"}
                  test-params-data)]
      (is (= :token (:type result)))
      (is (= ["Observation.code"] (:path result)))))

  (testing "extract composite includes component definitions"
    (let [result (ast/extract-data
                  {:restype "Observation"
                   :code "component-code-value-quantity"}
                  test-params-data)]
      (is (:component result))
      (is (= 2 (count (:component result))))
      (is (every? #(contains? % :path) (:component result))))))

(deftest process-composite-test
  (testing "process composite resolves component types correctly"
    (let [param {:name "component-code-value-quantity"
                 :composite true
                 :params [{:name "code" :value "loinc|1234"}
                          {:name "value-quantity" :value "5.4|mg"}]}

          components [{:type :token
                       :path ["Observation.code"]}
                      {:type :quantity
                       :path ["Observation.value.ofType(Quantity)"]}]

          result (ast/process-composite param components)]

      (is (= :composite (:type result)))
      (is (= 2 (count (:params result))))

      (is (= {:system "loinc" :code "1234"}
             (-> result :params first :value)))
      (is (= {:value "5.4" :code "mg"}
             (-> result :params second :value)))))

  (testing "incorrect number of components throws"
    (let [param {:name "test"
                 :composite true
                 :params [{:name "code" :value "loinc|1234"}]}
          components [{:type :token} {:type :quantity}]]
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           #"Incorrect composite parameter"
           (ast/process-composite param components))))))

(deftest enrich-ast-test
  (testing "enrich spec.md ASTs examples"
    (are [input expected] (= expected (ast/enrich input))
      ;; Case 1.2
      ;; in
      {:type "Condition"
       :compartment {:type "Patient"
                     :id "p123"}
       :join :fhir.search.join/and
       :params
       [{:name "code"
         :modifier :fhir.search.modifier/in
         :value "http://hspc.org/ValueSet/acute-concerns"}]}
      ;; out
      {:type "Condition"
       :compartment {:type "Patient"
                     :id "p123"}
       :join :fhir.search.join/and
       :params
       [{:name "code"
         :modifier :fhir.search.modifier/in
         :value "http://hspc.org/ValueSet/acute-concerns"
         :type :token
         :path ["Condition.code"]}]}

      ;; Case 2.1
      ;; in
      {:type "Patient"
       :join :fhir.search.join/and
       :params
       [{:name "given"
         :value "John"}
        {:name "family"
         :value "Doe"}]}
      ;; out
      {:type "Patient"
       :join :fhir.search.join/and
       :params
       [{:name "given"
         :value "John"
         :type :string
         :path ["Patient.name.given"]}
        {:name "family"
         :value "Doe"
         :type :string
         :path ["Patient.name.family"]}]}

      ;; Case 2.2
      ;; in
      {:type "Patient"
       :join :fhir.search.join/and
       :params
       [{:join :fhir.search.join/or
         :name "given"
         :params
         [{:modifier :fhir.search.modifier/exact
           :value "GivenA"}
          {:modifier :fhir.search.modifier/exact
           :value "GivenB"}]}]}
      ;; out
      {:type "Patient"
       :join :fhir.search.join/and
       :params
       [{:join :fhir.search.join/or
         :name "given"
         :type :string
         :path ["Patient.name.given"]
         :params
         [{:modifier :fhir.search.modifier/exact
           :value "GivenA"}
          {:modifier :fhir.search.modifier/exact
           :value "GivenB"}]}]}

      ;; Case 2.3
      ;; in
      {:type "Observation"
       :join :fhir.search.join/and
       :params
       [{:name "code"
         :modifier :fhir.search.modifier/in
         :value "http://loinc.org|8867-4"}
        {:join :fhir.search.join/or
         :name "value-quantity"
         :params
         [{:prefix :fhir.search.prefix/lt
           :value "60"}
          {:prefix :fhir.search.prefix/gt
           :value "100"}]}]}
      ;; out
      {:type "Observation"
       :join :fhir.search.join/and
       :params
       [{:name "code"
         :modifier :fhir.search.modifier/in
         :type :token
         :path ["Observation.code"]
         :value {:system "http://loinc.org" :code "8867-4"}}
        {:join :fhir.search.join/or
         :name "value-quantity"
         :type :quantity
         :path ["(Observation.value as Quantity)" "(Observation.value as SampledData)"]
         :params
         [{:prefix :fhir.search.prefix/lt :value "60"}
          {:prefix :fhir.search.prefix/gt :value "100"}]}]}

      ;; Case 3.1
      ;; in
      {:type "Observation"
       :join :fhir.search.join/and
       :params
       [{:name "code-value-quantity"
         :join :fhir.search.join/or
         :composite true
         :params
         [{:name "code"
           :value "loinc|12907-2"}
          {:name "value"
           :prefix :fhir.search.prefix/ge
           :value "150|http://unitsofmeasure.org|mmol/L"}]}
        {:name "based-on"
         :value "ServiceRequest/f8d0ee15-43dc-4090-a2d5-379d247672eb"}]}
      ;; out
      {:type "Observation"
       :join :fhir.search.join/and
       :params
       [{:name "code-value-quantity"
         :join :fhir.search.join/or
         :type :composite
         :params
         [{:name "code" 
           :value {:system "loinc" :code "12907-2"} 
           :type :token 
           :path ["Observation.code"]}
          {:name "value"
           :prefix :fhir.search.prefix/ge
           :type :quantity
           :path ["(Observation.value as Quantity)" "(Observation.value as SampledData)"]
           :value {:value "150" 
                   :system "http://unitsofmeasure.org" 
                   :code "mmol/L"}}]}
        {:name "based-on"
         :value "ServiceRequest/f8d0ee15-43dc-4090-a2d5-379d247672eb"
         :type :reference
         :path ["Observation.basedOn"]}]})))