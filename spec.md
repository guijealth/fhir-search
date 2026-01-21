# FHIR search map

FHIR search map represents a fhir search interaction request. A 
search url should be parsed to a map. Discover variants of this
through examples.

## 1. Context (https://www.hl7.org/fhir/search.html#searchcontexts)

### 1.1

```clj
;; /Patient/p123/Condition

{:type "Condition"
 :compartment {:type "Patient"
               :id "p123"}}
```

```clj
;; /Condition/c123

{:type "Condition"
 :id "c123"}
```


```clj
;; /Condition

{:type "Condition"}
```

### 1.2

```clj
;; /Patient/p123/Condition?code:in=http%3A%2F%2Fhspc.org%2FValueSet%2Facute-concerns

{:type "Condition"
 :compartment {:type "Patient"
               :id "p123"}
 :join :fhir.search.join/and
 :params [{:name "code"
           :modifier :fhir.search.modifier/in
           :value "http://hspc.org/ValueSet/acute-concerns"}]}
```

## 2. Multiple Values (https://www.hl7.org/fhir/search.html#combining)

### 2.1

```clj
;; /Patient?given=John&family=Doe

{:type "Patient"
 :join :fhir.search.join/and
 :params [{:name "given"
           :value "John"}
          {:name "family"
           :value "Doe"}]}
```

### 2.2

```clj
;; /Patient?given:exact=GivenA,GivenB

{:type "Patient"
 :join :fhir.search.join/and
 :params [{:join :fhir.search.join/or
           :name "given"
           :params [{:modifier :fhir.search.modifier/exact
                     :value "GivenA"}
                    {:modifier :fhir.search.modifier/exact
                     :value "GivenB"}]}]}
```

### 2.3

```clj
;; /Observation?code:in=http%3A%2F%2Floinc.org%7C8867-4&value-quantity=lt60%2Cgt100

{:type "Observation"
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
```

## 3. Composite Param (https://www.hl7.org/fhir/search.html#composite)

### 3.1


```clj
;; /Observation?component-code-value-quantity%3Dhttp%3A%2F%2Floinc.org%7C8480-6%24lt60

{:type "Observation"
 :join :fhir.search.join/and
 :params
 [{:name "component-code-value-quantity"
   :components 
   [{:value "http://loinc.org|8480-6"} 
   {:value "60"
    :prefix :fhir.search.prefix/lt}]}]}

```
### 3.2
```clj
;; /Group?characteristic-value%3Dgender%24mixed

{:type "Group"
 :join :fhir.search.join/and
 :params 
 [{:name "characteristic-value"
   :components 
   [{:value "gender"} 
    {:value "mixed"}]}]}

```
### 3.3
```clj
;; /Questionnaire?context-type-value%3Dfocus%24http%3A%2F%2Fsnomed.info%2Fsct%7C408934002

{:type "Questionnaire"
 :join :fhir.search.join/and
 :params 
 [{:name "context-type-value"
   :components 
   [{:value "focus"} 
    {:value "http://snomed.info/sct|408934002"}]}]}

```
## 4. Chained Params (https://www.hl7.org/fhir/search.html#chaining)

### 4.1

```clj
;; /Patient?general-practitioner%3APractitionerRole.practitioner%3APractitioner.name%3Acontains%3DJohn%26organization%3DOrganization%2F909823472760

{:type "Patient"
 :join :fhir.search.join/and
 :params [{:name "general-practitioner"
           :target "PractitionerRole"
           :join :fhir.search.join/and
           :chained true
           :params [{:name "practitioner"
                     :target "Practitioner"
                     :join :fhir.search.join/and
                     :chained true
                     :params [{:name "name"
                               :modifier :fhir.search.modifier/contains
                               :value "John"}]}]}
          {:name "organization"
           :value "Organization/909823472760"}]}
```


### 4.2

```clj
;; /DiagnosticReport?subject.name=peter

{:type "DiagnosticReport"
 :join :fhir.search.join/and
 :params [{:name "subject"
           :join :fhir.search.join/and
           :chained true
           :params [{:name "name"
                     :value "peter"}]}]}
```

### 4.3

```clj
;; /DiagnosticReport?subject:Patient.name=peter

{:type "DiagnosticReport"
 :join :fhir.search.join/and
 :params [{:name "subject"
           :target "Patient"
           :join :fhir.search.join/and
           :chained true
           :params [{:name "name"
                     :value "peter"}]}]}
```

### 4.4

```clj
;; /Patient?general-practitioner.name=Joe&general-practitioner.address-state=MN

{:type "Patient"
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
```

## 5 Reverse chaining (https://www.hl7.org/fhir/search.html#has)
### 5.1
```clj
;; /Patient?_has:Observation:patient:code=1234-5

{:type "Patient"
 :join :fhir.search.join/and
 :params [{:name "patient"
           :target "Observation"
           :reverse true
           :join :fhir.search.join/and
           :params [{:name "code"
                     :value "1234-5"}]}]}

```
### 5.2
```clj
;; /Patient?_has:Observation:patient:_has:AuditEvent:entity:agent=MyUserId&name:contains=Joe

{:type "Patient"
 :join :fhir.search.join/and
 :params [{:name "patient"
           :target "Observation"
           :join :fhir.search.join/and
           :reverse true
           :params [{:name "entity"
                     :target "AuditEvent"
                     :join :fhir.search.join/and
                     :reverse true
                     :params [{:name "agent"
                               :value "MyUserId"}]}]}
          {:name "name"
           :modifier :fhir.search.modifier/contains
           :value "Joe"}]}
```
### 5.3
```clj 
;; /Encounter?patient._has:Group:member:_id=102

{:type "Encounter"
 :join :fhir.search.join/and
 :params [{:name "patient"
           :chained true
           :join :fhir.search.join/and
           :params [{:name "member"
                     :target "Group"
                     :reverse true
                     :join :fhir.search.join/and
                     :params [{:name "_id"
                               :value "102"}]}]}]}

```
