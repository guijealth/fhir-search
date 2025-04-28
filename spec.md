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

### 1.2

```clj
;; /Patient/p123/Condition
;;    ?code:in=http%3A%2F%2Fhspc.org%2FValueSet%2Facute-concerns

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
           :values [{:modifier :fhir.search.modifier/exact
                     :value "GivenA"}
                    {:modifier :fhir.search.modifier/exact
                     :value "GivenB"}]}]}
```

### 2.3

```clj
;; /Observation 
;;    ?code:in=http%3A%2F%2Floinc.org%7C8867-4
;;    &value-quantity=lt60%2Cgt100

{:type "Observation"
 :join :fhir.search.join/and
 :params [{:name "code"
           :modifier :fhir.search.modifier/in
           :value "http://loinc.org|8867-4"}
          {:join :fhir.search.join/or
           :name "value-quantity"
           :values [{:prefix :fhir.search.prefix/lt
                     :value "60"}
                    {:prefix :fhir.search.prefix/gt
                     :value "100"}]}]}
```

## 3. Composite Param (https://www.hl7.org/fhir/search.html#composite)

### 3.1


```clj
;; /Observation
;;    ?code-value-quantity=code$loinc|12907-2,value$ge150|http://unitsofmeasure.org|mmol/L
;;    &based-on=ServiceRequest/f8d0ee15-43dc-4090-a2d5-379d247672eb

{:type "Observation"
 :join :fhir.search.join/and
 :params [{:name "code-value-quantity"
           :join :fhir.search.join/or
           :composite true
           :values [{:name "code"
                     :value "loinc|12907-2"}
                    {:name "value"
                     :prefix :fhir.search.prefix/ge
                     :value "150|http://unitsofmeasure.org|mmol/L"}]}
          {:name "based-on"
           :value "ServiceRequest/f8d0ee15-43dc-4090-a2d5-379d247672eb"}]}
```

## 4. Chained Params (https://www.hl7.org/fhir/search.html#chaining)

### 4.1

```clj
;; /Patient
;;    ?general-practitioner:PractitionerRole.practitioner:Practitioner.name:contains=John
;;    &organization=Organization/909823472760

{:type "Patient"
 :join :fhir.search.join/and
 :params [{:name "general-practitioner"
           :type "PractitionerRole"
           :join :fhir.search.join/and
           :chained true
           :params [{:name "practitioner"
                     :type "Practitioner"
                     :join :fhir.search.join/and
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
           :type "Patient"
           :join :fhir.search.join/and
           :chained true
           :params [{:name "name"
                     :value "peter"}]}]}
```

### 4.4

```clj
;; /Patient
;;    ?general-practitioner.name=Joe
;;    &general-practitioner.address-state=MN

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

```clj
;; /Patient
;;   ?_has:Observation:patient._has:AuditEvent:entity:agent=MyUserId
;;   &name:contains=Joe

{:type "Patient"
 :join :fhir.search.join/and
 :params [{:name "patient"
           :type "Observation"
           :join :fhir.search.join/and
           :chained true
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
```
