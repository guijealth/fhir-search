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
 :params {:join :fhir.search.join/and
          :values [{:name "code"
                    :modifier :fhir.search.modifier/in
                    :value "http://hspc.org/ValueSet/acute-concerns"}]}}
```

## 2. Multiple Values (https://www.hl7.org/fhir/search.html#combining)

### 2.1

```clj
;; /Patient?given=John&family=Doe

{:type "Patient"
 :params {:join :fhir.search.join/and
          :values [{:name "given"
                    :value "John"}
                   {:name "family"
                    :value "Doe"}]}}
```

### 2.2

```clj
;; /Patient?given:exact=GivenA,GivenB

{:type "Patient"
 :params {:join :fhir.search.join/and
          :values [{:join :fhir.search.join/or
                    :name "given"
                    :values [{:modifier :fhir.search.modifier/exact
                              :value "GivenA"}
                             {:modifier :fhir.search.modifier/exact
                              :value "GivenB"}]}]}}
```

### 2.3

```clj
;; /Observation 
;;    ?code=http%3A%2F%2Floinc.org%7C8867-4
;;    &value-quantity=lt60%2Cgt100

{:type "Observation"
 :params {:join :fhir.search.join/and
          :values [{:name "code"
                    :modifier :fhir.search.modifier/in
                    :value "http://loinc.org|8867-4"}
                   {:name "value-quantity"
                    :join :fhir.search.join/or
                    :values [{:prefix :fhir.search.prefix/lt
                              :value "60"}
                             {:prefix :fhir.search.prefix/gt
                              :value "100"}]}]}}
```

## 3. Modifiers (https://www.hl7.org/fhir/search.html#modifiers)

[wip]