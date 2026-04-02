;; Note: this is an automatically generated file. Do not edit.
;; checking

(ns test-ns
  (:require [clojure.test :refer :all]))

;; Advanced

;; Annotated terms

;; Top-level annotations

(deftest test-all-negadvanced-negannotated-terms-negtop-neglevel-annotations-negannotated-literal

  (is (= int32

         int32)))

(deftest test-all-negadvanced-negannotated-terms-negtop-neglevel-annotations-negannotated-list

  (is (= list<string>

         list<string>)))

(deftest test-all-negadvanced-negannotated-terms-negtop-neglevel-annotations-negannotated-record

  (is (= Person

         Person)))

(deftest test-all-negadvanced-negannotated-terms-negtop-neglevel-annotations-negannotated-lambda

  (is (= (∀t0.(t0 → t0))

         (∀t0.(t0 → t0)))))

;; Nested annotations

(deftest test-all-negadvanced-negannotated-terms-negnested-annotations-negannotation-within-annotation

  (is (= int32

         int32)))

(deftest test-all-negadvanced-negannotated-terms-negnested-annotations-negannotated-terms-in-tuple

  (is (= (int32, string)

         (int32, string))))

(deftest test-all-negadvanced-negannotated-terms-negnested-annotations-negannotated-term-in-function-application

  (is (= int32

         int32)))

;; Annotations in complex contexts

(deftest test-all-negadvanced-negannotated-terms-negannotations-in-complex-contexts-negannotated-let-binding

  (is (= (int32, string)

         (int32, string))))

(deftest test-all-negadvanced-negannotated-terms-negannotations-in-complex-contexts-negannotated-record-fields

  (is (= Person

         Person)))

(deftest test-all-negadvanced-negannotated-terms-negannotations-in-complex-contexts-negannotated-function-in-application

  (is (= int32

         int32)))

;; Algebraic types

;; Unit

;; Unit term

(deftest test-all-negalgebraic-types-negunit-negunit-term-negunit-literal

  (is (= unit

         unit)))

;; Unit term in polymorphic context

(deftest test-all-negalgebraic-types-negunit-negunit-term-in-polymorphic-context-negunit-from-lambda

  (is (= (∀t0.(t0 → unit))

         (∀t0.(t0 → unit)))))

;; Pairs

;; Basic pairs

(deftest test-all-negalgebraic-types-negpairs-negbasic-pairs-negpair-of-int-and-string

  (is (= (int32, string)

         (int32, string))))

(deftest test-all-negalgebraic-types-negpairs-negbasic-pairs-negpair-of-string-and-boolean

  (is (= (string, boolean)

         (string, boolean))))

(deftest test-all-negalgebraic-types-negpairs-negbasic-pairs-negpair-of-boolean-and-int

  (is (= (boolean, int32)

         (boolean, int32))))

;; Polymorphic pairs

(deftest test-all-negalgebraic-types-negpairs-negpolymorphic-pairs-negpair-from-lambda-first-element

  (is (= (∀t0.(t0 → (t0, string)))

         (∀t0.(t0 → (t0, string))))))

(deftest test-all-negalgebraic-types-negpairs-negpolymorphic-pairs-negpair-from-lambda-second-element

  (is (= (∀t0.(t0 → (string, t0)))

         (∀t0.(t0 → (string, t0))))))

(deftest test-all-negalgebraic-types-negpairs-negpolymorphic-pairs-negpair-from-two-lambdas

  (is (= (∀t0.(∀t1.(t0 → t1 → (t0, t1))))

         (∀t0.(∀t1.(t0 → t1 → (t0, t1)))))))

(deftest test-all-negalgebraic-types-negpairs-negpolymorphic-pairs-negpair-with-repeated-variable

  (is (= (∀t0.(t0 → (t0, t0)))

         (∀t0.(t0 → (t0, t0))))))

;; Pairs in complex contexts

(deftest test-all-negalgebraic-types-negpairs-negpairs-in-complex-contexts-negpair-in-list

  (is (= list<(int32, string)>

         list<(int32, string)>)))

(deftest test-all-negalgebraic-types-negpairs-negpairs-in-complex-contexts-negpair-in-let-binding

  (is (= (int32, string)

         (int32, string))))

;; Nested pairs

(deftest test-all-negalgebraic-types-negpairs-negnested-pairs-negpair-of-pairs

  (is (= ((int32, string), (boolean, int32))

         ((int32, string), (boolean, int32)))))

(deftest test-all-negalgebraic-types-negpairs-negnested-pairs-negpair-with-list

  (is (= (list<int32>, string)

         (list<int32>, string))))

(deftest test-all-negalgebraic-types-negpairs-negnested-pairs-neglist-of-pairs

  (is (= list<(int32, string)>

         list<(int32, string)>)))

;; Pairs with complex types

(deftest test-all-negalgebraic-types-negpairs-negpairs-with-complex-types-negpair-with-record-on-first

  (is (= (Person, int32)

         (Person, int32))))

(deftest test-all-negalgebraic-types-negpairs-negpairs-with-complex-types-negpair-with-record-on-second

  (is (= (string, Person)

         (string, Person))))

;; Eithers

;; Left values

(deftest test-all-negalgebraic-types-negeithers-negleft-values-negleft-int

  (is (= (∀t0.either<int32, t0>)

         (∀t0.either<int32, t0>))))

(deftest test-all-negalgebraic-types-negeithers-negleft-values-negleft-string

  (is (= (∀t0.either<string, t0>)

         (∀t0.either<string, t0>))))

(deftest test-all-negalgebraic-types-negeithers-negleft-values-negleft-boolean

  (is (= (∀t0.either<boolean, t0>)

         (∀t0.either<boolean, t0>))))

;; Right values

(deftest test-all-negalgebraic-types-negeithers-negright-values-negright-int

  (is (= (∀t0.either<t0, int32>)

         (∀t0.either<t0, int32>))))

(deftest test-all-negalgebraic-types-negeithers-negright-values-negright-string

  (is (= (∀t0.either<t0, string>)

         (∀t0.either<t0, string>))))

(deftest test-all-negalgebraic-types-negeithers-negright-values-negright-boolean

  (is (= (∀t0.either<t0, boolean>)

         (∀t0.either<t0, boolean>))))

;; Polymorphic eithers

(deftest test-all-negalgebraic-types-negeithers-negpolymorphic-eithers-negleft-from-lambda

  (is (= (∀t0.(∀t1.(t0 → either<t0, t1>)))

         (∀t0.(∀t1.(t0 → either<t0, t1>))))))

(deftest test-all-negalgebraic-types-negeithers-negpolymorphic-eithers-negright-from-lambda

  (is (= (∀t0.(∀t1.(t0 → either<t1, t0>)))

         (∀t0.(∀t1.(t0 → either<t1, t0>))))))

(deftest test-all-negalgebraic-types-negeithers-negpolymorphic-eithers-negeither-from-two-lambdas

  (is (= (∀t0.(boolean → t0 → either<t0, t0>))

         (∀t0.(boolean → t0 → either<t0, t0>)))))

;; Eithers in complex contexts

(deftest test-all-negalgebraic-types-negeithers-negeithers-in-complex-contexts-negeither-in-list

  (is (= list<either<string, int32>>

         list<either<string, int32>>)))

(deftest test-all-negalgebraic-types-negeithers-negeithers-in-complex-contexts-negeither-in-let-binding

  (is (= (∀t0.either<t0, int32>)

         (∀t0.either<t0, int32>))))

;; Nested eithers

(deftest test-all-negalgebraic-types-negeithers-negnested-eithers-negeither-of-either-left-left

  (is (= (∀t0.(∀t1.either<either<int32, t0>, t1>))

         (∀t0.(∀t1.either<either<int32, t0>, t1>)))))

(deftest test-all-negalgebraic-types-negeithers-negnested-eithers-negeither-of-either-left-right

  (is (= (∀t0.(∀t1.either<either<t0, string>, t1>))

         (∀t0.(∀t1.either<either<t0, string>, t1>)))))

(deftest test-all-negalgebraic-types-negeithers-negnested-eithers-negeither-of-either-right

  (is (= (∀t0.either<t0, boolean>)

         (∀t0.either<t0, boolean>))))

(deftest test-all-negalgebraic-types-negeithers-negnested-eithers-negeither-of-list

  (is (= (∀t0.either<list<int32>, t0>)

         (∀t0.either<list<int32>, t0>))))

(deftest test-all-negalgebraic-types-negeithers-negnested-eithers-neglist-of-eithers

  (is (= list<either<string, int32>>

         list<either<string, int32>>)))

;; Eithers with complex types

(deftest test-all-negalgebraic-types-negeithers-negeithers-with-complex-types-negeither-with-record-on-left

  (is (= (∀t0.either<Person, t0>)

         (∀t0.either<Person, t0>))))

(deftest test-all-negalgebraic-types-negeithers-negeithers-with-complex-types-negeither-with-record-on-right

  (is (= (∀t0.either<t0, Person>)

         (∀t0.either<t0, Person>))))

;; Optionals

;; Monomorphic optionals

(deftest test-all-negalgebraic-types-negoptionals-negmonomorphic-optionals-negnothing

  (is (= (∀t0.maybe<t0>)

         (∀t0.maybe<t0>))))

(deftest test-all-negalgebraic-types-negoptionals-negmonomorphic-optionals-negjust-int

  (is (= maybe<int32>

         maybe<int32>)))

(deftest test-all-negalgebraic-types-negoptionals-negmonomorphic-optionals-negjust-string

  (is (= maybe<string>

         maybe<string>)))

(deftest test-all-negalgebraic-types-negoptionals-negmonomorphic-optionals-negjust-boolean

  (is (= maybe<boolean>

         maybe<boolean>)))

;; Polymorphic optionals

(deftest test-all-negalgebraic-types-negoptionals-negpolymorphic-optionals-negoptional-from-lambda

  (is (= (∀t0.(t0 → maybe<t0>))

         (∀t0.(t0 → maybe<t0>)))))

(deftest test-all-negalgebraic-types-negoptionals-negpolymorphic-optionals-negnothing-from-lambda

  (is (= (∀t0.(∀t1.(t0 → maybe<t1>)))

         (∀t0.(∀t1.(t0 → maybe<t1>))))))

(deftest test-all-negalgebraic-types-negoptionals-negpolymorphic-optionals-negconditional-optional

  (is (= (∀t0.(t0 → boolean → maybe<t0>))

         (∀t0.(t0 → boolean → maybe<t0>)))))

;; Optionals in complex contexts

(deftest test-all-negalgebraic-types-negoptionals-negoptionals-in-complex-contexts-negoptional-in-record

  (is (= (BuddyListA @ string)

         (BuddyListA @ string))))

(deftest test-all-negalgebraic-types-negoptionals-negoptionals-in-complex-contexts-negoptional-in-let-binding

  (is (= maybe<int32>

         maybe<int32>)))

;; Nested optionals

(deftest test-all-negalgebraic-types-negoptionals-negnested-optionals-negoptional-of-optional

  (is (= maybe<maybe<string>>

         maybe<maybe<string>>)))

(deftest test-all-negalgebraic-types-negoptionals-negnested-optionals-negoptional-of-list

  (is (= maybe<list<int32>>

         maybe<list<int32>>)))

(deftest test-all-negalgebraic-types-negoptionals-negnested-optionals-neglist-of-optionals

  (is (= list<maybe<string>>

         list<maybe<string>>)))

;; Optionals with complex types

(deftest test-all-negalgebraic-types-negoptionals-negoptionals-with-complex-types-negoptional-map

  (is (= maybe<map<string, int32>>

         maybe<map<string, int32>>)))

;; Collections

;; Lists

;; Lists of literals

(deftest test-all-negcollections-neglists-neglists-of-literals-negint-list

  (is (= list<int32>

         list<int32>)))

(deftest test-all-negcollections-neglists-neglists-of-literals-negstring-list

  (is (= list<string>

         list<string>)))

(deftest test-all-negcollections-neglists-neglists-of-literals-negsingle-element-list

  (is (= list<bigint>

         list<bigint>)))

(deftest test-all-negcollections-neglists-neglists-of-literals-negmixed-numeric-types

  (is (= list<float32>

         list<float32>)))

;; Empty lists

(deftest test-all-negcollections-neglists-negempty-lists-negempty-list

  (is (= (∀t0.list<t0>)

         (∀t0.list<t0>))))

(deftest test-all-negcollections-neglists-negempty-lists-negpair-of-empty-lists

  (is (= (∀t0.(∀t1.(list<t0>, list<t1>)))

         (∀t0.(∀t1.(list<t0>, list<t1>))))))

(deftest test-all-negcollections-neglists-negempty-lists-negempty-list-in-tuple

  (is (= (∀t0.(list<t0>, string))

         (∀t0.(list<t0>, string)))))

;; Polymorphic lists

(deftest test-all-negcollections-neglists-negpolymorphic-lists-neglist-from-lambda

  (is (= (∀t0.(t0 → list<t0>))

         (∀t0.(t0 → list<t0>)))))

(deftest test-all-negcollections-neglists-negpolymorphic-lists-neglist-with-repeated-var

  (is (= (∀t0.(t0 → list<t0>))

         (∀t0.(t0 → list<t0>)))))

(deftest test-all-negcollections-neglists-negpolymorphic-lists-neglist-from-two-lambdas

  (is (= (∀t0.(t0 → t0 → list<t0>))

         (∀t0.(t0 → t0 → list<t0>)))))

;; Nested lists

(deftest test-all-negcollections-neglists-negnested-lists-neglist-of-lists

  (is (= list<list<int32>>

         list<list<int32>>)))

(deftest test-all-negcollections-neglists-negnested-lists-negempty-nested-lists

  (is (= (∀t0.list<list<t0>>)

         (∀t0.list<list<t0>>))))

(deftest test-all-negcollections-neglists-negnested-lists-negnested-polymorphic

  (is (= (∀t0.(t0 → list<list<t0>>))

         (∀t0.(t0 → list<list<t0>>)))))

;; Lists in complex contexts

(deftest test-all-negcollections-neglists-neglists-in-complex-contexts-negmultiple-lists-in-tuple

  (is (= (list<int32>, list<string>)

         (list<int32>, list<string>))))

;; Sets

;; Monomorphic sets

(deftest test-all-negcollections-negsets-negmonomorphic-sets-negempty-set

  (is (= (∀t0.set<t0>)

         (∀t0.set<t0>))))

(deftest test-all-negcollections-negsets-negmonomorphic-sets-negint-set

  (is (= set<int32>

         set<int32>)))

(deftest test-all-negcollections-negsets-negmonomorphic-sets-negstring-set

  (is (= set<string>

         set<string>)))

(deftest test-all-negcollections-negsets-negmonomorphic-sets-negsingle-element-set

  (is (= set<boolean>

         set<boolean>)))

;; Polymorphic sets

(deftest test-all-negcollections-negsets-negpolymorphic-sets-negset-from-lambda

  (is (= (∀t0.(t0 → set<t0>))

         (∀t0.(t0 → set<t0>)))))

(deftest test-all-negcollections-negsets-negpolymorphic-sets-negset-with-repeated-variable

  (is (= (∀t0.(t0 → set<t0>))

         (∀t0.(t0 → set<t0>)))))

(deftest test-all-negcollections-negsets-negpolymorphic-sets-negset-from-two-variables

  (is (= (∀t0.(t0 → t0 → set<t0>))

         (∀t0.(t0 → t0 → set<t0>)))))

;; Sets in complex contexts

(deftest test-all-negcollections-negsets-negsets-in-complex-contexts-negset-in-tuple

  (is (= (set<int32>, string)

         (set<int32>, string))))

(deftest test-all-negcollections-negsets-negsets-in-complex-contexts-negset-in-let-binding

  (is (= set<int32>

         set<int32>)))

;; Nested sets

(deftest test-all-negcollections-negsets-negnested-sets-negset-of-lists

  (is (= set<list<string>>

         set<list<string>>)))

(deftest test-all-negcollections-negsets-negnested-sets-negset-of-tuples

  (is (= set<(int32, int32)>

         set<(int32, int32)>)))

(deftest test-all-negcollections-negsets-negnested-sets-negset-of-sets

  (is (= set<set<string>>

         set<set<string>>)))

;; Sets with complex types

(deftest test-all-negcollections-negsets-negsets-with-complex-types-negset-of-records

  (is (= set<Person>

         set<Person>)))

(deftest test-all-negcollections-negsets-negsets-with-complex-types-negset-of-optionals

  (is (= set<maybe<int32>>

         set<maybe<int32>>)))

(deftest test-all-negcollections-negsets-negsets-with-complex-types-negset-of-maps

  (is (= set<map<string, int32>>

         set<map<string, int32>>)))

;; Maps

;; Monomorphic maps

(deftest test-all-negcollections-negmaps-negmonomorphic-maps-negempty-map

  (is (= (∀t0.(∀t1.map<t0, t1>))

         (∀t0.(∀t1.map<t0, t1>)))))

(deftest test-all-negcollections-negmaps-negmonomorphic-maps-negint-to-string-map

  (is (= map<int32, string>

         map<int32, string>)))

(deftest test-all-negcollections-negmaps-negmonomorphic-maps-negstring-to-int-map

  (is (= map<string, int32>

         map<string, int32>)))

(deftest test-all-negcollections-negmaps-negmonomorphic-maps-negsingle-entry-map

  (is (= map<bigint, boolean>

         map<bigint, boolean>)))

;; Polymorphic maps

(deftest test-all-negcollections-negmaps-negpolymorphic-maps-negmap-from-lambda-keys

  (is (= (∀t0.(t0 → map<t0, string>))

         (∀t0.(t0 → map<t0, string>)))))

(deftest test-all-negcollections-negmaps-negpolymorphic-maps-negmap-from-lambda-values

  (is (= (∀t0.(t0 → map<string, t0>))

         (∀t0.(t0 → map<string, t0>)))))

(deftest test-all-negcollections-negmaps-negpolymorphic-maps-negmap-from-lambda-both

  (is (= (∀t0.(∀t1.(t0 → t1 → map<t0, t1>)))

         (∀t0.(∀t1.(t0 → t1 → map<t0, t1>))))))

(deftest test-all-negcollections-negmaps-negpolymorphic-maps-negmap-with-repeated-variables

  (is (= (∀t0.(t0 → map<t0, t0>))

         (∀t0.(t0 → map<t0, t0>)))))

;; Maps in complex contexts

(deftest test-all-negcollections-negmaps-negmaps-in-complex-contexts-negmap-in-tuple

  (is (= (map<int32, string>, string)

         (map<int32, string>, string))))

(deftest test-all-negcollections-negmaps-negmaps-in-complex-contexts-negnested-maps

  (is (= map<string, map<int32, boolean>>

         map<string, map<int32, boolean>>)))

(deftest test-all-negcollections-negmaps-negmaps-in-complex-contexts-negmap-in-let-binding

  (is (= map<string, int32>

         map<string, int32>)))

;; Maps with complex types

(deftest test-all-negcollections-negmaps-negmaps-with-complex-types-negmap-of-records

  (is (= map<string, Person>

         map<string, Person>)))

(deftest test-all-negcollections-negmaps-negmaps-with-complex-types-negmap-of-lists

  (is (= map<int32, list<string>>

         map<int32, list<string>>)))

(deftest test-all-negcollections-negmaps-negmaps-with-complex-types-negmap-of-tuples

  (is (= map<string, (int32, int32)>

         map<string, (int32, int32)>)))

;; Fundamentals

;; Literals

;; Boolean literals

(deftest test-all-negfundamentals-negliterals-negboolean-literals-negtrue

  (is (= boolean

         boolean)))

(deftest test-all-negfundamentals-negliterals-negboolean-literals-negfalse

  (is (= boolean

         boolean)))

;; String literals

(deftest test-all-negfundamentals-negliterals-negstring-literals-negsimple-string

  (is (= string

         string)))

(deftest test-all-negfundamentals-negliterals-negstring-literals-negempty-string

  (is (= string

         string)))

(deftest test-all-negfundamentals-negliterals-negstring-literals-negunicode-string

  (is (= string

         string)))

;; Integer literals

(deftest test-all-negfundamentals-negliterals-neginteger-literals-negbigint

  (is (= bigint

         bigint)))

(deftest test-all-negfundamentals-negliterals-neginteger-literals-negint8

  (is (= int8

         int8)))

(deftest test-all-negfundamentals-negliterals-neginteger-literals-negint16

  (is (= int16

         int16)))

(deftest test-all-negfundamentals-negliterals-neginteger-literals-negint32

  (is (= int32

         int32)))

(deftest test-all-negfundamentals-negliterals-neginteger-literals-negint64

  (is (= int64

         int64)))

(deftest test-all-negfundamentals-negliterals-neginteger-literals-neguint8

  (is (= uint8

         uint8)))

(deftest test-all-negfundamentals-negliterals-neginteger-literals-neguint16

  (is (= uint16

         uint16)))

(deftest test-all-negfundamentals-negliterals-neginteger-literals-neguint32

  (is (= uint32

         uint32)))

(deftest test-all-negfundamentals-negliterals-neginteger-literals-neguint64

  (is (= uint64

         uint64)))

;; Float literals

(deftest test-all-negfundamentals-negliterals-negfloat-literals-negbigfloat

  (is (= bigfloat

         bigfloat)))

(deftest test-all-negfundamentals-negliterals-negfloat-literals-negfloat32

  (is (= float32

         float32)))

(deftest test-all-negfundamentals-negliterals-negfloat-literals-negfloat64

  (is (= float64

         float64)))

;; Literals in complex contexts

(deftest test-all-negfundamentals-negliterals-negliterals-in-complex-contexts-negliterals-in-tuple

  (is (= (boolean, (string, (int32, float32)))

         (boolean, (string, (int32, float32))))))

(deftest test-all-negfundamentals-negliterals-negliterals-in-complex-contexts-negliterals-in-list

  (is (= list<string>

         list<string>)))

;; Variables

;; Simple variable lookup

(deftest test-all-negfundamentals-negvariables-negsimple-variable-lookup-negint-variable

  (is (= (∀t0.(t0 → t0))

         (∀t0.(t0 → t0)))))

(deftest test-all-negfundamentals-negvariables-negsimple-variable-lookup-negvariable-in-let-binding

  (is (= int32

         int32)))

(deftest test-all-negfundamentals-negvariables-negsimple-variable-lookup-negmultiple-variables

  (is (= (string, int32)

         (string, int32))))

;; Variable scoping

(deftest test-all-negfundamentals-negvariables-negvariable-scoping-neglambda-parameter

  (is (= (∀t0.(∀t1.(t0 → t1 → t0)))

         (∀t0.(∀t1.(t0 → t1 → t0))))))

(deftest test-all-negfundamentals-negvariables-negvariable-scoping-neglet-binding-scope

  (is (= int32

         int32)))

(deftest test-all-negfundamentals-negvariables-negvariable-scoping-negvariable-shadowing

  (is (= (∀t0.(t0 → t0))

         (∀t0.(t0 → t0)))))

(deftest test-all-negfundamentals-negvariables-negvariable-scoping-negnested-scoping

  (is (= (∀t0.(∀t1.(t0 → t1 → (t0, (t0, t1)))))

         (∀t0.(∀t1.(t0 → t1 → (t0, (t0, t1))))))))

;; Polymorphic variables

(deftest test-all-negfundamentals-negvariables-negpolymorphic-variables-negpolymorphic-function

  (is (= (∀t0.(t0 → t0))

         (∀t0.(t0 → t0)))))

(deftest test-all-negfundamentals-negvariables-negpolymorphic-variables-negpolymorphic-application

  (is (= (int32, string)

         (int32, string))))

(deftest test-all-negfundamentals-negvariables-negpolymorphic-variables-neghigher-order-polymorphic

  (is (= (∀t0.(∀t1.((t0 → t1) → t0 → t1)))

         (∀t0.(∀t1.((t0 → t1) → t0 → t1))))))

;; Variables in complex contexts

(deftest test-all-negfundamentals-negvariables-negvariables-in-complex-contexts-negvariable-in-record

  (is (= (string → Person)

         (string → Person))))

(deftest test-all-negfundamentals-negvariables-negvariables-in-complex-contexts-negvariable-in-list

  (is (= (∀t0.(t0 → list<t0>))

         (∀t0.(t0 → list<t0>)))))

(deftest test-all-negfundamentals-negvariables-negvariables-in-complex-contexts-negvariable-in-map

  (is (= (∀t0.(∀t1.(t0 → t1 → map<t0, t1>)))

         (∀t0.(∀t1.(t0 → t1 → map<t0, t1>))))))

(deftest test-all-negfundamentals-negvariables-negvariables-in-complex-contexts-negvariable-in-optional

  (is (= (∀t0.(t0 → maybe<t0>))

         (∀t0.(t0 → maybe<t0>)))))

;; Recursive variables

(deftest test-all-negfundamentals-negvariables-negrecursive-variables-negsimple-recursion

  (is (= (int32 → int32)

         (int32 → int32))))

(deftest test-all-negfundamentals-negvariables-negrecursive-variables-negmutual-recursion

  (is (= (int32 → int32)

         (int32 → int32))))

;; Lambdas

;; Simple lambdas

(deftest test-all-negfundamentals-neglambdas-negsimple-lambdas-negidentity-function

  (is (= (∀t0.(t0 → t0))

         (∀t0.(t0 → t0)))))

(deftest test-all-negfundamentals-neglambdas-negsimple-lambdas-negconstant-function

  (is (= (∀t0.(t0 → int32))

         (∀t0.(t0 → int32)))))

;; Multi-parameter lambdas

(deftest test-all-negfundamentals-neglambdas-negmulti-negparameter-lambdas-negtwo-parameters

  (is (= (∀t0.(∀t1.(t0 → t1 → t0)))

         (∀t0.(∀t1.(t0 → t1 → t0))))))

(deftest test-all-negfundamentals-neglambdas-negmulti-negparameter-lambdas-negthree-parameters

  (is (= (∀t0.(∀t1.(∀t2.(t0 → t1 → t2 → t1))))

         (∀t0.(∀t1.(∀t2.(t0 → t1 → t2 → t1)))))))

(deftest test-all-negfundamentals-neglambdas-negmulti-negparameter-lambdas-negparameter-reuse

  (is (= (∀t0.(∀t1.(t0 → t1 → (t0, (t0, t1)))))

         (∀t0.(∀t1.(t0 → t1 → (t0, (t0, t1))))))))

;; Lambdas with operations

(deftest test-all-negfundamentals-neglambdas-neglambdas-with-operations-neglambda-with-primitive

  (is (= (int32 → int32)

         (int32 → int32))))

(deftest test-all-negfundamentals-neglambdas-neglambdas-with-operations-neglambda-with-application

  (is (= (∀t0.(∀t1.((t0 → t1) → t0 → t1)))

         (∀t0.(∀t1.((t0 → t1) → t0 → t1))))))

(deftest test-all-negfundamentals-neglambdas-neglambdas-with-operations-neglambda-with-construction

  (is (= (∀t0.(∀t1.(t0 → t1 → (t0, t1))))

         (∀t0.(∀t1.(t0 → t1 → (t0, t1)))))))

;; Nested lambdas

(deftest test-all-negfundamentals-neglambdas-negnested-lambdas-neglambda-returning-lambda

  (is (= (∀t0.(∀t1.(∀t2.(t0 → t1 → t2 → t0))))

         (∀t0.(∀t1.(∀t2.(t0 → t1 → t2 → t0)))))))

(deftest test-all-negfundamentals-neglambdas-negnested-lambdas-neglambda-with-let-binding

  (is (= (∀t0.(t0 → t0))

         (∀t0.(t0 → t0)))))

(deftest test-all-negfundamentals-neglambdas-negnested-lambdas-neglambda-with-inner-lambda

  (is (= (∀t0.(t0 → t0))

         (∀t0.(t0 → t0)))))

;; Lambdas in complex contexts

(deftest test-all-negfundamentals-neglambdas-neglambdas-in-complex-contexts-neglambda-in-tuple

  (is (= (∀t0.((t0 → t0), int32))

         (∀t0.((t0 → t0), int32)))))

(deftest test-all-negfundamentals-neglambdas-neglambdas-in-complex-contexts-neglambda-in-list

  (is (= list<(int32 → int32)>

         list<(int32 → int32)>)))

(deftest test-all-negfundamentals-neglambdas-neglambdas-in-complex-contexts-neglambda-in-record

  (is (= (string → Person)

         (string → Person))))

;; Higher-order lambdas

(deftest test-all-negfundamentals-neglambdas-neghigher-negorder-lambdas-negfunction-composition

  (is (= (∀t0.(∀t1.(∀t2.((t0 → t1) → (t2 → t0) → t2 → t1))))

         (∀t0.(∀t1.(∀t2.((t0 → t1) → (t2 → t0) → t2 → t1)))))))

(deftest test-all-negfundamentals-neglambdas-neghigher-negorder-lambdas-negfunction-application

  (is (= (∀t0.(∀t1.((t0 → t1) → t0 → t1)))

         (∀t0.(∀t1.((t0 → t1) → t0 → t1))))))

(deftest test-all-negfundamentals-neglambdas-neghigher-negorder-lambdas-negcurried-function

  (is (= (∀t0.(boolean → t0 → t0 → t0))

         (∀t0.(boolean → t0 → t0 → t0)))))

;; Applications

;; Simple function applications

(deftest test-all-negfundamentals-negapplications-negsimple-function-applications-negidentity-application

  (is (= int32

         int32)))

(deftest test-all-negfundamentals-negapplications-negsimple-function-applications-negprimitive-application

  (is (= int32

         int32)))

(deftest test-all-negfundamentals-negapplications-negsimple-function-applications-negstring-concatenation

  (is (= string

         string)))

;; Partial applications

(deftest test-all-negfundamentals-negapplications-negpartial-applications-negpartially-applied-add

  (is (= (int32 → int32)

         (int32 → int32))))

(deftest test-all-negfundamentals-negapplications-negpartial-applications-negpartially-applied-string-cat

  (is (= (string → string)

         (string → string))))

;; Higher-order applications

(deftest test-all-negfundamentals-negapplications-neghigher-negorder-applications-negapply-function-to-function

  (is (= int32

         int32)))

(deftest test-all-negfundamentals-negapplications-neghigher-negorder-applications-negfunction-composition

  (is (= int32

         int32)))

;; Polymorphic applications

(deftest test-all-negfundamentals-negapplications-negpolymorphic-applications-negpolymorphic-identity

  (is (= (int32, string)

         (int32, string))))

(deftest test-all-negfundamentals-negapplications-negpolymorphic-applications-negpolymorphic-const

  (is (= string

         string)))

(deftest test-all-negfundamentals-negapplications-negpolymorphic-applications-negpolymorphic-flip

  (is (= string

         string)))

;; Applications in complex contexts

(deftest test-all-negfundamentals-negapplications-negapplications-in-complex-contexts-negapplication-in-tuple

  (is (= (int32, string)

         (int32, string))))

(deftest test-all-negfundamentals-negapplications-negapplications-in-complex-contexts-negapplication-in-record

  (is (= Person

         Person)))

(deftest test-all-negfundamentals-negapplications-negapplications-in-complex-contexts-negapplication-in-let-binding

  (is (= int32

         int32)))

(deftest test-all-negfundamentals-negapplications-negapplications-in-complex-contexts-negnested-applications

  (is (= int32

         int32)))

;; Applications with complex arguments

(deftest test-all-negfundamentals-negapplications-negapplications-with-complex-arguments-negapplication-with-record-argument

  (is (= string

         string)))

(deftest test-all-negfundamentals-negapplications-negapplications-with-complex-arguments-negapplication-with-list-argument

  (is (= string

         string)))

;; Let terms

;; Simple let bindings

(deftest test-all-negfundamentals-neglet-terms-negsimple-let-bindings-negsingle-binding

  (is (= int32

         int32)))

(deftest test-all-negfundamentals-neglet-terms-negsimple-let-bindings-negmultiple-bindings

  (is (= (int32, string)

         (int32, string))))

;; Let terms with shadowing

(deftest test-all-negfundamentals-neglet-terms-neglet-terms-with-shadowing-neglambda-parameter-shadowing-let-binding

  (is (= (∀t0.(t0 → t0))

         (∀t0.(t0 → t0)))))

(deftest test-all-negfundamentals-neglet-terms-neglet-terms-with-shadowing-negnested-lambda-shadowing

  (is (= (∀t0.(∀t1.(t0 → t1 → (t1, t0))))

         (∀t0.(∀t1.(t0 → t1 → (t1, t0)))))))

(deftest test-all-negfundamentals-neglet-terms-neglet-terms-with-shadowing-negmultiple-levels-of-let-shadowing

  (is (= boolean

         boolean)))

(deftest test-all-negfundamentals-neglet-terms-neglet-terms-with-shadowing-neglet-shadowing-with-lambda-and-reference-to-outer-binding

  (is (= (∀t0.(t0 → (t0, int32)))

         (∀t0.(t0 → (t0, int32))))))

;; Recursive bindings

(deftest test-all-negfundamentals-neglet-terms-negrecursive-bindings-negsimple-arithmetic-recursion

  (is (= int32

         int32)))

;; Mutual recursion

(deftest test-all-negfundamentals-neglet-terms-negmutual-recursion-negmutually-recursive-data

  (is (= (BuddyListA @ int32)

         (BuddyListA @ int32))))

(deftest test-all-negfundamentals-neglet-terms-negmutual-recursion-neg-monomorphic-mutually-recursive-functions

  (is (= int32

         int32)))

;; Nested let terms

(deftest test-all-negfundamentals-neglet-terms-negnested-let-terms-negmonomorphic-nesting

  (is (= int32

         int32)))

(deftest test-all-negfundamentals-neglet-terms-negnested-let-terms-negpolymorphic-nesting

  (is (= string

         string)))

(deftest test-all-negfundamentals-neglet-terms-negnested-let-terms-negvariable-capture-avoidance

  (is (= (∀t0.(t0 → t0))

         (∀t0.(t0 → t0)))))

(deftest test-all-negfundamentals-neglet-terms-negnested-let-terms-negsimple-let-in-lambda

  (is (= (∀t0.(t0 → t0))

         (∀t0.(t0 → t0)))))

;; Let with complex expressions

(deftest test-all-negfundamentals-neglet-terms-neglet-with-complex-expressions-neglet-in-record

  (is (= Person

         Person)))

(deftest test-all-negfundamentals-neglet-terms-neglet-with-complex-expressions-neglet-in-function-application

  (is (= int32

         int32)))

(deftest test-all-negfundamentals-neglet-terms-neglet-with-complex-expressions-negpolymorphic-let-binding

  (is (= (int32, string)

         (int32, string))))

(deftest test-all-negfundamentals-neglet-terms-neglet-with-complex-expressions-negcomposition

  (is (= int32

         int32)))

;; Primitives

;; Nullary primitives

(deftest test-all-negfundamentals-negprimitives-negnullary-primitives-negempty-map

  (is (= (∀t0.(∀t1.map<t0, t1>))

         (∀t0.(∀t1.map<t0, t1>)))))

(deftest test-all-negfundamentals-negprimitives-negnullary-primitives-negempty-set

  (is (= (∀t0.set<t0>)

         (∀t0.set<t0>))))

;; Unary primitives

(deftest test-all-negfundamentals-negprimitives-negunary-primitives-neglists-head

  (is (= (∀t0.(list<t0> → t0))

         (∀t0.(list<t0> → t0)))))

(deftest test-all-negfundamentals-negprimitives-negunary-primitives-negmath-neg

  (is (= (int32 → int32)

         (int32 → int32))))

(deftest test-all-negfundamentals-negprimitives-negunary-primitives-neglogic-not

  (is (= (boolean → boolean)

         (boolean → boolean))))

;; Binary primitives

(deftest test-all-negfundamentals-negprimitives-negbinary-primitives-negmath-add

  (is (= (int32 → int32 → int32)

         (int32 → int32 → int32))))

(deftest test-all-negfundamentals-negprimitives-negbinary-primitives-neglists-cons

  (is (= (∀t0.(t0 → list<t0> → list<t0>))

         (∀t0.(t0 → list<t0> → list<t0>)))))

(deftest test-all-negfundamentals-negprimitives-negbinary-primitives-negmaps-insert

  (is (= (∀t0.(∀t1.(t0 → t1 → map<t0, t1> → map<t0, t1>)))

         (∀t0.(∀t1.(t0 → t1 → map<t0, t1> → map<t0, t1>))))))

;; Ternary primitives

(deftest test-all-negfundamentals-negprimitives-negternary-primitives-neglogic-ifelse

  (is (= (∀t0.(boolean → t0 → t0 → t0))

         (∀t0.(boolean → t0 → t0 → t0)))))

(deftest test-all-negfundamentals-negprimitives-negternary-primitives-neglists-foldl

  (is (= (∀t0.(∀t1.((t0 → t1 → t0) → t0 → list<t1> → t0)))

         (∀t0.(∀t1.((t0 → t1 → t0) → t0 → list<t1> → t0))))))

;; Monomorphic vs polymorphic

(deftest test-all-negfundamentals-negprimitives-negmonomorphic-vs-polymorphic-negmonomorphic-math

  (is (= (int32 → int32 → int32)

         (int32 → int32 → int32))))

(deftest test-all-negfundamentals-negprimitives-negmonomorphic-vs-polymorphic-negpolymorphic-identity

  (is (= (∀t0.(t0 → t0))

         (∀t0.(t0 → t0)))))

(deftest test-all-negfundamentals-negprimitives-negmonomorphic-vs-polymorphic-negpolymorphic-map

  (is (= (∀t0.(∀t1.((t0 → t1) → list<t0> → list<t1>)))

         (∀t0.(∀t1.((t0 → t1) → list<t0> → list<t1>))))))

;; Higher-order primitives

(deftest test-all-negfundamentals-negprimitives-neghigher-negorder-primitives-neglists-map-function

  (is (= (list<int32> → list<int32>)

         (list<int32> → list<int32>))))

(deftest test-all-negfundamentals-negprimitives-neghigher-negorder-primitives-neglists-filter

  (is (= (∀t0.((t0 → boolean) → list<t0> → list<t0>))

         (∀t0.((t0 → boolean) → list<t0> → list<t0>)))))

(deftest test-all-negfundamentals-negprimitives-neghigher-negorder-primitives-negoptionals-maybe

  (is (= (∀t0.(∀t1.(t0 → (t1 → t0) → maybe<t1> → t0)))

         (∀t0.(∀t1.(t0 → (t1 → t0) → maybe<t1> → t0))))))

;; Primitives in complex contexts

(deftest test-all-negfundamentals-negprimitives-negprimitives-in-complex-contexts-negprimitive-composition

  (is (= list<int32>

         list<int32>)))

(deftest test-all-negfundamentals-negprimitives-negprimitives-in-complex-contexts-negnested-higher-negorder

  (is (= list<list<int32>>

         list<list<int32>>)))

;; Nominal types

;; Records

;; Monomorphic records

(deftest test-all-negnominal-types-negrecords-negmonomorphic-records-neglatlon-record

  (is (= LatLon

         LatLon)))

(deftest test-all-negnominal-types-negrecords-negmonomorphic-records-neglatlon-with-variable

  (is (= (float32 → LatLon)

         (float32 → LatLon))))

(deftest test-all-negnominal-types-negrecords-negmonomorphic-records-negperson-record

  (is (= Person

         Person)))

(deftest test-all-negnominal-types-negrecords-negmonomorphic-records-negempty-record

  (is (= Unit

         Unit)))

(deftest test-all-negnominal-types-negrecords-negmonomorphic-records-negperson-with-variables

  (is (= (string → int32 → Person)

         (string → int32 → Person))))

;; Polymorphic records

(deftest test-all-negnominal-types-negrecords-negpolymorphic-records-neglatlon-poly-float

  (is (= (LatLonPoly @ float32)

         (LatLonPoly @ float32))))

(deftest test-all-negnominal-types-negrecords-negpolymorphic-records-neglatlon-poly-int64

  (is (= (LatLonPoly @ int64)

         (LatLonPoly @ int64))))

(deftest test-all-negnominal-types-negrecords-negpolymorphic-records-neglatlon-poly-variable

  (is (= (∀t0.(t0 → (LatLonPoly @ t0)))

         (∀t0.(t0 → (LatLonPoly @ t0))))))

(deftest test-all-negnominal-types-negrecords-negpolymorphic-records-negbuddylist-string

  (is (= (BuddyListA @ string)

         (BuddyListA @ string))))

(deftest test-all-negnominal-types-negrecords-negpolymorphic-records-negbuddylist-variable

  (is (= (∀t0.(t0 → (BuddyListA @ t0)))

         (∀t0.(t0 → (BuddyListA @ t0))))))

;; Records in complex contexts

(deftest test-all-negnominal-types-negrecords-negrecords-in-complex-contexts-negrecords-in-tuple

  (is (= (Person, LatLon)

         (Person, LatLon))))

(deftest test-all-negnominal-types-negrecords-negrecords-in-complex-contexts-negpoly-records-in-tuple

  (is (= ((LatLonPoly @ int32), (BuddyListA @ string))

         ((LatLonPoly @ int32), (BuddyListA @ string)))))

(deftest test-all-negnominal-types-negrecords-negrecords-in-complex-contexts-negrecursive-record

  (is (= IntList

         IntList)))

;; Multi-parameter polymorphic records

(deftest test-all-negnominal-types-negrecords-negmulti-negparameter-polymorphic-records-negtriple-with-three-monomorphic-types

  (is (= (Triple @ int32 @ string @ boolean)

         (Triple @ int32 @ string @ boolean))))

(deftest test-all-negnominal-types-negrecords-negmulti-negparameter-polymorphic-records-negtriple-with-personorsomething-containing-map

  (is (= (∀t0.(∀t1.(t0 → t1 → (Triple @ string @ (PersonOrSomething @ map<t0, t1>) @ int32))))

         (∀t0.(∀t1.(t0 → t1 → (Triple @ string @ (PersonOrSomething @ map<t0, t1>) @ int32)))))))

;; Unions

;; Simple union injections

(deftest test-all-negnominal-types-negunions-negsimple-union-injections-neginject-into-comparison-lessthan-variant

  (is (= Comparison

         Comparison)))

(deftest test-all-negnominal-types-negunions-negsimple-union-injections-neginject-into-comparison-equalto-variant

  (is (= Comparison

         Comparison)))

(deftest test-all-negnominal-types-negunions-negsimple-union-injections-neginject-into-comparison-greaterthan-variant

  (is (= Comparison

         Comparison)))

;; Union injections with data

(deftest test-all-negnominal-types-negunions-negunion-injections-with-data-neginject-into-number-int-variant

  (is (= Number

         Number)))

(deftest test-all-negnominal-types-negunions-negunion-injections-with-data-neginject-into-number-float-variant

  (is (= Number

         Number)))

(deftest test-all-negnominal-types-negunions-negunion-injections-with-data-neginject-into-timestamp-unixtimemillis-variant

  (is (= Timestamp

         Timestamp)))

(deftest test-all-negnominal-types-negunions-negunion-injections-with-data-neginject-into-timestamp-date-variant

  (is (= Timestamp

         Timestamp)))

;; Polymorphic union injections

(deftest test-all-negnominal-types-negunions-negpolymorphic-union-injections-neginject-person-into-personorsomething

  (is (= (∀t0.(PersonOrSomething @ t0))

         (∀t0.(PersonOrSomething @ t0)))))

(deftest test-all-negnominal-types-negunions-negpolymorphic-union-injections-neginject-string-into-personorsomething-other-variant

  (is (= (PersonOrSomething @ string)

         (PersonOrSomething @ string))))

(deftest test-all-negnominal-types-negunions-negpolymorphic-union-injections-neginject-int-into-personorsomething-other-variant

  (is (= (PersonOrSomething @ int32)

         (PersonOrSomething @ int32))))

;; Polymorphic recursive union injections

(deftest test-all-negnominal-types-negunions-negpolymorphic-recursive-union-injections-neginject-boolean-into-unionpolymorphicrecursive

  (is (= (∀t0.(UnionPolymorphicRecursive @ t0))

         (∀t0.(UnionPolymorphicRecursive @ t0)))))

(deftest test-all-negnominal-types-negunions-negpolymorphic-recursive-union-injections-neginject-string-value-into-unionpolymorphicrecursive

  (is (= (UnionPolymorphicRecursive @ string)

         (UnionPolymorphicRecursive @ string))))

(deftest test-all-negnominal-types-negunions-negpolymorphic-recursive-union-injections-neginject-int-value-into-unionpolymorphicrecursive

  (is (= (UnionPolymorphicRecursive @ int32)

         (UnionPolymorphicRecursive @ int32))))

;; Polymorphic unions from lambda

(deftest test-all-negnominal-types-negunions-negpolymorphic-unions-from-lambda-neglambda-creating-personorsomething-other-variant

  (is (= (∀t0.(t0 → (PersonOrSomething @ t0)))

         (∀t0.(t0 → (PersonOrSomething @ t0))))))

(deftest test-all-negnominal-types-negunions-negpolymorphic-unions-from-lambda-neglambda-creating-unionpolymorphicrecursive-value-variant

  (is (= (∀t0.(t0 → (UnionPolymorphicRecursive @ t0)))

         (∀t0.(t0 → (UnionPolymorphicRecursive @ t0))))))

;; Unions in complex contexts

(deftest test-all-negnominal-types-negunions-negunions-in-complex-contexts-negunion-in-tuple

  (is (= (Number, string)

         (Number, string))))

(deftest test-all-negnominal-types-negunions-negunions-in-complex-contexts-negunion-in-list

  (is (= list<Number>

         list<Number>)))

(deftest test-all-negnominal-types-negunions-negunions-in-complex-contexts-negpolymorphic-union-in-let-binding

  (is (= (PersonOrSomething @ string)

         (PersonOrSomething @ string))))

;; Multi-parameter polymorphic injections

(deftest test-all-negnominal-types-negunions-negmulti-negparameter-polymorphic-injections-negeither-left-with-int

  (is (= (∀t0.(Either @ int32 @ t0))

         (∀t0.(Either @ int32 @ t0)))))

(deftest test-all-negnominal-types-negunions-negmulti-negparameter-polymorphic-injections-negeither-right-with-string

  (is (= (∀t0.(Either @ t0 @ string))

         (∀t0.(Either @ t0 @ string)))))

(deftest test-all-negnominal-types-negunions-negmulti-negparameter-polymorphic-injections-negeither-containing-latlonpoly-in-list

  (is (= (∀t0.(Either @ t0 @ list<(LatLonPoly @ int32)>))

         (∀t0.(Either @ t0 @ list<(LatLonPoly @ int32)>)))))

(deftest test-all-negnominal-types-negunions-negmulti-negparameter-polymorphic-injections-negeither-in-triple-in-map-with-shared-type-variables

  (is (= (∀t0.(∀t1.(∀t2.(∀t3.(∀t4.(∀t5.(t0 → t1 → t2 → map<string, (Triple @ (Either @ t0 @ t3) @ (Either @ t0 @ t4) @ (Either @ t5 @ t1))>)))))))

         (∀t0.(∀t1.(∀t2.(∀t3.(∀t4.(∀t5.(t0 → t1 → t2 → map<string, (Triple @ (Either @ t0 @ t3) @ (Either @ t0 @ t4) @ (Either @ t5 @ t1))>))))))))))

;; Wrapped terms

;; Monomorphic wrapped terms

(deftest test-all-negnominal-types-negwrapped-terms-negmonomorphic-wrapped-terms-negstring-alias

  (is (= StringAlias

         StringAlias)))

(deftest test-all-negnominal-types-negwrapped-terms-negmonomorphic-wrapped-terms-negwrapped-integer

  (is (= StringAlias

         StringAlias)))

(deftest test-all-negnominal-types-negwrapped-terms-negmonomorphic-wrapped-terms-negwrapped-in-tuple

  (is (= (StringAlias, string)

         (StringAlias, string))))

;; Polymorphic wrapped terms

(deftest test-all-negnominal-types-negwrapped-terms-negpolymorphic-wrapped-terms-negpolymorphic-wrapper-with-int

  (is (= (PolymorphicWrapper @ int32)

         (PolymorphicWrapper @ int32))))

(deftest test-all-negnominal-types-negwrapped-terms-negpolymorphic-wrapped-terms-negpolymorphic-wrapper-with-string

  (is (= (PolymorphicWrapper @ string)

         (PolymorphicWrapper @ string))))

(deftest test-all-negnominal-types-negwrapped-terms-negpolymorphic-wrapped-terms-negpolymorphic-wrapper-from-lambda

  (is (= (∀t0.(t0 → (PolymorphicWrapper @ t0)))

         (∀t0.(t0 → (PolymorphicWrapper @ t0))))))

;; Wrapped terms in complex contexts

(deftest test-all-negnominal-types-negwrapped-terms-negwrapped-terms-in-complex-contexts-negwrapped-in-record

  (is (= Person

         Person)))

(deftest test-all-negnominal-types-negwrapped-terms-negwrapped-terms-in-complex-contexts-negwrapped-in-let-binding

  (is (= StringAlias

         StringAlias)))

(deftest test-all-negnominal-types-negwrapped-terms-negwrapped-terms-in-complex-contexts-negwrapped-in-list

  (is (= list<StringAlias>

         list<StringAlias>)))

;; Nested wrapped terms

(deftest test-all-negnominal-types-negwrapped-terms-negnested-wrapped-terms-negwrapped-tuple

  (is (= (PolymorphicWrapper @ (int32, string))

         (PolymorphicWrapper @ (int32, string)))))

(deftest test-all-negnominal-types-negwrapped-terms-negnested-wrapped-terms-negwrapped-optional

  (is (= (PolymorphicWrapper @ maybe<int32>)

         (PolymorphicWrapper @ maybe<int32>))))

(deftest test-all-negnominal-types-negwrapped-terms-negnested-wrapped-terms-negwrapped-map

  (is (= (PolymorphicWrapper @ map<string, int32>)

         (PolymorphicWrapper @ map<string, int32>))))

;; Multiple wrapping levels

(deftest test-all-negnominal-types-negwrapped-terms-negmultiple-wrapping-levels-negwrapped-in-optional

  (is (= maybe<StringAlias>

         maybe<StringAlias>)))

(deftest test-all-negnominal-types-negwrapped-terms-negmultiple-wrapping-levels-neglist-of-wrapped-polymorphic

  (is (= list<(PolymorphicWrapper @ int32)>

         list<(PolymorphicWrapper @ int32)>)))

;; Multi-parameter polymorphic wrappers

(deftest test-all-negnominal-types-negwrapped-terms-negmulti-negparameter-polymorphic-wrappers-negsymmetric-triple-wrapping-simple-types

  (is (= (SymmetricTriple @ int32 @ string)

         (SymmetricTriple @ int32 @ string))))

(deftest test-all-negnominal-types-negwrapped-terms-negmulti-negparameter-polymorphic-wrappers-negsymmetric-triple-from-lambda

  (is (= (∀t0.(∀t1.(t0 → t1 → t0 → (SymmetricTriple @ t0 @ t1))))

         (∀t0.(∀t1.(t0 → t1 → t0 → (SymmetricTriple @ t0 @ t1)))))))

(deftest test-all-negnominal-types-negwrapped-terms-negmulti-negparameter-polymorphic-wrappers-negsymmetric-triple-with-nested-polymorphic-types-and-foldl

  (is (= (list<int32> → list<int32> → (SymmetricTriple @ int32 @ list<list<int32>>))

         (list<int32> → list<int32> → (SymmetricTriple @ int32 @ list<list<int32>>)))))

;; Eliminations

;; Record eliminations

;; Simple record projections

(deftest test-all-negnominal-types-negeliminations-negrecord-eliminations-negsimple-record-projections-negproject-firstname-from-person

  (is (= (Person → string)

         (Person → string))))

(deftest test-all-negnominal-types-negeliminations-negrecord-eliminations-negsimple-record-projections-negproject-lastname-from-person

  (is (= (Person → string)

         (Person → string))))

(deftest test-all-negnominal-types-negeliminations-negrecord-eliminations-negsimple-record-projections-negproject-age-from-person

  (is (= (Person → int32)

         (Person → int32))))

(deftest test-all-negnominal-types-negeliminations-negrecord-eliminations-negsimple-record-projections-negproject-lat-from-latlon

  (is (= (LatLon → float32)

         (LatLon → float32))))

(deftest test-all-negnominal-types-negeliminations-negrecord-eliminations-negsimple-record-projections-negproject-lon-from-latlon

  (is (= (LatLon → float32)

         (LatLon → float32))))

;; Record projections applied to records

(deftest test-all-negnominal-types-negeliminations-negrecord-eliminations-negrecord-projections-applied-to-records-negproject-firstname-applied-to-person-record

  (is (= string

         string)))

(deftest test-all-negnominal-types-negeliminations-negrecord-eliminations-negrecord-projections-applied-to-records-negproject-age-applied-to-person-record

  (is (= int32

         int32)))

(deftest test-all-negnominal-types-negeliminations-negrecord-eliminations-negrecord-projections-applied-to-records-negproject-lat-applied-to-latlon-record

  (is (= float32

         float32)))

;; Polymorphic record projections

(deftest test-all-negnominal-types-negeliminations-negrecord-eliminations-negpolymorphic-record-projections-negproject-lat-from-polymorphic-latlonpoly

  (is (= (∀t0.((LatLonPoly @ t0) → t0))

         (∀t0.((LatLonPoly @ t0) → t0)))))

(deftest test-all-negnominal-types-negeliminations-negrecord-eliminations-negpolymorphic-record-projections-negproject-lon-from-polymorphic-latlonpoly

  (is (= (∀t0.((LatLonPoly @ t0) → t0))

         (∀t0.((LatLonPoly @ t0) → t0)))))

(deftest test-all-negnominal-types-negeliminations-negrecord-eliminations-negpolymorphic-record-projections-negproject-head-from-buddylista

  (is (= (∀t0.((BuddyListA @ t0) → t0))

         (∀t0.((BuddyListA @ t0) → t0)))))

(deftest test-all-negnominal-types-negeliminations-negrecord-eliminations-negpolymorphic-record-projections-negproject-tail-from-buddylista

  (is (= (∀t0.((BuddyListA @ t0) → maybe<(BuddyListB @ t0)>))

         (∀t0.((BuddyListA @ t0) → maybe<(BuddyListB @ t0)>)))))

;; Polymorphic record projections applied

(deftest test-all-negnominal-types-negeliminations-negrecord-eliminations-negpolymorphic-record-projections-applied-negproject-lat-from-latlonpoly-with-int32

  (is (= int32

         int32)))

(deftest test-all-negnominal-types-negeliminations-negrecord-eliminations-negpolymorphic-record-projections-applied-negproject-lon-from-latlonpoly-with-float64

  (is (= float64

         float64)))

(deftest test-all-negnominal-types-negeliminations-negrecord-eliminations-negpolymorphic-record-projections-applied-negproject-head-from-buddylista-with-string

  (is (= string

         string)))

;; Record projections with variables

(deftest test-all-negnominal-types-negeliminations-negrecord-eliminations-negrecord-projections-with-variables-negproject-from-lambda-parameter

  (is (= (Person → string)

         (Person → string))))

(deftest test-all-negnominal-types-negeliminations-negrecord-eliminations-negrecord-projections-with-variables-negproject-from-polymorphic-lambda-parameter

  (is (= (∀t0.((LatLonPoly @ t0) → t0))

         (∀t0.((LatLonPoly @ t0) → t0)))))

(deftest test-all-negnominal-types-negeliminations-negrecord-eliminations-negrecord-projections-with-variables-negmultiple-projections-from-same-record

  (is (= (Person → (string, string))

         (Person → (string, string)))))

;; Record projections in complex contexts

(deftest test-all-negnominal-types-negeliminations-negrecord-eliminations-negrecord-projections-in-complex-contexts-negprojection-in-let-binding

  (is (= string

         string)))

(deftest test-all-negnominal-types-negeliminations-negrecord-eliminations-negrecord-projections-in-complex-contexts-negprojection-in-tuple

  (is (= ((Person → string), (Person → int32))

         ((Person → string), (Person → int32)))))

(deftest test-all-negnominal-types-negeliminations-negrecord-eliminations-negrecord-projections-in-complex-contexts-negprojection-in-list

  (is (= list<(Person → string)>

         list<(Person → string)>)))

;; Multi-parameter polymorphic projections

(deftest test-all-negnominal-types-negeliminations-negrecord-eliminations-negmulti-negparameter-polymorphic-projections-negproject-first-from-triple

  (is (= (∀t0.(∀t1.(∀t2.((Triple @ t0 @ t1 @ t2) → t0))))

         (∀t0.(∀t1.(∀t2.((Triple @ t0 @ t1 @ t2) → t0)))))))

(deftest test-all-negnominal-types-negeliminations-negrecord-eliminations-negmulti-negparameter-polymorphic-projections-negproject-second-from-triple-applied

  (is (= string

         string)))

(deftest test-all-negnominal-types-negeliminations-negrecord-eliminations-negmulti-negparameter-polymorphic-projections-negproject-from-triple-and-use-second-field-which-is-another-polymorphic-record

  (is (= (∀t0.(∀t1.(∀t2.(∀t3.((Triple @ t0 @ (PersonOrSomething @ map<t1, t2>) @ t3) → t1 → maybe<t2>)))))

         (∀t0.(∀t1.(∀t2.(∀t3.((Triple @ t0 @ (PersonOrSomething @ map<t1, t2>) @ t3) → t1 → maybe<t2>))))))))

;; Higher-order record projections

(deftest test-all-negnominal-types-negeliminations-negrecord-eliminations-neghigher-negorder-record-projections-negmap-projection-over-list-of-records

  (is (= list<string>

         list<string>)))

(deftest test-all-negnominal-types-negeliminations-negrecord-eliminations-neghigher-negorder-record-projections-negmap-polymorphic-projection

  (is (= list<int32>

         list<int32>)))

(deftest test-all-negnominal-types-negeliminations-negrecord-eliminations-neghigher-negorder-record-projections-negfilter-using-projection

  (is (= list<Person>

         list<Person>)))

;; Recursive record projections

(deftest test-all-negnominal-types-negeliminations-negrecord-eliminations-negrecursive-record-projections-negnested-projection-from-recursive-record

  (is (= (IntList → int32)

         (IntList → int32))))

;; Record projections with mutual recursion

(deftest test-all-negnominal-types-negeliminations-negrecord-eliminations-negrecord-projections-with-mutual-recursion-negproject-head-from-buddylista

  (is (= (∀t0.((BuddyListA @ t0) → t0))

         (∀t0.((BuddyListA @ t0) → t0)))))

(deftest test-all-negnominal-types-negeliminations-negrecord-eliminations-negrecord-projections-with-mutual-recursion-negproject-tail-from-buddylistb

  (is (= (∀t0.((BuddyListB @ t0) → maybe<(BuddyListA @ t0)>))

         (∀t0.((BuddyListB @ t0) → maybe<(BuddyListA @ t0)>)))))

(deftest test-all-negnominal-types-negeliminations-negrecord-eliminations-negrecord-projections-with-mutual-recursion-negchained-projections-across-mutual-recursion

  (is (= (∀t0.((BuddyListA @ t0) → maybe<(BuddyListB @ t0)>))

         (∀t0.((BuddyListA @ t0) → maybe<(BuddyListB @ t0)>)))))

;; Projections with variables

(deftest test-all-negnominal-types-negeliminations-negrecord-eliminations-negprojections-with-variables-negproject-from-lambda-parameter

  (is (= (Person → string)

         (Person → string))))

(deftest test-all-negnominal-types-negeliminations-negrecord-eliminations-negprojections-with-variables-negproject-from-polymorphic-lambda-parameter

  (is (= (∀t0.((LatLonPoly @ t0) → t0))

         (∀t0.((LatLonPoly @ t0) → t0)))))

(deftest test-all-negnominal-types-negeliminations-negrecord-eliminations-negprojections-with-variables-negmultiple-projections-from-same-record

  (is (= (Person → (string, string))

         (Person → (string, string)))))

;; Union eliminations

;; Simple unit inject eliminations

(deftest test-all-negnominal-types-negeliminations-negunion-eliminations-negsimple-unit-inject-eliminations-negmatch-comparison-with-all-cases

  (is (= (Comparison → string)

         (Comparison → string))))

(deftest test-all-negnominal-types-negeliminations-negunion-eliminations-negsimple-unit-inject-eliminations-negmatch-comparison-returning-int32

  (is (= (Comparison → int32)

         (Comparison → int32))))

(deftest test-all-negnominal-types-negeliminations-negunion-eliminations-negsimple-unit-inject-eliminations-negmatch-applied-to-comparison-variant

  (is (= string

         string)))

;; Union eliminations with data

(deftest test-all-negnominal-types-negeliminations-negunion-eliminations-negunion-eliminations-with-data-negmatch-number-extracting-int-values

  (is (= (Number → int32)

         (Number → int32))))

(deftest test-all-negnominal-types-negeliminations-negunion-eliminations-negunion-eliminations-with-data-negmatch-number-converting-to-string

  (is (= (Number → string)

         (Number → string))))

(deftest test-all-negnominal-types-negeliminations-negunion-eliminations-negunion-eliminations-with-data-negmatch-number-applied-to-int-variant

  (is (= int32

         int32)))

(deftest test-all-negnominal-types-negeliminations-negunion-eliminations-negunion-eliminations-with-data-negmatch-timestamp-with-mixed-data-types

  (is (= (Timestamp → string)

         (Timestamp → string))))

;; Polymorphic union eliminations

;; Simple polymorphic unions

(deftest test-all-negnominal-types-negeliminations-negunion-eliminations-negpolymorphic-union-eliminations-negsimple-polymorphic-unions-negmatch-personorsomething-with-string

  (is (= ((PersonOrSomething @ string) → string)

         ((PersonOrSomething @ string) → string))))

(deftest test-all-negnominal-types-negeliminations-negunion-eliminations-negpolymorphic-union-eliminations-negsimple-polymorphic-unions-negmatch-personorsomething-instantiated-with-string

  (is (= string

         string)))

;; using UnionPolymorphicRecursive

(deftest test-all-negnominal-types-negeliminations-negunion-eliminations-negpolymorphic-union-eliminations-negusing-unionpolymorphicrecursive-negnon-negapplied-unionpolymorphicrecursive

  (is (= ((UnionPolymorphicRecursive @ int32) → string)

         ((UnionPolymorphicRecursive @ int32) → string))))

(deftest test-all-negnominal-types-negeliminations-negunion-eliminations-negpolymorphic-union-eliminations-negusing-unionpolymorphicrecursive-negapplied-unionpolymorphicrecursive-with-int32

  (is (= string

         string)))

(deftest test-all-negnominal-types-negeliminations-negunion-eliminations-negpolymorphic-union-eliminations-negusing-unionpolymorphicrecursive-negapplied-unionpolymorphicrecursive-with-int32-in-lambda

  (is (= ((UnionPolymorphicRecursive @ int32) → string)

         ((UnionPolymorphicRecursive @ int32) → string))))

(deftest test-all-negnominal-types-negeliminations-negunion-eliminations-negpolymorphic-union-eliminations-negusing-unionpolymorphicrecursive-negapplied-generic-unionpolymorphicrecursive-in-lambda

  (is (= (∀t0.((UnionPolymorphicRecursive @ t0) → string))

         (∀t0.((UnionPolymorphicRecursive @ t0) → string)))))

;; Using kernel types

(deftest test-all-negnominal-types-negeliminations-negunion-eliminations-negpolymorphic-union-eliminations-negusing-kernel-types-negcase-statement-on-coderdirection-applied-to-argument

  (is (= (∀t0.(hydra.coders.CoderDirection → (hydra.coders.Coder @ t0 @ t0) → hydra.context.Context → t0 → either<(hydra.context.InContext @ hydra.errors.Error), t0>))

         (∀t0.(hydra.coders.CoderDirection → (hydra.coders.Coder @ t0 @ t0) → hydra.context.Context → t0 → either<(hydra.context.InContext @ hydra.errors.Error), t0>)))))

;; Union eliminations with defaults

(deftest test-all-negnominal-types-negeliminations-negunion-eliminations-negunion-eliminations-with-defaults-negmatch-comparison-with-default-case

  (is (= (Comparison → string)

         (Comparison → string))))

(deftest test-all-negnominal-types-negeliminations-negunion-eliminations-negunion-eliminations-with-defaults-negmatch-number-with-default-case

  (is (= (Number → int32)

         (Number → int32))))

(deftest test-all-negnominal-types-negeliminations-negunion-eliminations-negunion-eliminations-with-defaults-negmatch-unionmonomorphic-with-default

  (is (= (UnionMonomorphic → string)

         (UnionMonomorphic → string))))

;; Nested union eliminations

(deftest test-all-negnominal-types-negeliminations-negunion-eliminations-negnested-union-eliminations-negnested-match-statements

  (is (= ((PersonOrSomething @ Number) → string)

         ((PersonOrSomething @ Number) → string))))

(deftest test-all-negnominal-types-negeliminations-negunion-eliminations-negnested-union-eliminations-negmatch-in-tuple

  (is (= ((Comparison → int32), string)

         ((Comparison → int32), string))))

;; Union eliminations in complex contexts

(deftest test-all-negnominal-types-negeliminations-negunion-eliminations-negunion-eliminations-in-complex-contexts-negmatch-in-let-binding

  (is (= (Comparison → string)

         (Comparison → string))))

(deftest test-all-negnominal-types-negeliminations-negunion-eliminations-negunion-eliminations-in-complex-contexts-negmatch-in-record

  (is (= Person

         Person)))

(deftest test-all-negnominal-types-negeliminations-negunion-eliminations-negunion-eliminations-in-complex-contexts-negmatch-with-polymorphic-result-in-list

  (is (= list<int32>

         list<int32>)))

;; Multi-parameter polymorphic case statements

(deftest test-all-negnominal-types-negeliminations-negunion-eliminations-negmulti-negparameter-polymorphic-case-statements-negcase-either-converting-both-to-string

  (is (= ((Either @ int32 @ float32) → string)

         ((Either @ int32 @ float32) → string))))

(deftest test-all-negnominal-types-negeliminations-negunion-eliminations-negmulti-negparameter-polymorphic-case-statements-negcase-either-applied-to-injection

  (is (= int32

         int32)))

(deftest test-all-negnominal-types-negeliminations-negunion-eliminations-negmulti-negparameter-polymorphic-case-statements-negcase-either-with-triple-and-nested-projections

  (is (= (∀t0.(∀t1.(∀t2.(∀t3.(∀t4.((Triple @ t0 @ (Either @ (LatLonPoly @ t1) @ (Triple @ t1 @ t2 @ t3)) @ t4) → t1))))))

         (∀t0.(∀t1.(∀t2.(∀t3.(∀t4.((Triple @ t0 @ (Either @ (LatLonPoly @ t1) @ (Triple @ t1 @ t2 @ t3)) @ t4) → t1)))))))))

(deftest test-all-negnominal-types-negeliminations-negunion-eliminations-negmulti-negparameter-polymorphic-case-statements-negcase-either-with-polymorphic-let-bindings

  (is (= ((Either @ int32 @ string) → (Either @ int32 @ int32))

         ((Either @ int32 @ string) → (Either @ int32 @ int32)))))

;; Higher-order union eliminations

(deftest test-all-negnominal-types-negeliminations-negunion-eliminations-neghigher-negorder-union-eliminations-negmap-match-over-list

  (is (= list<string>

         list<string>)))

(deftest test-all-negnominal-types-negeliminations-negunion-eliminations-neghigher-negorder-union-eliminations-negcompose-match-with-other-functions

  (is (= (Comparison → int32)

         (Comparison → int32))))

(deftest test-all-negnominal-types-negeliminations-negunion-eliminations-neghigher-negorder-union-eliminations-negmatch-in-lambda-body

  (is (= (Number → int32)

         (Number → int32))))

;; Recursive union eliminations

(deftest test-all-negnominal-types-negeliminations-negunion-eliminations-negrecursive-union-eliminations-negmatch-hydratype-recursively

  (is (= (HydraType → string)

         (HydraType → string))))

;; Wrap eliminations

;; Monomorphic unwrapping

(deftest test-all-negnominal-types-negeliminations-negwrap-eliminations-negmonomorphic-unwrapping-negunwrap-string-alias

  (is (= (StringAlias → string)

         (StringAlias → string))))

;; Polymorphic unwrapping

(deftest test-all-negnominal-types-negeliminations-negwrap-eliminations-negpolymorphic-unwrapping-negunwrap-polymorphic-wrapper

  (is (= (∀t0.((PolymorphicWrapper @ t0) → list<t0>))

         (∀t0.((PolymorphicWrapper @ t0) → list<t0>)))))

;; Unwrap eliminations in applications

(deftest test-all-negnominal-types-negeliminations-negwrap-eliminations-negunwrap-eliminations-in-applications-negunwrap-applied-to-wrapped-term

  (is (= string

         string)))

(deftest test-all-negnominal-types-negeliminations-negwrap-eliminations-negunwrap-eliminations-in-applications-negunwrap-polymorphic-applied

  (is (= list<int32>

         list<int32>)))

;; Unwrap in complex contexts

(deftest test-all-negnominal-types-negeliminations-negwrap-eliminations-negunwrap-in-complex-contexts-negunwrap-in-let-binding

  (is (= string

         string)))

(deftest test-all-negnominal-types-negeliminations-negwrap-eliminations-negunwrap-in-complex-contexts-negunwrap-in-tuple

  (is (= ((StringAlias → string), string)

         ((StringAlias → string), string))))

(deftest test-all-negnominal-types-negeliminations-negwrap-eliminations-negunwrap-in-complex-contexts-negunwrap-in-lambda

  (is (= (StringAlias → string)

         (StringAlias → string))))

;; Multi-parameter polymorphic unwrappers

(deftest test-all-negnominal-types-negeliminations-negwrap-eliminations-negmulti-negparameter-polymorphic-unwrappers-negunwrap-symmetric-triple-to-tuple

  (is (= (∀t0.(∀t1.((SymmetricTriple @ t0 @ t1) → (t0, t0))))

         (∀t0.(∀t1.((SymmetricTriple @ t0 @ t1) → (t0, t0)))))))

(deftest test-all-negnominal-types-negeliminations-negwrap-eliminations-negmulti-negparameter-polymorphic-unwrappers-negunwrap-and-collect-edges-in-set

  (is (= (∀t0.(∀t1.(set<(SymmetricTriple @ t0 @ t1)> → set<t1>)))

         (∀t0.(∀t1.(set<(SymmetricTriple @ t0 @ t1)> → set<t1>))))))

(deftest test-all-negnominal-types-negeliminations-negwrap-eliminations-negmulti-negparameter-polymorphic-unwrappers-negunwrap-with-maybe-to-handle-optional-symmetric-triple

  (is (= (∀t0.(∀t1.(maybe<(SymmetricTriple @ t0 @ t1)> → maybe<t1>)))

         (∀t0.(∀t1.(maybe<(SymmetricTriple @ t0 @ t1)> → maybe<t1>))))))

;; Chained unwrapping

(deftest test-all-negnominal-types-negeliminations-negwrap-eliminations-negchained-unwrapping-negunwrap-then-process

  (is (= (StringAlias → string)

         (StringAlias → string))))

(deftest test-all-negnominal-types-negeliminations-negwrap-eliminations-negchained-unwrapping-negunwrap-polymorphic-then-map

  (is (= ((PolymorphicWrapper @ int32) → list<int32>)

         ((PolymorphicWrapper @ int32) → list<int32>))))

;; Multiple unwrap operations

(deftest test-all-negnominal-types-negeliminations-negwrap-eliminations-negmultiple-unwrap-operations-negunwrap-different-types

  (is (= (∀t0.(StringAlias → (PolymorphicWrapper @ t0) → (string, list<t0>)))

         (∀t0.(StringAlias → (PolymorphicWrapper @ t0) → (string, list<t0>))))))
