;;; Note: this is an automatically generated file. Do not edit. -*- lexical-binding: t; coding: utf-8 -*-
;;; checking

(require 'ert)

;; Advanced

;; Annotated terms

;; Top-level annotations

(ert-deftest test-all-negadvanced-negannotated-terms-negtop-neglevel-annotations-negannotated-literal ()

  (should (equal int32 int32)))

(ert-deftest test-all-negadvanced-negannotated-terms-negtop-neglevel-annotations-negannotated-list ()

  (should (equal list<string> list<string>)))

(ert-deftest test-all-negadvanced-negannotated-terms-negtop-neglevel-annotations-negannotated-record ()

  (should (equal Person Person)))

(ert-deftest test-all-negadvanced-negannotated-terms-negtop-neglevel-annotations-negannotated-lambda ()

  (should (equal (∀t0.(t0 → t0)) (∀t0.(t0 → t0)))))

;; Nested annotations

(ert-deftest test-all-negadvanced-negannotated-terms-negnested-annotations-negannotation-within-annotation ()

  (should (equal int32 int32)))

(ert-deftest test-all-negadvanced-negannotated-terms-negnested-annotations-negannotated-terms-in-tuple ()

  (should (equal (int32, string) (int32, string))))

(ert-deftest test-all-negadvanced-negannotated-terms-negnested-annotations-negannotated-term-in-function-application ()

  (should (equal int32 int32)))

;; Annotations in complex contexts

(ert-deftest test-all-negadvanced-negannotated-terms-negannotations-in-complex-contexts-negannotated-let-binding ()

  (should (equal (int32, string) (int32, string))))

(ert-deftest test-all-negadvanced-negannotated-terms-negannotations-in-complex-contexts-negannotated-record-fields ()

  (should (equal Person Person)))

(ert-deftest test-all-negadvanced-negannotated-terms-negannotations-in-complex-contexts-negannotated-function-in-application ()

  (should (equal int32 int32)))

;; Algebraic types

;; Unit

;; Unit term

(ert-deftest test-all-negalgebraic-types-negunit-negunit-term-negunit-literal ()

  (should (equal unit unit)))

;; Unit term in polymorphic context

(ert-deftest test-all-negalgebraic-types-negunit-negunit-term-in-polymorphic-context-negunit-from-lambda ()

  (should (equal (∀t0.(t0 → unit)) (∀t0.(t0 → unit)))))

;; Pairs

;; Basic pairs

(ert-deftest test-all-negalgebraic-types-negpairs-negbasic-pairs-negpair-of-int-and-string ()

  (should (equal (int32, string) (int32, string))))

(ert-deftest test-all-negalgebraic-types-negpairs-negbasic-pairs-negpair-of-string-and-boolean ()

  (should (equal (string, boolean) (string, boolean))))

(ert-deftest test-all-negalgebraic-types-negpairs-negbasic-pairs-negpair-of-boolean-and-int ()

  (should (equal (boolean, int32) (boolean, int32))))

;; Polymorphic pairs

(ert-deftest test-all-negalgebraic-types-negpairs-negpolymorphic-pairs-negpair-from-lambda-first-element ()

  (should (equal (∀t0.(t0 → (t0, string))) (∀t0.(t0 → (t0, string))))))

(ert-deftest test-all-negalgebraic-types-negpairs-negpolymorphic-pairs-negpair-from-lambda-second-element ()

  (should (equal (∀t0.(t0 → (string, t0))) (∀t0.(t0 → (string, t0))))))

(ert-deftest test-all-negalgebraic-types-negpairs-negpolymorphic-pairs-negpair-from-two-lambdas ()

  (should (equal (∀t0.(∀t1.(t0 → t1 → (t0, t1)))) (∀t0.(∀t1.(t0 → t1 → (t0, t1)))))))

(ert-deftest test-all-negalgebraic-types-negpairs-negpolymorphic-pairs-negpair-with-repeated-variable ()

  (should (equal (∀t0.(t0 → (t0, t0))) (∀t0.(t0 → (t0, t0))))))

;; Pairs in complex contexts

(ert-deftest test-all-negalgebraic-types-negpairs-negpairs-in-complex-contexts-negpair-in-list ()

  (should (equal list<(int32, string)> list<(int32, string)>)))

(ert-deftest test-all-negalgebraic-types-negpairs-negpairs-in-complex-contexts-negpair-in-let-binding ()

  (should (equal (int32, string) (int32, string))))

;; Nested pairs

(ert-deftest test-all-negalgebraic-types-negpairs-negnested-pairs-negpair-of-pairs ()

  (should (equal ((int32, string), (boolean, int32)) ((int32, string), (boolean, int32)))))

(ert-deftest test-all-negalgebraic-types-negpairs-negnested-pairs-negpair-with-list ()

  (should (equal (list<int32>, string) (list<int32>, string))))

(ert-deftest test-all-negalgebraic-types-negpairs-negnested-pairs-neglist-of-pairs ()

  (should (equal list<(int32, string)> list<(int32, string)>)))

;; Pairs with complex types

(ert-deftest test-all-negalgebraic-types-negpairs-negpairs-with-complex-types-negpair-with-record-on-first ()

  (should (equal (Person, int32) (Person, int32))))

(ert-deftest test-all-negalgebraic-types-negpairs-negpairs-with-complex-types-negpair-with-record-on-second ()

  (should (equal (string, Person) (string, Person))))

;; Eithers

;; Left values

(ert-deftest test-all-negalgebraic-types-negeithers-negleft-values-negleft-int ()

  (should (equal (∀t0.either<int32, t0>) (∀t0.either<int32, t0>))))

(ert-deftest test-all-negalgebraic-types-negeithers-negleft-values-negleft-string ()

  (should (equal (∀t0.either<string, t0>) (∀t0.either<string, t0>))))

(ert-deftest test-all-negalgebraic-types-negeithers-negleft-values-negleft-boolean ()

  (should (equal (∀t0.either<boolean, t0>) (∀t0.either<boolean, t0>))))

;; Right values

(ert-deftest test-all-negalgebraic-types-negeithers-negright-values-negright-int ()

  (should (equal (∀t0.either<t0, int32>) (∀t0.either<t0, int32>))))

(ert-deftest test-all-negalgebraic-types-negeithers-negright-values-negright-string ()

  (should (equal (∀t0.either<t0, string>) (∀t0.either<t0, string>))))

(ert-deftest test-all-negalgebraic-types-negeithers-negright-values-negright-boolean ()

  (should (equal (∀t0.either<t0, boolean>) (∀t0.either<t0, boolean>))))

;; Polymorphic eithers

(ert-deftest test-all-negalgebraic-types-negeithers-negpolymorphic-eithers-negleft-from-lambda ()

  (should (equal (∀t0.(∀t1.(t0 → either<t0, t1>))) (∀t0.(∀t1.(t0 → either<t0, t1>))))))

(ert-deftest test-all-negalgebraic-types-negeithers-negpolymorphic-eithers-negright-from-lambda ()

  (should (equal (∀t0.(∀t1.(t0 → either<t1, t0>))) (∀t0.(∀t1.(t0 → either<t1, t0>))))))

(ert-deftest test-all-negalgebraic-types-negeithers-negpolymorphic-eithers-negeither-from-two-lambdas ()

  (should (equal (∀t0.(boolean → t0 → either<t0, t0>)) (∀t0.(boolean → t0 → either<t0, t0>)))))

;; Eithers in complex contexts

(ert-deftest test-all-negalgebraic-types-negeithers-negeithers-in-complex-contexts-negeither-in-list ()

  (should (equal list<either<string, int32>> list<either<string, int32>>)))

(ert-deftest test-all-negalgebraic-types-negeithers-negeithers-in-complex-contexts-negeither-in-let-binding ()

  (should (equal (∀t0.either<t0, int32>) (∀t0.either<t0, int32>))))

;; Nested eithers

(ert-deftest test-all-negalgebraic-types-negeithers-negnested-eithers-negeither-of-either-left-left ()

  (should (equal (∀t0.(∀t1.either<either<int32, t0>, t1>)) (∀t0.(∀t1.either<either<int32, t0>, t1>)))))

(ert-deftest test-all-negalgebraic-types-negeithers-negnested-eithers-negeither-of-either-left-right ()

  (should (equal (∀t0.(∀t1.either<either<t0, string>, t1>)) (∀t0.(∀t1.either<either<t0, string>, t1>)))))

(ert-deftest test-all-negalgebraic-types-negeithers-negnested-eithers-negeither-of-either-right ()

  (should (equal (∀t0.either<t0, boolean>) (∀t0.either<t0, boolean>))))

(ert-deftest test-all-negalgebraic-types-negeithers-negnested-eithers-negeither-of-list ()

  (should (equal (∀t0.either<list<int32>, t0>) (∀t0.either<list<int32>, t0>))))

(ert-deftest test-all-negalgebraic-types-negeithers-negnested-eithers-neglist-of-eithers ()

  (should (equal list<either<string, int32>> list<either<string, int32>>)))

;; Eithers with complex types

(ert-deftest test-all-negalgebraic-types-negeithers-negeithers-with-complex-types-negeither-with-record-on-left ()

  (should (equal (∀t0.either<Person, t0>) (∀t0.either<Person, t0>))))

(ert-deftest test-all-negalgebraic-types-negeithers-negeithers-with-complex-types-negeither-with-record-on-right ()

  (should (equal (∀t0.either<t0, Person>) (∀t0.either<t0, Person>))))

;; Optionals

;; Monomorphic optionals

(ert-deftest test-all-negalgebraic-types-negoptionals-negmonomorphic-optionals-negnothing ()

  (should (equal (∀t0.maybe<t0>) (∀t0.maybe<t0>))))

(ert-deftest test-all-negalgebraic-types-negoptionals-negmonomorphic-optionals-negjust-int ()

  (should (equal maybe<int32> maybe<int32>)))

(ert-deftest test-all-negalgebraic-types-negoptionals-negmonomorphic-optionals-negjust-string ()

  (should (equal maybe<string> maybe<string>)))

(ert-deftest test-all-negalgebraic-types-negoptionals-negmonomorphic-optionals-negjust-boolean ()

  (should (equal maybe<boolean> maybe<boolean>)))

;; Polymorphic optionals

(ert-deftest test-all-negalgebraic-types-negoptionals-negpolymorphic-optionals-negoptional-from-lambda ()

  (should (equal (∀t0.(t0 → maybe<t0>)) (∀t0.(t0 → maybe<t0>)))))

(ert-deftest test-all-negalgebraic-types-negoptionals-negpolymorphic-optionals-negnothing-from-lambda ()

  (should (equal (∀t0.(∀t1.(t0 → maybe<t1>))) (∀t0.(∀t1.(t0 → maybe<t1>))))))

(ert-deftest test-all-negalgebraic-types-negoptionals-negpolymorphic-optionals-negconditional-optional ()

  (should (equal (∀t0.(t0 → boolean → maybe<t0>)) (∀t0.(t0 → boolean → maybe<t0>)))))

;; Optionals in complex contexts

(ert-deftest test-all-negalgebraic-types-negoptionals-negoptionals-in-complex-contexts-negoptional-in-record ()

  (should (equal (BuddyListA @ string) (BuddyListA @ string))))

(ert-deftest test-all-negalgebraic-types-negoptionals-negoptionals-in-complex-contexts-negoptional-in-let-binding ()

  (should (equal maybe<int32> maybe<int32>)))

;; Nested optionals

(ert-deftest test-all-negalgebraic-types-negoptionals-negnested-optionals-negoptional-of-optional ()

  (should (equal maybe<maybe<string>> maybe<maybe<string>>)))

(ert-deftest test-all-negalgebraic-types-negoptionals-negnested-optionals-negoptional-of-list ()

  (should (equal maybe<list<int32>> maybe<list<int32>>)))

(ert-deftest test-all-negalgebraic-types-negoptionals-negnested-optionals-neglist-of-optionals ()

  (should (equal list<maybe<string>> list<maybe<string>>)))

;; Optionals with complex types

(ert-deftest test-all-negalgebraic-types-negoptionals-negoptionals-with-complex-types-negoptional-map ()

  (should (equal maybe<map<string, int32>> maybe<map<string, int32>>)))

;; Collections

;; Lists

;; Lists of literals

(ert-deftest test-all-negcollections-neglists-neglists-of-literals-negint-list ()

  (should (equal list<int32> list<int32>)))

(ert-deftest test-all-negcollections-neglists-neglists-of-literals-negstring-list ()

  (should (equal list<string> list<string>)))

(ert-deftest test-all-negcollections-neglists-neglists-of-literals-negsingle-element-list ()

  (should (equal list<bigint> list<bigint>)))

(ert-deftest test-all-negcollections-neglists-neglists-of-literals-negmixed-numeric-types ()

  (should (equal list<float32> list<float32>)))

;; Empty lists

(ert-deftest test-all-negcollections-neglists-negempty-lists-negempty-list ()

  (should (equal (∀t0.list<t0>) (∀t0.list<t0>))))

(ert-deftest test-all-negcollections-neglists-negempty-lists-negpair-of-empty-lists ()

  (should (equal (∀t0.(∀t1.(list<t0>, list<t1>))) (∀t0.(∀t1.(list<t0>, list<t1>))))))

(ert-deftest test-all-negcollections-neglists-negempty-lists-negempty-list-in-tuple ()

  (should (equal (∀t0.(list<t0>, string)) (∀t0.(list<t0>, string)))))

;; Polymorphic lists

(ert-deftest test-all-negcollections-neglists-negpolymorphic-lists-neglist-from-lambda ()

  (should (equal (∀t0.(t0 → list<t0>)) (∀t0.(t0 → list<t0>)))))

(ert-deftest test-all-negcollections-neglists-negpolymorphic-lists-neglist-with-repeated-var ()

  (should (equal (∀t0.(t0 → list<t0>)) (∀t0.(t0 → list<t0>)))))

(ert-deftest test-all-negcollections-neglists-negpolymorphic-lists-neglist-from-two-lambdas ()

  (should (equal (∀t0.(t0 → t0 → list<t0>)) (∀t0.(t0 → t0 → list<t0>)))))

;; Nested lists

(ert-deftest test-all-negcollections-neglists-negnested-lists-neglist-of-lists ()

  (should (equal list<list<int32>> list<list<int32>>)))

(ert-deftest test-all-negcollections-neglists-negnested-lists-negempty-nested-lists ()

  (should (equal (∀t0.list<list<t0>>) (∀t0.list<list<t0>>))))

(ert-deftest test-all-negcollections-neglists-negnested-lists-negnested-polymorphic ()

  (should (equal (∀t0.(t0 → list<list<t0>>)) (∀t0.(t0 → list<list<t0>>)))))

;; Lists in complex contexts

(ert-deftest test-all-negcollections-neglists-neglists-in-complex-contexts-negmultiple-lists-in-tuple ()

  (should (equal (list<int32>, list<string>) (list<int32>, list<string>))))

;; Sets

;; Monomorphic sets

(ert-deftest test-all-negcollections-negsets-negmonomorphic-sets-negempty-set ()

  (should (equal (∀t0.set<t0>) (∀t0.set<t0>))))

(ert-deftest test-all-negcollections-negsets-negmonomorphic-sets-negint-set ()

  (should (equal set<int32> set<int32>)))

(ert-deftest test-all-negcollections-negsets-negmonomorphic-sets-negstring-set ()

  (should (equal set<string> set<string>)))

(ert-deftest test-all-negcollections-negsets-negmonomorphic-sets-negsingle-element-set ()

  (should (equal set<boolean> set<boolean>)))

;; Polymorphic sets

(ert-deftest test-all-negcollections-negsets-negpolymorphic-sets-negset-from-lambda ()

  (should (equal (∀t0.(t0 → set<t0>)) (∀t0.(t0 → set<t0>)))))

(ert-deftest test-all-negcollections-negsets-negpolymorphic-sets-negset-with-repeated-variable ()

  (should (equal (∀t0.(t0 → set<t0>)) (∀t0.(t0 → set<t0>)))))

(ert-deftest test-all-negcollections-negsets-negpolymorphic-sets-negset-from-two-variables ()

  (should (equal (∀t0.(t0 → t0 → set<t0>)) (∀t0.(t0 → t0 → set<t0>)))))

;; Sets in complex contexts

(ert-deftest test-all-negcollections-negsets-negsets-in-complex-contexts-negset-in-tuple ()

  (should (equal (set<int32>, string) (set<int32>, string))))

(ert-deftest test-all-negcollections-negsets-negsets-in-complex-contexts-negset-in-let-binding ()

  (should (equal set<int32> set<int32>)))

;; Nested sets

(ert-deftest test-all-negcollections-negsets-negnested-sets-negset-of-lists ()

  (should (equal set<list<string>> set<list<string>>)))

(ert-deftest test-all-negcollections-negsets-negnested-sets-negset-of-tuples ()

  (should (equal set<(int32, int32)> set<(int32, int32)>)))

(ert-deftest test-all-negcollections-negsets-negnested-sets-negset-of-sets ()

  (should (equal set<set<string>> set<set<string>>)))

;; Sets with complex types

(ert-deftest test-all-negcollections-negsets-negsets-with-complex-types-negset-of-records ()

  (should (equal set<Person> set<Person>)))

(ert-deftest test-all-negcollections-negsets-negsets-with-complex-types-negset-of-optionals ()

  (should (equal set<maybe<int32>> set<maybe<int32>>)))

(ert-deftest test-all-negcollections-negsets-negsets-with-complex-types-negset-of-maps ()

  (should (equal set<map<string, int32>> set<map<string, int32>>)))

;; Maps

;; Monomorphic maps

(ert-deftest test-all-negcollections-negmaps-negmonomorphic-maps-negempty-map ()

  (should (equal (∀t0.(∀t1.map<t0, t1>)) (∀t0.(∀t1.map<t0, t1>)))))

(ert-deftest test-all-negcollections-negmaps-negmonomorphic-maps-negint-to-string-map ()

  (should (equal map<int32, string> map<int32, string>)))

(ert-deftest test-all-negcollections-negmaps-negmonomorphic-maps-negstring-to-int-map ()

  (should (equal map<string, int32> map<string, int32>)))

(ert-deftest test-all-negcollections-negmaps-negmonomorphic-maps-negsingle-entry-map ()

  (should (equal map<bigint, boolean> map<bigint, boolean>)))

;; Polymorphic maps

(ert-deftest test-all-negcollections-negmaps-negpolymorphic-maps-negmap-from-lambda-keys ()

  (should (equal (∀t0.(t0 → map<t0, string>)) (∀t0.(t0 → map<t0, string>)))))

(ert-deftest test-all-negcollections-negmaps-negpolymorphic-maps-negmap-from-lambda-values ()

  (should (equal (∀t0.(t0 → map<string, t0>)) (∀t0.(t0 → map<string, t0>)))))

(ert-deftest test-all-negcollections-negmaps-negpolymorphic-maps-negmap-from-lambda-both ()

  (should (equal (∀t0.(∀t1.(t0 → t1 → map<t0, t1>))) (∀t0.(∀t1.(t0 → t1 → map<t0, t1>))))))

(ert-deftest test-all-negcollections-negmaps-negpolymorphic-maps-negmap-with-repeated-variables ()

  (should (equal (∀t0.(t0 → map<t0, t0>)) (∀t0.(t0 → map<t0, t0>)))))

;; Maps in complex contexts

(ert-deftest test-all-negcollections-negmaps-negmaps-in-complex-contexts-negmap-in-tuple ()

  (should (equal (map<int32, string>, string) (map<int32, string>, string))))

(ert-deftest test-all-negcollections-negmaps-negmaps-in-complex-contexts-negnested-maps ()

  (should (equal map<string, map<int32, boolean>> map<string, map<int32, boolean>>)))

(ert-deftest test-all-negcollections-negmaps-negmaps-in-complex-contexts-negmap-in-let-binding ()

  (should (equal map<string, int32> map<string, int32>)))

;; Maps with complex types

(ert-deftest test-all-negcollections-negmaps-negmaps-with-complex-types-negmap-of-records ()

  (should (equal map<string, Person> map<string, Person>)))

(ert-deftest test-all-negcollections-negmaps-negmaps-with-complex-types-negmap-of-lists ()

  (should (equal map<int32, list<string>> map<int32, list<string>>)))

(ert-deftest test-all-negcollections-negmaps-negmaps-with-complex-types-negmap-of-tuples ()

  (should (equal map<string, (int32, int32)> map<string, (int32, int32)>)))

;; Fundamentals

;; Literals

;; Boolean literals

(ert-deftest test-all-negfundamentals-negliterals-negboolean-literals-negtrue ()

  (should (equal boolean boolean)))

(ert-deftest test-all-negfundamentals-negliterals-negboolean-literals-negfalse ()

  (should (equal boolean boolean)))

;; String literals

(ert-deftest test-all-negfundamentals-negliterals-negstring-literals-negsimple-string ()

  (should (equal string string)))

(ert-deftest test-all-negfundamentals-negliterals-negstring-literals-negempty-string ()

  (should (equal string string)))

(ert-deftest test-all-negfundamentals-negliterals-negstring-literals-negunicode-string ()

  (should (equal string string)))

;; Integer literals

(ert-deftest test-all-negfundamentals-negliterals-neginteger-literals-negbigint ()

  (should (equal bigint bigint)))

(ert-deftest test-all-negfundamentals-negliterals-neginteger-literals-negint8 ()

  (should (equal int8 int8)))

(ert-deftest test-all-negfundamentals-negliterals-neginteger-literals-negint16 ()

  (should (equal int16 int16)))

(ert-deftest test-all-negfundamentals-negliterals-neginteger-literals-negint32 ()

  (should (equal int32 int32)))

(ert-deftest test-all-negfundamentals-negliterals-neginteger-literals-negint64 ()

  (should (equal int64 int64)))

(ert-deftest test-all-negfundamentals-negliterals-neginteger-literals-neguint8 ()

  (should (equal uint8 uint8)))

(ert-deftest test-all-negfundamentals-negliterals-neginteger-literals-neguint16 ()

  (should (equal uint16 uint16)))

(ert-deftest test-all-negfundamentals-negliterals-neginteger-literals-neguint32 ()

  (should (equal uint32 uint32)))

(ert-deftest test-all-negfundamentals-negliterals-neginteger-literals-neguint64 ()

  (should (equal uint64 uint64)))

;; Float literals

(ert-deftest test-all-negfundamentals-negliterals-negfloat-literals-negbigfloat ()

  (should (equal bigfloat bigfloat)))

(ert-deftest test-all-negfundamentals-negliterals-negfloat-literals-negfloat32 ()

  (should (equal float32 float32)))

(ert-deftest test-all-negfundamentals-negliterals-negfloat-literals-negfloat64 ()

  (should (equal float64 float64)))

;; Literals in complex contexts

(ert-deftest test-all-negfundamentals-negliterals-negliterals-in-complex-contexts-negliterals-in-tuple ()

  (should (equal (boolean, (string, (int32, float32))) (boolean, (string, (int32, float32))))))

(ert-deftest test-all-negfundamentals-negliterals-negliterals-in-complex-contexts-negliterals-in-list ()

  (should (equal list<string> list<string>)))

;; Variables

;; Simple variable lookup

(ert-deftest test-all-negfundamentals-negvariables-negsimple-variable-lookup-negint-variable ()

  (should (equal (∀t0.(t0 → t0)) (∀t0.(t0 → t0)))))

(ert-deftest test-all-negfundamentals-negvariables-negsimple-variable-lookup-negvariable-in-let-binding ()

  (should (equal int32 int32)))

(ert-deftest test-all-negfundamentals-negvariables-negsimple-variable-lookup-negmultiple-variables ()

  (should (equal (string, int32) (string, int32))))

;; Variable scoping

(ert-deftest test-all-negfundamentals-negvariables-negvariable-scoping-neglambda-parameter ()

  (should (equal (∀t0.(∀t1.(t0 → t1 → t0))) (∀t0.(∀t1.(t0 → t1 → t0))))))

(ert-deftest test-all-negfundamentals-negvariables-negvariable-scoping-neglet-binding-scope ()

  (should (equal int32 int32)))

(ert-deftest test-all-negfundamentals-negvariables-negvariable-scoping-negvariable-shadowing ()

  (should (equal (∀t0.(t0 → t0)) (∀t0.(t0 → t0)))))

(ert-deftest test-all-negfundamentals-negvariables-negvariable-scoping-negnested-scoping ()

  (should (equal (∀t0.(∀t1.(t0 → t1 → (t0, (t0, t1))))) (∀t0.(∀t1.(t0 → t1 → (t0, (t0, t1))))))))

;; Polymorphic variables

(ert-deftest test-all-negfundamentals-negvariables-negpolymorphic-variables-negpolymorphic-function ()

  (should (equal (∀t0.(t0 → t0)) (∀t0.(t0 → t0)))))

(ert-deftest test-all-negfundamentals-negvariables-negpolymorphic-variables-negpolymorphic-application ()

  (should (equal (int32, string) (int32, string))))

(ert-deftest test-all-negfundamentals-negvariables-negpolymorphic-variables-neghigher-order-polymorphic ()

  (should (equal (∀t0.(∀t1.((t0 → t1) → t0 → t1))) (∀t0.(∀t1.((t0 → t1) → t0 → t1))))))

;; Variables in complex contexts

(ert-deftest test-all-negfundamentals-negvariables-negvariables-in-complex-contexts-negvariable-in-record ()

  (should (equal (string → Person) (string → Person))))

(ert-deftest test-all-negfundamentals-negvariables-negvariables-in-complex-contexts-negvariable-in-list ()

  (should (equal (∀t0.(t0 → list<t0>)) (∀t0.(t0 → list<t0>)))))

(ert-deftest test-all-negfundamentals-negvariables-negvariables-in-complex-contexts-negvariable-in-map ()

  (should (equal (∀t0.(∀t1.(t0 → t1 → map<t0, t1>))) (∀t0.(∀t1.(t0 → t1 → map<t0, t1>))))))

(ert-deftest test-all-negfundamentals-negvariables-negvariables-in-complex-contexts-negvariable-in-optional ()

  (should (equal (∀t0.(t0 → maybe<t0>)) (∀t0.(t0 → maybe<t0>)))))

;; Recursive variables

(ert-deftest test-all-negfundamentals-negvariables-negrecursive-variables-negsimple-recursion ()

  (should (equal (int32 → int32) (int32 → int32))))

(ert-deftest test-all-negfundamentals-negvariables-negrecursive-variables-negmutual-recursion ()

  (should (equal (int32 → int32) (int32 → int32))))

;; Lambdas

;; Simple lambdas

(ert-deftest test-all-negfundamentals-neglambdas-negsimple-lambdas-negidentity-function ()

  (should (equal (∀t0.(t0 → t0)) (∀t0.(t0 → t0)))))

(ert-deftest test-all-negfundamentals-neglambdas-negsimple-lambdas-negconstant-function ()

  (should (equal (∀t0.(t0 → int32)) (∀t0.(t0 → int32)))))

;; Multi-parameter lambdas

(ert-deftest test-all-negfundamentals-neglambdas-negmulti-negparameter-lambdas-negtwo-parameters ()

  (should (equal (∀t0.(∀t1.(t0 → t1 → t0))) (∀t0.(∀t1.(t0 → t1 → t0))))))

(ert-deftest test-all-negfundamentals-neglambdas-negmulti-negparameter-lambdas-negthree-parameters ()

  (should (equal (∀t0.(∀t1.(∀t2.(t0 → t1 → t2 → t1)))) (∀t0.(∀t1.(∀t2.(t0 → t1 → t2 → t1)))))))

(ert-deftest test-all-negfundamentals-neglambdas-negmulti-negparameter-lambdas-negparameter-reuse ()

  (should (equal (∀t0.(∀t1.(t0 → t1 → (t0, (t0, t1))))) (∀t0.(∀t1.(t0 → t1 → (t0, (t0, t1))))))))

;; Lambdas with operations

(ert-deftest test-all-negfundamentals-neglambdas-neglambdas-with-operations-neglambda-with-primitive ()

  (should (equal (int32 → int32) (int32 → int32))))

(ert-deftest test-all-negfundamentals-neglambdas-neglambdas-with-operations-neglambda-with-application ()

  (should (equal (∀t0.(∀t1.((t0 → t1) → t0 → t1))) (∀t0.(∀t1.((t0 → t1) → t0 → t1))))))

(ert-deftest test-all-negfundamentals-neglambdas-neglambdas-with-operations-neglambda-with-construction ()

  (should (equal (∀t0.(∀t1.(t0 → t1 → (t0, t1)))) (∀t0.(∀t1.(t0 → t1 → (t0, t1)))))))

;; Nested lambdas

(ert-deftest test-all-negfundamentals-neglambdas-negnested-lambdas-neglambda-returning-lambda ()

  (should (equal (∀t0.(∀t1.(∀t2.(t0 → t1 → t2 → t0)))) (∀t0.(∀t1.(∀t2.(t0 → t1 → t2 → t0)))))))

(ert-deftest test-all-negfundamentals-neglambdas-negnested-lambdas-neglambda-with-let-binding ()

  (should (equal (∀t0.(t0 → t0)) (∀t0.(t0 → t0)))))

(ert-deftest test-all-negfundamentals-neglambdas-negnested-lambdas-neglambda-with-inner-lambda ()

  (should (equal (∀t0.(t0 → t0)) (∀t0.(t0 → t0)))))

;; Lambdas in complex contexts

(ert-deftest test-all-negfundamentals-neglambdas-neglambdas-in-complex-contexts-neglambda-in-tuple ()

  (should (equal (∀t0.((t0 → t0), int32)) (∀t0.((t0 → t0), int32)))))

(ert-deftest test-all-negfundamentals-neglambdas-neglambdas-in-complex-contexts-neglambda-in-list ()

  (should (equal list<(int32 → int32)> list<(int32 → int32)>)))

(ert-deftest test-all-negfundamentals-neglambdas-neglambdas-in-complex-contexts-neglambda-in-record ()

  (should (equal (string → Person) (string → Person))))

;; Higher-order lambdas

(ert-deftest test-all-negfundamentals-neglambdas-neghigher-negorder-lambdas-negfunction-composition ()

  (should (equal (∀t0.(∀t1.(∀t2.((t0 → t1) → (t2 → t0) → t2 → t1)))) (∀t0.(∀t1.(∀t2.((t0 → t1) → (t2 → t0) → t2 → t1)))))))

(ert-deftest test-all-negfundamentals-neglambdas-neghigher-negorder-lambdas-negfunction-application ()

  (should (equal (∀t0.(∀t1.((t0 → t1) → t0 → t1))) (∀t0.(∀t1.((t0 → t1) → t0 → t1))))))

(ert-deftest test-all-negfundamentals-neglambdas-neghigher-negorder-lambdas-negcurried-function ()

  (should (equal (∀t0.(boolean → t0 → t0 → t0)) (∀t0.(boolean → t0 → t0 → t0)))))

;; Applications

;; Simple function applications

(ert-deftest test-all-negfundamentals-negapplications-negsimple-function-applications-negidentity-application ()

  (should (equal int32 int32)))

(ert-deftest test-all-negfundamentals-negapplications-negsimple-function-applications-negprimitive-application ()

  (should (equal int32 int32)))

(ert-deftest test-all-negfundamentals-negapplications-negsimple-function-applications-negstring-concatenation ()

  (should (equal string string)))

;; Partial applications

(ert-deftest test-all-negfundamentals-negapplications-negpartial-applications-negpartially-applied-add ()

  (should (equal (int32 → int32) (int32 → int32))))

(ert-deftest test-all-negfundamentals-negapplications-negpartial-applications-negpartially-applied-string-cat ()

  (should (equal (string → string) (string → string))))

;; Higher-order applications

(ert-deftest test-all-negfundamentals-negapplications-neghigher-negorder-applications-negapply-function-to-function ()

  (should (equal int32 int32)))

(ert-deftest test-all-negfundamentals-negapplications-neghigher-negorder-applications-negfunction-composition ()

  (should (equal int32 int32)))

;; Polymorphic applications

(ert-deftest test-all-negfundamentals-negapplications-negpolymorphic-applications-negpolymorphic-identity ()

  (should (equal (int32, string) (int32, string))))

(ert-deftest test-all-negfundamentals-negapplications-negpolymorphic-applications-negpolymorphic-const ()

  (should (equal string string)))

(ert-deftest test-all-negfundamentals-negapplications-negpolymorphic-applications-negpolymorphic-flip ()

  (should (equal string string)))

;; Applications in complex contexts

(ert-deftest test-all-negfundamentals-negapplications-negapplications-in-complex-contexts-negapplication-in-tuple ()

  (should (equal (int32, string) (int32, string))))

(ert-deftest test-all-negfundamentals-negapplications-negapplications-in-complex-contexts-negapplication-in-record ()

  (should (equal Person Person)))

(ert-deftest test-all-negfundamentals-negapplications-negapplications-in-complex-contexts-negapplication-in-let-binding ()

  (should (equal int32 int32)))

(ert-deftest test-all-negfundamentals-negapplications-negapplications-in-complex-contexts-negnested-applications ()

  (should (equal int32 int32)))

;; Applications with complex arguments

(ert-deftest test-all-negfundamentals-negapplications-negapplications-with-complex-arguments-negapplication-with-record-argument ()

  (should (equal string string)))

(ert-deftest test-all-negfundamentals-negapplications-negapplications-with-complex-arguments-negapplication-with-list-argument ()

  (should (equal string string)))

;; Let terms

;; Simple let bindings

(ert-deftest test-all-negfundamentals-neglet-terms-negsimple-let-bindings-negsingle-binding ()

  (should (equal int32 int32)))

(ert-deftest test-all-negfundamentals-neglet-terms-negsimple-let-bindings-negmultiple-bindings ()

  (should (equal (int32, string) (int32, string))))

;; Let terms with shadowing

(ert-deftest test-all-negfundamentals-neglet-terms-neglet-terms-with-shadowing-neglambda-parameter-shadowing-let-binding ()

  (should (equal (∀t0.(t0 → t0)) (∀t0.(t0 → t0)))))

(ert-deftest test-all-negfundamentals-neglet-terms-neglet-terms-with-shadowing-negnested-lambda-shadowing ()

  (should (equal (∀t0.(∀t1.(t0 → t1 → (t1, t0)))) (∀t0.(∀t1.(t0 → t1 → (t1, t0)))))))

(ert-deftest test-all-negfundamentals-neglet-terms-neglet-terms-with-shadowing-negmultiple-levels-of-let-shadowing ()

  (should (equal boolean boolean)))

(ert-deftest test-all-negfundamentals-neglet-terms-neglet-terms-with-shadowing-neglet-shadowing-with-lambda-and-reference-to-outer-binding ()

  (should (equal (∀t0.(t0 → (t0, int32))) (∀t0.(t0 → (t0, int32))))))

;; Recursive bindings

(ert-deftest test-all-negfundamentals-neglet-terms-negrecursive-bindings-negsimple-arithmetic-recursion ()

  (should (equal int32 int32)))

;; Mutual recursion

(ert-deftest test-all-negfundamentals-neglet-terms-negmutual-recursion-negmutually-recursive-data ()

  (should (equal (BuddyListA @ int32) (BuddyListA @ int32))))

(ert-deftest test-all-negfundamentals-neglet-terms-negmutual-recursion-neg-monomorphic-mutually-recursive-functions ()

  (should (equal int32 int32)))

;; Nested let terms

(ert-deftest test-all-negfundamentals-neglet-terms-negnested-let-terms-negmonomorphic-nesting ()

  (should (equal int32 int32)))

(ert-deftest test-all-negfundamentals-neglet-terms-negnested-let-terms-negpolymorphic-nesting ()

  (should (equal string string)))

(ert-deftest test-all-negfundamentals-neglet-terms-negnested-let-terms-negvariable-capture-avoidance ()

  (should (equal (∀t0.(t0 → t0)) (∀t0.(t0 → t0)))))

(ert-deftest test-all-negfundamentals-neglet-terms-negnested-let-terms-negsimple-let-in-lambda ()

  (should (equal (∀t0.(t0 → t0)) (∀t0.(t0 → t0)))))

;; Let with complex expressions

(ert-deftest test-all-negfundamentals-neglet-terms-neglet-with-complex-expressions-neglet-in-record ()

  (should (equal Person Person)))

(ert-deftest test-all-negfundamentals-neglet-terms-neglet-with-complex-expressions-neglet-in-function-application ()

  (should (equal int32 int32)))

(ert-deftest test-all-negfundamentals-neglet-terms-neglet-with-complex-expressions-negpolymorphic-let-binding ()

  (should (equal (int32, string) (int32, string))))

(ert-deftest test-all-negfundamentals-neglet-terms-neglet-with-complex-expressions-negcomposition ()

  (should (equal int32 int32)))

;; Primitives

;; Nullary primitives

(ert-deftest test-all-negfundamentals-negprimitives-negnullary-primitives-negempty-map ()

  (should (equal (∀t0.(∀t1.map<t0, t1>)) (∀t0.(∀t1.map<t0, t1>)))))

(ert-deftest test-all-negfundamentals-negprimitives-negnullary-primitives-negempty-set ()

  (should (equal (∀t0.set<t0>) (∀t0.set<t0>))))

;; Unary primitives

(ert-deftest test-all-negfundamentals-negprimitives-negunary-primitives-neglists-head ()

  (should (equal (∀t0.(list<t0> → t0)) (∀t0.(list<t0> → t0)))))

(ert-deftest test-all-negfundamentals-negprimitives-negunary-primitives-negmath-neg ()

  (should (equal (int32 → int32) (int32 → int32))))

(ert-deftest test-all-negfundamentals-negprimitives-negunary-primitives-neglogic-not ()

  (should (equal (boolean → boolean) (boolean → boolean))))

;; Binary primitives

(ert-deftest test-all-negfundamentals-negprimitives-negbinary-primitives-negmath-add ()

  (should (equal (int32 → int32 → int32) (int32 → int32 → int32))))

(ert-deftest test-all-negfundamentals-negprimitives-negbinary-primitives-neglists-cons ()

  (should (equal (∀t0.(t0 → list<t0> → list<t0>)) (∀t0.(t0 → list<t0> → list<t0>)))))

(ert-deftest test-all-negfundamentals-negprimitives-negbinary-primitives-negmaps-insert ()

  (should (equal (∀t0.(∀t1.(t0 → t1 → map<t0, t1> → map<t0, t1>))) (∀t0.(∀t1.(t0 → t1 → map<t0, t1> → map<t0, t1>))))))

;; Ternary primitives

(ert-deftest test-all-negfundamentals-negprimitives-negternary-primitives-neglogic-ifelse ()

  (should (equal (∀t0.(boolean → t0 → t0 → t0)) (∀t0.(boolean → t0 → t0 → t0)))))

(ert-deftest test-all-negfundamentals-negprimitives-negternary-primitives-neglists-foldl ()

  (should (equal (∀t0.(∀t1.((t0 → t1 → t0) → t0 → list<t1> → t0))) (∀t0.(∀t1.((t0 → t1 → t0) → t0 → list<t1> → t0))))))

;; Monomorphic vs polymorphic

(ert-deftest test-all-negfundamentals-negprimitives-negmonomorphic-vs-polymorphic-negmonomorphic-math ()

  (should (equal (int32 → int32 → int32) (int32 → int32 → int32))))

(ert-deftest test-all-negfundamentals-negprimitives-negmonomorphic-vs-polymorphic-negpolymorphic-identity ()

  (should (equal (∀t0.(t0 → t0)) (∀t0.(t0 → t0)))))

(ert-deftest test-all-negfundamentals-negprimitives-negmonomorphic-vs-polymorphic-negpolymorphic-map ()

  (should (equal (∀t0.(∀t1.((t0 → t1) → list<t0> → list<t1>))) (∀t0.(∀t1.((t0 → t1) → list<t0> → list<t1>))))))

;; Higher-order primitives

(ert-deftest test-all-negfundamentals-negprimitives-neghigher-negorder-primitives-neglists-map-function ()

  (should (equal (list<int32> → list<int32>) (list<int32> → list<int32>))))

(ert-deftest test-all-negfundamentals-negprimitives-neghigher-negorder-primitives-neglists-filter ()

  (should (equal (∀t0.((t0 → boolean) → list<t0> → list<t0>)) (∀t0.((t0 → boolean) → list<t0> → list<t0>)))))

(ert-deftest test-all-negfundamentals-negprimitives-neghigher-negorder-primitives-negoptionals-maybe ()

  (should (equal (∀t0.(∀t1.(t0 → (t1 → t0) → maybe<t1> → t0))) (∀t0.(∀t1.(t0 → (t1 → t0) → maybe<t1> → t0))))))

;; Primitives in complex contexts

(ert-deftest test-all-negfundamentals-negprimitives-negprimitives-in-complex-contexts-negprimitive-composition ()

  (should (equal list<int32> list<int32>)))

(ert-deftest test-all-negfundamentals-negprimitives-negprimitives-in-complex-contexts-negnested-higher-negorder ()

  (should (equal list<list<int32>> list<list<int32>>)))

;; Nominal types

;; Records

;; Monomorphic records

(ert-deftest test-all-negnominal-types-negrecords-negmonomorphic-records-neglatlon-record ()

  (should (equal LatLon LatLon)))

(ert-deftest test-all-negnominal-types-negrecords-negmonomorphic-records-neglatlon-with-variable ()

  (should (equal (float32 → LatLon) (float32 → LatLon))))

(ert-deftest test-all-negnominal-types-negrecords-negmonomorphic-records-negperson-record ()

  (should (equal Person Person)))

(ert-deftest test-all-negnominal-types-negrecords-negmonomorphic-records-negempty-record ()

  (should (equal Unit Unit)))

(ert-deftest test-all-negnominal-types-negrecords-negmonomorphic-records-negperson-with-variables ()

  (should (equal (string → int32 → Person) (string → int32 → Person))))

;; Polymorphic records

(ert-deftest test-all-negnominal-types-negrecords-negpolymorphic-records-neglatlon-poly-float ()

  (should (equal (LatLonPoly @ float32) (LatLonPoly @ float32))))

(ert-deftest test-all-negnominal-types-negrecords-negpolymorphic-records-neglatlon-poly-int64 ()

  (should (equal (LatLonPoly @ int64) (LatLonPoly @ int64))))

(ert-deftest test-all-negnominal-types-negrecords-negpolymorphic-records-neglatlon-poly-variable ()

  (should (equal (∀t0.(t0 → (LatLonPoly @ t0))) (∀t0.(t0 → (LatLonPoly @ t0))))))

(ert-deftest test-all-negnominal-types-negrecords-negpolymorphic-records-negbuddylist-string ()

  (should (equal (BuddyListA @ string) (BuddyListA @ string))))

(ert-deftest test-all-negnominal-types-negrecords-negpolymorphic-records-negbuddylist-variable ()

  (should (equal (∀t0.(t0 → (BuddyListA @ t0))) (∀t0.(t0 → (BuddyListA @ t0))))))

;; Records in complex contexts

(ert-deftest test-all-negnominal-types-negrecords-negrecords-in-complex-contexts-negrecords-in-tuple ()

  (should (equal (Person, LatLon) (Person, LatLon))))

(ert-deftest test-all-negnominal-types-negrecords-negrecords-in-complex-contexts-negpoly-records-in-tuple ()

  (should (equal ((LatLonPoly @ int32), (BuddyListA @ string)) ((LatLonPoly @ int32), (BuddyListA @ string)))))

(ert-deftest test-all-negnominal-types-negrecords-negrecords-in-complex-contexts-negrecursive-record ()

  (should (equal IntList IntList)))

;; Multi-parameter polymorphic records

(ert-deftest test-all-negnominal-types-negrecords-negmulti-negparameter-polymorphic-records-negtriple-with-three-monomorphic-types ()

  (should (equal (Triple @ int32 @ string @ boolean) (Triple @ int32 @ string @ boolean))))

(ert-deftest test-all-negnominal-types-negrecords-negmulti-negparameter-polymorphic-records-negtriple-with-personorsomething-containing-map ()

  (should (equal (∀t0.(∀t1.(t0 → t1 → (Triple @ string @ (PersonOrSomething @ map<t0, t1>) @ int32)))) (∀t0.(∀t1.(t0 → t1 → (Triple @ string @ (PersonOrSomething @ map<t0, t1>) @ int32)))))))

;; Unions

;; Simple union injections

(ert-deftest test-all-negnominal-types-negunions-negsimple-union-injections-neginject-into-comparison-lessthan-variant ()

  (should (equal Comparison Comparison)))

(ert-deftest test-all-negnominal-types-negunions-negsimple-union-injections-neginject-into-comparison-equalto-variant ()

  (should (equal Comparison Comparison)))

(ert-deftest test-all-negnominal-types-negunions-negsimple-union-injections-neginject-into-comparison-greaterthan-variant ()

  (should (equal Comparison Comparison)))

;; Union injections with data

(ert-deftest test-all-negnominal-types-negunions-negunion-injections-with-data-neginject-into-number-int-variant ()

  (should (equal Number Number)))

(ert-deftest test-all-negnominal-types-negunions-negunion-injections-with-data-neginject-into-number-float-variant ()

  (should (equal Number Number)))

(ert-deftest test-all-negnominal-types-negunions-negunion-injections-with-data-neginject-into-timestamp-unixtimemillis-variant ()

  (should (equal Timestamp Timestamp)))

(ert-deftest test-all-negnominal-types-negunions-negunion-injections-with-data-neginject-into-timestamp-date-variant ()

  (should (equal Timestamp Timestamp)))

;; Polymorphic union injections

(ert-deftest test-all-negnominal-types-negunions-negpolymorphic-union-injections-neginject-person-into-personorsomething ()

  (should (equal (∀t0.(PersonOrSomething @ t0)) (∀t0.(PersonOrSomething @ t0)))))

(ert-deftest test-all-negnominal-types-negunions-negpolymorphic-union-injections-neginject-string-into-personorsomething-other-variant ()

  (should (equal (PersonOrSomething @ string) (PersonOrSomething @ string))))

(ert-deftest test-all-negnominal-types-negunions-negpolymorphic-union-injections-neginject-int-into-personorsomething-other-variant ()

  (should (equal (PersonOrSomething @ int32) (PersonOrSomething @ int32))))

;; Polymorphic recursive union injections

(ert-deftest test-all-negnominal-types-negunions-negpolymorphic-recursive-union-injections-neginject-boolean-into-unionpolymorphicrecursive ()

  (should (equal (∀t0.(UnionPolymorphicRecursive @ t0)) (∀t0.(UnionPolymorphicRecursive @ t0)))))

(ert-deftest test-all-negnominal-types-negunions-negpolymorphic-recursive-union-injections-neginject-string-value-into-unionpolymorphicrecursive ()

  (should (equal (UnionPolymorphicRecursive @ string) (UnionPolymorphicRecursive @ string))))

(ert-deftest test-all-negnominal-types-negunions-negpolymorphic-recursive-union-injections-neginject-int-value-into-unionpolymorphicrecursive ()

  (should (equal (UnionPolymorphicRecursive @ int32) (UnionPolymorphicRecursive @ int32))))

;; Polymorphic unions from lambda

(ert-deftest test-all-negnominal-types-negunions-negpolymorphic-unions-from-lambda-neglambda-creating-personorsomething-other-variant ()

  (should (equal (∀t0.(t0 → (PersonOrSomething @ t0))) (∀t0.(t0 → (PersonOrSomething @ t0))))))

(ert-deftest test-all-negnominal-types-negunions-negpolymorphic-unions-from-lambda-neglambda-creating-unionpolymorphicrecursive-value-variant ()

  (should (equal (∀t0.(t0 → (UnionPolymorphicRecursive @ t0))) (∀t0.(t0 → (UnionPolymorphicRecursive @ t0))))))

;; Unions in complex contexts

(ert-deftest test-all-negnominal-types-negunions-negunions-in-complex-contexts-negunion-in-tuple ()

  (should (equal (Number, string) (Number, string))))

(ert-deftest test-all-negnominal-types-negunions-negunions-in-complex-contexts-negunion-in-list ()

  (should (equal list<Number> list<Number>)))

(ert-deftest test-all-negnominal-types-negunions-negunions-in-complex-contexts-negpolymorphic-union-in-let-binding ()

  (should (equal (PersonOrSomething @ string) (PersonOrSomething @ string))))

;; Multi-parameter polymorphic injections

(ert-deftest test-all-negnominal-types-negunions-negmulti-negparameter-polymorphic-injections-negeither-left-with-int ()

  (should (equal (∀t0.(Either @ int32 @ t0)) (∀t0.(Either @ int32 @ t0)))))

(ert-deftest test-all-negnominal-types-negunions-negmulti-negparameter-polymorphic-injections-negeither-right-with-string ()

  (should (equal (∀t0.(Either @ t0 @ string)) (∀t0.(Either @ t0 @ string)))))

(ert-deftest test-all-negnominal-types-negunions-negmulti-negparameter-polymorphic-injections-negeither-containing-latlonpoly-in-list ()

  (should (equal (∀t0.(Either @ t0 @ list<(LatLonPoly @ int32)>)) (∀t0.(Either @ t0 @ list<(LatLonPoly @ int32)>)))))

(ert-deftest test-all-negnominal-types-negunions-negmulti-negparameter-polymorphic-injections-negeither-in-triple-in-map-with-shared-type-variables ()

  (should (equal (∀t0.(∀t1.(∀t2.(∀t3.(∀t4.(∀t5.(t0 → t1 → t2 → map<string, (Triple @ (Either @ t0 @ t3) @ (Either @ t0 @ t4) @ (Either @ t5 @ t1))>))))))) (∀t0.(∀t1.(∀t2.(∀t3.(∀t4.(∀t5.(t0 → t1 → t2 → map<string, (Triple @ (Either @ t0 @ t3) @ (Either @ t0 @ t4) @ (Either @ t5 @ t1))>))))))))))

;; Wrapped terms

;; Monomorphic wrapped terms

(ert-deftest test-all-negnominal-types-negwrapped-terms-negmonomorphic-wrapped-terms-negstring-alias ()

  (should (equal StringAlias StringAlias)))

(ert-deftest test-all-negnominal-types-negwrapped-terms-negmonomorphic-wrapped-terms-negwrapped-integer ()

  (should (equal StringAlias StringAlias)))

(ert-deftest test-all-negnominal-types-negwrapped-terms-negmonomorphic-wrapped-terms-negwrapped-in-tuple ()

  (should (equal (StringAlias, string) (StringAlias, string))))

;; Polymorphic wrapped terms

(ert-deftest test-all-negnominal-types-negwrapped-terms-negpolymorphic-wrapped-terms-negpolymorphic-wrapper-with-int ()

  (should (equal (PolymorphicWrapper @ int32) (PolymorphicWrapper @ int32))))

(ert-deftest test-all-negnominal-types-negwrapped-terms-negpolymorphic-wrapped-terms-negpolymorphic-wrapper-with-string ()

  (should (equal (PolymorphicWrapper @ string) (PolymorphicWrapper @ string))))

(ert-deftest test-all-negnominal-types-negwrapped-terms-negpolymorphic-wrapped-terms-negpolymorphic-wrapper-from-lambda ()

  (should (equal (∀t0.(t0 → (PolymorphicWrapper @ t0))) (∀t0.(t0 → (PolymorphicWrapper @ t0))))))

;; Wrapped terms in complex contexts

(ert-deftest test-all-negnominal-types-negwrapped-terms-negwrapped-terms-in-complex-contexts-negwrapped-in-record ()

  (should (equal Person Person)))

(ert-deftest test-all-negnominal-types-negwrapped-terms-negwrapped-terms-in-complex-contexts-negwrapped-in-let-binding ()

  (should (equal StringAlias StringAlias)))

(ert-deftest test-all-negnominal-types-negwrapped-terms-negwrapped-terms-in-complex-contexts-negwrapped-in-list ()

  (should (equal list<StringAlias> list<StringAlias>)))

;; Nested wrapped terms

(ert-deftest test-all-negnominal-types-negwrapped-terms-negnested-wrapped-terms-negwrapped-tuple ()

  (should (equal (PolymorphicWrapper @ (int32, string)) (PolymorphicWrapper @ (int32, string)))))

(ert-deftest test-all-negnominal-types-negwrapped-terms-negnested-wrapped-terms-negwrapped-optional ()

  (should (equal (PolymorphicWrapper @ maybe<int32>) (PolymorphicWrapper @ maybe<int32>))))

(ert-deftest test-all-negnominal-types-negwrapped-terms-negnested-wrapped-terms-negwrapped-map ()

  (should (equal (PolymorphicWrapper @ map<string, int32>) (PolymorphicWrapper @ map<string, int32>))))

;; Multiple wrapping levels

(ert-deftest test-all-negnominal-types-negwrapped-terms-negmultiple-wrapping-levels-negwrapped-in-optional ()

  (should (equal maybe<StringAlias> maybe<StringAlias>)))

(ert-deftest test-all-negnominal-types-negwrapped-terms-negmultiple-wrapping-levels-neglist-of-wrapped-polymorphic ()

  (should (equal list<(PolymorphicWrapper @ int32)> list<(PolymorphicWrapper @ int32)>)))

;; Multi-parameter polymorphic wrappers

(ert-deftest test-all-negnominal-types-negwrapped-terms-negmulti-negparameter-polymorphic-wrappers-negsymmetric-triple-wrapping-simple-types ()

  (should (equal (SymmetricTriple @ int32 @ string) (SymmetricTriple @ int32 @ string))))

(ert-deftest test-all-negnominal-types-negwrapped-terms-negmulti-negparameter-polymorphic-wrappers-negsymmetric-triple-from-lambda ()

  (should (equal (∀t0.(∀t1.(t0 → t1 → t0 → (SymmetricTriple @ t0 @ t1)))) (∀t0.(∀t1.(t0 → t1 → t0 → (SymmetricTriple @ t0 @ t1)))))))

(ert-deftest test-all-negnominal-types-negwrapped-terms-negmulti-negparameter-polymorphic-wrappers-negsymmetric-triple-with-nested-polymorphic-types-and-foldl ()

  (should (equal (list<int32> → list<int32> → (SymmetricTriple @ int32 @ list<list<int32>>)) (list<int32> → list<int32> → (SymmetricTriple @ int32 @ list<list<int32>>)))))

;; Eliminations

;; Record eliminations

;; Simple record projections

(ert-deftest test-all-negnominal-types-negeliminations-negrecord-eliminations-negsimple-record-projections-negproject-firstname-from-person ()

  (should (equal (Person → string) (Person → string))))

(ert-deftest test-all-negnominal-types-negeliminations-negrecord-eliminations-negsimple-record-projections-negproject-lastname-from-person ()

  (should (equal (Person → string) (Person → string))))

(ert-deftest test-all-negnominal-types-negeliminations-negrecord-eliminations-negsimple-record-projections-negproject-age-from-person ()

  (should (equal (Person → int32) (Person → int32))))

(ert-deftest test-all-negnominal-types-negeliminations-negrecord-eliminations-negsimple-record-projections-negproject-lat-from-latlon ()

  (should (equal (LatLon → float32) (LatLon → float32))))

(ert-deftest test-all-negnominal-types-negeliminations-negrecord-eliminations-negsimple-record-projections-negproject-lon-from-latlon ()

  (should (equal (LatLon → float32) (LatLon → float32))))

;; Record projections applied to records

(ert-deftest test-all-negnominal-types-negeliminations-negrecord-eliminations-negrecord-projections-applied-to-records-negproject-firstname-applied-to-person-record ()

  (should (equal string string)))

(ert-deftest test-all-negnominal-types-negeliminations-negrecord-eliminations-negrecord-projections-applied-to-records-negproject-age-applied-to-person-record ()

  (should (equal int32 int32)))

(ert-deftest test-all-negnominal-types-negeliminations-negrecord-eliminations-negrecord-projections-applied-to-records-negproject-lat-applied-to-latlon-record ()

  (should (equal float32 float32)))

;; Polymorphic record projections

(ert-deftest test-all-negnominal-types-negeliminations-negrecord-eliminations-negpolymorphic-record-projections-negproject-lat-from-polymorphic-latlonpoly ()

  (should (equal (∀t0.((LatLonPoly @ t0) → t0)) (∀t0.((LatLonPoly @ t0) → t0)))))

(ert-deftest test-all-negnominal-types-negeliminations-negrecord-eliminations-negpolymorphic-record-projections-negproject-lon-from-polymorphic-latlonpoly ()

  (should (equal (∀t0.((LatLonPoly @ t0) → t0)) (∀t0.((LatLonPoly @ t0) → t0)))))

(ert-deftest test-all-negnominal-types-negeliminations-negrecord-eliminations-negpolymorphic-record-projections-negproject-head-from-buddylista ()

  (should (equal (∀t0.((BuddyListA @ t0) → t0)) (∀t0.((BuddyListA @ t0) → t0)))))

(ert-deftest test-all-negnominal-types-negeliminations-negrecord-eliminations-negpolymorphic-record-projections-negproject-tail-from-buddylista ()

  (should (equal (∀t0.((BuddyListA @ t0) → maybe<(BuddyListB @ t0)>)) (∀t0.((BuddyListA @ t0) → maybe<(BuddyListB @ t0)>)))))

;; Polymorphic record projections applied

(ert-deftest test-all-negnominal-types-negeliminations-negrecord-eliminations-negpolymorphic-record-projections-applied-negproject-lat-from-latlonpoly-with-int32 ()

  (should (equal int32 int32)))

(ert-deftest test-all-negnominal-types-negeliminations-negrecord-eliminations-negpolymorphic-record-projections-applied-negproject-lon-from-latlonpoly-with-float64 ()

  (should (equal float64 float64)))

(ert-deftest test-all-negnominal-types-negeliminations-negrecord-eliminations-negpolymorphic-record-projections-applied-negproject-head-from-buddylista-with-string ()

  (should (equal string string)))

;; Record projections with variables

(ert-deftest test-all-negnominal-types-negeliminations-negrecord-eliminations-negrecord-projections-with-variables-negproject-from-lambda-parameter ()

  (should (equal (Person → string) (Person → string))))

(ert-deftest test-all-negnominal-types-negeliminations-negrecord-eliminations-negrecord-projections-with-variables-negproject-from-polymorphic-lambda-parameter ()

  (should (equal (∀t0.((LatLonPoly @ t0) → t0)) (∀t0.((LatLonPoly @ t0) → t0)))))

(ert-deftest test-all-negnominal-types-negeliminations-negrecord-eliminations-negrecord-projections-with-variables-negmultiple-projections-from-same-record ()

  (should (equal (Person → (string, string)) (Person → (string, string)))))

;; Record projections in complex contexts

(ert-deftest test-all-negnominal-types-negeliminations-negrecord-eliminations-negrecord-projections-in-complex-contexts-negprojection-in-let-binding ()

  (should (equal string string)))

(ert-deftest test-all-negnominal-types-negeliminations-negrecord-eliminations-negrecord-projections-in-complex-contexts-negprojection-in-tuple ()

  (should (equal ((Person → string), (Person → int32)) ((Person → string), (Person → int32)))))

(ert-deftest test-all-negnominal-types-negeliminations-negrecord-eliminations-negrecord-projections-in-complex-contexts-negprojection-in-list ()

  (should (equal list<(Person → string)> list<(Person → string)>)))

;; Multi-parameter polymorphic projections

(ert-deftest test-all-negnominal-types-negeliminations-negrecord-eliminations-negmulti-negparameter-polymorphic-projections-negproject-first-from-triple ()

  (should (equal (∀t0.(∀t1.(∀t2.((Triple @ t0 @ t1 @ t2) → t0)))) (∀t0.(∀t1.(∀t2.((Triple @ t0 @ t1 @ t2) → t0)))))))

(ert-deftest test-all-negnominal-types-negeliminations-negrecord-eliminations-negmulti-negparameter-polymorphic-projections-negproject-second-from-triple-applied ()

  (should (equal string string)))

(ert-deftest test-all-negnominal-types-negeliminations-negrecord-eliminations-negmulti-negparameter-polymorphic-projections-negproject-from-triple-and-use-second-field-which-is-another-polymorphic-record ()

  (should (equal (∀t0.(∀t1.(∀t2.(∀t3.((Triple @ t0 @ (PersonOrSomething @ map<t1, t2>) @ t3) → t1 → maybe<t2>))))) (∀t0.(∀t1.(∀t2.(∀t3.((Triple @ t0 @ (PersonOrSomething @ map<t1, t2>) @ t3) → t1 → maybe<t2>))))))))

;; Higher-order record projections

(ert-deftest test-all-negnominal-types-negeliminations-negrecord-eliminations-neghigher-negorder-record-projections-negmap-projection-over-list-of-records ()

  (should (equal list<string> list<string>)))

(ert-deftest test-all-negnominal-types-negeliminations-negrecord-eliminations-neghigher-negorder-record-projections-negmap-polymorphic-projection ()

  (should (equal list<int32> list<int32>)))

(ert-deftest test-all-negnominal-types-negeliminations-negrecord-eliminations-neghigher-negorder-record-projections-negfilter-using-projection ()

  (should (equal list<Person> list<Person>)))

;; Recursive record projections

(ert-deftest test-all-negnominal-types-negeliminations-negrecord-eliminations-negrecursive-record-projections-negnested-projection-from-recursive-record ()

  (should (equal (IntList → int32) (IntList → int32))))

;; Record projections with mutual recursion

(ert-deftest test-all-negnominal-types-negeliminations-negrecord-eliminations-negrecord-projections-with-mutual-recursion-negproject-head-from-buddylista ()

  (should (equal (∀t0.((BuddyListA @ t0) → t0)) (∀t0.((BuddyListA @ t0) → t0)))))

(ert-deftest test-all-negnominal-types-negeliminations-negrecord-eliminations-negrecord-projections-with-mutual-recursion-negproject-tail-from-buddylistb ()

  (should (equal (∀t0.((BuddyListB @ t0) → maybe<(BuddyListA @ t0)>)) (∀t0.((BuddyListB @ t0) → maybe<(BuddyListA @ t0)>)))))

(ert-deftest test-all-negnominal-types-negeliminations-negrecord-eliminations-negrecord-projections-with-mutual-recursion-negchained-projections-across-mutual-recursion ()

  (should (equal (∀t0.((BuddyListA @ t0) → maybe<(BuddyListB @ t0)>)) (∀t0.((BuddyListA @ t0) → maybe<(BuddyListB @ t0)>)))))

;; Projections with variables

(ert-deftest test-all-negnominal-types-negeliminations-negrecord-eliminations-negprojections-with-variables-negproject-from-lambda-parameter ()

  (should (equal (Person → string) (Person → string))))

(ert-deftest test-all-negnominal-types-negeliminations-negrecord-eliminations-negprojections-with-variables-negproject-from-polymorphic-lambda-parameter ()

  (should (equal (∀t0.((LatLonPoly @ t0) → t0)) (∀t0.((LatLonPoly @ t0) → t0)))))

(ert-deftest test-all-negnominal-types-negeliminations-negrecord-eliminations-negprojections-with-variables-negmultiple-projections-from-same-record ()

  (should (equal (Person → (string, string)) (Person → (string, string)))))

;; Union eliminations

;; Simple unit inject eliminations

(ert-deftest test-all-negnominal-types-negeliminations-negunion-eliminations-negsimple-unit-inject-eliminations-negmatch-comparison-with-all-cases ()

  (should (equal (Comparison → string) (Comparison → string))))

(ert-deftest test-all-negnominal-types-negeliminations-negunion-eliminations-negsimple-unit-inject-eliminations-negmatch-comparison-returning-int32 ()

  (should (equal (Comparison → int32) (Comparison → int32))))

(ert-deftest test-all-negnominal-types-negeliminations-negunion-eliminations-negsimple-unit-inject-eliminations-negmatch-applied-to-comparison-variant ()

  (should (equal string string)))

;; Union eliminations with data

(ert-deftest test-all-negnominal-types-negeliminations-negunion-eliminations-negunion-eliminations-with-data-negmatch-number-extracting-int-values ()

  (should (equal (Number → int32) (Number → int32))))

(ert-deftest test-all-negnominal-types-negeliminations-negunion-eliminations-negunion-eliminations-with-data-negmatch-number-converting-to-string ()

  (should (equal (Number → string) (Number → string))))

(ert-deftest test-all-negnominal-types-negeliminations-negunion-eliminations-negunion-eliminations-with-data-negmatch-number-applied-to-int-variant ()

  (should (equal int32 int32)))

(ert-deftest test-all-negnominal-types-negeliminations-negunion-eliminations-negunion-eliminations-with-data-negmatch-timestamp-with-mixed-data-types ()

  (should (equal (Timestamp → string) (Timestamp → string))))

;; Polymorphic union eliminations

;; Simple polymorphic unions

(ert-deftest test-all-negnominal-types-negeliminations-negunion-eliminations-negpolymorphic-union-eliminations-negsimple-polymorphic-unions-negmatch-personorsomething-with-string ()

  (should (equal ((PersonOrSomething @ string) → string) ((PersonOrSomething @ string) → string))))

(ert-deftest test-all-negnominal-types-negeliminations-negunion-eliminations-negpolymorphic-union-eliminations-negsimple-polymorphic-unions-negmatch-personorsomething-instantiated-with-string ()

  (should (equal string string)))

;; using UnionPolymorphicRecursive

(ert-deftest test-all-negnominal-types-negeliminations-negunion-eliminations-negpolymorphic-union-eliminations-negusing-unionpolymorphicrecursive-negnon-negapplied-unionpolymorphicrecursive ()

  (should (equal ((UnionPolymorphicRecursive @ int32) → string) ((UnionPolymorphicRecursive @ int32) → string))))

(ert-deftest test-all-negnominal-types-negeliminations-negunion-eliminations-negpolymorphic-union-eliminations-negusing-unionpolymorphicrecursive-negapplied-unionpolymorphicrecursive-with-int32 ()

  (should (equal string string)))

(ert-deftest test-all-negnominal-types-negeliminations-negunion-eliminations-negpolymorphic-union-eliminations-negusing-unionpolymorphicrecursive-negapplied-unionpolymorphicrecursive-with-int32-in-lambda ()

  (should (equal ((UnionPolymorphicRecursive @ int32) → string) ((UnionPolymorphicRecursive @ int32) → string))))

(ert-deftest test-all-negnominal-types-negeliminations-negunion-eliminations-negpolymorphic-union-eliminations-negusing-unionpolymorphicrecursive-negapplied-generic-unionpolymorphicrecursive-in-lambda ()

  (should (equal (∀t0.((UnionPolymorphicRecursive @ t0) → string)) (∀t0.((UnionPolymorphicRecursive @ t0) → string)))))

;; Using kernel types

(ert-deftest test-all-negnominal-types-negeliminations-negunion-eliminations-negpolymorphic-union-eliminations-negusing-kernel-types-negcase-statement-on-coderdirection-applied-to-argument ()

  (should (equal (∀t0.(hydra.coders.CoderDirection → (hydra.coders.Coder @ t0 @ t0) → hydra.context.Context → t0 → either<(hydra.context.InContext @ hydra.errors.Error), t0>)) (∀t0.(hydra.coders.CoderDirection → (hydra.coders.Coder @ t0 @ t0) → hydra.context.Context → t0 → either<hydra.errors.Error, t0>)))))

;; Union eliminations with defaults

(ert-deftest test-all-negnominal-types-negeliminations-negunion-eliminations-negunion-eliminations-with-defaults-negmatch-comparison-with-default-case ()

  (should (equal (Comparison → string) (Comparison → string))))

(ert-deftest test-all-negnominal-types-negeliminations-negunion-eliminations-negunion-eliminations-with-defaults-negmatch-number-with-default-case ()

  (should (equal (Number → int32) (Number → int32))))

(ert-deftest test-all-negnominal-types-negeliminations-negunion-eliminations-negunion-eliminations-with-defaults-negmatch-unionmonomorphic-with-default ()

  (should (equal (UnionMonomorphic → string) (UnionMonomorphic → string))))

;; Nested union eliminations

(ert-deftest test-all-negnominal-types-negeliminations-negunion-eliminations-negnested-union-eliminations-negnested-match-statements ()

  (should (equal ((PersonOrSomething @ Number) → string) ((PersonOrSomething @ Number) → string))))

(ert-deftest test-all-negnominal-types-negeliminations-negunion-eliminations-negnested-union-eliminations-negmatch-in-tuple ()

  (should (equal ((Comparison → int32), string) ((Comparison → int32), string))))

;; Union eliminations in complex contexts

(ert-deftest test-all-negnominal-types-negeliminations-negunion-eliminations-negunion-eliminations-in-complex-contexts-negmatch-in-let-binding ()

  (should (equal (Comparison → string) (Comparison → string))))

(ert-deftest test-all-negnominal-types-negeliminations-negunion-eliminations-negunion-eliminations-in-complex-contexts-negmatch-in-record ()

  (should (equal Person Person)))

(ert-deftest test-all-negnominal-types-negeliminations-negunion-eliminations-negunion-eliminations-in-complex-contexts-negmatch-with-polymorphic-result-in-list ()

  (should (equal list<int32> list<int32>)))

;; Multi-parameter polymorphic case statements

(ert-deftest test-all-negnominal-types-negeliminations-negunion-eliminations-negmulti-negparameter-polymorphic-case-statements-negcase-either-converting-both-to-string ()

  (should (equal ((Either @ int32 @ float32) → string) ((Either @ int32 @ float32) → string))))

(ert-deftest test-all-negnominal-types-negeliminations-negunion-eliminations-negmulti-negparameter-polymorphic-case-statements-negcase-either-applied-to-injection ()

  (should (equal int32 int32)))

(ert-deftest test-all-negnominal-types-negeliminations-negunion-eliminations-negmulti-negparameter-polymorphic-case-statements-negcase-either-with-triple-and-nested-projections ()

  (should (equal (∀t0.(∀t1.(∀t2.(∀t3.(∀t4.((Triple @ t0 @ (Either @ (LatLonPoly @ t1) @ (Triple @ t1 @ t2 @ t3)) @ t4) → t1)))))) (∀t0.(∀t1.(∀t2.(∀t3.(∀t4.((Triple @ t0 @ (Either @ (LatLonPoly @ t1) @ (Triple @ t1 @ t2 @ t3)) @ t4) → t1)))))))))

(ert-deftest test-all-negnominal-types-negeliminations-negunion-eliminations-negmulti-negparameter-polymorphic-case-statements-negcase-either-with-polymorphic-let-bindings ()

  (should (equal ((Either @ int32 @ string) → (Either @ int32 @ int32)) ((Either @ int32 @ string) → (Either @ int32 @ int32)))))

;; Higher-order union eliminations

(ert-deftest test-all-negnominal-types-negeliminations-negunion-eliminations-neghigher-negorder-union-eliminations-negmap-match-over-list ()

  (should (equal list<string> list<string>)))

(ert-deftest test-all-negnominal-types-negeliminations-negunion-eliminations-neghigher-negorder-union-eliminations-negcompose-match-with-other-functions ()

  (should (equal (Comparison → int32) (Comparison → int32))))

(ert-deftest test-all-negnominal-types-negeliminations-negunion-eliminations-neghigher-negorder-union-eliminations-negmatch-in-lambda-body ()

  (should (equal (Number → int32) (Number → int32))))

;; Recursive union eliminations

(ert-deftest test-all-negnominal-types-negeliminations-negunion-eliminations-negrecursive-union-eliminations-negmatch-hydratype-recursively ()

  (should (equal (HydraType → string) (HydraType → string))))

;; Wrap eliminations

;; Monomorphic unwrapping

(ert-deftest test-all-negnominal-types-negeliminations-negwrap-eliminations-negmonomorphic-unwrapping-negunwrap-string-alias ()

  (should (equal (StringAlias → string) (StringAlias → string))))

;; Polymorphic unwrapping

(ert-deftest test-all-negnominal-types-negeliminations-negwrap-eliminations-negpolymorphic-unwrapping-negunwrap-polymorphic-wrapper ()

  (should (equal (∀t0.((PolymorphicWrapper @ t0) → list<t0>)) (∀t0.((PolymorphicWrapper @ t0) → list<t0>)))))

;; Unwrap eliminations in applications

(ert-deftest test-all-negnominal-types-negeliminations-negwrap-eliminations-negunwrap-eliminations-in-applications-negunwrap-applied-to-wrapped-term ()

  (should (equal string string)))

(ert-deftest test-all-negnominal-types-negeliminations-negwrap-eliminations-negunwrap-eliminations-in-applications-negunwrap-polymorphic-applied ()

  (should (equal list<int32> list<int32>)))

;; Unwrap in complex contexts

(ert-deftest test-all-negnominal-types-negeliminations-negwrap-eliminations-negunwrap-in-complex-contexts-negunwrap-in-let-binding ()

  (should (equal string string)))

(ert-deftest test-all-negnominal-types-negeliminations-negwrap-eliminations-negunwrap-in-complex-contexts-negunwrap-in-tuple ()

  (should (equal ((StringAlias → string), string) ((StringAlias → string), string))))

(ert-deftest test-all-negnominal-types-negeliminations-negwrap-eliminations-negunwrap-in-complex-contexts-negunwrap-in-lambda ()

  (should (equal (StringAlias → string) (StringAlias → string))))

;; Multi-parameter polymorphic unwrappers

(ert-deftest test-all-negnominal-types-negeliminations-negwrap-eliminations-negmulti-negparameter-polymorphic-unwrappers-negunwrap-symmetric-triple-to-tuple ()

  (should (equal (∀t0.(∀t1.((SymmetricTriple @ t0 @ t1) → (t0, t0)))) (∀t0.(∀t1.((SymmetricTriple @ t0 @ t1) → (t0, t0)))))))

(ert-deftest test-all-negnominal-types-negeliminations-negwrap-eliminations-negmulti-negparameter-polymorphic-unwrappers-negunwrap-and-collect-edges-in-set ()

  (should (equal (∀t0.(∀t1.(set<(SymmetricTriple @ t0 @ t1)> → set<t1>))) (∀t0.(∀t1.(set<(SymmetricTriple @ t0 @ t1)> → set<t1>))))))

(ert-deftest test-all-negnominal-types-negeliminations-negwrap-eliminations-negmulti-negparameter-polymorphic-unwrappers-negunwrap-with-maybe-to-handle-optional-symmetric-triple ()

  (should (equal (∀t0.(∀t1.(maybe<(SymmetricTriple @ t0 @ t1)> → maybe<t1>))) (∀t0.(∀t1.(maybe<(SymmetricTriple @ t0 @ t1)> → maybe<t1>))))))

;; Chained unwrapping

(ert-deftest test-all-negnominal-types-negeliminations-negwrap-eliminations-negchained-unwrapping-negunwrap-then-process ()

  (should (equal (StringAlias → string) (StringAlias → string))))

(ert-deftest test-all-negnominal-types-negeliminations-negwrap-eliminations-negchained-unwrapping-negunwrap-polymorphic-then-map ()

  (should (equal ((PolymorphicWrapper @ int32) → list<int32>) ((PolymorphicWrapper @ int32) → list<int32>))))

;; Multiple unwrap operations

(ert-deftest test-all-negnominal-types-negeliminations-negwrap-eliminations-negmultiple-unwrap-operations-negunwrap-different-types ()

  (should (equal (∀t0.(StringAlias → (PolymorphicWrapper @ t0) → (string, list<t0>))) (∀t0.(StringAlias → (PolymorphicWrapper @ t0) → (string, list<t0>))))))
