;; Note: this is an automatically generated file. Do not edit.
;; checking

;; Advanced

;; Annotated terms

;; Top-level annotations

(defun test-all-negadvanced-negannotated-terms-negtop-neglevel-annotations-negannotated-literal ()

  (assert (equal int32 int32)))

(defun test-all-negadvanced-negannotated-terms-negtop-neglevel-annotations-negannotated-list ()

  (assert (equal list<string> list<string>)))

(defun test-all-negadvanced-negannotated-terms-negtop-neglevel-annotations-negannotated-record ()

  (assert (equal Person Person)))

(defun test-all-negadvanced-negannotated-terms-negtop-neglevel-annotations-negannotated-lambda ()

  (assert (equal (∀t0.(t0 → t0)) (∀t0.(t0 → t0)))))

;; Nested annotations

(defun test-all-negadvanced-negannotated-terms-negnested-annotations-negannotation-within-annotation ()

  (assert (equal int32 int32)))

(defun test-all-negadvanced-negannotated-terms-negnested-annotations-negannotated-terms-in-tuple ()

  (assert (equal (int32, string) (int32, string))))

(defun test-all-negadvanced-negannotated-terms-negnested-annotations-negannotated-term-in-function-application ()

  (assert (equal int32 int32)))

;; Annotations in complex contexts

(defun test-all-negadvanced-negannotated-terms-negannotations-in-complex-contexts-negannotated-let-binding ()

  (assert (equal (int32, string) (int32, string))))

(defun test-all-negadvanced-negannotated-terms-negannotations-in-complex-contexts-negannotated-record-fields ()

  (assert (equal Person Person)))

(defun test-all-negadvanced-negannotated-terms-negannotations-in-complex-contexts-negannotated-function-in-application ()

  (assert (equal int32 int32)))

;; Algebraic types

;; Unit

;; Unit term

(defun test-all-negalgebraic-types-negunit-negunit-term-negunit-literal ()

  (assert (equal unit unit)))

;; Unit term in polymorphic context

(defun test-all-negalgebraic-types-negunit-negunit-term-in-polymorphic-context-negunit-from-lambda ()

  (assert (equal (∀t0.(t0 → unit)) (∀t0.(t0 → unit)))))

;; Pairs

;; Basic pairs

(defun test-all-negalgebraic-types-negpairs-negbasic-pairs-negpair-of-int-and-string ()

  (assert (equal (int32, string) (int32, string))))

(defun test-all-negalgebraic-types-negpairs-negbasic-pairs-negpair-of-string-and-boolean ()

  (assert (equal (string, boolean) (string, boolean))))

(defun test-all-negalgebraic-types-negpairs-negbasic-pairs-negpair-of-boolean-and-int ()

  (assert (equal (boolean, int32) (boolean, int32))))

;; Polymorphic pairs

(defun test-all-negalgebraic-types-negpairs-negpolymorphic-pairs-negpair-from-lambda-first-element ()

  (assert (equal (∀t0.(t0 → (t0, string))) (∀t0.(t0 → (t0, string))))))

(defun test-all-negalgebraic-types-negpairs-negpolymorphic-pairs-negpair-from-lambda-second-element ()

  (assert (equal (∀t0.(t0 → (string, t0))) (∀t0.(t0 → (string, t0))))))

(defun test-all-negalgebraic-types-negpairs-negpolymorphic-pairs-negpair-from-two-lambdas ()

  (assert (equal (∀t0.(∀t1.(t0 → t1 → (t0, t1)))) (∀t0.(∀t1.(t0 → t1 → (t0, t1)))))))

(defun test-all-negalgebraic-types-negpairs-negpolymorphic-pairs-negpair-with-repeated-variable ()

  (assert (equal (∀t0.(t0 → (t0, t0))) (∀t0.(t0 → (t0, t0))))))

;; Pairs in complex contexts

(defun test-all-negalgebraic-types-negpairs-negpairs-in-complex-contexts-negpair-in-list ()

  (assert (equal list<(int32, string)> list<(int32, string)>)))

(defun test-all-negalgebraic-types-negpairs-negpairs-in-complex-contexts-negpair-in-let-binding ()

  (assert (equal (int32, string) (int32, string))))

;; Nested pairs

(defun test-all-negalgebraic-types-negpairs-negnested-pairs-negpair-of-pairs ()

  (assert (equal ((int32, string), (boolean, int32)) ((int32, string), (boolean, int32)))))

(defun test-all-negalgebraic-types-negpairs-negnested-pairs-negpair-with-list ()

  (assert (equal (list<int32>, string) (list<int32>, string))))

(defun test-all-negalgebraic-types-negpairs-negnested-pairs-neglist-of-pairs ()

  (assert (equal list<(int32, string)> list<(int32, string)>)))

;; Pairs with complex types

(defun test-all-negalgebraic-types-negpairs-negpairs-with-complex-types-negpair-with-record-on-first ()

  (assert (equal (Person, int32) (Person, int32))))

(defun test-all-negalgebraic-types-negpairs-negpairs-with-complex-types-negpair-with-record-on-second ()

  (assert (equal (string, Person) (string, Person))))

;; Eithers

;; Left values

(defun test-all-negalgebraic-types-negeithers-negleft-values-negleft-int ()

  (assert (equal (∀t0.either<int32, t0>) (∀t0.either<int32, t0>))))

(defun test-all-negalgebraic-types-negeithers-negleft-values-negleft-string ()

  (assert (equal (∀t0.either<string, t0>) (∀t0.either<string, t0>))))

(defun test-all-negalgebraic-types-negeithers-negleft-values-negleft-boolean ()

  (assert (equal (∀t0.either<boolean, t0>) (∀t0.either<boolean, t0>))))

;; Right values

(defun test-all-negalgebraic-types-negeithers-negright-values-negright-int ()

  (assert (equal (∀t0.either<t0, int32>) (∀t0.either<t0, int32>))))

(defun test-all-negalgebraic-types-negeithers-negright-values-negright-string ()

  (assert (equal (∀t0.either<t0, string>) (∀t0.either<t0, string>))))

(defun test-all-negalgebraic-types-negeithers-negright-values-negright-boolean ()

  (assert (equal (∀t0.either<t0, boolean>) (∀t0.either<t0, boolean>))))

;; Polymorphic eithers

(defun test-all-negalgebraic-types-negeithers-negpolymorphic-eithers-negleft-from-lambda ()

  (assert (equal (∀t0.(∀t1.(t0 → either<t0, t1>))) (∀t0.(∀t1.(t0 → either<t0, t1>))))))

(defun test-all-negalgebraic-types-negeithers-negpolymorphic-eithers-negright-from-lambda ()

  (assert (equal (∀t0.(∀t1.(t0 → either<t1, t0>))) (∀t0.(∀t1.(t0 → either<t1, t0>))))))

(defun test-all-negalgebraic-types-negeithers-negpolymorphic-eithers-negeither-from-two-lambdas ()

  (assert (equal (∀t0.(boolean → t0 → either<t0, t0>)) (∀t0.(boolean → t0 → either<t0, t0>)))))

;; Eithers in complex contexts

(defun test-all-negalgebraic-types-negeithers-negeithers-in-complex-contexts-negeither-in-list ()

  (assert (equal list<either<string, int32>> list<either<string, int32>>)))

(defun test-all-negalgebraic-types-negeithers-negeithers-in-complex-contexts-negeither-in-let-binding ()

  (assert (equal (∀t0.either<t0, int32>) (∀t0.either<t0, int32>))))

;; Nested eithers

(defun test-all-negalgebraic-types-negeithers-negnested-eithers-negeither-of-either-left-left ()

  (assert (equal (∀t0.(∀t1.either<either<int32, t0>, t1>)) (∀t0.(∀t1.either<either<int32, t0>, t1>)))))

(defun test-all-negalgebraic-types-negeithers-negnested-eithers-negeither-of-either-left-right ()

  (assert (equal (∀t0.(∀t1.either<either<t0, string>, t1>)) (∀t0.(∀t1.either<either<t0, string>, t1>)))))

(defun test-all-negalgebraic-types-negeithers-negnested-eithers-negeither-of-either-right ()

  (assert (equal (∀t0.either<t0, boolean>) (∀t0.either<t0, boolean>))))

(defun test-all-negalgebraic-types-negeithers-negnested-eithers-negeither-of-list ()

  (assert (equal (∀t0.either<list<int32>, t0>) (∀t0.either<list<int32>, t0>))))

(defun test-all-negalgebraic-types-negeithers-negnested-eithers-neglist-of-eithers ()

  (assert (equal list<either<string, int32>> list<either<string, int32>>)))

;; Eithers with complex types

(defun test-all-negalgebraic-types-negeithers-negeithers-with-complex-types-negeither-with-record-on-left ()

  (assert (equal (∀t0.either<Person, t0>) (∀t0.either<Person, t0>))))

(defun test-all-negalgebraic-types-negeithers-negeithers-with-complex-types-negeither-with-record-on-right ()

  (assert (equal (∀t0.either<t0, Person>) (∀t0.either<t0, Person>))))

;; Optionals

;; Monomorphic optionals

(defun test-all-negalgebraic-types-negoptionals-negmonomorphic-optionals-negnothing ()

  (assert (equal (∀t0.maybe<t0>) (∀t0.maybe<t0>))))

(defun test-all-negalgebraic-types-negoptionals-negmonomorphic-optionals-negjust-int ()

  (assert (equal maybe<int32> maybe<int32>)))

(defun test-all-negalgebraic-types-negoptionals-negmonomorphic-optionals-negjust-string ()

  (assert (equal maybe<string> maybe<string>)))

(defun test-all-negalgebraic-types-negoptionals-negmonomorphic-optionals-negjust-boolean ()

  (assert (equal maybe<boolean> maybe<boolean>)))

;; Polymorphic optionals

(defun test-all-negalgebraic-types-negoptionals-negpolymorphic-optionals-negoptional-from-lambda ()

  (assert (equal (∀t0.(t0 → maybe<t0>)) (∀t0.(t0 → maybe<t0>)))))

(defun test-all-negalgebraic-types-negoptionals-negpolymorphic-optionals-negnothing-from-lambda ()

  (assert (equal (∀t0.(∀t1.(t0 → maybe<t1>))) (∀t0.(∀t1.(t0 → maybe<t1>))))))

(defun test-all-negalgebraic-types-negoptionals-negpolymorphic-optionals-negconditional-optional ()

  (assert (equal (∀t0.(t0 → boolean → maybe<t0>)) (∀t0.(t0 → boolean → maybe<t0>)))))

;; Optionals in complex contexts

(defun test-all-negalgebraic-types-negoptionals-negoptionals-in-complex-contexts-negoptional-in-record ()

  (assert (equal (BuddyListA @ string) (BuddyListA @ string))))

(defun test-all-negalgebraic-types-negoptionals-negoptionals-in-complex-contexts-negoptional-in-let-binding ()

  (assert (equal maybe<int32> maybe<int32>)))

;; Nested optionals

(defun test-all-negalgebraic-types-negoptionals-negnested-optionals-negoptional-of-optional ()

  (assert (equal maybe<maybe<string>> maybe<maybe<string>>)))

(defun test-all-negalgebraic-types-negoptionals-negnested-optionals-negoptional-of-list ()

  (assert (equal maybe<list<int32>> maybe<list<int32>>)))

(defun test-all-negalgebraic-types-negoptionals-negnested-optionals-neglist-of-optionals ()

  (assert (equal list<maybe<string>> list<maybe<string>>)))

;; Optionals with complex types

(defun test-all-negalgebraic-types-negoptionals-negoptionals-with-complex-types-negoptional-map ()

  (assert (equal maybe<map<string, int32>> maybe<map<string, int32>>)))

;; Collections

;; Lists

;; Lists of literals

(defun test-all-negcollections-neglists-neglists-of-literals-negint-list ()

  (assert (equal list<int32> list<int32>)))

(defun test-all-negcollections-neglists-neglists-of-literals-negstring-list ()

  (assert (equal list<string> list<string>)))

(defun test-all-negcollections-neglists-neglists-of-literals-negsingle-element-list ()

  (assert (equal list<bigint> list<bigint>)))

(defun test-all-negcollections-neglists-neglists-of-literals-negmixed-numeric-types ()

  (assert (equal list<float32> list<float32>)))

;; Empty lists

(defun test-all-negcollections-neglists-negempty-lists-negempty-list ()

  (assert (equal (∀t0.list<t0>) (∀t0.list<t0>))))

(defun test-all-negcollections-neglists-negempty-lists-negpair-of-empty-lists ()

  (assert (equal (∀t0.(∀t1.(list<t0>, list<t1>))) (∀t0.(∀t1.(list<t0>, list<t1>))))))

(defun test-all-negcollections-neglists-negempty-lists-negempty-list-in-tuple ()

  (assert (equal (∀t0.(list<t0>, string)) (∀t0.(list<t0>, string)))))

;; Polymorphic lists

(defun test-all-negcollections-neglists-negpolymorphic-lists-neglist-from-lambda ()

  (assert (equal (∀t0.(t0 → list<t0>)) (∀t0.(t0 → list<t0>)))))

(defun test-all-negcollections-neglists-negpolymorphic-lists-neglist-with-repeated-var ()

  (assert (equal (∀t0.(t0 → list<t0>)) (∀t0.(t0 → list<t0>)))))

(defun test-all-negcollections-neglists-negpolymorphic-lists-neglist-from-two-lambdas ()

  (assert (equal (∀t0.(t0 → t0 → list<t0>)) (∀t0.(t0 → t0 → list<t0>)))))

;; Nested lists

(defun test-all-negcollections-neglists-negnested-lists-neglist-of-lists ()

  (assert (equal list<list<int32>> list<list<int32>>)))

(defun test-all-negcollections-neglists-negnested-lists-negempty-nested-lists ()

  (assert (equal (∀t0.list<list<t0>>) (∀t0.list<list<t0>>))))

(defun test-all-negcollections-neglists-negnested-lists-negnested-polymorphic ()

  (assert (equal (∀t0.(t0 → list<list<t0>>)) (∀t0.(t0 → list<list<t0>>)))))

;; Lists in complex contexts

(defun test-all-negcollections-neglists-neglists-in-complex-contexts-negmultiple-lists-in-tuple ()

  (assert (equal (list<int32>, list<string>) (list<int32>, list<string>))))

;; Sets

;; Monomorphic sets

(defun test-all-negcollections-negsets-negmonomorphic-sets-negempty-set ()

  (assert (equal (∀t0.set<t0>) (∀t0.set<t0>))))

(defun test-all-negcollections-negsets-negmonomorphic-sets-negint-set ()

  (assert (equal set<int32> set<int32>)))

(defun test-all-negcollections-negsets-negmonomorphic-sets-negstring-set ()

  (assert (equal set<string> set<string>)))

(defun test-all-negcollections-negsets-negmonomorphic-sets-negsingle-element-set ()

  (assert (equal set<boolean> set<boolean>)))

;; Polymorphic sets

(defun test-all-negcollections-negsets-negpolymorphic-sets-negset-from-lambda ()

  (assert (equal (∀t0.(t0 → set<t0>)) (∀t0.(t0 → set<t0>)))))

(defun test-all-negcollections-negsets-negpolymorphic-sets-negset-with-repeated-variable ()

  (assert (equal (∀t0.(t0 → set<t0>)) (∀t0.(t0 → set<t0>)))))

(defun test-all-negcollections-negsets-negpolymorphic-sets-negset-from-two-variables ()

  (assert (equal (∀t0.(t0 → t0 → set<t0>)) (∀t0.(t0 → t0 → set<t0>)))))

;; Sets in complex contexts

(defun test-all-negcollections-negsets-negsets-in-complex-contexts-negset-in-tuple ()

  (assert (equal (set<int32>, string) (set<int32>, string))))

(defun test-all-negcollections-negsets-negsets-in-complex-contexts-negset-in-let-binding ()

  (assert (equal set<int32> set<int32>)))

;; Nested sets

(defun test-all-negcollections-negsets-negnested-sets-negset-of-lists ()

  (assert (equal set<list<string>> set<list<string>>)))

(defun test-all-negcollections-negsets-negnested-sets-negset-of-tuples ()

  (assert (equal set<(int32, int32)> set<(int32, int32)>)))

(defun test-all-negcollections-negsets-negnested-sets-negset-of-sets ()

  (assert (equal set<set<string>> set<set<string>>)))

;; Sets with complex types

(defun test-all-negcollections-negsets-negsets-with-complex-types-negset-of-records ()

  (assert (equal set<Person> set<Person>)))

(defun test-all-negcollections-negsets-negsets-with-complex-types-negset-of-optionals ()

  (assert (equal set<maybe<int32>> set<maybe<int32>>)))

(defun test-all-negcollections-negsets-negsets-with-complex-types-negset-of-maps ()

  (assert (equal set<map<string, int32>> set<map<string, int32>>)))

;; Maps

;; Monomorphic maps

(defun test-all-negcollections-negmaps-negmonomorphic-maps-negempty-map ()

  (assert (equal (∀t0.(∀t1.map<t0, t1>)) (∀t0.(∀t1.map<t0, t1>)))))

(defun test-all-negcollections-negmaps-negmonomorphic-maps-negint-to-string-map ()

  (assert (equal map<int32, string> map<int32, string>)))

(defun test-all-negcollections-negmaps-negmonomorphic-maps-negstring-to-int-map ()

  (assert (equal map<string, int32> map<string, int32>)))

(defun test-all-negcollections-negmaps-negmonomorphic-maps-negsingle-entry-map ()

  (assert (equal map<bigint, boolean> map<bigint, boolean>)))

;; Polymorphic maps

(defun test-all-negcollections-negmaps-negpolymorphic-maps-negmap-from-lambda-keys ()

  (assert (equal (∀t0.(t0 → map<t0, string>)) (∀t0.(t0 → map<t0, string>)))))

(defun test-all-negcollections-negmaps-negpolymorphic-maps-negmap-from-lambda-values ()

  (assert (equal (∀t0.(t0 → map<string, t0>)) (∀t0.(t0 → map<string, t0>)))))

(defun test-all-negcollections-negmaps-negpolymorphic-maps-negmap-from-lambda-both ()

  (assert (equal (∀t0.(∀t1.(t0 → t1 → map<t0, t1>))) (∀t0.(∀t1.(t0 → t1 → map<t0, t1>))))))

(defun test-all-negcollections-negmaps-negpolymorphic-maps-negmap-with-repeated-variables ()

  (assert (equal (∀t0.(t0 → map<t0, t0>)) (∀t0.(t0 → map<t0, t0>)))))

;; Maps in complex contexts

(defun test-all-negcollections-negmaps-negmaps-in-complex-contexts-negmap-in-tuple ()

  (assert (equal (map<int32, string>, string) (map<int32, string>, string))))

(defun test-all-negcollections-negmaps-negmaps-in-complex-contexts-negnested-maps ()

  (assert (equal map<string, map<int32, boolean>> map<string, map<int32, boolean>>)))

(defun test-all-negcollections-negmaps-negmaps-in-complex-contexts-negmap-in-let-binding ()

  (assert (equal map<string, int32> map<string, int32>)))

;; Maps with complex types

(defun test-all-negcollections-negmaps-negmaps-with-complex-types-negmap-of-records ()

  (assert (equal map<string, Person> map<string, Person>)))

(defun test-all-negcollections-negmaps-negmaps-with-complex-types-negmap-of-lists ()

  (assert (equal map<int32, list<string>> map<int32, list<string>>)))

(defun test-all-negcollections-negmaps-negmaps-with-complex-types-negmap-of-tuples ()

  (assert (equal map<string, (int32, int32)> map<string, (int32, int32)>)))

;; Fundamentals

;; Literals

;; Boolean literals

(defun test-all-negfundamentals-negliterals-negboolean-literals-negtrue ()

  (assert (equal boolean boolean)))

(defun test-all-negfundamentals-negliterals-negboolean-literals-negfalse ()

  (assert (equal boolean boolean)))

;; String literals

(defun test-all-negfundamentals-negliterals-negstring-literals-negsimple-string ()

  (assert (equal string string)))

(defun test-all-negfundamentals-negliterals-negstring-literals-negempty-string ()

  (assert (equal string string)))

(defun test-all-negfundamentals-negliterals-negstring-literals-negunicode-string ()

  (assert (equal string string)))

;; Integer literals

(defun test-all-negfundamentals-negliterals-neginteger-literals-negbigint ()

  (assert (equal bigint bigint)))

(defun test-all-negfundamentals-negliterals-neginteger-literals-negint8 ()

  (assert (equal int8 int8)))

(defun test-all-negfundamentals-negliterals-neginteger-literals-negint16 ()

  (assert (equal int16 int16)))

(defun test-all-negfundamentals-negliterals-neginteger-literals-negint32 ()

  (assert (equal int32 int32)))

(defun test-all-negfundamentals-negliterals-neginteger-literals-negint64 ()

  (assert (equal int64 int64)))

(defun test-all-negfundamentals-negliterals-neginteger-literals-neguint8 ()

  (assert (equal uint8 uint8)))

(defun test-all-negfundamentals-negliterals-neginteger-literals-neguint16 ()

  (assert (equal uint16 uint16)))

(defun test-all-negfundamentals-negliterals-neginteger-literals-neguint32 ()

  (assert (equal uint32 uint32)))

(defun test-all-negfundamentals-negliterals-neginteger-literals-neguint64 ()

  (assert (equal uint64 uint64)))

;; Float literals

(defun test-all-negfundamentals-negliterals-negfloat-literals-negbigfloat ()

  (assert (equal bigfloat bigfloat)))

(defun test-all-negfundamentals-negliterals-negfloat-literals-negfloat32 ()

  (assert (equal float32 float32)))

(defun test-all-negfundamentals-negliterals-negfloat-literals-negfloat64 ()

  (assert (equal float64 float64)))

;; Literals in complex contexts

(defun test-all-negfundamentals-negliterals-negliterals-in-complex-contexts-negliterals-in-tuple ()

  (assert (equal (boolean, (string, (int32, float32))) (boolean, (string, (int32, float32))))))

(defun test-all-negfundamentals-negliterals-negliterals-in-complex-contexts-negliterals-in-list ()

  (assert (equal list<string> list<string>)))

;; Variables

;; Simple variable lookup

(defun test-all-negfundamentals-negvariables-negsimple-variable-lookup-negint-variable ()

  (assert (equal (∀t0.(t0 → t0)) (∀t0.(t0 → t0)))))

(defun test-all-negfundamentals-negvariables-negsimple-variable-lookup-negvariable-in-let-binding ()

  (assert (equal int32 int32)))

(defun test-all-negfundamentals-negvariables-negsimple-variable-lookup-negmultiple-variables ()

  (assert (equal (string, int32) (string, int32))))

;; Variable scoping

(defun test-all-negfundamentals-negvariables-negvariable-scoping-neglambda-parameter ()

  (assert (equal (∀t0.(∀t1.(t0 → t1 → t0))) (∀t0.(∀t1.(t0 → t1 → t0))))))

(defun test-all-negfundamentals-negvariables-negvariable-scoping-neglet-binding-scope ()

  (assert (equal int32 int32)))

(defun test-all-negfundamentals-negvariables-negvariable-scoping-negvariable-shadowing ()

  (assert (equal (∀t0.(t0 → t0)) (∀t0.(t0 → t0)))))

(defun test-all-negfundamentals-negvariables-negvariable-scoping-negnested-scoping ()

  (assert (equal (∀t0.(∀t1.(t0 → t1 → (t0, (t0, t1))))) (∀t0.(∀t1.(t0 → t1 → (t0, (t0, t1))))))))

;; Polymorphic variables

(defun test-all-negfundamentals-negvariables-negpolymorphic-variables-negpolymorphic-function ()

  (assert (equal (∀t0.(t0 → t0)) (∀t0.(t0 → t0)))))

(defun test-all-negfundamentals-negvariables-negpolymorphic-variables-negpolymorphic-application ()

  (assert (equal (int32, string) (int32, string))))

(defun test-all-negfundamentals-negvariables-negpolymorphic-variables-neghigher-order-polymorphic ()

  (assert (equal (∀t0.(∀t1.((t0 → t1) → t0 → t1))) (∀t0.(∀t1.((t0 → t1) → t0 → t1))))))

;; Variables in complex contexts

(defun test-all-negfundamentals-negvariables-negvariables-in-complex-contexts-negvariable-in-record ()

  (assert (equal (string → Person) (string → Person))))

(defun test-all-negfundamentals-negvariables-negvariables-in-complex-contexts-negvariable-in-list ()

  (assert (equal (∀t0.(t0 → list<t0>)) (∀t0.(t0 → list<t0>)))))

(defun test-all-negfundamentals-negvariables-negvariables-in-complex-contexts-negvariable-in-map ()

  (assert (equal (∀t0.(∀t1.(t0 → t1 → map<t0, t1>))) (∀t0.(∀t1.(t0 → t1 → map<t0, t1>))))))

(defun test-all-negfundamentals-negvariables-negvariables-in-complex-contexts-negvariable-in-optional ()

  (assert (equal (∀t0.(t0 → maybe<t0>)) (∀t0.(t0 → maybe<t0>)))))

;; Recursive variables

(defun test-all-negfundamentals-negvariables-negrecursive-variables-negsimple-recursion ()

  (assert (equal (int32 → int32) (int32 → int32))))

(defun test-all-negfundamentals-negvariables-negrecursive-variables-negmutual-recursion ()

  (assert (equal (int32 → int32) (int32 → int32))))

;; Lambdas

;; Simple lambdas

(defun test-all-negfundamentals-neglambdas-negsimple-lambdas-negidentity-function ()

  (assert (equal (∀t0.(t0 → t0)) (∀t0.(t0 → t0)))))

(defun test-all-negfundamentals-neglambdas-negsimple-lambdas-negconstant-function ()

  (assert (equal (∀t0.(t0 → int32)) (∀t0.(t0 → int32)))))

;; Multi-parameter lambdas

(defun test-all-negfundamentals-neglambdas-negmulti-negparameter-lambdas-negtwo-parameters ()

  (assert (equal (∀t0.(∀t1.(t0 → t1 → t0))) (∀t0.(∀t1.(t0 → t1 → t0))))))

(defun test-all-negfundamentals-neglambdas-negmulti-negparameter-lambdas-negthree-parameters ()

  (assert (equal (∀t0.(∀t1.(∀t2.(t0 → t1 → t2 → t1)))) (∀t0.(∀t1.(∀t2.(t0 → t1 → t2 → t1)))))))

(defun test-all-negfundamentals-neglambdas-negmulti-negparameter-lambdas-negparameter-reuse ()

  (assert (equal (∀t0.(∀t1.(t0 → t1 → (t0, (t0, t1))))) (∀t0.(∀t1.(t0 → t1 → (t0, (t0, t1))))))))

;; Lambdas with operations

(defun test-all-negfundamentals-neglambdas-neglambdas-with-operations-neglambda-with-primitive ()

  (assert (equal (int32 → int32) (int32 → int32))))

(defun test-all-negfundamentals-neglambdas-neglambdas-with-operations-neglambda-with-application ()

  (assert (equal (∀t0.(∀t1.((t0 → t1) → t0 → t1))) (∀t0.(∀t1.((t0 → t1) → t0 → t1))))))

(defun test-all-negfundamentals-neglambdas-neglambdas-with-operations-neglambda-with-construction ()

  (assert (equal (∀t0.(∀t1.(t0 → t1 → (t0, t1)))) (∀t0.(∀t1.(t0 → t1 → (t0, t1)))))))

;; Nested lambdas

(defun test-all-negfundamentals-neglambdas-negnested-lambdas-neglambda-returning-lambda ()

  (assert (equal (∀t0.(∀t1.(∀t2.(t0 → t1 → t2 → t0)))) (∀t0.(∀t1.(∀t2.(t0 → t1 → t2 → t0)))))))

(defun test-all-negfundamentals-neglambdas-negnested-lambdas-neglambda-with-let-binding ()

  (assert (equal (∀t0.(t0 → t0)) (∀t0.(t0 → t0)))))

(defun test-all-negfundamentals-neglambdas-negnested-lambdas-neglambda-with-inner-lambda ()

  (assert (equal (∀t0.(t0 → t0)) (∀t0.(t0 → t0)))))

;; Lambdas in complex contexts

(defun test-all-negfundamentals-neglambdas-neglambdas-in-complex-contexts-neglambda-in-tuple ()

  (assert (equal (∀t0.((t0 → t0), int32)) (∀t0.((t0 → t0), int32)))))

(defun test-all-negfundamentals-neglambdas-neglambdas-in-complex-contexts-neglambda-in-list ()

  (assert (equal list<(int32 → int32)> list<(int32 → int32)>)))

(defun test-all-negfundamentals-neglambdas-neglambdas-in-complex-contexts-neglambda-in-record ()

  (assert (equal (string → Person) (string → Person))))

;; Higher-order lambdas

(defun test-all-negfundamentals-neglambdas-neghigher-negorder-lambdas-negfunction-composition ()

  (assert (equal (∀t0.(∀t1.(∀t2.((t0 → t1) → (t2 → t0) → t2 → t1)))) (∀t0.(∀t1.(∀t2.((t0 → t1) → (t2 → t0) → t2 → t1)))))))

(defun test-all-negfundamentals-neglambdas-neghigher-negorder-lambdas-negfunction-application ()

  (assert (equal (∀t0.(∀t1.((t0 → t1) → t0 → t1))) (∀t0.(∀t1.((t0 → t1) → t0 → t1))))))

(defun test-all-negfundamentals-neglambdas-neghigher-negorder-lambdas-negcurried-function ()

  (assert (equal (∀t0.(boolean → t0 → t0 → t0)) (∀t0.(boolean → t0 → t0 → t0)))))

;; Applications

;; Simple function applications

(defun test-all-negfundamentals-negapplications-negsimple-function-applications-negidentity-application ()

  (assert (equal int32 int32)))

(defun test-all-negfundamentals-negapplications-negsimple-function-applications-negprimitive-application ()

  (assert (equal int32 int32)))

(defun test-all-negfundamentals-negapplications-negsimple-function-applications-negstring-concatenation ()

  (assert (equal string string)))

;; Partial applications

(defun test-all-negfundamentals-negapplications-negpartial-applications-negpartially-applied-add ()

  (assert (equal (int32 → int32) (int32 → int32))))

(defun test-all-negfundamentals-negapplications-negpartial-applications-negpartially-applied-string-cat ()

  (assert (equal (string → string) (string → string))))

;; Higher-order applications

(defun test-all-negfundamentals-negapplications-neghigher-negorder-applications-negapply-function-to-function ()

  (assert (equal int32 int32)))

(defun test-all-negfundamentals-negapplications-neghigher-negorder-applications-negfunction-composition ()

  (assert (equal int32 int32)))

;; Polymorphic applications

(defun test-all-negfundamentals-negapplications-negpolymorphic-applications-negpolymorphic-identity ()

  (assert (equal (int32, string) (int32, string))))

(defun test-all-negfundamentals-negapplications-negpolymorphic-applications-negpolymorphic-const ()

  (assert (equal string string)))

(defun test-all-negfundamentals-negapplications-negpolymorphic-applications-negpolymorphic-flip ()

  (assert (equal string string)))

;; Applications in complex contexts

(defun test-all-negfundamentals-negapplications-negapplications-in-complex-contexts-negapplication-in-tuple ()

  (assert (equal (int32, string) (int32, string))))

(defun test-all-negfundamentals-negapplications-negapplications-in-complex-contexts-negapplication-in-record ()

  (assert (equal Person Person)))

(defun test-all-negfundamentals-negapplications-negapplications-in-complex-contexts-negapplication-in-let-binding ()

  (assert (equal int32 int32)))

(defun test-all-negfundamentals-negapplications-negapplications-in-complex-contexts-negnested-applications ()

  (assert (equal int32 int32)))

;; Applications with complex arguments

(defun test-all-negfundamentals-negapplications-negapplications-with-complex-arguments-negapplication-with-record-argument ()

  (assert (equal string string)))

(defun test-all-negfundamentals-negapplications-negapplications-with-complex-arguments-negapplication-with-list-argument ()

  (assert (equal string string)))

;; Let terms

;; Simple let bindings

(defun test-all-negfundamentals-neglet-terms-negsimple-let-bindings-negsingle-binding ()

  (assert (equal int32 int32)))

(defun test-all-negfundamentals-neglet-terms-negsimple-let-bindings-negmultiple-bindings ()

  (assert (equal (int32, string) (int32, string))))

;; Let terms with shadowing

(defun test-all-negfundamentals-neglet-terms-neglet-terms-with-shadowing-neglambda-parameter-shadowing-let-binding ()

  (assert (equal (∀t0.(t0 → t0)) (∀t0.(t0 → t0)))))

(defun test-all-negfundamentals-neglet-terms-neglet-terms-with-shadowing-negnested-lambda-shadowing ()

  (assert (equal (∀t0.(∀t1.(t0 → t1 → (t1, t0)))) (∀t0.(∀t1.(t0 → t1 → (t1, t0)))))))

(defun test-all-negfundamentals-neglet-terms-neglet-terms-with-shadowing-negmultiple-levels-of-let-shadowing ()

  (assert (equal boolean boolean)))

(defun test-all-negfundamentals-neglet-terms-neglet-terms-with-shadowing-neglet-shadowing-with-lambda-and-reference-to-outer-binding ()

  (assert (equal (∀t0.(t0 → (t0, int32))) (∀t0.(t0 → (t0, int32))))))

;; Recursive bindings

(defun test-all-negfundamentals-neglet-terms-negrecursive-bindings-negsimple-arithmetic-recursion ()

  (assert (equal int32 int32)))

;; Mutual recursion

(defun test-all-negfundamentals-neglet-terms-negmutual-recursion-negmutually-recursive-data ()

  (assert (equal (BuddyListA @ int32) (BuddyListA @ int32))))

(defun test-all-negfundamentals-neglet-terms-negmutual-recursion-neg-monomorphic-mutually-recursive-functions ()

  (assert (equal int32 int32)))

;; Nested let terms

(defun test-all-negfundamentals-neglet-terms-negnested-let-terms-negmonomorphic-nesting ()

  (assert (equal int32 int32)))

(defun test-all-negfundamentals-neglet-terms-negnested-let-terms-negpolymorphic-nesting ()

  (assert (equal string string)))

(defun test-all-negfundamentals-neglet-terms-negnested-let-terms-negvariable-capture-avoidance ()

  (assert (equal (∀t0.(t0 → t0)) (∀t0.(t0 → t0)))))

(defun test-all-negfundamentals-neglet-terms-negnested-let-terms-negsimple-let-in-lambda ()

  (assert (equal (∀t0.(t0 → t0)) (∀t0.(t0 → t0)))))

;; Let with complex expressions

(defun test-all-negfundamentals-neglet-terms-neglet-with-complex-expressions-neglet-in-record ()

  (assert (equal Person Person)))

(defun test-all-negfundamentals-neglet-terms-neglet-with-complex-expressions-neglet-in-function-application ()

  (assert (equal int32 int32)))

(defun test-all-negfundamentals-neglet-terms-neglet-with-complex-expressions-negpolymorphic-let-binding ()

  (assert (equal (int32, string) (int32, string))))

(defun test-all-negfundamentals-neglet-terms-neglet-with-complex-expressions-negcomposition ()

  (assert (equal int32 int32)))

;; Primitives

;; Nullary primitives

(defun test-all-negfundamentals-negprimitives-negnullary-primitives-negempty-map ()

  (assert (equal (∀t0.(∀t1.map<t0, t1>)) (∀t0.(∀t1.map<t0, t1>)))))

(defun test-all-negfundamentals-negprimitives-negnullary-primitives-negempty-set ()

  (assert (equal (∀t0.set<t0>) (∀t0.set<t0>))))

;; Unary primitives

(defun test-all-negfundamentals-negprimitives-negunary-primitives-neglists-head ()

  (assert (equal (∀t0.(list<t0> → t0)) (∀t0.(list<t0> → t0)))))

(defun test-all-negfundamentals-negprimitives-negunary-primitives-negmath-neg ()

  (assert (equal (int32 → int32) (int32 → int32))))

(defun test-all-negfundamentals-negprimitives-negunary-primitives-neglogic-not ()

  (assert (equal (boolean → boolean) (boolean → boolean))))

;; Binary primitives

(defun test-all-negfundamentals-negprimitives-negbinary-primitives-negmath-add ()

  (assert (equal (int32 → int32 → int32) (int32 → int32 → int32))))

(defun test-all-negfundamentals-negprimitives-negbinary-primitives-neglists-cons ()

  (assert (equal (∀t0.(t0 → list<t0> → list<t0>)) (∀t0.(t0 → list<t0> → list<t0>)))))

(defun test-all-negfundamentals-negprimitives-negbinary-primitives-negmaps-insert ()

  (assert (equal (∀t0.(∀t1.(t0 → t1 → map<t0, t1> → map<t0, t1>))) (∀t0.(∀t1.(t0 → t1 → map<t0, t1> → map<t0, t1>))))))

;; Ternary primitives

(defun test-all-negfundamentals-negprimitives-negternary-primitives-neglogic-ifelse ()

  (assert (equal (∀t0.(boolean → t0 → t0 → t0)) (∀t0.(boolean → t0 → t0 → t0)))))

(defun test-all-negfundamentals-negprimitives-negternary-primitives-neglists-foldl ()

  (assert (equal (∀t0.(∀t1.((t0 → t1 → t0) → t0 → list<t1> → t0))) (∀t0.(∀t1.((t0 → t1 → t0) → t0 → list<t1> → t0))))))

;; Monomorphic vs polymorphic

(defun test-all-negfundamentals-negprimitives-negmonomorphic-vs-polymorphic-negmonomorphic-math ()

  (assert (equal (int32 → int32 → int32) (int32 → int32 → int32))))

(defun test-all-negfundamentals-negprimitives-negmonomorphic-vs-polymorphic-negpolymorphic-identity ()

  (assert (equal (∀t0.(t0 → t0)) (∀t0.(t0 → t0)))))

(defun test-all-negfundamentals-negprimitives-negmonomorphic-vs-polymorphic-negpolymorphic-map ()

  (assert (equal (∀t0.(∀t1.((t0 → t1) → list<t0> → list<t1>))) (∀t0.(∀t1.((t0 → t1) → list<t0> → list<t1>))))))

;; Higher-order primitives

(defun test-all-negfundamentals-negprimitives-neghigher-negorder-primitives-neglists-map-function ()

  (assert (equal (list<int32> → list<int32>) (list<int32> → list<int32>))))

(defun test-all-negfundamentals-negprimitives-neghigher-negorder-primitives-neglists-filter ()

  (assert (equal (∀t0.((t0 → boolean) → list<t0> → list<t0>)) (∀t0.((t0 → boolean) → list<t0> → list<t0>)))))

(defun test-all-negfundamentals-negprimitives-neghigher-negorder-primitives-negoptionals-maybe ()

  (assert (equal (∀t0.(∀t1.(t0 → (t1 → t0) → maybe<t1> → t0))) (∀t0.(∀t1.(t0 → (t1 → t0) → maybe<t1> → t0))))))

;; Primitives in complex contexts

(defun test-all-negfundamentals-negprimitives-negprimitives-in-complex-contexts-negprimitive-composition ()

  (assert (equal list<int32> list<int32>)))

(defun test-all-negfundamentals-negprimitives-negprimitives-in-complex-contexts-negnested-higher-negorder ()

  (assert (equal list<list<int32>> list<list<int32>>)))

;; Nominal types

;; Records

;; Monomorphic records

(defun test-all-negnominal-types-negrecords-negmonomorphic-records-neglatlon-record ()

  (assert (equal LatLon LatLon)))

(defun test-all-negnominal-types-negrecords-negmonomorphic-records-neglatlon-with-variable ()

  (assert (equal (float32 → LatLon) (float32 → LatLon))))

(defun test-all-negnominal-types-negrecords-negmonomorphic-records-negperson-record ()

  (assert (equal Person Person)))

(defun test-all-negnominal-types-negrecords-negmonomorphic-records-negempty-record ()

  (assert (equal Unit Unit)))

(defun test-all-negnominal-types-negrecords-negmonomorphic-records-negperson-with-variables ()

  (assert (equal (string → int32 → Person) (string → int32 → Person))))

;; Polymorphic records

(defun test-all-negnominal-types-negrecords-negpolymorphic-records-neglatlon-poly-float ()

  (assert (equal (LatLonPoly @ float32) (LatLonPoly @ float32))))

(defun test-all-negnominal-types-negrecords-negpolymorphic-records-neglatlon-poly-int64 ()

  (assert (equal (LatLonPoly @ int64) (LatLonPoly @ int64))))

(defun test-all-negnominal-types-negrecords-negpolymorphic-records-neglatlon-poly-variable ()

  (assert (equal (∀t0.(t0 → (LatLonPoly @ t0))) (∀t0.(t0 → (LatLonPoly @ t0))))))

(defun test-all-negnominal-types-negrecords-negpolymorphic-records-negbuddylist-string ()

  (assert (equal (BuddyListA @ string) (BuddyListA @ string))))

(defun test-all-negnominal-types-negrecords-negpolymorphic-records-negbuddylist-variable ()

  (assert (equal (∀t0.(t0 → (BuddyListA @ t0))) (∀t0.(t0 → (BuddyListA @ t0))))))

;; Records in complex contexts

(defun test-all-negnominal-types-negrecords-negrecords-in-complex-contexts-negrecords-in-tuple ()

  (assert (equal (Person, LatLon) (Person, LatLon))))

(defun test-all-negnominal-types-negrecords-negrecords-in-complex-contexts-negpoly-records-in-tuple ()

  (assert (equal ((LatLonPoly @ int32), (BuddyListA @ string)) ((LatLonPoly @ int32), (BuddyListA @ string)))))

(defun test-all-negnominal-types-negrecords-negrecords-in-complex-contexts-negrecursive-record ()

  (assert (equal IntList IntList)))

;; Multi-parameter polymorphic records

(defun test-all-negnominal-types-negrecords-negmulti-negparameter-polymorphic-records-negtriple-with-three-monomorphic-types ()

  (assert (equal (Triple @ int32 @ string @ boolean) (Triple @ int32 @ string @ boolean))))

(defun test-all-negnominal-types-negrecords-negmulti-negparameter-polymorphic-records-negtriple-with-personorsomething-containing-map ()

  (assert (equal (∀t0.(∀t1.(t0 → t1 → (Triple @ string @ (PersonOrSomething @ map<t0, t1>) @ int32)))) (∀t0.(∀t1.(t0 → t1 → (Triple @ string @ (PersonOrSomething @ map<t0, t1>) @ int32)))))))

;; Unions

;; Simple union injections

(defun test-all-negnominal-types-negunions-negsimple-union-injections-neginject-into-comparison-lessthan-variant ()

  (assert (equal Comparison Comparison)))

(defun test-all-negnominal-types-negunions-negsimple-union-injections-neginject-into-comparison-equalto-variant ()

  (assert (equal Comparison Comparison)))

(defun test-all-negnominal-types-negunions-negsimple-union-injections-neginject-into-comparison-greaterthan-variant ()

  (assert (equal Comparison Comparison)))

;; Union injections with data

(defun test-all-negnominal-types-negunions-negunion-injections-with-data-neginject-into-number-int-variant ()

  (assert (equal Number Number)))

(defun test-all-negnominal-types-negunions-negunion-injections-with-data-neginject-into-number-float-variant ()

  (assert (equal Number Number)))

(defun test-all-negnominal-types-negunions-negunion-injections-with-data-neginject-into-timestamp-unixtimemillis-variant ()

  (assert (equal Timestamp Timestamp)))

(defun test-all-negnominal-types-negunions-negunion-injections-with-data-neginject-into-timestamp-date-variant ()

  (assert (equal Timestamp Timestamp)))

;; Polymorphic union injections

(defun test-all-negnominal-types-negunions-negpolymorphic-union-injections-neginject-person-into-personorsomething ()

  (assert (equal (∀t0.(PersonOrSomething @ t0)) (∀t0.(PersonOrSomething @ t0)))))

(defun test-all-negnominal-types-negunions-negpolymorphic-union-injections-neginject-string-into-personorsomething-other-variant ()

  (assert (equal (PersonOrSomething @ string) (PersonOrSomething @ string))))

(defun test-all-negnominal-types-negunions-negpolymorphic-union-injections-neginject-int-into-personorsomething-other-variant ()

  (assert (equal (PersonOrSomething @ int32) (PersonOrSomething @ int32))))

;; Polymorphic recursive union injections

(defun test-all-negnominal-types-negunions-negpolymorphic-recursive-union-injections-neginject-boolean-into-unionpolymorphicrecursive ()

  (assert (equal (∀t0.(UnionPolymorphicRecursive @ t0)) (∀t0.(UnionPolymorphicRecursive @ t0)))))

(defun test-all-negnominal-types-negunions-negpolymorphic-recursive-union-injections-neginject-string-value-into-unionpolymorphicrecursive ()

  (assert (equal (UnionPolymorphicRecursive @ string) (UnionPolymorphicRecursive @ string))))

(defun test-all-negnominal-types-negunions-negpolymorphic-recursive-union-injections-neginject-int-value-into-unionpolymorphicrecursive ()

  (assert (equal (UnionPolymorphicRecursive @ int32) (UnionPolymorphicRecursive @ int32))))

;; Polymorphic unions from lambda

(defun test-all-negnominal-types-negunions-negpolymorphic-unions-from-lambda-neglambda-creating-personorsomething-other-variant ()

  (assert (equal (∀t0.(t0 → (PersonOrSomething @ t0))) (∀t0.(t0 → (PersonOrSomething @ t0))))))

(defun test-all-negnominal-types-negunions-negpolymorphic-unions-from-lambda-neglambda-creating-unionpolymorphicrecursive-value-variant ()

  (assert (equal (∀t0.(t0 → (UnionPolymorphicRecursive @ t0))) (∀t0.(t0 → (UnionPolymorphicRecursive @ t0))))))

;; Unions in complex contexts

(defun test-all-negnominal-types-negunions-negunions-in-complex-contexts-negunion-in-tuple ()

  (assert (equal (Number, string) (Number, string))))

(defun test-all-negnominal-types-negunions-negunions-in-complex-contexts-negunion-in-list ()

  (assert (equal list<Number> list<Number>)))

(defun test-all-negnominal-types-negunions-negunions-in-complex-contexts-negpolymorphic-union-in-let-binding ()

  (assert (equal (PersonOrSomething @ string) (PersonOrSomething @ string))))

;; Multi-parameter polymorphic injections

(defun test-all-negnominal-types-negunions-negmulti-negparameter-polymorphic-injections-negeither-left-with-int ()

  (assert (equal (∀t0.(Either @ int32 @ t0)) (∀t0.(Either @ int32 @ t0)))))

(defun test-all-negnominal-types-negunions-negmulti-negparameter-polymorphic-injections-negeither-right-with-string ()

  (assert (equal (∀t0.(Either @ t0 @ string)) (∀t0.(Either @ t0 @ string)))))

(defun test-all-negnominal-types-negunions-negmulti-negparameter-polymorphic-injections-negeither-containing-latlonpoly-in-list ()

  (assert (equal (∀t0.(Either @ t0 @ list<(LatLonPoly @ int32)>)) (∀t0.(Either @ t0 @ list<(LatLonPoly @ int32)>)))))

(defun test-all-negnominal-types-negunions-negmulti-negparameter-polymorphic-injections-negeither-in-triple-in-map-with-shared-type-variables ()

  (assert (equal (∀t0.(∀t1.(∀t2.(∀t3.(∀t4.(∀t5.(t0 → t1 → t2 → map<string, (Triple @ (Either @ t0 @ t3) @ (Either @ t0 @ t4) @ (Either @ t5 @ t1))>))))))) (∀t0.(∀t1.(∀t2.(∀t3.(∀t4.(∀t5.(t0 → t1 → t2 → map<string, (Triple @ (Either @ t0 @ t3) @ (Either @ t0 @ t4) @ (Either @ t5 @ t1))>))))))))))

;; Wrapped terms

;; Monomorphic wrapped terms

(defun test-all-negnominal-types-negwrapped-terms-negmonomorphic-wrapped-terms-negstring-alias ()

  (assert (equal StringAlias StringAlias)))

(defun test-all-negnominal-types-negwrapped-terms-negmonomorphic-wrapped-terms-negwrapped-integer ()

  (assert (equal StringAlias StringAlias)))

(defun test-all-negnominal-types-negwrapped-terms-negmonomorphic-wrapped-terms-negwrapped-in-tuple ()

  (assert (equal (StringAlias, string) (StringAlias, string))))

;; Polymorphic wrapped terms

(defun test-all-negnominal-types-negwrapped-terms-negpolymorphic-wrapped-terms-negpolymorphic-wrapper-with-int ()

  (assert (equal (PolymorphicWrapper @ int32) (PolymorphicWrapper @ int32))))

(defun test-all-negnominal-types-negwrapped-terms-negpolymorphic-wrapped-terms-negpolymorphic-wrapper-with-string ()

  (assert (equal (PolymorphicWrapper @ string) (PolymorphicWrapper @ string))))

(defun test-all-negnominal-types-negwrapped-terms-negpolymorphic-wrapped-terms-negpolymorphic-wrapper-from-lambda ()

  (assert (equal (∀t0.(t0 → (PolymorphicWrapper @ t0))) (∀t0.(t0 → (PolymorphicWrapper @ t0))))))

;; Wrapped terms in complex contexts

(defun test-all-negnominal-types-negwrapped-terms-negwrapped-terms-in-complex-contexts-negwrapped-in-record ()

  (assert (equal Person Person)))

(defun test-all-negnominal-types-negwrapped-terms-negwrapped-terms-in-complex-contexts-negwrapped-in-let-binding ()

  (assert (equal StringAlias StringAlias)))

(defun test-all-negnominal-types-negwrapped-terms-negwrapped-terms-in-complex-contexts-negwrapped-in-list ()

  (assert (equal list<StringAlias> list<StringAlias>)))

;; Nested wrapped terms

(defun test-all-negnominal-types-negwrapped-terms-negnested-wrapped-terms-negwrapped-tuple ()

  (assert (equal (PolymorphicWrapper @ (int32, string)) (PolymorphicWrapper @ (int32, string)))))

(defun test-all-negnominal-types-negwrapped-terms-negnested-wrapped-terms-negwrapped-optional ()

  (assert (equal (PolymorphicWrapper @ maybe<int32>) (PolymorphicWrapper @ maybe<int32>))))

(defun test-all-negnominal-types-negwrapped-terms-negnested-wrapped-terms-negwrapped-map ()

  (assert (equal (PolymorphicWrapper @ map<string, int32>) (PolymorphicWrapper @ map<string, int32>))))

;; Multiple wrapping levels

(defun test-all-negnominal-types-negwrapped-terms-negmultiple-wrapping-levels-negwrapped-in-optional ()

  (assert (equal maybe<StringAlias> maybe<StringAlias>)))

(defun test-all-negnominal-types-negwrapped-terms-negmultiple-wrapping-levels-neglist-of-wrapped-polymorphic ()

  (assert (equal list<(PolymorphicWrapper @ int32)> list<(PolymorphicWrapper @ int32)>)))

;; Multi-parameter polymorphic wrappers

(defun test-all-negnominal-types-negwrapped-terms-negmulti-negparameter-polymorphic-wrappers-negsymmetric-triple-wrapping-simple-types ()

  (assert (equal (SymmetricTriple @ int32 @ string) (SymmetricTriple @ int32 @ string))))

(defun test-all-negnominal-types-negwrapped-terms-negmulti-negparameter-polymorphic-wrappers-negsymmetric-triple-from-lambda ()

  (assert (equal (∀t0.(∀t1.(t0 → t1 → t0 → (SymmetricTriple @ t0 @ t1)))) (∀t0.(∀t1.(t0 → t1 → t0 → (SymmetricTriple @ t0 @ t1)))))))

(defun test-all-negnominal-types-negwrapped-terms-negmulti-negparameter-polymorphic-wrappers-negsymmetric-triple-with-nested-polymorphic-types-and-foldl ()

  (assert (equal (list<int32> → list<int32> → (SymmetricTriple @ int32 @ list<list<int32>>)) (list<int32> → list<int32> → (SymmetricTriple @ int32 @ list<list<int32>>)))))

;; Eliminations

;; Record eliminations

;; Simple record projections

(defun test-all-negnominal-types-negeliminations-negrecord-eliminations-negsimple-record-projections-negproject-firstname-from-person ()

  (assert (equal (Person → string) (Person → string))))

(defun test-all-negnominal-types-negeliminations-negrecord-eliminations-negsimple-record-projections-negproject-lastname-from-person ()

  (assert (equal (Person → string) (Person → string))))

(defun test-all-negnominal-types-negeliminations-negrecord-eliminations-negsimple-record-projections-negproject-age-from-person ()

  (assert (equal (Person → int32) (Person → int32))))

(defun test-all-negnominal-types-negeliminations-negrecord-eliminations-negsimple-record-projections-negproject-lat-from-latlon ()

  (assert (equal (LatLon → float32) (LatLon → float32))))

(defun test-all-negnominal-types-negeliminations-negrecord-eliminations-negsimple-record-projections-negproject-lon-from-latlon ()

  (assert (equal (LatLon → float32) (LatLon → float32))))

;; Record projections applied to records

(defun test-all-negnominal-types-negeliminations-negrecord-eliminations-negrecord-projections-applied-to-records-negproject-firstname-applied-to-person-record ()

  (assert (equal string string)))

(defun test-all-negnominal-types-negeliminations-negrecord-eliminations-negrecord-projections-applied-to-records-negproject-age-applied-to-person-record ()

  (assert (equal int32 int32)))

(defun test-all-negnominal-types-negeliminations-negrecord-eliminations-negrecord-projections-applied-to-records-negproject-lat-applied-to-latlon-record ()

  (assert (equal float32 float32)))

;; Polymorphic record projections

(defun test-all-negnominal-types-negeliminations-negrecord-eliminations-negpolymorphic-record-projections-negproject-lat-from-polymorphic-latlonpoly ()

  (assert (equal (∀t0.((LatLonPoly @ t0) → t0)) (∀t0.((LatLonPoly @ t0) → t0)))))

(defun test-all-negnominal-types-negeliminations-negrecord-eliminations-negpolymorphic-record-projections-negproject-lon-from-polymorphic-latlonpoly ()

  (assert (equal (∀t0.((LatLonPoly @ t0) → t0)) (∀t0.((LatLonPoly @ t0) → t0)))))

(defun test-all-negnominal-types-negeliminations-negrecord-eliminations-negpolymorphic-record-projections-negproject-head-from-buddylista ()

  (assert (equal (∀t0.((BuddyListA @ t0) → t0)) (∀t0.((BuddyListA @ t0) → t0)))))

(defun test-all-negnominal-types-negeliminations-negrecord-eliminations-negpolymorphic-record-projections-negproject-tail-from-buddylista ()

  (assert (equal (∀t0.((BuddyListA @ t0) → maybe<(BuddyListB @ t0)>)) (∀t0.((BuddyListA @ t0) → maybe<(BuddyListB @ t0)>)))))

;; Polymorphic record projections applied

(defun test-all-negnominal-types-negeliminations-negrecord-eliminations-negpolymorphic-record-projections-applied-negproject-lat-from-latlonpoly-with-int32 ()

  (assert (equal int32 int32)))

(defun test-all-negnominal-types-negeliminations-negrecord-eliminations-negpolymorphic-record-projections-applied-negproject-lon-from-latlonpoly-with-float64 ()

  (assert (equal float64 float64)))

(defun test-all-negnominal-types-negeliminations-negrecord-eliminations-negpolymorphic-record-projections-applied-negproject-head-from-buddylista-with-string ()

  (assert (equal string string)))

;; Record projections with variables

(defun test-all-negnominal-types-negeliminations-negrecord-eliminations-negrecord-projections-with-variables-negproject-from-lambda-parameter ()

  (assert (equal (Person → string) (Person → string))))

(defun test-all-negnominal-types-negeliminations-negrecord-eliminations-negrecord-projections-with-variables-negproject-from-polymorphic-lambda-parameter ()

  (assert (equal (∀t0.((LatLonPoly @ t0) → t0)) (∀t0.((LatLonPoly @ t0) → t0)))))

(defun test-all-negnominal-types-negeliminations-negrecord-eliminations-negrecord-projections-with-variables-negmultiple-projections-from-same-record ()

  (assert (equal (Person → (string, string)) (Person → (string, string)))))

;; Record projections in complex contexts

(defun test-all-negnominal-types-negeliminations-negrecord-eliminations-negrecord-projections-in-complex-contexts-negprojection-in-let-binding ()

  (assert (equal string string)))

(defun test-all-negnominal-types-negeliminations-negrecord-eliminations-negrecord-projections-in-complex-contexts-negprojection-in-tuple ()

  (assert (equal ((Person → string), (Person → int32)) ((Person → string), (Person → int32)))))

(defun test-all-negnominal-types-negeliminations-negrecord-eliminations-negrecord-projections-in-complex-contexts-negprojection-in-list ()

  (assert (equal list<(Person → string)> list<(Person → string)>)))

;; Multi-parameter polymorphic projections

(defun test-all-negnominal-types-negeliminations-negrecord-eliminations-negmulti-negparameter-polymorphic-projections-negproject-first-from-triple ()

  (assert (equal (∀t0.(∀t1.(∀t2.((Triple @ t0 @ t1 @ t2) → t0)))) (∀t0.(∀t1.(∀t2.((Triple @ t0 @ t1 @ t2) → t0)))))))

(defun test-all-negnominal-types-negeliminations-negrecord-eliminations-negmulti-negparameter-polymorphic-projections-negproject-second-from-triple-applied ()

  (assert (equal string string)))

(defun test-all-negnominal-types-negeliminations-negrecord-eliminations-negmulti-negparameter-polymorphic-projections-negproject-from-triple-and-use-second-field-which-is-another-polymorphic-record ()

  (assert (equal (∀t0.(∀t1.(∀t2.(∀t3.((Triple @ t0 @ (PersonOrSomething @ map<t1, t2>) @ t3) → t1 → maybe<t2>))))) (∀t0.(∀t1.(∀t2.(∀t3.((Triple @ t0 @ (PersonOrSomething @ map<t1, t2>) @ t3) → t1 → maybe<t2>))))))))

;; Higher-order record projections

(defun test-all-negnominal-types-negeliminations-negrecord-eliminations-neghigher-negorder-record-projections-negmap-projection-over-list-of-records ()

  (assert (equal list<string> list<string>)))

(defun test-all-negnominal-types-negeliminations-negrecord-eliminations-neghigher-negorder-record-projections-negmap-polymorphic-projection ()

  (assert (equal list<int32> list<int32>)))

(defun test-all-negnominal-types-negeliminations-negrecord-eliminations-neghigher-negorder-record-projections-negfilter-using-projection ()

  (assert (equal list<Person> list<Person>)))

;; Recursive record projections

(defun test-all-negnominal-types-negeliminations-negrecord-eliminations-negrecursive-record-projections-negnested-projection-from-recursive-record ()

  (assert (equal (IntList → int32) (IntList → int32))))

;; Record projections with mutual recursion

(defun test-all-negnominal-types-negeliminations-negrecord-eliminations-negrecord-projections-with-mutual-recursion-negproject-head-from-buddylista ()

  (assert (equal (∀t0.((BuddyListA @ t0) → t0)) (∀t0.((BuddyListA @ t0) → t0)))))

(defun test-all-negnominal-types-negeliminations-negrecord-eliminations-negrecord-projections-with-mutual-recursion-negproject-tail-from-buddylistb ()

  (assert (equal (∀t0.((BuddyListB @ t0) → maybe<(BuddyListA @ t0)>)) (∀t0.((BuddyListB @ t0) → maybe<(BuddyListA @ t0)>)))))

(defun test-all-negnominal-types-negeliminations-negrecord-eliminations-negrecord-projections-with-mutual-recursion-negchained-projections-across-mutual-recursion ()

  (assert (equal (∀t0.((BuddyListA @ t0) → maybe<(BuddyListB @ t0)>)) (∀t0.((BuddyListA @ t0) → maybe<(BuddyListB @ t0)>)))))

;; Projections with variables

(defun test-all-negnominal-types-negeliminations-negrecord-eliminations-negprojections-with-variables-negproject-from-lambda-parameter ()

  (assert (equal (Person → string) (Person → string))))

(defun test-all-negnominal-types-negeliminations-negrecord-eliminations-negprojections-with-variables-negproject-from-polymorphic-lambda-parameter ()

  (assert (equal (∀t0.((LatLonPoly @ t0) → t0)) (∀t0.((LatLonPoly @ t0) → t0)))))

(defun test-all-negnominal-types-negeliminations-negrecord-eliminations-negprojections-with-variables-negmultiple-projections-from-same-record ()

  (assert (equal (Person → (string, string)) (Person → (string, string)))))

;; Union eliminations

;; Simple unit inject eliminations

(defun test-all-negnominal-types-negeliminations-negunion-eliminations-negsimple-unit-inject-eliminations-negmatch-comparison-with-all-cases ()

  (assert (equal (Comparison → string) (Comparison → string))))

(defun test-all-negnominal-types-negeliminations-negunion-eliminations-negsimple-unit-inject-eliminations-negmatch-comparison-returning-int32 ()

  (assert (equal (Comparison → int32) (Comparison → int32))))

(defun test-all-negnominal-types-negeliminations-negunion-eliminations-negsimple-unit-inject-eliminations-negmatch-applied-to-comparison-variant ()

  (assert (equal string string)))

;; Union eliminations with data

(defun test-all-negnominal-types-negeliminations-negunion-eliminations-negunion-eliminations-with-data-negmatch-number-extracting-int-values ()

  (assert (equal (Number → int32) (Number → int32))))

(defun test-all-negnominal-types-negeliminations-negunion-eliminations-negunion-eliminations-with-data-negmatch-number-converting-to-string ()

  (assert (equal (Number → string) (Number → string))))

(defun test-all-negnominal-types-negeliminations-negunion-eliminations-negunion-eliminations-with-data-negmatch-number-applied-to-int-variant ()

  (assert (equal int32 int32)))

(defun test-all-negnominal-types-negeliminations-negunion-eliminations-negunion-eliminations-with-data-negmatch-timestamp-with-mixed-data-types ()

  (assert (equal (Timestamp → string) (Timestamp → string))))

;; Polymorphic union eliminations

;; Simple polymorphic unions

(defun test-all-negnominal-types-negeliminations-negunion-eliminations-negpolymorphic-union-eliminations-negsimple-polymorphic-unions-negmatch-personorsomething-with-string ()

  (assert (equal ((PersonOrSomething @ string) → string) ((PersonOrSomething @ string) → string))))

(defun test-all-negnominal-types-negeliminations-negunion-eliminations-negpolymorphic-union-eliminations-negsimple-polymorphic-unions-negmatch-personorsomething-instantiated-with-string ()

  (assert (equal string string)))

;; using UnionPolymorphicRecursive

(defun test-all-negnominal-types-negeliminations-negunion-eliminations-negpolymorphic-union-eliminations-negusing-unionpolymorphicrecursive-negnon-negapplied-unionpolymorphicrecursive ()

  (assert (equal ((UnionPolymorphicRecursive @ int32) → string) ((UnionPolymorphicRecursive @ int32) → string))))

(defun test-all-negnominal-types-negeliminations-negunion-eliminations-negpolymorphic-union-eliminations-negusing-unionpolymorphicrecursive-negapplied-unionpolymorphicrecursive-with-int32 ()

  (assert (equal string string)))

(defun test-all-negnominal-types-negeliminations-negunion-eliminations-negpolymorphic-union-eliminations-negusing-unionpolymorphicrecursive-negapplied-unionpolymorphicrecursive-with-int32-in-lambda ()

  (assert (equal ((UnionPolymorphicRecursive @ int32) → string) ((UnionPolymorphicRecursive @ int32) → string))))

(defun test-all-negnominal-types-negeliminations-negunion-eliminations-negpolymorphic-union-eliminations-negusing-unionpolymorphicrecursive-negapplied-generic-unionpolymorphicrecursive-in-lambda ()

  (assert (equal (∀t0.((UnionPolymorphicRecursive @ t0) → string)) (∀t0.((UnionPolymorphicRecursive @ t0) → string)))))

;; Using kernel types

(defun test-all-negnominal-types-negeliminations-negunion-eliminations-negpolymorphic-union-eliminations-negusing-kernel-types-negcase-statement-on-coderdirection-applied-to-argument ()

  (assert (equal (∀t0.(hydra.coders.CoderDirection → (hydra.coders.Coder @ t0 @ t0) → hydra.context.Context → t0 → either<(hydra.context.InContext @ hydra.errors.Error), t0>)) (∀t0.(hydra.coders.CoderDirection → (hydra.coders.Coder @ t0 @ t0) → hydra.context.Context → t0 → either<(hydra.context.InContext @ hydra.errors.Error), t0>)))))

;; Union eliminations with defaults

(defun test-all-negnominal-types-negeliminations-negunion-eliminations-negunion-eliminations-with-defaults-negmatch-comparison-with-default-case ()

  (assert (equal (Comparison → string) (Comparison → string))))

(defun test-all-negnominal-types-negeliminations-negunion-eliminations-negunion-eliminations-with-defaults-negmatch-number-with-default-case ()

  (assert (equal (Number → int32) (Number → int32))))

(defun test-all-negnominal-types-negeliminations-negunion-eliminations-negunion-eliminations-with-defaults-negmatch-unionmonomorphic-with-default ()

  (assert (equal (UnionMonomorphic → string) (UnionMonomorphic → string))))

;; Nested union eliminations

(defun test-all-negnominal-types-negeliminations-negunion-eliminations-negnested-union-eliminations-negnested-match-statements ()

  (assert (equal ((PersonOrSomething @ Number) → string) ((PersonOrSomething @ Number) → string))))

(defun test-all-negnominal-types-negeliminations-negunion-eliminations-negnested-union-eliminations-negmatch-in-tuple ()

  (assert (equal ((Comparison → int32), string) ((Comparison → int32), string))))

;; Union eliminations in complex contexts

(defun test-all-negnominal-types-negeliminations-negunion-eliminations-negunion-eliminations-in-complex-contexts-negmatch-in-let-binding ()

  (assert (equal (Comparison → string) (Comparison → string))))

(defun test-all-negnominal-types-negeliminations-negunion-eliminations-negunion-eliminations-in-complex-contexts-negmatch-in-record ()

  (assert (equal Person Person)))

(defun test-all-negnominal-types-negeliminations-negunion-eliminations-negunion-eliminations-in-complex-contexts-negmatch-with-polymorphic-result-in-list ()

  (assert (equal list<int32> list<int32>)))

;; Multi-parameter polymorphic case statements

(defun test-all-negnominal-types-negeliminations-negunion-eliminations-negmulti-negparameter-polymorphic-case-statements-negcase-either-converting-both-to-string ()

  (assert (equal ((Either @ int32 @ float32) → string) ((Either @ int32 @ float32) → string))))

(defun test-all-negnominal-types-negeliminations-negunion-eliminations-negmulti-negparameter-polymorphic-case-statements-negcase-either-applied-to-injection ()

  (assert (equal int32 int32)))

(defun test-all-negnominal-types-negeliminations-negunion-eliminations-negmulti-negparameter-polymorphic-case-statements-negcase-either-with-triple-and-nested-projections ()

  (assert (equal (∀t0.(∀t1.(∀t2.(∀t3.(∀t4.((Triple @ t0 @ (Either @ (LatLonPoly @ t1) @ (Triple @ t1 @ t2 @ t3)) @ t4) → t1)))))) (∀t0.(∀t1.(∀t2.(∀t3.(∀t4.((Triple @ t0 @ (Either @ (LatLonPoly @ t1) @ (Triple @ t1 @ t2 @ t3)) @ t4) → t1)))))))))

(defun test-all-negnominal-types-negeliminations-negunion-eliminations-negmulti-negparameter-polymorphic-case-statements-negcase-either-with-polymorphic-let-bindings ()

  (assert (equal ((Either @ int32 @ string) → (Either @ int32 @ int32)) ((Either @ int32 @ string) → (Either @ int32 @ int32)))))

;; Higher-order union eliminations

(defun test-all-negnominal-types-negeliminations-negunion-eliminations-neghigher-negorder-union-eliminations-negmap-match-over-list ()

  (assert (equal list<string> list<string>)))

(defun test-all-negnominal-types-negeliminations-negunion-eliminations-neghigher-negorder-union-eliminations-negcompose-match-with-other-functions ()

  (assert (equal (Comparison → int32) (Comparison → int32))))

(defun test-all-negnominal-types-negeliminations-negunion-eliminations-neghigher-negorder-union-eliminations-negmatch-in-lambda-body ()

  (assert (equal (Number → int32) (Number → int32))))

;; Recursive union eliminations

(defun test-all-negnominal-types-negeliminations-negunion-eliminations-negrecursive-union-eliminations-negmatch-hydratype-recursively ()

  (assert (equal (HydraType → string) (HydraType → string))))

;; Wrap eliminations

;; Monomorphic unwrapping

(defun test-all-negnominal-types-negeliminations-negwrap-eliminations-negmonomorphic-unwrapping-negunwrap-string-alias ()

  (assert (equal (StringAlias → string) (StringAlias → string))))

;; Polymorphic unwrapping

(defun test-all-negnominal-types-negeliminations-negwrap-eliminations-negpolymorphic-unwrapping-negunwrap-polymorphic-wrapper ()

  (assert (equal (∀t0.((PolymorphicWrapper @ t0) → list<t0>)) (∀t0.((PolymorphicWrapper @ t0) → list<t0>)))))

;; Unwrap eliminations in applications

(defun test-all-negnominal-types-negeliminations-negwrap-eliminations-negunwrap-eliminations-in-applications-negunwrap-applied-to-wrapped-term ()

  (assert (equal string string)))

(defun test-all-negnominal-types-negeliminations-negwrap-eliminations-negunwrap-eliminations-in-applications-negunwrap-polymorphic-applied ()

  (assert (equal list<int32> list<int32>)))

;; Unwrap in complex contexts

(defun test-all-negnominal-types-negeliminations-negwrap-eliminations-negunwrap-in-complex-contexts-negunwrap-in-let-binding ()

  (assert (equal string string)))

(defun test-all-negnominal-types-negeliminations-negwrap-eliminations-negunwrap-in-complex-contexts-negunwrap-in-tuple ()

  (assert (equal ((StringAlias → string), string) ((StringAlias → string), string))))

(defun test-all-negnominal-types-negeliminations-negwrap-eliminations-negunwrap-in-complex-contexts-negunwrap-in-lambda ()

  (assert (equal (StringAlias → string) (StringAlias → string))))

;; Multi-parameter polymorphic unwrappers

(defun test-all-negnominal-types-negeliminations-negwrap-eliminations-negmulti-negparameter-polymorphic-unwrappers-negunwrap-symmetric-triple-to-tuple ()

  (assert (equal (∀t0.(∀t1.((SymmetricTriple @ t0 @ t1) → (t0, t0)))) (∀t0.(∀t1.((SymmetricTriple @ t0 @ t1) → (t0, t0)))))))

(defun test-all-negnominal-types-negeliminations-negwrap-eliminations-negmulti-negparameter-polymorphic-unwrappers-negunwrap-and-collect-edges-in-set ()

  (assert (equal (∀t0.(∀t1.(set<(SymmetricTriple @ t0 @ t1)> → set<t1>))) (∀t0.(∀t1.(set<(SymmetricTriple @ t0 @ t1)> → set<t1>))))))

(defun test-all-negnominal-types-negeliminations-negwrap-eliminations-negmulti-negparameter-polymorphic-unwrappers-negunwrap-with-maybe-to-handle-optional-symmetric-triple ()

  (assert (equal (∀t0.(∀t1.(maybe<(SymmetricTriple @ t0 @ t1)> → maybe<t1>))) (∀t0.(∀t1.(maybe<(SymmetricTriple @ t0 @ t1)> → maybe<t1>))))))

;; Chained unwrapping

(defun test-all-negnominal-types-negeliminations-negwrap-eliminations-negchained-unwrapping-negunwrap-then-process ()

  (assert (equal (StringAlias → string) (StringAlias → string))))

(defun test-all-negnominal-types-negeliminations-negwrap-eliminations-negchained-unwrapping-negunwrap-polymorphic-then-map ()

  (assert (equal ((PolymorphicWrapper @ int32) → list<int32>) ((PolymorphicWrapper @ int32) → list<int32>))))

;; Multiple unwrap operations

(defun test-all-negnominal-types-negeliminations-negwrap-eliminations-negmultiple-unwrap-operations-negunwrap-different-types ()

  (assert (equal (∀t0.(StringAlias → (PolymorphicWrapper @ t0) → (string, list<t0>))) (∀t0.(StringAlias → (PolymorphicWrapper @ t0) → (string, list<t0>))))))
