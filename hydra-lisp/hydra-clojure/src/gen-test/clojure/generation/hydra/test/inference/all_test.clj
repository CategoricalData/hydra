;; Note: this is an automatically generated file. Do not edit.
;; inference

(ns test-ns
  (:require [clojure.test :refer :all]))

;; Algebraic terms

;; Collection primitives

;; maps.map applied to a function

(deftest test-all-negalgebraic-terms-negcollection-primitives-negmaps-dotmap-applied-to-a-function-neg-num1

  (is (= (forall t0. (ordering t0) => (map<t0, int32> → map<t0, int32>))

         (forall t0. (ordering t0) => (map<t0, int32> → map<t0, int32>)))))

(deftest test-all-negalgebraic-terms-negcollection-primitives-negmaps-dotmap-applied-to-a-function-neg-num2

  (is (= (forall t0,t1. (ordering t0) => (map<t0, t1> → map<t0, list<t1>>))

         (forall t0,t1. (ordering t0) => (map<t0, t1> → map<t0, list<t1>>)))))

(deftest test-all-negalgebraic-terms-negcollection-primitives-negmaps-dotmap-applied-to-a-function-neg-num3

  (is (= (forall t0,t1. (ordering t0, ordering t1) => (map<t0, list<t1>> → map<t0, set<t1>>))

         (forall t0,t1. (ordering t0, ordering t1) => (map<t0, list<t1>> → map<t0, set<t1>>)))))

;; sets.map applied to a function

(deftest test-all-negalgebraic-terms-negcollection-primitives-negsets-dotmap-applied-to-a-function-neg-num1

  (is (= ((set<int32> → set<int32>))

         ((set<int32> → set<int32>)))))

(deftest test-all-negalgebraic-terms-negcollection-primitives-negsets-dotmap-applied-to-a-function-neg-num2

  (is (= (forall t0. (ordering t0) => (set<list<t0>> → set<int32>))

         (forall t0. (ordering t0) => (set<list<t0>> → set<int32>)))))

;; Composing collection primitives in let

(deftest test-all-negalgebraic-terms-negcollection-primitives-negcomposing-collection-primitives-in-let-neg-num1

  (is (= (forall t0,t1. (ordering t0, ordering t1) => (map<t0, list<t1>> → map<t0, set<t1>>))

         (forall t0,t1. (ordering t0, ordering t1) => (map<t0, list<t1>> → map<t0, set<t1>>)))))

(deftest test-all-negalgebraic-terms-negcollection-primitives-negcomposing-collection-primitives-in-let-neg-num2

  (is (= (map<string, set<int32>>)

         (map<string, set<int32>>))))

;; Map operations in lambdas

(deftest test-all-negalgebraic-terms-negcollection-primitives-negmap-operations-in-lambdas-neg-num1

  (is (= (forall t0,t1. (ordering t0) => (map<t0, list<t1>> → map<t0, int32>))

         (forall t0,t1. (ordering t0) => (map<t0, list<t1>> → map<t0, int32>)))))

(deftest test-all-negalgebraic-terms-negcollection-primitives-negmap-operations-in-lambdas-neg-num2

  (is (= (forall t0,t1,t2. (ordering t2) => ((t0 → t1) → map<t2, t0> → map<t2, t1>))

         (forall t0,t1,t2. (ordering t2) => ((t0 → t1) → map<t2, t0> → map<t2, t1>)))))

;; Fully applied collection conversions

(deftest test-all-negalgebraic-terms-negcollection-primitives-negfully-applied-collection-conversions-neg-num1

  (is (= (set<int32>)

         (set<int32>))))

(deftest test-all-negalgebraic-terms-negcollection-primitives-negfully-applied-collection-conversions-neg-num2

  (is (= (map<int32, int32>)

         (map<int32, int32>))))

(deftest test-all-negalgebraic-terms-negcollection-primitives-negfully-applied-collection-conversions-neg-num3

  (is (= (map<string, set<int32>>)

         (map<string, set<int32>>))))

;; Either terms

;; Left values

(deftest test-all-negalgebraic-terms-negeither-terms-negleft-values-neg-num1

  (is (= (list<either<string, int32>>)

         (list<either<string, int32>>))))

(deftest test-all-negalgebraic-terms-negeither-terms-negleft-values-neg-num2

  (is (= (forall t0. either<string, t0>)

         (forall t0. either<string, t0>))))

;; Right values

(deftest test-all-negalgebraic-terms-negeither-terms-negright-values-neg-num1

  (is (= (list<either<string, int32>>)

         (list<either<string, int32>>))))

(deftest test-all-negalgebraic-terms-negeither-terms-negright-values-neg-num2

  (is (= (forall t0. either<t0, int32>)

         (forall t0. either<t0, int32>))))

;; Polymorphic either values

(deftest test-all-negalgebraic-terms-negeither-terms-negpolymorphic-either-values-neg-num1

  (is (= (forall t0,t1. either<list<t0>, t1>)

         (forall t0,t1. either<list<t0>, t1>))))

(deftest test-all-negalgebraic-terms-negeither-terms-negpolymorphic-either-values-neg-num2

  (is (= (forall t0,t1. either<t0, list<t1>>)

         (forall t0,t1. either<t0, list<t1>>))))

;; Nested either values

(deftest test-all-negalgebraic-terms-negeither-terms-negnested-either-values-neg-num1

  (is (= (list<either<either<int32, string>, boolean>>)

         (list<either<either<int32, string>, boolean>>))))

(deftest test-all-negalgebraic-terms-negeither-terms-negnested-either-values-neg-num2

  (is (= (list<either<string, either<int32, boolean>>>)

         (list<either<string, either<int32, boolean>>>))))

;; Either in lambda

(deftest test-all-negalgebraic-terms-negeither-terms-negeither-in-lambda-neg-num1

  (is (= (forall t0,t1. (t0 → either<t0, t1>))

         (forall t0,t1. (t0 → either<t0, t1>)))))

(deftest test-all-negalgebraic-terms-negeither-terms-negeither-in-lambda-neg-num2

  (is (= (forall t0,t1. (t0 → either<t1, t0>))

         (forall t0,t1. (t0 → either<t1, t0>)))))

;; Either in data structures

(deftest test-all-negalgebraic-terms-negeither-terms-negeither-in-data-structures-neg-num1

  (is (= (list<either<string, int32>>)

         (list<either<string, int32>>))))

(deftest test-all-negalgebraic-terms-negeither-terms-negeither-in-data-structures-neg-num2

  (is (= (forall t0. (list<either<string, int32>>, list<t0>))

         (forall t0. (list<either<string, int32>>, list<t0>)))))

;; Eliminations

;; List eliminations (folds)

(deftest test-all-negalgebraic-terms-negeliminations-neglist-eliminations-folds--neg-num1

  (is (= ((int32 → list<int32> → int32))

         ((int32 → list<int32> → int32)))))

(deftest test-all-negalgebraic-terms-negeliminations-neglist-eliminations-folds--neg-num2

  (is (= ((list<int32> → int32))

         ((list<int32> → int32)))))

(deftest test-all-negalgebraic-terms-negeliminations-neglist-eliminations-folds--neg-num3

  (is (= (int32)

         (int32))))

;; Optional eliminations

(deftest test-all-negalgebraic-terms-negeliminations-negoptional-eliminations-neg-num1

  (is (= ((maybe<int32> → int32))

         ((maybe<int32> → int32)))))

(deftest test-all-negalgebraic-terms-negeliminations-negoptional-eliminations-neg-num2

  (is (= (int32)

         (int32))))

(deftest test-all-negalgebraic-terms-negeliminations-negoptional-eliminations-neg-num3

  (is (= (int32)

         (int32))))

(deftest test-all-negalgebraic-terms-negeliminations-negoptional-eliminations-neg-num4

  (is (= (forall t0. (maybe<t0> → maybe<t0>))

         (forall t0. (maybe<t0> → maybe<t0>)))))

(deftest test-all-negalgebraic-terms-negeliminations-negoptional-eliminations-neg-num5

  (is (= (forall t0. (maybe<t0> → list<t0>))

         (forall t0. (maybe<t0> → list<t0>)))))

;; List terms

;; List of strings

(deftest test-all-negalgebraic-terms-neglist-terms-neglist-of-strings-neg-num1

  (is (= (list<string>)

         (list<string>))))

;; List of lists of strings

(deftest test-all-negalgebraic-terms-neglist-terms-neglist-of-lists-of-strings-neg-num1

  (is (= (list<list<string>>)

         (list<list<string>>))))

;; Empty list

(deftest test-all-negalgebraic-terms-neglist-terms-negempty-list-neg-num1

  (is (= (forall t0. list<t0>)

         (forall t0. list<t0>))))

;; List containing an empty list

(deftest test-all-negalgebraic-terms-neglist-terms-neglist-containing-an-empty-list-neg-num1

  (is (= (forall t0. list<list<t0>>)

         (forall t0. list<list<t0>>))))

;; Lambda producing a polymorphic list

(deftest test-all-negalgebraic-terms-neglist-terms-neglambda-producing-a-polymorphic-list-neg-num1

  (is (= (forall t0. (t0 → list<t0>))

         (forall t0. (t0 → list<t0>)))))

;; Lambda producing a list of integers

(deftest test-all-negalgebraic-terms-neglist-terms-neglambda-producing-a-list-of-integers-neg-num1

  (is (= ((int32 → list<int32>))

         ((int32 → list<int32>)))))

;; List with repeated variables

(deftest test-all-negalgebraic-terms-neglist-terms-neglist-with-repeated-variables-neg-num1

  (is (= ((string → list<string>))

         ((string → list<string>)))))

;; Map terms

(deftest test-all-negalgebraic-terms-negmap-terms-neg-num1

  (is (= (map<string, string>)

         (map<string, string>))))

(deftest test-all-negalgebraic-terms-negmap-terms-neg-num2

  (is (= (forall t0,t1. (ordering t0) => map<t0, t1>)

         (forall t0,t1. (ordering t0) => map<t0, t1>))))

(deftest test-all-negalgebraic-terms-negmap-terms-neg-num3

  (is (= (forall t0. (ordering t0) => (t0 → t0 → map<t0, float64>))

         (forall t0. (ordering t0) => (t0 → t0 → map<t0, float64>)))))

;; Optional terms

(deftest test-all-negalgebraic-terms-negoptional-terms-neg-num1

  (is (= (maybe<int32>)

         (maybe<int32>))))

(deftest test-all-negalgebraic-terms-negoptional-terms-neg-num2

  (is (= (forall t0. maybe<t0>)

         (forall t0. maybe<t0>))))

;; Pair terms

;; Monotyped pairs

(deftest test-all-negalgebraic-terms-negpair-terms-negmonotyped-pairs-neg-num1

  (is (= ((string, int32))

         ((string, int32)))))

(deftest test-all-negalgebraic-terms-negpair-terms-negmonotyped-pairs-neg-num2

  (is (= ((string, list<float32>))

         ((string, list<float32>)))))

;; Polytyped pairs

(deftest test-all-negalgebraic-terms-negpair-terms-negpolytyped-pairs-neg-num1

  (is (= (forall t0. (list<t0>, string))

         (forall t0. (list<t0>, string)))))

(deftest test-all-negalgebraic-terms-negpair-terms-negpolytyped-pairs-neg-num2

  (is (= (forall t0,t1. (list<t0>, list<t1>))

         (forall t0,t1. (list<t0>, list<t1>)))))

;; Nested pairs

(deftest test-all-negalgebraic-terms-negpair-terms-negnested-pairs-neg-num1

  (is (= (((int32, string), boolean))

         (((int32, string), boolean)))))

(deftest test-all-negalgebraic-terms-negpair-terms-negnested-pairs-neg-num2

  (is (= ((string, (int32, list<float32>)))

         ((string, (int32, list<float32>))))))

;; Pairs in lambda

(deftest test-all-negalgebraic-terms-negpair-terms-negpairs-in-lambda-neg-num1

  (is (= (forall t0. (t0 → (t0, string)))

         (forall t0. (t0 → (t0, string))))))

(deftest test-all-negalgebraic-terms-negpair-terms-negpairs-in-lambda-neg-num2

  (is (= (forall t0. (t0 → (t0, t0)))

         (forall t0. (t0 → (t0, t0))))))

;; Pairs in data structures

(deftest test-all-negalgebraic-terms-negpair-terms-negpairs-in-data-structures-neg-num1

  (is (= (list<(string, int32)>)

         (list<(string, int32)>))))

(deftest test-all-negalgebraic-terms-negpair-terms-negpairs-in-data-structures-neg-num2

  (is (= (forall t0. list<(list<t0>, string)>)

         (forall t0. list<(list<t0>, string)>))))

;; Additional cases

(deftest test-all-negalgebraic-terms-negpair-terms-negadditional-cases-neg-num1

  (is (= ((int32, string))

         ((int32, string)))))

(deftest test-all-negalgebraic-terms-negpair-terms-negadditional-cases-neg-num2

  (is (= (forall t0. (list<t0>, string))

         (forall t0. (list<t0>, string)))))

(deftest test-all-negalgebraic-terms-negpair-terms-negadditional-cases-neg-num3

  (is (= (forall t0,t1. (list<t0>, list<t1>))

         (forall t0,t1. (list<t0>, list<t1>)))))

;; Set terms

(deftest test-all-negalgebraic-terms-negset-terms-neg-num1

  (is (= (set<boolean>)

         (set<boolean>))))

(deftest test-all-negalgebraic-terms-negset-terms-neg-num2

  (is (= (forall t0. (ordering t0) => set<set<t0>>)

         (forall t0. (ordering t0) => set<set<t0>>))))

;; Algorithm W test cases

;; STLC to System F

(deftest test-all-negalgorithm-w-test-cases-negstlc-to-system-f-neg-num1

  (is (= (forall t0. (t0 → t0))

         (forall t0. (t0 → t0)))))

(deftest test-all-negalgorithm-w-test-cases-negstlc-to-system-f-neg-num2

  (is (= (int32)

         (int32))))

(deftest test-all-negalgorithm-w-test-cases-negstlc-to-system-f-neg-num3

  (is (= (int32)

         (int32))))

(deftest test-all-negalgorithm-w-test-cases-negstlc-to-system-f-neg-num4

  (is (= (int32)

         (int32))))

(deftest test-all-negalgorithm-w-test-cases-negstlc-to-system-f-neg-num5

  (is (= (forall t0. (t0 → list<t0>))

         (forall t0. (t0 → list<t0>)))))

(deftest test-all-negalgorithm-w-test-cases-negstlc-to-system-f-neg-num6

  (is (= ((list<int32>, list<string>))

         ((list<int32>, list<string>)))))

(deftest test-all-negalgorithm-w-test-cases-negstlc-to-system-f-neg-num7

  (is (= (int32)

         (int32))))

(deftest test-all-negalgorithm-w-test-cases-negstlc-to-system-f-neg-num9

  (is (= (forall t0. (int32 → int32 → t0))

         (forall t0. (int32 → int32 → t0)))))

(deftest test-all-negalgorithm-w-test-cases-negstlc-to-system-f-neg-num10

  (is (= (forall t0,t1. ((int32 → int32 → t0), (int32 → int32 → t1)))

         (forall t0,t1. ((int32 → int32 → t0), (int32 → int32 → t1))))))

(deftest test-all-negalgorithm-w-test-cases-negstlc-to-system-f-neg-num11

  (is (= (forall t0,t1,t2,t3. ((t0 → int32 → t1), (int32 → t2 → t3)))

         (forall t0,t1,t2,t3. ((t0 → int32 → t1), (int32 → t2 → t3))))))

(deftest test-all-negalgorithm-w-test-cases-negstlc-to-system-f-neg-num12

  (is (= (forall t0,t1. ((int32 → int32 → t0), (int32 → int32 → t1)))

         (forall t0,t1. ((int32 → int32 → t0), (int32 → int32 → t1))))))

(deftest test-all-negalgorithm-w-test-cases-negstlc-to-system-f-neg-num13

  (is (= (forall t0,t1. ((int32 → int32 → t0), (int32 → int32 → t1)))

         (forall t0,t1. ((int32 → int32 → t0), (int32 → int32 → t1))))))

;; Type classes

;; Monomorphic (constraints vanish)

;; Map operations with concrete types

(deftest test-all-negtype-classes-negmonomorphic-constraints-vanish--negmap-operations-with-concrete-types-neg-num1

  (is (= (map<string, int32>)

         (map<string, int32>))))

(deftest test-all-negtype-classes-negmonomorphic-constraints-vanish--negmap-operations-with-concrete-types-neg-num2

  (is (= (maybe<int32>)

         (maybe<int32>))))

(deftest test-all-negtype-classes-negmonomorphic-constraints-vanish--negmap-operations-with-concrete-types-neg-num3

  (is (= (map<string, int32>)

         (map<string, int32>))))

;; Set operations with concrete types

(deftest test-all-negtype-classes-negmonomorphic-constraints-vanish--negset-operations-with-concrete-types-neg-num1

  (is (= (set<int32>)

         (set<int32>))))

(deftest test-all-negtype-classes-negmonomorphic-constraints-vanish--negset-operations-with-concrete-types-neg-num2

  (is (= (boolean)

         (boolean))))

;; Equality operations with concrete types

(deftest test-all-negtype-classes-negmonomorphic-constraints-vanish--negequality-operations-with-concrete-types-neg-num1

  (is (= (boolean)

         (boolean))))

(deftest test-all-negtype-classes-negmonomorphic-constraints-vanish--negequality-operations-with-concrete-types-neg-num2

  (is (= (hydra.util.Comparison)

         (hydra.util.Comparison))))

;; List operations with concrete types

(deftest test-all-negtype-classes-negmonomorphic-constraints-vanish--neglist-operations-with-concrete-types-neg-num1

  (is (= (list<int32>)

         (list<int32>))))

;; Primitive references with constraints

;; Map primitives (ordering on key type)

(deftest test-all-negtype-classes-negprimitive-references-with-constraints-negmap-primitives-ordering-on-key-type--neg-num1

  (is (= (forall t0,t1. (ordering t0) => (list<(t0, t1)> → map<t0, t1>))

         (forall t0,t1. (ordering t0) => (list<(t0, t1)> → map<t0, t1>)))))

(deftest test-all-negtype-classes-negprimitive-references-with-constraints-negmap-primitives-ordering-on-key-type--neg-num2

  (is (= (forall t0,t1. (ordering t0) => (t0 → map<t0, t1> → maybe<t1>))

         (forall t0,t1. (ordering t0) => (t0 → map<t0, t1> → maybe<t1>)))))

(deftest test-all-negtype-classes-negprimitive-references-with-constraints-negmap-primitives-ordering-on-key-type--neg-num3

  (is (= (forall t0,t1. (ordering t0) => (t0 → t1 → map<t0, t1> → map<t0, t1>))

         (forall t0,t1. (ordering t0) => (t0 → t1 → map<t0, t1> → map<t0, t1>)))))

(deftest test-all-negtype-classes-negprimitive-references-with-constraints-negmap-primitives-ordering-on-key-type--neg-num4

  (is (= (forall t0,t1,t2. (ordering t2) => ((t0 → t1) → map<t2, t0> → map<t2, t1>))

         (forall t0,t1,t2. (ordering t2) => ((t0 → t1) → map<t2, t0> → map<t2, t1>)))))

(deftest test-all-negtype-classes-negprimitive-references-with-constraints-negmap-primitives-ordering-on-key-type--neg-num5

  (is (= (forall t0,t1. (ordering t0) => map<t0, t1>)

         (forall t0,t1. (ordering t0) => map<t0, t1>))))

;; Set primitives (ordering on element type)

(deftest test-all-negtype-classes-negprimitive-references-with-constraints-negset-primitives-ordering-on-element-type--neg-num1

  (is (= (forall t0. (ordering t0) => (list<t0> → set<t0>))

         (forall t0. (ordering t0) => (list<t0> → set<t0>)))))

(deftest test-all-negtype-classes-negprimitive-references-with-constraints-negset-primitives-ordering-on-element-type--neg-num2

  (is (= (forall t0. (ordering t0) => (t0 → set<t0> → boolean))

         (forall t0. (ordering t0) => (t0 → set<t0> → boolean)))))

(deftest test-all-negtype-classes-negprimitive-references-with-constraints-negset-primitives-ordering-on-element-type--neg-num3

  (is (= (forall t0. (ordering t0) => (t0 → set<t0> → set<t0>))

         (forall t0. (ordering t0) => (t0 → set<t0> → set<t0>)))))

(deftest test-all-negtype-classes-negprimitive-references-with-constraints-negset-primitives-ordering-on-element-type--neg-num4

  (is (= (forall t0,t1. (ordering t0, ordering t1) => ((t0 → t1) → set<t0> → set<t1>))

         (forall t0,t1. (ordering t0, ordering t1) => ((t0 → t1) → set<t0> → set<t1>)))))

;; Equality primitives

(deftest test-all-negtype-classes-negprimitive-references-with-constraints-negequality-primitives-neg-num1

  (is (= (forall t0. (equality t0) => (t0 → t0 → boolean))

         (forall t0. (equality t0) => (t0 → t0 → boolean)))))

(deftest test-all-negtype-classes-negprimitive-references-with-constraints-negequality-primitives-neg-num2

  (is (= (forall t0. (ordering t0) => (t0 → t0 → hydra.util.Comparison))

         (forall t0. (ordering t0) => (t0 → t0 → hydra.util.Comparison)))))

;; List primitives with constraints

(deftest test-all-negtype-classes-negprimitive-references-with-constraints-neglist-primitives-with-constraints-neg-num1

  (is (= (forall t0. (ordering t0) => (list<t0> → list<t0>))

         (forall t0. (ordering t0) => (list<t0> → list<t0>)))))

(deftest test-all-negtype-classes-negprimitive-references-with-constraints-neglist-primitives-with-constraints-neg-num2

  (is (= (forall t0. (equality t0) => (list<t0> → list<t0>))

         (forall t0. (equality t0) => (list<t0> → list<t0>)))))

(deftest test-all-negtype-classes-negprimitive-references-with-constraints-neglist-primitives-with-constraints-neg-num3

  (is (= (forall t0. (equality t0) => (t0 → list<t0> → boolean))

         (forall t0. (equality t0) => (t0 → list<t0> → boolean)))))

;; Partial application preserving constraints

;; Map partial application

(deftest test-all-negtype-classes-negpartial-application-preserving-constraints-negmap-partial-application-neg-num1

  (is (= (forall t0,t1. (ordering t0) => (t0 → map<t0, t1> → maybe<t1>))

         (forall t0,t1. (ordering t0) => (t0 → map<t0, t1> → maybe<t1>)))))

(deftest test-all-negtype-classes-negpartial-application-preserving-constraints-negmap-partial-application-neg-num2

  (is (= (forall t0,t1. (ordering t0) => (t0 → t1 → map<t0, t1>))

         (forall t0,t1. (ordering t0) => (t0 → t1 → map<t0, t1>)))))

;; Set partial application

(deftest test-all-negtype-classes-negpartial-application-preserving-constraints-negset-partial-application-neg-num1

  (is (= (forall t0. (ordering t0) => (t0 → set<t0> → boolean))

         (forall t0. (ordering t0) => (t0 → set<t0> → boolean)))))

;; Equality partial application

(deftest test-all-negtype-classes-negpartial-application-preserving-constraints-negequality-partial-application-neg-num1

  (is (= (forall t0. (equality t0) => (t0 → t0 → boolean))

         (forall t0. (equality t0) => (t0 → t0 → boolean)))))

;; Partial application fixing the constrained variable

(deftest test-all-negtype-classes-negpartial-application-preserving-constraints-negpartial-application-fixing-the-constrained-variable-neg-num1

  (is (= (forall t0. (t0 → map<string, t0>))

         (forall t0. (t0 → map<string, t0>)))))

;; Let binding constraint propagation

;; Simple let-bound wrappers

(deftest test-all-negtype-classes-neglet-binding-constraint-propagation-negsimple-let-negbound-wrappers-neg-num1

  (is (= (forall t0,t1. (ordering t0) => (t0 → map<t0, t1> → maybe<t1>))

         (forall t0,t1. (ordering t0) => (t0 → map<t0, t1> → maybe<t1>)))))

(deftest test-all-negtype-classes-neglet-binding-constraint-propagation-negsimple-let-negbound-wrappers-neg-num2

  (is (= (forall t0. (ordering t0) => (t0 → set<t0> → boolean))

         (forall t0. (ordering t0) => (t0 → set<t0> → boolean)))))

(deftest test-all-negtype-classes-neglet-binding-constraint-propagation-negsimple-let-negbound-wrappers-neg-num3

  (is (= (forall t0,t1. (ordering t0) => (list<(t0, t1)> → map<t0, t1>))

         (forall t0,t1. (ordering t0) => (list<(t0, t1)> → map<t0, t1>)))))

;; Let-bound with partial instantiation

(deftest test-all-negtype-classes-neglet-binding-constraint-propagation-neglet-negbound-with-partial-instantiation-neg-num1

  (is (= (forall t0. (ordering t0) => (map<t0, int32> → map<t0, int32>))

         (forall t0. (ordering t0) => (map<t0, int32> → map<t0, int32>)))))

(deftest test-all-negtype-classes-neglet-binding-constraint-propagation-neglet-negbound-with-partial-instantiation-neg-num2

  (is (= (forall t0. (ordering t0) => (list<t0> → set<t0>))

         (forall t0. (ordering t0) => (list<t0> → set<t0>)))))

;; Multiple uses of a constrained let binding

(deftest test-all-negtype-classes-neglet-binding-constraint-propagation-negmultiple-uses-of-a-constrained-let-binding-neg-num1

  (is (= ((map<string, int32>, map<boolean, string>))

         ((map<string, int32>, map<boolean, string>)))))

;; Composition and constraint merging

;; Composing constrained primitives

(deftest test-all-negtype-classes-negcomposition-and-constraint-merging-negcomposing-constrained-primitives-neg-num1

  (is (= (forall t0. (ordering t0) => (list<t0> → map<t0, t0>))

         (forall t0. (ordering t0) => (list<t0> → map<t0, t0>)))))

(deftest test-all-negtype-classes-negcomposition-and-constraint-merging-negcomposing-constrained-primitives-neg-num2

  (is (= (forall t0,t1. (ordering t1) => ((t0 → t1) → list<t0> → set<t1>))

         (forall t0,t1. (ordering t1) => ((t0 → t1) → list<t0> → set<t1>)))))

;; Composing map and sort

(deftest test-all-negtype-classes-negcomposition-and-constraint-merging-negcomposing-map-and-sort-neg-num1

  (is (= (forall t0,t1. (ordering t0, ordering t1) => (map<t0, list<t1>> → map<t0, list<t1>>))

         (forall t0,t1. (ordering t0, ordering t1) => (map<t0, list<t1>> → map<t0, list<t1>>)))))

;; Nested containers

;; Maps of sets

(deftest test-all-negtype-classes-negnested-containers-negmaps-of-sets-neg-num1

  (is (= (forall t0,t1. (ordering t0, ordering t1) => (map<t0, list<t1>> → map<t0, set<t1>>))

         (forall t0,t1. (ordering t0, ordering t1) => (map<t0, list<t1>> → map<t0, set<t1>>)))))

;; Sets of sets

(deftest test-all-negtype-classes-negnested-containers-negsets-of-sets-neg-num1

  (is (= (forall t0. (ordering t0) => (set<list<t0>> → set<set<t0>>))

         (forall t0. (ordering t0) => (set<list<t0>> → set<set<t0>>)))))

;; Map from sorted list

(deftest test-all-negtype-classes-negnested-containers-negmap-from-sorted-list-neg-num1

  (is (= (forall t0. (ordering t0) => (list<t0> → map<t0, set<t0>>))

         (forall t0. (ordering t0) => (list<t0> → map<t0, set<t0>>)))))

;; Collection term constraints

;; Set literals

(deftest test-all-negtype-classes-negcollection-term-constraints-negset-literals-neg-num1

  (is (= (forall t0. (ordering t0) => (t0 → set<t0>))

         (forall t0. (ordering t0) => (t0 → set<t0>)))))

(deftest test-all-negtype-classes-negcollection-term-constraints-negset-literals-neg-num2

  (is (= (forall t0. (ordering t0) => (t0 → t0 → set<t0>))

         (forall t0. (ordering t0) => (t0 → t0 → set<t0>)))))

(deftest test-all-negtype-classes-negcollection-term-constraints-negset-literals-neg-num3

  (is (= (set<int32>)

         (set<int32>))))

;; Map literals

(deftest test-all-negtype-classes-negcollection-term-constraints-negmap-literals-neg-num1

  (is (= (forall t0,t1. (ordering t0) => (t0 → t1 → map<t0, t1>))

         (forall t0,t1. (ordering t0) => (t0 → t1 → map<t0, t1>)))))

(deftest test-all-negtype-classes-negcollection-term-constraints-negmap-literals-neg-num2

  (is (= (map<string, int32>)

         (map<string, int32>))))

;; Collection terms with primitives

(deftest test-all-negtype-classes-negcollection-term-constraints-negcollection-terms-with-primitives-neg-num1

  (is (= ((int32 → set<int32>))

         ((int32 → set<int32>)))))

(deftest test-all-negtype-classes-negcollection-term-constraints-negcollection-terms-with-primitives-neg-num2

  (is (= (forall t0. (ordering t0) => (t0 → map<t0, list<t0>>))

         (forall t0. (ordering t0) => (t0 → map<t0, list<t0>>)))))

;; Constraint propagation through collection elements

(deftest test-all-negtype-classes-negcollection-term-constraints-negconstraint-propagation-through-collection-elements-neg-num1

  (is (= (forall t0. (ordering t0) => (list<t0> → map<int32, set<t0>>))

         (forall t0. (ordering t0) => (list<t0> → map<int32, set<t0>>)))))

(deftest test-all-negtype-classes-negcollection-term-constraints-negconstraint-propagation-through-collection-elements-neg-num2

  (is (= (forall t0. (ordering t0) => list<(list<t0> → list<t0>)>)

         (forall t0. (ordering t0) => list<(list<t0> → list<t0>)>))))

(deftest test-all-negtype-classes-negcollection-term-constraints-negconstraint-propagation-through-collection-elements-neg-num3

  (is (= (forall t0. (ordering t0) => ((list<t0> → set<t0>), int32))

         (forall t0. (ordering t0) => ((list<t0> → set<t0>), int32)))))

(deftest test-all-negtype-classes-negcollection-term-constraints-negconstraint-propagation-through-collection-elements-neg-num4

  (is (= (forall t0. (ordering t0) => (list<t0> → set<set<t0>>))

         (forall t0. (ordering t0) => (list<t0> → set<set<t0>>)))))

;; Expected failures

;; Undefined variable

;; Basic unbound variables

(deftest test-all-negexpected-failures-negundefined-variable-negbasic-unbound-variables-neg-num1

  (is (= FAIL

         FAIL)))

(deftest test-all-negexpected-failures-negundefined-variable-negbasic-unbound-variables-neg-num2

  (is (= FAIL

         FAIL)))

(deftest test-all-negexpected-failures-negundefined-variable-negbasic-unbound-variables-neg-num3

  (is (= FAIL

         FAIL)))

;; Unbound in let expressions

(deftest test-all-negexpected-failures-negundefined-variable-negunbound-in-let-expressions-neg-num1

  (is (= FAIL

         FAIL)))

(deftest test-all-negexpected-failures-negundefined-variable-negunbound-in-let-expressions-neg-num2

  (is (= FAIL

         FAIL)))

(deftest test-all-negexpected-failures-negundefined-variable-negunbound-in-let-expressions-neg-num3

  (is (= FAIL

         FAIL)))

;; Shadowing scope errors

(deftest test-all-negexpected-failures-negundefined-variable-negshadowing-scope-errors-neg-num1

  (is (= FAIL

         FAIL)))

(deftest test-all-negexpected-failures-negundefined-variable-negshadowing-scope-errors-neg-num2

  (is (= FAIL

         FAIL)))

(deftest test-all-negexpected-failures-negundefined-variable-negshadowing-scope-errors-neg-num3

  (is (= FAIL

         FAIL)))

;; Unification failure

;; Basic type mismatches

(deftest test-all-negexpected-failures-negunification-failure-negbasic-type-mismatches-neg-num1

  (is (= FAIL

         FAIL)))

(deftest test-all-negexpected-failures-negunification-failure-negbasic-type-mismatches-neg-num2

  (is (= FAIL

         FAIL)))

(deftest test-all-negexpected-failures-negunification-failure-negbasic-type-mismatches-neg-num3

  (is (= FAIL

         FAIL)))

(deftest test-all-negexpected-failures-negunification-failure-negbasic-type-mismatches-neg-num4

  (is (= FAIL

         FAIL)))

;; Collection type mismatches

(deftest test-all-negexpected-failures-negunification-failure-negcollection-type-mismatches-neg-num1

  (is (= FAIL

         FAIL)))

(deftest test-all-negexpected-failures-negunification-failure-negcollection-type-mismatches-neg-num2

  (is (= FAIL

         FAIL)))

(deftest test-all-negexpected-failures-negunification-failure-negcollection-type-mismatches-neg-num3

  (is (= FAIL

         FAIL)))

(deftest test-all-negexpected-failures-negunification-failure-negcollection-type-mismatches-neg-num4

  (is (= FAIL

         FAIL)))

;; Conditional type mismatches

(deftest test-all-negexpected-failures-negunification-failure-negconditional-type-mismatches-neg-num1

  (is (= FAIL

         FAIL)))

(deftest test-all-negexpected-failures-negunification-failure-negconditional-type-mismatches-neg-num2

  (is (= FAIL

         FAIL)))

(deftest test-all-negexpected-failures-negunification-failure-negconditional-type-mismatches-neg-num3

  (is (= FAIL

         FAIL)))

;; Polymorphic instantiation conflicts

(deftest test-all-negexpected-failures-negunification-failure-negpolymorphic-instantiation-conflicts-neg-num1

  (is (= FAIL

         FAIL)))

(deftest test-all-negexpected-failures-negunification-failure-negpolymorphic-instantiation-conflicts-neg-num2

  (is (= FAIL

         FAIL)))

(deftest test-all-negexpected-failures-negunification-failure-negpolymorphic-instantiation-conflicts-neg-num3

  (is (= FAIL

         FAIL)))

;; Invalid application

;; Non-function application

(deftest test-all-negexpected-failures-neginvalid-application-negnon-negfunction-application-neg-num1

  (is (= FAIL

         FAIL)))

(deftest test-all-negexpected-failures-neginvalid-application-negnon-negfunction-application-neg-num2

  (is (= FAIL

         FAIL)))

(deftest test-all-negexpected-failures-neginvalid-application-negnon-negfunction-application-neg-num3

  (is (= FAIL

         FAIL)))

(deftest test-all-negexpected-failures-neginvalid-application-negnon-negfunction-application-neg-num4

  (is (= FAIL

         FAIL)))

;; Collection application

(deftest test-all-negexpected-failures-neginvalid-application-negcollection-application-neg-num1

  (is (= FAIL

         FAIL)))

(deftest test-all-negexpected-failures-neginvalid-application-negcollection-application-neg-num2

  (is (= FAIL

         FAIL)))

(deftest test-all-negexpected-failures-neginvalid-application-negcollection-application-neg-num3

  (is (= FAIL

         FAIL)))

(deftest test-all-negexpected-failures-neginvalid-application-negcollection-application-neg-num4

  (is (= FAIL

         FAIL)))

;; Primitive misapplication

(deftest test-all-negexpected-failures-neginvalid-application-negprimitive-misapplication-neg-num1

  (is (= FAIL

         FAIL)))

(deftest test-all-negexpected-failures-neginvalid-application-negprimitive-misapplication-neg-num2

  (is (= FAIL

         FAIL)))

(deftest test-all-negexpected-failures-neginvalid-application-negprimitive-misapplication-neg-num3

  (is (= FAIL

         FAIL)))

(deftest test-all-negexpected-failures-neginvalid-application-negprimitive-misapplication-neg-num4

  (is (= FAIL

         FAIL)))

;; Self-application

;; Direct self-application

(deftest test-all-negexpected-failures-negself-negapplication-negdirect-self-negapplication-neg-num1

  (is (= FAIL

         FAIL)))

(deftest test-all-negexpected-failures-negself-negapplication-negdirect-self-negapplication-neg-num2

  (is (= FAIL

         FAIL)))

;; Indirect self-application

(deftest test-all-negexpected-failures-negself-negapplication-negindirect-self-negapplication-neg-num1

  (is (= FAIL

         FAIL)))

(deftest test-all-negexpected-failures-negself-negapplication-negindirect-self-negapplication-neg-num2

  (is (= FAIL

         FAIL)))

(deftest test-all-negexpected-failures-negself-negapplication-negindirect-self-negapplication-neg-num3

  (is (= FAIL

         FAIL)))

;; Arity mismatch

;; Too many arguments

(deftest test-all-negexpected-failures-negarity-mismatch-negtoo-many-arguments-neg-num1

  (is (= FAIL

         FAIL)))

(deftest test-all-negexpected-failures-negarity-mismatch-negtoo-many-arguments-neg-num2

  (is (= FAIL

         FAIL)))

(deftest test-all-negexpected-failures-negarity-mismatch-negtoo-many-arguments-neg-num3

  (is (= FAIL

         FAIL)))

;; Wrong argument types with extra args

(deftest test-all-negexpected-failures-negarity-mismatch-negwrong-argument-types-with-extra-args-neg-num1

  (is (= FAIL

         FAIL)))

(deftest test-all-negexpected-failures-negarity-mismatch-negwrong-argument-types-with-extra-args-neg-num2

  (is (= FAIL

         FAIL)))

(deftest test-all-negexpected-failures-negarity-mismatch-negwrong-argument-types-with-extra-args-neg-num3

  (is (= FAIL

         FAIL)))

;; Recursive type construction

;; Direct recursive types

(deftest test-all-negexpected-failures-negrecursive-type-construction-negdirect-recursive-types-neg-num1

  (is (= FAIL

         FAIL)))

(deftest test-all-negexpected-failures-negrecursive-type-construction-negdirect-recursive-types-neg-num2

  (is (= FAIL

         FAIL)))

(deftest test-all-negexpected-failures-negrecursive-type-construction-negdirect-recursive-types-neg-num3

  (is (= FAIL

         FAIL)))

;; Recursive function types

(deftest test-all-negexpected-failures-negrecursive-type-construction-negrecursive-function-types-neg-num1

  (is (= FAIL

         FAIL)))

(deftest test-all-negexpected-failures-negrecursive-type-construction-negrecursive-function-types-neg-num2

  (is (= FAIL

         FAIL)))

(deftest test-all-negexpected-failures-negrecursive-type-construction-negrecursive-function-types-neg-num3

  (is (= FAIL

         FAIL)))

;; Mutually recursive types

(deftest test-all-negexpected-failures-negrecursive-type-construction-negmutually-recursive-types-neg-num1

  (is (= FAIL

         FAIL)))

(deftest test-all-negexpected-failures-negrecursive-type-construction-negmutually-recursive-types-neg-num2

  (is (= FAIL

         FAIL)))

(deftest test-all-negexpected-failures-negrecursive-type-construction-negmutually-recursive-types-neg-num3

  (is (= FAIL

         FAIL)))

;; Occur check failures

;; Function occur checks

(deftest test-all-negexpected-failures-negoccur-check-failures-negfunction-occur-checks-neg-num1

  (is (= FAIL

         FAIL)))

;; Mutual occur checks

(deftest test-all-negexpected-failures-negoccur-check-failures-negmutual-occur-checks-neg-num1

  (is (= FAIL

         FAIL)))

(deftest test-all-negexpected-failures-negoccur-check-failures-negmutual-occur-checks-neg-num2

  (is (= FAIL

         FAIL)))

(deftest test-all-negexpected-failures-negoccur-check-failures-negmutual-occur-checks-neg-num3

  (is (= FAIL

         FAIL)))

;; Complex occur checks

(deftest test-all-negexpected-failures-negoccur-check-failures-negcomplex-occur-checks-neg-num1

  (is (= FAIL

         FAIL)))

(deftest test-all-negexpected-failures-negoccur-check-failures-negcomplex-occur-checks-neg-num2

  (is (= FAIL

         FAIL)))

;; Type constructor misuse

;; List constructor errors

(deftest test-all-negexpected-failures-negtype-constructor-misuse-neglist-constructor-errors-neg-num1

  (is (= FAIL

         FAIL)))

(deftest test-all-negexpected-failures-negtype-constructor-misuse-neglist-constructor-errors-neg-num2

  (is (= FAIL

         FAIL)))

(deftest test-all-negexpected-failures-negtype-constructor-misuse-neglist-constructor-errors-neg-num3

  (is (= FAIL

         FAIL)))

(deftest test-all-negexpected-failures-negtype-constructor-misuse-neglist-constructor-errors-neg-num4

  (is (= FAIL

         FAIL)))

;; String constructor errors

(deftest test-all-negexpected-failures-negtype-constructor-misuse-negstring-constructor-errors-neg-num1

  (is (= FAIL

         FAIL)))

(deftest test-all-negexpected-failures-negtype-constructor-misuse-negstring-constructor-errors-neg-num2

  (is (= FAIL

         FAIL)))

(deftest test-all-negexpected-failures-negtype-constructor-misuse-negstring-constructor-errors-neg-num3

  (is (= FAIL

         FAIL)))

(deftest test-all-negexpected-failures-negtype-constructor-misuse-negstring-constructor-errors-neg-num4

  (is (= FAIL

         FAIL)))

;; Math constructor errors

(deftest test-all-negexpected-failures-negtype-constructor-misuse-negmath-constructor-errors-neg-num1

  (is (= FAIL

         FAIL)))

(deftest test-all-negexpected-failures-negtype-constructor-misuse-negmath-constructor-errors-neg-num2

  (is (= FAIL

         FAIL)))

(deftest test-all-negexpected-failures-negtype-constructor-misuse-negmath-constructor-errors-neg-num3

  (is (= FAIL

         FAIL)))

(deftest test-all-negexpected-failures-negtype-constructor-misuse-negmath-constructor-errors-neg-num4

  (is (= FAIL

         FAIL)))

;; Polymorphism violations

;; Identity function violations

(deftest test-all-negexpected-failures-negpolymorphism-violations-negidentity-function-violations-neg-num1

  (is (= FAIL

         FAIL)))

(deftest test-all-negexpected-failures-negpolymorphism-violations-negidentity-function-violations-neg-num2

  (is (= FAIL

         FAIL)))

(deftest test-all-negexpected-failures-negpolymorphism-violations-negidentity-function-violations-neg-num3

  (is (= FAIL

         FAIL)))

;; Constrained polymorphism violations

(deftest test-all-negexpected-failures-negpolymorphism-violations-negconstrained-polymorphism-violations-neg-num1

  (is (= FAIL

         FAIL)))

(deftest test-all-negexpected-failures-negpolymorphism-violations-negconstrained-polymorphism-violations-neg-num2

  (is (= FAIL

         FAIL)))

(deftest test-all-negexpected-failures-negpolymorphism-violations-negconstrained-polymorphism-violations-neg-num3

  (is (= FAIL

         FAIL)))

;; Higher-order polymorphism violations

(deftest test-all-negexpected-failures-negpolymorphism-violations-neghigher-negorder-polymorphism-violations-neg-num1

  (is (= FAIL

         FAIL)))

(deftest test-all-negexpected-failures-negpolymorphism-violations-neghigher-negorder-polymorphism-violations-neg-num2

  (is (= FAIL

         FAIL)))

(deftest test-all-negexpected-failures-negpolymorphism-violations-neghigher-negorder-polymorphism-violations-neg-num3

  (is (= FAIL

         FAIL)))

;; Let binding type mismatches

;; Application type mismatches

(deftest test-all-negexpected-failures-neglet-binding-type-mismatches-negapplication-type-mismatches-neg-num1

  (is (= FAIL

         FAIL)))

(deftest test-all-negexpected-failures-neglet-binding-type-mismatches-negapplication-type-mismatches-neg-num2

  (is (= FAIL

         FAIL)))

(deftest test-all-negexpected-failures-neglet-binding-type-mismatches-negapplication-type-mismatches-neg-num3

  (is (= FAIL

         FAIL)))

;; Collection type mismatches

(deftest test-all-negexpected-failures-neglet-binding-type-mismatches-negcollection-type-mismatches-neg-num1

  (is (= FAIL

         FAIL)))

(deftest test-all-negexpected-failures-neglet-binding-type-mismatches-negcollection-type-mismatches-neg-num2

  (is (= FAIL

         FAIL)))

(deftest test-all-negexpected-failures-neglet-binding-type-mismatches-negcollection-type-mismatches-neg-num3

  (is (= FAIL

         FAIL)))

;; Function binding mismatches

(deftest test-all-negexpected-failures-neglet-binding-type-mismatches-negfunction-binding-mismatches-neg-num1

  (is (= FAIL

         FAIL)))

(deftest test-all-negexpected-failures-neglet-binding-type-mismatches-negfunction-binding-mismatches-neg-num2

  (is (= FAIL

         FAIL)))

;; Constraint solver edge cases

;; Complex constraint propagation

(deftest test-all-negexpected-failures-negconstraint-solver-edge-cases-negcomplex-constraint-propagation-neg-num1

  (is (= FAIL

         FAIL)))

;; Fixed point combinators

(deftest test-all-negexpected-failures-negconstraint-solver-edge-cases-negfixed-point-combinators-neg-num1

  (is (= FAIL

         FAIL)))

(deftest test-all-negexpected-failures-negconstraint-solver-edge-cases-negfixed-point-combinators-neg-num2

  (is (= FAIL

         FAIL)))

(deftest test-all-negexpected-failures-negconstraint-solver-edge-cases-negfixed-point-combinators-neg-num3

  (is (= FAIL

         FAIL)))

;; Constraint cycles

(deftest test-all-negexpected-failures-negconstraint-solver-edge-cases-negconstraint-cycles-neg-num1

  (is (= FAIL

         FAIL)))

(deftest test-all-negexpected-failures-negconstraint-solver-edge-cases-negconstraint-cycles-neg-num2

  (is (= FAIL

         FAIL)))

;; Primitive function type errors

;; Logic primitive errors

(deftest test-all-negexpected-failures-negprimitive-function-type-errors-neglogic-primitive-errors-neg-num1

  (is (= FAIL

         FAIL)))

(deftest test-all-negexpected-failures-negprimitive-function-type-errors-neglogic-primitive-errors-neg-num2

  (is (= FAIL

         FAIL)))

(deftest test-all-negexpected-failures-negprimitive-function-type-errors-neglogic-primitive-errors-neg-num3

  (is (= FAIL

         FAIL)))

(deftest test-all-negexpected-failures-negprimitive-function-type-errors-neglogic-primitive-errors-neg-num4

  (is (= FAIL

         FAIL)))

;; Collection primitive errors

(deftest test-all-negexpected-failures-negprimitive-function-type-errors-negcollection-primitive-errors-neg-num1

  (is (= FAIL

         FAIL)))

(deftest test-all-negexpected-failures-negprimitive-function-type-errors-negcollection-primitive-errors-neg-num2

  (is (= FAIL

         FAIL)))

(deftest test-all-negexpected-failures-negprimitive-function-type-errors-negcollection-primitive-errors-neg-num3

  (is (= FAIL

         FAIL)))

(deftest test-all-negexpected-failures-negprimitive-function-type-errors-negcollection-primitive-errors-neg-num4

  (is (= FAIL

         FAIL)))

;; Math primitive errors

(deftest test-all-negexpected-failures-negprimitive-function-type-errors-negmath-primitive-errors-neg-num1

  (is (= FAIL

         FAIL)))

(deftest test-all-negexpected-failures-negprimitive-function-type-errors-negmath-primitive-errors-neg-num2

  (is (= FAIL

         FAIL)))

(deftest test-all-negexpected-failures-negprimitive-function-type-errors-negmath-primitive-errors-neg-num3

  (is (= FAIL

         FAIL)))

(deftest test-all-negexpected-failures-negprimitive-function-type-errors-negmath-primitive-errors-neg-num4

  (is (= FAIL

         FAIL)))

;; Complex constraint failures

;; Multi-level constraint conflicts

(deftest test-all-negexpected-failures-negcomplex-constraint-failures-negmulti-neglevel-constraint-conflicts-neg-num1

  (is (= FAIL

         FAIL)))

(deftest test-all-negexpected-failures-negcomplex-constraint-failures-negmulti-neglevel-constraint-conflicts-neg-num2

  (is (= FAIL

         FAIL)))

(deftest test-all-negexpected-failures-negcomplex-constraint-failures-negmulti-neglevel-constraint-conflicts-neg-num3

  (is (= FAIL

         FAIL)))

;; Function composition failures

(deftest test-all-negexpected-failures-negcomplex-constraint-failures-negfunction-composition-failures-neg-num1

  (is (= FAIL

         FAIL)))

(deftest test-all-negexpected-failures-negcomplex-constraint-failures-negfunction-composition-failures-neg-num2

  (is (= FAIL

         FAIL)))

;; Fundamentals

;; Lambdas

;; Simple lambdas

(deftest test-all-negfundamentals-neglambdas-negsimple-lambdas-neg-num1

  (is (= (forall t0. (t0 → t0))

         (forall t0. (t0 → t0)))))

(deftest test-all-negfundamentals-neglambdas-negsimple-lambdas-neg-num2

  (is (= (forall t0. (t0 → int16))

         (forall t0. (t0 → int16)))))

;; Nested lambdas

(deftest test-all-negfundamentals-neglambdas-negnested-lambdas-neg-num1

  (is (= ((int32 → int32 → int32))

         ((int32 → int32 → int32)))))

(deftest test-all-negfundamentals-neglambdas-negnested-lambdas-neg-num2

  (is (= ((int32 → list<(int32 → int32)>))

         ((int32 → list<(int32 → int32)>)))))

;; Nested lambdas with shadowing

(deftest test-all-negfundamentals-neglambdas-negnested-lambdas-with-shadowing-neg-num1

  (is (= (forall t0. (t0 → int32 → int32))

         (forall t0. (t0 → int32 → int32)))))

;; Let terms

;; Simple

(deftest test-all-negfundamentals-neglet-terms-negsimple-neg-num1

  (is (= (forall t0,t1. (t0 → t1 → float32))

         (forall t0,t1. (t0 → t1 → float32)))))

;; Empty let

(deftest test-all-negfundamentals-neglet-terms-negempty-let-neg-num1

  (is (= (int32)

         (int32))))

(deftest test-all-negfundamentals-neglet-terms-negempty-let-neg-num2

  (is (= (forall t0. (t0 → t0))

         (forall t0. (t0 → t0)))))

;; Trivial let

(deftest test-all-negfundamentals-neglet-terms-negtrivial-let-neg-num1

  (is (= (int32)

         (int32))))

(deftest test-all-negfundamentals-neglet-terms-negtrivial-let-neg-num2

  (is (= (int32)

         (int32))))

;; Multiple references to a let-bound term

(deftest test-all-negfundamentals-neglet-terms-negmultiple-references-to-a-let-negbound-term-neg-num1

  (is (= (list<int32>)

         (list<int32>))))

;; Nested let

(deftest test-all-negfundamentals-neglet-terms-negnested-let-neg-num1

  (is (= (list<int32>)

         (list<int32>))))

(deftest test-all-negfundamentals-neglet-terms-negnested-let-neg-num2

  (is (= ((int32, int32))

         ((int32, int32)))))

(deftest test-all-negfundamentals-neglet-terms-negnested-let-neg-num3

  (is (= (forall t0. (list<int32>, (list<string>, list<list<t0>>)))

         (forall t0. (list<int32>, (list<string>, list<list<t0>>))))))

;; Nested let with shadowing

(deftest test-all-negfundamentals-neglet-terms-negnested-let-with-shadowing-neg-num1

  (is (= (int32)

         (int32))))

(deftest test-all-negfundamentals-neglet-terms-negnested-let-with-shadowing-neg-num2

  (is (= ((string, int32))

         ((string, int32)))))

;; Let-polymorphism

(deftest test-all-negfundamentals-neglet-terms-neglet-negpolymorphism-neg-num1

  (is (= (forall t0,t1. (t0 → t1 → float32))

         (forall t0,t1. (t0 → t1 → float32)))))

(deftest test-all-negfundamentals-neglet-terms-neglet-negpolymorphism-neg-num2

  (is (= (((int32 → boolean → boolean) → int32 → boolean → boolean))

         (((int32 → boolean → boolean) → int32 → boolean → boolean)))))

(deftest test-all-negfundamentals-neglet-terms-neglet-negpolymorphism-neg-num3

  (is (= (forall t0. (t0 → t0))

         (forall t0. (t0 → t0)))))

(deftest test-all-negfundamentals-neglet-terms-neglet-negpolymorphism-neg-num4

  (is (= (list<int32>)

         (list<int32>))))

(deftest test-all-negfundamentals-neglet-terms-neglet-negpolymorphism-neg-num5

  (is (= (forall t0. (t0 → list<t0>))

         (forall t0. (t0 → list<t0>)))))

(deftest test-all-negfundamentals-neglet-terms-neglet-negpolymorphism-neg-num6

  (is (= ((int32, string))

         ((int32, string)))))

(deftest test-all-negfundamentals-neglet-terms-neglet-negpolymorphism-neg-num7

  (is (= ((list<int32>, list<string>))

         ((list<int32>, list<string>)))))

(deftest test-all-negfundamentals-neglet-terms-neglet-negpolymorphism-neg-num8

  (is (= (forall t0. list<(int32, t0)>)

         (forall t0. (int32 → t0 → list<(list<int32>, list<t0>)>)))))

(deftest test-all-negfundamentals-neglet-terms-neglet-negpolymorphism-neg-num9

  (is (= ((int32, string))

         ((int32, string)))))

(deftest test-all-negfundamentals-neglet-terms-neglet-negpolymorphism-neg-num10

  (is (= ((int32, string))

         ((int32, string)))))

;; Recursive and mutually recursive let (@wisnesky's test cases)

(deftest test-all-negfundamentals-neglet-terms-negrecursive-and-mutually-recursive-let-wisnesky-s-test-cases--neg-num1

  (is (= (forall t0. (int32 → int32 → t0))

         (forall t0. (int32 → int32 → t0)))))

(deftest test-all-negfundamentals-neglet-terms-negrecursive-and-mutually-recursive-let-wisnesky-s-test-cases--neg-num2

  (is (= (forall t0,t1. (t0, t1))

         (forall t0,t1. (t0, t1)))))

(deftest test-all-negfundamentals-neglet-terms-negrecursive-and-mutually-recursive-let-wisnesky-s-test-cases--neg-num3

  (is (= (forall t0,t1. ((t0 → int32 → t1), (int32 → v0 → t1)))

         (forall t0,t1,t2,t3. ((t0 → int32 → t1), (int32 → t2 → t3))))))

(deftest test-all-negfundamentals-neglet-terms-negrecursive-and-mutually-recursive-let-wisnesky-s-test-cases--neg-num4

  (is (= (int32)

         (int32))))

(deftest test-all-negfundamentals-neglet-terms-negrecursive-and-mutually-recursive-let-wisnesky-s-test-cases--neg-num5

  (is (= (int32)

         (int32))))

(deftest test-all-negfundamentals-neglet-terms-negrecursive-and-mutually-recursive-let-wisnesky-s-test-cases--neg-num6

  (is (= (forall t0,t1. ((t0 → t0), (t1 → t1)))

         (forall t0,t1. ((t0 → t0), (t1 → t1))))))

(deftest test-all-negfundamentals-neglet-terms-negrecursive-and-mutually-recursive-let-wisnesky-s-test-cases--neg-num7

  (is (= (forall t0,t1,t2. ((t0 → t0), ((t1 → t1), (t2 → t2))))

         (forall t0,t1,t2. ((t0 → t0), ((t1 → t1), (t2 → t2)))))))

;; Recursive and mutually recursive let with polymorphism

(deftest test-all-negfundamentals-neglet-terms-negrecursive-and-mutually-recursive-let-with-polymorphism-neg-num1

  (is (= ((int32, string))

         ((int32, string)))))

(deftest test-all-negfundamentals-neglet-terms-negrecursive-and-mutually-recursive-let-with-polymorphism-neg-num2

  (is (= ((int32, string))

         ((int32, string)))))

(deftest test-all-negfundamentals-neglet-terms-negrecursive-and-mutually-recursive-let-with-polymorphism-neg-num3

  (is (= ((int32, string))

         ((int32, string)))))

;; Recursion involving polymorphic functions

(deftest test-all-negfundamentals-neglet-terms-negrecursion-involving-polymorphic-functions-neg-num1

  (is (= (forall t0. (boolean → t0 → list<list<t0>>))

         (forall t0. (boolean → t0 → list<list<t0>>)))))

(deftest test-all-negfundamentals-neglet-terms-negrecursion-involving-polymorphic-functions-neg-num2

  (is (= (forall t0,t1. (boolean, ((t0 → t0) → t1 → t0)))

         (forall t0,t1. (boolean, ((t0 → t0) → t1 → t0))))))

(deftest test-all-negfundamentals-neglet-terms-negrecursion-involving-polymorphic-functions-neg-num3

  (is (= (forall t0. (boolean, ((t0 → t0) → t0)))

         (forall t0. (boolean, ((t0 → t0) → t0))))))

(deftest test-all-negfundamentals-neglet-terms-negrecursion-involving-polymorphic-functions-neg-num4

  (is (= (forall t0. (boolean, (int32, ((t0 → t0) → t0))))

         (forall t0. (boolean, (int32, ((t0 → t0) → t0)))))))

(deftest test-all-negfundamentals-neglet-terms-negrecursion-involving-polymorphic-functions-neg-num5

  (is (= (forall t0,t1. (t0, t1))

         (forall t0,t1. (t0, t1)))))

;; Over-generalization of hoisted let-bindings

(deftest test-all-negfundamentals-neglet-terms-negover-neggeneralization-of-hoisted-let-negbindings-neg-num1

  (is (= (forall t0,t1. ((t0 → (t0, t1)) → t0 → (t0, t1)))

         (forall t0,t1. ((t0 → (t0, t1)) → t0 → (t0, t1))))))

(deftest test-all-negfundamentals-neglet-terms-negover-neggeneralization-of-hoisted-let-negbindings-neg-num2

  (is (= (forall t0,t1. ((t0 → (t0, t1)) → t0 → (t0, t1)))

         (forall t0,t1. ((t0 → (t0, t1)) → t0 → (t0, t1))))))

(deftest test-all-negfundamentals-neglet-terms-negover-neggeneralization-of-hoisted-let-negbindings-neg-num3

  (is (= (forall t0,t1,t2,t3. ((t0 → t1 → (t2, t3)) → t0 → t1 → (t2, t1)))

         (forall t0,t1,t2,t3. ((t0 → t1 → (t2, t3)) → t0 → t1 → (t2, t1))))))

(deftest test-all-negfundamentals-neglet-terms-negover-neggeneralization-of-hoisted-let-negbindings-neg-num4

  (is (= (forall t0. ((t0 → (t0, int32)) → t0 → boolean → (t0, int32)))

         (forall t0. ((t0 → (t0, int32)) → t0 → boolean → (t0, int32))))))

(deftest test-all-negfundamentals-neglet-terms-negover-neggeneralization-of-hoisted-let-negbindings-neg-num5

  (is (= (forall t0. ((t0 → (t0, int32)) → t0 → boolean → (t0, int32)))

         (forall t0. ((t0 → (t0, int32)) → t0 → boolean → (t0, int32))))))

(deftest test-all-negfundamentals-neglet-terms-negover-neggeneralization-of-hoisted-let-negbindings-neg-num6

  (is (= (forall t0. ((t0 → (t0, int32)) → t0 → boolean → (t0, int32)))

         (forall t0. ((t0 → (t0, int32)) → t0 → boolean → (t0, int32))))))

;; Literals

(deftest test-all-negfundamentals-negliterals-neg-num1

  (is (= (int32)

         (int32))))

(deftest test-all-negfundamentals-negliterals-neg-num2

  (is (= (string)

         (string))))

(deftest test-all-negfundamentals-negliterals-neg-num3

  (is (= (boolean)

         (boolean))))

(deftest test-all-negfundamentals-negliterals-neg-num4

  (is (= (float64)

         (float64))))

;; Pathological terms

;; Recursion

(deftest test-all-negfundamentals-negpathological-terms-negrecursion-neg-num1

  (is (= (forall t0. t0)

         (forall t0. t0))))

(deftest test-all-negfundamentals-negpathological-terms-negrecursion-neg-num2

  (is (= (forall t0. (t0 → t0))

         (forall t0. (t0 → t0)))))

(deftest test-all-negfundamentals-negpathological-terms-negrecursion-neg-num3

  (is (= (forall t0. (t0 → t0))

         (forall t0. (t0 → t0)))))

(deftest test-all-negfundamentals-negpathological-terms-negrecursion-neg-num4

  (is (= (forall t0,t1. (t0 → t1))

         (forall t0,t1. (t0 → t1)))))

(deftest test-all-negfundamentals-negpathological-terms-negrecursion-neg-num5

  (is (= (forall t0. ((t0 → t0) → t0))

         (forall t0. ((t0 → t0) → t0)))))

(deftest test-all-negfundamentals-negpathological-terms-negrecursion-neg-num6

  (is (= (int32)

         (int32))))

;; Infinite lists

(deftest test-all-negfundamentals-negpathological-terms-neginfinite-lists-neg-num1

  (is (= (list<int32>)

         (list<int32>))))

(deftest test-all-negfundamentals-negpathological-terms-neginfinite-lists-neg-num2

  (is (= (forall t0. (t0 → list<t0>))

         (forall t0. (t0 → list<t0>)))))

(deftest test-all-negfundamentals-negpathological-terms-neginfinite-lists-neg-num3

  (is (= (forall t0. (t0 → t0))

         (forall t0. (t0 → list<t0>)))))

(deftest test-all-negfundamentals-negpathological-terms-neginfinite-lists-neg-num4

  (is (= (list<int32>)

         (list<int32>))))

;; Polymorphism

;; Simple lists and optionals

(deftest test-all-negfundamentals-negpolymorphism-negsimple-lists-and-optionals-neg-num1

  (is (= (forall t0. list<t0>)

         (forall t0. list<t0>))))

(deftest test-all-negfundamentals-negpolymorphism-negsimple-lists-and-optionals-neg-num2

  (is (= (forall t0. maybe<t0>)

         (forall t0. maybe<t0>))))

(deftest test-all-negfundamentals-negpolymorphism-negsimple-lists-and-optionals-neg-num3

  (is (= (maybe<int32>)

         (maybe<int32>))))

;; Lambdas, lists, and products

(deftest test-all-negfundamentals-negpolymorphism-neglambdas-lists-and-products-neg-num1

  (is (= (forall t0. (t0 → t0))

         (forall t0. (t0 → t0)))))

(deftest test-all-negfundamentals-negpolymorphism-neglambdas-lists-and-products-neg-num2

  (is (= (forall t0. (t0 → (t0, t0)))

         (forall t0. (t0 → (t0, t0))))))

(deftest test-all-negfundamentals-negpolymorphism-neglambdas-lists-and-products-neg-num3

  (is (= (forall t0. (t0 → list<t0>))

         (forall t0. (t0 → list<t0>)))))

(deftest test-all-negfundamentals-negpolymorphism-neglambdas-lists-and-products-neg-num4

  (is (= (forall t0. list<(t0 → t0)>)

         (forall t0. list<(t0 → t0)>))))

(deftest test-all-negfundamentals-negpolymorphism-neglambdas-lists-and-products-neg-num5

  (is (= (forall t0,t1. list<(t0 → t1 → (t1, t0))>)

         (forall t0,t1. list<(t0 → t1 → (t1, t0))>))))

;; Lambdas and application

(deftest test-all-negfundamentals-negpolymorphism-neglambdas-and-application-neg-num1

  (is (= (string)

         (string))))

;; Primitives and application

(deftest test-all-negfundamentals-negpolymorphism-negprimitives-and-application-neg-num1

  (is (= (list<int32>)

         (list<int32>))))

;; Lambdas and primitives

(deftest test-all-negfundamentals-negpolymorphism-neglambdas-and-primitives-neg-num1

  (is (= ((int32 → int32 → int32))

         ((int32 → int32 → int32)))))

(deftest test-all-negfundamentals-negpolymorphism-neglambdas-and-primitives-neg-num2

  (is (= ((int32 → int32 → int32))

         ((int32 → int32 → int32)))))

(deftest test-all-negfundamentals-negpolymorphism-neglambdas-and-primitives-neg-num3

  (is (= ((int32 → int32))

         ((int32 → int32)))))

;; Mixed expressions with lambdas, constants, and primitive functions

(deftest test-all-negfundamentals-negpolymorphism-negmixed-expressions-with-lambdas-constants-and-primitive-functions-neg-num1

  (is (= ((int32 → int32))

         ((int32 → int32)))))

;; Application terms

(deftest test-all-negfundamentals-negpolymorphism-negapplication-terms-neg-num1

  (is (= (string)

         (string))))

(deftest test-all-negfundamentals-negpolymorphism-negapplication-terms-neg-num2

  (is (= ((int32 → int32))

         ((int32 → int32)))))

;; Primitives

;; Monomorphic primitive functions

(deftest test-all-negfundamentals-negprimitives-negmonomorphic-primitive-functions-neg-num1

  (is (= ((string → int32))

         ((string → int32)))))

(deftest test-all-negfundamentals-negprimitives-negmonomorphic-primitive-functions-neg-num2

  (is (= ((int32 → int32 → int32))

         ((int32 → int32 → int32)))))

;; Polymorphic primitive functions

(deftest test-all-negfundamentals-negprimitives-negpolymorphic-primitive-functions-neg-num1

  (is (= (forall t0. (t0 → int32))

         (forall t0. (t0 → int32)))))

(deftest test-all-negfundamentals-negprimitives-negpolymorphic-primitive-functions-neg-num2

  (is (= ((int32 → int32))

         ((int32 → int32)))))

(deftest test-all-negfundamentals-negprimitives-negpolymorphic-primitive-functions-neg-num3

  (is (= (forall t0. (list<list<t0>> → list<t0>))

         (forall t0. (list<list<t0>> → list<t0>)))))

(deftest test-all-negfundamentals-negprimitives-negpolymorphic-primitive-functions-neg-num4

  (is (= (forall t0. (list<list<t0>> → list<t0>))

         (forall t0. (list<list<t0>> → list<t0>)))))

(deftest test-all-negfundamentals-negprimitives-negpolymorphic-primitive-functions-neg-num5

  (is (= (forall t0. (list<list<t0>> → int32))

         (forall t0. (list<list<t0>> → int32)))))

(deftest test-all-negfundamentals-negprimitives-negpolymorphic-primitive-functions-neg-num6

  (is (= (forall t0. (list<t0> → int32))

         (forall t0. (list<t0> → int32)))))

(deftest test-all-negfundamentals-negprimitives-negpolymorphic-primitive-functions-neg-num7

  (is (= (forall t0. (list<t0> → int32))

         (forall t0. (list<t0> → int32)))))

(deftest test-all-negfundamentals-negprimitives-negpolymorphic-primitive-functions-neg-num8

  (is (= (forall t0. (list<list<t0>> → int32))

         (forall t0. (list<list<t0>> → int32)))))

;; Examples from the Hydra kernel

;; Nested let

;; hydra.formatting.mapFirstLetter

(deftest test-all-negexamples-from-the-hydra-kernel-negnested-let-neghydra-dotformatting-dotmapfirstletter-neg-num1

  (is (= (((string → string) → string → string))

         (((string → string) → string → string)))))

;; Recursive let with pair return (ifElse)

(deftest test-all-negexamples-from-the-hydra-kernel-negnested-let-negrecursive-let-with-pair-return-ifelse--neg-num2

  (is (= ((string → (map<string, string>, string)))

         ((string → (map<string, string>, string))))))

;; Recursive let with pair return (case on Type)

(deftest test-all-negexamples-from-the-hydra-kernel-negnested-let-negrecursive-let-with-pair-return-case-on-type--neg-num3

  (is (= ((hydra.core.Type → (map<hydra.core.Name, hydra.core.Name>, hydra.core.Type)))

         ((hydra.core.Type → (map<hydra.core.Name, hydra.core.Name>, hydra.core.Type))))))

;; Nominal terms

;; Case statements

(deftest test-all-negnominal-terms-negcase-statements-neg-num1

  (is (= ((SimpleNumber → int32))

         ((SimpleNumber → int32)))))

(deftest test-all-negnominal-terms-negcase-statements-neg-num2

  (is (= ((UnionMonomorphic → boolean))

         ((UnionMonomorphic → boolean)))))

;; Projections

;; Record eliminations

(deftest test-all-negnominal-terms-negprojections-negrecord-eliminations-neg-num1

  (is (= ((Person → string))

         ((Person → string)))))

;; Pair projections

(deftest test-all-negnominal-terms-negprojections-negpair-projections-neg-num1

  (is (= (forall t0,t1. ((t0, t1) → t0))

         (forall t0,t1. ((t0, t1) → t0)))))

(deftest test-all-negnominal-terms-negprojections-negpair-projections-neg-num2

  (is (= (string)

         (string))))

;; Records

;; Simple records

(deftest test-all-negnominal-terms-negrecords-negsimple-records-neg-num1

  (is (= (LatLon)

         (LatLon))))

(deftest test-all-negnominal-terms-negrecords-negsimple-records-neg-num2

  (is (= ((LatLonPoly @ float32))

         ((LatLonPoly @ float32)))))

(deftest test-all-negnominal-terms-negrecords-negsimple-records-neg-num3

  (is (= ((float32 → (LatLonPoly @ float32)))

         ((float32 → (LatLonPoly @ float32))))))

(deftest test-all-negnominal-terms-negrecords-negsimple-records-neg-num4

  (is (= (forall t0. (t0 → (LatLonPoly @ t0)))

         (forall t0. (t0 → (LatLonPoly @ t0))))))

(deftest test-all-negnominal-terms-negrecords-negsimple-records-neg-num5

  (is (= (Person)

         (Person))))

;; Record instances of simply recursive record types

(deftest test-all-negnominal-terms-negrecords-negrecord-instances-of-simply-recursive-record-types-neg-num1

  (is (= (IntList)

         (IntList))))

(deftest test-all-negnominal-terms-negrecords-negrecord-instances-of-simply-recursive-record-types-neg-num2

  (is (= (IntList)

         (IntList))))

(deftest test-all-negnominal-terms-negrecords-negrecord-instances-of-simply-recursive-record-types-neg-num3

  (is (= ((List @ int32))

         ((List @ int32)))))

(deftest test-all-negnominal-terms-negrecords-negrecord-instances-of-simply-recursive-record-types-neg-num4

  (is (= ((List @ int32))

         ((List @ int32)))))

(deftest test-all-negnominal-terms-negrecords-negrecord-instances-of-simply-recursive-record-types-neg-num5

  (is (= (forall t0. (t0 → (List @ t0)))

         (forall t0. (t0 → (List @ t0))))))

;; Record instances of mutually recursive record types

(deftest test-all-negnominal-terms-negrecords-negrecord-instances-of-mutually-recursive-record-types-neg-num1

  (is (= ((BuddyListA @ int32))

         ((BuddyListA @ int32)))))

(deftest test-all-negnominal-terms-negrecords-negrecord-instances-of-mutually-recursive-record-types-neg-num2

  (is (= (forall t0. (t0 → (BuddyListA @ t0)))

         (forall t0. (t0 → (BuddyListA @ t0))))))

;; Variant terms

;; Variants

(deftest test-all-negnominal-terms-negvariant-terms-negvariants-neg-num1

  (is (= (Timestamp)

         (Timestamp))))

(deftest test-all-negnominal-terms-negvariant-terms-negvariants-neg-num2

  (is (= (UnionMonomorphic)

         (UnionMonomorphic))))

;; Polymorphic and recursive variants

(deftest test-all-negnominal-terms-negvariant-terms-negpolymorphic-and-recursive-variants-neg-num1

  (is (= (forall t0. (UnionPolymorphicRecursive @ t0))

         (forall t0. (UnionPolymorphicRecursive @ t0)))))

(deftest test-all-negnominal-terms-negvariant-terms-negpolymorphic-and-recursive-variants-neg-num2

  (is (= ((UnionPolymorphicRecursive @ string))

         ((UnionPolymorphicRecursive @ string)))))

(deftest test-all-negnominal-terms-negvariant-terms-negpolymorphic-and-recursive-variants-neg-num3

  (is (= ((UnionPolymorphicRecursive @ int32))

         ((UnionPolymorphicRecursive @ int32)))))

;; Wrapper introductions and eliminations

;; Wrapper introductions

(deftest test-all-negnominal-terms-negwrapper-introductions-and-eliminations-negwrapper-introductions-neg-num1

  (is (= (StringAlias)

         (StringAlias))))

(deftest test-all-negnominal-terms-negwrapper-introductions-and-eliminations-negwrapper-introductions-neg-num2

  (is (= ((string → StringAlias))

         ((string → StringAlias)))))

;; Wrapper eliminations

(deftest test-all-negnominal-terms-negwrapper-introductions-and-eliminations-negwrapper-eliminations-neg-num1

  (is (= ((StringAlias → string))

         ((StringAlias → string)))))

(deftest test-all-negnominal-terms-negwrapper-introductions-and-eliminations-negwrapper-eliminations-neg-num2

  (is (= (string)

         (string))))
