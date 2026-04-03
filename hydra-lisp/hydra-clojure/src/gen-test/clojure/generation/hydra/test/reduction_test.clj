;; Note: this is an automatically generated file. Do not edit.
;; reduction

(ns test-ns
  (:require [clojure.test :refer :all]))

;; beta reduction

(deftest test-reduction-negbeta-reduction-negidentity-function-applied-to-literal

  (is (= 42:int32

         42:int32)))

(deftest test-reduction-negbeta-reduction-negconstant-function

  (is (= 1:int32

         1:int32)))

(deftest test-reduction-negbeta-reduction-negnested-application

  (is (= 1:int32

         1:int32)))

;; monomorphic primitives

(deftest test-reduction-negmonomorphic-primitives-negtoupper-on-lowercase

  (is (= "HELLO"

         "HELLO")))

(deftest test-reduction-negmonomorphic-primitives-negtoupper-on-mixed-case

  (is (= "HELLO WORLD"

         "HELLO WORLD")))

(deftest test-reduction-negmonomorphic-primitives-negtoupper-on-empty-string

  (is (= ""

         "")))

(deftest test-reduction-negmonomorphic-primitives-negtolower-on-uppercase

  (is (= "hello"

         "hello")))

(deftest test-reduction-negmonomorphic-primitives-negstring-length

  (is (= 5:int32

         5:int32)))

(deftest test-reduction-negmonomorphic-primitives-negstring-length-of-empty

  (is (= 0:int32

         0:int32)))

(deftest test-reduction-negmonomorphic-primitives-negadd-two-positive-integers

  (is (= 8:int32

         8:int32)))

(deftest test-reduction-negmonomorphic-primitives-negadd-negative-and-positive

  (is (= -7:int32

         -7:int32)))

(deftest test-reduction-negmonomorphic-primitives-negadd-with-zero

  (is (= 42:int32

         42:int32)))

(deftest test-reduction-negmonomorphic-primitives-negsubtract-integers

  (is (= 7:int32

         7:int32)))

(deftest test-reduction-negmonomorphic-primitives-negmultiply-integers

  (is (= 42:int32

         42:int32)))

(deftest test-reduction-negmonomorphic-primitives-negmultiply-by-zero

  (is (= 0:int32

         0:int32)))

(deftest test-reduction-negmonomorphic-primitives-negdivide-integers

  (is (= 5:int32

         5:int32)))

(deftest test-reduction-negmonomorphic-primitives-negmodulo

  (is (= 2:int32

         2:int32)))

(deftest test-reduction-negmonomorphic-primitives-negspliton-basic

  (is (= ["a", "b", "c"]

         ["a", "b", "c"])))

(deftest test-reduction-negmonomorphic-primitives-negcat2-strings

  (is (= "helloworld"

         "helloworld")))

;; polymorphic primitives

(deftest test-reduction-negpolymorphic-primitives-neglength-of-integer-list

  (is (= 3:int32

         3:int32)))

(deftest test-reduction-negpolymorphic-primitives-neglength-of-string-list

  (is (= 2:int32

         2:int32)))

(deftest test-reduction-negpolymorphic-primitives-neglength-of-empty-list

  (is (= 0:int32

         0:int32)))

(deftest test-reduction-negpolymorphic-primitives-neglength-of-single-element-list

  (is (= 1:int32

         1:int32)))

(deftest test-reduction-negpolymorphic-primitives-neghead-of-integer-list

  (is (= 10:int32

         10:int32)))

(deftest test-reduction-negpolymorphic-primitives-neghead-of-string-list

  (is (= "first"

         "first")))

(deftest test-reduction-negpolymorphic-primitives-neglast-of-integer-list

  (is (= 30:int32

         30:int32)))

(deftest test-reduction-negpolymorphic-primitives-negconcat-two-integer-lists

  (is (= [1:int32, 2:int32, 3:int32, 4:int32]

         [1:int32, 2:int32, 3:int32, 4:int32])))

(deftest test-reduction-negpolymorphic-primitives-negconcat-with-empty-list

  (is (= [1:int32, 2:int32]

         [1:int32, 2:int32])))

(deftest test-reduction-negpolymorphic-primitives-negreverse-integer-list

  (is (= [3:int32, 2:int32, 1:int32]

         [3:int32, 2:int32, 1:int32])))

(deftest test-reduction-negpolymorphic-primitives-negreverse-empty-list

  (is (= []

         [])))

;; nullary primitives

(deftest test-reduction-negnullary-primitives-negempty-set-has-size-zero

  (is (= 0:int32

         0:int32)))

;; literals as values

(deftest test-reduction-negliterals-as-values-neginteger-literal-is-a-value

  (is (= 42:int32

         42:int32)))

(deftest test-reduction-negliterals-as-values-negnegative-integer-literal

  (is (= -17:int32

         -17:int32)))

(deftest test-reduction-negliterals-as-values-negzero-integer-literal

  (is (= 0:int32

         0:int32)))

(deftest test-reduction-negliterals-as-values-negstring-literal-is-a-value

  (is (= "hello"

         "hello")))

(deftest test-reduction-negliterals-as-values-negempty-string-literal

  (is (= ""

         "")))

(deftest test-reduction-negliterals-as-values-negstring-with-special-characters

  (is (= "hello\nworld\ttab"

         "hello\nworld\ttab")))

(deftest test-reduction-negliterals-as-values-negboolean-true-is-a-value

  (is (= true

         true)))

(deftest test-reduction-negliterals-as-values-negboolean-false-is-a-value

  (is (= false

         false)))

(deftest test-reduction-negliterals-as-values-negfloat-literal-is-a-value

  (is (= 3.14:float64

         3.14:float64)))

(deftest test-reduction-negliterals-as-values-negnegative-float-literal

  (is (= -2.718:float64

         -2.718:float64)))

(deftest test-reduction-negliterals-as-values-negzero-float-literal

  (is (= 0.0:float64

         0.0:float64)))

;; list reduction

(deftest test-reduction-neglist-reduction-negempty-list-is-a-value

  (is (= []

         [])))

(deftest test-reduction-neglist-reduction-neglist-of-literals-is-a-value

  (is (= [1:int32, 2:int32, 3:int32]

         [1:int32, 2:int32, 3:int32])))

(deftest test-reduction-neglist-reduction-neglist-with-reducible-element

  (is (= [42:int32]

         [42:int32])))

;; optional reduction

(deftest test-reduction-negoptional-reduction-negnothing-is-a-value

  (is (= nothing

         nothing)))

(deftest test-reduction-negoptional-reduction-negjust-literal-is-a-value

  (is (= just(42:int32)

         just(42:int32))))

(deftest test-reduction-negoptional-reduction-negjust-with-reducible-content

  (is (= just(42:int32)

         just(42:int32))))

;; alpha conversion

(deftest test-reduction-negalpha-conversion-negvariable-at-top-level

  (is (= y

         y)))

(deftest test-reduction-negalpha-conversion-negvariable-in-list

  (is (= [42:int32, y]

         [42:int32, y])))

(deftest test-reduction-negalpha-conversion-neglambda-with-different-variable-is-transparent

  (is (= λz.[42:int32, y, z]

         λz.[42:int32, y, z])))

(deftest test-reduction-negalpha-conversion-neglambda-with-same-variable-is-opaque

  (is (= λx.[42:int32, x, z]

         λx.[42:int32, x, z])))

(deftest test-reduction-negalpha-conversion-negnested-lambda-outer-variable

  (is (= λa.λb.y

         λa.λb.y)))

(deftest test-reduction-negalpha-conversion-negnested-lambda-shadows-outer

  (is (= λx.λy.x

         λx.λy.x)))

(deftest test-reduction-negalpha-conversion-negapplication-with-variable

  (is (= (f @ y)

         (f @ y))))

(deftest test-reduction-negalpha-conversion-negapplication-with-both-variables-same

  (is (= (y @ y)

         (y @ y))))

;; type reduction

(deftest test-reduction-negtype-reduction-negunit-type-unchanged

  (is (= unit

         unit)))

(deftest test-reduction-negtype-reduction-negstring-type-unchanged

  (is (= string

         string)))

(deftest test-reduction-negtype-reduction-negint32-type-unchanged

  (is (= int32

         int32)))

(deftest test-reduction-negtype-reduction-negidentity-type-applied-to-string

  (is (= (string → string)

         (string → string))))

(deftest test-reduction-negtype-reduction-negconstant-type-ignores-argument

  (is (= int32

         int32)))

(deftest test-reduction-negtype-reduction-negnested-forall-first-application

  (is (= (∀y.(int32 → y))

         (∀y.(int32 → y)))))

(deftest test-reduction-negtype-reduction-negnested-forall-both-applications

  (is (= (int32 → string)

         (int32 → string))))

(deftest test-reduction-negtype-reduction-neglist-type-applied

  (is (= list<int32>

         list<int32>)))

(deftest test-reduction-negtype-reduction-negoptional-type-applied

  (is (= maybe<string>

         maybe<string>)))
