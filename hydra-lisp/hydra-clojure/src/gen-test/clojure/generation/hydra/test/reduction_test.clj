;; Note: this is an automatically generated file. Do not edit.
;; reduction

(ns generation.hydra.test.reduction-test
  (:require [clojure.test :refer :all]))

;; beta reduction

(deftest test-beta-reduction-negidentity-function-applied-to-literal

  (is (= 42

         ((fn [x] x) 42))))

(deftest test-beta-reduction-negconstant-function

  (is (= 1

         ((fn [x] 1) 42))))

(deftest test-beta-reduction-negnested-application

  (is (= 1

         (((fn [x] (fn [y] x)) 1) 2))))

;; monomorphic primitives

(deftest test-monomorphic-primitives-negtoupper-on-lowercase

  (is (= "HELLO"

         (hydra_lib_strings_to_upper "hello"))))

(deftest test-monomorphic-primitives-negtoupper-on-mixed-case

  (is (= "HELLO WORLD"

         (hydra_lib_strings_to_upper "Hello World"))))

(deftest test-monomorphic-primitives-negtoupper-on-empty-string

  (is (= ""

         (hydra_lib_strings_to_upper ""))))

(deftest test-monomorphic-primitives-negtolower-on-uppercase

  (is (= "hello"

         (hydra_lib_strings_to_lower "HELLO"))))

(deftest test-monomorphic-primitives-negstring-length

  (is (= 5

         (hydra_lib_strings_length "hello"))))

(deftest test-monomorphic-primitives-negstring-length-of-empty

  (is (= 0

         (hydra_lib_strings_length ""))))

(deftest test-monomorphic-primitives-negadd-two-positive-integers

  (is (= 8

         ((hydra_lib_math_add 3) 5))))

(deftest test-monomorphic-primitives-negadd-negative-and-positive

  (is (= -7

         ((hydra_lib_math_add -10) 3))))

(deftest test-monomorphic-primitives-negadd-with-zero

  (is (= 42

         ((hydra_lib_math_add 0) 42))))

(deftest test-monomorphic-primitives-negsubtract-integers

  (is (= 7

         ((hydra_lib_math_sub 10) 3))))

(deftest test-monomorphic-primitives-negmultiply-integers

  (is (= 42

         ((hydra_lib_math_mul 6) 7))))

(deftest test-monomorphic-primitives-negmultiply-by-zero

  (is (= 0

         ((hydra_lib_math_mul 100) 0))))

(deftest test-monomorphic-primitives-negdivide-integers

  (is (= 5

         ((hydra_lib_math_div 20) 4))))

(deftest test-monomorphic-primitives-negmodulo

  (is (= 2

         ((hydra_lib_math_mod 17) 5))))

(deftest test-monomorphic-primitives-negspliton-basic

  (is (= (list "a" "b" "c")

         ((hydra_lib_strings_split_on ",") "a,b,c"))))

(deftest test-monomorphic-primitives-negcat2-strings

  (is (= "helloworld"

         ((hydra_lib_strings_cat2 "hello") "world"))))

;; polymorphic primitives

(deftest test-polymorphic-primitives-neglength-of-integer-list

  (is (= 3

         (hydra_lib_lists_length (list 1 2 3)))))

(deftest test-polymorphic-primitives-neglength-of-string-list

  (is (= 2

         (hydra_lib_lists_length (list "a" "b")))))

(deftest test-polymorphic-primitives-neglength-of-empty-list

  (is (= 0

         (hydra_lib_lists_length (list )))))

(deftest test-polymorphic-primitives-neglength-of-single-element-list

  (is (= 1

         (hydra_lib_lists_length (list true)))))

(deftest test-polymorphic-primitives-neghead-of-integer-list

  (is (= 10

         (hydra_lib_lists_head (list 10 20 30)))))

(deftest test-polymorphic-primitives-neghead-of-string-list

  (is (= "first"

         (hydra_lib_lists_head (list "first" "second")))))

(deftest test-polymorphic-primitives-neglast-of-integer-list

  (is (= 30

         (hydra_lib_lists_last (list 10 20 30)))))

(deftest test-polymorphic-primitives-negconcat-two-integer-lists

  (is (= (list 1 2 3 4)

         ((hydra_lib_lists_concat2 (list 1 2)) (list 3 4)))))

(deftest test-polymorphic-primitives-negconcat-with-empty-list

  (is (= (list 1 2)

         ((hydra_lib_lists_concat2 (list )) (list 1 2)))))

(deftest test-polymorphic-primitives-negreverse-integer-list

  (is (= (list 3 2 1)

         (hydra_lib_lists_reverse (list 1 2 3)))))

(deftest test-polymorphic-primitives-negreverse-empty-list

  (is (= (list )

         (hydra_lib_lists_reverse (list )))))

;; nullary primitives

(deftest test-nullary-primitives-negempty-set-has-size-zero

  (is (= 0

         (hydra_lib_sets_size hydra_lib_sets_empty))))

;; literals as values

(deftest test-literals-as-values-neginteger-literal-is-a-value

  (is (= 42

         42)))

(deftest test-literals-as-values-negnegative-integer-literal

  (is (= -17

         -17)))

(deftest test-literals-as-values-negzero-integer-literal

  (is (= 0

         0)))

(deftest test-literals-as-values-negstring-literal-is-a-value

  (is (= "hello"

         "hello")))

(deftest test-literals-as-values-negempty-string-literal

  (is (= ""

         "")))

(deftest test-literals-as-values-negstring-with-special-characters

  (is (= "hello\nworld\ttab"

         "hello\nworld\ttab")))

(deftest test-literals-as-values-negboolean-true-is-a-value

  (is (= true

         true)))

(deftest test-literals-as-values-negboolean-false-is-a-value

  (is (= false

         false)))

(deftest test-literals-as-values-negfloat-literal-is-a-value

  (is (= 3.14

         3.14)))

(deftest test-literals-as-values-negnegative-float-literal

  (is (= -2.718

         -2.718)))

(deftest test-literals-as-values-negzero-float-literal

  (is (= 0.0

         0.0)))

;; list reduction

(deftest test-list-reduction-negempty-list-is-a-value

  (is (= (list )

         (list ))))

(deftest test-list-reduction-neglist-of-literals-is-a-value

  (is (= (list 1 2 3)

         (list 1 2 3))))

(deftest test-list-reduction-neglist-with-reducible-element

  (is (= (list 42)

         (list ((fn [x] x) 42)))))

;; optional reduction

(deftest test-optional-reduction-negnothing-is-a-value

  (is (= (list :nothing)

         (list :nothing))))

(deftest test-optional-reduction-negjust-literal-is-a-value

  (is (= (list :just 42)

         (list :just 42))))

(deftest test-optional-reduction-negjust-with-reducible-content

  (is (= (list :just 42)

         (list :just ((fn [x] x) 42)))))
