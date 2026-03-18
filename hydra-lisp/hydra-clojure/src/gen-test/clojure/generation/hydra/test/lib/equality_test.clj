;; Note: this is an automatically generated file. Do not edit.
;; hydra.lib.equality primitives

(ns generation.hydra.test.lib.equality-test
  (:require [clojure.test :refer :all]))

;; compare

(deftest test-compare-negless-than

  (is (= (list :less_than nil)

         ((hydra_lib_equality_compare 3) 5))))

(deftest test-compare-negequal

  (is (= (list :equal_to nil)

         ((hydra_lib_equality_compare 5) 5))))

(deftest test-compare-neggreater-than

  (is (= (list :greater_than nil)

         ((hydra_lib_equality_compare 5) 3))))

;; equal

(deftest test-equal-negequal-integers

  (is (= true

         ((hydra_lib_equality_equal 5) 5))))

(deftest test-equal-negunequal-integers

  (is (= false

         ((hydra_lib_equality_equal 5) 3))))

;; gt

(deftest test-gt-neggreater

  (is (= true

         ((hydra_lib_equality_gt 5) 3))))

(deftest test-gt-negequal

  (is (= false

         ((hydra_lib_equality_gt 5) 5))))

(deftest test-gt-negless

  (is (= false

         ((hydra_lib_equality_gt 3) 5))))

;; gte

(deftest test-gte-neggreater

  (is (= true

         ((hydra_lib_equality_gte 5) 3))))

(deftest test-gte-negequal

  (is (= true

         ((hydra_lib_equality_gte 5) 5))))

(deftest test-gte-negless

  (is (= false

         ((hydra_lib_equality_gte 3) 5))))

;; identity

(deftest test-identity-neginteger

  (is (= 42

         (hydra_lib_equality_identity 42))))

;; lt

(deftest test-lt-negless

  (is (= true

         ((hydra_lib_equality_lt 3) 5))))

(deftest test-lt-negequal

  (is (= false

         ((hydra_lib_equality_lt 5) 5))))

(deftest test-lt-neggreater

  (is (= false

         ((hydra_lib_equality_lt 5) 3))))

;; lte

(deftest test-lte-negless

  (is (= true

         ((hydra_lib_equality_lte 3) 5))))

(deftest test-lte-negequal

  (is (= true

         ((hydra_lib_equality_lte 5) 5))))

(deftest test-lte-neggreater

  (is (= false

         ((hydra_lib_equality_lte 5) 3))))

;; max

(deftest test-max-negfirst-greater

  (is (= 5

         ((hydra_lib_equality_max 5) 3))))

(deftest test-max-negsecond-greater

  (is (= 5

         ((hydra_lib_equality_max 3) 5))))

(deftest test-max-negequal

  (is (= 5

         ((hydra_lib_equality_max 5) 5))))

;; min

(deftest test-min-negfirst-less

  (is (= 3

         ((hydra_lib_equality_min 3) 5))))

(deftest test-min-negsecond-less

  (is (= 3

         ((hydra_lib_equality_min 5) 3))))

(deftest test-min-negequal

  (is (= 5

         ((hydra_lib_equality_min 5) 5))))

;; compare strings

(deftest test-compare-strings-negless-than-lexicographic

  (is (= (list :less_than nil)

         ((hydra_lib_equality_compare "apple") "banana"))))

(deftest test-compare-strings-negequal

  (is (= (list :equal_to nil)

         ((hydra_lib_equality_compare "hello") "hello"))))

(deftest test-compare-strings-neggreater-than-lexicographic

  (is (= (list :greater_than nil)

         ((hydra_lib_equality_compare "zebra") "apple"))))

(deftest test-compare-strings-negempty-vs-non-negempty

  (is (= (list :less_than nil)

         ((hydra_lib_equality_compare "") "a"))))

(deftest test-compare-strings-negprefix-vs-longer

  (is (= (list :less_than nil)

         ((hydra_lib_equality_compare "ab") "abc"))))

;; lt strings

(deftest test-lt-strings-negless-lexicographic

  (is (= true

         ((hydra_lib_equality_lt "apple") "banana"))))

(deftest test-lt-strings-negequal

  (is (= false

         ((hydra_lib_equality_lt "hello") "hello"))))

(deftest test-lt-strings-neggreater

  (is (= false

         ((hydra_lib_equality_lt "zebra") "apple"))))

;; gt strings

(deftest test-gt-strings-neggreater-lexicographic

  (is (= true

         ((hydra_lib_equality_gt "zebra") "apple"))))

(deftest test-gt-strings-negequal

  (is (= false

         ((hydra_lib_equality_gt "hello") "hello"))))

(deftest test-gt-strings-negless

  (is (= false

         ((hydra_lib_equality_gt "apple") "banana"))))

;; max strings

(deftest test-max-strings-negfirst-greater

  (is (= "zebra"

         ((hydra_lib_equality_max "zebra") "apple"))))

(deftest test-max-strings-negsecond-greater

  (is (= "zebra"

         ((hydra_lib_equality_max "apple") "zebra"))))

(deftest test-max-strings-negequal

  (is (= "hello"

         ((hydra_lib_equality_max "hello") "hello"))))

;; min strings

(deftest test-min-strings-negfirst-less

  (is (= "apple"

         ((hydra_lib_equality_min "apple") "zebra"))))

(deftest test-min-strings-negsecond-less

  (is (= "apple"

         ((hydra_lib_equality_min "zebra") "apple"))))

(deftest test-min-strings-negequal

  (is (= "hello"

         ((hydra_lib_equality_min "hello") "hello"))))

;; compare floats

(deftest test-compare-floats-negless-than

  (is (= (list :less_than nil)

         ((hydra_lib_equality_compare 1.5) 2.5))))

(deftest test-compare-floats-negequal

  (is (= (list :equal_to nil)

         ((hydra_lib_equality_compare 3.14) 3.14))))

(deftest test-compare-floats-neggreater-than

  (is (= (list :greater_than nil)

         ((hydra_lib_equality_compare 5.0) 3.0))))

(deftest test-compare-floats-negnegative-vs-positive

  (is (= (list :less_than nil)

         ((hydra_lib_equality_compare -1.0) 1.0))))

;; lt floats

(deftest test-lt-floats-negless

  (is (= true

         ((hydra_lib_equality_lt 1.5) 2.5))))

(deftest test-lt-floats-negequal

  (is (= false

         ((hydra_lib_equality_lt 3.14) 3.14))))

(deftest test-lt-floats-neggreater

  (is (= false

         ((hydra_lib_equality_lt 5.0) 3.0))))

;; gt floats

(deftest test-gt-floats-neggreater

  (is (= true

         ((hydra_lib_equality_gt 5.0) 3.0))))

(deftest test-gt-floats-negequal

  (is (= false

         ((hydra_lib_equality_gt 3.14) 3.14))))

(deftest test-gt-floats-negless

  (is (= false

         ((hydra_lib_equality_gt 1.5) 2.5))))
