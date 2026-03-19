;; Note: this is an automatically generated file. Do not edit.
;; hydra.lib.equality primitives

(ns generation.hydra.test.lib.equality-test
  (:require [clojure.test :refer :all]))

;; compare

(deftest test-equality-negcompare-negless-than

  (is (= (list :less_than nil)

         ((hydra_lib_equality_compare 3) 5))))

(deftest test-equality-negcompare-negequal

  (is (= (list :equal_to nil)

         ((hydra_lib_equality_compare 5) 5))))

(deftest test-equality-negcompare-neggreater-than

  (is (= (list :greater_than nil)

         ((hydra_lib_equality_compare 5) 3))))

;; equal

(deftest test-equality-negequal-negequal-integers

  (is (= true

         ((hydra_lib_equality_equal 5) 5))))

(deftest test-equality-negequal-negunequal-integers

  (is (= false

         ((hydra_lib_equality_equal 5) 3))))

;; gt

(deftest test-equality-neggt-neggreater

  (is (= true

         ((hydra_lib_equality_gt 5) 3))))

(deftest test-equality-neggt-negequal

  (is (= false

         ((hydra_lib_equality_gt 5) 5))))

(deftest test-equality-neggt-negless

  (is (= false

         ((hydra_lib_equality_gt 3) 5))))

;; gte

(deftest test-equality-neggte-neggreater

  (is (= true

         ((hydra_lib_equality_gte 5) 3))))

(deftest test-equality-neggte-negequal

  (is (= true

         ((hydra_lib_equality_gte 5) 5))))

(deftest test-equality-neggte-negless

  (is (= false

         ((hydra_lib_equality_gte 3) 5))))

;; identity

(deftest test-equality-negidentity-neginteger

  (is (= 42

         (hydra_lib_equality_identity 42))))

;; lt

(deftest test-equality-neglt-negless

  (is (= true

         ((hydra_lib_equality_lt 3) 5))))

(deftest test-equality-neglt-negequal

  (is (= false

         ((hydra_lib_equality_lt 5) 5))))

(deftest test-equality-neglt-neggreater

  (is (= false

         ((hydra_lib_equality_lt 5) 3))))

;; lte

(deftest test-equality-neglte-negless

  (is (= true

         ((hydra_lib_equality_lte 3) 5))))

(deftest test-equality-neglte-negequal

  (is (= true

         ((hydra_lib_equality_lte 5) 5))))

(deftest test-equality-neglte-neggreater

  (is (= false

         ((hydra_lib_equality_lte 5) 3))))

;; max

(deftest test-equality-negmax-negfirst-greater

  (is (= 5

         ((hydra_lib_equality_max 5) 3))))

(deftest test-equality-negmax-negsecond-greater

  (is (= 5

         ((hydra_lib_equality_max 3) 5))))

(deftest test-equality-negmax-negequal

  (is (= 5

         ((hydra_lib_equality_max 5) 5))))

;; min

(deftest test-equality-negmin-negfirst-less

  (is (= 3

         ((hydra_lib_equality_min 3) 5))))

(deftest test-equality-negmin-negsecond-less

  (is (= 3

         ((hydra_lib_equality_min 5) 3))))

(deftest test-equality-negmin-negequal

  (is (= 5

         ((hydra_lib_equality_min 5) 5))))

;; compare strings

(deftest test-equality-negcompare-strings-negless-than-lexicographic

  (is (= (list :less_than nil)

         ((hydra_lib_equality_compare "apple") "banana"))))

(deftest test-equality-negcompare-strings-negequal

  (is (= (list :equal_to nil)

         ((hydra_lib_equality_compare "hello") "hello"))))

(deftest test-equality-negcompare-strings-neggreater-than-lexicographic

  (is (= (list :greater_than nil)

         ((hydra_lib_equality_compare "zebra") "apple"))))

(deftest test-equality-negcompare-strings-negempty-vs-non-negempty

  (is (= (list :less_than nil)

         ((hydra_lib_equality_compare "") "a"))))

(deftest test-equality-negcompare-strings-negprefix-vs-longer

  (is (= (list :less_than nil)

         ((hydra_lib_equality_compare "ab") "abc"))))

;; lt strings

(deftest test-equality-neglt-strings-negless-lexicographic

  (is (= true

         ((hydra_lib_equality_lt "apple") "banana"))))

(deftest test-equality-neglt-strings-negequal

  (is (= false

         ((hydra_lib_equality_lt "hello") "hello"))))

(deftest test-equality-neglt-strings-neggreater

  (is (= false

         ((hydra_lib_equality_lt "zebra") "apple"))))

;; gt strings

(deftest test-equality-neggt-strings-neggreater-lexicographic

  (is (= true

         ((hydra_lib_equality_gt "zebra") "apple"))))

(deftest test-equality-neggt-strings-negequal

  (is (= false

         ((hydra_lib_equality_gt "hello") "hello"))))

(deftest test-equality-neggt-strings-negless

  (is (= false

         ((hydra_lib_equality_gt "apple") "banana"))))

;; max strings

(deftest test-equality-negmax-strings-negfirst-greater

  (is (= "zebra"

         ((hydra_lib_equality_max "zebra") "apple"))))

(deftest test-equality-negmax-strings-negsecond-greater

  (is (= "zebra"

         ((hydra_lib_equality_max "apple") "zebra"))))

(deftest test-equality-negmax-strings-negequal

  (is (= "hello"

         ((hydra_lib_equality_max "hello") "hello"))))

;; min strings

(deftest test-equality-negmin-strings-negfirst-less

  (is (= "apple"

         ((hydra_lib_equality_min "apple") "zebra"))))

(deftest test-equality-negmin-strings-negsecond-less

  (is (= "apple"

         ((hydra_lib_equality_min "zebra") "apple"))))

(deftest test-equality-negmin-strings-negequal

  (is (= "hello"

         ((hydra_lib_equality_min "hello") "hello"))))

;; compare floats

(deftest test-equality-negcompare-floats-negless-than

  (is (= (list :less_than nil)

         ((hydra_lib_equality_compare 1.5) 2.5))))

(deftest test-equality-negcompare-floats-negequal

  (is (= (list :equal_to nil)

         ((hydra_lib_equality_compare 3.14) 3.14))))

(deftest test-equality-negcompare-floats-neggreater-than

  (is (= (list :greater_than nil)

         ((hydra_lib_equality_compare 5.0) 3.0))))

(deftest test-equality-negcompare-floats-negnegative-vs-positive

  (is (= (list :less_than nil)

         ((hydra_lib_equality_compare -1.0) 1.0))))

;; lt floats

(deftest test-equality-neglt-floats-negless

  (is (= true

         ((hydra_lib_equality_lt 1.5) 2.5))))

(deftest test-equality-neglt-floats-negequal

  (is (= false

         ((hydra_lib_equality_lt 3.14) 3.14))))

(deftest test-equality-neglt-floats-neggreater

  (is (= false

         ((hydra_lib_equality_lt 5.0) 3.0))))

;; gt floats

(deftest test-equality-neggt-floats-neggreater

  (is (= true

         ((hydra_lib_equality_gt 5.0) 3.0))))

(deftest test-equality-neggt-floats-negequal

  (is (= false

         ((hydra_lib_equality_gt 3.14) 3.14))))

(deftest test-equality-neggt-floats-negless

  (is (= false

         ((hydra_lib_equality_gt 1.5) 2.5))))
