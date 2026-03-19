;; Note: this is an automatically generated file. Do not edit.
;; hydra.lib.pairs primitives

(ns generation.hydra.test.lib.pairs-test
  (:require [clojure.test :refer :all]))

;; bimap

(deftest test-pairs-negbimap-negtransform-both-elements

  (is (= (list 10 2)

         (((hydra_lib_pairs_bimap (fn [x] ((hydra_lib_math_mul x) 2))) (fn [s] (hydra_lib_strings_length s))) (list 5 "ab")))))

(deftest test-pairs-negbimap-negwith-zero

  (is (= (list 0 5)

         (((hydra_lib_pairs_bimap (fn [x] ((hydra_lib_math_mul x) 2))) (fn [s] (hydra_lib_strings_length s))) (list 0 "hello")))))

;; first

(deftest test-pairs-negfirst-negextract-first-element

  (is (= 42

         (hydra_lib_pairs_first (list 42 "hello")))))

(deftest test-pairs-negfirst-negwith-zero

  (is (= 0

         (hydra_lib_pairs_first (list 0 "world")))))

(deftest test-pairs-negfirst-negnegative-number

  (is (= -5

         (hydra_lib_pairs_first (list -5 "test")))))

;; second

(deftest test-pairs-negsecond-negextract-second-element

  (is (= "hello"

         (hydra_lib_pairs_second (list 42 "hello")))))

(deftest test-pairs-negsecond-negempty-string

  (is (= ""

         (hydra_lib_pairs_second (list 0 "")))))

(deftest test-pairs-negsecond-neglong-string

  (is (= "testing"

         (hydra_lib_pairs_second (list 123 "testing")))))
