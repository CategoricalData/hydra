;; Note: this is an automatically generated file. Do not edit.
;; hydra.lib.pairs primitives

(ns generation.hydra.test.lib.pairs-test
  (:require [clojure.test :refer :all]))

;; bimap

(deftest test-bimap-negtransform-both-elements

  (is (= (list 10 2)

         (((hydra_lib_pairs_bimap (fn [x] ((hydra_lib_math_mul x) 2))) (fn [s] (hydra_lib_strings_length s))) (list 5 "ab")))))

(deftest test-bimap-negwith-zero

  (is (= (list 0 5)

         (((hydra_lib_pairs_bimap (fn [x] ((hydra_lib_math_mul x) 2))) (fn [s] (hydra_lib_strings_length s))) (list 0 "hello")))))

;; first

(deftest test-first-negextract-first-element

  (is (= 42

         (hydra_lib_pairs_first (list 42 "hello")))))

(deftest test-first-negwith-zero

  (is (= 0

         (hydra_lib_pairs_first (list 0 "world")))))

(deftest test-first-negnegative-number

  (is (= -5

         (hydra_lib_pairs_first (list -5 "test")))))

;; second

(deftest test-second-negextract-second-element

  (is (= "hello"

         (hydra_lib_pairs_second (list 42 "hello")))))

(deftest test-second-negempty-string

  (is (= ""

         (hydra_lib_pairs_second (list 0 "")))))

(deftest test-second-neglong-string

  (is (= "testing"

         (hydra_lib_pairs_second (list 123 "testing")))))
