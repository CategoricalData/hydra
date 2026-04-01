;; Note: this is an automatically generated file. Do not edit.
;; hydra.lib.pairs primitives

(ns test-ns
  (:require [clojure.test :refer :all]))

;; bimap

(deftest test-pairs-negbimap-negtransform-both-elements

  (is (= (10, 2)

         (10, 2))))

(deftest test-pairs-negbimap-negwith-zero

  (is (= (0, 5)

         (0, 5))))

;; first

(deftest test-pairs-negfirst-negextract-first-element

  (is (= 42

         42)))

(deftest test-pairs-negfirst-negwith-zero

  (is (= 0

         0)))

(deftest test-pairs-negfirst-negnegative-number

  (is (= -5

         -5)))

;; second

(deftest test-pairs-negsecond-negextract-second-element

  (is (= hello

         hello)))

(deftest test-pairs-negsecond-negempty-string

  (is (= 

         )))

(deftest test-pairs-negsecond-neglong-string

  (is (= testing

         testing)))
