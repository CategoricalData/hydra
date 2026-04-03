;; Note: this is an automatically generated file. Do not edit.
;; hydra.lib.chars primitives

(ns test-ns
  (:require [clojure.test :refer :all]))

;; isAlphaNum

(deftest test-chars-negisalphanum-negletter

  (is (= true

         true)))

(deftest test-chars-negisalphanum-negdigit

  (is (= true

         true)))

(deftest test-chars-negisalphanum-negspace

  (is (= false

         false)))

(deftest test-chars-negisalphanum-negpunctuation

  (is (= false

         false)))

;; isLower

(deftest test-chars-negislower-neglowercase

  (is (= true

         true)))

(deftest test-chars-negislower-neguppercase

  (is (= false

         false)))

(deftest test-chars-negislower-negdigit

  (is (= false

         false)))

;; isSpace

(deftest test-chars-negisspace-negspace

  (is (= true

         true)))

(deftest test-chars-negisspace-negtab

  (is (= true

         true)))

(deftest test-chars-negisspace-negnewline

  (is (= true

         true)))

(deftest test-chars-negisspace-negletter

  (is (= false

         false)))

;; isUpper

(deftest test-chars-negisupper-neguppercase

  (is (= true

         true)))

(deftest test-chars-negisupper-neglowercase

  (is (= false

         false)))

(deftest test-chars-negisupper-negdigit

  (is (= false

         false)))

;; toLower

(deftest test-chars-negtolower-neguppercase

  (is (= 97:int32

         97:int32)))

(deftest test-chars-negtolower-neglowercase

  (is (= 97:int32

         97:int32)))

(deftest test-chars-negtolower-negdigit

  (is (= 53:int32

         53:int32)))

;; toUpper

(deftest test-chars-negtoupper-neglowercase

  (is (= 65:int32

         65:int32)))

(deftest test-chars-negtoupper-neguppercase

  (is (= 65:int32

         65:int32)))

(deftest test-chars-negtoupper-negdigit

  (is (= 53:int32

         53:int32)))
