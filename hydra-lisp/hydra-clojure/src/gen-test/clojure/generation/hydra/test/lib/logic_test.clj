;; Note: this is an automatically generated file. Do not edit.
;; hydra.lib.logic primitives

(ns test-ns
  (:require [clojure.test :refer :all]))

;; and

(deftest test-logic-negand-negtrue-and-true

  (is (= true

         true)))

(deftest test-logic-negand-negtrue-and-false

  (is (= false

         false)))

(deftest test-logic-negand-negfalse-and-true

  (is (= false

         false)))

(deftest test-logic-negand-negfalse-and-false

  (is (= false

         false)))

;; ifElse

;; boolean values

(deftest test-logic-negifelse-negboolean-values-negtrue-condition-returns-then

  (is (= true

         true)))

(deftest test-logic-negifelse-negboolean-values-negfalse-condition-returns-else

  (is (= false

         false)))

;; integer values

(deftest test-logic-negifelse-neginteger-values-negtrue-selects-first-int

  (is (= 42:int32

         42:int32)))

(deftest test-logic-negifelse-neginteger-values-negfalse-selects-second-int

  (is (= 0:int32

         0:int32)))

;; string values

(deftest test-logic-negifelse-negstring-values-negtrue-selects-first-string

  (is (= "yes"

         "yes")))

(deftest test-logic-negifelse-negstring-values-negfalse-selects-second-string

  (is (= "no"

         "no")))

;; not

(deftest test-logic-negnot-negnot-true

  (is (= false

         false)))

(deftest test-logic-negnot-negnot-false

  (is (= true

         true)))

;; or

(deftest test-logic-negor-negtrue-or-true

  (is (= true

         true)))

(deftest test-logic-negor-negtrue-or-false

  (is (= true

         true)))

(deftest test-logic-negor-negfalse-or-true

  (is (= true

         true)))

(deftest test-logic-negor-negfalse-or-false

  (is (= false

         false)))
