;; Note: this is an automatically generated file. Do not edit.
;; hydra.lib.logic primitives

(ns generation.hydra.test.lib.logic-test
  (:require [clojure.test :refer :all]))

;; and

(deftest test-logic-negand-negtrue-and-true

  (is (= true

         ((hydra_lib_logic_and true) true))))

(deftest test-logic-negand-negtrue-and-false

  (is (= false

         ((hydra_lib_logic_and true) false))))

(deftest test-logic-negand-negfalse-and-true

  (is (= false

         ((hydra_lib_logic_and false) true))))

(deftest test-logic-negand-negfalse-and-false

  (is (= false

         ((hydra_lib_logic_and false) false))))

;; ifElse

;; boolean values

(deftest test-logic-negifelse-negboolean-values-negtrue-condition-returns-then

  (is (= true

         (((hydra_lib_logic_if_else true) true) false))))

(deftest test-logic-negifelse-negboolean-values-negfalse-condition-returns-else

  (is (= false

         (((hydra_lib_logic_if_else false) true) false))))

;; integer values

(deftest test-logic-negifelse-neginteger-values-negtrue-selects-first-int

  (is (= 42

         (((hydra_lib_logic_if_else true) 42) 0))))

(deftest test-logic-negifelse-neginteger-values-negfalse-selects-second-int

  (is (= 0

         (((hydra_lib_logic_if_else false) 42) 0))))

;; string values

(deftest test-logic-negifelse-negstring-values-negtrue-selects-first-string

  (is (= "yes"

         (((hydra_lib_logic_if_else true) "yes") "no"))))

(deftest test-logic-negifelse-negstring-values-negfalse-selects-second-string

  (is (= "no"

         (((hydra_lib_logic_if_else false) "yes") "no"))))

;; not

(deftest test-logic-negnot-negnot-true

  (is (= false

         (hydra_lib_logic_not true))))

(deftest test-logic-negnot-negnot-false

  (is (= true

         (hydra_lib_logic_not false))))

;; or

(deftest test-logic-negor-negtrue-or-true

  (is (= true

         ((hydra_lib_logic_or true) true))))

(deftest test-logic-negor-negtrue-or-false

  (is (= true

         ((hydra_lib_logic_or true) false))))

(deftest test-logic-negor-negfalse-or-true

  (is (= true

         ((hydra_lib_logic_or false) true))))

(deftest test-logic-negor-negfalse-or-false

  (is (= false

         ((hydra_lib_logic_or false) false))))
