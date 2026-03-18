;; Note: this is an automatically generated file. Do not edit.
;; hydra.lib.logic primitives

(ns generation.hydra.test.lib.logic-test
  (:require [clojure.test :refer :all]))

;; and

(deftest test-and-negtrue-and-true

  (is (= true

         ((hydra_lib_logic_and true) true))))

(deftest test-and-negtrue-and-false

  (is (= false

         ((hydra_lib_logic_and true) false))))

(deftest test-and-negfalse-and-true

  (is (= false

         ((hydra_lib_logic_and false) true))))

(deftest test-and-negfalse-and-false

  (is (= false

         ((hydra_lib_logic_and false) false))))

;; ifElse

;; boolean values

(deftest test-ifelse-negboolean-values-negtrue-condition-returns-then

  (is (= true

         (((hydra_lib_logic_if_else true) true) false))))

(deftest test-ifelse-negboolean-values-negfalse-condition-returns-else

  (is (= false

         (((hydra_lib_logic_if_else false) true) false))))

;; integer values

(deftest test-ifelse-neginteger-values-negtrue-selects-first-int

  (is (= 42

         (((hydra_lib_logic_if_else true) 42) 0))))

(deftest test-ifelse-neginteger-values-negfalse-selects-second-int

  (is (= 0

         (((hydra_lib_logic_if_else false) 42) 0))))

;; string values

(deftest test-ifelse-negstring-values-negtrue-selects-first-string

  (is (= "yes"

         (((hydra_lib_logic_if_else true) "yes") "no"))))

(deftest test-ifelse-negstring-values-negfalse-selects-second-string

  (is (= "no"

         (((hydra_lib_logic_if_else false) "yes") "no"))))

;; not

(deftest test-not-negnot-true

  (is (= false

         (hydra_lib_logic_not true))))

(deftest test-not-negnot-false

  (is (= true

         (hydra_lib_logic_not false))))

;; or

(deftest test-or-negtrue-or-true

  (is (= true

         ((hydra_lib_logic_or true) true))))

(deftest test-or-negtrue-or-false

  (is (= true

         ((hydra_lib_logic_or true) false))))

(deftest test-or-negfalse-or-true

  (is (= true

         ((hydra_lib_logic_or false) true))))

(deftest test-or-negfalse-or-false

  (is (= false

         ((hydra_lib_logic_or false) false))))
