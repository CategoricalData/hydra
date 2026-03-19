;;; Note: this is an automatically generated file. Do not edit. -*- lexical-binding: t; coding: utf-8 -*-
;;; hydra.lib.logic primitives

(require 'ert)

;; and

(ert-deftest test-logic-negand-negtrue-and-true ()

  (should (equal t (funcall (funcall hydra_lib_logic_and t) t))))

(ert-deftest test-logic-negand-negtrue-and-false ()

  (should (equal nil (funcall (funcall hydra_lib_logic_and t) nil))))

(ert-deftest test-logic-negand-negfalse-and-true ()

  (should (equal nil (funcall (funcall hydra_lib_logic_and nil) t))))

(ert-deftest test-logic-negand-negfalse-and-false ()

  (should (equal nil (funcall (funcall hydra_lib_logic_and nil) nil))))

;; ifElse

;; boolean values

(ert-deftest test-logic-negifelse-negboolean-values-negtrue-condition-returns-then ()

  (should (equal t (funcall (funcall (funcall hydra_lib_logic_if_else t) t) nil))))

(ert-deftest test-logic-negifelse-negboolean-values-negfalse-condition-returns-else ()

  (should (equal nil (funcall (funcall (funcall hydra_lib_logic_if_else nil) t) nil))))

;; integer values

(ert-deftest test-logic-negifelse-neginteger-values-negtrue-selects-first-int ()

  (should (equal 42 (funcall (funcall (funcall hydra_lib_logic_if_else t) 42) 0))))

(ert-deftest test-logic-negifelse-neginteger-values-negfalse-selects-second-int ()

  (should (equal 0 (funcall (funcall (funcall hydra_lib_logic_if_else nil) 42) 0))))

;; string values

(ert-deftest test-logic-negifelse-negstring-values-negtrue-selects-first-string ()

  (should (equal "yes" (funcall (funcall (funcall hydra_lib_logic_if_else t) "yes") "no"))))

(ert-deftest test-logic-negifelse-negstring-values-negfalse-selects-second-string ()

  (should (equal "no" (funcall (funcall (funcall hydra_lib_logic_if_else nil) "yes") "no"))))

;; not

(ert-deftest test-logic-negnot-negnot-true ()

  (should (equal nil (funcall hydra_lib_logic_not t))))

(ert-deftest test-logic-negnot-negnot-false ()

  (should (equal t (funcall hydra_lib_logic_not nil))))

;; or

(ert-deftest test-logic-negor-negtrue-or-true ()

  (should (equal t (funcall (funcall hydra_lib_logic_or t) t))))

(ert-deftest test-logic-negor-negtrue-or-false ()

  (should (equal t (funcall (funcall hydra_lib_logic_or t) nil))))

(ert-deftest test-logic-negor-negfalse-or-true ()

  (should (equal t (funcall (funcall hydra_lib_logic_or nil) t))))

(ert-deftest test-logic-negor-negfalse-or-false ()

  (should (equal nil (funcall (funcall hydra_lib_logic_or nil) nil))))
