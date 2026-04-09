;;; Note: this is an automatically generated file. Do not edit. -*- lexical-binding: t; coding: utf-8 -*-
;;; hydra.lib.logic primitives

(require 'ert)

;; and

(ert-deftest test-logic-negand-negtrue-and-true ()

  (should (equal true true)))

(ert-deftest test-logic-negand-negtrue-and-false ()

  (should (equal false false)))

(ert-deftest test-logic-negand-negfalse-and-true ()

  (should (equal false false)))

(ert-deftest test-logic-negand-negfalse-and-false ()

  (should (equal false false)))

;; ifElse

;; boolean values

(ert-deftest test-logic-negifelse-negboolean-values-negtrue-condition-returns-then ()

  (should (equal true true)))

(ert-deftest test-logic-negifelse-negboolean-values-negfalse-condition-returns-else ()

  (should (equal false false)))

;; integer values

(ert-deftest test-logic-negifelse-neginteger-values-negtrue-selects-first-int ()

  (should (equal 42:int32 42:int32)))

(ert-deftest test-logic-negifelse-neginteger-values-negfalse-selects-second-int ()

  (should (equal 0:int32 0:int32)))

;; string values

(ert-deftest test-logic-negifelse-negstring-values-negtrue-selects-first-string ()

  (should (equal "yes" "yes")))

(ert-deftest test-logic-negifelse-negstring-values-negfalse-selects-second-string ()

  (should (equal "no" "no")))

;; not

(ert-deftest test-logic-negnot-negnot-true ()

  (should (equal false false)))

(ert-deftest test-logic-negnot-negnot-false ()

  (should (equal true true)))

;; or

(ert-deftest test-logic-negor-negtrue-or-true ()

  (should (equal true true)))

(ert-deftest test-logic-negor-negtrue-or-false ()

  (should (equal true true)))

(ert-deftest test-logic-negor-negfalse-or-true ()

  (should (equal true true)))

(ert-deftest test-logic-negor-negfalse-or-false ()

  (should (equal false false)))
