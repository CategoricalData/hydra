;; Note: this is an automatically generated file. Do not edit.
;; hydra.lib.logic primitives

;; and

(defun test-logic-negand-negtrue-and-true ()

  (assert (equal true true)))

(defun test-logic-negand-negtrue-and-false ()

  (assert (equal false false)))

(defun test-logic-negand-negfalse-and-true ()

  (assert (equal false false)))

(defun test-logic-negand-negfalse-and-false ()

  (assert (equal false false)))

;; ifElse

;; boolean values

(defun test-logic-negifelse-negboolean-values-negtrue-condition-returns-then ()

  (assert (equal true true)))

(defun test-logic-negifelse-negboolean-values-negfalse-condition-returns-else ()

  (assert (equal false false)))

;; integer values

(defun test-logic-negifelse-neginteger-values-negtrue-selects-first-int ()

  (assert (equal 42:int32 42:int32)))

(defun test-logic-negifelse-neginteger-values-negfalse-selects-second-int ()

  (assert (equal 0:int32 0:int32)))

;; string values

(defun test-logic-negifelse-negstring-values-negtrue-selects-first-string ()

  (assert (equal "yes" "yes")))

(defun test-logic-negifelse-negstring-values-negfalse-selects-second-string ()

  (assert (equal "no" "no")))

;; not

(defun test-logic-negnot-negnot-true ()

  (assert (equal false false)))

(defun test-logic-negnot-negnot-false ()

  (assert (equal true true)))

;; or

(defun test-logic-negor-negtrue-or-true ()

  (assert (equal true true)))

(defun test-logic-negor-negtrue-or-false ()

  (assert (equal true true)))

(defun test-logic-negor-negfalse-or-true ()

  (assert (equal true true)))

(defun test-logic-negor-negfalse-or-false ()

  (assert (equal false false)))
