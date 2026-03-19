;; Note: this is an automatically generated file. Do not edit.
;; hydra.lib.logic primitives

;; and

(defun test-logic-negand-negtrue-and-true ()

  (assert (equal cl:t ((hydra_lib_logic_and cl:t) cl:t))))

(defun test-logic-negand-negtrue-and-false ()

  (assert (equal cl:nil ((hydra_lib_logic_and cl:t) cl:nil))))

(defun test-logic-negand-negfalse-and-true ()

  (assert (equal cl:nil ((hydra_lib_logic_and cl:nil) cl:t))))

(defun test-logic-negand-negfalse-and-false ()

  (assert (equal cl:nil ((hydra_lib_logic_and cl:nil) cl:nil))))

;; ifElse

;; boolean values

(defun test-logic-negifelse-negboolean-values-negtrue-condition-returns-then ()

  (assert (equal cl:t (((hydra_lib_logic_if_else cl:t) cl:t) cl:nil))))

(defun test-logic-negifelse-negboolean-values-negfalse-condition-returns-else ()

  (assert (equal cl:nil (((hydra_lib_logic_if_else cl:nil) cl:t) cl:nil))))

;; integer values

(defun test-logic-negifelse-neginteger-values-negtrue-selects-first-int ()

  (assert (equal 42 (((hydra_lib_logic_if_else cl:t) 42) 0))))

(defun test-logic-negifelse-neginteger-values-negfalse-selects-second-int ()

  (assert (equal 0 (((hydra_lib_logic_if_else cl:nil) 42) 0))))

;; string values

(defun test-logic-negifelse-negstring-values-negtrue-selects-first-string ()

  (assert (equal "yes" (((hydra_lib_logic_if_else cl:t) "yes") "no"))))

(defun test-logic-negifelse-negstring-values-negfalse-selects-second-string ()

  (assert (equal "no" (((hydra_lib_logic_if_else cl:nil) "yes") "no"))))

;; not

(defun test-logic-negnot-negnot-true ()

  (assert (equal cl:nil (hydra_lib_logic_not cl:t))))

(defun test-logic-negnot-negnot-false ()

  (assert (equal cl:t (hydra_lib_logic_not cl:nil))))

;; or

(defun test-logic-negor-negtrue-or-true ()

  (assert (equal cl:t ((hydra_lib_logic_or cl:t) cl:t))))

(defun test-logic-negor-negtrue-or-false ()

  (assert (equal cl:t ((hydra_lib_logic_or cl:t) cl:nil))))

(defun test-logic-negor-negfalse-or-true ()

  (assert (equal cl:t ((hydra_lib_logic_or cl:nil) cl:t))))

(defun test-logic-negor-negfalse-or-false ()

  (assert (equal cl:nil ((hydra_lib_logic_or cl:nil) cl:nil))))
