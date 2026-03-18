;; Note: this is an automatically generated file. Do not edit.
;; hydra.lib.logic primitives

;; and

(defun test-and-negtrue-and-true ()

  (assert (equal cl:t ((hydra_lib_logic_and cl:t) cl:t))))

(defun test-and-negtrue-and-false ()

  (assert (equal cl:nil ((hydra_lib_logic_and cl:t) cl:nil))))

(defun test-and-negfalse-and-true ()

  (assert (equal cl:nil ((hydra_lib_logic_and cl:nil) cl:t))))

(defun test-and-negfalse-and-false ()

  (assert (equal cl:nil ((hydra_lib_logic_and cl:nil) cl:nil))))

;; ifElse

;; boolean values

(defun test-ifelse-negboolean-values-negtrue-condition-returns-then ()

  (assert (equal cl:t (((hydra_lib_logic_if_else cl:t) cl:t) cl:nil))))

(defun test-ifelse-negboolean-values-negfalse-condition-returns-else ()

  (assert (equal cl:nil (((hydra_lib_logic_if_else cl:nil) cl:t) cl:nil))))

;; integer values

(defun test-ifelse-neginteger-values-negtrue-selects-first-int ()

  (assert (equal 42 (((hydra_lib_logic_if_else cl:t) 42) 0))))

(defun test-ifelse-neginteger-values-negfalse-selects-second-int ()

  (assert (equal 0 (((hydra_lib_logic_if_else cl:nil) 42) 0))))

;; string values

(defun test-ifelse-negstring-values-negtrue-selects-first-string ()

  (assert (equal "yes" (((hydra_lib_logic_if_else cl:t) "yes") "no"))))

(defun test-ifelse-negstring-values-negfalse-selects-second-string ()

  (assert (equal "no" (((hydra_lib_logic_if_else cl:nil) "yes") "no"))))

;; not

(defun test-not-negnot-true ()

  (assert (equal cl:nil (hydra_lib_logic_not cl:t))))

(defun test-not-negnot-false ()

  (assert (equal cl:t (hydra_lib_logic_not cl:nil))))

;; or

(defun test-or-negtrue-or-true ()

  (assert (equal cl:t ((hydra_lib_logic_or cl:t) cl:t))))

(defun test-or-negtrue-or-false ()

  (assert (equal cl:t ((hydra_lib_logic_or cl:t) cl:nil))))

(defun test-or-negfalse-or-true ()

  (assert (equal cl:t ((hydra_lib_logic_or cl:nil) cl:t))))

(defun test-or-negfalse-or-false ()

  (assert (equal cl:nil ((hydra_lib_logic_or cl:nil) cl:nil))))
