;;; Note: this is an automatically generated file. Do not edit.
;;; hydra.lib.logic primitives

(require 'ert)

;; and

(ert-deftest test-and-negtrue-and-true ()

  (should (equal t ((hydra_lib_logic_and t) t))))

(ert-deftest test-and-negtrue-and-false ()

  (should (equal nil ((hydra_lib_logic_and t) nil))))

(ert-deftest test-and-negfalse-and-true ()

  (should (equal nil ((hydra_lib_logic_and nil) t))))

(ert-deftest test-and-negfalse-and-false ()

  (should (equal nil ((hydra_lib_logic_and nil) nil))))

;; ifElse

;; boolean values

(ert-deftest test-ifelse-negboolean-values-negtrue-condition-returns-then ()

  (should (equal t (((hydra_lib_logic_if_else t) t) nil))))

(ert-deftest test-ifelse-negboolean-values-negfalse-condition-returns-else ()

  (should (equal nil (((hydra_lib_logic_if_else nil) t) nil))))

;; integer values

(ert-deftest test-ifelse-neginteger-values-negtrue-selects-first-int ()

  (should (equal 42 (((hydra_lib_logic_if_else t) 42) 0))))

(ert-deftest test-ifelse-neginteger-values-negfalse-selects-second-int ()

  (should (equal 0 (((hydra_lib_logic_if_else nil) 42) 0))))

;; string values

(ert-deftest test-ifelse-negstring-values-negtrue-selects-first-string ()

  (should (equal "yes" (((hydra_lib_logic_if_else t) "yes") "no"))))

(ert-deftest test-ifelse-negstring-values-negfalse-selects-second-string ()

  (should (equal "no" (((hydra_lib_logic_if_else nil) "yes") "no"))))

;; not

(ert-deftest test-not-negnot-true ()

  (should (equal nil (hydra_lib_logic_not t))))

(ert-deftest test-not-negnot-false ()

  (should (equal t (hydra_lib_logic_not nil))))

;; or

(ert-deftest test-or-negtrue-or-true ()

  (should (equal t ((hydra_lib_logic_or t) t))))

(ert-deftest test-or-negtrue-or-false ()

  (should (equal t ((hydra_lib_logic_or t) nil))))

(ert-deftest test-or-negfalse-or-true ()

  (should (equal t ((hydra_lib_logic_or nil) t))))

(ert-deftest test-or-negfalse-or-false ()

  (should (equal nil ((hydra_lib_logic_or nil) nil))))
