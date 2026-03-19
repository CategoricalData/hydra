;;; Note: this is an automatically generated file. Do not edit. -*- lexical-binding: t; coding: utf-8 -*-
;;; reduction

(require 'ert)

;; beta reduction

(ert-deftest test-reduction-negbeta-reduction-negidentity-function-applied-to-literal ()

  (should (equal 42 (funcall (lambda (x) x) 42))))

(ert-deftest test-reduction-negbeta-reduction-negconstant-function ()

  (should (equal 1 (funcall (lambda (x) 1) 42))))

(ert-deftest test-reduction-negbeta-reduction-negnested-application ()

  (should (equal 1 (funcall (funcall (lambda (x) (lambda (y) x)) 1) 2))))

;; monomorphic primitives

(ert-deftest test-reduction-negmonomorphic-primitives-negtoupper-on-lowercase ()

  (should (equal "HELLO" (funcall hydra_lib_strings_to_upper "hello"))))

(ert-deftest test-reduction-negmonomorphic-primitives-negtoupper-on-mixed-case ()

  (should (equal "HELLO WORLD" (funcall hydra_lib_strings_to_upper "Hello World"))))

(ert-deftest test-reduction-negmonomorphic-primitives-negtoupper-on-empty-string ()

  (should (equal "" (funcall hydra_lib_strings_to_upper ""))))

(ert-deftest test-reduction-negmonomorphic-primitives-negtolower-on-uppercase ()

  (should (equal "hello" (funcall hydra_lib_strings_to_lower "HELLO"))))

(ert-deftest test-reduction-negmonomorphic-primitives-negstring-length ()

  (should (equal 5 (funcall hydra_lib_strings_length "hello"))))

(ert-deftest test-reduction-negmonomorphic-primitives-negstring-length-of-empty ()

  (should (equal 0 (funcall hydra_lib_strings_length ""))))

(ert-deftest test-reduction-negmonomorphic-primitives-negadd-two-positive-integers ()

  (should (equal 8 (funcall (funcall hydra_lib_math_add 3) 5))))

(ert-deftest test-reduction-negmonomorphic-primitives-negadd-negative-and-positive ()

  (should (equal -7 (funcall (funcall hydra_lib_math_add -10) 3))))

(ert-deftest test-reduction-negmonomorphic-primitives-negadd-with-zero ()

  (should (equal 42 (funcall (funcall hydra_lib_math_add 0) 42))))

(ert-deftest test-reduction-negmonomorphic-primitives-negsubtract-integers ()

  (should (equal 7 (funcall (funcall hydra_lib_math_sub 10) 3))))

(ert-deftest test-reduction-negmonomorphic-primitives-negmultiply-integers ()

  (should (equal 42 (funcall (funcall hydra_lib_math_mul 6) 7))))

(ert-deftest test-reduction-negmonomorphic-primitives-negmultiply-by-zero ()

  (should (equal 0 (funcall (funcall hydra_lib_math_mul 100) 0))))

(ert-deftest test-reduction-negmonomorphic-primitives-negdivide-integers ()

  (should (equal 5 (funcall (funcall hydra_lib_math_div 20) 4))))

(ert-deftest test-reduction-negmonomorphic-primitives-negmodulo ()

  (should (equal 2 (funcall (funcall hydra_lib_math_mod 17) 5))))

(ert-deftest test-reduction-negmonomorphic-primitives-negspliton-basic ()

  (should (equal (list "a" "b" "c") (funcall (funcall hydra_lib_strings_split_on ",") "a,b,c"))))

(ert-deftest test-reduction-negmonomorphic-primitives-negcat2-strings ()

  (should (equal "helloworld" (funcall (funcall hydra_lib_strings_cat2 "hello") "world"))))

;; polymorphic primitives

(ert-deftest test-reduction-negpolymorphic-primitives-neglength-of-integer-list ()

  (should (equal 3 (funcall hydra_lib_lists_length (list 1 2 3)))))

(ert-deftest test-reduction-negpolymorphic-primitives-neglength-of-string-list ()

  (should (equal 2 (funcall hydra_lib_lists_length (list "a" "b")))))

(ert-deftest test-reduction-negpolymorphic-primitives-neglength-of-empty-list ()

  (should (equal 0 (funcall hydra_lib_lists_length (list )))))

(ert-deftest test-reduction-negpolymorphic-primitives-neglength-of-single-element-list ()

  (should (equal 1 (funcall hydra_lib_lists_length (list t)))))

(ert-deftest test-reduction-negpolymorphic-primitives-neghead-of-integer-list ()

  (should (equal 10 (funcall hydra_lib_lists_head (list 10 20 30)))))

(ert-deftest test-reduction-negpolymorphic-primitives-neghead-of-string-list ()

  (should (equal "first" (funcall hydra_lib_lists_head (list "first" "second")))))

(ert-deftest test-reduction-negpolymorphic-primitives-neglast-of-integer-list ()

  (should (equal 30 (funcall hydra_lib_lists_last (list 10 20 30)))))

(ert-deftest test-reduction-negpolymorphic-primitives-negconcat-two-integer-lists ()

  (should (equal (list 1 2 3 4) (funcall (funcall hydra_lib_lists_concat2 (list 1 2)) (list 3 4)))))

(ert-deftest test-reduction-negpolymorphic-primitives-negconcat-with-empty-list ()

  (should (equal (list 1 2) (funcall (funcall hydra_lib_lists_concat2 (list )) (list 1 2)))))

(ert-deftest test-reduction-negpolymorphic-primitives-negreverse-integer-list ()

  (should (equal (list 3 2 1) (funcall hydra_lib_lists_reverse (list 1 2 3)))))

(ert-deftest test-reduction-negpolymorphic-primitives-negreverse-empty-list ()

  (should (equal (list ) (funcall hydra_lib_lists_reverse (list )))))

;; nullary primitives

(ert-deftest test-reduction-negnullary-primitives-negempty-set-has-size-zero ()

  (should (equal 0 (funcall hydra_lib_sets_size hydra_lib_sets_empty))))

;; literals as values

(ert-deftest test-reduction-negliterals-as-values-neginteger-literal-is-a-value ()

  (should (equal 42 42)))

(ert-deftest test-reduction-negliterals-as-values-negnegative-integer-literal ()

  (should (equal -17 -17)))

(ert-deftest test-reduction-negliterals-as-values-negzero-integer-literal ()

  (should (equal 0 0)))

(ert-deftest test-reduction-negliterals-as-values-negstring-literal-is-a-value ()

  (should (equal "hello" "hello")))

(ert-deftest test-reduction-negliterals-as-values-negempty-string-literal ()

  (should (equal "" "")))

(ert-deftest test-reduction-negliterals-as-values-negstring-with-special-characters ()

  (should (equal "hello\nworld\ttab" "hello\nworld\ttab")))

(ert-deftest test-reduction-negliterals-as-values-negboolean-true-is-a-value ()

  (should (equal t t)))

(ert-deftest test-reduction-negliterals-as-values-negboolean-false-is-a-value ()

  (should (equal nil nil)))

(ert-deftest test-reduction-negliterals-as-values-negfloat-literal-is-a-value ()

  (should (equal 3.14 3.14)))

(ert-deftest test-reduction-negliterals-as-values-negnegative-float-literal ()

  (should (equal -2.718 -2.718)))

(ert-deftest test-reduction-negliterals-as-values-negzero-float-literal ()

  (should (equal 0.0 0.0)))

;; list reduction

(ert-deftest test-reduction-neglist-reduction-negempty-list-is-a-value ()

  (should (equal (list ) (list ))))

(ert-deftest test-reduction-neglist-reduction-neglist-of-literals-is-a-value ()

  (should (equal (list 1 2 3) (list 1 2 3))))

(ert-deftest test-reduction-neglist-reduction-neglist-with-reducible-element ()

  (should (equal (list 42) (list (funcall (lambda (x) x) 42)))))

;; optional reduction

(ert-deftest test-reduction-negoptional-reduction-negnothing-is-a-value ()

  (should (equal (list :nothing) (list :nothing))))

(ert-deftest test-reduction-negoptional-reduction-negjust-literal-is-a-value ()

  (should (equal (list :just 42) (list :just 42))))

(ert-deftest test-reduction-negoptional-reduction-negjust-with-reducible-content ()

  (should (equal (list :just 42) (list :just (funcall (lambda (x) x) 42)))))
