;;; Note: this is an automatically generated file. Do not edit.
;;; reduction

(require 'ert)

;; beta reduction

(ert-deftest test-beta-reduction-negidentity-function-applied-to-literal ()

  (should (equal 42 ((lambda (x) x) 42))))

(ert-deftest test-beta-reduction-negconstant-function ()

  (should (equal 1 ((lambda (x) 1) 42))))

(ert-deftest test-beta-reduction-negnested-application ()

  (should (equal 1 (((lambda (x) (lambda (y) x)) 1) 2))))

;; monomorphic primitives

(ert-deftest test-monomorphic-primitives-negtoupper-on-lowercase ()

  (should (equal "HELLO" (hydra_lib_strings_to_upper "hello"))))

(ert-deftest test-monomorphic-primitives-negtoupper-on-mixed-case ()

  (should (equal "HELLO WORLD" (hydra_lib_strings_to_upper "Hello World"))))

(ert-deftest test-monomorphic-primitives-negtoupper-on-empty-string ()

  (should (equal "" (hydra_lib_strings_to_upper ""))))

(ert-deftest test-monomorphic-primitives-negtolower-on-uppercase ()

  (should (equal "hello" (hydra_lib_strings_to_lower "HELLO"))))

(ert-deftest test-monomorphic-primitives-negstring-length ()

  (should (equal 5 (hydra_lib_strings_length "hello"))))

(ert-deftest test-monomorphic-primitives-negstring-length-of-empty ()

  (should (equal 0 (hydra_lib_strings_length ""))))

(ert-deftest test-monomorphic-primitives-negadd-two-positive-integers ()

  (should (equal 8 ((hydra_lib_math_add 3) 5))))

(ert-deftest test-monomorphic-primitives-negadd-negative-and-positive ()

  (should (equal -7 ((hydra_lib_math_add -10) 3))))

(ert-deftest test-monomorphic-primitives-negadd-with-zero ()

  (should (equal 42 ((hydra_lib_math_add 0) 42))))

(ert-deftest test-monomorphic-primitives-negsubtract-integers ()

  (should (equal 7 ((hydra_lib_math_sub 10) 3))))

(ert-deftest test-monomorphic-primitives-negmultiply-integers ()

  (should (equal 42 ((hydra_lib_math_mul 6) 7))))

(ert-deftest test-monomorphic-primitives-negmultiply-by-zero ()

  (should (equal 0 ((hydra_lib_math_mul 100) 0))))

(ert-deftest test-monomorphic-primitives-negdivide-integers ()

  (should (equal 5 ((hydra_lib_math_div 20) 4))))

(ert-deftest test-monomorphic-primitives-negmodulo ()

  (should (equal 2 ((hydra_lib_math_mod 17) 5))))

(ert-deftest test-monomorphic-primitives-negspliton-basic ()

  (should (equal (list "a" "b" "c") ((hydra_lib_strings_split_on ",") "a,b,c"))))

(ert-deftest test-monomorphic-primitives-negcat2-strings ()

  (should (equal "helloworld" ((hydra_lib_strings_cat2 "hello") "world"))))

;; polymorphic primitives

(ert-deftest test-polymorphic-primitives-neglength-of-integer-list ()

  (should (equal 3 (hydra_lib_lists_length (list 1 2 3)))))

(ert-deftest test-polymorphic-primitives-neglength-of-string-list ()

  (should (equal 2 (hydra_lib_lists_length (list "a" "b")))))

(ert-deftest test-polymorphic-primitives-neglength-of-empty-list ()

  (should (equal 0 (hydra_lib_lists_length (list )))))

(ert-deftest test-polymorphic-primitives-neglength-of-single-element-list ()

  (should (equal 1 (hydra_lib_lists_length (list t)))))

(ert-deftest test-polymorphic-primitives-neghead-of-integer-list ()

  (should (equal 10 (hydra_lib_lists_head (list 10 20 30)))))

(ert-deftest test-polymorphic-primitives-neghead-of-string-list ()

  (should (equal "first" (hydra_lib_lists_head (list "first" "second")))))

(ert-deftest test-polymorphic-primitives-neglast-of-integer-list ()

  (should (equal 30 (hydra_lib_lists_last (list 10 20 30)))))

(ert-deftest test-polymorphic-primitives-negconcat-two-integer-lists ()

  (should (equal (list 1 2 3 4) ((hydra_lib_lists_concat2 (list 1 2)) (list 3 4)))))

(ert-deftest test-polymorphic-primitives-negconcat-with-empty-list ()

  (should (equal (list 1 2) ((hydra_lib_lists_concat2 (list )) (list 1 2)))))

(ert-deftest test-polymorphic-primitives-negreverse-integer-list ()

  (should (equal (list 3 2 1) (hydra_lib_lists_reverse (list 1 2 3)))))

(ert-deftest test-polymorphic-primitives-negreverse-empty-list ()

  (should (equal (list ) (hydra_lib_lists_reverse (list )))))

;; nullary primitives

(ert-deftest test-nullary-primitives-negempty-set-has-size-zero ()

  (should (equal 0 (hydra_lib_sets_size hydra_lib_sets_empty))))

;; literals as values

(ert-deftest test-literals-as-values-neginteger-literal-is-a-value ()

  (should (equal 42 42)))

(ert-deftest test-literals-as-values-negnegative-integer-literal ()

  (should (equal -17 -17)))

(ert-deftest test-literals-as-values-negzero-integer-literal ()

  (should (equal 0 0)))

(ert-deftest test-literals-as-values-negstring-literal-is-a-value ()

  (should (equal "hello" "hello")))

(ert-deftest test-literals-as-values-negempty-string-literal ()

  (should (equal "" "")))

(ert-deftest test-literals-as-values-negstring-with-special-characters ()

  (should (equal "hello\nworld\ttab" "hello\nworld\ttab")))

(ert-deftest test-literals-as-values-negboolean-true-is-a-value ()

  (should (equal t t)))

(ert-deftest test-literals-as-values-negboolean-false-is-a-value ()

  (should (equal nil nil)))

(ert-deftest test-literals-as-values-negfloat-literal-is-a-value ()

  (should (equal 3.14 3.14)))

(ert-deftest test-literals-as-values-negnegative-float-literal ()

  (should (equal -2.718 -2.718)))

(ert-deftest test-literals-as-values-negzero-float-literal ()

  (should (equal 0.0 0.0)))

;; list reduction

(ert-deftest test-list-reduction-negempty-list-is-a-value ()

  (should (equal (list ) (list ))))

(ert-deftest test-list-reduction-neglist-of-literals-is-a-value ()

  (should (equal (list 1 2 3) (list 1 2 3))))

(ert-deftest test-list-reduction-neglist-with-reducible-element ()

  (should (equal (list 42) (list ((lambda (x) x) 42)))))

;; optional reduction

(ert-deftest test-optional-reduction-negnothing-is-a-value ()

  (should (equal nil nil)))

(ert-deftest test-optional-reduction-negjust-literal-is-a-value ()

  (should (equal 42 42)))

(ert-deftest test-optional-reduction-negjust-with-reducible-content ()

  (should (equal 42 ((lambda (x) x) 42))))
