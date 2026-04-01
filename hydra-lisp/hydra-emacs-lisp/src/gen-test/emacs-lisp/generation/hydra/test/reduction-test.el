;;; Note: this is an automatically generated file. Do not edit. -*- lexical-binding: t; coding: utf-8 -*-
;;; reduction

(require 'ert)

;; beta reduction

(ert-deftest test-reduction-negbeta-reduction-negidentity-function-applied-to-literal ()

  (should (equal 42:int32 42:int32)))

(ert-deftest test-reduction-negbeta-reduction-negconstant-function ()

  (should (equal 1:int32 1:int32)))

(ert-deftest test-reduction-negbeta-reduction-negnested-application ()

  (should (equal 1:int32 1:int32)))

;; monomorphic primitives

(ert-deftest test-reduction-negmonomorphic-primitives-negtoupper-on-lowercase ()

  (should (equal "HELLO" "HELLO")))

(ert-deftest test-reduction-negmonomorphic-primitives-negtoupper-on-mixed-case ()

  (should (equal "HELLO WORLD" "HELLO WORLD")))

(ert-deftest test-reduction-negmonomorphic-primitives-negtoupper-on-empty-string ()

  (should (equal "" "")))

(ert-deftest test-reduction-negmonomorphic-primitives-negtolower-on-uppercase ()

  (should (equal "hello" "hello")))

(ert-deftest test-reduction-negmonomorphic-primitives-negstring-length ()

  (should (equal 5:int32 5:int32)))

(ert-deftest test-reduction-negmonomorphic-primitives-negstring-length-of-empty ()

  (should (equal 0:int32 0:int32)))

(ert-deftest test-reduction-negmonomorphic-primitives-negadd-two-positive-integers ()

  (should (equal 8:int32 8:int32)))

(ert-deftest test-reduction-negmonomorphic-primitives-negadd-negative-and-positive ()

  (should (equal -7:int32 -7:int32)))

(ert-deftest test-reduction-negmonomorphic-primitives-negadd-with-zero ()

  (should (equal 42:int32 42:int32)))

(ert-deftest test-reduction-negmonomorphic-primitives-negsubtract-integers ()

  (should (equal 7:int32 7:int32)))

(ert-deftest test-reduction-negmonomorphic-primitives-negmultiply-integers ()

  (should (equal 42:int32 42:int32)))

(ert-deftest test-reduction-negmonomorphic-primitives-negmultiply-by-zero ()

  (should (equal 0:int32 0:int32)))

(ert-deftest test-reduction-negmonomorphic-primitives-negdivide-integers ()

  (should (equal 5:int32 5:int32)))

(ert-deftest test-reduction-negmonomorphic-primitives-negmodulo ()

  (should (equal 2:int32 2:int32)))

(ert-deftest test-reduction-negmonomorphic-primitives-negspliton-basic ()

  (should (equal ["a", "b", "c"] ["a", "b", "c"])))

(ert-deftest test-reduction-negmonomorphic-primitives-negcat2-strings ()

  (should (equal "helloworld" "helloworld")))

;; polymorphic primitives

(ert-deftest test-reduction-negpolymorphic-primitives-neglength-of-integer-list ()

  (should (equal 3:int32 3:int32)))

(ert-deftest test-reduction-negpolymorphic-primitives-neglength-of-string-list ()

  (should (equal 2:int32 2:int32)))

(ert-deftest test-reduction-negpolymorphic-primitives-neglength-of-empty-list ()

  (should (equal 0:int32 0:int32)))

(ert-deftest test-reduction-negpolymorphic-primitives-neglength-of-single-element-list ()

  (should (equal 1:int32 1:int32)))

(ert-deftest test-reduction-negpolymorphic-primitives-neghead-of-integer-list ()

  (should (equal 10:int32 10:int32)))

(ert-deftest test-reduction-negpolymorphic-primitives-neghead-of-string-list ()

  (should (equal "first" "first")))

(ert-deftest test-reduction-negpolymorphic-primitives-neglast-of-integer-list ()

  (should (equal 30:int32 30:int32)))

(ert-deftest test-reduction-negpolymorphic-primitives-negconcat-two-integer-lists ()

  (should (equal [1:int32, 2:int32, 3:int32, 4:int32] [1:int32, 2:int32, 3:int32, 4:int32])))

(ert-deftest test-reduction-negpolymorphic-primitives-negconcat-with-empty-list ()

  (should (equal [1:int32, 2:int32] [1:int32, 2:int32])))

(ert-deftest test-reduction-negpolymorphic-primitives-negreverse-integer-list ()

  (should (equal [3:int32, 2:int32, 1:int32] [3:int32, 2:int32, 1:int32])))

(ert-deftest test-reduction-negpolymorphic-primitives-negreverse-empty-list ()

  (should (equal [] [])))

;; nullary primitives

(ert-deftest test-reduction-negnullary-primitives-negempty-set-has-size-zero ()

  (should (equal 0:int32 0:int32)))

;; literals as values

(ert-deftest test-reduction-negliterals-as-values-neginteger-literal-is-a-value ()

  (should (equal 42:int32 42:int32)))

(ert-deftest test-reduction-negliterals-as-values-negnegative-integer-literal ()

  (should (equal -17:int32 -17:int32)))

(ert-deftest test-reduction-negliterals-as-values-negzero-integer-literal ()

  (should (equal 0:int32 0:int32)))

(ert-deftest test-reduction-negliterals-as-values-negstring-literal-is-a-value ()

  (should (equal "hello" "hello")))

(ert-deftest test-reduction-negliterals-as-values-negempty-string-literal ()

  (should (equal "" "")))

(ert-deftest test-reduction-negliterals-as-values-negstring-with-special-characters ()

  (should (equal "hello\nworld\ttab" "hello\nworld\ttab")))

(ert-deftest test-reduction-negliterals-as-values-negboolean-true-is-a-value ()

  (should (equal true true)))

(ert-deftest test-reduction-negliterals-as-values-negboolean-false-is-a-value ()

  (should (equal false false)))

(ert-deftest test-reduction-negliterals-as-values-negfloat-literal-is-a-value ()

  (should (equal 3.14:float64 3.14:float64)))

(ert-deftest test-reduction-negliterals-as-values-negnegative-float-literal ()

  (should (equal -2.718:float64 -2.718:float64)))

(ert-deftest test-reduction-negliterals-as-values-negzero-float-literal ()

  (should (equal 0.0:float64 0.0:float64)))

;; list reduction

(ert-deftest test-reduction-neglist-reduction-negempty-list-is-a-value ()

  (should (equal [] [])))

(ert-deftest test-reduction-neglist-reduction-neglist-of-literals-is-a-value ()

  (should (equal [1:int32, 2:int32, 3:int32] [1:int32, 2:int32, 3:int32])))

(ert-deftest test-reduction-neglist-reduction-neglist-with-reducible-element ()

  (should (equal [42:int32] [42:int32])))

;; optional reduction

(ert-deftest test-reduction-negoptional-reduction-negnothing-is-a-value ()

  (should (equal nothing nothing)))

(ert-deftest test-reduction-negoptional-reduction-negjust-literal-is-a-value ()

  (should (equal just(42:int32) just(42:int32))))

(ert-deftest test-reduction-negoptional-reduction-negjust-with-reducible-content ()

  (should (equal just(42:int32) just(42:int32))))

;; alpha conversion

(ert-deftest test-reduction-negalpha-conversion-negvariable-at-top-level ()

  (should (equal y y)))

(ert-deftest test-reduction-negalpha-conversion-negvariable-in-list ()

  (should (equal [42:int32, y] [42:int32, y])))

(ert-deftest test-reduction-negalpha-conversion-neglambda-with-different-variable-is-transparent ()

  (should (equal λz.[42:int32, y, z] λz.[42:int32, y, z])))

(ert-deftest test-reduction-negalpha-conversion-neglambda-with-same-variable-is-opaque ()

  (should (equal λx.[42:int32, x, z] λx.[42:int32, x, z])))

(ert-deftest test-reduction-negalpha-conversion-negnested-lambda-outer-variable ()

  (should (equal λa.λb.y λa.λb.y)))

(ert-deftest test-reduction-negalpha-conversion-negnested-lambda-shadows-outer ()

  (should (equal λx.λy.x λx.λy.x)))

(ert-deftest test-reduction-negalpha-conversion-negapplication-with-variable ()

  (should (equal (f @ y) (f @ y))))

(ert-deftest test-reduction-negalpha-conversion-negapplication-with-both-variables-same ()

  (should (equal (y @ y) (y @ y))))

;; type reduction

(ert-deftest test-reduction-negtype-reduction-negunit-type-unchanged ()

  (should (equal unit unit)))

(ert-deftest test-reduction-negtype-reduction-negstring-type-unchanged ()

  (should (equal string string)))

(ert-deftest test-reduction-negtype-reduction-negint32-type-unchanged ()

  (should (equal int32 int32)))

(ert-deftest test-reduction-negtype-reduction-negidentity-type-applied-to-string ()

  (should (equal (string → string) (string → string))))

(ert-deftest test-reduction-negtype-reduction-negconstant-type-ignores-argument ()

  (should (equal int32 int32)))

(ert-deftest test-reduction-negtype-reduction-negnested-forall-first-application ()

  (should (equal (∀y.(int32 → y)) (∀y.(int32 → y)))))

(ert-deftest test-reduction-negtype-reduction-negnested-forall-both-applications ()

  (should (equal (int32 → string) (int32 → string))))

(ert-deftest test-reduction-negtype-reduction-neglist-type-applied ()

  (should (equal list<int32> list<int32>)))

(ert-deftest test-reduction-negtype-reduction-negoptional-type-applied ()

  (should (equal maybe<string> maybe<string>)))
