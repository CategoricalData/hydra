;;; Note: this is an automatically generated file. Do not edit. -*- lexical-binding: t; coding: utf-8 -*-
;;; substitution

(require 'ert)

;; substInType

(ert-deftest test-substitution-negsubstintype-negempty-substitution-returns-type-unchanged ()

  (should (equal string string)))

(ert-deftest test-substitution-negsubstintype-negsubstitute-type-variable-with-int32 ()

  (should (equal int32 int32)))

(ert-deftest test-substitution-negsubstintype-negnon-negmatching-variable-unchanged ()

  (should (equal b b)))

(ert-deftest test-substitution-negsubstintype-negsubstitute-in-function-domain ()

  (should (equal (int32 → string) (int32 → string))))

(ert-deftest test-substitution-negsubstintype-negsubstitute-in-function-codomain ()

  (should (equal (int32 → string) (int32 → string))))

(ert-deftest test-substitution-negsubstintype-negsubstitute-in-list-element-type ()

  (should (equal list<int32> list<int32>)))

(ert-deftest test-substitution-negsubstintype-negsubstitute-in-optional-type ()

  (should (equal maybe<string> maybe<string>)))

(ert-deftest test-substitution-negsubstintype-negsubstitute-in-pair-type-both-sides ()

  (should (equal (int32, int32) (int32, int32))))

(ert-deftest test-substitution-negsubstintype-negsubstitute-in-either-type ()

  (should (equal either<string, int32> either<string, int32>)))

(ert-deftest test-substitution-negsubstintype-negsubstitute-in-map-key-type ()

  (should (equal map<string, int32> map<string, int32>)))

(ert-deftest test-substitution-negsubstintype-negsubstitute-in-set-type ()

  (should (equal set<int32> set<int32>)))

(ert-deftest test-substitution-negsubstintype-negnested-substitution-in-list-of-pairs ()

  (should (equal list<(int32, string)> list<(int32, string)>)))

(ert-deftest test-substitution-negsubstintype-negmultiple-substitutions ()

  (should (equal (int32, string) (int32, string))))

(ert-deftest test-substitution-negsubstintype-negforall-bound-variable-not-substituted ()

  (should (equal (∀a.(a → a)) (∀a.(a → a)))))

(ert-deftest test-substitution-negsubstintype-negforall-free-variable-substituted ()

  (should (equal (∀a.(a → string)) (∀a.(a → string)))))
