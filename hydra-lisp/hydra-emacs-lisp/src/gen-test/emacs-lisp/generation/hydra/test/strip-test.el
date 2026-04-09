;;; Note: this is an automatically generated file. Do not edit. -*- lexical-binding: t; coding: utf-8 -*-
;;; strip

(require 'ert)

;; deannotateTerm

(ert-deftest test-strip-negdeannotateterm-negunannotated-literal-unchanged ()

  (should (equal 42:int32 42:int32)))

(ert-deftest test-strip-negdeannotateterm-negunannotated-variable-unchanged ()

  (should (equal x x)))

(ert-deftest test-strip-negdeannotateterm-negunannotated-lambda-unchanged ()

  (should (equal λx.x λx.x)))

(ert-deftest test-strip-negdeannotateterm-negsingle-annotation-stripped ()

  (should (equal 42:int32 42:int32)))

(ert-deftest test-strip-negdeannotateterm-negnested-annotations-stripped ()

  (should (equal 42:int32 42:int32)))

(ert-deftest test-strip-negdeannotateterm-negannotated-lambda-stripped ()

  (should (equal λx.x λx.x)))

(ert-deftest test-strip-negdeannotateterm-negannotated-application-stripped ()

  (should (equal (f @ x) (f @ x))))

;; deannotateType

(ert-deftest test-strip-negdeannotatetype-negunannotated-primitive-type-unchanged ()

  (should (equal int32 int32)))

(ert-deftest test-strip-negdeannotatetype-negunannotated-string-type-unchanged ()

  (should (equal string string)))

(ert-deftest test-strip-negdeannotatetype-negunannotated-function-type-unchanged ()

  (should (equal (int32 → string) (int32 → string))))

(ert-deftest test-strip-negdeannotatetype-negsingle-annotation-stripped ()

  (should (equal int32 int32)))

(ert-deftest test-strip-negdeannotatetype-negnested-annotations-stripped ()

  (should (equal string string)))

(ert-deftest test-strip-negdeannotatetype-negannotated-list-type-stripped ()

  (should (equal list<int32> list<int32>)))

(ert-deftest test-strip-negdeannotatetype-negannotated-function-type-stripped ()

  (should (equal (int32 → string) (int32 → string))))
