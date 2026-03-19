;;; Note: this is an automatically generated file. Do not edit. -*- lexical-binding: t; coding: utf-8 -*-
;;; hydra.lib.equality primitives

(require 'ert)

;; compare

(ert-deftest test-equality-negcompare-negless-than ()

  (should (equal (list :less_than nil) (funcall (funcall hydra_lib_equality_compare 3) 5))))

(ert-deftest test-equality-negcompare-negequal ()

  (should (equal (list :equal_to nil) (funcall (funcall hydra_lib_equality_compare 5) 5))))

(ert-deftest test-equality-negcompare-neggreater-than ()

  (should (equal (list :greater_than nil) (funcall (funcall hydra_lib_equality_compare 5) 3))))

;; equal

(ert-deftest test-equality-negequal-negequal-integers ()

  (should (equal t (funcall (funcall hydra_lib_equality_equal 5) 5))))

(ert-deftest test-equality-negequal-negunequal-integers ()

  (should (equal nil (funcall (funcall hydra_lib_equality_equal 5) 3))))

;; gt

(ert-deftest test-equality-neggt-neggreater ()

  (should (equal t (funcall (funcall hydra_lib_equality_gt 5) 3))))

(ert-deftest test-equality-neggt-negequal ()

  (should (equal nil (funcall (funcall hydra_lib_equality_gt 5) 5))))

(ert-deftest test-equality-neggt-negless ()

  (should (equal nil (funcall (funcall hydra_lib_equality_gt 3) 5))))

;; gte

(ert-deftest test-equality-neggte-neggreater ()

  (should (equal t (funcall (funcall hydra_lib_equality_gte 5) 3))))

(ert-deftest test-equality-neggte-negequal ()

  (should (equal t (funcall (funcall hydra_lib_equality_gte 5) 5))))

(ert-deftest test-equality-neggte-negless ()

  (should (equal nil (funcall (funcall hydra_lib_equality_gte 3) 5))))

;; identity

(ert-deftest test-equality-negidentity-neginteger ()

  (should (equal 42 (funcall hydra_lib_equality_identity 42))))

;; lt

(ert-deftest test-equality-neglt-negless ()

  (should (equal t (funcall (funcall hydra_lib_equality_lt 3) 5))))

(ert-deftest test-equality-neglt-negequal ()

  (should (equal nil (funcall (funcall hydra_lib_equality_lt 5) 5))))

(ert-deftest test-equality-neglt-neggreater ()

  (should (equal nil (funcall (funcall hydra_lib_equality_lt 5) 3))))

;; lte

(ert-deftest test-equality-neglte-negless ()

  (should (equal t (funcall (funcall hydra_lib_equality_lte 3) 5))))

(ert-deftest test-equality-neglte-negequal ()

  (should (equal t (funcall (funcall hydra_lib_equality_lte 5) 5))))

(ert-deftest test-equality-neglte-neggreater ()

  (should (equal nil (funcall (funcall hydra_lib_equality_lte 5) 3))))

;; max

(ert-deftest test-equality-negmax-negfirst-greater ()

  (should (equal 5 (funcall (funcall hydra_lib_equality_max 5) 3))))

(ert-deftest test-equality-negmax-negsecond-greater ()

  (should (equal 5 (funcall (funcall hydra_lib_equality_max 3) 5))))

(ert-deftest test-equality-negmax-negequal ()

  (should (equal 5 (funcall (funcall hydra_lib_equality_max 5) 5))))

;; min

(ert-deftest test-equality-negmin-negfirst-less ()

  (should (equal 3 (funcall (funcall hydra_lib_equality_min 3) 5))))

(ert-deftest test-equality-negmin-negsecond-less ()

  (should (equal 3 (funcall (funcall hydra_lib_equality_min 5) 3))))

(ert-deftest test-equality-negmin-negequal ()

  (should (equal 5 (funcall (funcall hydra_lib_equality_min 5) 5))))

;; compare strings

(ert-deftest test-equality-negcompare-strings-negless-than-lexicographic ()

  (should (equal (list :less_than nil) (funcall (funcall hydra_lib_equality_compare "apple") "banana"))))

(ert-deftest test-equality-negcompare-strings-negequal ()

  (should (equal (list :equal_to nil) (funcall (funcall hydra_lib_equality_compare "hello") "hello"))))

(ert-deftest test-equality-negcompare-strings-neggreater-than-lexicographic ()

  (should (equal (list :greater_than nil) (funcall (funcall hydra_lib_equality_compare "zebra") "apple"))))

(ert-deftest test-equality-negcompare-strings-negempty-vs-non-negempty ()

  (should (equal (list :less_than nil) (funcall (funcall hydra_lib_equality_compare "") "a"))))

(ert-deftest test-equality-negcompare-strings-negprefix-vs-longer ()

  (should (equal (list :less_than nil) (funcall (funcall hydra_lib_equality_compare "ab") "abc"))))

;; lt strings

(ert-deftest test-equality-neglt-strings-negless-lexicographic ()

  (should (equal t (funcall (funcall hydra_lib_equality_lt "apple") "banana"))))

(ert-deftest test-equality-neglt-strings-negequal ()

  (should (equal nil (funcall (funcall hydra_lib_equality_lt "hello") "hello"))))

(ert-deftest test-equality-neglt-strings-neggreater ()

  (should (equal nil (funcall (funcall hydra_lib_equality_lt "zebra") "apple"))))

;; gt strings

(ert-deftest test-equality-neggt-strings-neggreater-lexicographic ()

  (should (equal t (funcall (funcall hydra_lib_equality_gt "zebra") "apple"))))

(ert-deftest test-equality-neggt-strings-negequal ()

  (should (equal nil (funcall (funcall hydra_lib_equality_gt "hello") "hello"))))

(ert-deftest test-equality-neggt-strings-negless ()

  (should (equal nil (funcall (funcall hydra_lib_equality_gt "apple") "banana"))))

;; max strings

(ert-deftest test-equality-negmax-strings-negfirst-greater ()

  (should (equal "zebra" (funcall (funcall hydra_lib_equality_max "zebra") "apple"))))

(ert-deftest test-equality-negmax-strings-negsecond-greater ()

  (should (equal "zebra" (funcall (funcall hydra_lib_equality_max "apple") "zebra"))))

(ert-deftest test-equality-negmax-strings-negequal ()

  (should (equal "hello" (funcall (funcall hydra_lib_equality_max "hello") "hello"))))

;; min strings

(ert-deftest test-equality-negmin-strings-negfirst-less ()

  (should (equal "apple" (funcall (funcall hydra_lib_equality_min "apple") "zebra"))))

(ert-deftest test-equality-negmin-strings-negsecond-less ()

  (should (equal "apple" (funcall (funcall hydra_lib_equality_min "zebra") "apple"))))

(ert-deftest test-equality-negmin-strings-negequal ()

  (should (equal "hello" (funcall (funcall hydra_lib_equality_min "hello") "hello"))))

;; compare floats

(ert-deftest test-equality-negcompare-floats-negless-than ()

  (should (equal (list :less_than nil) (funcall (funcall hydra_lib_equality_compare 1.5) 2.5))))

(ert-deftest test-equality-negcompare-floats-negequal ()

  (should (equal (list :equal_to nil) (funcall (funcall hydra_lib_equality_compare 3.14) 3.14))))

(ert-deftest test-equality-negcompare-floats-neggreater-than ()

  (should (equal (list :greater_than nil) (funcall (funcall hydra_lib_equality_compare 5.0) 3.0))))

(ert-deftest test-equality-negcompare-floats-negnegative-vs-positive ()

  (should (equal (list :less_than nil) (funcall (funcall hydra_lib_equality_compare -1.0) 1.0))))

;; lt floats

(ert-deftest test-equality-neglt-floats-negless ()

  (should (equal t (funcall (funcall hydra_lib_equality_lt 1.5) 2.5))))

(ert-deftest test-equality-neglt-floats-negequal ()

  (should (equal nil (funcall (funcall hydra_lib_equality_lt 3.14) 3.14))))

(ert-deftest test-equality-neglt-floats-neggreater ()

  (should (equal nil (funcall (funcall hydra_lib_equality_lt 5.0) 3.0))))

;; gt floats

(ert-deftest test-equality-neggt-floats-neggreater ()

  (should (equal t (funcall (funcall hydra_lib_equality_gt 5.0) 3.0))))

(ert-deftest test-equality-neggt-floats-negequal ()

  (should (equal nil (funcall (funcall hydra_lib_equality_gt 3.14) 3.14))))

(ert-deftest test-equality-neggt-floats-negless ()

  (should (equal nil (funcall (funcall hydra_lib_equality_gt 1.5) 2.5))))
