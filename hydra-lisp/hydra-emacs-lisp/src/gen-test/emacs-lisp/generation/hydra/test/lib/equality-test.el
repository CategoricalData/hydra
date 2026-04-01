;;; Note: this is an automatically generated file. Do not edit. -*- lexical-binding: t; coding: utf-8 -*-
;;; hydra.lib.equality primitives

(require 'ert)

;; compare

(ert-deftest test-equality-negcompare-negless-than ()

  (should (equal inject(hydra.util.Comparison){lessThan=unit} inject(hydra.util.Comparison){lessThan=unit})))

(ert-deftest test-equality-negcompare-negequal ()

  (should (equal inject(hydra.util.Comparison){equalTo=unit} inject(hydra.util.Comparison){equalTo=unit})))

(ert-deftest test-equality-negcompare-neggreater-than ()

  (should (equal inject(hydra.util.Comparison){greaterThan=unit} inject(hydra.util.Comparison){greaterThan=unit})))

;; equal

(ert-deftest test-equality-negequal-negequal-integers ()

  (should (equal true true)))

(ert-deftest test-equality-negequal-negunequal-integers ()

  (should (equal false false)))

;; gt

(ert-deftest test-equality-neggt-neggreater ()

  (should (equal true true)))

(ert-deftest test-equality-neggt-negequal ()

  (should (equal false false)))

(ert-deftest test-equality-neggt-negless ()

  (should (equal false false)))

;; gte

(ert-deftest test-equality-neggte-neggreater ()

  (should (equal true true)))

(ert-deftest test-equality-neggte-negequal ()

  (should (equal true true)))

(ert-deftest test-equality-neggte-negless ()

  (should (equal false false)))

;; identity

(ert-deftest test-equality-negidentity-neginteger ()

  (should (equal 42:int32 42:int32)))

;; lt

(ert-deftest test-equality-neglt-negless ()

  (should (equal true true)))

(ert-deftest test-equality-neglt-negequal ()

  (should (equal false false)))

(ert-deftest test-equality-neglt-neggreater ()

  (should (equal false false)))

;; lte

(ert-deftest test-equality-neglte-negless ()

  (should (equal true true)))

(ert-deftest test-equality-neglte-negequal ()

  (should (equal true true)))

(ert-deftest test-equality-neglte-neggreater ()

  (should (equal false false)))

;; max

(ert-deftest test-equality-negmax-negfirst-greater ()

  (should (equal 5:int32 5:int32)))

(ert-deftest test-equality-negmax-negsecond-greater ()

  (should (equal 5:int32 5:int32)))

(ert-deftest test-equality-negmax-negequal ()

  (should (equal 5:int32 5:int32)))

;; min

(ert-deftest test-equality-negmin-negfirst-less ()

  (should (equal 3:int32 3:int32)))

(ert-deftest test-equality-negmin-negsecond-less ()

  (should (equal 3:int32 3:int32)))

(ert-deftest test-equality-negmin-negequal ()

  (should (equal 5:int32 5:int32)))

;; compare strings

(ert-deftest test-equality-negcompare-strings-negless-than-lexicographic ()

  (should (equal inject(hydra.util.Comparison){lessThan=unit} inject(hydra.util.Comparison){lessThan=unit})))

(ert-deftest test-equality-negcompare-strings-negequal ()

  (should (equal inject(hydra.util.Comparison){equalTo=unit} inject(hydra.util.Comparison){equalTo=unit})))

(ert-deftest test-equality-negcompare-strings-neggreater-than-lexicographic ()

  (should (equal inject(hydra.util.Comparison){greaterThan=unit} inject(hydra.util.Comparison){greaterThan=unit})))

(ert-deftest test-equality-negcompare-strings-negempty-vs-non-negempty ()

  (should (equal inject(hydra.util.Comparison){lessThan=unit} inject(hydra.util.Comparison){lessThan=unit})))

(ert-deftest test-equality-negcompare-strings-negprefix-vs-longer ()

  (should (equal inject(hydra.util.Comparison){lessThan=unit} inject(hydra.util.Comparison){lessThan=unit})))

;; lt strings

(ert-deftest test-equality-neglt-strings-negless-lexicographic ()

  (should (equal true true)))

(ert-deftest test-equality-neglt-strings-negequal ()

  (should (equal false false)))

(ert-deftest test-equality-neglt-strings-neggreater ()

  (should (equal false false)))

;; gt strings

(ert-deftest test-equality-neggt-strings-neggreater-lexicographic ()

  (should (equal true true)))

(ert-deftest test-equality-neggt-strings-negequal ()

  (should (equal false false)))

(ert-deftest test-equality-neggt-strings-negless ()

  (should (equal false false)))

;; max strings

(ert-deftest test-equality-negmax-strings-negfirst-greater ()

  (should (equal "zebra" "zebra")))

(ert-deftest test-equality-negmax-strings-negsecond-greater ()

  (should (equal "zebra" "zebra")))

(ert-deftest test-equality-negmax-strings-negequal ()

  (should (equal "hello" "hello")))

;; min strings

(ert-deftest test-equality-negmin-strings-negfirst-less ()

  (should (equal "apple" "apple")))

(ert-deftest test-equality-negmin-strings-negsecond-less ()

  (should (equal "apple" "apple")))

(ert-deftest test-equality-negmin-strings-negequal ()

  (should (equal "hello" "hello")))

;; compare floats

(ert-deftest test-equality-negcompare-floats-negless-than ()

  (should (equal inject(hydra.util.Comparison){lessThan=unit} inject(hydra.util.Comparison){lessThan=unit})))

(ert-deftest test-equality-negcompare-floats-negequal ()

  (should (equal inject(hydra.util.Comparison){equalTo=unit} inject(hydra.util.Comparison){equalTo=unit})))

(ert-deftest test-equality-negcompare-floats-neggreater-than ()

  (should (equal inject(hydra.util.Comparison){greaterThan=unit} inject(hydra.util.Comparison){greaterThan=unit})))

(ert-deftest test-equality-negcompare-floats-negnegative-vs-positive ()

  (should (equal inject(hydra.util.Comparison){lessThan=unit} inject(hydra.util.Comparison){lessThan=unit})))

;; lt floats

(ert-deftest test-equality-neglt-floats-negless ()

  (should (equal true true)))

(ert-deftest test-equality-neglt-floats-negequal ()

  (should (equal false false)))

(ert-deftest test-equality-neglt-floats-neggreater ()

  (should (equal false false)))

;; gt floats

(ert-deftest test-equality-neggt-floats-neggreater ()

  (should (equal true true)))

(ert-deftest test-equality-neggt-floats-negequal ()

  (should (equal false false)))

(ert-deftest test-equality-neggt-floats-negless ()

  (should (equal false false)))
