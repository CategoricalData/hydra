;;; Note: this is an automatically generated file. Do not edit. -*- lexical-binding: t; coding: utf-8 -*-
;;; hydra.lib.chars primitives

(require 'ert)

;; isAlphaNum

(ert-deftest test-chars-negisalphanum-negletter ()

  (should (equal true true)))

(ert-deftest test-chars-negisalphanum-negdigit ()

  (should (equal true true)))

(ert-deftest test-chars-negisalphanum-negspace ()

  (should (equal false false)))

(ert-deftest test-chars-negisalphanum-negpunctuation ()

  (should (equal false false)))

;; isLower

(ert-deftest test-chars-negislower-neglowercase ()

  (should (equal true true)))

(ert-deftest test-chars-negislower-neguppercase ()

  (should (equal false false)))

(ert-deftest test-chars-negislower-negdigit ()

  (should (equal false false)))

;; isSpace

(ert-deftest test-chars-negisspace-negspace ()

  (should (equal true true)))

(ert-deftest test-chars-negisspace-negtab ()

  (should (equal true true)))

(ert-deftest test-chars-negisspace-negnewline ()

  (should (equal true true)))

(ert-deftest test-chars-negisspace-negletter ()

  (should (equal false false)))

;; isUpper

(ert-deftest test-chars-negisupper-neguppercase ()

  (should (equal true true)))

(ert-deftest test-chars-negisupper-neglowercase ()

  (should (equal false false)))

(ert-deftest test-chars-negisupper-negdigit ()

  (should (equal false false)))

;; toLower

(ert-deftest test-chars-negtolower-neguppercase ()

  (should (equal 97:int32 97:int32)))

(ert-deftest test-chars-negtolower-neglowercase ()

  (should (equal 97:int32 97:int32)))

(ert-deftest test-chars-negtolower-negdigit ()

  (should (equal 53:int32 53:int32)))

;; toUpper

(ert-deftest test-chars-negtoupper-neglowercase ()

  (should (equal 65:int32 65:int32)))

(ert-deftest test-chars-negtoupper-neguppercase ()

  (should (equal 65:int32 65:int32)))

(ert-deftest test-chars-negtoupper-negdigit ()

  (should (equal 53:int32 53:int32)))
