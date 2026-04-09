;;; Note: this is an automatically generated file. Do not edit. -*- lexical-binding: t; coding: utf-8 -*-
;;; hydra.lib.pairs primitives

(require 'ert)

;; bimap

(ert-deftest test-pairs-negbimap-negtransform-both-elements ()

  (should (equal (10, 2) (10, 2))))

(ert-deftest test-pairs-negbimap-negwith-zero ()

  (should (equal (0, 5) (0, 5))))

;; first

(ert-deftest test-pairs-negfirst-negextract-first-element ()

  (should (equal 42 42)))

(ert-deftest test-pairs-negfirst-negwith-zero ()

  (should (equal 0 0)))

(ert-deftest test-pairs-negfirst-negnegative-number ()

  (should (equal -5 -5)))

;; second

(ert-deftest test-pairs-negsecond-negextract-second-element ()

  (should (equal hello hello)))

(ert-deftest test-pairs-negsecond-negempty-string ()

  (should (equal  )))

(ert-deftest test-pairs-negsecond-neglong-string ()

  (should (equal testing testing)))
