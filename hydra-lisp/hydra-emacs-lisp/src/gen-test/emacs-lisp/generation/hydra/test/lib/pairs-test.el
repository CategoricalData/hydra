;;; Note: this is an automatically generated file. Do not edit. -*- lexical-binding: t; coding: utf-8 -*-
;;; hydra.lib.pairs primitives

(require 'ert)

;; bimap

(ert-deftest test-pairs-negbimap-negtransform-both-elements ()

  (should (equal (list 10 2) (funcall (funcall (funcall hydra_lib_pairs_bimap (lambda (x) (funcall (hydra_lib_math_mul x) 2))) (lambda (s) (hydra_lib_strings_length s))) (list 5 "ab")))))

(ert-deftest test-pairs-negbimap-negwith-zero ()

  (should (equal (list 0 5) (funcall (funcall (funcall hydra_lib_pairs_bimap (lambda (x) (funcall (hydra_lib_math_mul x) 2))) (lambda (s) (hydra_lib_strings_length s))) (list 0 "hello")))))

;; first

(ert-deftest test-pairs-negfirst-negextract-first-element ()

  (should (equal 42 (funcall hydra_lib_pairs_first (list 42 "hello")))))

(ert-deftest test-pairs-negfirst-negwith-zero ()

  (should (equal 0 (funcall hydra_lib_pairs_first (list 0 "world")))))

(ert-deftest test-pairs-negfirst-negnegative-number ()

  (should (equal -5 (funcall hydra_lib_pairs_first (list -5 "test")))))

;; second

(ert-deftest test-pairs-negsecond-negextract-second-element ()

  (should (equal "hello" (funcall hydra_lib_pairs_second (list 42 "hello")))))

(ert-deftest test-pairs-negsecond-negempty-string ()

  (should (equal "" (funcall hydra_lib_pairs_second (list 0 "")))))

(ert-deftest test-pairs-negsecond-neglong-string ()

  (should (equal "testing" (funcall hydra_lib_pairs_second (list 123 "testing")))))
