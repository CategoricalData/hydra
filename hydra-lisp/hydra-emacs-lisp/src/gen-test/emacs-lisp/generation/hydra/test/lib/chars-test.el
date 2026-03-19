;;; Note: this is an automatically generated file. Do not edit. -*- lexical-binding: t; coding: utf-8 -*-
;;; hydra.lib.chars primitives

(require 'ert)

;; isAlphaNum

(ert-deftest test-chars-negisalphanum-negletter ()

  (should (equal t (funcall hydra_lib_chars_is_alpha_num 97))))

(ert-deftest test-chars-negisalphanum-negdigit ()

  (should (equal t (funcall hydra_lib_chars_is_alpha_num 53))))

(ert-deftest test-chars-negisalphanum-negspace ()

  (should (equal nil (funcall hydra_lib_chars_is_alpha_num 32))))

(ert-deftest test-chars-negisalphanum-negpunctuation ()

  (should (equal nil (funcall hydra_lib_chars_is_alpha_num 46))))

;; isLower

(ert-deftest test-chars-negislower-neglowercase ()

  (should (equal t (funcall hydra_lib_chars_is_lower 97))))

(ert-deftest test-chars-negislower-neguppercase ()

  (should (equal nil (funcall hydra_lib_chars_is_lower 65))))

(ert-deftest test-chars-negislower-negdigit ()

  (should (equal nil (funcall hydra_lib_chars_is_lower 53))))

;; isSpace

(ert-deftest test-chars-negisspace-negspace ()

  (should (equal t (funcall hydra_lib_chars_is_space 32))))

(ert-deftest test-chars-negisspace-negtab ()

  (should (equal t (funcall hydra_lib_chars_is_space 9))))

(ert-deftest test-chars-negisspace-negnewline ()

  (should (equal t (funcall hydra_lib_chars_is_space 10))))

(ert-deftest test-chars-negisspace-negletter ()

  (should (equal nil (funcall hydra_lib_chars_is_space 97))))

;; isUpper

(ert-deftest test-chars-negisupper-neguppercase ()

  (should (equal t (funcall hydra_lib_chars_is_upper 65))))

(ert-deftest test-chars-negisupper-neglowercase ()

  (should (equal nil (funcall hydra_lib_chars_is_upper 97))))

(ert-deftest test-chars-negisupper-negdigit ()

  (should (equal nil (funcall hydra_lib_chars_is_upper 53))))

;; toLower

(ert-deftest test-chars-negtolower-neguppercase ()

  (should (equal 97 (funcall hydra_lib_chars_to_lower 65))))

(ert-deftest test-chars-negtolower-neglowercase ()

  (should (equal 97 (funcall hydra_lib_chars_to_lower 97))))

(ert-deftest test-chars-negtolower-negdigit ()

  (should (equal 53 (funcall hydra_lib_chars_to_lower 53))))

;; toUpper

(ert-deftest test-chars-negtoupper-neglowercase ()

  (should (equal 65 (funcall hydra_lib_chars_to_upper 97))))

(ert-deftest test-chars-negtoupper-neguppercase ()

  (should (equal 65 (funcall hydra_lib_chars_to_upper 65))))

(ert-deftest test-chars-negtoupper-negdigit ()

  (should (equal 53 (funcall hydra_lib_chars_to_upper 53))))
