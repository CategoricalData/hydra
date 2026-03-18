;;; Note: this is an automatically generated file. Do not edit.
;;; hydra.lib.chars primitives

(require 'ert)

;; isAlphaNum

(ert-deftest test-isalphanum-negletter ()

  (should (equal t (hydra_lib_chars_is_alpha_num 97))))

(ert-deftest test-isalphanum-negdigit ()

  (should (equal t (hydra_lib_chars_is_alpha_num 53))))

(ert-deftest test-isalphanum-negspace ()

  (should (equal nil (hydra_lib_chars_is_alpha_num 32))))

(ert-deftest test-isalphanum-negpunctuation ()

  (should (equal nil (hydra_lib_chars_is_alpha_num 46))))

;; isLower

(ert-deftest test-islower-neglowercase ()

  (should (equal t (hydra_lib_chars_is_lower 97))))

(ert-deftest test-islower-neguppercase ()

  (should (equal nil (hydra_lib_chars_is_lower 65))))

(ert-deftest test-islower-negdigit ()

  (should (equal nil (hydra_lib_chars_is_lower 53))))

;; isSpace

(ert-deftest test-isspace-negspace ()

  (should (equal t (hydra_lib_chars_is_space 32))))

(ert-deftest test-isspace-negtab ()

  (should (equal t (hydra_lib_chars_is_space 9))))

(ert-deftest test-isspace-negnewline ()

  (should (equal t (hydra_lib_chars_is_space 10))))

(ert-deftest test-isspace-negletter ()

  (should (equal nil (hydra_lib_chars_is_space 97))))

;; isUpper

(ert-deftest test-isupper-neguppercase ()

  (should (equal t (hydra_lib_chars_is_upper 65))))

(ert-deftest test-isupper-neglowercase ()

  (should (equal nil (hydra_lib_chars_is_upper 97))))

(ert-deftest test-isupper-negdigit ()

  (should (equal nil (hydra_lib_chars_is_upper 53))))

;; toLower

(ert-deftest test-tolower-neguppercase ()

  (should (equal 97 (hydra_lib_chars_to_lower 65))))

(ert-deftest test-tolower-neglowercase ()

  (should (equal 97 (hydra_lib_chars_to_lower 97))))

(ert-deftest test-tolower-negdigit ()

  (should (equal 53 (hydra_lib_chars_to_lower 53))))

;; toUpper

(ert-deftest test-toupper-neglowercase ()

  (should (equal 65 (hydra_lib_chars_to_upper 97))))

(ert-deftest test-toupper-neguppercase ()

  (should (equal 65 (hydra_lib_chars_to_upper 65))))

(ert-deftest test-toupper-negdigit ()

  (should (equal 53 (hydra_lib_chars_to_upper 53))))
