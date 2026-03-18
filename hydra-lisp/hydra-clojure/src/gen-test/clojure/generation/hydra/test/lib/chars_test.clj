;; Note: this is an automatically generated file. Do not edit.
;; hydra.lib.chars primitives

(ns generation.hydra.test.lib.chars-test
  (:require [clojure.test :refer :all]))

;; isAlphaNum

(deftest test-isalphanum-negletter

  (is (= true

         (hydra_lib_chars_is_alpha_num 97))))

(deftest test-isalphanum-negdigit

  (is (= true

         (hydra_lib_chars_is_alpha_num 53))))

(deftest test-isalphanum-negspace

  (is (= false

         (hydra_lib_chars_is_alpha_num 32))))

(deftest test-isalphanum-negpunctuation

  (is (= false

         (hydra_lib_chars_is_alpha_num 46))))

;; isLower

(deftest test-islower-neglowercase

  (is (= true

         (hydra_lib_chars_is_lower 97))))

(deftest test-islower-neguppercase

  (is (= false

         (hydra_lib_chars_is_lower 65))))

(deftest test-islower-negdigit

  (is (= false

         (hydra_lib_chars_is_lower 53))))

;; isSpace

(deftest test-isspace-negspace

  (is (= true

         (hydra_lib_chars_is_space 32))))

(deftest test-isspace-negtab

  (is (= true

         (hydra_lib_chars_is_space 9))))

(deftest test-isspace-negnewline

  (is (= true

         (hydra_lib_chars_is_space 10))))

(deftest test-isspace-negletter

  (is (= false

         (hydra_lib_chars_is_space 97))))

;; isUpper

(deftest test-isupper-neguppercase

  (is (= true

         (hydra_lib_chars_is_upper 65))))

(deftest test-isupper-neglowercase

  (is (= false

         (hydra_lib_chars_is_upper 97))))

(deftest test-isupper-negdigit

  (is (= false

         (hydra_lib_chars_is_upper 53))))

;; toLower

(deftest test-tolower-neguppercase

  (is (= 97

         (hydra_lib_chars_to_lower 65))))

(deftest test-tolower-neglowercase

  (is (= 97

         (hydra_lib_chars_to_lower 97))))

(deftest test-tolower-negdigit

  (is (= 53

         (hydra_lib_chars_to_lower 53))))

;; toUpper

(deftest test-toupper-neglowercase

  (is (= 65

         (hydra_lib_chars_to_upper 97))))

(deftest test-toupper-neguppercase

  (is (= 65

         (hydra_lib_chars_to_upper 65))))

(deftest test-toupper-negdigit

  (is (= 53

         (hydra_lib_chars_to_upper 53))))
