;; Note: this is an automatically generated file. Do not edit.
;; hydra.lib.chars primitives

(ns generation.hydra.test.lib.chars-test
  (:require [clojure.test :refer :all]))

;; isAlphaNum

(deftest test-chars-negisalphanum-negletter

  (is (= true

         (hydra_lib_chars_is_alpha_num 97))))

(deftest test-chars-negisalphanum-negdigit

  (is (= true

         (hydra_lib_chars_is_alpha_num 53))))

(deftest test-chars-negisalphanum-negspace

  (is (= false

         (hydra_lib_chars_is_alpha_num 32))))

(deftest test-chars-negisalphanum-negpunctuation

  (is (= false

         (hydra_lib_chars_is_alpha_num 46))))

;; isLower

(deftest test-chars-negislower-neglowercase

  (is (= true

         (hydra_lib_chars_is_lower 97))))

(deftest test-chars-negislower-neguppercase

  (is (= false

         (hydra_lib_chars_is_lower 65))))

(deftest test-chars-negislower-negdigit

  (is (= false

         (hydra_lib_chars_is_lower 53))))

;; isSpace

(deftest test-chars-negisspace-negspace

  (is (= true

         (hydra_lib_chars_is_space 32))))

(deftest test-chars-negisspace-negtab

  (is (= true

         (hydra_lib_chars_is_space 9))))

(deftest test-chars-negisspace-negnewline

  (is (= true

         (hydra_lib_chars_is_space 10))))

(deftest test-chars-negisspace-negletter

  (is (= false

         (hydra_lib_chars_is_space 97))))

;; isUpper

(deftest test-chars-negisupper-neguppercase

  (is (= true

         (hydra_lib_chars_is_upper 65))))

(deftest test-chars-negisupper-neglowercase

  (is (= false

         (hydra_lib_chars_is_upper 97))))

(deftest test-chars-negisupper-negdigit

  (is (= false

         (hydra_lib_chars_is_upper 53))))

;; toLower

(deftest test-chars-negtolower-neguppercase

  (is (= 97

         (hydra_lib_chars_to_lower 65))))

(deftest test-chars-negtolower-neglowercase

  (is (= 97

         (hydra_lib_chars_to_lower 97))))

(deftest test-chars-negtolower-negdigit

  (is (= 53

         (hydra_lib_chars_to_lower 53))))

;; toUpper

(deftest test-chars-negtoupper-neglowercase

  (is (= 65

         (hydra_lib_chars_to_upper 97))))

(deftest test-chars-negtoupper-neguppercase

  (is (= 65

         (hydra_lib_chars_to_upper 65))))

(deftest test-chars-negtoupper-negdigit

  (is (= 53

         (hydra_lib_chars_to_upper 53))))
