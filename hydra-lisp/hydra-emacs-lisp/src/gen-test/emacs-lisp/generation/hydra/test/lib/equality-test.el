;;; Note: this is an automatically generated file. Do not edit.
;;; hydra.lib.equality primitives

(require 'ert)

;; compare

(ert-deftest test-compare-negless-than ()

  (should (equal (list :less_than nil) ((hydra_lib_equality_compare 3) 5))))

(ert-deftest test-compare-negequal ()

  (should (equal (list :equal_to nil) ((hydra_lib_equality_compare 5) 5))))

(ert-deftest test-compare-neggreater-than ()

  (should (equal (list :greater_than nil) ((hydra_lib_equality_compare 5) 3))))

;; equal

(ert-deftest test-equal-negequal-integers ()

  (should (equal t ((hydra_lib_equality_equal 5) 5))))

(ert-deftest test-equal-negunequal-integers ()

  (should (equal nil ((hydra_lib_equality_equal 5) 3))))

;; gt

(ert-deftest test-gt-neggreater ()

  (should (equal t ((hydra_lib_equality_gt 5) 3))))

(ert-deftest test-gt-negequal ()

  (should (equal nil ((hydra_lib_equality_gt 5) 5))))

(ert-deftest test-gt-negless ()

  (should (equal nil ((hydra_lib_equality_gt 3) 5))))

;; gte

(ert-deftest test-gte-neggreater ()

  (should (equal t ((hydra_lib_equality_gte 5) 3))))

(ert-deftest test-gte-negequal ()

  (should (equal t ((hydra_lib_equality_gte 5) 5))))

(ert-deftest test-gte-negless ()

  (should (equal nil ((hydra_lib_equality_gte 3) 5))))

;; identity

(ert-deftest test-identity-neginteger ()

  (should (equal 42 (hydra_lib_equality_identity 42))))

;; lt

(ert-deftest test-lt-negless ()

  (should (equal t ((hydra_lib_equality_lt 3) 5))))

(ert-deftest test-lt-negequal ()

  (should (equal nil ((hydra_lib_equality_lt 5) 5))))

(ert-deftest test-lt-neggreater ()

  (should (equal nil ((hydra_lib_equality_lt 5) 3))))

;; lte

(ert-deftest test-lte-negless ()

  (should (equal t ((hydra_lib_equality_lte 3) 5))))

(ert-deftest test-lte-negequal ()

  (should (equal t ((hydra_lib_equality_lte 5) 5))))

(ert-deftest test-lte-neggreater ()

  (should (equal nil ((hydra_lib_equality_lte 5) 3))))

;; max

(ert-deftest test-max-negfirst-greater ()

  (should (equal 5 ((hydra_lib_equality_max 5) 3))))

(ert-deftest test-max-negsecond-greater ()

  (should (equal 5 ((hydra_lib_equality_max 3) 5))))

(ert-deftest test-max-negequal ()

  (should (equal 5 ((hydra_lib_equality_max 5) 5))))

;; min

(ert-deftest test-min-negfirst-less ()

  (should (equal 3 ((hydra_lib_equality_min 3) 5))))

(ert-deftest test-min-negsecond-less ()

  (should (equal 3 ((hydra_lib_equality_min 5) 3))))

(ert-deftest test-min-negequal ()

  (should (equal 5 ((hydra_lib_equality_min 5) 5))))

;; compare strings

(ert-deftest test-compare-strings-negless-than-lexicographic ()

  (should (equal (list :less_than nil) ((hydra_lib_equality_compare "apple") "banana"))))

(ert-deftest test-compare-strings-negequal ()

  (should (equal (list :equal_to nil) ((hydra_lib_equality_compare "hello") "hello"))))

(ert-deftest test-compare-strings-neggreater-than-lexicographic ()

  (should (equal (list :greater_than nil) ((hydra_lib_equality_compare "zebra") "apple"))))

(ert-deftest test-compare-strings-negempty-vs-non-negempty ()

  (should (equal (list :less_than nil) ((hydra_lib_equality_compare "") "a"))))

(ert-deftest test-compare-strings-negprefix-vs-longer ()

  (should (equal (list :less_than nil) ((hydra_lib_equality_compare "ab") "abc"))))

;; lt strings

(ert-deftest test-lt-strings-negless-lexicographic ()

  (should (equal t ((hydra_lib_equality_lt "apple") "banana"))))

(ert-deftest test-lt-strings-negequal ()

  (should (equal nil ((hydra_lib_equality_lt "hello") "hello"))))

(ert-deftest test-lt-strings-neggreater ()

  (should (equal nil ((hydra_lib_equality_lt "zebra") "apple"))))

;; gt strings

(ert-deftest test-gt-strings-neggreater-lexicographic ()

  (should (equal t ((hydra_lib_equality_gt "zebra") "apple"))))

(ert-deftest test-gt-strings-negequal ()

  (should (equal nil ((hydra_lib_equality_gt "hello") "hello"))))

(ert-deftest test-gt-strings-negless ()

  (should (equal nil ((hydra_lib_equality_gt "apple") "banana"))))

;; max strings

(ert-deftest test-max-strings-negfirst-greater ()

  (should (equal "zebra" ((hydra_lib_equality_max "zebra") "apple"))))

(ert-deftest test-max-strings-negsecond-greater ()

  (should (equal "zebra" ((hydra_lib_equality_max "apple") "zebra"))))

(ert-deftest test-max-strings-negequal ()

  (should (equal "hello" ((hydra_lib_equality_max "hello") "hello"))))

;; min strings

(ert-deftest test-min-strings-negfirst-less ()

  (should (equal "apple" ((hydra_lib_equality_min "apple") "zebra"))))

(ert-deftest test-min-strings-negsecond-less ()

  (should (equal "apple" ((hydra_lib_equality_min "zebra") "apple"))))

(ert-deftest test-min-strings-negequal ()

  (should (equal "hello" ((hydra_lib_equality_min "hello") "hello"))))

;; compare floats

(ert-deftest test-compare-floats-negless-than ()

  (should (equal (list :less_than nil) ((hydra_lib_equality_compare 1.5) 2.5))))

(ert-deftest test-compare-floats-negequal ()

  (should (equal (list :equal_to nil) ((hydra_lib_equality_compare 3.14) 3.14))))

(ert-deftest test-compare-floats-neggreater-than ()

  (should (equal (list :greater_than nil) ((hydra_lib_equality_compare 5.0) 3.0))))

(ert-deftest test-compare-floats-negnegative-vs-positive ()

  (should (equal (list :less_than nil) ((hydra_lib_equality_compare -1.0) 1.0))))

;; lt floats

(ert-deftest test-lt-floats-negless ()

  (should (equal t ((hydra_lib_equality_lt 1.5) 2.5))))

(ert-deftest test-lt-floats-negequal ()

  (should (equal nil ((hydra_lib_equality_lt 3.14) 3.14))))

(ert-deftest test-lt-floats-neggreater ()

  (should (equal nil ((hydra_lib_equality_lt 5.0) 3.0))))

;; gt floats

(ert-deftest test-gt-floats-neggreater ()

  (should (equal t ((hydra_lib_equality_gt 5.0) 3.0))))

(ert-deftest test-gt-floats-negequal ()

  (should (equal nil ((hydra_lib_equality_gt 3.14) 3.14))))

(ert-deftest test-gt-floats-negless ()

  (should (equal nil ((hydra_lib_equality_gt 1.5) 2.5))))
