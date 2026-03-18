;; Note: this is an automatically generated file. Do not edit.
;; hydra.lib.equality primitives

;; compare

(defun test-compare-negless-than ()

  (assert (equal (list :less_than cl:nil) ((hydra_lib_equality_compare 3) 5))))

(defun test-compare-negequal ()

  (assert (equal (list :equal_to cl:nil) ((hydra_lib_equality_compare 5) 5))))

(defun test-compare-neggreater-than ()

  (assert (equal (list :greater_than cl:nil) ((hydra_lib_equality_compare 5) 3))))

;; equal

(defun test-equal-negequal-integers ()

  (assert (equal cl:t ((hydra_lib_equality_equal 5) 5))))

(defun test-equal-negunequal-integers ()

  (assert (equal cl:nil ((hydra_lib_equality_equal 5) 3))))

;; gt

(defun test-gt-neggreater ()

  (assert (equal cl:t ((hydra_lib_equality_gt 5) 3))))

(defun test-gt-negequal ()

  (assert (equal cl:nil ((hydra_lib_equality_gt 5) 5))))

(defun test-gt-negless ()

  (assert (equal cl:nil ((hydra_lib_equality_gt 3) 5))))

;; gte

(defun test-gte-neggreater ()

  (assert (equal cl:t ((hydra_lib_equality_gte 5) 3))))

(defun test-gte-negequal ()

  (assert (equal cl:t ((hydra_lib_equality_gte 5) 5))))

(defun test-gte-negless ()

  (assert (equal cl:nil ((hydra_lib_equality_gte 3) 5))))

;; identity

(defun test-identity-neginteger ()

  (assert (equal 42 (hydra_lib_equality_identity 42))))

;; lt

(defun test-lt-negless ()

  (assert (equal cl:t ((hydra_lib_equality_lt 3) 5))))

(defun test-lt-negequal ()

  (assert (equal cl:nil ((hydra_lib_equality_lt 5) 5))))

(defun test-lt-neggreater ()

  (assert (equal cl:nil ((hydra_lib_equality_lt 5) 3))))

;; lte

(defun test-lte-negless ()

  (assert (equal cl:t ((hydra_lib_equality_lte 3) 5))))

(defun test-lte-negequal ()

  (assert (equal cl:t ((hydra_lib_equality_lte 5) 5))))

(defun test-lte-neggreater ()

  (assert (equal cl:nil ((hydra_lib_equality_lte 5) 3))))

;; max

(defun test-max-negfirst-greater ()

  (assert (equal 5 ((hydra_lib_equality_max 5) 3))))

(defun test-max-negsecond-greater ()

  (assert (equal 5 ((hydra_lib_equality_max 3) 5))))

(defun test-max-negequal ()

  (assert (equal 5 ((hydra_lib_equality_max 5) 5))))

;; min

(defun test-min-negfirst-less ()

  (assert (equal 3 ((hydra_lib_equality_min 3) 5))))

(defun test-min-negsecond-less ()

  (assert (equal 3 ((hydra_lib_equality_min 5) 3))))

(defun test-min-negequal ()

  (assert (equal 5 ((hydra_lib_equality_min 5) 5))))

;; compare strings

(defun test-compare-strings-negless-than-lexicographic ()

  (assert (equal (list :less_than cl:nil) ((hydra_lib_equality_compare "apple") "banana"))))

(defun test-compare-strings-negequal ()

  (assert (equal (list :equal_to cl:nil) ((hydra_lib_equality_compare "hello") "hello"))))

(defun test-compare-strings-neggreater-than-lexicographic ()

  (assert (equal (list :greater_than cl:nil) ((hydra_lib_equality_compare "zebra") "apple"))))

(defun test-compare-strings-negempty-vs-non-negempty ()

  (assert (equal (list :less_than cl:nil) ((hydra_lib_equality_compare "") "a"))))

(defun test-compare-strings-negprefix-vs-longer ()

  (assert (equal (list :less_than cl:nil) ((hydra_lib_equality_compare "ab") "abc"))))

;; lt strings

(defun test-lt-strings-negless-lexicographic ()

  (assert (equal cl:t ((hydra_lib_equality_lt "apple") "banana"))))

(defun test-lt-strings-negequal ()

  (assert (equal cl:nil ((hydra_lib_equality_lt "hello") "hello"))))

(defun test-lt-strings-neggreater ()

  (assert (equal cl:nil ((hydra_lib_equality_lt "zebra") "apple"))))

;; gt strings

(defun test-gt-strings-neggreater-lexicographic ()

  (assert (equal cl:t ((hydra_lib_equality_gt "zebra") "apple"))))

(defun test-gt-strings-negequal ()

  (assert (equal cl:nil ((hydra_lib_equality_gt "hello") "hello"))))

(defun test-gt-strings-negless ()

  (assert (equal cl:nil ((hydra_lib_equality_gt "apple") "banana"))))

;; max strings

(defun test-max-strings-negfirst-greater ()

  (assert (equal "zebra" ((hydra_lib_equality_max "zebra") "apple"))))

(defun test-max-strings-negsecond-greater ()

  (assert (equal "zebra" ((hydra_lib_equality_max "apple") "zebra"))))

(defun test-max-strings-negequal ()

  (assert (equal "hello" ((hydra_lib_equality_max "hello") "hello"))))

;; min strings

(defun test-min-strings-negfirst-less ()

  (assert (equal "apple" ((hydra_lib_equality_min "apple") "zebra"))))

(defun test-min-strings-negsecond-less ()

  (assert (equal "apple" ((hydra_lib_equality_min "zebra") "apple"))))

(defun test-min-strings-negequal ()

  (assert (equal "hello" ((hydra_lib_equality_min "hello") "hello"))))

;; compare floats

(defun test-compare-floats-negless-than ()

  (assert (equal (list :less_than cl:nil) ((hydra_lib_equality_compare 1.5) 2.5))))

(defun test-compare-floats-negequal ()

  (assert (equal (list :equal_to cl:nil) ((hydra_lib_equality_compare 3.14) 3.14))))

(defun test-compare-floats-neggreater-than ()

  (assert (equal (list :greater_than cl:nil) ((hydra_lib_equality_compare 5.0) 3.0))))

(defun test-compare-floats-negnegative-vs-positive ()

  (assert (equal (list :less_than cl:nil) ((hydra_lib_equality_compare -1.0) 1.0))))

;; lt floats

(defun test-lt-floats-negless ()

  (assert (equal cl:t ((hydra_lib_equality_lt 1.5) 2.5))))

(defun test-lt-floats-negequal ()

  (assert (equal cl:nil ((hydra_lib_equality_lt 3.14) 3.14))))

(defun test-lt-floats-neggreater ()

  (assert (equal cl:nil ((hydra_lib_equality_lt 5.0) 3.0))))

;; gt floats

(defun test-gt-floats-neggreater ()

  (assert (equal cl:t ((hydra_lib_equality_gt 5.0) 3.0))))

(defun test-gt-floats-negequal ()

  (assert (equal cl:nil ((hydra_lib_equality_gt 3.14) 3.14))))

(defun test-gt-floats-negless ()

  (assert (equal cl:nil ((hydra_lib_equality_gt 1.5) 2.5))))
