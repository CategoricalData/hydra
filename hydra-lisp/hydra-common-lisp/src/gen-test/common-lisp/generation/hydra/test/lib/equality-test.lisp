;; Note: this is an automatically generated file. Do not edit.
;; hydra.lib.equality primitives

;; compare

(defun test-equality-negcompare-negless-than ()

  (assert (equal (list :less_than cl:nil) ((hydra_lib_equality_compare 3) 5))))

(defun test-equality-negcompare-negequal ()

  (assert (equal (list :equal_to cl:nil) ((hydra_lib_equality_compare 5) 5))))

(defun test-equality-negcompare-neggreater-than ()

  (assert (equal (list :greater_than cl:nil) ((hydra_lib_equality_compare 5) 3))))

;; equal

(defun test-equality-negequal-negequal-integers ()

  (assert (equal cl:t ((hydra_lib_equality_equal 5) 5))))

(defun test-equality-negequal-negunequal-integers ()

  (assert (equal cl:nil ((hydra_lib_equality_equal 5) 3))))

;; gt

(defun test-equality-neggt-neggreater ()

  (assert (equal cl:t ((hydra_lib_equality_gt 5) 3))))

(defun test-equality-neggt-negequal ()

  (assert (equal cl:nil ((hydra_lib_equality_gt 5) 5))))

(defun test-equality-neggt-negless ()

  (assert (equal cl:nil ((hydra_lib_equality_gt 3) 5))))

;; gte

(defun test-equality-neggte-neggreater ()

  (assert (equal cl:t ((hydra_lib_equality_gte 5) 3))))

(defun test-equality-neggte-negequal ()

  (assert (equal cl:t ((hydra_lib_equality_gte 5) 5))))

(defun test-equality-neggte-negless ()

  (assert (equal cl:nil ((hydra_lib_equality_gte 3) 5))))

;; identity

(defun test-equality-negidentity-neginteger ()

  (assert (equal 42 (hydra_lib_equality_identity 42))))

;; lt

(defun test-equality-neglt-negless ()

  (assert (equal cl:t ((hydra_lib_equality_lt 3) 5))))

(defun test-equality-neglt-negequal ()

  (assert (equal cl:nil ((hydra_lib_equality_lt 5) 5))))

(defun test-equality-neglt-neggreater ()

  (assert (equal cl:nil ((hydra_lib_equality_lt 5) 3))))

;; lte

(defun test-equality-neglte-negless ()

  (assert (equal cl:t ((hydra_lib_equality_lte 3) 5))))

(defun test-equality-neglte-negequal ()

  (assert (equal cl:t ((hydra_lib_equality_lte 5) 5))))

(defun test-equality-neglte-neggreater ()

  (assert (equal cl:nil ((hydra_lib_equality_lte 5) 3))))

;; max

(defun test-equality-negmax-negfirst-greater ()

  (assert (equal 5 ((hydra_lib_equality_max 5) 3))))

(defun test-equality-negmax-negsecond-greater ()

  (assert (equal 5 ((hydra_lib_equality_max 3) 5))))

(defun test-equality-negmax-negequal ()

  (assert (equal 5 ((hydra_lib_equality_max 5) 5))))

;; min

(defun test-equality-negmin-negfirst-less ()

  (assert (equal 3 ((hydra_lib_equality_min 3) 5))))

(defun test-equality-negmin-negsecond-less ()

  (assert (equal 3 ((hydra_lib_equality_min 5) 3))))

(defun test-equality-negmin-negequal ()

  (assert (equal 5 ((hydra_lib_equality_min 5) 5))))

;; compare strings

(defun test-equality-negcompare-strings-negless-than-lexicographic ()

  (assert (equal (list :less_than cl:nil) ((hydra_lib_equality_compare "apple") "banana"))))

(defun test-equality-negcompare-strings-negequal ()

  (assert (equal (list :equal_to cl:nil) ((hydra_lib_equality_compare "hello") "hello"))))

(defun test-equality-negcompare-strings-neggreater-than-lexicographic ()

  (assert (equal (list :greater_than cl:nil) ((hydra_lib_equality_compare "zebra") "apple"))))

(defun test-equality-negcompare-strings-negempty-vs-non-negempty ()

  (assert (equal (list :less_than cl:nil) ((hydra_lib_equality_compare "") "a"))))

(defun test-equality-negcompare-strings-negprefix-vs-longer ()

  (assert (equal (list :less_than cl:nil) ((hydra_lib_equality_compare "ab") "abc"))))

;; lt strings

(defun test-equality-neglt-strings-negless-lexicographic ()

  (assert (equal cl:t ((hydra_lib_equality_lt "apple") "banana"))))

(defun test-equality-neglt-strings-negequal ()

  (assert (equal cl:nil ((hydra_lib_equality_lt "hello") "hello"))))

(defun test-equality-neglt-strings-neggreater ()

  (assert (equal cl:nil ((hydra_lib_equality_lt "zebra") "apple"))))

;; gt strings

(defun test-equality-neggt-strings-neggreater-lexicographic ()

  (assert (equal cl:t ((hydra_lib_equality_gt "zebra") "apple"))))

(defun test-equality-neggt-strings-negequal ()

  (assert (equal cl:nil ((hydra_lib_equality_gt "hello") "hello"))))

(defun test-equality-neggt-strings-negless ()

  (assert (equal cl:nil ((hydra_lib_equality_gt "apple") "banana"))))

;; max strings

(defun test-equality-negmax-strings-negfirst-greater ()

  (assert (equal "zebra" ((hydra_lib_equality_max "zebra") "apple"))))

(defun test-equality-negmax-strings-negsecond-greater ()

  (assert (equal "zebra" ((hydra_lib_equality_max "apple") "zebra"))))

(defun test-equality-negmax-strings-negequal ()

  (assert (equal "hello" ((hydra_lib_equality_max "hello") "hello"))))

;; min strings

(defun test-equality-negmin-strings-negfirst-less ()

  (assert (equal "apple" ((hydra_lib_equality_min "apple") "zebra"))))

(defun test-equality-negmin-strings-negsecond-less ()

  (assert (equal "apple" ((hydra_lib_equality_min "zebra") "apple"))))

(defun test-equality-negmin-strings-negequal ()

  (assert (equal "hello" ((hydra_lib_equality_min "hello") "hello"))))

;; compare floats

(defun test-equality-negcompare-floats-negless-than ()

  (assert (equal (list :less_than cl:nil) ((hydra_lib_equality_compare 1.5) 2.5))))

(defun test-equality-negcompare-floats-negequal ()

  (assert (equal (list :equal_to cl:nil) ((hydra_lib_equality_compare 3.14) 3.14))))

(defun test-equality-negcompare-floats-neggreater-than ()

  (assert (equal (list :greater_than cl:nil) ((hydra_lib_equality_compare 5.0) 3.0))))

(defun test-equality-negcompare-floats-negnegative-vs-positive ()

  (assert (equal (list :less_than cl:nil) ((hydra_lib_equality_compare -1.0) 1.0))))

;; lt floats

(defun test-equality-neglt-floats-negless ()

  (assert (equal cl:t ((hydra_lib_equality_lt 1.5) 2.5))))

(defun test-equality-neglt-floats-negequal ()

  (assert (equal cl:nil ((hydra_lib_equality_lt 3.14) 3.14))))

(defun test-equality-neglt-floats-neggreater ()

  (assert (equal cl:nil ((hydra_lib_equality_lt 5.0) 3.0))))

;; gt floats

(defun test-equality-neggt-floats-neggreater ()

  (assert (equal cl:t ((hydra_lib_equality_gt 5.0) 3.0))))

(defun test-equality-neggt-floats-negequal ()

  (assert (equal cl:nil ((hydra_lib_equality_gt 3.14) 3.14))))

(defun test-equality-neggt-floats-negless ()

  (assert (equal cl:nil ((hydra_lib_equality_gt 1.5) 2.5))))
