;; Note: this is an automatically generated file. Do not edit.
;; hydra.lib.equality primitives

;; compare

(defun test-equality-negcompare-negless-than ()

  (assert (equal inject(hydra.util.Comparison){lessThan=unit} inject(hydra.util.Comparison){lessThan=unit})))

(defun test-equality-negcompare-negequal ()

  (assert (equal inject(hydra.util.Comparison){equalTo=unit} inject(hydra.util.Comparison){equalTo=unit})))

(defun test-equality-negcompare-neggreater-than ()

  (assert (equal inject(hydra.util.Comparison){greaterThan=unit} inject(hydra.util.Comparison){greaterThan=unit})))

;; equal

(defun test-equality-negequal-negequal-integers ()

  (assert (equal true true)))

(defun test-equality-negequal-negunequal-integers ()

  (assert (equal false false)))

;; gt

(defun test-equality-neggt-neggreater ()

  (assert (equal true true)))

(defun test-equality-neggt-negequal ()

  (assert (equal false false)))

(defun test-equality-neggt-negless ()

  (assert (equal false false)))

;; gte

(defun test-equality-neggte-neggreater ()

  (assert (equal true true)))

(defun test-equality-neggte-negequal ()

  (assert (equal true true)))

(defun test-equality-neggte-negless ()

  (assert (equal false false)))

;; identity

(defun test-equality-negidentity-neginteger ()

  (assert (equal 42:int32 42:int32)))

;; lt

(defun test-equality-neglt-negless ()

  (assert (equal true true)))

(defun test-equality-neglt-negequal ()

  (assert (equal false false)))

(defun test-equality-neglt-neggreater ()

  (assert (equal false false)))

;; lte

(defun test-equality-neglte-negless ()

  (assert (equal true true)))

(defun test-equality-neglte-negequal ()

  (assert (equal true true)))

(defun test-equality-neglte-neggreater ()

  (assert (equal false false)))

;; max

(defun test-equality-negmax-negfirst-greater ()

  (assert (equal 5:int32 5:int32)))

(defun test-equality-negmax-negsecond-greater ()

  (assert (equal 5:int32 5:int32)))

(defun test-equality-negmax-negequal ()

  (assert (equal 5:int32 5:int32)))

;; min

(defun test-equality-negmin-negfirst-less ()

  (assert (equal 3:int32 3:int32)))

(defun test-equality-negmin-negsecond-less ()

  (assert (equal 3:int32 3:int32)))

(defun test-equality-negmin-negequal ()

  (assert (equal 5:int32 5:int32)))

;; compare strings

(defun test-equality-negcompare-strings-negless-than-lexicographic ()

  (assert (equal inject(hydra.util.Comparison){lessThan=unit} inject(hydra.util.Comparison){lessThan=unit})))

(defun test-equality-negcompare-strings-negequal ()

  (assert (equal inject(hydra.util.Comparison){equalTo=unit} inject(hydra.util.Comparison){equalTo=unit})))

(defun test-equality-negcompare-strings-neggreater-than-lexicographic ()

  (assert (equal inject(hydra.util.Comparison){greaterThan=unit} inject(hydra.util.Comparison){greaterThan=unit})))

(defun test-equality-negcompare-strings-negempty-vs-non-negempty ()

  (assert (equal inject(hydra.util.Comparison){lessThan=unit} inject(hydra.util.Comparison){lessThan=unit})))

(defun test-equality-negcompare-strings-negprefix-vs-longer ()

  (assert (equal inject(hydra.util.Comparison){lessThan=unit} inject(hydra.util.Comparison){lessThan=unit})))

;; lt strings

(defun test-equality-neglt-strings-negless-lexicographic ()

  (assert (equal true true)))

(defun test-equality-neglt-strings-negequal ()

  (assert (equal false false)))

(defun test-equality-neglt-strings-neggreater ()

  (assert (equal false false)))

;; gt strings

(defun test-equality-neggt-strings-neggreater-lexicographic ()

  (assert (equal true true)))

(defun test-equality-neggt-strings-negequal ()

  (assert (equal false false)))

(defun test-equality-neggt-strings-negless ()

  (assert (equal false false)))

;; max strings

(defun test-equality-negmax-strings-negfirst-greater ()

  (assert (equal "zebra" "zebra")))

(defun test-equality-negmax-strings-negsecond-greater ()

  (assert (equal "zebra" "zebra")))

(defun test-equality-negmax-strings-negequal ()

  (assert (equal "hello" "hello")))

;; min strings

(defun test-equality-negmin-strings-negfirst-less ()

  (assert (equal "apple" "apple")))

(defun test-equality-negmin-strings-negsecond-less ()

  (assert (equal "apple" "apple")))

(defun test-equality-negmin-strings-negequal ()

  (assert (equal "hello" "hello")))

;; compare floats

(defun test-equality-negcompare-floats-negless-than ()

  (assert (equal inject(hydra.util.Comparison){lessThan=unit} inject(hydra.util.Comparison){lessThan=unit})))

(defun test-equality-negcompare-floats-negequal ()

  (assert (equal inject(hydra.util.Comparison){equalTo=unit} inject(hydra.util.Comparison){equalTo=unit})))

(defun test-equality-negcompare-floats-neggreater-than ()

  (assert (equal inject(hydra.util.Comparison){greaterThan=unit} inject(hydra.util.Comparison){greaterThan=unit})))

(defun test-equality-negcompare-floats-negnegative-vs-positive ()

  (assert (equal inject(hydra.util.Comparison){lessThan=unit} inject(hydra.util.Comparison){lessThan=unit})))

;; lt floats

(defun test-equality-neglt-floats-negless ()

  (assert (equal true true)))

(defun test-equality-neglt-floats-negequal ()

  (assert (equal false false)))

(defun test-equality-neglt-floats-neggreater ()

  (assert (equal false false)))

;; gt floats

(defun test-equality-neggt-floats-neggreater ()

  (assert (equal true true)))

(defun test-equality-neggt-floats-negequal ()

  (assert (equal false false)))

(defun test-equality-neggt-floats-negless ()

  (assert (equal false false)))
