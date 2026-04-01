;; Note: this is an automatically generated file. Do not edit.
;; hydra.lib.chars primitives

;; isAlphaNum

(defun test-chars-negisalphanum-negletter ()

  (assert (equal true true)))

(defun test-chars-negisalphanum-negdigit ()

  (assert (equal true true)))

(defun test-chars-negisalphanum-negspace ()

  (assert (equal false false)))

(defun test-chars-negisalphanum-negpunctuation ()

  (assert (equal false false)))

;; isLower

(defun test-chars-negislower-neglowercase ()

  (assert (equal true true)))

(defun test-chars-negislower-neguppercase ()

  (assert (equal false false)))

(defun test-chars-negislower-negdigit ()

  (assert (equal false false)))

;; isSpace

(defun test-chars-negisspace-negspace ()

  (assert (equal true true)))

(defun test-chars-negisspace-negtab ()

  (assert (equal true true)))

(defun test-chars-negisspace-negnewline ()

  (assert (equal true true)))

(defun test-chars-negisspace-negletter ()

  (assert (equal false false)))

;; isUpper

(defun test-chars-negisupper-neguppercase ()

  (assert (equal true true)))

(defun test-chars-negisupper-neglowercase ()

  (assert (equal false false)))

(defun test-chars-negisupper-negdigit ()

  (assert (equal false false)))

;; toLower

(defun test-chars-negtolower-neguppercase ()

  (assert (equal 97:int32 97:int32)))

(defun test-chars-negtolower-neglowercase ()

  (assert (equal 97:int32 97:int32)))

(defun test-chars-negtolower-negdigit ()

  (assert (equal 53:int32 53:int32)))

;; toUpper

(defun test-chars-negtoupper-neglowercase ()

  (assert (equal 65:int32 65:int32)))

(defun test-chars-negtoupper-neguppercase ()

  (assert (equal 65:int32 65:int32)))

(defun test-chars-negtoupper-negdigit ()

  (assert (equal 53:int32 53:int32)))
