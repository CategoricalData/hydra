;; Note: this is an automatically generated file. Do not edit.
;; hydra.lib.chars primitives

;; isAlphaNum

(defun test-chars-negisalphanum-negletter ()

  (assert (equal cl:t (hydra_lib_chars_is_alpha_num 97))))

(defun test-chars-negisalphanum-negdigit ()

  (assert (equal cl:t (hydra_lib_chars_is_alpha_num 53))))

(defun test-chars-negisalphanum-negspace ()

  (assert (equal cl:nil (hydra_lib_chars_is_alpha_num 32))))

(defun test-chars-negisalphanum-negpunctuation ()

  (assert (equal cl:nil (hydra_lib_chars_is_alpha_num 46))))

;; isLower

(defun test-chars-negislower-neglowercase ()

  (assert (equal cl:t (hydra_lib_chars_is_lower 97))))

(defun test-chars-negislower-neguppercase ()

  (assert (equal cl:nil (hydra_lib_chars_is_lower 65))))

(defun test-chars-negislower-negdigit ()

  (assert (equal cl:nil (hydra_lib_chars_is_lower 53))))

;; isSpace

(defun test-chars-negisspace-negspace ()

  (assert (equal cl:t (hydra_lib_chars_is_space 32))))

(defun test-chars-negisspace-negtab ()

  (assert (equal cl:t (hydra_lib_chars_is_space 9))))

(defun test-chars-negisspace-negnewline ()

  (assert (equal cl:t (hydra_lib_chars_is_space 10))))

(defun test-chars-negisspace-negletter ()

  (assert (equal cl:nil (hydra_lib_chars_is_space 97))))

;; isUpper

(defun test-chars-negisupper-neguppercase ()

  (assert (equal cl:t (hydra_lib_chars_is_upper 65))))

(defun test-chars-negisupper-neglowercase ()

  (assert (equal cl:nil (hydra_lib_chars_is_upper 97))))

(defun test-chars-negisupper-negdigit ()

  (assert (equal cl:nil (hydra_lib_chars_is_upper 53))))

;; toLower

(defun test-chars-negtolower-neguppercase ()

  (assert (equal 97 (hydra_lib_chars_to_lower 65))))

(defun test-chars-negtolower-neglowercase ()

  (assert (equal 97 (hydra_lib_chars_to_lower 97))))

(defun test-chars-negtolower-negdigit ()

  (assert (equal 53 (hydra_lib_chars_to_lower 53))))

;; toUpper

(defun test-chars-negtoupper-neglowercase ()

  (assert (equal 65 (hydra_lib_chars_to_upper 97))))

(defun test-chars-negtoupper-neguppercase ()

  (assert (equal 65 (hydra_lib_chars_to_upper 65))))

(defun test-chars-negtoupper-negdigit ()

  (assert (equal 53 (hydra_lib_chars_to_upper 53))))
