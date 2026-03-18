;; Note: this is an automatically generated file. Do not edit.
;; hydra.lib.chars primitives

;; isAlphaNum

(defun test-isalphanum-negletter ()

  (assert (equal cl:t (hydra_lib_chars_is_alpha_num 97))))

(defun test-isalphanum-negdigit ()

  (assert (equal cl:t (hydra_lib_chars_is_alpha_num 53))))

(defun test-isalphanum-negspace ()

  (assert (equal cl:nil (hydra_lib_chars_is_alpha_num 32))))

(defun test-isalphanum-negpunctuation ()

  (assert (equal cl:nil (hydra_lib_chars_is_alpha_num 46))))

;; isLower

(defun test-islower-neglowercase ()

  (assert (equal cl:t (hydra_lib_chars_is_lower 97))))

(defun test-islower-neguppercase ()

  (assert (equal cl:nil (hydra_lib_chars_is_lower 65))))

(defun test-islower-negdigit ()

  (assert (equal cl:nil (hydra_lib_chars_is_lower 53))))

;; isSpace

(defun test-isspace-negspace ()

  (assert (equal cl:t (hydra_lib_chars_is_space 32))))

(defun test-isspace-negtab ()

  (assert (equal cl:t (hydra_lib_chars_is_space 9))))

(defun test-isspace-negnewline ()

  (assert (equal cl:t (hydra_lib_chars_is_space 10))))

(defun test-isspace-negletter ()

  (assert (equal cl:nil (hydra_lib_chars_is_space 97))))

;; isUpper

(defun test-isupper-neguppercase ()

  (assert (equal cl:t (hydra_lib_chars_is_upper 65))))

(defun test-isupper-neglowercase ()

  (assert (equal cl:nil (hydra_lib_chars_is_upper 97))))

(defun test-isupper-negdigit ()

  (assert (equal cl:nil (hydra_lib_chars_is_upper 53))))

;; toLower

(defun test-tolower-neguppercase ()

  (assert (equal 97 (hydra_lib_chars_to_lower 65))))

(defun test-tolower-neglowercase ()

  (assert (equal 97 (hydra_lib_chars_to_lower 97))))

(defun test-tolower-negdigit ()

  (assert (equal 53 (hydra_lib_chars_to_lower 53))))

;; toUpper

(defun test-toupper-neglowercase ()

  (assert (equal 65 (hydra_lib_chars_to_upper 97))))

(defun test-toupper-neguppercase ()

  (assert (equal 65 (hydra_lib_chars_to_upper 65))))

(defun test-toupper-negdigit ()

  (assert (equal 53 (hydra_lib_chars_to_upper 53))))
