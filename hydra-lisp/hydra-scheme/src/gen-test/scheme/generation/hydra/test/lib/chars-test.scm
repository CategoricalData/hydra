;; Note: this is an automatically generated file. Do not edit.
;; hydra.lib.chars primitives

(import (scheme base))

;; isAlphaNum

(define (test-isalphanum-negletter)

  (assert (equal? #t (hydra_lib_chars_is_alpha_num 97))))

(define (test-isalphanum-negdigit)

  (assert (equal? #t (hydra_lib_chars_is_alpha_num 53))))

(define (test-isalphanum-negspace)

  (assert (equal? #f (hydra_lib_chars_is_alpha_num 32))))

(define (test-isalphanum-negpunctuation)

  (assert (equal? #f (hydra_lib_chars_is_alpha_num 46))))

;; isLower

(define (test-islower-neglowercase)

  (assert (equal? #t (hydra_lib_chars_is_lower 97))))

(define (test-islower-neguppercase)

  (assert (equal? #f (hydra_lib_chars_is_lower 65))))

(define (test-islower-negdigit)

  (assert (equal? #f (hydra_lib_chars_is_lower 53))))

;; isSpace

(define (test-isspace-negspace)

  (assert (equal? #t (hydra_lib_chars_is_space 32))))

(define (test-isspace-negtab)

  (assert (equal? #t (hydra_lib_chars_is_space 9))))

(define (test-isspace-negnewline)

  (assert (equal? #t (hydra_lib_chars_is_space 10))))

(define (test-isspace-negletter)

  (assert (equal? #f (hydra_lib_chars_is_space 97))))

;; isUpper

(define (test-isupper-neguppercase)

  (assert (equal? #t (hydra_lib_chars_is_upper 65))))

(define (test-isupper-neglowercase)

  (assert (equal? #f (hydra_lib_chars_is_upper 97))))

(define (test-isupper-negdigit)

  (assert (equal? #f (hydra_lib_chars_is_upper 53))))

;; toLower

(define (test-tolower-neguppercase)

  (assert (equal? 97 (hydra_lib_chars_to_lower 65))))

(define (test-tolower-neglowercase)

  (assert (equal? 97 (hydra_lib_chars_to_lower 97))))

(define (test-tolower-negdigit)

  (assert (equal? 53 (hydra_lib_chars_to_lower 53))))

;; toUpper

(define (test-toupper-neglowercase)

  (assert (equal? 65 (hydra_lib_chars_to_upper 97))))

(define (test-toupper-neguppercase)

  (assert (equal? 65 (hydra_lib_chars_to_upper 65))))

(define (test-toupper-negdigit)

  (assert (equal? 53 (hydra_lib_chars_to_upper 53))))
