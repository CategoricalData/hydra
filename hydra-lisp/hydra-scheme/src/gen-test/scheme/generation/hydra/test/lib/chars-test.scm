;; Note: this is an automatically generated file. Do not edit.
;; hydra.lib.chars primitives

(import (scheme base))

;; isAlphaNum

(define (test-chars-negisalphanum-negletter)

  (assert (equal? #t (hydra_lib_chars_is_alpha_num 97))))

(define (test-chars-negisalphanum-negdigit)

  (assert (equal? #t (hydra_lib_chars_is_alpha_num 53))))

(define (test-chars-negisalphanum-negspace)

  (assert (equal? #f (hydra_lib_chars_is_alpha_num 32))))

(define (test-chars-negisalphanum-negpunctuation)

  (assert (equal? #f (hydra_lib_chars_is_alpha_num 46))))

;; isLower

(define (test-chars-negislower-neglowercase)

  (assert (equal? #t (hydra_lib_chars_is_lower 97))))

(define (test-chars-negislower-neguppercase)

  (assert (equal? #f (hydra_lib_chars_is_lower 65))))

(define (test-chars-negislower-negdigit)

  (assert (equal? #f (hydra_lib_chars_is_lower 53))))

;; isSpace

(define (test-chars-negisspace-negspace)

  (assert (equal? #t (hydra_lib_chars_is_space 32))))

(define (test-chars-negisspace-negtab)

  (assert (equal? #t (hydra_lib_chars_is_space 9))))

(define (test-chars-negisspace-negnewline)

  (assert (equal? #t (hydra_lib_chars_is_space 10))))

(define (test-chars-negisspace-negletter)

  (assert (equal? #f (hydra_lib_chars_is_space 97))))

;; isUpper

(define (test-chars-negisupper-neguppercase)

  (assert (equal? #t (hydra_lib_chars_is_upper 65))))

(define (test-chars-negisupper-neglowercase)

  (assert (equal? #f (hydra_lib_chars_is_upper 97))))

(define (test-chars-negisupper-negdigit)

  (assert (equal? #f (hydra_lib_chars_is_upper 53))))

;; toLower

(define (test-chars-negtolower-neguppercase)

  (assert (equal? 97 (hydra_lib_chars_to_lower 65))))

(define (test-chars-negtolower-neglowercase)

  (assert (equal? 97 (hydra_lib_chars_to_lower 97))))

(define (test-chars-negtolower-negdigit)

  (assert (equal? 53 (hydra_lib_chars_to_lower 53))))

;; toUpper

(define (test-chars-negtoupper-neglowercase)

  (assert (equal? 65 (hydra_lib_chars_to_upper 97))))

(define (test-chars-negtoupper-neguppercase)

  (assert (equal? 65 (hydra_lib_chars_to_upper 65))))

(define (test-chars-negtoupper-negdigit)

  (assert (equal? 53 (hydra_lib_chars_to_upper 53))))
