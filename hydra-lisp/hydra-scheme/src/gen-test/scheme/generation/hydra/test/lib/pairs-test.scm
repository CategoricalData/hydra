;; Note: this is an automatically generated file. Do not edit.
;; hydra.lib.pairs primitives

(import (scheme base))

;; bimap

(define (test-bimap-negtransform-both-elements)

  (assert (equal? (list 10 2) (((hydra_lib_pairs_bimap (lambda (x) ((hydra_lib_math_mul x) 2))) (lambda (s) (hydra_lib_strings_length s))) (list 5 "ab")))))

(define (test-bimap-negwith-zero)

  (assert (equal? (list 0 5) (((hydra_lib_pairs_bimap (lambda (x) ((hydra_lib_math_mul x) 2))) (lambda (s) (hydra_lib_strings_length s))) (list 0 "hello")))))

;; first

(define (test-first-negextract-first-element)

  (assert (equal? 42 (hydra_lib_pairs_first (list 42 "hello")))))

(define (test-first-negwith-zero)

  (assert (equal? 0 (hydra_lib_pairs_first (list 0 "world")))))

(define (test-first-negnegative-number)

  (assert (equal? -5 (hydra_lib_pairs_first (list -5 "test")))))

;; second

(define (test-second-negextract-second-element)

  (assert (equal? "hello" (hydra_lib_pairs_second (list 42 "hello")))))

(define (test-second-negempty-string)

  (assert (equal? "" (hydra_lib_pairs_second (list 0 "")))))

(define (test-second-neglong-string)

  (assert (equal? "testing" (hydra_lib_pairs_second (list 123 "testing")))))
