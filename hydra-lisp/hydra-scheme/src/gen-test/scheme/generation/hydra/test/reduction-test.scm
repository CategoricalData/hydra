;; Note: this is an automatically generated file. Do not edit.
;; reduction

(import (scheme base))

;; beta reduction

(define (test-beta-reduction-negidentity-function-applied-to-literal)

  (assert (equal? 42 ((lambda (x) x) 42))))

(define (test-beta-reduction-negconstant-function)

  (assert (equal? 1 ((lambda (x) 1) 42))))

(define (test-beta-reduction-negnested-application)

  (assert (equal? 1 (((lambda (x) (lambda (y) x)) 1) 2))))

;; monomorphic primitives

(define (test-monomorphic-primitives-negtoupper-on-lowercase)

  (assert (equal? "HELLO" (hydra_lib_strings_to_upper "hello"))))

(define (test-monomorphic-primitives-negtoupper-on-mixed-case)

  (assert (equal? "HELLO WORLD" (hydra_lib_strings_to_upper "Hello World"))))

(define (test-monomorphic-primitives-negtoupper-on-empty-string)

  (assert (equal? "" (hydra_lib_strings_to_upper ""))))

(define (test-monomorphic-primitives-negtolower-on-uppercase)

  (assert (equal? "hello" (hydra_lib_strings_to_lower "HELLO"))))

(define (test-monomorphic-primitives-negstring-length)

  (assert (equal? 5 (hydra_lib_strings_length "hello"))))

(define (test-monomorphic-primitives-negstring-length-of-empty)

  (assert (equal? 0 (hydra_lib_strings_length ""))))

(define (test-monomorphic-primitives-negadd-two-positive-integers)

  (assert (equal? 8 ((hydra_lib_math_add 3) 5))))

(define (test-monomorphic-primitives-negadd-negative-and-positive)

  (assert (equal? -7 ((hydra_lib_math_add -10) 3))))

(define (test-monomorphic-primitives-negadd-with-zero)

  (assert (equal? 42 ((hydra_lib_math_add 0) 42))))

(define (test-monomorphic-primitives-negsubtract-integers)

  (assert (equal? 7 ((hydra_lib_math_sub 10) 3))))

(define (test-monomorphic-primitives-negmultiply-integers)

  (assert (equal? 42 ((hydra_lib_math_mul 6) 7))))

(define (test-monomorphic-primitives-negmultiply-by-zero)

  (assert (equal? 0 ((hydra_lib_math_mul 100) 0))))

(define (test-monomorphic-primitives-negdivide-integers)

  (assert (equal? 5 ((hydra_lib_math_div 20) 4))))

(define (test-monomorphic-primitives-negmodulo)

  (assert (equal? 2 ((hydra_lib_math_mod 17) 5))))

(define (test-monomorphic-primitives-negspliton-basic)

  (assert (equal? (list "a" "b" "c") ((hydra_lib_strings_split_on ",") "a,b,c"))))

(define (test-monomorphic-primitives-negcat2-strings)

  (assert (equal? "helloworld" ((hydra_lib_strings_cat2 "hello") "world"))))

;; polymorphic primitives

(define (test-polymorphic-primitives-neglength-of-integer-list)

  (assert (equal? 3 (hydra_lib_lists_length (list 1 2 3)))))

(define (test-polymorphic-primitives-neglength-of-string-list)

  (assert (equal? 2 (hydra_lib_lists_length (list "a" "b")))))

(define (test-polymorphic-primitives-neglength-of-empty-list)

  (assert (equal? 0 (hydra_lib_lists_length (list )))))

(define (test-polymorphic-primitives-neglength-of-single-element-list)

  (assert (equal? 1 (hydra_lib_lists_length (list #t)))))

(define (test-polymorphic-primitives-neghead-of-integer-list)

  (assert (equal? 10 (hydra_lib_lists_head (list 10 20 30)))))

(define (test-polymorphic-primitives-neghead-of-string-list)

  (assert (equal? "first" (hydra_lib_lists_head (list "first" "second")))))

(define (test-polymorphic-primitives-neglast-of-integer-list)

  (assert (equal? 30 (hydra_lib_lists_last (list 10 20 30)))))

(define (test-polymorphic-primitives-negconcat-two-integer-lists)

  (assert (equal? (list 1 2 3 4) ((hydra_lib_lists_concat2 (list 1 2)) (list 3 4)))))

(define (test-polymorphic-primitives-negconcat-with-empty-list)

  (assert (equal? (list 1 2) ((hydra_lib_lists_concat2 (list )) (list 1 2)))))

(define (test-polymorphic-primitives-negreverse-integer-list)

  (assert (equal? (list 3 2 1) (hydra_lib_lists_reverse (list 1 2 3)))))

(define (test-polymorphic-primitives-negreverse-empty-list)

  (assert (equal? (list ) (hydra_lib_lists_reverse (list )))))

;; nullary primitives

(define (test-nullary-primitives-negempty-set-has-size-zero)

  (assert (equal? 0 (hydra_lib_sets_size hydra_lib_sets_empty))))

;; literals as values

(define (test-literals-as-values-neginteger-literal-is-a-value)

  (assert (equal? 42 42)))

(define (test-literals-as-values-negnegative-integer-literal)

  (assert (equal? -17 -17)))

(define (test-literals-as-values-negzero-integer-literal)

  (assert (equal? 0 0)))

(define (test-literals-as-values-negstring-literal-is-a-value)

  (assert (equal? "hello" "hello")))

(define (test-literals-as-values-negempty-string-literal)

  (assert (equal? "" "")))

(define (test-literals-as-values-negstring-with-special-characters)

  (assert (equal? "hello\nworld\ttab" "hello\nworld\ttab")))

(define (test-literals-as-values-negboolean-true-is-a-value)

  (assert (equal? #t #t)))

(define (test-literals-as-values-negboolean-false-is-a-value)

  (assert (equal? #f #f)))

(define (test-literals-as-values-negfloat-literal-is-a-value)

  (assert (equal? 3.14 3.14)))

(define (test-literals-as-values-negnegative-float-literal)

  (assert (equal? -2.718 -2.718)))

(define (test-literals-as-values-negzero-float-literal)

  (assert (equal? 0.0 0.0)))

;; list reduction

(define (test-list-reduction-negempty-list-is-a-value)

  (assert (equal? (list ) (list ))))

(define (test-list-reduction-neglist-of-literals-is-a-value)

  (assert (equal? (list 1 2 3) (list 1 2 3))))

(define (test-list-reduction-neglist-with-reducible-element)

  (assert (equal? (list 42) (list ((lambda (x) x) 42)))))

;; optional reduction

(define (test-optional-reduction-negnothing-is-a-value)

  (assert (equal? nil nil)))

(define (test-optional-reduction-negjust-literal-is-a-value)

  (assert (equal? 42 42)))

(define (test-optional-reduction-negjust-with-reducible-content)

  (assert (equal? 42 ((lambda (x) x) 42))))
