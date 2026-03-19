;; Note: this is an automatically generated file. Do not edit.
;; reduction

(import (scheme base))

;; beta reduction

(define (test-reduction-negbeta-reduction-negidentity-function-applied-to-literal)

  (assert (equal? 42 ((lambda (x) x) 42))))

(define (test-reduction-negbeta-reduction-negconstant-function)

  (assert (equal? 1 ((lambda (x) 1) 42))))

(define (test-reduction-negbeta-reduction-negnested-application)

  (assert (equal? 1 (((lambda (x) (lambda (y) x)) 1) 2))))

;; monomorphic primitives

(define (test-reduction-negmonomorphic-primitives-negtoupper-on-lowercase)

  (assert (equal? "HELLO" (hydra_lib_strings_to_upper "hello"))))

(define (test-reduction-negmonomorphic-primitives-negtoupper-on-mixed-case)

  (assert (equal? "HELLO WORLD" (hydra_lib_strings_to_upper "Hello World"))))

(define (test-reduction-negmonomorphic-primitives-negtoupper-on-empty-string)

  (assert (equal? "" (hydra_lib_strings_to_upper ""))))

(define (test-reduction-negmonomorphic-primitives-negtolower-on-uppercase)

  (assert (equal? "hello" (hydra_lib_strings_to_lower "HELLO"))))

(define (test-reduction-negmonomorphic-primitives-negstring-length)

  (assert (equal? 5 (hydra_lib_strings_length "hello"))))

(define (test-reduction-negmonomorphic-primitives-negstring-length-of-empty)

  (assert (equal? 0 (hydra_lib_strings_length ""))))

(define (test-reduction-negmonomorphic-primitives-negadd-two-positive-integers)

  (assert (equal? 8 ((hydra_lib_math_add 3) 5))))

(define (test-reduction-negmonomorphic-primitives-negadd-negative-and-positive)

  (assert (equal? -7 ((hydra_lib_math_add -10) 3))))

(define (test-reduction-negmonomorphic-primitives-negadd-with-zero)

  (assert (equal? 42 ((hydra_lib_math_add 0) 42))))

(define (test-reduction-negmonomorphic-primitives-negsubtract-integers)

  (assert (equal? 7 ((hydra_lib_math_sub 10) 3))))

(define (test-reduction-negmonomorphic-primitives-negmultiply-integers)

  (assert (equal? 42 ((hydra_lib_math_mul 6) 7))))

(define (test-reduction-negmonomorphic-primitives-negmultiply-by-zero)

  (assert (equal? 0 ((hydra_lib_math_mul 100) 0))))

(define (test-reduction-negmonomorphic-primitives-negdivide-integers)

  (assert (equal? 5 ((hydra_lib_math_div 20) 4))))

(define (test-reduction-negmonomorphic-primitives-negmodulo)

  (assert (equal? 2 ((hydra_lib_math_mod 17) 5))))

(define (test-reduction-negmonomorphic-primitives-negspliton-basic)

  (assert (equal? (list "a" "b" "c") ((hydra_lib_strings_split_on ",") "a,b,c"))))

(define (test-reduction-negmonomorphic-primitives-negcat2-strings)

  (assert (equal? "helloworld" ((hydra_lib_strings_cat2 "hello") "world"))))

;; polymorphic primitives

(define (test-reduction-negpolymorphic-primitives-neglength-of-integer-list)

  (assert (equal? 3 (hydra_lib_lists_length (list 1 2 3)))))

(define (test-reduction-negpolymorphic-primitives-neglength-of-string-list)

  (assert (equal? 2 (hydra_lib_lists_length (list "a" "b")))))

(define (test-reduction-negpolymorphic-primitives-neglength-of-empty-list)

  (assert (equal? 0 (hydra_lib_lists_length (list )))))

(define (test-reduction-negpolymorphic-primitives-neglength-of-single-element-list)

  (assert (equal? 1 (hydra_lib_lists_length (list #t)))))

(define (test-reduction-negpolymorphic-primitives-neghead-of-integer-list)

  (assert (equal? 10 (hydra_lib_lists_head (list 10 20 30)))))

(define (test-reduction-negpolymorphic-primitives-neghead-of-string-list)

  (assert (equal? "first" (hydra_lib_lists_head (list "first" "second")))))

(define (test-reduction-negpolymorphic-primitives-neglast-of-integer-list)

  (assert (equal? 30 (hydra_lib_lists_last (list 10 20 30)))))

(define (test-reduction-negpolymorphic-primitives-negconcat-two-integer-lists)

  (assert (equal? (list 1 2 3 4) ((hydra_lib_lists_concat2 (list 1 2)) (list 3 4)))))

(define (test-reduction-negpolymorphic-primitives-negconcat-with-empty-list)

  (assert (equal? (list 1 2) ((hydra_lib_lists_concat2 (list )) (list 1 2)))))

(define (test-reduction-negpolymorphic-primitives-negreverse-integer-list)

  (assert (equal? (list 3 2 1) (hydra_lib_lists_reverse (list 1 2 3)))))

(define (test-reduction-negpolymorphic-primitives-negreverse-empty-list)

  (assert (equal? (list ) (hydra_lib_lists_reverse (list )))))

;; nullary primitives

(define (test-reduction-negnullary-primitives-negempty-set-has-size-zero)

  (assert (equal? 0 (hydra_lib_sets_size hydra_lib_sets_empty))))

;; literals as values

(define (test-reduction-negliterals-as-values-neginteger-literal-is-a-value)

  (assert (equal? 42 42)))

(define (test-reduction-negliterals-as-values-negnegative-integer-literal)

  (assert (equal? -17 -17)))

(define (test-reduction-negliterals-as-values-negzero-integer-literal)

  (assert (equal? 0 0)))

(define (test-reduction-negliterals-as-values-negstring-literal-is-a-value)

  (assert (equal? "hello" "hello")))

(define (test-reduction-negliterals-as-values-negempty-string-literal)

  (assert (equal? "" "")))

(define (test-reduction-negliterals-as-values-negstring-with-special-characters)

  (assert (equal? "hello\nworld\ttab" "hello\nworld\ttab")))

(define (test-reduction-negliterals-as-values-negboolean-true-is-a-value)

  (assert (equal? #t #t)))

(define (test-reduction-negliterals-as-values-negboolean-false-is-a-value)

  (assert (equal? #f #f)))

(define (test-reduction-negliterals-as-values-negfloat-literal-is-a-value)

  (assert (equal? 3.14 3.14)))

(define (test-reduction-negliterals-as-values-negnegative-float-literal)

  (assert (equal? -2.718 -2.718)))

(define (test-reduction-negliterals-as-values-negzero-float-literal)

  (assert (equal? 0.0 0.0)))

;; list reduction

(define (test-reduction-neglist-reduction-negempty-list-is-a-value)

  (assert (equal? (list ) (list ))))

(define (test-reduction-neglist-reduction-neglist-of-literals-is-a-value)

  (assert (equal? (list 1 2 3) (list 1 2 3))))

(define (test-reduction-neglist-reduction-neglist-with-reducible-element)

  (assert (equal? (list 42) (list ((lambda (x) x) 42)))))

;; optional reduction

(define (test-reduction-negoptional-reduction-negnothing-is-a-value)

  (assert (equal? (list 'nothing) (list 'nothing))))

(define (test-reduction-negoptional-reduction-negjust-literal-is-a-value)

  (assert (equal? (list 'just 42) (list 'just 42))))

(define (test-reduction-negoptional-reduction-negjust-with-reducible-content)

  (assert (equal? (list 'just 42) (list 'just ((lambda (x) x) 42)))))
