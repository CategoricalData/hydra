;; Note: this is an automatically generated file. Do not edit.
;; hydra.lib.logic primitives

(import (scheme base))

;; and

(define (test-and-negtrue-and-true)

  (assert (equal? #t ((hydra_lib_logic_and #t) #t))))

(define (test-and-negtrue-and-false)

  (assert (equal? #f ((hydra_lib_logic_and #t) #f))))

(define (test-and-negfalse-and-true)

  (assert (equal? #f ((hydra_lib_logic_and #f) #t))))

(define (test-and-negfalse-and-false)

  (assert (equal? #f ((hydra_lib_logic_and #f) #f))))

;; ifElse

;; boolean values

(define (test-ifelse-negboolean-values-negtrue-condition-returns-then)

  (assert (equal? #t (((hydra_lib_logic_if_else #t) #t) #f))))

(define (test-ifelse-negboolean-values-negfalse-condition-returns-else)

  (assert (equal? #f (((hydra_lib_logic_if_else #f) #t) #f))))

;; integer values

(define (test-ifelse-neginteger-values-negtrue-selects-first-int)

  (assert (equal? 42 (((hydra_lib_logic_if_else #t) 42) 0))))

(define (test-ifelse-neginteger-values-negfalse-selects-second-int)

  (assert (equal? 0 (((hydra_lib_logic_if_else #f) 42) 0))))

;; string values

(define (test-ifelse-negstring-values-negtrue-selects-first-string)

  (assert (equal? "yes" (((hydra_lib_logic_if_else #t) "yes") "no"))))

(define (test-ifelse-negstring-values-negfalse-selects-second-string)

  (assert (equal? "no" (((hydra_lib_logic_if_else #f) "yes") "no"))))

;; not

(define (test-not-negnot-true)

  (assert (equal? #f (hydra_lib_logic_not #t))))

(define (test-not-negnot-false)

  (assert (equal? #t (hydra_lib_logic_not #f))))

;; or

(define (test-or-negtrue-or-true)

  (assert (equal? #t ((hydra_lib_logic_or #t) #t))))

(define (test-or-negtrue-or-false)

  (assert (equal? #t ((hydra_lib_logic_or #t) #f))))

(define (test-or-negfalse-or-true)

  (assert (equal? #t ((hydra_lib_logic_or #f) #t))))

(define (test-or-negfalse-or-false)

  (assert (equal? #f ((hydra_lib_logic_or #f) #f))))
