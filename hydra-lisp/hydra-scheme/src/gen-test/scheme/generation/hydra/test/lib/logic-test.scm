;; Note: this is an automatically generated file. Do not edit.
;; hydra.lib.logic primitives

(import (scheme base))

;; and

(define (test-logic-negand-negtrue-and-true)

  (assert (equal? true true)))

(define (test-logic-negand-negtrue-and-false)

  (assert (equal? false false)))

(define (test-logic-negand-negfalse-and-true)

  (assert (equal? false false)))

(define (test-logic-negand-negfalse-and-false)

  (assert (equal? false false)))

;; ifElse

;; boolean values

(define (test-logic-negifelse-negboolean-values-negtrue-condition-returns-then)

  (assert (equal? true true)))

(define (test-logic-negifelse-negboolean-values-negfalse-condition-returns-else)

  (assert (equal? false false)))

;; integer values

(define (test-logic-negifelse-neginteger-values-negtrue-selects-first-int)

  (assert (equal? 42:int32 42:int32)))

(define (test-logic-negifelse-neginteger-values-negfalse-selects-second-int)

  (assert (equal? 0:int32 0:int32)))

;; string values

(define (test-logic-negifelse-negstring-values-negtrue-selects-first-string)

  (assert (equal? "yes" "yes")))

(define (test-logic-negifelse-negstring-values-negfalse-selects-second-string)

  (assert (equal? "no" "no")))

;; not

(define (test-logic-negnot-negnot-true)

  (assert (equal? false false)))

(define (test-logic-negnot-negnot-false)

  (assert (equal? true true)))

;; or

(define (test-logic-negor-negtrue-or-true)

  (assert (equal? true true)))

(define (test-logic-negor-negtrue-or-false)

  (assert (equal? true true)))

(define (test-logic-negor-negfalse-or-true)

  (assert (equal? true true)))

(define (test-logic-negor-negfalse-or-false)

  (assert (equal? false false)))
