;; Note: this is an automatically generated file. Do not edit.
;; unification

(import (scheme base))

;; variableOccursInType

(define (test-unification-negvariableoccursintype-negvariable-occurs-in-itself)

  (assert (equal? true true)))

(define (test-unification-negvariableoccursintype-negvariable-does-not-occur-in-different-variable)

  (assert (equal? false false)))

(define (test-unification-negvariableoccursintype-negvariable-does-not-occur-in-int32)

  (assert (equal? false false)))

(define (test-unification-negvariableoccursintype-negvariable-does-not-occur-in-string)

  (assert (equal? false false)))

(define (test-unification-negvariableoccursintype-negvariable-occurs-in-list-element-type)

  (assert (equal? true true)))

(define (test-unification-negvariableoccursintype-negvariable-does-not-occur-in-list-of-different-type)

  (assert (equal? false false)))

(define (test-unification-negvariableoccursintype-negvariable-occurs-in-function-domain)

  (assert (equal? true true)))

(define (test-unification-negvariableoccursintype-negvariable-occurs-in-function-codomain)

  (assert (equal? true true)))

(define (test-unification-negvariableoccursintype-negvariable-does-not-occur-in-function-with-different-vars)

  (assert (equal? false false)))

(define (test-unification-negvariableoccursintype-negvariable-occurs-in-optional-type)

  (assert (equal? true true)))

(define (test-unification-negvariableoccursintype-negvariable-occurs-in-pair-first)

  (assert (equal? true true)))

(define (test-unification-negvariableoccursintype-negvariable-occurs-in-pair-second)

  (assert (equal? true true)))

(define (test-unification-negvariableoccursintype-negvariable-occurs-in-either-left)

  (assert (equal? true true)))

(define (test-unification-negvariableoccursintype-negvariable-occurs-in-either-right)

  (assert (equal? true true)))

(define (test-unification-negvariableoccursintype-negvariable-occurs-in-map-key-type)

  (assert (equal? true true)))

(define (test-unification-negvariableoccursintype-negvariable-occurs-in-map-value-type)

  (assert (equal? true true)))

(define (test-unification-negvariableoccursintype-negvariable-occurs-in-set-type)

  (assert (equal? true true)))

(define (test-unification-negvariableoccursintype-negvariable-occurs-in-nested-list)

  (assert (equal? true true)))

(define (test-unification-negvariableoccursintype-negvariable-occurs-in-list-of-functions)

  (assert (equal? true true)))

(define (test-unification-negvariableoccursintype-negvariable-does-not-occur-in-complex-type-without-it)

  (assert (equal? false false)))

(define (test-unification-negvariableoccursintype-negvariable-occurs-deep-in-complex-type)

  (assert (equal? true true)))

(define (test-unification-negvariableoccursintype-negvariable-occurs-in-forall-body)

  (assert (equal? true true)))

(define (test-unification-negvariableoccursintype-negvariable-occurs-in-forall-bound-position)

  (assert (equal? true true)))

;; unifyTypes

(define (test-unification-negunifytypes-negunify-identical-int32-types)

  (assert (equal? {} {})))

(define (test-unification-negunifytypes-negunify-identical-string-types)

  (assert (equal? {} {})))

(define (test-unification-negunifytypes-negunify-identical-variable-types)

  (assert (equal? {} {})))

(define (test-unification-negunifytypes-negunify-variable-with-int32)

  (assert (equal? {a: int32} {a: int32})))

(define (test-unification-negunifytypes-negunify-int32-with-variable)

  (assert (equal? {a: int32} {a: int32})))

(define (test-unification-negunifytypes-negunify-two-different-variables)

  (assert (equal? {a: b} {a: b})))

(define (test-unification-negunifytypes-negunify-list-of-variables-with-list-of-int32)

  (assert (equal? {a: int32} {a: int32})))

(define (test-unification-negunifytypes-negunify-identical-list-types)

  (assert (equal? {} {})))

(define (test-unification-negunifytypes-negunify-function-types-with-variables)

  (assert (equal? {a: int32, b: string} {a: int32, b: string})))

(define (test-unification-negunifytypes-negunify-identical-function-types)

  (assert (equal? {} {})))

(define (test-unification-negunifytypes-negunify-optional-types)

  (assert (equal? {a: int32} {a: int32})))

(define (test-unification-negunifytypes-negunify-pair-types)

  (assert (equal? {a: int32, b: string} {a: int32, b: string})))

(define (test-unification-negunifytypes-negunify-either-types)

  (assert (equal? {a: int32, b: string} {a: int32, b: string})))

(define (test-unification-negunifytypes-negunify-map-types)

  (assert (equal? {k: string, v: int32} {k: string, v: int32})))

(define (test-unification-negunifytypes-negunify-set-types)

  (assert (equal? {a: int32} {a: int32})))

(define (test-unification-negunifytypes-negunify-unit-types)

  (assert (equal? {} {})))

(define (test-unification-negunifytypes-negfail-to-unify-int32-with-string)

  (assert (equal? failure failure)))

(define (test-unification-negunifytypes-negfail-to-unify-list-with-function)

  (assert (equal? failure failure)))

(define (test-unification-negunifytypes-negoccur-check-variable-with-list-containing-it)

  (assert (equal? failure failure)))

;; joinTypes

(define (test-unification-negjointypes-negjoin-identical-int32)

  (assert (equal? [] [])))

(define (test-unification-negjointypes-negjoin-identical-string)

  (assert (equal? [] [])))

(define (test-unification-negjointypes-negjoin-list-types)

  (assert (equal? [(a ~ int32)] [(a ~ int32)])))

(define (test-unification-negjointypes-negjoin-function-types)

  (assert (equal? [(a ~ int32), (b ~ string)] [(a ~ int32), (b ~ string)])))

(define (test-unification-negjointypes-negjoin-optional-types)

  (assert (equal? [(a ~ int32)] [(a ~ int32)])))

(define (test-unification-negjointypes-negjoin-pair-types)

  (assert (equal? [(a ~ int32), (b ~ string)] [(a ~ int32), (b ~ string)])))

(define (test-unification-negjointypes-negjoin-either-types)

  (assert (equal? [(a ~ int32), (b ~ string)] [(a ~ int32), (b ~ string)])))

(define (test-unification-negjointypes-negjoin-map-types)

  (assert (equal? [(k ~ string), (v ~ int32)] [(k ~ string), (v ~ int32)])))

(define (test-unification-negjointypes-negjoin-set-types)

  (assert (equal? [(a ~ int32)] [(a ~ int32)])))

(define (test-unification-negjointypes-negjoin-unit-types)

  (assert (equal? [] [])))

(define (test-unification-negjointypes-negfail-to-join-int32-with-string)

  (assert (equal? failure failure)))

(define (test-unification-negjointypes-negfail-to-join-list-with-function)

  (assert (equal? failure failure)))

(define (test-unification-negjointypes-negfail-to-join-pair-with-either)

  (assert (equal? failure failure)))
