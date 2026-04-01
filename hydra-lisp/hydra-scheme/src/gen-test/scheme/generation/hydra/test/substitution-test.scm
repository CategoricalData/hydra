;; Note: this is an automatically generated file. Do not edit.
;; substitution

(import (scheme base))

;; substInType

(define (test-substitution-negsubstintype-negempty-substitution-returns-type-unchanged)

  (assert (equal? string string)))

(define (test-substitution-negsubstintype-negsubstitute-type-variable-with-int32)

  (assert (equal? int32 int32)))

(define (test-substitution-negsubstintype-negnon-negmatching-variable-unchanged)

  (assert (equal? b b)))

(define (test-substitution-negsubstintype-negsubstitute-in-function-domain)

  (assert (equal? (int32 → string) (int32 → string))))

(define (test-substitution-negsubstintype-negsubstitute-in-function-codomain)

  (assert (equal? (int32 → string) (int32 → string))))

(define (test-substitution-negsubstintype-negsubstitute-in-list-element-type)

  (assert (equal? list<int32> list<int32>)))

(define (test-substitution-negsubstintype-negsubstitute-in-optional-type)

  (assert (equal? maybe<string> maybe<string>)))

(define (test-substitution-negsubstintype-negsubstitute-in-pair-type-both-sides)

  (assert (equal? (int32, int32) (int32, int32))))

(define (test-substitution-negsubstintype-negsubstitute-in-either-type)

  (assert (equal? either<string, int32> either<string, int32>)))

(define (test-substitution-negsubstintype-negsubstitute-in-map-key-type)

  (assert (equal? map<string, int32> map<string, int32>)))

(define (test-substitution-negsubstintype-negsubstitute-in-set-type)

  (assert (equal? set<int32> set<int32>)))

(define (test-substitution-negsubstintype-negnested-substitution-in-list-of-pairs)

  (assert (equal? list<(int32, string)> list<(int32, string)>)))

(define (test-substitution-negsubstintype-negmultiple-substitutions)

  (assert (equal? (int32, string) (int32, string))))

(define (test-substitution-negsubstintype-negforall-bound-variable-not-substituted)

  (assert (equal? (∀a.(a → a)) (∀a.(a → a)))))

(define (test-substitution-negsubstintype-negforall-free-variable-substituted)

  (assert (equal? (∀a.(a → string)) (∀a.(a → string)))))
