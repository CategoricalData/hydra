;; Note: this is an automatically generated file. Do not edit.
;; strip

(import (scheme base))

;; deannotateTerm

(define (test-strip-negdeannotateterm-negunannotated-literal-unchanged)

  (assert (equal? 42:int32 42:int32)))

(define (test-strip-negdeannotateterm-negunannotated-variable-unchanged)

  (assert (equal? x x)))

(define (test-strip-negdeannotateterm-negunannotated-lambda-unchanged)

  (assert (equal? λx.x λx.x)))

(define (test-strip-negdeannotateterm-negsingle-annotation-stripped)

  (assert (equal? 42:int32 42:int32)))

(define (test-strip-negdeannotateterm-negnested-annotations-stripped)

  (assert (equal? 42:int32 42:int32)))

(define (test-strip-negdeannotateterm-negannotated-lambda-stripped)

  (assert (equal? λx.x λx.x)))

(define (test-strip-negdeannotateterm-negannotated-application-stripped)

  (assert (equal? (f @ x) (f @ x))))

;; deannotateType

(define (test-strip-negdeannotatetype-negunannotated-primitive-type-unchanged)

  (assert (equal? int32 int32)))

(define (test-strip-negdeannotatetype-negunannotated-string-type-unchanged)

  (assert (equal? string string)))

(define (test-strip-negdeannotatetype-negunannotated-function-type-unchanged)

  (assert (equal? (int32 → string) (int32 → string))))

(define (test-strip-negdeannotatetype-negsingle-annotation-stripped)

  (assert (equal? int32 int32)))

(define (test-strip-negdeannotatetype-negnested-annotations-stripped)

  (assert (equal? string string)))

(define (test-strip-negdeannotatetype-negannotated-list-type-stripped)

  (assert (equal? list<int32> list<int32>)))

(define (test-strip-negdeannotatetype-negannotated-function-type-stripped)

  (assert (equal? (int32 → string) (int32 → string))))
