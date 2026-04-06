;; Note: this is an automatically generated file. Do not edit.
;; reduction

(import (scheme base))

;; beta reduction

(define (test-reduction-negbeta-reduction-negidentity-function-applied-to-literal)

  (assert (equal? 42:int32 42:int32)))

(define (test-reduction-negbeta-reduction-negconstant-function)

  (assert (equal? 1:int32 1:int32)))

(define (test-reduction-negbeta-reduction-negnested-application)

  (assert (equal? 1:int32 1:int32)))

;; monomorphic primitives

(define (test-reduction-negmonomorphic-primitives-negtoupper-on-lowercase)

  (assert (equal? "HELLO" "HELLO")))

(define (test-reduction-negmonomorphic-primitives-negtoupper-on-mixed-case)

  (assert (equal? "HELLO WORLD" "HELLO WORLD")))

(define (test-reduction-negmonomorphic-primitives-negtoupper-on-empty-string)

  (assert (equal? "" "")))

(define (test-reduction-negmonomorphic-primitives-negtolower-on-uppercase)

  (assert (equal? "hello" "hello")))

(define (test-reduction-negmonomorphic-primitives-negstring-length)

  (assert (equal? 5:int32 5:int32)))

(define (test-reduction-negmonomorphic-primitives-negstring-length-of-empty)

  (assert (equal? 0:int32 0:int32)))

(define (test-reduction-negmonomorphic-primitives-negadd-two-positive-integers)

  (assert (equal? 8:int32 8:int32)))

(define (test-reduction-negmonomorphic-primitives-negadd-negative-and-positive)

  (assert (equal? -7:int32 -7:int32)))

(define (test-reduction-negmonomorphic-primitives-negadd-with-zero)

  (assert (equal? 42:int32 42:int32)))

(define (test-reduction-negmonomorphic-primitives-negsubtract-integers)

  (assert (equal? 7:int32 7:int32)))

(define (test-reduction-negmonomorphic-primitives-negmultiply-integers)

  (assert (equal? 42:int32 42:int32)))

(define (test-reduction-negmonomorphic-primitives-negmultiply-by-zero)

  (assert (equal? 0:int32 0:int32)))

(define (test-reduction-negmonomorphic-primitives-negdivide-integers)

  (assert (equal? 5:int32 5:int32)))

(define (test-reduction-negmonomorphic-primitives-negmodulo)

  (assert (equal? 2:int32 2:int32)))

(define (test-reduction-negmonomorphic-primitives-negspliton-basic)

  (assert (equal? ["a", "b", "c"] ["a", "b", "c"])))

(define (test-reduction-negmonomorphic-primitives-negcat2-strings)

  (assert (equal? "helloworld" "helloworld")))

;; polymorphic primitives

(define (test-reduction-negpolymorphic-primitives-neglength-of-integer-list)

  (assert (equal? 3:int32 3:int32)))

(define (test-reduction-negpolymorphic-primitives-neglength-of-string-list)

  (assert (equal? 2:int32 2:int32)))

(define (test-reduction-negpolymorphic-primitives-neglength-of-empty-list)

  (assert (equal? 0:int32 0:int32)))

(define (test-reduction-negpolymorphic-primitives-neglength-of-single-element-list)

  (assert (equal? 1:int32 1:int32)))

(define (test-reduction-negpolymorphic-primitives-neghead-of-integer-list)

  (assert (equal? 10:int32 10:int32)))

(define (test-reduction-negpolymorphic-primitives-neghead-of-string-list)

  (assert (equal? "first" "first")))

(define (test-reduction-negpolymorphic-primitives-neglast-of-integer-list)

  (assert (equal? 30:int32 30:int32)))

(define (test-reduction-negpolymorphic-primitives-negconcat-two-integer-lists)

  (assert (equal? [1:int32, 2:int32, 3:int32, 4:int32] [1:int32, 2:int32, 3:int32, 4:int32])))

(define (test-reduction-negpolymorphic-primitives-negconcat-with-empty-list)

  (assert (equal? [1:int32, 2:int32] [1:int32, 2:int32])))

(define (test-reduction-negpolymorphic-primitives-negreverse-integer-list)

  (assert (equal? [3:int32, 2:int32, 1:int32] [3:int32, 2:int32, 1:int32])))

(define (test-reduction-negpolymorphic-primitives-negreverse-empty-list)

  (assert (equal? [] [])))

;; nullary primitives

(define (test-reduction-negnullary-primitives-negempty-set-has-size-zero)

  (assert (equal? 0:int32 0:int32)))

;; literals as values

(define (test-reduction-negliterals-as-values-neginteger-literal-is-a-value)

  (assert (equal? 42:int32 42:int32)))

(define (test-reduction-negliterals-as-values-negnegative-integer-literal)

  (assert (equal? -17:int32 -17:int32)))

(define (test-reduction-negliterals-as-values-negzero-integer-literal)

  (assert (equal? 0:int32 0:int32)))

(define (test-reduction-negliterals-as-values-negstring-literal-is-a-value)

  (assert (equal? "hello" "hello")))

(define (test-reduction-negliterals-as-values-negempty-string-literal)

  (assert (equal? "" "")))

(define (test-reduction-negliterals-as-values-negstring-with-special-characters)

  (assert (equal? "hello\nworld\ttab" "hello\nworld\ttab")))

(define (test-reduction-negliterals-as-values-negboolean-true-is-a-value)

  (assert (equal? true true)))

(define (test-reduction-negliterals-as-values-negboolean-false-is-a-value)

  (assert (equal? false false)))

(define (test-reduction-negliterals-as-values-negfloat-literal-is-a-value)

  (assert (equal? 3.14:float64 3.14:float64)))

(define (test-reduction-negliterals-as-values-negnegative-float-literal)

  (assert (equal? -2.718:float64 -2.718:float64)))

(define (test-reduction-negliterals-as-values-negzero-float-literal)

  (assert (equal? 0.0:float64 0.0:float64)))

;; list reduction

(define (test-reduction-neglist-reduction-negempty-list-is-a-value)

  (assert (equal? [] [])))

(define (test-reduction-neglist-reduction-neglist-of-literals-is-a-value)

  (assert (equal? [1:int32, 2:int32, 3:int32] [1:int32, 2:int32, 3:int32])))

(define (test-reduction-neglist-reduction-neglist-with-reducible-element)

  (assert (equal? [42:int32] [42:int32])))

;; optional reduction

(define (test-reduction-negoptional-reduction-negnothing-is-a-value)

  (assert (equal? nothing nothing)))

(define (test-reduction-negoptional-reduction-negjust-literal-is-a-value)

  (assert (equal? just(42:int32) just(42:int32))))

(define (test-reduction-negoptional-reduction-negjust-with-reducible-content)

  (assert (equal? just(42:int32) just(42:int32))))

;; alpha conversion

(define (test-reduction-negalpha-conversion-negvariable-at-top-level)

  (assert (equal? y y)))

(define (test-reduction-negalpha-conversion-negvariable-in-list)

  (assert (equal? [42:int32, y] [42:int32, y])))

(define (test-reduction-negalpha-conversion-neglambda-with-different-variable-is-transparent)

  (assert (equal? λz.[42:int32, y, z] λz.[42:int32, y, z])))

(define (test-reduction-negalpha-conversion-neglambda-with-same-variable-is-opaque)

  (assert (equal? λx.[42:int32, x, z] λx.[42:int32, x, z])))

(define (test-reduction-negalpha-conversion-negnested-lambda-outer-variable)

  (assert (equal? λa.λb.y λa.λb.y)))

(define (test-reduction-negalpha-conversion-negnested-lambda-shadows-outer)

  (assert (equal? λx.λy.x λx.λy.x)))

(define (test-reduction-negalpha-conversion-negapplication-with-variable)

  (assert (equal? (f @ y) (f @ y))))

(define (test-reduction-negalpha-conversion-negapplication-with-both-variables-same)

  (assert (equal? (y @ y) (y @ y))))

;; type reduction

(define (test-reduction-negtype-reduction-negunit-type-unchanged)

  (assert (equal? unit unit)))

(define (test-reduction-negtype-reduction-negstring-type-unchanged)

  (assert (equal? string string)))

(define (test-reduction-negtype-reduction-negint32-type-unchanged)

  (assert (equal? int32 int32)))

(define (test-reduction-negtype-reduction-negidentity-type-applied-to-string)

  (assert (equal? (string → string) (string → string))))

(define (test-reduction-negtype-reduction-negconstant-type-ignores-argument)

  (assert (equal? int32 int32)))

(define (test-reduction-negtype-reduction-negnested-forall-first-application)

  (assert (equal? (∀y.(int32 → y)) (∀y.(int32 → y)))))

(define (test-reduction-negtype-reduction-negnested-forall-both-applications)

  (assert (equal? (int32 → string) (int32 → string))))

(define (test-reduction-negtype-reduction-neglist-type-applied)

  (assert (equal? list<int32> list<int32>)))

(define (test-reduction-negtype-reduction-negoptional-type-applied)

  (assert (equal? maybe<string> maybe<string>)))

;; etaExpandTerm

(define (test-reduction-negetaexpandterm-neginteger-literal-unchanged)

  (assert (equal? 42:int32 42:int32)))

(define (test-reduction-negetaexpandterm-negstring-list-unchanged)

  (assert (equal? ["foo", "bar"] ["foo", "bar"])))

(define (test-reduction-negetaexpandterm-negfully-applied-binary-function-unchanged)

  (assert (equal? (hydra.lib.strings.splitOn @ "foo" @ "bar") (hydra.lib.strings.splitOn @ "foo" @ "bar"))))

(define (test-reduction-negetaexpandterm-neglambda-with-fully-applied-primitive-unchanged)

  (assert (equal? λx.(hydra.lib.strings.splitOn @ "," @ x) λx.(hydra.lib.strings.splitOn @ "," @ x))))

(define (test-reduction-negetaexpandterm-neglambda-returning-constant-unchanged)

  (assert (equal? λx.42:int32 λx.42:int32)))

(define (test-reduction-negetaexpandterm-negbare-unary-primitive-unchanged)

  (assert (equal? hydra.lib.strings.toLower hydra.lib.strings.toLower)))

(define (test-reduction-negetaexpandterm-negbare-binary-primitive-unchanged)

  (assert (equal? hydra.lib.strings.splitOn hydra.lib.strings.splitOn)))

(define (test-reduction-negetaexpandterm-negpartially-applied-binary-primitive-expands-to-one-lambda)

  (assert (equal? λv1.(hydra.lib.strings.splitOn @ foo @ v1) λv1.(hydra.lib.strings.splitOn @ foo @ v1))))

(define (test-reduction-negetaexpandterm-negprojection-expands-to-lambda)

  (assert (equal? λv1.(project(Person){firstName} @ v1) λv1.(project(Person){firstName} @ v1))))

(define (test-reduction-negetaexpandterm-negpartial-application-inside-lambda-expands)

  (assert (equal? λx.λv1.(hydra.lib.strings.splitOn @ x @ v1) λx.λv1.(hydra.lib.strings.splitOn @ x @ v1))))

(define (test-reduction-negetaexpandterm-neglet-with-constant-body-unchanged)

  (assert (equal? let foo = 137:int32 in 42:int32 let foo = 137:int32 in 42:int32)))

(define (test-reduction-negetaexpandterm-neglet-with-bare-primitive-value-unchanged)

  (assert (equal? let foo = hydra.lib.strings.splitOn in foo let foo = hydra.lib.strings.splitOn in foo)))

(define (test-reduction-negetaexpandterm-negfully-applied-unary-unchanged)

  (assert (equal? (hydra.lib.strings.toLower @ "FOO") (hydra.lib.strings.toLower @ "FOO"))))

(define (test-reduction-negetaexpandterm-negpartial-application-in-list-expands)

  (assert (equal? [λx.["foo"], λv1.(hydra.lib.strings.splitOn @ "bar" @ v1)] [λx.["foo"], λv1.(hydra.lib.strings.splitOn @ "bar" @ v1)])))
