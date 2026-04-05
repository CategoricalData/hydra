;; Note: this is an automatically generated file. Do not edit.
;; reduction

;; beta reduction

(defun test-reduction-negbeta-reduction-negidentity-function-applied-to-literal ()

  (assert (equal 42:int32 42:int32)))

(defun test-reduction-negbeta-reduction-negconstant-function ()

  (assert (equal 1:int32 1:int32)))

(defun test-reduction-negbeta-reduction-negnested-application ()

  (assert (equal 1:int32 1:int32)))

;; monomorphic primitives

(defun test-reduction-negmonomorphic-primitives-negtoupper-on-lowercase ()

  (assert (equal "HELLO" "HELLO")))

(defun test-reduction-negmonomorphic-primitives-negtoupper-on-mixed-case ()

  (assert (equal "HELLO WORLD" "HELLO WORLD")))

(defun test-reduction-negmonomorphic-primitives-negtoupper-on-empty-string ()

  (assert (equal "" "")))

(defun test-reduction-negmonomorphic-primitives-negtolower-on-uppercase ()

  (assert (equal "hello" "hello")))

(defun test-reduction-negmonomorphic-primitives-negstring-length ()

  (assert (equal 5:int32 5:int32)))

(defun test-reduction-negmonomorphic-primitives-negstring-length-of-empty ()

  (assert (equal 0:int32 0:int32)))

(defun test-reduction-negmonomorphic-primitives-negadd-two-positive-integers ()

  (assert (equal 8:int32 8:int32)))

(defun test-reduction-negmonomorphic-primitives-negadd-negative-and-positive ()

  (assert (equal -7:int32 -7:int32)))

(defun test-reduction-negmonomorphic-primitives-negadd-with-zero ()

  (assert (equal 42:int32 42:int32)))

(defun test-reduction-negmonomorphic-primitives-negsubtract-integers ()

  (assert (equal 7:int32 7:int32)))

(defun test-reduction-negmonomorphic-primitives-negmultiply-integers ()

  (assert (equal 42:int32 42:int32)))

(defun test-reduction-negmonomorphic-primitives-negmultiply-by-zero ()

  (assert (equal 0:int32 0:int32)))

(defun test-reduction-negmonomorphic-primitives-negdivide-integers ()

  (assert (equal 5:int32 5:int32)))

(defun test-reduction-negmonomorphic-primitives-negmodulo ()

  (assert (equal 2:int32 2:int32)))

(defun test-reduction-negmonomorphic-primitives-negspliton-basic ()

  (assert (equal ["a", "b", "c"] ["a", "b", "c"])))

(defun test-reduction-negmonomorphic-primitives-negcat2-strings ()

  (assert (equal "helloworld" "helloworld")))

;; polymorphic primitives

(defun test-reduction-negpolymorphic-primitives-neglength-of-integer-list ()

  (assert (equal 3:int32 3:int32)))

(defun test-reduction-negpolymorphic-primitives-neglength-of-string-list ()

  (assert (equal 2:int32 2:int32)))

(defun test-reduction-negpolymorphic-primitives-neglength-of-empty-list ()

  (assert (equal 0:int32 0:int32)))

(defun test-reduction-negpolymorphic-primitives-neglength-of-single-element-list ()

  (assert (equal 1:int32 1:int32)))

(defun test-reduction-negpolymorphic-primitives-neghead-of-integer-list ()

  (assert (equal 10:int32 10:int32)))

(defun test-reduction-negpolymorphic-primitives-neghead-of-string-list ()

  (assert (equal "first" "first")))

(defun test-reduction-negpolymorphic-primitives-neglast-of-integer-list ()

  (assert (equal 30:int32 30:int32)))

(defun test-reduction-negpolymorphic-primitives-negconcat-two-integer-lists ()

  (assert (equal [1:int32, 2:int32, 3:int32, 4:int32] [1:int32, 2:int32, 3:int32, 4:int32])))

(defun test-reduction-negpolymorphic-primitives-negconcat-with-empty-list ()

  (assert (equal [1:int32, 2:int32] [1:int32, 2:int32])))

(defun test-reduction-negpolymorphic-primitives-negreverse-integer-list ()

  (assert (equal [3:int32, 2:int32, 1:int32] [3:int32, 2:int32, 1:int32])))

(defun test-reduction-negpolymorphic-primitives-negreverse-empty-list ()

  (assert (equal [] [])))

;; nullary primitives

(defun test-reduction-negnullary-primitives-negempty-set-has-size-zero ()

  (assert (equal 0:int32 0:int32)))

;; literals as values

(defun test-reduction-negliterals-as-values-neginteger-literal-is-a-value ()

  (assert (equal 42:int32 42:int32)))

(defun test-reduction-negliterals-as-values-negnegative-integer-literal ()

  (assert (equal -17:int32 -17:int32)))

(defun test-reduction-negliterals-as-values-negzero-integer-literal ()

  (assert (equal 0:int32 0:int32)))

(defun test-reduction-negliterals-as-values-negstring-literal-is-a-value ()

  (assert (equal "hello" "hello")))

(defun test-reduction-negliterals-as-values-negempty-string-literal ()

  (assert (equal "" "")))

(defun test-reduction-negliterals-as-values-negstring-with-special-characters ()

  (assert (equal "hello\nworld\ttab" "hello\nworld\ttab")))

(defun test-reduction-negliterals-as-values-negboolean-true-is-a-value ()

  (assert (equal true true)))

(defun test-reduction-negliterals-as-values-negboolean-false-is-a-value ()

  (assert (equal false false)))

(defun test-reduction-negliterals-as-values-negfloat-literal-is-a-value ()

  (assert (equal 3.14:float64 3.14:float64)))

(defun test-reduction-negliterals-as-values-negnegative-float-literal ()

  (assert (equal -2.718:float64 -2.718:float64)))

(defun test-reduction-negliterals-as-values-negzero-float-literal ()

  (assert (equal 0.0:float64 0.0:float64)))

;; list reduction

(defun test-reduction-neglist-reduction-negempty-list-is-a-value ()

  (assert (equal [] [])))

(defun test-reduction-neglist-reduction-neglist-of-literals-is-a-value ()

  (assert (equal [1:int32, 2:int32, 3:int32] [1:int32, 2:int32, 3:int32])))

(defun test-reduction-neglist-reduction-neglist-with-reducible-element ()

  (assert (equal [42:int32] [42:int32])))

;; optional reduction

(defun test-reduction-negoptional-reduction-negnothing-is-a-value ()

  (assert (equal nothing nothing)))

(defun test-reduction-negoptional-reduction-negjust-literal-is-a-value ()

  (assert (equal just(42:int32) just(42:int32))))

(defun test-reduction-negoptional-reduction-negjust-with-reducible-content ()

  (assert (equal just(42:int32) just(42:int32))))

;; alpha conversion

(defun test-reduction-negalpha-conversion-negvariable-at-top-level ()

  (assert (equal y y)))

(defun test-reduction-negalpha-conversion-negvariable-in-list ()

  (assert (equal [42:int32, y] [42:int32, y])))

(defun test-reduction-negalpha-conversion-neglambda-with-different-variable-is-transparent ()

  (assert (equal λz.[42:int32, y, z] λz.[42:int32, y, z])))

(defun test-reduction-negalpha-conversion-neglambda-with-same-variable-is-opaque ()

  (assert (equal λx.[42:int32, x, z] λx.[42:int32, x, z])))

(defun test-reduction-negalpha-conversion-negnested-lambda-outer-variable ()

  (assert (equal λa.λb.y λa.λb.y)))

(defun test-reduction-negalpha-conversion-negnested-lambda-shadows-outer ()

  (assert (equal λx.λy.x λx.λy.x)))

(defun test-reduction-negalpha-conversion-negapplication-with-variable ()

  (assert (equal (f @ y) (f @ y))))

(defun test-reduction-negalpha-conversion-negapplication-with-both-variables-same ()

  (assert (equal (y @ y) (y @ y))))

;; type reduction

(defun test-reduction-negtype-reduction-negunit-type-unchanged ()

  (assert (equal unit unit)))

(defun test-reduction-negtype-reduction-negstring-type-unchanged ()

  (assert (equal string string)))

(defun test-reduction-negtype-reduction-negint32-type-unchanged ()

  (assert (equal int32 int32)))

(defun test-reduction-negtype-reduction-negidentity-type-applied-to-string ()

  (assert (equal (string → string) (string → string))))

(defun test-reduction-negtype-reduction-negconstant-type-ignores-argument ()

  (assert (equal int32 int32)))

(defun test-reduction-negtype-reduction-negnested-forall-first-application ()

  (assert (equal (∀y.(int32 → y)) (∀y.(int32 → y)))))

(defun test-reduction-negtype-reduction-negnested-forall-both-applications ()

  (assert (equal (int32 → string) (int32 → string))))

(defun test-reduction-negtype-reduction-neglist-type-applied ()

  (assert (equal list<int32> list<int32>)))

(defun test-reduction-negtype-reduction-negoptional-type-applied ()

  (assert (equal maybe<string> maybe<string>)))

;; etaExpandTerm

(defun test-reduction-negetaexpandterm-neginteger-literal-unchanged ()

  (assert (equal 42:int32 42:int32)))

(defun test-reduction-negetaexpandterm-negstring-list-unchanged ()

  (assert (equal ["foo", "bar"] ["foo", "bar"])))

(defun test-reduction-negetaexpandterm-negfully-applied-binary-function-unchanged ()

  (assert (equal (hydra.lib.strings.splitOn @ "foo" @ "bar") (hydra.lib.strings.splitOn @ "foo" @ "bar"))))

(defun test-reduction-negetaexpandterm-neglambda-with-fully-applied-primitive-unchanged ()

  (assert (equal λx.(hydra.lib.strings.splitOn @ "," @ x) λx.(hydra.lib.strings.splitOn @ "," @ x))))

(defun test-reduction-negetaexpandterm-neglambda-returning-constant-unchanged ()

  (assert (equal λx.42:int32 λx.42:int32)))

(defun test-reduction-negetaexpandterm-negbare-unary-primitive-unchanged ()

  (assert (equal hydra.lib.strings.toLower hydra.lib.strings.toLower)))

(defun test-reduction-negetaexpandterm-negbare-binary-primitive-unchanged ()

  (assert (equal hydra.lib.strings.splitOn hydra.lib.strings.splitOn)))

(defun test-reduction-negetaexpandterm-negpartially-applied-binary-primitive-expands-to-one-lambda ()

  (assert (equal λv1.(hydra.lib.strings.splitOn @ foo @ v1) λv1.(hydra.lib.strings.splitOn @ foo @ v1))))

(defun test-reduction-negetaexpandterm-negprojection-expands-to-lambda ()

  (assert (equal λv1.(project(Person){firstName} @ v1) λv1.(project(Person){firstName} @ v1))))

(defun test-reduction-negetaexpandterm-negpartial-application-inside-lambda-expands ()

  (assert (equal λx.λv1.(hydra.lib.strings.splitOn @ x @ v1) λx.λv1.(hydra.lib.strings.splitOn @ x @ v1))))

(defun test-reduction-negetaexpandterm-neglet-with-constant-body-unchanged ()

  (assert (equal let foo = 137:int32 in 42:int32 let foo = 137:int32 in 42:int32)))

(defun test-reduction-negetaexpandterm-neglet-with-bare-primitive-value-unchanged ()

  (assert (equal let foo = hydra.lib.strings.splitOn in foo let foo = hydra.lib.strings.splitOn in foo)))

(defun test-reduction-negetaexpandterm-negfully-applied-unary-unchanged ()

  (assert (equal (hydra.lib.strings.toLower @ "FOO") (hydra.lib.strings.toLower @ "FOO"))))

(defun test-reduction-negetaexpandterm-negpartial-application-in-list-expands ()

  (assert (equal [λx.["foo"], λv1.(hydra.lib.strings.splitOn @ "bar" @ v1)] [λx.["foo"], λv1.(hydra.lib.strings.splitOn @ "bar" @ v1)])))
