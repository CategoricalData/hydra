;;; Note: this is an automatically generated file. Do not edit. -*- lexical-binding: t; coding: utf-8 -*-
;;; inference

(require 'ert)

;; Algebraic terms

;; Collection primitives

;; maps.map applied to a function



;; sets.map applied to a function



;; Composing collection primitives in let



;; Map operations in lambdas



;; Fully applied collection conversions



;; Either terms

;; Left values



;; Right values



;; Polymorphic either values



;; Nested either values



;; Either in lambda



;; Either in data structures



;; Eliminations

;; List eliminations (folds)



;; Optional eliminations



;; List terms

;; List of strings



;; List of lists of strings



;; Empty list



;; List containing an empty list



;; Lambda producing a polymorphic list



;; Lambda producing a list of integers



;; List with repeated variables



;; Map terms



;; Optional terms



;; Pair terms

;; Monotyped pairs



;; Polytyped pairs



;; Nested pairs



;; Pairs in lambda



;; Pairs in data structures



;; Additional cases



;; Set terms



;; Algorithm W test cases

;; STLC to System F



;; Type classes

;; Monomorphic (constraints vanish)

;; Map operations with concrete types



;; Set operations with concrete types



;; Equality operations with concrete types



;; List operations with concrete types



;; Primitive references with constraints

;; Map primitives (ordering on key type)



;; Set primitives (ordering on element type)



;; Equality primitives



;; List primitives with constraints



;; Partial application preserving constraints

;; Map partial application



;; Set partial application



;; Equality partial application



;; Partial application fixing the constrained variable



;; Let binding constraint propagation

;; Simple let-bound wrappers



;; Let-bound with partial instantiation



;; Multiple uses of a constrained let binding



;; Composition and constraint merging

;; Composing constrained primitives



;; Composing map and sort



;; Nested containers

;; Maps of sets



;; Sets of sets



;; Map from sorted list



;; Collection term constraints

;; Set literals



;; Map literals



;; Collection terms with primitives



;; Constraint propagation through collection elements



;; Expected failures

;; Undefined variable

;; Basic unbound variables



;; Unbound in let expressions



;; Shadowing scope errors



;; Unification failure

;; Basic type mismatches



;; Collection type mismatches



;; Conditional type mismatches



;; Polymorphic instantiation conflicts



;; Invalid application

;; Non-function application



;; Collection application



;; Primitive misapplication



;; Self-application

;; Direct self-application



;; Indirect self-application



;; Arity mismatch

;; Too many arguments



;; Wrong argument types with extra args



;; Recursive type construction

;; Direct recursive types



;; Recursive function types



;; Mutually recursive types



;; Occur check failures

;; Function occur checks



;; Mutual occur checks



;; Complex occur checks



;; Type constructor misuse

;; List constructor errors



;; String constructor errors



;; Math constructor errors



;; Polymorphism violations

;; Identity function violations



;; Constrained polymorphism violations



;; Higher-order polymorphism violations



;; Let binding type mismatches

;; Application type mismatches



;; Collection type mismatches



;; Function binding mismatches



;; Constraint solver edge cases

;; Complex constraint propagation



;; Fixed point combinators



;; Constraint cycles



;; Primitive function type errors

;; Logic primitive errors



;; Collection primitive errors



;; Math primitive errors



;; Complex constraint failures

;; Multi-level constraint conflicts



;; Function composition failures



;; Fundamentals

;; Lambdas

;; Simple lambdas



;; Nested lambdas



;; Nested lambdas with shadowing



;; Let terms

;; Simple



;; Empty let



;; Trivial let



;; Multiple references to a let-bound term



;; Nested let



;; Nested let with shadowing



;; Let-polymorphism



;; Recursive and mutually recursive let (@wisnesky's test cases)



;; Recursive and mutually recursive let with polymorphism



;; Recursion involving polymorphic functions



;; Over-generalization of hoisted let-bindings



;; Literals



;; Pathological terms

;; Recursion



;; Infinite lists



;; Polymorphism

;; Simple lists and optionals



;; Lambdas, lists, and products



;; Lambdas and application



;; Primitives and application



;; Lambdas and primitives



;; Mixed expressions with lambdas, constants, and primitive functions



;; Application terms



;; Primitives

;; Monomorphic primitive functions



;; Polymorphic primitive functions



;; Examples from the Hydra kernel

;; Nested let

;; hydra.formatting.mapFirstLetter



;; Recursive let with pair return (ifElse)



;; Recursive let with pair return (case on Type)



;; Nominal terms

;; Case statements



;; Projections

;; Record eliminations



;; Pair projections



;; Records

;; Simple records



;; Record instances of simply recursive record types



;; Record instances of mutually recursive record types



;; Variant terms

;; Variants



;; Polymorphic and recursive variants



;; Wrapper introductions and eliminations

;; Wrapper introductions



;; Wrapper eliminations


