// Note: this is an automatically generated file. Do not edit.
// eta expansion

package generation.hydra.test

import org.scalatest.funsuite.AnyFunSuite

class EtaExpansionTest extends AnyFunSuite {

  // Partial application of primitives

  // Bare primitives are not expanded

  test("Partial application of primitives - Bare primitives are not expanded - unary primitive") {

    assert((

      hydra.lib.strings.toLower!) == (

      hydra.lib.strings.toLower!))

  }

  test("Partial application of primitives - Bare primitives are not expanded - binary primitive") {

    assert((

      hydra.lib.strings.splitOn!) == (

      hydra.lib.strings.splitOn!))

  }

  // Partially applied primitives expand with lambdas

  test("Partial application of primitives - Partially applied primitives expand with lambdas - binary primitive with one argument") {

    assert((

      λv1.(hydra.lib.strings.splitOn! @ "foo" @ v1)) == (

      λv1.(hydra.lib.strings.splitOn! @ "foo" @ v1)))

  }

  test("Partial application of primitives - Partially applied primitives expand with lambdas - ternary primitive with one argument") {

    assert((

      λv1.λv2.(hydra.lib.lists.foldl! @ f @ v1 @ v2)) == (

      λv1.λv2.(hydra.lib.lists.foldl! @ f @ v1 @ v2)))

  }

  // Fully applied primitives are not expanded

  test("Partial application of primitives - Fully applied primitives are not expanded - unary primitive") {

    assert((

      (hydra.lib.strings.toLower! @ "FOO")) == (

      (hydra.lib.strings.toLower! @ "FOO")))

  }

  test("Partial application of primitives - Fully applied primitives are not expanded - binary primitive") {

    assert((

      (hydra.lib.strings.splitOn! @ "," @ "a,b,c")) == (

      (hydra.lib.strings.splitOn! @ "," @ "a,b,c")))

  }

  // Record projections

  // Bare projections expand with a lambda

  test("Partial application of primitives - Record projections - Bare projections expand with a lambda - projection without argument") {

    assert((

      λv1.(project(Person){firstName} @ v1)) == (

      λv1.(project(Person){firstName} @ v1)))

  }

  // Applied projections are not expanded

  test("Partial application of primitives - Record projections - Applied projections are not expanded - projection with argument") {

    assert((

      (project(Person){firstName} @ person)) == (

      (project(Person){firstName} @ person)))

  }

  test("Partial application of primitives - Record projections - Applied projections are not expanded - projection applied to a record") {

    assert((

      (project(Person){firstName} @ record(Person){firstName="John", lastName="Doe"})) == (

      (project(Person){firstName} @ record(Person){firstName="John", lastName="Doe"})))

  }

  // Projections nested in other structures

  test("Partial application of primitives - Record projections - Projections nested in other structures - projection in a list") {

    assert((

      [λv1.(project(Person){firstName} @ v1), hydra.lib.strings.toLower!]) == (

      [λv1.(project(Person){firstName} @ v1), hydra.lib.strings.toLower!]))

  }

  test("Partial application of primitives - Record projections - Projections nested in other structures - projection in a tuple") {

    assert((

      (λv1.(project(Person){firstName} @ v1), "default")) == (

      (λv1.(project(Person){firstName} @ v1), "default")))

  }

  test("Partial application of primitives - Record projections - Projections nested in other structures - projection in let binding") {

    assert((

      let getter = λv1.(project(Person){firstName} @ v1) in getter) == (

      let getter = λv1.(project(Person){firstName} @ v1) in getter))

  }

  test("Partial application of primitives - Record projections - Projections nested in other structures - projection in lambda body") {

    assert((

      λx.λv1.(project(Person){firstName} @ v1)) == (

      λx.λv1.(project(Person){firstName} @ v1)))

  }

  // Function-valued projections

  test("Partial application of primitives - Record projections - Function-valued projections - projection of function-valued field applied to arguments should not be expanded") {

    assert((

      (project(Triple){first}⟨(string → string)⟩⟨string⟩⟨string⟩ @ record(Triple){first=hydra.lib.strings.toLower!, second="middle", third="last"}⟨(string → string)⟩⟨string⟩⟨string⟩ @ "DATA")) == (

      (project(Triple){first}⟨(string → string)⟩⟨string⟩⟨string⟩ @ record(Triple){first=hydra.lib.strings.toLower!, second="middle", third="last"}⟨(string → string)⟩⟨string⟩⟨string⟩ @ "DATA")))

  }

  // Polymorphic terms (System F)

  // Type lambdas in let bindings

  test("Polymorphic terms (System F) - Type lambdas in let bindings - polymorphic identity function") {

    assert((

      let id:((forall a. (a → a))) = Λa.λx.x in id) == (

      let id:((forall a. (a → a))) = Λa.λx.x in id))

  }

  test("Polymorphic terms (System F) - Type lambdas in let bindings - monomorphic partially applied primitive") {

    assert((

      let partial:(((string → list<string>))) = λv1.(hydra.lib.strings.splitOn! @ "foo" @ v1) in partial) == (

      let partial:(((string → list<string>))) = λv1.(hydra.lib.strings.splitOn! @ "foo" @ v1) in partial))

  }

  test("Polymorphic terms (System F) - Type lambdas in let bindings - monomorphic projection") {

    assert((

      let getter:(((Person → string))) = λv1.(project(Person){firstName} @ v1) in getter) == (

      let getter:(((Person → string))) = λv1.(project(Person){firstName} @ v1) in getter))

  }

  // Type applications of polymorphic bindings

  test("Polymorphic terms (System F) - Type applications of polymorphic bindings - polymorphic variable with type application") {

    assert((

      let id:((forall a. (a → a))) = Λa.λx.x in id⟨string⟩) == (

      let id:((forall a. (a → a))) = Λa.λx.x in id⟨string⟩))

  }

  test("Polymorphic terms (System F) - Type applications of polymorphic bindings - type application of identity applied to binary function with no arguments") {

    assert((

      let id:((forall a. (a → a))) = Λa.λx.x in (id⟨(string → string → list<string>)⟩ @ hydra.lib.strings.splitOn!)) == (

      let id:((forall a. (a → a))) = Λa.λx.x in (id⟨(string → string → list<string>)⟩ @ hydra.lib.strings.splitOn!)))

  }

  test("Polymorphic terms (System F) - Type applications of polymorphic bindings - type application of identity applied to partially applied binary function") {

    assert((

      let id:((forall a. (a → a))) = Λa.λx.x in (id⟨(string → list<string>)⟩ @ λv1.(hydra.lib.strings.splitOn! @ "," @ v1))) == (

      let id:((forall a. (a → a))) = Λa.λx.x in (id⟨(string → list<string>)⟩ @ λv1.(hydra.lib.strings.splitOn! @ "," @ v1))))

  }

  test("Polymorphic terms (System F) - Type applications of polymorphic bindings - type application of identity applied to fully applied binary function") {

    assert((

      let id:((forall a. (a → a))) = Λa.λx.x in (id⟨list<string>⟩ @ (hydra.lib.strings.splitOn! @ "," @ "foo,bar"))) == (

      let id:((forall a. (a → a))) = Λa.λx.x in (id⟨list<string>⟩ @ (hydra.lib.strings.splitOn! @ "," @ "foo,bar"))))

  }

  test("Polymorphic terms (System F) - Type applications of polymorphic bindings - type application of identity applied to binary function, then applied to one argument") {

    assert((

      let id:((forall a. (a → a))) = Λa.λx.x in λv1.(id⟨(string → string → list<string>)⟩ @ hydra.lib.strings.splitOn! @ "," @ v1)) == (

      let id:((forall a. (a → a))) = Λa.λx.x in λv1.(id⟨(string → string → list<string>)⟩ @ hydra.lib.strings.splitOn! @ "," @ v1)))

  }

  test("Polymorphic terms (System F) - Type applications of polymorphic bindings - type application of identity applied to binary function, then fully applied to two arguments") {

    assert((

      let id:((forall a. (a → a))) = Λa.λx.x in (id⟨(string → string → list<string>)⟩ @ hydra.lib.strings.splitOn! @ "," @ "foo,bar")) == (

      let id:((forall a. (a → a))) = Λa.λx.x in (id⟨(string → string → list<string>)⟩ @ hydra.lib.strings.splitOn! @ "," @ "foo,bar")))

  }

  // Higher-Order Functions

  // Functions that return functions

  test("Higher-Order Functions - Functions that return functions - lambda returning bare binary primitive") {

    assert((

      λx.hydra.lib.strings.splitOn!) == (

      λx.hydra.lib.strings.splitOn!))

  }

  test("Higher-Order Functions - Functions that return functions - lambda returning bare unary primitive") {

    assert((

      λx.hydra.lib.strings.toLower!) == (

      λx.hydra.lib.strings.toLower!))

  }

  test("Higher-Order Functions - Functions that return functions - lambda returning partially applied primitive") {

    assert((

      λx.λv1.(hydra.lib.strings.splitOn! @ "," @ v1)) == (

      λx.λv1.(hydra.lib.strings.splitOn! @ "," @ v1)))

  }

  test("Higher-Order Functions - Functions that return functions - lambda returning fully applied primitive") {

    assert((

      λx.(hydra.lib.strings.splitOn! @ "," @ x)) == (

      λx.(hydra.lib.strings.splitOn! @ "," @ x)))

  }

  test("Higher-Order Functions - Functions that return functions - lambda returning bare projection") {

    assert((

      λperson.λv1.(project(Person){firstName} @ v1)) == (

      λperson.λv1.(project(Person){firstName} @ v1)))

  }

  test("Higher-Order Functions - Functions that return functions - nested lambdas with partial application in body") {

    assert((

      λx.λy.λv1.(hydra.lib.strings.splitOn! @ x @ v1)) == (

      λx.λy.λv1.(hydra.lib.strings.splitOn! @ x @ v1)))

  }

  test("Higher-Order Functions - Functions that return functions - lambda returning lambda returning partial application") {

    assert((

      λx.λy.λz.λv1.(hydra.lib.strings.splitOn! @ x @ v1)) == (

      λx.λy.λz.λv1.(hydra.lib.strings.splitOn! @ x @ v1)))

  }

  // Let terms

  // partial application of a let-bound function

  test("Let terms - partial application of a let-bound function - simple") {

    assert((

      let helper:(((string → string → string → string))) = λarg1.λarg2.λarg3.(hydra.lib.strings.cat! @ [arg1, arg2, arg3]) in λv1.λv2.(helper @ "foo" @ v1 @ v2)) == (

      let helper:(((string → string → string → string))) = λarg1.λarg2.λarg3.(hydra.lib.strings.cat! @ [arg1, arg2, arg3]) in λv1.λv2.(helper @ "foo" @ v1 @ v2)))

  }

  test("Let terms - partial application of a let-bound function - in a fold") {

    assert((

      let helper:(((string → string → string → string))) = λarg1.λarg2.λarg3.(hydra.lib.strings.cat! @ [arg1, arg2, arg3]) in (hydra.lib.lists.foldl!⟨string⟩⟨string⟩ @ λv1.λv2.(helper @ "foo" @ v1 @ v2) @ "" @ ["bar", "baz"])) == (

      let helper:(((string → string → string → string))) = λarg1.λarg2.λarg3.(hydra.lib.strings.cat! @ [arg1, arg2, arg3]) in (hydra.lib.lists.foldl!⟨string⟩⟨string⟩ @ λv1.λv2.(helper @ "foo" @ v1 @ v2) @ "" @ ["bar", "baz"])))

  }

  test("Let terms - partial application of a let-bound function - within another let binding") {

    assert((

      let tryme:(((string → string → string))) = let helper:(((string → string → string → string))) = λarg1.λarg2.λarg3.(hydra.lib.strings.cat! @ [arg1, arg2, arg3]) in λv1.λv2.(helper @ "foo" @ v1 @ v2) in unit) == (

      let tryme:(((string → string → string))) = let helper:(((string → string → string → string))) = λarg1.λarg2.λarg3.(hydra.lib.strings.cat! @ [arg1, arg2, arg3]) in λv1.λv2.(helper @ "foo" @ v1 @ v2) in unit))

  }

  // Case statements

  // monomorphic at top level

  test("Case statements - monomorphic at top level - non-applied case statement") {

    assert((

      λv1.(case(UnionMonomorphic){string=λs.s, [default]="other"} @ v1)) == (

      λv1.(case(UnionMonomorphic){string=λs.s, [default]="other"} @ v1)))

  }

  test("Case statements - monomorphic at top level - applied case statement") {

    assert((

      (case(UnionMonomorphic){string=λs:string.s, [default]="other"} @ inject(UnionMonomorphic){string="foo"})) == (

      (case(UnionMonomorphic){string=λs:string.s, [default]="other"} @ inject(UnionMonomorphic){string="foo"})))

  }

  test("Case statements - monomorphic at top level - applied case statement in lambda") {

    assert((

      λx:UnionMonomorphic.(case(UnionMonomorphic){string=λs:string.s, [default]="other"} @ x)) == (

      λx:UnionMonomorphic.(case(UnionMonomorphic){string=λs:string.s, [default]="other"} @ x)))

  }

  // monomorphic in let binding

  test("Case statements - monomorphic in let binding - non-applied case statement") {

    assert((

      let test:(((UnionMonomorphic → string))) = λv1.(case(UnionMonomorphic){string=λs.s, [default]="other"} @ v1) in "ignored") == (

      let test:(((UnionMonomorphic → string))) = λv1.(case(UnionMonomorphic){string=λs.s, [default]="other"} @ v1) in "ignored"))

  }

  test("Case statements - monomorphic in let binding - applied case statement") {

    assert((

      let test:((string)) = (case(UnionMonomorphic){string=λs:string.s, [default]="other"} @ inject(UnionMonomorphic){string="foo"}) in "ignored") == (

      let test:((string)) = (case(UnionMonomorphic){string=λs:string.s, [default]="other"} @ inject(UnionMonomorphic){string="foo"}) in "ignored"))

  }

  test("Case statements - monomorphic in let binding - applied case statement in lambda") {

    assert((

      let test:(((string → string))) = λx:UnionMonomorphic.(case(UnionMonomorphic){string=λs:string.s, [default]="other"} @ x) in "ignored") == (

      let test:(((string → string))) = λx:UnionMonomorphic.(case(UnionMonomorphic){string=λs:string.s, [default]="other"} @ x) in "ignored"))

  }

  // polymorphic in let binding

  test("Case statements - polymorphic in let binding - non-applied UnionPolymorphicRecursive") {

    assert((

      let test:((((UnionPolymorphicRecursive @ int32) → string))) = λv1.(case(UnionPolymorphicRecursive){value=λi:int32.(hydra.lib.literals.showInt32! @ i), [default]="other"}⟨int32⟩ @ v1) in test) == (

      let test:((((UnionPolymorphicRecursive @ int32) → string))) = λv1.(case(UnionPolymorphicRecursive){value=λi:int32.(hydra.lib.literals.showInt32! @ i), [default]="other"}⟨int32⟩ @ v1) in test))

  }

  test("Case statements - polymorphic in let binding - applied UnionPolymorphicRecursive with int32") {

    assert((

      let test:((string)) = (case(UnionPolymorphicRecursive){value=λi:int32.(hydra.lib.literals.showInt32! @ i), [default]="other"}⟨int32⟩ @ inject(UnionPolymorphicRecursive){value=42:int32}⟨int32⟩) in test) == (

      let test:((string)) = (case(UnionPolymorphicRecursive){value=λi:int32.(hydra.lib.literals.showInt32! @ i), [default]="other"}⟨int32⟩ @ inject(UnionPolymorphicRecursive){value=42:int32}⟨int32⟩) in test))

  }

  test("Case statements - polymorphic in let binding - applied UnionPolymorphicRecursive with int32 in lambda") {

    assert((

      let test:((((UnionPolymorphicRecursive @ int32) → string))) = λx:(UnionPolymorphicRecursive @ int32).(case(UnionPolymorphicRecursive){value=λi:int32.(hydra.lib.literals.showInt32! @ i), [default]="other"}⟨int32⟩ @ x) in test) == (

      let test:((((UnionPolymorphicRecursive @ int32) → string))) = λx:(UnionPolymorphicRecursive @ int32).(case(UnionPolymorphicRecursive){value=λi:int32.(hydra.lib.literals.showInt32! @ i), [default]="other"}⟨int32⟩ @ x) in test))

  }

  test("Case statements - polymorphic in let binding - applied generic UnionPolymorphicRecursive in lambda") {

    assert((

      Λt0.let test:((forall t1. ((UnionPolymorphicRecursive @ t1) → string))) = Λt1.λx:(UnionPolymorphicRecursive @ t1).(case(UnionPolymorphicRecursive){value=λignored:t1."foo", [default]="other"}⟨t1⟩ @ x) in test⟨t0⟩) == (

      Λt0.let test:((forall t1. ((UnionPolymorphicRecursive @ t1) → string))) = Λt1.λx:(UnionPolymorphicRecursive @ t1).(case(UnionPolymorphicRecursive){value=λignored:t1."foo", [default]="other"}⟨t1⟩ @ x) in test⟨t0⟩))

  }

  // Forced expansion in case statement branches

  test("Case statements - Forced expansion in case statement branches - variable reference in case branch is expanded") {

    assert((

      let handler:(((string → string))) = hydra.lib.strings.toLower! in λv1.(case(UnionMonomorphic){bool=λignored."boolean value", string=λv1.(handler @ v1), unit=λignored."unit value"} @ v1)) == (

      let handler:(((string → string))) = hydra.lib.strings.toLower! in λv1.(case(UnionMonomorphic){bool=λignored."boolean value", string=λv1.(handler @ v1), unit=λignored."unit value"} @ v1)))

  }

  test("Case statements - Forced expansion in case statement branches - bare primitive in case branch is expanded") {

    assert((

      λv1.(case(UnionMonomorphic){bool=λignored."boolean value", string=λv1.(hydra.lib.strings.toLower! @ v1), unit=λignored."unit value"} @ v1)) == (

      λv1.(case(UnionMonomorphic){bool=λignored."boolean value", string=λv1.(hydra.lib.strings.toLower! @ v1), unit=λignored."unit value"} @ v1)))

  }

  test("Case statements - Forced expansion in case statement branches - variable reference outside case branch is not expanded") {

    assert((

      let handler = hydra.lib.strings.toLower! in handler) == (

      let handler = hydra.lib.strings.toLower! in handler))

  }

  test("Case statements - Forced expansion in case statement branches - bare primitive outside case branch is not expanded") {

    assert((

      hydra.lib.strings.toLower!) == (

      hydra.lib.strings.toLower!))

  }

  // Non-expansion of eliminations which produce functions

  test("Non-expansion of eliminations which produce functions - applied case statement") {

    assert((

      Λt0.λdir:hydra.coders.CoderDirection.λcoder:(hydra.util.Coder @ t0 @ t0).λcx:hydra.context.Context.λv1:t0.(case(hydra.coders.CoderDirection){encode=λ_:unit.(project(hydra.util.Coder){encode}⟨t0⟩⟨t0⟩ @ coder @ cx @ v1), decode=λ_:unit.(project(hydra.util.Coder){decode}⟨t0⟩⟨t0⟩ @ coder @ cx @ v1)} @ dir)) == (

      Λt0.λdir:hydra.coders.CoderDirection.λcoder:(hydra.util.Coder @ t0 @ t0).λcx:hydra.context.Context.λv1:t0.(case(hydra.coders.CoderDirection){encode=λ_:unit.(project(hydra.util.Coder){encode}⟨t0⟩⟨t0⟩ @ coder @ cx @ v1), decode=λ_:unit.(project(hydra.util.Coder){decode}⟨t0⟩⟨t0⟩ @ coder @ cx @ v1)} @ dir)))

  }

  test("Non-expansion of eliminations which produce functions - applied projection") {

    assert((

      (project(Triple){third}⟨int32⟩⟨int32⟩⟨(string → string)⟩ @ record(Triple){first=42:int32, second=137:int32, third=λs.(hydra.lib.strings.toLower! @ s)})) == (

      (project(Triple){third}⟨int32⟩⟨int32⟩⟨(string → string)⟩ @ record(Triple){first=42:int32, second=137:int32, third=λs.(hydra.lib.strings.toLower! @ s)})))

  }
}
