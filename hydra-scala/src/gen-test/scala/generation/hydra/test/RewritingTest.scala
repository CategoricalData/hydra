// Note: this is an automatically generated file. Do not edit.
// rewriting

package generation.hydra.test

import org.scalatest.funsuite.AnyFunSuite

class RewritingTest extends AnyFunSuite {

  // freeVariables

  test("freeVariables - string literal has no free variables") {

    assert((

      {}) == (

      {}))

  }

  test("freeVariables - single variable") {

    assert((

      {x}) == (

      {x}))

  }

  test("freeVariables - bound variable is not free") {

    assert((

      {}) == (

      {}))

  }

  test("freeVariables - unbound variable in lambda body") {

    assert((

      {x}) == (

      {x}))

  }

  test("freeVariables - mixed free and bound variables") {

    assert((

      {x}) == (

      {x}))

  }

  test("freeVariables - multiple free variables") {

    assert((

      {x, y}) == (

      {x, y}))

  }

  // simplifyTerm

  test("simplifyTerm - const application with literal") {

    assert((

      "foo") == (

      "foo"))

  }

  test("simplifyTerm - identity application") {

    assert((

      [y, y]) == (

      [y, y]))

  }

  test("simplifyTerm - unused parameter") {

    assert((

      "foo") == (

      "foo"))

  }

  test("simplifyTerm - nested lambda applications") {

    assert((

      ["foo", y]) == (

      ["foo", y]))

  }

  // flattenLetTerms

  test("flattenLetTerms - non-let term unchanged") {

    assert((

      42:int32) == (

      42:int32))

  }

  test("flattenLetTerms - list term unchanged") {

    assert((

      ["foo"]) == (

      ["foo"]))

  }

  test("flattenLetTerms - sequential lets in body are flattened") {

    assert((

      let x = 1:int32, y = 2:int32 in [x, y]) == (

      let x = 1:int32, y = 2:int32 in [x, y]))

  }

  test("flattenLetTerms - nested binding in let value is flattened") {

    assert((

      let a = 1:int32, b_x = 1:int32, b_y = 2:int32, b = [b_x, b_y] in [a, b]) == (

      let a = 1:int32, b_x = 1:int32, b_y = 2:int32, b = [b_x, b_y] in [a, b]))

  }

  test("flattenLetTerms - multiple levels of nesting are flattened") {

    assert((

      let a = 1:int32, b_x = 1:int32, b_y_p = 137:int32, b_y_q = [b_x, 5:int32], b_y = [a, b_y_q], b = [b_x, b_y] in [a, b]) == (

      let a = 1:int32, b_x = 1:int32, b_y_p = 137:int32, b_y_q = [b_x, 5:int32], b_y = [a, b_y_q], b = [b_x, b_y] in [a, b]))

  }

  // liftLambdaAboveLet

  test("liftLambdaAboveLet - simple let with lambda in body") {

    assert((

      λy.let x = 42:int32 in x) == (

      λy.let x = 42:int32 in x))

  }

  test("liftLambdaAboveLet - bare lambda unchanged") {

    assert((

      λx.x) == (

      λx.x))

  }

  test("liftLambdaAboveLet - bare let unchanged") {

    assert((

      let x = 42:int32 in x) == (

      let x = 42:int32 in x))

  }

  test("liftLambdaAboveLet - lambda with let in body unchanged") {

    assert((

      λy.let x = 42:int32 in x) == (

      λy.let x = 42:int32 in x))

  }

  test("liftLambdaAboveLet - let with two nested lambdas") {

    assert((

      λy.λz.let x = 42:int32 in x) == (

      λy.λz.let x = 42:int32 in x))

  }

  test("liftLambdaAboveLet - lambda inside let body already above let") {

    assert((

      λx.λy.let z = 42:int32 in z) == (

      λx.λy.let z = 42:int32 in z))

  }

  test("liftLambdaAboveLet - let without lambda in body unchanged") {

    assert((

      let x = 42:int32, y = "hello" in (x, y)) == (

      let x = 42:int32, y = "hello" in (x, y)))

  }

  test("liftLambdaAboveLet - multiple let bindings with lambda") {

    assert((

      λz.let x = 42:int32, y = "hello" in x) == (

      λz.let x = 42:int32, y = "hello" in x))

  }

  test("liftLambdaAboveLet - nested lets with lambda at innermost level") {

    assert((

      λz.let x = 42:int32 in let y = "hello" in x) == (

      λz.let x = 42:int32 in let y = "hello" in x))

  }

  test("liftLambdaAboveLet - lambda between two lets") {

    assert((

      λy.let x = 42:int32 in let z = "hello" in x) == (

      λy.let x = 42:int32 in let z = "hello" in x))

  }

  test("liftLambdaAboveLet - multiple lambdas between nested lets") {

    assert((

      λx.λy.let a = 1:int32 in let b = 2:int32 in a) == (

      λx.λy.let a = 1:int32 in let b = 2:int32 in a))

  }

  test("liftLambdaAboveLet - multiple lambdas already above let") {

    assert((

      λx.λy.let z = 42:int32 in z) == (

      λx.λy.let z = 42:int32 in z))

  }

  test("liftLambdaAboveLet - annotation above let containing lambda") {

    assert((

      λy.let x = 42:int32 in x) == (

      λy.let x = 42:int32 in x))

  }

  test("liftLambdaAboveLet - annotation above lambda in let body") {

    assert((

      λy.let x = 42:int32 in x) == (

      λy.let x = 42:int32 in x))

  }

  test("liftLambdaAboveLet - annotation between two lambdas") {

    assert((

      λy.λz.let x = 42:int32 in x) == (

      λy.λz.let x = 42:int32 in x))

  }

  test("liftLambdaAboveLet - annotation on the body of lambda in let") {

    assert((

      λy.let x = 42:int32 in x) == (

      λy.let x = 42:int32 in x))

  }

  test("liftLambdaAboveLet - annotation on lambda already above let") {

    assert((

      λy.let x = 42:int32 in x) == (

      λy.let x = 42:int32 in x))

  }

  test("liftLambdaAboveLet - let-lambda inside a list") {

    assert((

      [1:int32, λy.let x = 42:int32 in x, 2:int32]) == (

      [1:int32, λy.let x = 42:int32 in x, 2:int32]))

  }

  test("liftLambdaAboveLet - let-lambda in multiple list elements") {

    assert((

      [λy.let x = 1:int32 in x, λw.let z = 2:int32 in z]) == (

      [λy.let x = 1:int32 in x, λw.let z = 2:int32 in z]))

  }

  test("liftLambdaAboveLet - let-lambda in a let binding value") {

    assert((

      let f = λy.let x = 42:int32 in x in f) == (

      let f = λy.let x = 42:int32 in x in f))

  }

  test("liftLambdaAboveLet - let-lambda inside a pair") {

    assert((

      (λy.let x = 42:int32 in x, "test")) == (

      (λy.let x = 42:int32 in x, "test")))

  }

  test("liftLambdaAboveLet - let-lambda in both elements of a pair") {

    assert((

      (λy.let x = 1:int32 in x, λw.let z = 2:int32 in z)) == (

      (λy.let x = 1:int32 in x, λw.let z = 2:int32 in z)))

  }

  test("liftLambdaAboveLet - let-lambda inside lambda body") {

    assert((

      λouter.λinner.let x = 42:int32 in x) == (

      λouter.λinner.let x = 42:int32 in x))

  }

  // deannotateTerm

  test("deannotateTerm - unannotated literal unchanged") {

    assert((

      42:int32) == (

      42:int32))

  }

  test("deannotateTerm - unannotated variable unchanged") {

    assert((

      x) == (

      x))

  }

  test("deannotateTerm - unannotated lambda unchanged") {

    assert((

      λx.x) == (

      λx.x))

  }

  test("deannotateTerm - single annotation stripped") {

    assert((

      42:int32) == (

      42:int32))

  }

  test("deannotateTerm - nested annotations stripped") {

    assert((

      42:int32) == (

      42:int32))

  }

  test("deannotateTerm - annotated lambda stripped") {

    assert((

      λx.x) == (

      λx.x))

  }

  test("deannotateTerm - annotated application stripped") {

    assert((

      (f @ x)) == (

      (f @ x)))

  }

  // deannotateType

  test("deannotateType - unannotated primitive type unchanged") {

    assert((

      int32) == (

      int32))

  }

  test("deannotateType - unannotated string type unchanged") {

    assert((

      string) == (

      string))

  }

  test("deannotateType - unannotated function type unchanged") {

    assert((

      (int32 → string)) == (

      (int32 → string)))

  }

  test("deannotateType - single annotation stripped") {

    assert((

      int32) == (

      int32))

  }

  test("deannotateType - nested annotations stripped") {

    assert((

      string) == (

      string))

  }

  test("deannotateType - annotated list type stripped") {

    assert((

      list<int32>) == (

      list<int32>))

  }

  test("deannotateType - annotated function type stripped") {

    assert((

      (int32 → string)) == (

      (int32 → string)))

  }

  // topologicalSortBindings

  test("topologicalSortBindings - isolated bindings") {

    assert((

      [[(a, "foo")], [(b, "bar")]]) == (

      [[(a, "foo")], [(b, "bar")]]))

  }

  test("topologicalSortBindings - single recursive binding") {

    assert((

      [[(a, [a])]]) == (

      [[(a, [a])]]))

  }

  test("topologicalSortBindings - mutually recursive bindings") {

    assert((

      [[(a, [b]), (b, [a])]]) == (

      [[(a, [b]), (b, [a])]]))

  }

  test("topologicalSortBindings - mixed bindings") {

    assert((

      [[(c, "foo")], [(a, b), (b, [a, c])], [(d, "bar")]]) == (

      [[(c, "foo")], [(a, b), (b, [a, c])], [(d, "bar")]]))

  }

  // normalizeTypeVariables

  test("normalizeTypeVariables - literal without type variables unchanged") {

    assert((

      42:int32) == (

      42:int32))

  }

  test("normalizeTypeVariables - simple let without type annotations unchanged") {

    assert((

      let foo = "foo" in 42:int32) == (

      let foo = "foo" in 42:int32))

  }

  test("normalizeTypeVariables - let with monomorphic type scheme unchanged") {

    assert((

      let foo:((string)) = "foo" in 42:int32) == (

      let foo:((string)) = "foo" in 42:int32))

  }

  test("normalizeTypeVariables - let with monomorphic binding referencing string") {

    assert((

      let foo:((string)) = "foo" in 42:int32) == (

      let foo:((string)) = "foo" in 42:int32))

  }

  test("normalizeTypeVariables - polymorphic binding with free type variable unchanged") {

    assert((

      let foo:((a)) = bar in 42:int32) == (

      let foo:((a)) = bar in 42:int32))

  }

  test("normalizeTypeVariables - monomorphic binding with typed lambda unchanged") {

    assert((

      let foo:((string)) = "foo" in λx:(a → int32).42:int32) == (

      let foo:((string)) = "foo" in λx:(a → int32).42:int32))

  }

  test("normalizeTypeVariables - polymorphic binding with typed lambda in body unchanged") {

    assert((

      let foo:((a)) = bar in λx:(a → int32).42:int32) == (

      let foo:((a)) = bar in λx:(a → int32).42:int32))

  }

  test("normalizeTypeVariables - polymorphic identity function normalized") {

    assert((

      let id:((forall t0. (t0 → t0))) = λx.x in (id @ 42:int32)) == (

      let id:((forall t0. (t0 → t0))) = λx.x in (id @ 42:int32)))

  }

  test("normalizeTypeVariables - polymorphic const function normalized") {

    assert((

      let const:((forall t0,t1. (t0 → t1 → t0))) = λx.λy.x in (const @ 42:int32 @ "foo")) == (

      let const:((forall t0,t1. (t0 → t1 → t0))) = λx.λy.x in (const @ 42:int32 @ "foo")))

  }

  test("normalizeTypeVariables - binding rewriting does not affect body with typed lambda") {

    assert((

      let id:((forall t0. (t0 → t0))) = λx.x in λx:(a → int32).42:int32) == (

      let id:((forall t0. (t0 → t0))) = λx.x in λx:(a → int32).42:int32))

  }

  test("normalizeTypeVariables - nested polymorphic lets normalized") {

    assert((

      let id:((forall t0. (t0 → t0))) = λx.x in let id2:((forall t0. (t0 → t0))) = λy.y in (id @ (id2 @ 42:int32))) == (

      let id:((forall t0. (t0 → t0))) = λx.x in let id2:((forall t0. (t0 → t0))) = λy.y in (id @ (id2 @ 42:int32))))

  }

  test("normalizeTypeVariables - nested same substitution in bindings and environment") {

    assert((

      let id:((forall t0. (t0 → t0))) = λx.x in let id2:((forall t0. (t0 → t0))) = λx.x in (id @ 42:int32)) == (

      let id:((forall t0. (t0 → t0))) = λx.x in let id2:((forall t0. (t0 → t0))) = λx.x in (id @ 42:int32)))

  }

  test("normalizeTypeVariables - parent type variable shadows child variable") {

    assert((

      let id:((forall t0. (t0 → t0))) = let id2:((forall t1. (t1 → t1))) = λx:t1.x in λy:t0.(id2 @ y) in (id @ 42:int32)) == (

      let id:((forall t0. (t0 → t0))) = let id2:((forall t1. (t1 → t1))) = λx:t1.x in λy:t0.(id2 @ y) in (id @ 42:int32)))

  }

  test("normalizeTypeVariables - no shadowing distinct type variables") {

    assert((

      let id:((forall t0. (t0 → t0))) = let id2:((forall t1. (t1 → t1))) = λx:t1.x in λy:t0.(id2 @ y) in (id @ 42:int32)) == (

      let id:((forall t0. (t0 → t0))) = let id2:((forall t1. (t1 → t1))) = λx:t1.x in λy:t0.(id2 @ y) in (id @ 42:int32)))

  }

  test("normalizeTypeVariables - locally free type variable in nested binding") {

    assert((

      let fun1:((forall t0,t1. (t0 → t1 → (t0, t1)))) = λx:t0.λy:t1.let fun2:((forall t2. (t2 → (t2, t1)))) = λz:t2.(z, y) in (fun2 @ x) in (fun1 @ "foo" @ 42:int32)) == (

      let fun1:((forall t0,t1. (t0 → t1 → (t0, t1)))) = λx:t0.λy:t1.let fun2:((forall t2. (t2 → (t2, t1)))) = λz:t2.(z, y) in (fun2 @ x) in (fun1 @ "foo" @ 42:int32)))

  }

  // etaExpandTerm

  test("etaExpandTerm - integer literal unchanged") {

    assert((

      42:int32) == (

      42:int32))

  }

  test("etaExpandTerm - string list unchanged") {

    assert((

      ["foo", "bar"]) == (

      ["foo", "bar"]))

  }

  test("etaExpandTerm - fully applied binary function unchanged") {

    assert((

      (hydra.lib.strings.splitOn! @ "foo" @ "bar")) == (

      (hydra.lib.strings.splitOn! @ "foo" @ "bar")))

  }

  test("etaExpandTerm - lambda with fully applied primitive unchanged") {

    assert((

      λx.(hydra.lib.strings.splitOn! @ "," @ x)) == (

      λx.(hydra.lib.strings.splitOn! @ "," @ x)))

  }

  test("etaExpandTerm - lambda returning constant unchanged") {

    assert((

      λx.42:int32) == (

      λx.42:int32))

  }

  test("etaExpandTerm - bare unary primitive unchanged") {

    assert((

      hydra.lib.strings.toLower!) == (

      hydra.lib.strings.toLower!))

  }

  test("etaExpandTerm - bare binary primitive unchanged") {

    assert((

      hydra.lib.strings.splitOn!) == (

      hydra.lib.strings.splitOn!))

  }

  test("etaExpandTerm - partially applied binary primitive expands to one lambda") {

    assert((

      λv1.(hydra.lib.strings.splitOn! @ foo @ v1)) == (

      λv1.(hydra.lib.strings.splitOn! @ foo @ v1)))

  }

  test("etaExpandTerm - projection expands to lambda") {

    assert((

      λv1.(project(Person){firstName} @ v1)) == (

      λv1.(project(Person){firstName} @ v1)))

  }

  test("etaExpandTerm - partial application inside lambda expands") {

    assert((

      λx.λv1.(hydra.lib.strings.splitOn! @ x @ v1)) == (

      λx.λv1.(hydra.lib.strings.splitOn! @ x @ v1)))

  }

  test("etaExpandTerm - let with constant body unchanged") {

    assert((

      let foo = 137:int32 in 42:int32) == (

      let foo = 137:int32 in 42:int32))

  }

  test("etaExpandTerm - let with bare primitive value unchanged") {

    assert((

      let foo = hydra.lib.strings.splitOn! in foo) == (

      let foo = hydra.lib.strings.splitOn! in foo))

  }

  test("etaExpandTerm - fully applied unary unchanged") {

    assert((

      (hydra.lib.strings.toLower! @ "FOO")) == (

      (hydra.lib.strings.toLower! @ "FOO")))

  }

  test("etaExpandTerm - partial application in list expands") {

    assert((

      [λx.["foo"], λv1.(hydra.lib.strings.splitOn! @ "bar" @ v1)]) == (

      [λx.["foo"], λv1.(hydra.lib.strings.splitOn! @ "bar" @ v1)]))

  }

  // foldOverTerm

  test("foldOverTerm - collect labels from single node - pre-order") {

    assert((

      ["a"]) == (

      ["a"]))

  }

  test("foldOverTerm - collect labels from tree - pre-order") {

    assert((

      ["a", "b", "c", "d"]) == (

      ["a", "b", "c", "d"]))

  }

  test("foldOverTerm - collect labels from single node - post-order") {

    assert((

      ["a"]) == (

      ["a"]))

  }

  test("foldOverTerm - collect labels from tree - post-order") {

    assert((

      ["b", "d", "c", "a"]) == (

      ["b", "d", "c", "a"]))

  }

  test("foldOverTerm - sum int32 literals") {

    assert((

      52:int32) == (

      52:int32))

  }

  test("foldOverTerm - collect list lengths - pre-order") {

    assert((

      [2:int32, 2:int32, 1:int32]) == (

      [2:int32, 2:int32, 1:int32]))

  }

  test("foldOverTerm - collect list lengths - post-order") {

    assert((

      [2:int32, 1:int32, 2:int32]) == (

      [2:int32, 1:int32, 2:int32]))

  }

  // rewriteType

  test("rewriteType - String type in left side of either is replaced") {

    assert((

      either<int32, int32>) == (

      either<int32, int32>))

  }

  test("rewriteType - String type in right side of either is replaced") {

    assert((

      either<int32, int32>) == (

      either<int32, int32>))

  }

  test("rewriteType - String types in both sides of either are replaced") {

    assert((

      either<int32, int32>) == (

      either<int32, int32>))

  }

  test("rewriteType - String type in nested either (left of left) is replaced") {

    assert((

      either<either<int32, int32>, int64>) == (

      either<either<int32, int32>, int64>))

  }

  test("rewriteType - String type in nested either (right of right) is replaced") {

    assert((

      either<int64, either<int32, int32>>) == (

      either<int64, either<int32, int32>>))

  }

  test("rewriteType - String types in complex nested either are all replaced") {

    assert((

      either<either<int32, int32>, either<int32, int64>>) == (

      either<either<int32, int32>, either<int32, int64>>))

  }

  test("rewriteType - String in list type is replaced") {

    assert((

      list<int32>) == (

      list<int32>))

  }

  test("rewriteType - String in function domain is replaced") {

    assert((

      (int32 → int64)) == (

      (int32 → int64)))

  }

  test("rewriteType - String in function codomain is replaced") {

    assert((

      (int64 → int32)) == (

      (int64 → int32)))

  }

  test("rewriteType - String in optional type is replaced") {

    assert((

      maybe<int32>) == (

      maybe<int32>))

  }

  // rewriteTerm

  test("rewriteTerm - string literal foo replaced with bar") {

    assert((

      "bar") == (

      "bar"))

  }

  test("rewriteTerm - string in variable not changed") {

    assert((

      x) == (

      x))

  }

  test("rewriteTerm - string in list") {

    assert((

      ["bar", "baz"]) == (

      ["bar", "baz"]))

  }

  test("rewriteTerm - multiple strings in list") {

    assert((

      ["bar", "bar", "baz"]) == (

      ["bar", "bar", "baz"]))

  }

  test("rewriteTerm - string in optional (just)") {

    assert((

      just("bar")) == (

      just("bar")))

  }

  test("rewriteTerm - string in function application") {

    assert((

      (print @ "bar")) == (

      (print @ "bar")))

  }

  test("rewriteTerm - string in lambda body") {

    assert((

      λx."bar") == (

      λx."bar"))

  }

  test("rewriteTerm - string in nested applications") {

    assert((

      (f @ (g @ "bar"))) == (

      (f @ (g @ "bar"))))

  }

  test("rewriteTerm - string in record field") {

    assert((

      record(Person){name="bar"}) == (

      record(Person){name="bar"}))

  }

  test("rewriteTerm - strings in multiple record fields") {

    assert((

      record(Data){a="bar", b="baz", c="bar"}) == (

      record(Data){a="bar", b="baz", c="bar"}))

  }

  test("rewriteTerm - string in pair") {

    assert((

      ("bar", 42:int32)) == (

      ("bar", 42:int32)))

  }

  test("rewriteTerm - string in let binding value") {

    assert((

      let x = "bar" in x) == (

      let x = "bar" in x))

  }

  test("rewriteTerm - string in let body") {

    assert((

      let x = 1:int32 in "bar") == (

      let x = 1:int32 in "bar"))

  }

  test("rewriteTerm - string in first case branch") {

    assert((

      case(Result){success="bar", error="baz"}) == (

      case(Result){success="bar", error="baz"}))

  }

  test("rewriteTerm - string in second case branch") {

    assert((

      case(Result){success="baz", error="bar"}) == (

      case(Result){success="baz", error="bar"}))

  }

  test("rewriteTerm - string in default branch") {

    assert((

      case(Result){success="baz", error="baz", [default]="bar"}) == (

      case(Result){success="baz", error="baz", [default]="bar"}))

  }

  test("rewriteTerm - string deeply nested in record in list in application") {

    assert((

      (process @ [record(Item){value="bar"}])) == (

      (process @ [record(Item){value="bar"}])))

  }

  test("rewriteTerm - string in union inject value") {

    assert((

      inject(Result){success="bar"}) == (

      inject(Result){success="bar"}))

  }

  test("rewriteTerm - string in wrapped term") {

    assert((

      wrap(Email){"bar"}) == (

      wrap(Email){"bar"}))

  }

  test("rewriteTerm - string in annotated term body") {

    assert((

      "bar") == (

      "bar"))

  }

  test("rewriteTerm - string in first of multiple let bindings") {

    assert((

      let x = "bar", y = "baz" in x) == (

      let x = "bar", y = "baz" in x))

  }

  test("rewriteTerm - string in second of multiple let bindings") {

    assert((

      let x = "baz", y = "bar" in y) == (

      let x = "baz", y = "bar" in y))

  }

  test("rewriteTerm - string in all let bindings and body") {

    assert((

      let x = "bar", y = "bar" in "bar") == (

      let x = "bar", y = "bar" in "bar"))

  }

  test("rewriteTerm - string in set") {

    assert((

      {"bar", "baz"}) == (

      {"bar", "baz"}))

  }

  test("rewriteTerm - string in type lambda body") {

    assert((

      Λa."bar") == (

      Λa."bar"))

  }

  test("rewriteTerm - string in type application body") {

    assert((

      "bar"⟨string⟩) == (

      "bar"⟨string⟩))

  }

  test("rewriteTerm - string in nested type lambdas") {

    assert((

      Λa.Λb."bar") == (

      Λa.Λb."bar"))

  }

  test("rewriteTerm - string in case branch within let binding") {

    assert((

      let handler = case(Result){ok="bar", err="baz"} in handler) == (

      let handler = case(Result){ok="bar", err="baz"} in handler))

  }

  test("rewriteTerm - string in annotated wrapped record field") {

    assert((

      wrap(User){record(UserData){name="bar"}}) == (

      wrap(User){record(UserData){name="bar"}}))

  }

  // rewriteAndFoldTermWithPath

  test("rewriteAndFoldTermWithPath - path tracking through application - sum literals") {

    assert((

      42:int32) == (

      42:int32))

  }

  test("rewriteAndFoldTermWithPath - path tracking through nested applications") {

    assert((

      3:int32) == (

      3:int32))

  }

  test("rewriteAndFoldTermWithPath - path tracking through let bindings") {

    assert((

      42:int32) == (

      42:int32))

  }

  test("rewriteAndFoldTermWithPath - path tracking through record fields") {

    assert((

      30:int32) == (

      30:int32))

  }

  test("rewriteAndFoldTermWithPath - path tracking through case branches") {

    assert((

      3:int32) == (

      3:int32))

  }

  test("rewriteAndFoldTermWithPath - path tracking through pair") {

    assert((

      12:int32) == (

      12:int32))

  }

  test("rewriteAndFoldTermWithPath - path tracking through optional") {

    assert((

      42:int32) == (

      42:int32))

  }

  test("rewriteAndFoldTermWithPath - path tracking through wrapped term") {

    assert((

      25:int32) == (

      25:int32))

  }

  test("rewriteAndFoldTermWithPath - path tracking through type lambda") {

    assert((

      100:int32) == (

      100:int32))

  }

  test("rewriteAndFoldTermWithPath - path tracking through type application") {

    assert((

      50:int32) == (

      50:int32))

  }

  test("rewriteAndFoldTermWithPath - path tracking through set elements") {

    assert((

      6:int32) == (

      6:int32))

  }

  test("rewriteAndFoldTermWithPath - deep nesting - application in lambda in let") {

    assert((

      15:int32) == (

      15:int32))

  }

  test("rewriteAndFoldTermWithPath - collect list lengths in nested structure") {

    assert((

      [2:int32, 2:int32, 1:int32]) == (

      [2:int32, 2:int32, 1:int32]))

  }

  test("rewriteAndFoldTermWithPath - collect list lengths in let body") {

    assert((

      [2:int32, 1:int32]) == (

      [2:int32, 1:int32]))

  }

  // unshadowVariables

  test("unshadowVariables - literal unchanged") {

    assert((

      42:int32) == (

      42:int32))

  }

  test("unshadowVariables - variable unchanged") {

    assert((

      x) == (

      x))

  }

  test("unshadowVariables - single lambda unchanged") {

    assert((

      λx.x) == (

      λx.x))

  }

  test("unshadowVariables - distinct lambda parameters unchanged") {

    assert((

      λx.λy.[x, y]) == (

      λx.λy.[x, y]))

  }

  test("unshadowVariables - let with no shadowing unchanged") {

    assert((

      let x = 1:int32 in x) == (

      let x = 1:int32 in x))

  }

  test("unshadowVariables - let and lambda with distinct names unchanged") {

    assert((

      let x = 1:int32 in λy.[x, y]) == (

      let x = 1:int32 in λy.[x, y]))

  }

  test("unshadowVariables - inner lambda shadows outer lambda") {

    assert((

      λx.λx2.x2) == (

      λx.λx2.x2))

  }

  test("unshadowVariables - inner lambda shadows outer - body references both") {

    assert((

      λx.[x, λx2.x2]) == (

      λx.[x, λx2.x2]))

  }

  test("unshadowVariables - triple nested lambda same name") {

    assert((

      λx.λx2.λx3.x3) == (

      λx.λx2.λx3.x3))

  }

  test("unshadowVariables - two parameters shadow sequentially") {

    assert((

      λx.λy.λx2.λy2.[x2, y2]) == (

      λx.λy.λx2.λy2.[x2, y2]))

  }

  test("unshadowVariables - lambda shadows let-bound variable") {

    assert((

      let x = 1:int32 in λx2.x2) == (

      let x = 1:int32 in λx2.x2))

  }

  test("unshadowVariables - lambda shadows one of multiple let bindings") {

    assert((

      let x = 1:int32, y = 2:int32 in λx2.[x2, y]) == (

      let x = 1:int32, y = 2:int32 in λx2.[x2, y]))

  }

  test("unshadowVariables - inner let body with lambda shadowing outer let") {

    assert((

      let x = 1:int32 in let y = 2:int32 in λx2.x2) == (

      let x = 1:int32 in let y = 2:int32 in λx2.x2))

  }

  test("unshadowVariables - shadowed lambda in function position of application") {

    assert((

      λf.(λf2.f2 @ f)) == (

      λf.(λf2.f2 @ f)))

  }

  test("unshadowVariables - shadowed lambdas in list elements") {

    assert((

      λx.[λx2.x2, λx2.x2]) == (

      λx.[λx2.x2, λx2.x2]))

  }

  test("unshadowVariables - shadowed lambda in record field") {

    assert((

      λx.record(Pair){fst=λx2.x2, snd=x}) == (

      λx.record(Pair){fst=λx2.x2, snd=x}))

  }

  test("unshadowVariables - shadowed lambda in case branch") {

    assert((

      λx.case(Maybe){nothing=0:int32, just=λx2.x2}) == (

      λx.case(Maybe){nothing=0:int32, just=λx2.x2}))

  }

  test("unshadowVariables - shadowed lambda in pair") {

    assert((

      λx.(λx2.x2, x)) == (

      λx.(λx2.x2, x)))

  }

  test("unshadowVariables - shadowed lambda inside optional") {

    assert((

      λx.just(λx2.x2)) == (

      λx.just(λx2.x2)))

  }

  test("unshadowVariables - shadowed lambda inside set element") {

    assert((

      λx.{λx2.x2}) == (

      λx.{λx2.x2}))

  }

  test("unshadowVariables - shadowed lambda in union injection") {

    assert((

      λx.inject(Result){ok=λx2.x2}) == (

      λx.inject(Result){ok=λx2.x2}))

  }

  test("unshadowVariables - shadowed lambda inside wrapped term") {

    assert((

      λx.wrap(Age){λx2.x2}) == (

      λx.wrap(Age){λx2.x2}))

  }

  test("unshadowVariables - shadowed lambda inside type lambda") {

    assert((

      λx.Λa.λx2.x2) == (

      λx.Λa.λx2.x2))

  }

  test("unshadowVariables - shadowed lambda inside type application") {

    assert((

      λx.λx2.x2⟨string⟩) == (

      λx.λx2.x2⟨string⟩))

  }

  test("unshadowVariables - shadowed lambda inside annotated term") {

    assert((

      λx.λx2.x2) == (

      λx.λx2.x2))

  }

  test("unshadowVariables - shadowing at multiple depths") {

    assert((

      λx.λy.λx2.λy2.[x2, y2]) == (

      λx.λy.λx2.λy2.[x2, y2]))

  }

  test("unshadowVariables - let then lambda then lambda all same name") {

    assert((

      let x = 1:int32 in λx2.λx3.x3) == (

      let x = 1:int32 in λx2.λx3.x3))

  }

  test("unshadowVariables - lambda with shadowing in let binding value") {

    assert((

      λx.let y = λx2.x2 in (y @ x)) == (

      λx.let y = λx2.x2 in (y @ x)))

  }

  test("unshadowVariables - application without shadowing unchanged") {

    assert((

      (f @ 42:int32)) == (

      (f @ 42:int32)))

  }

  test("unshadowVariables - list of literals unchanged") {

    assert((

      [1:int32, 2:int32, 3:int32]) == (

      [1:int32, 2:int32, 3:int32]))

  }

  test("unshadowVariables - nested record unchanged") {

    assert((

      record(Point){x=10:int32, y=20:int32}) == (

      record(Point){x=10:int32, y=20:int32}))

  }
}
