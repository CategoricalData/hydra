// Note: this is an automatically generated file. Do not edit.
// rewriting

package generation.hydra.test

import org.scalatest.funsuite.AnyFunSuite

class RewritingTest extends AnyFunSuite {

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
}
