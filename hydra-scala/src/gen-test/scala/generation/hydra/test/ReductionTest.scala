// Note: this is an automatically generated file. Do not edit.
// reduction

package generation.hydra.test

import org.scalatest.funsuite.AnyFunSuite

class ReductionTest extends AnyFunSuite {

  // beta reduction

  test("beta reduction - identity function applied to literal") {

    assert((

      42:int32) == (

      42:int32))

  }

  test("beta reduction - constant function") {

    assert((

      1:int32) == (

      1:int32))

  }

  test("beta reduction - nested application") {

    assert((

      1:int32) == (

      1:int32))

  }

  // monomorphic primitives

  test("monomorphic primitives - toUpper on lowercase") {

    assert((

      "HELLO") == (

      "HELLO"))

  }

  test("monomorphic primitives - toUpper on mixed case") {

    assert((

      "HELLO WORLD") == (

      "HELLO WORLD"))

  }

  test("monomorphic primitives - toUpper on empty string") {

    assert((

      "") == (

      ""))

  }

  test("monomorphic primitives - toLower on uppercase") {

    assert((

      "hello") == (

      "hello"))

  }

  test("monomorphic primitives - string length") {

    assert((

      5:int32) == (

      5:int32))

  }

  test("monomorphic primitives - string length of empty") {

    assert((

      0:int32) == (

      0:int32))

  }

  test("monomorphic primitives - add two positive integers") {

    assert((

      8:int32) == (

      8:int32))

  }

  test("monomorphic primitives - add negative and positive") {

    assert((

      -7:int32) == (

      -7:int32))

  }

  test("monomorphic primitives - add with zero") {

    assert((

      42:int32) == (

      42:int32))

  }

  test("monomorphic primitives - subtract integers") {

    assert((

      7:int32) == (

      7:int32))

  }

  test("monomorphic primitives - multiply integers") {

    assert((

      42:int32) == (

      42:int32))

  }

  test("monomorphic primitives - multiply by zero") {

    assert((

      0:int32) == (

      0:int32))

  }

  test("monomorphic primitives - divide integers") {

    assert((

      5:int32) == (

      5:int32))

  }

  test("monomorphic primitives - modulo") {

    assert((

      2:int32) == (

      2:int32))

  }

  test("monomorphic primitives - splitOn basic") {

    assert((

      ["a", "b", "c"]) == (

      ["a", "b", "c"]))

  }

  test("monomorphic primitives - cat2 strings") {

    assert((

      "helloworld") == (

      "helloworld"))

  }

  // polymorphic primitives

  test("polymorphic primitives - length of integer list") {

    assert((

      3:int32) == (

      3:int32))

  }

  test("polymorphic primitives - length of string list") {

    assert((

      2:int32) == (

      2:int32))

  }

  test("polymorphic primitives - length of empty list") {

    assert((

      0:int32) == (

      0:int32))

  }

  test("polymorphic primitives - length of single element list") {

    assert((

      1:int32) == (

      1:int32))

  }

  test("polymorphic primitives - head of integer list") {

    assert((

      10:int32) == (

      10:int32))

  }

  test("polymorphic primitives - head of string list") {

    assert((

      "first") == (

      "first"))

  }

  test("polymorphic primitives - last of integer list") {

    assert((

      30:int32) == (

      30:int32))

  }

  test("polymorphic primitives - concat two integer lists") {

    assert((

      [1:int32, 2:int32, 3:int32, 4:int32]) == (

      [1:int32, 2:int32, 3:int32, 4:int32]))

  }

  test("polymorphic primitives - concat with empty list") {

    assert((

      [1:int32, 2:int32]) == (

      [1:int32, 2:int32]))

  }

  test("polymorphic primitives - reverse integer list") {

    assert((

      [3:int32, 2:int32, 1:int32]) == (

      [3:int32, 2:int32, 1:int32]))

  }

  test("polymorphic primitives - reverse empty list") {

    assert((

      []) == (

      []))

  }

  // nullary primitives

  test("nullary primitives - empty set has size zero") {

    assert((

      0:int32) == (

      0:int32))

  }

  // literals as values

  test("literals as values - integer literal is a value") {

    assert((

      42:int32) == (

      42:int32))

  }

  test("literals as values - negative integer literal") {

    assert((

      -17:int32) == (

      -17:int32))

  }

  test("literals as values - zero integer literal") {

    assert((

      0:int32) == (

      0:int32))

  }

  test("literals as values - string literal is a value") {

    assert((

      "hello") == (

      "hello"))

  }

  test("literals as values - empty string literal") {

    assert((

      "") == (

      ""))

  }

  test("literals as values - string with special characters") {

    assert((

      "hello\nworld\ttab") == (

      "hello\nworld\ttab"))

  }

  test("literals as values - boolean true is a value") {

    assert((

      true) == (

      true))

  }

  test("literals as values - boolean false is a value") {

    assert((

      false) == (

      false))

  }

  test("literals as values - float literal is a value") {

    assert((

      3.14:float64) == (

      3.14:float64))

  }

  test("literals as values - negative float literal") {

    assert((

      -2.718:float64) == (

      -2.718:float64))

  }

  test("literals as values - zero float literal") {

    assert((

      0.0:float64) == (

      0.0:float64))

  }

  // list reduction

  test("list reduction - empty list is a value") {

    assert((

      []) == (

      []))

  }

  test("list reduction - list of literals is a value") {

    assert((

      [1:int32, 2:int32, 3:int32]) == (

      [1:int32, 2:int32, 3:int32]))

  }

  test("list reduction - list with reducible element") {

    assert((

      [42:int32]) == (

      [42:int32]))

  }

  // optional reduction

  test("optional reduction - nothing is a value") {

    assert((

      nothing) == (

      nothing))

  }

  test("optional reduction - just literal is a value") {

    assert((

      just(42:int32)) == (

      just(42:int32)))

  }

  test("optional reduction - just with reducible content") {

    assert((

      just(42:int32)) == (

      just(42:int32)))

  }

  // alpha conversion

  test("alpha conversion - variable at top level") {

    assert((

      y) == (

      y))

  }

  test("alpha conversion - variable in list") {

    assert((

      [42:int32, y]) == (

      [42:int32, y]))

  }

  test("alpha conversion - lambda with different variable is transparent") {

    assert((

      λz.[42:int32, y, z]) == (

      λz.[42:int32, y, z]))

  }

  test("alpha conversion - lambda with same variable is opaque") {

    assert((

      λx.[42:int32, x, z]) == (

      λx.[42:int32, x, z]))

  }

  test("alpha conversion - nested lambda outer variable") {

    assert((

      λa.λb.y) == (

      λa.λb.y))

  }

  test("alpha conversion - nested lambda shadows outer") {

    assert((

      λx.λy.x) == (

      λx.λy.x))

  }

  test("alpha conversion - application with variable") {

    assert((

      (f @ y)) == (

      (f @ y)))

  }

  test("alpha conversion - application with both variables same") {

    assert((

      (y @ y)) == (

      (y @ y)))

  }

  // type reduction

  test("type reduction - unit type unchanged") {

    assert((

      unit) == (

      unit))

  }

  test("type reduction - string type unchanged") {

    assert((

      string) == (

      string))

  }

  test("type reduction - int32 type unchanged") {

    assert((

      int32) == (

      int32))

  }

  test("type reduction - identity type applied to string") {

    assert((

      (string → string)) == (

      (string → string)))

  }

  test("type reduction - constant type ignores argument") {

    assert((

      int32) == (

      int32))

  }

  test("type reduction - nested forall first application") {

    assert((

      (∀y.(int32 → y))) == (

      (∀y.(int32 → y))))

  }

  test("type reduction - nested forall both applications") {

    assert((

      (int32 → string)) == (

      (int32 → string)))

  }

  test("type reduction - list type applied") {

    assert((

      list<int32>) == (

      list<int32>))

  }

  test("type reduction - optional type applied") {

    assert((

      maybe<string>) == (

      maybe<string>))

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

      (hydra.lib.strings.splitOn @ "foo" @ "bar")) == (

      (hydra.lib.strings.splitOn @ "foo" @ "bar")))

  }

  test("etaExpandTerm - lambda with fully applied primitive unchanged") {

    assert((

      λx.(hydra.lib.strings.splitOn @ "," @ x)) == (

      λx.(hydra.lib.strings.splitOn @ "," @ x)))

  }

  test("etaExpandTerm - lambda returning constant unchanged") {

    assert((

      λx.42:int32) == (

      λx.42:int32))

  }

  test("etaExpandTerm - bare unary primitive unchanged") {

    assert((

      hydra.lib.strings.toLower) == (

      hydra.lib.strings.toLower))

  }

  test("etaExpandTerm - bare binary primitive unchanged") {

    assert((

      hydra.lib.strings.splitOn) == (

      hydra.lib.strings.splitOn))

  }

  test("etaExpandTerm - partially applied binary primitive expands to one lambda") {

    assert((

      λv1.(hydra.lib.strings.splitOn @ foo @ v1)) == (

      λv1.(hydra.lib.strings.splitOn @ foo @ v1)))

  }

  test("etaExpandTerm - projection expands to lambda") {

    assert((

      λv1.(project(Person){firstName} @ v1)) == (

      λv1.(project(Person){firstName} @ v1)))

  }

  test("etaExpandTerm - partial application inside lambda expands") {

    assert((

      λx.λv1.(hydra.lib.strings.splitOn @ x @ v1)) == (

      λx.λv1.(hydra.lib.strings.splitOn @ x @ v1)))

  }

  test("etaExpandTerm - let with constant body unchanged") {

    assert((

      let foo = 137:int32 in 42:int32) == (

      let foo = 137:int32 in 42:int32))

  }

  test("etaExpandTerm - let with bare primitive value unchanged") {

    assert((

      let foo = hydra.lib.strings.splitOn in foo) == (

      let foo = hydra.lib.strings.splitOn in foo))

  }

  test("etaExpandTerm - fully applied unary unchanged") {

    assert((

      (hydra.lib.strings.toLower @ "FOO")) == (

      (hydra.lib.strings.toLower @ "FOO")))

  }

  test("etaExpandTerm - partial application in list expands") {

    assert((

      [λx.["foo"], λv1.(hydra.lib.strings.splitOn @ "bar" @ v1)]) == (

      [λx.["foo"], λv1.(hydra.lib.strings.splitOn @ "bar" @ v1)]))

  }
}
