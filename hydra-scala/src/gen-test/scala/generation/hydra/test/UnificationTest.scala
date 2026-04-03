// Note: this is an automatically generated file. Do not edit.
// unification

package generation.hydra.test

import org.scalatest.funsuite.AnyFunSuite

class UnificationTest extends AnyFunSuite {

  // variableOccursInType

  test("variableOccursInType - variable occurs in itself") {

    assert((

      true) == (

      true))

  }

  test("variableOccursInType - variable does not occur in different variable") {

    assert((

      false) == (

      false))

  }

  test("variableOccursInType - variable does not occur in int32") {

    assert((

      false) == (

      false))

  }

  test("variableOccursInType - variable does not occur in string") {

    assert((

      false) == (

      false))

  }

  test("variableOccursInType - variable occurs in list element type") {

    assert((

      true) == (

      true))

  }

  test("variableOccursInType - variable does not occur in list of different type") {

    assert((

      false) == (

      false))

  }

  test("variableOccursInType - variable occurs in function domain") {

    assert((

      true) == (

      true))

  }

  test("variableOccursInType - variable occurs in function codomain") {

    assert((

      true) == (

      true))

  }

  test("variableOccursInType - variable does not occur in function with different vars") {

    assert((

      false) == (

      false))

  }

  test("variableOccursInType - variable occurs in optional type") {

    assert((

      true) == (

      true))

  }

  test("variableOccursInType - variable occurs in pair first") {

    assert((

      true) == (

      true))

  }

  test("variableOccursInType - variable occurs in pair second") {

    assert((

      true) == (

      true))

  }

  test("variableOccursInType - variable occurs in either left") {

    assert((

      true) == (

      true))

  }

  test("variableOccursInType - variable occurs in either right") {

    assert((

      true) == (

      true))

  }

  test("variableOccursInType - variable occurs in map key type") {

    assert((

      true) == (

      true))

  }

  test("variableOccursInType - variable occurs in map value type") {

    assert((

      true) == (

      true))

  }

  test("variableOccursInType - variable occurs in set type") {

    assert((

      true) == (

      true))

  }

  test("variableOccursInType - variable occurs in nested list") {

    assert((

      true) == (

      true))

  }

  test("variableOccursInType - variable occurs in list of functions") {

    assert((

      true) == (

      true))

  }

  test("variableOccursInType - variable does not occur in complex type without it") {

    assert((

      false) == (

      false))

  }

  test("variableOccursInType - variable occurs deep in complex type") {

    assert((

      true) == (

      true))

  }

  test("variableOccursInType - variable occurs in forAll body") {

    assert((

      true) == (

      true))

  }

  test("variableOccursInType - variable occurs in forAll bound position") {

    assert((

      true) == (

      true))

  }

  // unifyTypes

  test("unifyTypes - unify identical int32 types") {

    assert((

      {}) == (

      {}))

  }

  test("unifyTypes - unify identical string types") {

    assert((

      {}) == (

      {}))

  }

  test("unifyTypes - unify identical variable types") {

    assert((

      {}) == (

      {}))

  }

  test("unifyTypes - unify variable with int32") {

    assert((

      {a: int32}) == (

      {a: int32}))

  }

  test("unifyTypes - unify int32 with variable") {

    assert((

      {a: int32}) == (

      {a: int32}))

  }

  test("unifyTypes - unify two different variables") {

    assert((

      {a: b}) == (

      {a: b}))

  }

  test("unifyTypes - unify list of variables with list of int32") {

    assert((

      {a: int32}) == (

      {a: int32}))

  }

  test("unifyTypes - unify identical list types") {

    assert((

      {}) == (

      {}))

  }

  test("unifyTypes - unify function types with variables") {

    assert((

      {a: int32, b: string}) == (

      {a: int32, b: string}))

  }

  test("unifyTypes - unify identical function types") {

    assert((

      {}) == (

      {}))

  }

  test("unifyTypes - unify optional types") {

    assert((

      {a: int32}) == (

      {a: int32}))

  }

  test("unifyTypes - unify pair types") {

    assert((

      {a: int32, b: string}) == (

      {a: int32, b: string}))

  }

  test("unifyTypes - unify either types") {

    assert((

      {a: int32, b: string}) == (

      {a: int32, b: string}))

  }

  test("unifyTypes - unify map types") {

    assert((

      {k: string, v: int32}) == (

      {k: string, v: int32}))

  }

  test("unifyTypes - unify set types") {

    assert((

      {a: int32}) == (

      {a: int32}))

  }

  test("unifyTypes - unify unit types") {

    assert((

      {}) == (

      {}))

  }

  test("unifyTypes - fail to unify int32 with string") {

    assert((

      failure) == (

      failure))

  }

  test("unifyTypes - fail to unify list with function") {

    assert((

      failure) == (

      failure))

  }

  test("unifyTypes - occur check: variable with list containing it") {

    assert((

      failure) == (

      failure))

  }

  // joinTypes

  test("joinTypes - join identical int32") {

    assert((

      []) == (

      []))

  }

  test("joinTypes - join identical string") {

    assert((

      []) == (

      []))

  }

  test("joinTypes - join list types") {

    assert((

      [(a ~ int32)]) == (

      [(a ~ int32)]))

  }

  test("joinTypes - join function types") {

    assert((

      [(a ~ int32), (b ~ string)]) == (

      [(a ~ int32), (b ~ string)]))

  }

  test("joinTypes - join optional types") {

    assert((

      [(a ~ int32)]) == (

      [(a ~ int32)]))

  }

  test("joinTypes - join pair types") {

    assert((

      [(a ~ int32), (b ~ string)]) == (

      [(a ~ int32), (b ~ string)]))

  }

  test("joinTypes - join either types") {

    assert((

      [(a ~ int32), (b ~ string)]) == (

      [(a ~ int32), (b ~ string)]))

  }

  test("joinTypes - join map types") {

    assert((

      [(k ~ string), (v ~ int32)]) == (

      [(k ~ string), (v ~ int32)]))

  }

  test("joinTypes - join set types") {

    assert((

      [(a ~ int32)]) == (

      [(a ~ int32)]))

  }

  test("joinTypes - join unit types") {

    assert((

      []) == (

      []))

  }

  test("joinTypes - fail to join int32 with string") {

    assert((

      failure) == (

      failure))

  }

  test("joinTypes - fail to join list with function") {

    assert((

      failure) == (

      failure))

  }

  test("joinTypes - fail to join pair with either") {

    assert((

      failure) == (

      failure))

  }
}
