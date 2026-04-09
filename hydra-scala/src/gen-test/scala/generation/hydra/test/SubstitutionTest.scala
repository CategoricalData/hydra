// Note: this is an automatically generated file. Do not edit.
// substitution

package generation.hydra.test

import org.scalatest.funsuite.AnyFunSuite

class SubstitutionTest extends AnyFunSuite {

  // substInType

  test("substInType - empty substitution returns type unchanged") {

    assert((

      string) == (

      string))

  }

  test("substInType - substitute type variable with int32") {

    assert((

      int32) == (

      int32))

  }

  test("substInType - non-matching variable unchanged") {

    assert((

      b) == (

      b))

  }

  test("substInType - substitute in function domain") {

    assert((

      (int32 → string)) == (

      (int32 → string)))

  }

  test("substInType - substitute in function codomain") {

    assert((

      (int32 → string)) == (

      (int32 → string)))

  }

  test("substInType - substitute in list element type") {

    assert((

      list<int32>) == (

      list<int32>))

  }

  test("substInType - substitute in optional type") {

    assert((

      maybe<string>) == (

      maybe<string>))

  }

  test("substInType - substitute in pair type both sides") {

    assert((

      (int32, int32)) == (

      (int32, int32)))

  }

  test("substInType - substitute in either type") {

    assert((

      either<string, int32>) == (

      either<string, int32>))

  }

  test("substInType - substitute in map key type") {

    assert((

      map<string, int32>) == (

      map<string, int32>))

  }

  test("substInType - substitute in set type") {

    assert((

      set<int32>) == (

      set<int32>))

  }

  test("substInType - nested substitution in list of pairs") {

    assert((

      list<(int32, string)>) == (

      list<(int32, string)>))

  }

  test("substInType - multiple substitutions") {

    assert((

      (int32, string)) == (

      (int32, string)))

  }

  test("substInType - forAll bound variable not substituted") {

    assert((

      (∀a.(a → a))) == (

      (∀a.(a → a))))

  }

  test("substInType - forAll free variable substituted") {

    assert((

      (∀a.(a → string))) == (

      (∀a.(a → string))))

  }
}
