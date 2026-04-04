// Note: this is an automatically generated file. Do not edit.
// strip

package generation.hydra.test

import org.scalatest.funsuite.AnyFunSuite

class StripTest extends AnyFunSuite {

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
}
