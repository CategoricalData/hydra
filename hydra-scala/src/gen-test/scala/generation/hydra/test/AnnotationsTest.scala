// Note: this is an automatically generated file. Do not edit.
// annotations

package generation.hydra.test

import org.scalatest.funsuite.AnyFunSuite

class AnnotationsTest extends AnyFunSuite {

  // arbitrary annotations

  test("arbitrary annotations - set single annotation #1") {

    assert((

      hydra.annotations.setTermAnnotation("k1")(Some(hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(42)))))(hydra.core.Term.literal(hydra.core.Literal.string("foo")))) == (

      hydra.core.Term.annotated(hydra.core.AnnotatedTerm(hydra.core.Term.literal(hydra.core.Literal.string("foo")), Map("k1" -> hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(42))))))))

  }

  test("arbitrary annotations - set single annotation #2") {

    assert((

      hydra.annotations.setTermAnnotation("myKey")(Some(hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(-17)))))(hydra.core.Term.literal(hydra.core.Literal.string("bar")))) == (

      hydra.core.Term.annotated(hydra.core.AnnotatedTerm(hydra.core.Term.literal(hydra.core.Literal.string("bar")), Map("myKey" -> hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(-17))))))))

  }

  test("arbitrary annotations - set single annotation #3") {

    assert((

      hydra.annotations.setTermAnnotation("x")(Some(hydra.core.Term.literal(hydra.core.Literal.string("hello"))))(hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(0))))) == (

      hydra.core.Term.annotated(hydra.core.AnnotatedTerm(hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(0))), Map("x" -> hydra.core.Term.literal(hydra.core.Literal.string("hello")))))))

  }

  test("arbitrary annotations - get existing annotation #1") {

    assert((

      hydra.annotations.getTermAnnotation("k1")(hydra.annotations.setTermAnnotation("k1")(Some(hydra.core.Term.literal(hydra.core.Literal.string("value"))))(hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(42)))))) == (

      Some(hydra.core.Term.literal(hydra.core.Literal.string("value")))))

  }

  test("arbitrary annotations - get existing annotation #2") {

    assert((

      hydra.annotations.getTermAnnotation("foo")(hydra.annotations.setTermAnnotation("foo")(Some(hydra.core.Term.literal(hydra.core.Literal.string(""))))(hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(99)))))) == (

      Some(hydra.core.Term.literal(hydra.core.Literal.string("")))))

  }

  test("arbitrary annotations - get existing annotation #3") {

    assert((

      hydra.annotations.getTermAnnotation("key")(hydra.annotations.setTermAnnotation("key")(Some(hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(123)))))(hydra.core.Term.literal(hydra.core.Literal.string("test"))))) == (

      Some(hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(123))))))

  }

  test("arbitrary annotations - get missing annotation #1") {

    assert((

      hydra.annotations.getTermAnnotation("k1")(hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int16(42.toShort))))) == (

      None))

  }

  test("arbitrary annotations - get missing annotation #2") {

    assert((

      hydra.annotations.getTermAnnotation("nonexistent")(hydra.core.Term.literal(hydra.core.Literal.string("hello")))) == (

      None))

  }

  test("arbitrary annotations - get missing annotation #3") {

    assert((

      hydra.annotations.getTermAnnotation("k1")(hydra.annotations.setTermAnnotation("k2")(Some(hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(1)))))(hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(42)))))) == (

      None))

  }

  test("arbitrary annotations - set multiple annotations #1") {

    assert((

      hydra.annotations.setTermAnnotation("k2")(Some(hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(200)))))(hydra.annotations.setTermAnnotation("k1")(Some(hydra.core.Term.literal(hydra.core.Literal.string("first"))))(hydra.core.Term.literal(hydra.core.Literal.boolean(true))))) == (

      hydra.core.Term.annotated(hydra.core.AnnotatedTerm(hydra.core.Term.literal(hydra.core.Literal.boolean(true)), Map("k1" -> hydra.core.Term.literal(hydra.core.Literal.string("first")), "k2" -> hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(200))))))))

  }

  test("arbitrary annotations - set multiple annotations #2") {

    assert((

      hydra.annotations.setTermAnnotation("b")(Some(hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(0)))))(hydra.annotations.setTermAnnotation("a")(Some(hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(-5)))))(hydra.core.Term.literal(hydra.core.Literal.string("test"))))) == (

      hydra.core.Term.annotated(hydra.core.AnnotatedTerm(hydra.core.Term.literal(hydra.core.Literal.string("test")), Map("a" -> hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(-5))), "b" -> hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(0))))))))

  }

  test("arbitrary annotations - outer annotation overrides inner #1") {

    assert((

      hydra.annotations.setTermAnnotation("k1")(Some(hydra.core.Term.literal(hydra.core.Literal.string("outer"))))(hydra.annotations.setTermAnnotation("k1")(Some(hydra.core.Term.literal(hydra.core.Literal.string("inner"))))(hydra.core.Term.literal(hydra.core.Literal.string("bar"))))) == (

      hydra.core.Term.annotated(hydra.core.AnnotatedTerm(hydra.core.Term.literal(hydra.core.Literal.string("bar")), Map("k1" -> hydra.core.Term.literal(hydra.core.Literal.string("outer")))))))

  }

  test("arbitrary annotations - outer annotation overrides inner #2") {

    assert((

      hydra.annotations.setTermAnnotation("x")(Some(hydra.core.Term.literal(hydra.core.Literal.string("new"))))(hydra.annotations.setTermAnnotation("x")(Some(hydra.core.Term.literal(hydra.core.Literal.string("old"))))(hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(42)))))) == (

      hydra.core.Term.annotated(hydra.core.AnnotatedTerm(hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(42))), Map("x" -> hydra.core.Term.literal(hydra.core.Literal.string("new")))))))

  }

  test("arbitrary annotations - outer annotation overrides inner #3") {

    assert((

      hydra.annotations.setTermAnnotation("key")(Some(hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(999)))))(hydra.annotations.setTermAnnotation("key")(Some(hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(1)))))(hydra.core.Term.literal(hydra.core.Literal.boolean(false))))) == (

      hydra.core.Term.annotated(hydra.core.AnnotatedTerm(hydra.core.Term.literal(hydra.core.Literal.boolean(false)), Map("key" -> hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(999))))))))

  }

  test("arbitrary annotations - unset single annotation #1") {

    assert((

      hydra.annotations.setTermAnnotation("k1")(None)(hydra.annotations.setTermAnnotation("k1")(Some(hydra.core.Term.literal(hydra.core.Literal.string("foo"))))(hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int64(137L)))))) == (

      hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int64(137L)))))

  }

  test("arbitrary annotations - unset single annotation #2") {

    assert((

      hydra.annotations.setTermAnnotation("x")(None)(hydra.annotations.setTermAnnotation("x")(Some(hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(42)))))(hydra.core.Term.literal(hydra.core.Literal.string("test"))))) == (

      hydra.core.Term.literal(hydra.core.Literal.string("test"))))

  }

  test("arbitrary annotations - unset one of multiple annotations #1") {

    assert((

      hydra.annotations.setTermAnnotation("k1")(None)(hydra.annotations.setTermAnnotation("k2")(Some(hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(200)))))(hydra.annotations.setTermAnnotation("k1")(Some(hydra.core.Term.literal(hydra.core.Literal.string("first"))))(hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int64(137L))))))) == (

      hydra.core.Term.annotated(hydra.core.AnnotatedTerm(hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int64(137L))), Map("k2" -> hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(200))))))))

  }

  test("arbitrary annotations - unset one of multiple annotations #2") {

    assert((

      hydra.annotations.setTermAnnotation("b")(None)(hydra.annotations.setTermAnnotation("b")(Some(hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(2)))))(hydra.annotations.setTermAnnotation("a")(Some(hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(1)))))(hydra.core.Term.literal(hydra.core.Literal.string("x")))))) == (

      hydra.core.Term.annotated(hydra.core.AnnotatedTerm(hydra.core.Term.literal(hydra.core.Literal.string("x")), Map("a" -> hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(1))))))))

  }

  // descriptions

  test("descriptions - set description #1") {

    assert((

      hydra.annotations.setTermDescription(Some("my description"))(hydra.core.Term.literal(hydra.core.Literal.string("foo")))) == (

      hydra.core.Term.annotated(hydra.core.AnnotatedTerm(hydra.core.Term.literal(hydra.core.Literal.string("foo")), Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("my description")))))))

  }

  test("descriptions - set description #2") {

    assert((

      hydra.annotations.setTermDescription(Some(""))(hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(42))))) == (

      hydra.core.Term.annotated(hydra.core.AnnotatedTerm(hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(42))), Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("")))))))

  }

  test("descriptions - set description #3") {

    assert((

      hydra.annotations.setTermDescription(Some("A longer description with spaces"))(hydra.core.Term.literal(hydra.core.Literal.boolean(true)))) == (

      hydra.core.Term.annotated(hydra.core.AnnotatedTerm(hydra.core.Term.literal(hydra.core.Literal.boolean(true)), Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("A longer description with spaces")))))))

  }

  test("descriptions - get existing description #1") {

    assert((

      hydra.annotations.getTermDescription(hydra.lexical.emptyContext)(hydra.lexical.emptyGraph)(hydra.annotations.setTermDescription(Some("hello"))(hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(42)))))) == (

      Right(Some("hello"))))

  }

  test("descriptions - get existing description #2") {

    assert((

      hydra.annotations.getTermDescription(hydra.lexical.emptyContext)(hydra.lexical.emptyGraph)(hydra.annotations.setTermDescription(Some(""))(hydra.core.Term.literal(hydra.core.Literal.string("test"))))) == (

      Right(Some(""))))

  }

  test("descriptions - get existing description #3") {

    assert((

      hydra.annotations.getTermDescription(hydra.lexical.emptyContext)(hydra.lexical.emptyGraph)(hydra.annotations.setTermDescription(Some("desc"))(hydra.core.Term.literal(hydra.core.Literal.boolean(false))))) == (

      Right(Some("desc"))))

  }

  test("descriptions - get missing description #1") {

    assert((

      hydra.annotations.getTermDescription(hydra.lexical.emptyContext)(hydra.lexical.emptyGraph)(hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int16(42.toShort))))) == (

      Right(None)))

  }

  test("descriptions - get missing description #2") {

    assert((

      hydra.annotations.getTermDescription(hydra.lexical.emptyContext)(hydra.lexical.emptyGraph)(hydra.core.Term.literal(hydra.core.Literal.string("no description here")))) == (

      Right(None)))

  }

  test("descriptions - get missing description #3") {

    assert((

      hydra.annotations.getTermDescription(hydra.lexical.emptyContext)(hydra.lexical.emptyGraph)(hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(0))))) == (

      Right(None)))

  }

  test("descriptions - outer description overrides inner #1") {

    assert((

      hydra.annotations.setTermDescription(Some("outer"))(hydra.annotations.setTermDescription(Some("inner"))(hydra.core.Term.literal(hydra.core.Literal.string("bar"))))) == (

      hydra.core.Term.annotated(hydra.core.AnnotatedTerm(hydra.core.Term.literal(hydra.core.Literal.string("bar")), Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("outer")))))))

  }

  test("descriptions - outer description overrides inner #2") {

    assert((

      hydra.annotations.setTermDescription(Some("new"))(hydra.annotations.setTermDescription(Some("old"))(hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(99)))))) == (

      hydra.core.Term.annotated(hydra.core.AnnotatedTerm(hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(99))), Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("new")))))))

  }

  test("descriptions - unset description #1") {

    assert((

      hydra.annotations.setTermDescription(None)(hydra.annotations.setTermDescription(Some("desc"))(hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int64(137L)))))) == (

      hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int64(137L)))))

  }

  test("descriptions - unset description #2") {

    assert((

      hydra.annotations.setTermDescription(None)(hydra.annotations.setTermDescription(Some("to be removed"))(hydra.core.Term.literal(hydra.core.Literal.string("test"))))) == (

      hydra.core.Term.literal(hydra.core.Literal.string("test"))))

  }

  // layered annotations

  test("layered annotations - get annotation from unannotated term") {

    assert((

      hydra.annotations.getTermAnnotation("one")(hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(42))))) == (

      None))

  }

  test("layered annotations - get annotation from singly annotated term") {

    assert((

      hydra.annotations.getTermAnnotation("one")(hydra.core.Term.annotated(hydra.core.AnnotatedTerm(hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(42))), Map("one" -> hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(1)))))))) == (

      Some(hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(1))))))

  }

  test("layered annotations - get inner annotation from doubly annotated term") {

    assert((

      hydra.annotations.getTermAnnotation("one")(hydra.core.Term.annotated(hydra.core.AnnotatedTerm(hydra.core.Term.annotated(hydra.core.AnnotatedTerm(hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(42))), Map("one" -> hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(1)))))), Map("two" -> hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(2)))))))) == (

      Some(hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(1))))))

  }

  test("layered annotations - get outer annotation from doubly annotated term") {

    assert((

      hydra.annotations.getTermAnnotation("two")(hydra.core.Term.annotated(hydra.core.AnnotatedTerm(hydra.core.Term.annotated(hydra.core.AnnotatedTerm(hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(42))), Map("one" -> hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(1)))))), Map("two" -> hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(2)))))))) == (

      Some(hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(2))))))

  }

  test("layered annotations - get non-overridden annotation from triply annotated term") {

    assert((

      hydra.annotations.getTermAnnotation("two")(hydra.core.Term.annotated(hydra.core.AnnotatedTerm(hydra.core.Term.annotated(hydra.core.AnnotatedTerm(hydra.core.Term.annotated(hydra.core.AnnotatedTerm(hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(42))), Map("one" -> hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(1)))))), Map("two" -> hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(2)))))), Map("one" -> hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(99)))))))) == (

      Some(hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(2))))))

  }

  test("layered annotations - outer annotation overrides inner in layered term") {

    assert((

      hydra.annotations.getTermAnnotation("one")(hydra.core.Term.annotated(hydra.core.AnnotatedTerm(hydra.core.Term.annotated(hydra.core.AnnotatedTerm(hydra.core.Term.annotated(hydra.core.AnnotatedTerm(hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(42))), Map("one" -> hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(1)))))), Map("two" -> hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(2)))))), Map("one" -> hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(99)))))))) == (

      Some(hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(99))))))

  }
}
