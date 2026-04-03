// Note: this is an automatically generated file. Do not edit.
// annotations

package generation.hydra.test

import org.scalatest.funsuite.AnyFunSuite

class AnnotationsTest extends AnyFunSuite {

  // arbitrary annotations

  test("arbitrary annotations - set single annotation #1") {

    assert((

      inject(hydra.core.Term){annotated=record(hydra.core.AnnotatedTerm){body=inject(hydra.core.Term){literal=inject(hydra.core.Literal){string="foo"}}, annotation={wrap(hydra.core.Name){"k1"}=inject(hydra.core.Term){literal=inject(hydra.core.Literal){integer=inject(hydra.core.IntegerValue){int32=42:int32}}}}}}) == (

      inject(hydra.core.Term){annotated=record(hydra.core.AnnotatedTerm){body=inject(hydra.core.Term){literal=inject(hydra.core.Literal){string="foo"}}, annotation={wrap(hydra.core.Name){"k1"}=inject(hydra.core.Term){literal=inject(hydra.core.Literal){integer=inject(hydra.core.IntegerValue){int32=42:int32}}}}}}))

  }

  test("arbitrary annotations - set single annotation #2") {

    assert((

      inject(hydra.core.Term){annotated=record(hydra.core.AnnotatedTerm){body=inject(hydra.core.Term){literal=inject(hydra.core.Literal){string="bar"}}, annotation={wrap(hydra.core.Name){"myKey"}=inject(hydra.core.Term){literal=inject(hydra.core.Literal){integer=inject(hydra.core.IntegerValue){int32=-17:int32}}}}}}) == (

      inject(hydra.core.Term){annotated=record(hydra.core.AnnotatedTerm){body=inject(hydra.core.Term){literal=inject(hydra.core.Literal){string="bar"}}, annotation={wrap(hydra.core.Name){"myKey"}=inject(hydra.core.Term){literal=inject(hydra.core.Literal){integer=inject(hydra.core.IntegerValue){int32=-17:int32}}}}}}))

  }

  test("arbitrary annotations - set single annotation #3") {

    assert((

      inject(hydra.core.Term){annotated=record(hydra.core.AnnotatedTerm){body=inject(hydra.core.Term){literal=inject(hydra.core.Literal){integer=inject(hydra.core.IntegerValue){int32=0:int32}}}, annotation={wrap(hydra.core.Name){"x"}=inject(hydra.core.Term){literal=inject(hydra.core.Literal){string="hello"}}}}}) == (

      inject(hydra.core.Term){annotated=record(hydra.core.AnnotatedTerm){body=inject(hydra.core.Term){literal=inject(hydra.core.Literal){integer=inject(hydra.core.IntegerValue){int32=0:int32}}}, annotation={wrap(hydra.core.Name){"x"}=inject(hydra.core.Term){literal=inject(hydra.core.Literal){string="hello"}}}}}))

  }

  test("arbitrary annotations - get existing annotation #1") {

    assert((

      just(inject(hydra.core.Term){literal=inject(hydra.core.Literal){string="value"}})) == (

      just(inject(hydra.core.Term){literal=inject(hydra.core.Literal){string="value"}})))

  }

  test("arbitrary annotations - get existing annotation #2") {

    assert((

      just(inject(hydra.core.Term){literal=inject(hydra.core.Literal){string=""}})) == (

      just(inject(hydra.core.Term){literal=inject(hydra.core.Literal){string=""}})))

  }

  test("arbitrary annotations - get existing annotation #3") {

    assert((

      just(inject(hydra.core.Term){literal=inject(hydra.core.Literal){integer=inject(hydra.core.IntegerValue){int32=123:int32}}})) == (

      just(inject(hydra.core.Term){literal=inject(hydra.core.Literal){integer=inject(hydra.core.IntegerValue){int32=123:int32}}})))

  }

  test("arbitrary annotations - get missing annotation #1") {

    assert((

      nothing) == (

      nothing))

  }

  test("arbitrary annotations - get missing annotation #2") {

    assert((

      nothing) == (

      nothing))

  }

  test("arbitrary annotations - get missing annotation #3") {

    assert((

      nothing) == (

      nothing))

  }

  test("arbitrary annotations - set multiple annotations #1") {

    assert((

      inject(hydra.core.Term){annotated=record(hydra.core.AnnotatedTerm){body=inject(hydra.core.Term){literal=inject(hydra.core.Literal){boolean=true}}, annotation={wrap(hydra.core.Name){"k1"}=inject(hydra.core.Term){literal=inject(hydra.core.Literal){string="first"}}, wrap(hydra.core.Name){"k2"}=inject(hydra.core.Term){literal=inject(hydra.core.Literal){integer=inject(hydra.core.IntegerValue){int32=200:int32}}}}}}) == (

      inject(hydra.core.Term){annotated=record(hydra.core.AnnotatedTerm){body=inject(hydra.core.Term){literal=inject(hydra.core.Literal){boolean=true}}, annotation={wrap(hydra.core.Name){"k1"}=inject(hydra.core.Term){literal=inject(hydra.core.Literal){string="first"}}, wrap(hydra.core.Name){"k2"}=inject(hydra.core.Term){literal=inject(hydra.core.Literal){integer=inject(hydra.core.IntegerValue){int32=200:int32}}}}}}))

  }

  test("arbitrary annotations - set multiple annotations #2") {

    assert((

      inject(hydra.core.Term){annotated=record(hydra.core.AnnotatedTerm){body=inject(hydra.core.Term){literal=inject(hydra.core.Literal){string="test"}}, annotation={wrap(hydra.core.Name){"a"}=inject(hydra.core.Term){literal=inject(hydra.core.Literal){integer=inject(hydra.core.IntegerValue){int32=-5:int32}}}, wrap(hydra.core.Name){"b"}=inject(hydra.core.Term){literal=inject(hydra.core.Literal){integer=inject(hydra.core.IntegerValue){int32=0:int32}}}}}}) == (

      inject(hydra.core.Term){annotated=record(hydra.core.AnnotatedTerm){body=inject(hydra.core.Term){literal=inject(hydra.core.Literal){string="test"}}, annotation={wrap(hydra.core.Name){"a"}=inject(hydra.core.Term){literal=inject(hydra.core.Literal){integer=inject(hydra.core.IntegerValue){int32=-5:int32}}}, wrap(hydra.core.Name){"b"}=inject(hydra.core.Term){literal=inject(hydra.core.Literal){integer=inject(hydra.core.IntegerValue){int32=0:int32}}}}}}))

  }

  test("arbitrary annotations - outer annotation overrides inner #1") {

    assert((

      inject(hydra.core.Term){annotated=record(hydra.core.AnnotatedTerm){body=inject(hydra.core.Term){literal=inject(hydra.core.Literal){string="bar"}}, annotation={wrap(hydra.core.Name){"k1"}=inject(hydra.core.Term){literal=inject(hydra.core.Literal){string="outer"}}}}}) == (

      inject(hydra.core.Term){annotated=record(hydra.core.AnnotatedTerm){body=inject(hydra.core.Term){literal=inject(hydra.core.Literal){string="bar"}}, annotation={wrap(hydra.core.Name){"k1"}=inject(hydra.core.Term){literal=inject(hydra.core.Literal){string="outer"}}}}}))

  }

  test("arbitrary annotations - outer annotation overrides inner #2") {

    assert((

      inject(hydra.core.Term){annotated=record(hydra.core.AnnotatedTerm){body=inject(hydra.core.Term){literal=inject(hydra.core.Literal){integer=inject(hydra.core.IntegerValue){int32=42:int32}}}, annotation={wrap(hydra.core.Name){"x"}=inject(hydra.core.Term){literal=inject(hydra.core.Literal){string="new"}}}}}) == (

      inject(hydra.core.Term){annotated=record(hydra.core.AnnotatedTerm){body=inject(hydra.core.Term){literal=inject(hydra.core.Literal){integer=inject(hydra.core.IntegerValue){int32=42:int32}}}, annotation={wrap(hydra.core.Name){"x"}=inject(hydra.core.Term){literal=inject(hydra.core.Literal){string="new"}}}}}))

  }

  test("arbitrary annotations - outer annotation overrides inner #3") {

    assert((

      inject(hydra.core.Term){annotated=record(hydra.core.AnnotatedTerm){body=inject(hydra.core.Term){literal=inject(hydra.core.Literal){boolean=false}}, annotation={wrap(hydra.core.Name){"key"}=inject(hydra.core.Term){literal=inject(hydra.core.Literal){integer=inject(hydra.core.IntegerValue){int32=999:int32}}}}}}) == (

      inject(hydra.core.Term){annotated=record(hydra.core.AnnotatedTerm){body=inject(hydra.core.Term){literal=inject(hydra.core.Literal){boolean=false}}, annotation={wrap(hydra.core.Name){"key"}=inject(hydra.core.Term){literal=inject(hydra.core.Literal){integer=inject(hydra.core.IntegerValue){int32=999:int32}}}}}}))

  }

  test("arbitrary annotations - unset single annotation #1") {

    assert((

      inject(hydra.core.Term){literal=inject(hydra.core.Literal){integer=inject(hydra.core.IntegerValue){int64=137:int64}}}) == (

      inject(hydra.core.Term){literal=inject(hydra.core.Literal){integer=inject(hydra.core.IntegerValue){int64=137:int64}}}))

  }

  test("arbitrary annotations - unset single annotation #2") {

    assert((

      inject(hydra.core.Term){literal=inject(hydra.core.Literal){string="test"}}) == (

      inject(hydra.core.Term){literal=inject(hydra.core.Literal){string="test"}}))

  }

  test("arbitrary annotations - unset one of multiple annotations #1") {

    assert((

      inject(hydra.core.Term){annotated=record(hydra.core.AnnotatedTerm){body=inject(hydra.core.Term){literal=inject(hydra.core.Literal){integer=inject(hydra.core.IntegerValue){int64=137:int64}}}, annotation={wrap(hydra.core.Name){"k2"}=inject(hydra.core.Term){literal=inject(hydra.core.Literal){integer=inject(hydra.core.IntegerValue){int32=200:int32}}}}}}) == (

      inject(hydra.core.Term){annotated=record(hydra.core.AnnotatedTerm){body=inject(hydra.core.Term){literal=inject(hydra.core.Literal){integer=inject(hydra.core.IntegerValue){int64=137:int64}}}, annotation={wrap(hydra.core.Name){"k2"}=inject(hydra.core.Term){literal=inject(hydra.core.Literal){integer=inject(hydra.core.IntegerValue){int32=200:int32}}}}}}))

  }

  test("arbitrary annotations - unset one of multiple annotations #2") {

    assert((

      inject(hydra.core.Term){annotated=record(hydra.core.AnnotatedTerm){body=inject(hydra.core.Term){literal=inject(hydra.core.Literal){string="x"}}, annotation={wrap(hydra.core.Name){"a"}=inject(hydra.core.Term){literal=inject(hydra.core.Literal){integer=inject(hydra.core.IntegerValue){int32=1:int32}}}}}}) == (

      inject(hydra.core.Term){annotated=record(hydra.core.AnnotatedTerm){body=inject(hydra.core.Term){literal=inject(hydra.core.Literal){string="x"}}, annotation={wrap(hydra.core.Name){"a"}=inject(hydra.core.Term){literal=inject(hydra.core.Literal){integer=inject(hydra.core.IntegerValue){int32=1:int32}}}}}}))

  }

  // descriptions

  test("descriptions - set description #1") {

    assert((

      inject(hydra.core.Term){annotated=record(hydra.core.AnnotatedTerm){body=inject(hydra.core.Term){literal=inject(hydra.core.Literal){string="foo"}}, annotation={wrap(hydra.core.Name){"description"}=inject(hydra.core.Term){literal=inject(hydra.core.Literal){string="my description"}}}}}) == (

      inject(hydra.core.Term){annotated=record(hydra.core.AnnotatedTerm){body=inject(hydra.core.Term){literal=inject(hydra.core.Literal){string="foo"}}, annotation={wrap(hydra.core.Name){"description"}=inject(hydra.core.Term){literal=inject(hydra.core.Literal){string="my description"}}}}}))

  }

  test("descriptions - set description #2") {

    assert((

      inject(hydra.core.Term){annotated=record(hydra.core.AnnotatedTerm){body=inject(hydra.core.Term){literal=inject(hydra.core.Literal){integer=inject(hydra.core.IntegerValue){int32=42:int32}}}, annotation={wrap(hydra.core.Name){"description"}=inject(hydra.core.Term){literal=inject(hydra.core.Literal){string=""}}}}}) == (

      inject(hydra.core.Term){annotated=record(hydra.core.AnnotatedTerm){body=inject(hydra.core.Term){literal=inject(hydra.core.Literal){integer=inject(hydra.core.IntegerValue){int32=42:int32}}}, annotation={wrap(hydra.core.Name){"description"}=inject(hydra.core.Term){literal=inject(hydra.core.Literal){string=""}}}}}))

  }

  test("descriptions - set description #3") {

    assert((

      inject(hydra.core.Term){annotated=record(hydra.core.AnnotatedTerm){body=inject(hydra.core.Term){literal=inject(hydra.core.Literal){boolean=true}}, annotation={wrap(hydra.core.Name){"description"}=inject(hydra.core.Term){literal=inject(hydra.core.Literal){string="A longer description with spaces"}}}}}) == (

      inject(hydra.core.Term){annotated=record(hydra.core.AnnotatedTerm){body=inject(hydra.core.Term){literal=inject(hydra.core.Literal){boolean=true}}, annotation={wrap(hydra.core.Name){"description"}=inject(hydra.core.Term){literal=inject(hydra.core.Literal){string="A longer description with spaces"}}}}}))

  }

  test("descriptions - get existing description #1") {

    assert((

      right(just("hello"))) == (

      right(just("hello"))))

  }

  test("descriptions - get existing description #2") {

    assert((

      right(just(""))) == (

      right(just(""))))

  }

  test("descriptions - get existing description #3") {

    assert((

      right(just("desc"))) == (

      right(just("desc"))))

  }

  test("descriptions - get missing description #1") {

    assert((

      right(nothing)) == (

      right(nothing)))

  }

  test("descriptions - get missing description #2") {

    assert((

      right(nothing)) == (

      right(nothing)))

  }

  test("descriptions - get missing description #3") {

    assert((

      right(nothing)) == (

      right(nothing)))

  }

  test("descriptions - outer description overrides inner #1") {

    assert((

      inject(hydra.core.Term){annotated=record(hydra.core.AnnotatedTerm){body=inject(hydra.core.Term){literal=inject(hydra.core.Literal){string="bar"}}, annotation={wrap(hydra.core.Name){"description"}=inject(hydra.core.Term){literal=inject(hydra.core.Literal){string="outer"}}}}}) == (

      inject(hydra.core.Term){annotated=record(hydra.core.AnnotatedTerm){body=inject(hydra.core.Term){literal=inject(hydra.core.Literal){string="bar"}}, annotation={wrap(hydra.core.Name){"description"}=inject(hydra.core.Term){literal=inject(hydra.core.Literal){string="outer"}}}}}))

  }

  test("descriptions - outer description overrides inner #2") {

    assert((

      inject(hydra.core.Term){annotated=record(hydra.core.AnnotatedTerm){body=inject(hydra.core.Term){literal=inject(hydra.core.Literal){integer=inject(hydra.core.IntegerValue){int32=99:int32}}}, annotation={wrap(hydra.core.Name){"description"}=inject(hydra.core.Term){literal=inject(hydra.core.Literal){string="new"}}}}}) == (

      inject(hydra.core.Term){annotated=record(hydra.core.AnnotatedTerm){body=inject(hydra.core.Term){literal=inject(hydra.core.Literal){integer=inject(hydra.core.IntegerValue){int32=99:int32}}}, annotation={wrap(hydra.core.Name){"description"}=inject(hydra.core.Term){literal=inject(hydra.core.Literal){string="new"}}}}}))

  }

  test("descriptions - unset description #1") {

    assert((

      inject(hydra.core.Term){literal=inject(hydra.core.Literal){integer=inject(hydra.core.IntegerValue){int64=137:int64}}}) == (

      inject(hydra.core.Term){literal=inject(hydra.core.Literal){integer=inject(hydra.core.IntegerValue){int64=137:int64}}}))

  }

  test("descriptions - unset description #2") {

    assert((

      inject(hydra.core.Term){literal=inject(hydra.core.Literal){string="test"}}) == (

      inject(hydra.core.Term){literal=inject(hydra.core.Literal){string="test"}}))

  }

  // layered annotations

  test("layered annotations - get annotation from unannotated term") {

    assert((

      nothing) == (

      nothing))

  }

  test("layered annotations - get annotation from singly annotated term") {

    assert((

      just(inject(hydra.core.Term){literal=inject(hydra.core.Literal){integer=inject(hydra.core.IntegerValue){int32=1:int32}}})) == (

      just(inject(hydra.core.Term){literal=inject(hydra.core.Literal){integer=inject(hydra.core.IntegerValue){int32=1:int32}}})))

  }

  test("layered annotations - get inner annotation from doubly annotated term") {

    assert((

      just(inject(hydra.core.Term){literal=inject(hydra.core.Literal){integer=inject(hydra.core.IntegerValue){int32=1:int32}}})) == (

      just(inject(hydra.core.Term){literal=inject(hydra.core.Literal){integer=inject(hydra.core.IntegerValue){int32=1:int32}}})))

  }

  test("layered annotations - get outer annotation from doubly annotated term") {

    assert((

      just(inject(hydra.core.Term){literal=inject(hydra.core.Literal){integer=inject(hydra.core.IntegerValue){int32=2:int32}}})) == (

      just(inject(hydra.core.Term){literal=inject(hydra.core.Literal){integer=inject(hydra.core.IntegerValue){int32=2:int32}}})))

  }

  test("layered annotations - get non-overridden annotation from triply annotated term") {

    assert((

      just(inject(hydra.core.Term){literal=inject(hydra.core.Literal){integer=inject(hydra.core.IntegerValue){int32=2:int32}}})) == (

      just(inject(hydra.core.Term){literal=inject(hydra.core.Literal){integer=inject(hydra.core.IntegerValue){int32=2:int32}}})))

  }

  test("layered annotations - outer annotation overrides inner in layered term") {

    assert((

      just(inject(hydra.core.Term){literal=inject(hydra.core.Literal){integer=inject(hydra.core.IntegerValue){int32=99:int32}}})) == (

      just(inject(hydra.core.Term){literal=inject(hydra.core.Literal){integer=inject(hydra.core.IntegerValue){int32=99:int32}}})))

  }
}
