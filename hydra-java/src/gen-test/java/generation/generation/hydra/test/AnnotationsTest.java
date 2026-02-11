// Note: this is an automatically generated file. Do not edit.
// annotations

package generation.hydra.test;

import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.*;
import java.util.*;
import hydra.util.*;

public class AnnotationsTest {

    // arbitrary annotations

    @Test

    public void testArbitraryAnnotationsSetSingleAnnotationNum1() {

        assertEquals(

            new hydra.core.Term.Annotated(new hydra.core.AnnotatedTerm(new hydra.core.Term.Literal(new hydra.core.Literal.String_("foo")), java.util.Map.ofEntries(java.util.Map.entry(
  new hydra.core.Name("k1"),
  new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42))))))),

            hydra.annotations.Annotations.setTermAnnotation(
  new hydra.core.Name("k1"),
  hydra.util.Maybe.just(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42)))),
  new hydra.core.Term.Literal(new hydra.core.Literal.String_("foo"))));

    }

    @Test

    public void testArbitraryAnnotationsSetSingleAnnotationNum2() {

        assertEquals(

            new hydra.core.Term.Annotated(new hydra.core.AnnotatedTerm(new hydra.core.Term.Literal(new hydra.core.Literal.String_("bar")), java.util.Map.ofEntries(java.util.Map.entry(
  new hydra.core.Name("myKey"),
  new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(-17))))))),

            hydra.annotations.Annotations.setTermAnnotation(
  new hydra.core.Name("myKey"),
  hydra.util.Maybe.just(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(-17)))),
  new hydra.core.Term.Literal(new hydra.core.Literal.String_("bar"))));

    }

    @Test

    public void testArbitraryAnnotationsSetSingleAnnotationNum3() {

        assertEquals(

            new hydra.core.Term.Annotated(new hydra.core.AnnotatedTerm(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(0))), java.util.Map.ofEntries(java.util.Map.entry(
  new hydra.core.Name("x"),
  new hydra.core.Term.Literal(new hydra.core.Literal.String_("hello")))))),

            hydra.annotations.Annotations.setTermAnnotation(
  new hydra.core.Name("x"),
  hydra.util.Maybe.just(new hydra.core.Term.Literal(new hydra.core.Literal.String_("hello"))),
  new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(0)))));

    }

    @Test

    public void testArbitraryAnnotationsGetExistingAnnotationNum1() {

        assertEquals(

            hydra.util.Maybe.just(new hydra.core.Term.Literal(new hydra.core.Literal.String_("value"))),

            hydra.annotations.Annotations.getTermAnnotation(
  new hydra.core.Name("k1"),
  hydra.annotations.Annotations.setTermAnnotation(
    new hydra.core.Name("k1"),
    hydra.util.Maybe.just(new hydra.core.Term.Literal(new hydra.core.Literal.String_("value"))),
    new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42))))));

    }

    @Test

    public void testArbitraryAnnotationsGetExistingAnnotationNum2() {

        assertEquals(

            hydra.util.Maybe.just(new hydra.core.Term.Literal(new hydra.core.Literal.String_(""))),

            hydra.annotations.Annotations.getTermAnnotation(
  new hydra.core.Name("foo"),
  hydra.annotations.Annotations.setTermAnnotation(
    new hydra.core.Name("foo"),
    hydra.util.Maybe.just(new hydra.core.Term.Literal(new hydra.core.Literal.String_(""))),
    new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(99))))));

    }

    @Test

    public void testArbitraryAnnotationsGetExistingAnnotationNum3() {

        assertEquals(

            hydra.util.Maybe.just(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(123)))),

            hydra.annotations.Annotations.getTermAnnotation(
  new hydra.core.Name("key"),
  hydra.annotations.Annotations.setTermAnnotation(
    new hydra.core.Name("key"),
    hydra.util.Maybe.just(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(123)))),
    new hydra.core.Term.Literal(new hydra.core.Literal.String_("test")))));

    }

    @Test

    public void testArbitraryAnnotationsGetMissingAnnotationNum1() {

        assertEquals(

            (hydra.util.Maybe<java.lang.Object>) (hydra.util.Maybe.<java.lang.Object>nothing()),

            hydra.annotations.Annotations.getTermAnnotation(
  new hydra.core.Name("k1"),
  new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int16((short) (42))))));

    }

    @Test

    public void testArbitraryAnnotationsGetMissingAnnotationNum2() {

        assertEquals(

            (hydra.util.Maybe<java.lang.Object>) (hydra.util.Maybe.<java.lang.Object>nothing()),

            hydra.annotations.Annotations.getTermAnnotation(
  new hydra.core.Name("nonexistent"),
  new hydra.core.Term.Literal(new hydra.core.Literal.String_("hello"))));

    }

    @Test

    public void testArbitraryAnnotationsGetMissingAnnotationNum3() {

        assertEquals(

            (hydra.util.Maybe<java.lang.Object>) (hydra.util.Maybe.<java.lang.Object>nothing()),

            hydra.annotations.Annotations.getTermAnnotation(
  new hydra.core.Name("k1"),
  hydra.annotations.Annotations.setTermAnnotation(
    new hydra.core.Name("k2"),
    hydra.util.Maybe.just(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(1)))),
    new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42))))));

    }

    @Test

    public void testArbitraryAnnotationsSetMultipleAnnotationsNum1() {

        assertEquals(

            new hydra.core.Term.Annotated(new hydra.core.AnnotatedTerm(new hydra.core.Term.Literal(new hydra.core.Literal.Boolean_(true)), java.util.Map.ofEntries(
  java.util.Map.entry(
    new hydra.core.Name("k1"),
    new hydra.core.Term.Literal(new hydra.core.Literal.String_("first"))),
  java.util.Map.entry(
    new hydra.core.Name("k2"),
    new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(200))))))),

            hydra.annotations.Annotations.setTermAnnotation(
  new hydra.core.Name("k2"),
  hydra.util.Maybe.just(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(200)))),
  hydra.annotations.Annotations.setTermAnnotation(
    new hydra.core.Name("k1"),
    hydra.util.Maybe.just(new hydra.core.Term.Literal(new hydra.core.Literal.String_("first"))),
    new hydra.core.Term.Literal(new hydra.core.Literal.Boolean_(true)))));

    }

    @Test

    public void testArbitraryAnnotationsSetMultipleAnnotationsNum2() {

        assertEquals(

            new hydra.core.Term.Annotated(new hydra.core.AnnotatedTerm(new hydra.core.Term.Literal(new hydra.core.Literal.String_("test")), java.util.Map.ofEntries(
  java.util.Map.entry(
    new hydra.core.Name("a"),
    new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(-5)))),
  java.util.Map.entry(
    new hydra.core.Name("b"),
    new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(0))))))),

            hydra.annotations.Annotations.setTermAnnotation(
  new hydra.core.Name("b"),
  hydra.util.Maybe.just(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(0)))),
  hydra.annotations.Annotations.setTermAnnotation(
    new hydra.core.Name("a"),
    hydra.util.Maybe.just(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(-5)))),
    new hydra.core.Term.Literal(new hydra.core.Literal.String_("test")))));

    }

    @Test

    public void testArbitraryAnnotationsOuterAnnotationOverridesInnerNum1() {

        assertEquals(

            new hydra.core.Term.Annotated(new hydra.core.AnnotatedTerm(new hydra.core.Term.Literal(new hydra.core.Literal.String_("bar")), java.util.Map.ofEntries(java.util.Map.entry(
  new hydra.core.Name("k1"),
  new hydra.core.Term.Literal(new hydra.core.Literal.String_("outer")))))),

            hydra.annotations.Annotations.setTermAnnotation(
  new hydra.core.Name("k1"),
  hydra.util.Maybe.just(new hydra.core.Term.Literal(new hydra.core.Literal.String_("outer"))),
  hydra.annotations.Annotations.setTermAnnotation(
    new hydra.core.Name("k1"),
    hydra.util.Maybe.just(new hydra.core.Term.Literal(new hydra.core.Literal.String_("inner"))),
    new hydra.core.Term.Literal(new hydra.core.Literal.String_("bar")))));

    }

    @Test

    public void testArbitraryAnnotationsOuterAnnotationOverridesInnerNum2() {

        assertEquals(

            new hydra.core.Term.Annotated(new hydra.core.AnnotatedTerm(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42))), java.util.Map.ofEntries(java.util.Map.entry(
  new hydra.core.Name("x"),
  new hydra.core.Term.Literal(new hydra.core.Literal.String_("new")))))),

            hydra.annotations.Annotations.setTermAnnotation(
  new hydra.core.Name("x"),
  hydra.util.Maybe.just(new hydra.core.Term.Literal(new hydra.core.Literal.String_("new"))),
  hydra.annotations.Annotations.setTermAnnotation(
    new hydra.core.Name("x"),
    hydra.util.Maybe.just(new hydra.core.Term.Literal(new hydra.core.Literal.String_("old"))),
    new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42))))));

    }

    @Test

    public void testArbitraryAnnotationsOuterAnnotationOverridesInnerNum3() {

        assertEquals(

            new hydra.core.Term.Annotated(new hydra.core.AnnotatedTerm(new hydra.core.Term.Literal(new hydra.core.Literal.Boolean_(false)), java.util.Map.ofEntries(java.util.Map.entry(
  new hydra.core.Name("key"),
  new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(999))))))),

            hydra.annotations.Annotations.setTermAnnotation(
  new hydra.core.Name("key"),
  hydra.util.Maybe.just(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(999)))),
  hydra.annotations.Annotations.setTermAnnotation(
    new hydra.core.Name("key"),
    hydra.util.Maybe.just(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(1)))),
    new hydra.core.Term.Literal(new hydra.core.Literal.Boolean_(false)))));

    }

    @Test

    public void testArbitraryAnnotationsUnsetSingleAnnotationNum1() {

        assertEquals(

            new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int64((long) (137)))),

            hydra.annotations.Annotations.setTermAnnotation(
  new hydra.core.Name("k1"),
  (hydra.util.Maybe<hydra.core.Term>) (hydra.util.Maybe.<hydra.core.Term>nothing()),
  hydra.annotations.Annotations.setTermAnnotation(
    new hydra.core.Name("k1"),
    hydra.util.Maybe.just(new hydra.core.Term.Literal(new hydra.core.Literal.String_("foo"))),
    new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int64((long) (137)))))));

    }

    @Test

    public void testArbitraryAnnotationsUnsetSingleAnnotationNum2() {

        assertEquals(

            new hydra.core.Term.Literal(new hydra.core.Literal.String_("test")),

            hydra.annotations.Annotations.setTermAnnotation(
  new hydra.core.Name("x"),
  (hydra.util.Maybe<hydra.core.Term>) (hydra.util.Maybe.<hydra.core.Term>nothing()),
  hydra.annotations.Annotations.setTermAnnotation(
    new hydra.core.Name("x"),
    hydra.util.Maybe.just(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42)))),
    new hydra.core.Term.Literal(new hydra.core.Literal.String_("test")))));

    }

    @Test

    public void testArbitraryAnnotationsUnsetOneOfMultipleAnnotationsNum1() {

        assertEquals(

            new hydra.core.Term.Annotated(new hydra.core.AnnotatedTerm(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int64((long) (137)))), java.util.Map.ofEntries(java.util.Map.entry(
  new hydra.core.Name("k2"),
  new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(200))))))),

            hydra.annotations.Annotations.setTermAnnotation(
  new hydra.core.Name("k1"),
  (hydra.util.Maybe<hydra.core.Term>) (hydra.util.Maybe.<hydra.core.Term>nothing()),
  hydra.annotations.Annotations.setTermAnnotation(
    new hydra.core.Name("k2"),
    hydra.util.Maybe.just(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(200)))),
    hydra.annotations.Annotations.setTermAnnotation(
      new hydra.core.Name("k1"),
      hydra.util.Maybe.just(new hydra.core.Term.Literal(new hydra.core.Literal.String_("first"))),
      new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int64((long) (137))))))));

    }

    @Test

    public void testArbitraryAnnotationsUnsetOneOfMultipleAnnotationsNum2() {

        assertEquals(

            new hydra.core.Term.Annotated(new hydra.core.AnnotatedTerm(new hydra.core.Term.Literal(new hydra.core.Literal.String_("x")), java.util.Map.ofEntries(java.util.Map.entry(
  new hydra.core.Name("a"),
  new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(1))))))),

            hydra.annotations.Annotations.setTermAnnotation(
  new hydra.core.Name("b"),
  (hydra.util.Maybe<hydra.core.Term>) (hydra.util.Maybe.<hydra.core.Term>nothing()),
  hydra.annotations.Annotations.setTermAnnotation(
    new hydra.core.Name("b"),
    hydra.util.Maybe.just(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(2)))),
    hydra.annotations.Annotations.setTermAnnotation(
      new hydra.core.Name("a"),
      hydra.util.Maybe.just(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(1)))),
      new hydra.core.Term.Literal(new hydra.core.Literal.String_("x"))))));

    }

    // descriptions

    @Test

    public void testDescriptionsSetDescriptionNum1() {

        assertEquals(

            new hydra.core.Term.Annotated(new hydra.core.AnnotatedTerm(new hydra.core.Term.Literal(new hydra.core.Literal.String_("foo")), java.util.Map.ofEntries(java.util.Map.entry(
  new hydra.core.Name("description"),
  new hydra.core.Term.Literal(new hydra.core.Literal.String_("my description")))))),

            hydra.annotations.Annotations.setTermDescription(
  hydra.util.Maybe.just("my description"),
  new hydra.core.Term.Literal(new hydra.core.Literal.String_("foo"))));

    }

    @Test

    public void testDescriptionsSetDescriptionNum2() {

        assertEquals(

            new hydra.core.Term.Annotated(new hydra.core.AnnotatedTerm(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42))), java.util.Map.ofEntries(java.util.Map.entry(
  new hydra.core.Name("description"),
  new hydra.core.Term.Literal(new hydra.core.Literal.String_("")))))),

            hydra.annotations.Annotations.setTermDescription(
  hydra.util.Maybe.just(""),
  new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42)))));

    }

    @Test

    public void testDescriptionsSetDescriptionNum3() {

        assertEquals(

            new hydra.core.Term.Annotated(new hydra.core.AnnotatedTerm(new hydra.core.Term.Literal(new hydra.core.Literal.Boolean_(true)), java.util.Map.ofEntries(java.util.Map.entry(
  new hydra.core.Name("description"),
  new hydra.core.Term.Literal(new hydra.core.Literal.String_("A longer description with spaces")))))),

            hydra.annotations.Annotations.setTermDescription(
  hydra.util.Maybe.just("A longer description with spaces"),
  new hydra.core.Term.Literal(new hydra.core.Literal.Boolean_(true))));

    }

    @Test

    public void testDescriptionsGetExistingDescriptionNum1() {

        assertEquals(

            (hydra.compute.FlowState<hydra.graph.Graph, hydra.util.Maybe<String>>) ((hydra.compute.FlowState<hydra.graph.Graph, hydra.util.Maybe<String>>) (new hydra.compute.FlowState<hydra.graph.Graph, hydra.util.Maybe<String>>(hydra.util.Maybe.just(hydra.util.Maybe.just("hello")), hydra.lexical.Lexical.emptyGraph(), hydra.monads.Monads.emptyTrace()))),

            ((((java.util.function.Function<hydra.compute.Flow<hydra.graph.Graph, hydra.util.Maybe<String>>, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.compute.Trace, hydra.compute.FlowState<hydra.graph.Graph, hydra.util.Maybe<String>>>>>) ((java.util.function.Function<hydra.compute.Flow<hydra.graph.Graph, hydra.util.Maybe<String>>, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.compute.Trace, hydra.compute.FlowState<hydra.graph.Graph, hydra.util.Maybe<String>>>>>) (wrapped -> (wrapped).value))).apply(hydra.annotations.Annotations.getTermDescription(hydra.annotations.Annotations.setTermDescription(
  hydra.util.Maybe.just("hello"),
  new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42))))))).apply(hydra.lexical.Lexical.emptyGraph())).apply(hydra.monads.Monads.emptyTrace()));

    }

    @Test

    public void testDescriptionsGetExistingDescriptionNum2() {

        assertEquals(

            (hydra.compute.FlowState<hydra.graph.Graph, hydra.util.Maybe<String>>) ((hydra.compute.FlowState<hydra.graph.Graph, hydra.util.Maybe<String>>) (new hydra.compute.FlowState<hydra.graph.Graph, hydra.util.Maybe<String>>(hydra.util.Maybe.just(hydra.util.Maybe.just("")), hydra.lexical.Lexical.emptyGraph(), hydra.monads.Monads.emptyTrace()))),

            ((((java.util.function.Function<hydra.compute.Flow<hydra.graph.Graph, hydra.util.Maybe<String>>, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.compute.Trace, hydra.compute.FlowState<hydra.graph.Graph, hydra.util.Maybe<String>>>>>) ((java.util.function.Function<hydra.compute.Flow<hydra.graph.Graph, hydra.util.Maybe<String>>, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.compute.Trace, hydra.compute.FlowState<hydra.graph.Graph, hydra.util.Maybe<String>>>>>) (wrapped -> (wrapped).value))).apply(hydra.annotations.Annotations.getTermDescription(hydra.annotations.Annotations.setTermDescription(
  hydra.util.Maybe.just(""),
  new hydra.core.Term.Literal(new hydra.core.Literal.String_("test")))))).apply(hydra.lexical.Lexical.emptyGraph())).apply(hydra.monads.Monads.emptyTrace()));

    }

    @Test

    public void testDescriptionsGetExistingDescriptionNum3() {

        assertEquals(

            (hydra.compute.FlowState<hydra.graph.Graph, hydra.util.Maybe<String>>) ((hydra.compute.FlowState<hydra.graph.Graph, hydra.util.Maybe<String>>) (new hydra.compute.FlowState<hydra.graph.Graph, hydra.util.Maybe<String>>(hydra.util.Maybe.just(hydra.util.Maybe.just("desc")), hydra.lexical.Lexical.emptyGraph(), hydra.monads.Monads.emptyTrace()))),

            ((((java.util.function.Function<hydra.compute.Flow<hydra.graph.Graph, hydra.util.Maybe<String>>, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.compute.Trace, hydra.compute.FlowState<hydra.graph.Graph, hydra.util.Maybe<String>>>>>) ((java.util.function.Function<hydra.compute.Flow<hydra.graph.Graph, hydra.util.Maybe<String>>, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.compute.Trace, hydra.compute.FlowState<hydra.graph.Graph, hydra.util.Maybe<String>>>>>) (wrapped -> (wrapped).value))).apply(hydra.annotations.Annotations.getTermDescription(hydra.annotations.Annotations.setTermDescription(
  hydra.util.Maybe.just("desc"),
  new hydra.core.Term.Literal(new hydra.core.Literal.Boolean_(false)))))).apply(hydra.lexical.Lexical.emptyGraph())).apply(hydra.monads.Monads.emptyTrace()));

    }

    @Test

    public void testDescriptionsGetMissingDescriptionNum1() {

        assertEquals(

            (hydra.compute.FlowState<hydra.graph.Graph, hydra.util.Maybe<java.lang.Object>>) ((hydra.compute.FlowState<hydra.graph.Graph, hydra.util.Maybe<java.lang.Object>>) (new hydra.compute.FlowState<hydra.graph.Graph, hydra.util.Maybe<java.lang.Object>>(hydra.util.Maybe.just((hydra.util.Maybe<java.lang.Object>) (hydra.util.Maybe.<java.lang.Object>nothing())), hydra.lexical.Lexical.emptyGraph(), hydra.monads.Monads.emptyTrace()))),

            ((((java.util.function.Function<hydra.compute.Flow<hydra.graph.Graph, hydra.util.Maybe<String>>, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.compute.Trace, hydra.compute.FlowState<hydra.graph.Graph, hydra.util.Maybe<String>>>>>) ((java.util.function.Function<hydra.compute.Flow<hydra.graph.Graph, hydra.util.Maybe<String>>, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.compute.Trace, hydra.compute.FlowState<hydra.graph.Graph, hydra.util.Maybe<String>>>>>) (wrapped -> (wrapped).value))).apply(hydra.annotations.Annotations.getTermDescription(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int16((short) (42))))))).apply(hydra.lexical.Lexical.emptyGraph())).apply(hydra.monads.Monads.emptyTrace()));

    }

    @Test

    public void testDescriptionsGetMissingDescriptionNum2() {

        assertEquals(

            (hydra.compute.FlowState<hydra.graph.Graph, hydra.util.Maybe<java.lang.Object>>) ((hydra.compute.FlowState<hydra.graph.Graph, hydra.util.Maybe<java.lang.Object>>) (new hydra.compute.FlowState<hydra.graph.Graph, hydra.util.Maybe<java.lang.Object>>(hydra.util.Maybe.just((hydra.util.Maybe<java.lang.Object>) (hydra.util.Maybe.<java.lang.Object>nothing())), hydra.lexical.Lexical.emptyGraph(), hydra.monads.Monads.emptyTrace()))),

            ((((java.util.function.Function<hydra.compute.Flow<hydra.graph.Graph, hydra.util.Maybe<String>>, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.compute.Trace, hydra.compute.FlowState<hydra.graph.Graph, hydra.util.Maybe<String>>>>>) ((java.util.function.Function<hydra.compute.Flow<hydra.graph.Graph, hydra.util.Maybe<String>>, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.compute.Trace, hydra.compute.FlowState<hydra.graph.Graph, hydra.util.Maybe<String>>>>>) (wrapped -> (wrapped).value))).apply(hydra.annotations.Annotations.getTermDescription(new hydra.core.Term.Literal(new hydra.core.Literal.String_("no description here"))))).apply(hydra.lexical.Lexical.emptyGraph())).apply(hydra.monads.Monads.emptyTrace()));

    }

    @Test

    public void testDescriptionsGetMissingDescriptionNum3() {

        assertEquals(

            (hydra.compute.FlowState<hydra.graph.Graph, hydra.util.Maybe<java.lang.Object>>) ((hydra.compute.FlowState<hydra.graph.Graph, hydra.util.Maybe<java.lang.Object>>) (new hydra.compute.FlowState<hydra.graph.Graph, hydra.util.Maybe<java.lang.Object>>(hydra.util.Maybe.just((hydra.util.Maybe<java.lang.Object>) (hydra.util.Maybe.<java.lang.Object>nothing())), hydra.lexical.Lexical.emptyGraph(), hydra.monads.Monads.emptyTrace()))),

            ((((java.util.function.Function<hydra.compute.Flow<hydra.graph.Graph, hydra.util.Maybe<String>>, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.compute.Trace, hydra.compute.FlowState<hydra.graph.Graph, hydra.util.Maybe<String>>>>>) ((java.util.function.Function<hydra.compute.Flow<hydra.graph.Graph, hydra.util.Maybe<String>>, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.compute.Trace, hydra.compute.FlowState<hydra.graph.Graph, hydra.util.Maybe<String>>>>>) (wrapped -> (wrapped).value))).apply(hydra.annotations.Annotations.getTermDescription(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(0)))))).apply(hydra.lexical.Lexical.emptyGraph())).apply(hydra.monads.Monads.emptyTrace()));

    }

    @Test

    public void testDescriptionsOuterDescriptionOverridesInnerNum1() {

        assertEquals(

            new hydra.core.Term.Annotated(new hydra.core.AnnotatedTerm(new hydra.core.Term.Literal(new hydra.core.Literal.String_("bar")), java.util.Map.ofEntries(java.util.Map.entry(
  new hydra.core.Name("description"),
  new hydra.core.Term.Literal(new hydra.core.Literal.String_("outer")))))),

            hydra.annotations.Annotations.setTermDescription(
  hydra.util.Maybe.just("outer"),
  hydra.annotations.Annotations.setTermDescription(
    hydra.util.Maybe.just("inner"),
    new hydra.core.Term.Literal(new hydra.core.Literal.String_("bar")))));

    }

    @Test

    public void testDescriptionsOuterDescriptionOverridesInnerNum2() {

        assertEquals(

            new hydra.core.Term.Annotated(new hydra.core.AnnotatedTerm(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(99))), java.util.Map.ofEntries(java.util.Map.entry(
  new hydra.core.Name("description"),
  new hydra.core.Term.Literal(new hydra.core.Literal.String_("new")))))),

            hydra.annotations.Annotations.setTermDescription(
  hydra.util.Maybe.just("new"),
  hydra.annotations.Annotations.setTermDescription(
    hydra.util.Maybe.just("old"),
    new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(99))))));

    }

    @Test

    public void testDescriptionsUnsetDescriptionNum1() {

        assertEquals(

            new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int64((long) (137)))),

            hydra.annotations.Annotations.setTermDescription(
  (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()),
  hydra.annotations.Annotations.setTermDescription(
    hydra.util.Maybe.just("desc"),
    new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int64((long) (137)))))));

    }

    @Test

    public void testDescriptionsUnsetDescriptionNum2() {

        assertEquals(

            new hydra.core.Term.Literal(new hydra.core.Literal.String_("test")),

            hydra.annotations.Annotations.setTermDescription(
  (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()),
  hydra.annotations.Annotations.setTermDescription(
    hydra.util.Maybe.just("to be removed"),
    new hydra.core.Term.Literal(new hydra.core.Literal.String_("test")))));

    }

    // layered annotations

    @Test

    public void testLayeredAnnotationsGetAnnotationFromUnannotatedTerm() {

        assertEquals(

            (hydra.util.Maybe<java.lang.Object>) (hydra.util.Maybe.<java.lang.Object>nothing()),

            hydra.annotations.Annotations.getTermAnnotation(
  new hydra.core.Name("one"),
  new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42)))));

    }

    @Test

    public void testLayeredAnnotationsGetAnnotationFromSinglyAnnotatedTerm() {

        assertEquals(

            hydra.util.Maybe.just(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(1)))),

            hydra.annotations.Annotations.getTermAnnotation(
  new hydra.core.Name("one"),
  new hydra.core.Term.Annotated(new hydra.core.AnnotatedTerm(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42))), java.util.Map.ofEntries(java.util.Map.entry(
    new hydra.core.Name("one"),
    new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(1)))))))));

    }

    @Test

    public void testLayeredAnnotationsGetInnerAnnotationFromDoublyAnnotatedTerm() {

        assertEquals(

            hydra.util.Maybe.just(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(1)))),

            hydra.annotations.Annotations.getTermAnnotation(
  new hydra.core.Name("one"),
  new hydra.core.Term.Annotated(new hydra.core.AnnotatedTerm(new hydra.core.Term.Annotated(new hydra.core.AnnotatedTerm(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42))), java.util.Map.ofEntries(java.util.Map.entry(
    new hydra.core.Name("one"),
    new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(1))))))), java.util.Map.ofEntries(java.util.Map.entry(
    new hydra.core.Name("two"),
    new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(2)))))))));

    }

    @Test

    public void testLayeredAnnotationsGetOuterAnnotationFromDoublyAnnotatedTerm() {

        assertEquals(

            hydra.util.Maybe.just(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(2)))),

            hydra.annotations.Annotations.getTermAnnotation(
  new hydra.core.Name("two"),
  new hydra.core.Term.Annotated(new hydra.core.AnnotatedTerm(new hydra.core.Term.Annotated(new hydra.core.AnnotatedTerm(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42))), java.util.Map.ofEntries(java.util.Map.entry(
    new hydra.core.Name("one"),
    new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(1))))))), java.util.Map.ofEntries(java.util.Map.entry(
    new hydra.core.Name("two"),
    new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(2)))))))));

    }

    @Test

    public void testLayeredAnnotationsGetNonNegoverriddenAnnotationFromTriplyAnnotatedTerm() {

        assertEquals(

            hydra.util.Maybe.just(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(2)))),

            hydra.annotations.Annotations.getTermAnnotation(
  new hydra.core.Name("two"),
  new hydra.core.Term.Annotated(new hydra.core.AnnotatedTerm(new hydra.core.Term.Annotated(new hydra.core.AnnotatedTerm(new hydra.core.Term.Annotated(new hydra.core.AnnotatedTerm(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42))), java.util.Map.ofEntries(java.util.Map.entry(
    new hydra.core.Name("one"),
    new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(1))))))), java.util.Map.ofEntries(java.util.Map.entry(
    new hydra.core.Name("two"),
    new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(2))))))), java.util.Map.ofEntries(java.util.Map.entry(
    new hydra.core.Name("one"),
    new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(99)))))))));

    }

    @Test

    public void testLayeredAnnotationsOuterAnnotationOverridesInnerInLayeredTerm() {

        assertEquals(

            hydra.util.Maybe.just(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(99)))),

            hydra.annotations.Annotations.getTermAnnotation(
  new hydra.core.Name("one"),
  new hydra.core.Term.Annotated(new hydra.core.AnnotatedTerm(new hydra.core.Term.Annotated(new hydra.core.AnnotatedTerm(new hydra.core.Term.Annotated(new hydra.core.AnnotatedTerm(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42))), java.util.Map.ofEntries(java.util.Map.entry(
    new hydra.core.Name("one"),
    new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(1))))))), java.util.Map.ofEntries(java.util.Map.entry(
    new hydra.core.Name("two"),
    new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(2))))))), java.util.Map.ofEntries(java.util.Map.entry(
    new hydra.core.Name("one"),
    new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(99)))))))));

    }
}
