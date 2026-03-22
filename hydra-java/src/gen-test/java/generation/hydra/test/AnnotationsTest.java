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

            new hydra.core.Term.Annotated(new hydra.core.AnnotatedTerm(new hydra.core.Term.Literal(new hydra.core.Literal.String_("foo")), hydra.util.PersistentMap.ofEntries(hydra.util.PersistentMap.entry(
  new hydra.core.Name("k1"),
  new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42))))))),

            hydra.Annotations.setTermAnnotation(
  new hydra.core.Name("k1"),
  hydra.util.Maybe.just(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42)))),
  new hydra.core.Term.Literal(new hydra.core.Literal.String_("foo"))));

    }

    @Test

    public void testArbitraryAnnotationsSetSingleAnnotationNum2() {

        assertEquals(

            new hydra.core.Term.Annotated(new hydra.core.AnnotatedTerm(new hydra.core.Term.Literal(new hydra.core.Literal.String_("bar")), hydra.util.PersistentMap.ofEntries(hydra.util.PersistentMap.entry(
  new hydra.core.Name("myKey"),
  new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(-17))))))),

            hydra.Annotations.setTermAnnotation(
  new hydra.core.Name("myKey"),
  hydra.util.Maybe.just(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(-17)))),
  new hydra.core.Term.Literal(new hydra.core.Literal.String_("bar"))));

    }

    @Test

    public void testArbitraryAnnotationsSetSingleAnnotationNum3() {

        assertEquals(

            new hydra.core.Term.Annotated(new hydra.core.AnnotatedTerm(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(0))), hydra.util.PersistentMap.ofEntries(hydra.util.PersistentMap.entry(
  new hydra.core.Name("x"),
  new hydra.core.Term.Literal(new hydra.core.Literal.String_("hello")))))),

            hydra.Annotations.setTermAnnotation(
  new hydra.core.Name("x"),
  hydra.util.Maybe.just(new hydra.core.Term.Literal(new hydra.core.Literal.String_("hello"))),
  new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(0)))));

    }

    @Test

    public void testArbitraryAnnotationsGetExistingAnnotationNum1() {

        assertEquals(

            hydra.util.Maybe.just(new hydra.core.Term.Literal(new hydra.core.Literal.String_("value"))),

            hydra.Annotations.getTermAnnotation(
  new hydra.core.Name("k1"),
  hydra.Annotations.setTermAnnotation(
    new hydra.core.Name("k1"),
    hydra.util.Maybe.just(new hydra.core.Term.Literal(new hydra.core.Literal.String_("value"))),
    new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42))))));

    }

    @Test

    public void testArbitraryAnnotationsGetExistingAnnotationNum2() {

        assertEquals(

            hydra.util.Maybe.just(new hydra.core.Term.Literal(new hydra.core.Literal.String_(""))),

            hydra.Annotations.getTermAnnotation(
  new hydra.core.Name("foo"),
  hydra.Annotations.setTermAnnotation(
    new hydra.core.Name("foo"),
    hydra.util.Maybe.just(new hydra.core.Term.Literal(new hydra.core.Literal.String_(""))),
    new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(99))))));

    }

    @Test

    public void testArbitraryAnnotationsGetExistingAnnotationNum3() {

        assertEquals(

            hydra.util.Maybe.just(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(123)))),

            hydra.Annotations.getTermAnnotation(
  new hydra.core.Name("key"),
  hydra.Annotations.setTermAnnotation(
    new hydra.core.Name("key"),
    hydra.util.Maybe.just(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(123)))),
    new hydra.core.Term.Literal(new hydra.core.Literal.String_("test")))));

    }

    @Test

    public <T0> void testArbitraryAnnotationsGetMissingAnnotationNum1() {

        assertEquals(

            (hydra.util.Maybe<T0>) (hydra.util.Maybe.<T0>nothing()),

            hydra.Annotations.getTermAnnotation(
  new hydra.core.Name("k1"),
  new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int16((short) (42))))));

    }

    @Test

    public <T0> void testArbitraryAnnotationsGetMissingAnnotationNum2() {

        assertEquals(

            (hydra.util.Maybe<T0>) (hydra.util.Maybe.<T0>nothing()),

            hydra.Annotations.getTermAnnotation(
  new hydra.core.Name("nonexistent"),
  new hydra.core.Term.Literal(new hydra.core.Literal.String_("hello"))));

    }

    @Test

    public <T0> void testArbitraryAnnotationsGetMissingAnnotationNum3() {

        assertEquals(

            (hydra.util.Maybe<T0>) (hydra.util.Maybe.<T0>nothing()),

            hydra.Annotations.getTermAnnotation(
  new hydra.core.Name("k1"),
  hydra.Annotations.setTermAnnotation(
    new hydra.core.Name("k2"),
    hydra.util.Maybe.just(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(1)))),
    new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42))))));

    }

    @Test

    public void testArbitraryAnnotationsSetMultipleAnnotationsNum1() {

        assertEquals(

            new hydra.core.Term.Annotated(new hydra.core.AnnotatedTerm(new hydra.core.Term.Literal(new hydra.core.Literal.Boolean_(true)), hydra.util.PersistentMap.ofEntries(
  hydra.util.PersistentMap.entry(
    new hydra.core.Name("k1"),
    new hydra.core.Term.Literal(new hydra.core.Literal.String_("first"))),
  hydra.util.PersistentMap.entry(
    new hydra.core.Name("k2"),
    new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(200))))))),

            hydra.Annotations.setTermAnnotation(
  new hydra.core.Name("k2"),
  hydra.util.Maybe.just(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(200)))),
  hydra.Annotations.setTermAnnotation(
    new hydra.core.Name("k1"),
    hydra.util.Maybe.just(new hydra.core.Term.Literal(new hydra.core.Literal.String_("first"))),
    new hydra.core.Term.Literal(new hydra.core.Literal.Boolean_(true)))));

    }

    @Test

    public void testArbitraryAnnotationsSetMultipleAnnotationsNum2() {

        assertEquals(

            new hydra.core.Term.Annotated(new hydra.core.AnnotatedTerm(new hydra.core.Term.Literal(new hydra.core.Literal.String_("test")), hydra.util.PersistentMap.ofEntries(
  hydra.util.PersistentMap.entry(
    new hydra.core.Name("a"),
    new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(-5)))),
  hydra.util.PersistentMap.entry(
    new hydra.core.Name("b"),
    new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(0))))))),

            hydra.Annotations.setTermAnnotation(
  new hydra.core.Name("b"),
  hydra.util.Maybe.just(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(0)))),
  hydra.Annotations.setTermAnnotation(
    new hydra.core.Name("a"),
    hydra.util.Maybe.just(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(-5)))),
    new hydra.core.Term.Literal(new hydra.core.Literal.String_("test")))));

    }

    @Test

    public void testArbitraryAnnotationsOuterAnnotationOverridesInnerNum1() {

        assertEquals(

            new hydra.core.Term.Annotated(new hydra.core.AnnotatedTerm(new hydra.core.Term.Literal(new hydra.core.Literal.String_("bar")), hydra.util.PersistentMap.ofEntries(hydra.util.PersistentMap.entry(
  new hydra.core.Name("k1"),
  new hydra.core.Term.Literal(new hydra.core.Literal.String_("outer")))))),

            hydra.Annotations.setTermAnnotation(
  new hydra.core.Name("k1"),
  hydra.util.Maybe.just(new hydra.core.Term.Literal(new hydra.core.Literal.String_("outer"))),
  hydra.Annotations.setTermAnnotation(
    new hydra.core.Name("k1"),
    hydra.util.Maybe.just(new hydra.core.Term.Literal(new hydra.core.Literal.String_("inner"))),
    new hydra.core.Term.Literal(new hydra.core.Literal.String_("bar")))));

    }

    @Test

    public void testArbitraryAnnotationsOuterAnnotationOverridesInnerNum2() {

        assertEquals(

            new hydra.core.Term.Annotated(new hydra.core.AnnotatedTerm(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42))), hydra.util.PersistentMap.ofEntries(hydra.util.PersistentMap.entry(
  new hydra.core.Name("x"),
  new hydra.core.Term.Literal(new hydra.core.Literal.String_("new")))))),

            hydra.Annotations.setTermAnnotation(
  new hydra.core.Name("x"),
  hydra.util.Maybe.just(new hydra.core.Term.Literal(new hydra.core.Literal.String_("new"))),
  hydra.Annotations.setTermAnnotation(
    new hydra.core.Name("x"),
    hydra.util.Maybe.just(new hydra.core.Term.Literal(new hydra.core.Literal.String_("old"))),
    new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42))))));

    }

    @Test

    public void testArbitraryAnnotationsOuterAnnotationOverridesInnerNum3() {

        assertEquals(

            new hydra.core.Term.Annotated(new hydra.core.AnnotatedTerm(new hydra.core.Term.Literal(new hydra.core.Literal.Boolean_(false)), hydra.util.PersistentMap.ofEntries(hydra.util.PersistentMap.entry(
  new hydra.core.Name("key"),
  new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(999))))))),

            hydra.Annotations.setTermAnnotation(
  new hydra.core.Name("key"),
  hydra.util.Maybe.just(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(999)))),
  hydra.Annotations.setTermAnnotation(
    new hydra.core.Name("key"),
    hydra.util.Maybe.just(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(1)))),
    new hydra.core.Term.Literal(new hydra.core.Literal.Boolean_(false)))));

    }

    @Test

    public void testArbitraryAnnotationsUnsetSingleAnnotationNum1() {

        assertEquals(

            new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int64((long) (137)))),

            hydra.Annotations.setTermAnnotation(
  new hydra.core.Name("k1"),
  (hydra.util.Maybe<hydra.core.Term>) (hydra.util.Maybe.<hydra.core.Term>nothing()),
  hydra.Annotations.setTermAnnotation(
    new hydra.core.Name("k1"),
    hydra.util.Maybe.just(new hydra.core.Term.Literal(new hydra.core.Literal.String_("foo"))),
    new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int64((long) (137)))))));

    }

    @Test

    public void testArbitraryAnnotationsUnsetSingleAnnotationNum2() {

        assertEquals(

            new hydra.core.Term.Literal(new hydra.core.Literal.String_("test")),

            hydra.Annotations.setTermAnnotation(
  new hydra.core.Name("x"),
  (hydra.util.Maybe<hydra.core.Term>) (hydra.util.Maybe.<hydra.core.Term>nothing()),
  hydra.Annotations.setTermAnnotation(
    new hydra.core.Name("x"),
    hydra.util.Maybe.just(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42)))),
    new hydra.core.Term.Literal(new hydra.core.Literal.String_("test")))));

    }

    @Test

    public void testArbitraryAnnotationsUnsetOneOfMultipleAnnotationsNum1() {

        assertEquals(

            new hydra.core.Term.Annotated(new hydra.core.AnnotatedTerm(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int64((long) (137)))), hydra.util.PersistentMap.ofEntries(hydra.util.PersistentMap.entry(
  new hydra.core.Name("k2"),
  new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(200))))))),

            hydra.Annotations.setTermAnnotation(
  new hydra.core.Name("k1"),
  (hydra.util.Maybe<hydra.core.Term>) (hydra.util.Maybe.<hydra.core.Term>nothing()),
  hydra.Annotations.setTermAnnotation(
    new hydra.core.Name("k2"),
    hydra.util.Maybe.just(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(200)))),
    hydra.Annotations.setTermAnnotation(
      new hydra.core.Name("k1"),
      hydra.util.Maybe.just(new hydra.core.Term.Literal(new hydra.core.Literal.String_("first"))),
      new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int64((long) (137))))))));

    }

    @Test

    public void testArbitraryAnnotationsUnsetOneOfMultipleAnnotationsNum2() {

        assertEquals(

            new hydra.core.Term.Annotated(new hydra.core.AnnotatedTerm(new hydra.core.Term.Literal(new hydra.core.Literal.String_("x")), hydra.util.PersistentMap.ofEntries(hydra.util.PersistentMap.entry(
  new hydra.core.Name("a"),
  new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(1))))))),

            hydra.Annotations.setTermAnnotation(
  new hydra.core.Name("b"),
  (hydra.util.Maybe<hydra.core.Term>) (hydra.util.Maybe.<hydra.core.Term>nothing()),
  hydra.Annotations.setTermAnnotation(
    new hydra.core.Name("b"),
    hydra.util.Maybe.just(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(2)))),
    hydra.Annotations.setTermAnnotation(
      new hydra.core.Name("a"),
      hydra.util.Maybe.just(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(1)))),
      new hydra.core.Term.Literal(new hydra.core.Literal.String_("x"))))));

    }

    // descriptions

    @Test

    public void testDescriptionsSetDescriptionNum1() {

        assertEquals(

            new hydra.core.Term.Annotated(new hydra.core.AnnotatedTerm(new hydra.core.Term.Literal(new hydra.core.Literal.String_("foo")), hydra.util.PersistentMap.ofEntries(hydra.util.PersistentMap.entry(
  new hydra.core.Name("description"),
  new hydra.core.Term.Literal(new hydra.core.Literal.String_("my description")))))),

            hydra.Annotations.setTermDescription(
  hydra.util.Maybe.just("my description"),
  new hydra.core.Term.Literal(new hydra.core.Literal.String_("foo"))));

    }

    @Test

    public void testDescriptionsSetDescriptionNum2() {

        assertEquals(

            new hydra.core.Term.Annotated(new hydra.core.AnnotatedTerm(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42))), hydra.util.PersistentMap.ofEntries(hydra.util.PersistentMap.entry(
  new hydra.core.Name("description"),
  new hydra.core.Term.Literal(new hydra.core.Literal.String_("")))))),

            hydra.Annotations.setTermDescription(
  hydra.util.Maybe.just(""),
  new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42)))));

    }

    @Test

    public void testDescriptionsSetDescriptionNum3() {

        assertEquals(

            new hydra.core.Term.Annotated(new hydra.core.AnnotatedTerm(new hydra.core.Term.Literal(new hydra.core.Literal.Boolean_(true)), hydra.util.PersistentMap.ofEntries(hydra.util.PersistentMap.entry(
  new hydra.core.Name("description"),
  new hydra.core.Term.Literal(new hydra.core.Literal.String_("A longer description with spaces")))))),

            hydra.Annotations.setTermDescription(
  hydra.util.Maybe.just("A longer description with spaces"),
  new hydra.core.Term.Literal(new hydra.core.Literal.Boolean_(true))));

    }

    @Test

    public <T1> void testDescriptionsGetExistingDescriptionNum1() {

        assertEquals(

            hydra.util.Either.<T1, hydra.util.Maybe<String>>right(hydra.util.Maybe.just("hello")),

            hydra.Annotations.getTermDescription(
  hydra.Lexical.emptyContext(),
  hydra.Lexical.emptyGraph(),
  hydra.Annotations.setTermDescription(
    hydra.util.Maybe.just("hello"),
    new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42))))));

    }

    @Test

    public <T1> void testDescriptionsGetExistingDescriptionNum2() {

        assertEquals(

            hydra.util.Either.<T1, hydra.util.Maybe<String>>right(hydra.util.Maybe.just("")),

            hydra.Annotations.getTermDescription(
  hydra.Lexical.emptyContext(),
  hydra.Lexical.emptyGraph(),
  hydra.Annotations.setTermDescription(
    hydra.util.Maybe.just(""),
    new hydra.core.Term.Literal(new hydra.core.Literal.String_("test")))));

    }

    @Test

    public <T1> void testDescriptionsGetExistingDescriptionNum3() {

        assertEquals(

            hydra.util.Either.<T1, hydra.util.Maybe<String>>right(hydra.util.Maybe.just("desc")),

            hydra.Annotations.getTermDescription(
  hydra.Lexical.emptyContext(),
  hydra.Lexical.emptyGraph(),
  hydra.Annotations.setTermDescription(
    hydra.util.Maybe.just("desc"),
    new hydra.core.Term.Literal(new hydra.core.Literal.Boolean_(false)))));

    }

    @Test

    public <T0, T1> void testDescriptionsGetMissingDescriptionNum1() {

        assertEquals(

            hydra.util.Either.<T1, hydra.util.Maybe<T0>>right((hydra.util.Maybe<T0>) (hydra.util.Maybe.<T0>nothing())),

            hydra.Annotations.getTermDescription(
  hydra.Lexical.emptyContext(),
  hydra.Lexical.emptyGraph(),
  new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int16((short) (42))))));

    }

    @Test

    public <T0, T1> void testDescriptionsGetMissingDescriptionNum2() {

        assertEquals(

            hydra.util.Either.<T1, hydra.util.Maybe<T0>>right((hydra.util.Maybe<T0>) (hydra.util.Maybe.<T0>nothing())),

            hydra.Annotations.getTermDescription(
  hydra.Lexical.emptyContext(),
  hydra.Lexical.emptyGraph(),
  new hydra.core.Term.Literal(new hydra.core.Literal.String_("no description here"))));

    }

    @Test

    public <T0, T1> void testDescriptionsGetMissingDescriptionNum3() {

        assertEquals(

            hydra.util.Either.<T1, hydra.util.Maybe<T0>>right((hydra.util.Maybe<T0>) (hydra.util.Maybe.<T0>nothing())),

            hydra.Annotations.getTermDescription(
  hydra.Lexical.emptyContext(),
  hydra.Lexical.emptyGraph(),
  new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(0)))));

    }

    @Test

    public void testDescriptionsOuterDescriptionOverridesInnerNum1() {

        assertEquals(

            new hydra.core.Term.Annotated(new hydra.core.AnnotatedTerm(new hydra.core.Term.Literal(new hydra.core.Literal.String_("bar")), hydra.util.PersistentMap.ofEntries(hydra.util.PersistentMap.entry(
  new hydra.core.Name("description"),
  new hydra.core.Term.Literal(new hydra.core.Literal.String_("outer")))))),

            hydra.Annotations.setTermDescription(
  hydra.util.Maybe.just("outer"),
  hydra.Annotations.setTermDescription(
    hydra.util.Maybe.just("inner"),
    new hydra.core.Term.Literal(new hydra.core.Literal.String_("bar")))));

    }

    @Test

    public void testDescriptionsOuterDescriptionOverridesInnerNum2() {

        assertEquals(

            new hydra.core.Term.Annotated(new hydra.core.AnnotatedTerm(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(99))), hydra.util.PersistentMap.ofEntries(hydra.util.PersistentMap.entry(
  new hydra.core.Name("description"),
  new hydra.core.Term.Literal(new hydra.core.Literal.String_("new")))))),

            hydra.Annotations.setTermDescription(
  hydra.util.Maybe.just("new"),
  hydra.Annotations.setTermDescription(
    hydra.util.Maybe.just("old"),
    new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(99))))));

    }

    @Test

    public void testDescriptionsUnsetDescriptionNum1() {

        assertEquals(

            new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int64((long) (137)))),

            hydra.Annotations.setTermDescription(
  (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()),
  hydra.Annotations.setTermDescription(
    hydra.util.Maybe.just("desc"),
    new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int64((long) (137)))))));

    }

    @Test

    public void testDescriptionsUnsetDescriptionNum2() {

        assertEquals(

            new hydra.core.Term.Literal(new hydra.core.Literal.String_("test")),

            hydra.Annotations.setTermDescription(
  (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()),
  hydra.Annotations.setTermDescription(
    hydra.util.Maybe.just("to be removed"),
    new hydra.core.Term.Literal(new hydra.core.Literal.String_("test")))));

    }

    // layered annotations

    @Test

    public <T0> void testLayeredAnnotationsGetAnnotationFromUnannotatedTerm() {

        assertEquals(

            (hydra.util.Maybe<T0>) (hydra.util.Maybe.<T0>nothing()),

            hydra.Annotations.getTermAnnotation(
  new hydra.core.Name("one"),
  new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42)))));

    }

    @Test

    public void testLayeredAnnotationsGetAnnotationFromSinglyAnnotatedTerm() {

        assertEquals(

            hydra.util.Maybe.just(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(1)))),

            hydra.Annotations.getTermAnnotation(
  new hydra.core.Name("one"),
  new hydra.core.Term.Annotated(new hydra.core.AnnotatedTerm(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42))), hydra.util.PersistentMap.ofEntries(hydra.util.PersistentMap.entry(
    new hydra.core.Name("one"),
    new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(1)))))))));

    }

    @Test

    public void testLayeredAnnotationsGetInnerAnnotationFromDoublyAnnotatedTerm() {

        assertEquals(

            hydra.util.Maybe.just(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(1)))),

            hydra.Annotations.getTermAnnotation(
  new hydra.core.Name("one"),
  new hydra.core.Term.Annotated(new hydra.core.AnnotatedTerm(new hydra.core.Term.Annotated(new hydra.core.AnnotatedTerm(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42))), hydra.util.PersistentMap.ofEntries(hydra.util.PersistentMap.entry(
    new hydra.core.Name("one"),
    new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(1))))))), hydra.util.PersistentMap.ofEntries(hydra.util.PersistentMap.entry(
    new hydra.core.Name("two"),
    new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(2)))))))));

    }

    @Test

    public void testLayeredAnnotationsGetOuterAnnotationFromDoublyAnnotatedTerm() {

        assertEquals(

            hydra.util.Maybe.just(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(2)))),

            hydra.Annotations.getTermAnnotation(
  new hydra.core.Name("two"),
  new hydra.core.Term.Annotated(new hydra.core.AnnotatedTerm(new hydra.core.Term.Annotated(new hydra.core.AnnotatedTerm(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42))), hydra.util.PersistentMap.ofEntries(hydra.util.PersistentMap.entry(
    new hydra.core.Name("one"),
    new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(1))))))), hydra.util.PersistentMap.ofEntries(hydra.util.PersistentMap.entry(
    new hydra.core.Name("two"),
    new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(2)))))))));

    }

    @Test

    public void testLayeredAnnotationsGetNonNegoverriddenAnnotationFromTriplyAnnotatedTerm() {

        assertEquals(

            hydra.util.Maybe.just(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(2)))),

            hydra.Annotations.getTermAnnotation(
  new hydra.core.Name("two"),
  new hydra.core.Term.Annotated(new hydra.core.AnnotatedTerm(new hydra.core.Term.Annotated(new hydra.core.AnnotatedTerm(new hydra.core.Term.Annotated(new hydra.core.AnnotatedTerm(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42))), hydra.util.PersistentMap.ofEntries(hydra.util.PersistentMap.entry(
    new hydra.core.Name("one"),
    new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(1))))))), hydra.util.PersistentMap.ofEntries(hydra.util.PersistentMap.entry(
    new hydra.core.Name("two"),
    new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(2))))))), hydra.util.PersistentMap.ofEntries(hydra.util.PersistentMap.entry(
    new hydra.core.Name("one"),
    new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(99)))))))));

    }

    @Test

    public void testLayeredAnnotationsOuterAnnotationOverridesInnerInLayeredTerm() {

        assertEquals(

            hydra.util.Maybe.just(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(99)))),

            hydra.Annotations.getTermAnnotation(
  new hydra.core.Name("one"),
  new hydra.core.Term.Annotated(new hydra.core.AnnotatedTerm(new hydra.core.Term.Annotated(new hydra.core.AnnotatedTerm(new hydra.core.Term.Annotated(new hydra.core.AnnotatedTerm(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42))), hydra.util.PersistentMap.ofEntries(hydra.util.PersistentMap.entry(
    new hydra.core.Name("one"),
    new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(1))))))), hydra.util.PersistentMap.ofEntries(hydra.util.PersistentMap.entry(
    new hydra.core.Name("two"),
    new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(2))))))), hydra.util.PersistentMap.ofEntries(hydra.util.PersistentMap.entry(
    new hydra.core.Name("one"),
    new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(99)))))))));

    }
}
