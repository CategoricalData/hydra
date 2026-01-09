# Note: this is an automatically generated file. Do not edit.
# annotations

from __future__ import annotations
from typing import cast
from decimal import Decimal
from hydra.dsl.python import FrozenDict, frozenlist, Either, Left, Right, Maybe, Just, Nothing
import hydra.accessors
import hydra.annotations
import hydra.ast
import hydra.classes
import hydra.coders
import hydra.compute
import hydra.constants
import hydra.core
import hydra.decode.core
import hydra.encode.core
import hydra.ext.haskell.operators
import hydra.extract.core
import hydra.extract.helpers
import hydra.formatting
import hydra.graph
import hydra.json.model
import hydra.lexical
import hydra.lib.chars
import hydra.lib.eithers
import hydra.lib.equality
import hydra.lib.flows
import hydra.lib.lists
import hydra.lib.literals
import hydra.lib.logic
import hydra.lib.maps
import hydra.lib.math
import hydra.lib.maybes
import hydra.lib.pairs
import hydra.lib.sets
import hydra.lib.strings
import hydra.module
import hydra.monads
import hydra.names
import hydra.parsing
import hydra.rewriting
import hydra.serialization
import hydra.show.core
import hydra.sorting
import hydra.tarjan
import hydra.testing
import hydra.topology
import hydra.typing
import hydra.util

# arbitrary annotations

def test_arbitrary_annotations__set_single_annotation__1():

    assert (hydra.annotations.set_term_annotation(hydra.core.Name("k1"), Just(cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueInt32(42))))))), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString("foo")))))) == (cast(hydra.core.Term, hydra.core.TermAnnotated(hydra.core.AnnotatedTerm(cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString("foo")))), FrozenDict({
  hydra.core.Name("k1"): cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueInt32(42))))))})))))

def test_arbitrary_annotations__set_single_annotation__2():

    assert (hydra.annotations.set_term_annotation(hydra.core.Name("myKey"), Just(cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueInt32(-17))))))), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString("bar")))))) == (cast(hydra.core.Term, hydra.core.TermAnnotated(hydra.core.AnnotatedTerm(cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString("bar")))), FrozenDict({
  hydra.core.Name("myKey"): cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueInt32(-17))))))})))))

def test_arbitrary_annotations__set_single_annotation__3():

    assert (hydra.annotations.set_term_annotation(hydra.core.Name("x"), Just(cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString("hello"))))), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueInt32(0)))))))) == (cast(hydra.core.Term, hydra.core.TermAnnotated(hydra.core.AnnotatedTerm(cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueInt32(0)))))), FrozenDict({
  hydra.core.Name("x"): cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString("hello"))))})))))

def test_arbitrary_annotations__get_existing_annotation__1():

    assert (hydra.annotations.get_term_annotation(hydra.core.Name("k1"), hydra.annotations.set_term_annotation(hydra.core.Name("k1"), Just(cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString("value"))))), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueInt32(42))))))))) == (Just(cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString("value"))))))

def test_arbitrary_annotations__get_existing_annotation__2():

    assert (hydra.annotations.get_term_annotation(hydra.core.Name("foo"), hydra.annotations.set_term_annotation(hydra.core.Name("foo"), Just(cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(""))))), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueInt32(99))))))))) == (Just(cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(""))))))

def test_arbitrary_annotations__get_existing_annotation__3():

    assert (hydra.annotations.get_term_annotation(hydra.core.Name("key"), hydra.annotations.set_term_annotation(hydra.core.Name("key"), Just(cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueInt32(123))))))), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString("test"))))))) == (Just(cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueInt32(123))))))))

def test_arbitrary_annotations__get_missing_annotation__1():

    assert (hydra.annotations.get_term_annotation(hydra.core.Name("k1"), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueInt16(42)))))))) == (Nothing())

def test_arbitrary_annotations__get_missing_annotation__2():

    assert (hydra.annotations.get_term_annotation(hydra.core.Name("nonexistent"), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString("hello")))))) == (Nothing())

def test_arbitrary_annotations__get_missing_annotation__3():

    assert (hydra.annotations.get_term_annotation(hydra.core.Name("k1"), hydra.annotations.set_term_annotation(hydra.core.Name("k2"), Just(cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueInt32(1))))))), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueInt32(42))))))))) == (Nothing())

def test_arbitrary_annotations__set_multiple_annotations__1():

    assert (hydra.annotations.set_term_annotation(hydra.core.Name("k2"), Just(cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueInt32(200))))))), hydra.annotations.set_term_annotation(hydra.core.Name("k1"), Just(cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString("first"))))), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralBoolean(True))))))) == (cast(hydra.core.Term, hydra.core.TermAnnotated(hydra.core.AnnotatedTerm(cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralBoolean(True)))), FrozenDict({
  hydra.core.Name("k1"): cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString("first")))),
  hydra.core.Name("k2"): cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueInt32(200))))))})))))

def test_arbitrary_annotations__set_multiple_annotations__2():

    assert (hydra.annotations.set_term_annotation(hydra.core.Name("b"), Just(cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueInt32(0))))))), hydra.annotations.set_term_annotation(hydra.core.Name("a"), Just(cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueInt32(-5))))))), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString("test"))))))) == (cast(hydra.core.Term, hydra.core.TermAnnotated(hydra.core.AnnotatedTerm(cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString("test")))), FrozenDict({
  hydra.core.Name("a"): cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueInt32(-5)))))),
  hydra.core.Name("b"): cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueInt32(0))))))})))))

def test_arbitrary_annotations__outer_annotation_overrides_inner__1():

    assert (hydra.annotations.set_term_annotation(hydra.core.Name("k1"), Just(cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString("outer"))))), hydra.annotations.set_term_annotation(hydra.core.Name("k1"), Just(cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString("inner"))))), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString("bar"))))))) == (cast(hydra.core.Term, hydra.core.TermAnnotated(hydra.core.AnnotatedTerm(cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString("bar")))), FrozenDict({
  hydra.core.Name("k1"): cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString("outer"))))})))))

def test_arbitrary_annotations__outer_annotation_overrides_inner__2():

    assert (hydra.annotations.set_term_annotation(hydra.core.Name("x"), Just(cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString("new"))))), hydra.annotations.set_term_annotation(hydra.core.Name("x"), Just(cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString("old"))))), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueInt32(42))))))))) == (cast(hydra.core.Term, hydra.core.TermAnnotated(hydra.core.AnnotatedTerm(cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueInt32(42)))))), FrozenDict({
  hydra.core.Name("x"): cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString("new"))))})))))

def test_arbitrary_annotations__outer_annotation_overrides_inner__3():

    assert (hydra.annotations.set_term_annotation(hydra.core.Name("key"), Just(cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueInt32(999))))))), hydra.annotations.set_term_annotation(hydra.core.Name("key"), Just(cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueInt32(1))))))), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralBoolean(False))))))) == (cast(hydra.core.Term, hydra.core.TermAnnotated(hydra.core.AnnotatedTerm(cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralBoolean(False)))), FrozenDict({
  hydra.core.Name("key"): cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueInt32(999))))))})))))

def test_arbitrary_annotations__unset_single_annotation__1():

    assert (hydra.annotations.set_term_annotation(hydra.core.Name("k1"), Nothing(), hydra.annotations.set_term_annotation(hydra.core.Name("k1"), Just(cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString("foo"))))), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueInt64(137))))))))) == (cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueInt64(137)))))))

def test_arbitrary_annotations__unset_single_annotation__2():

    assert (hydra.annotations.set_term_annotation(hydra.core.Name("x"), Nothing(), hydra.annotations.set_term_annotation(hydra.core.Name("x"), Just(cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueInt32(42))))))), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString("test"))))))) == (cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString("test")))))

def test_arbitrary_annotations__unset_one_of_multiple_annotations__1():

    assert (hydra.annotations.set_term_annotation(hydra.core.Name("k1"), Nothing(), hydra.annotations.set_term_annotation(hydra.core.Name("k2"), Just(cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueInt32(200))))))), hydra.annotations.set_term_annotation(hydra.core.Name("k1"), Just(cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString("first"))))), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueInt64(137)))))))))) == (cast(hydra.core.Term, hydra.core.TermAnnotated(hydra.core.AnnotatedTerm(cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueInt64(137)))))), FrozenDict({
  hydra.core.Name("k2"): cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueInt32(200))))))})))))

def test_arbitrary_annotations__unset_one_of_multiple_annotations__2():

    assert (hydra.annotations.set_term_annotation(hydra.core.Name("b"), Nothing(), hydra.annotations.set_term_annotation(hydra.core.Name("b"), Just(cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueInt32(2))))))), hydra.annotations.set_term_annotation(hydra.core.Name("a"), Just(cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueInt32(1))))))), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString("x")))))))) == (cast(hydra.core.Term, hydra.core.TermAnnotated(hydra.core.AnnotatedTerm(cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString("x")))), FrozenDict({
  hydra.core.Name("a"): cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueInt32(1))))))})))))

# descriptions

def test_descriptions__set_description__1():

    assert (hydra.annotations.set_term_description(Just("my description"), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString("foo")))))) == (cast(hydra.core.Term, hydra.core.TermAnnotated(hydra.core.AnnotatedTerm(cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString("foo")))), FrozenDict({
  hydra.core.Name("description"): cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString("my description"))))})))))

def test_descriptions__set_description__2():

    assert (hydra.annotations.set_term_description(Just(""), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueInt32(42)))))))) == (cast(hydra.core.Term, hydra.core.TermAnnotated(hydra.core.AnnotatedTerm(cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueInt32(42)))))), FrozenDict({
  hydra.core.Name("description"): cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(""))))})))))

def test_descriptions__set_description__3():

    assert (hydra.annotations.set_term_description(Just("A longer description with spaces"), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralBoolean(True)))))) == (cast(hydra.core.Term, hydra.core.TermAnnotated(hydra.core.AnnotatedTerm(cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralBoolean(True)))), FrozenDict({
  hydra.core.Name("description"): cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString("A longer description with spaces"))))})))))

def test_descriptions__get_existing_description__1():

    assert (hydra.annotations.get_term_description(hydra.annotations.set_term_description(Just("hello"), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueInt32(42)))))))).value(hydra.lexical.empty_graph, hydra.monads.empty_trace)) == (hydra.compute.FlowState(Just(Just("hello")), hydra.lexical.empty_graph, hydra.monads.empty_trace))

def test_descriptions__get_existing_description__2():

    assert (hydra.annotations.get_term_description(hydra.annotations.set_term_description(Just(""), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString("test")))))).value(hydra.lexical.empty_graph, hydra.monads.empty_trace)) == (hydra.compute.FlowState(Just(Just("")), hydra.lexical.empty_graph, hydra.monads.empty_trace))

def test_descriptions__get_existing_description__3():

    assert (hydra.annotations.get_term_description(hydra.annotations.set_term_description(Just("desc"), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralBoolean(False)))))).value(hydra.lexical.empty_graph, hydra.monads.empty_trace)) == (hydra.compute.FlowState(Just(Just("desc")), hydra.lexical.empty_graph, hydra.monads.empty_trace))

def test_descriptions__get_missing_description__1():

    assert (hydra.annotations.get_term_description(cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueInt16(42))))))).value(hydra.lexical.empty_graph, hydra.monads.empty_trace)) == (hydra.compute.FlowState(Just(Nothing()), hydra.lexical.empty_graph, hydra.monads.empty_trace))

def test_descriptions__get_missing_description__2():

    assert (hydra.annotations.get_term_description(cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString("no description here"))))).value(hydra.lexical.empty_graph, hydra.monads.empty_trace)) == (hydra.compute.FlowState(Just(Nothing()), hydra.lexical.empty_graph, hydra.monads.empty_trace))

def test_descriptions__get_missing_description__3():

    assert (hydra.annotations.get_term_description(cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueInt32(0))))))).value(hydra.lexical.empty_graph, hydra.monads.empty_trace)) == (hydra.compute.FlowState(Just(Nothing()), hydra.lexical.empty_graph, hydra.monads.empty_trace))

def test_descriptions__outer_description_overrides_inner__1():

    assert (hydra.annotations.set_term_description(Just("outer"), hydra.annotations.set_term_description(Just("inner"), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString("bar"))))))) == (cast(hydra.core.Term, hydra.core.TermAnnotated(hydra.core.AnnotatedTerm(cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString("bar")))), FrozenDict({
  hydra.core.Name("description"): cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString("outer"))))})))))

def test_descriptions__outer_description_overrides_inner__2():

    assert (hydra.annotations.set_term_description(Just("new"), hydra.annotations.set_term_description(Just("old"), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueInt32(99))))))))) == (cast(hydra.core.Term, hydra.core.TermAnnotated(hydra.core.AnnotatedTerm(cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueInt32(99)))))), FrozenDict({
  hydra.core.Name("description"): cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString("new"))))})))))

def test_descriptions__unset_description__1():

    assert (hydra.annotations.set_term_description(Nothing(), hydra.annotations.set_term_description(Just("desc"), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueInt64(137))))))))) == (cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueInt64(137)))))))

def test_descriptions__unset_description__2():

    assert (hydra.annotations.set_term_description(Nothing(), hydra.annotations.set_term_description(Just("to be removed"), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString("test"))))))) == (cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString("test")))))

# layered annotations

def test_layered_annotations__get_annotation_from_unannotated_term():

    assert (hydra.annotations.get_term_annotation(hydra.core.Name("one"), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueInt32(42)))))))) == (Nothing())

def test_layered_annotations__get_annotation_from_singly_annotated_term():

    assert (hydra.annotations.get_term_annotation(hydra.core.Name("one"), cast(hydra.core.Term, hydra.core.TermAnnotated(hydra.core.AnnotatedTerm(cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueInt32(42)))))), FrozenDict({
  hydra.core.Name("one"): cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueInt32(1))))))})))))) == (Just(cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueInt32(1))))))))

def test_layered_annotations__get_inner_annotation_from_doubly_annotated_term():

    assert (hydra.annotations.get_term_annotation(hydra.core.Name("one"), cast(hydra.core.Term, hydra.core.TermAnnotated(hydra.core.AnnotatedTerm(cast(hydra.core.Term, hydra.core.TermAnnotated(hydra.core.AnnotatedTerm(cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueInt32(42)))))), FrozenDict({
  hydra.core.Name("one"): cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueInt32(1))))))})))), FrozenDict({
  hydra.core.Name("two"): cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueInt32(2))))))})))))) == (Just(cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueInt32(1))))))))

def test_layered_annotations__get_outer_annotation_from_doubly_annotated_term():

    assert (hydra.annotations.get_term_annotation(hydra.core.Name("two"), cast(hydra.core.Term, hydra.core.TermAnnotated(hydra.core.AnnotatedTerm(cast(hydra.core.Term, hydra.core.TermAnnotated(hydra.core.AnnotatedTerm(cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueInt32(42)))))), FrozenDict({
  hydra.core.Name("one"): cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueInt32(1))))))})))), FrozenDict({
  hydra.core.Name("two"): cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueInt32(2))))))})))))) == (Just(cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueInt32(2))))))))

def test_layered_annotations__get_non_overridden_annotation_from_triply_annotated_term():

    assert (hydra.annotations.get_term_annotation(hydra.core.Name("two"), cast(hydra.core.Term, hydra.core.TermAnnotated(hydra.core.AnnotatedTerm(cast(hydra.core.Term, hydra.core.TermAnnotated(hydra.core.AnnotatedTerm(cast(hydra.core.Term, hydra.core.TermAnnotated(hydra.core.AnnotatedTerm(cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueInt32(42)))))), FrozenDict({
  hydra.core.Name("one"): cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueInt32(1))))))})))), FrozenDict({
  hydra.core.Name("two"): cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueInt32(2))))))})))), FrozenDict({
  hydra.core.Name("one"): cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueInt32(99))))))})))))) == (Just(cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueInt32(2))))))))

def test_layered_annotations__outer_annotation_overrides_inner_in_layered_term():

    assert (hydra.annotations.get_term_annotation(hydra.core.Name("one"), cast(hydra.core.Term, hydra.core.TermAnnotated(hydra.core.AnnotatedTerm(cast(hydra.core.Term, hydra.core.TermAnnotated(hydra.core.AnnotatedTerm(cast(hydra.core.Term, hydra.core.TermAnnotated(hydra.core.AnnotatedTerm(cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueInt32(42)))))), FrozenDict({
  hydra.core.Name("one"): cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueInt32(1))))))})))), FrozenDict({
  hydra.core.Name("two"): cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueInt32(2))))))})))), FrozenDict({
  hydra.core.Name("one"): cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueInt32(99))))))})))))) == (Just(cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueInt32(99))))))))
