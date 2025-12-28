# Note: this is an automatically generated file. Do not edit.

r"""Term decoders for hydra.core."""

from __future__ import annotations
from collections.abc import Callable
from decimal import Decimal
from hydra.dsl.python import Either, FrozenDict, Left, Maybe, Right, frozenlist
from typing import cast
import hydra.core
import hydra.graph
import hydra.lexical
import hydra.lib.eithers
import hydra.lib.lists
import hydra.lib.maps
import hydra.lib.maybes
import hydra.lib.pairs
import hydra.lib.sets
import hydra.lib.strings
import hydra.util

def name(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.core.Name]:
    return hydra.lib.eithers.either((lambda err: cast(Either[hydra.util.DecodingError, hydra.core.Name], Left(hydra.util.DecodingError(err)))), (lambda stripped: "inline match expressions are unsupported"), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def projection(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.core.Projection]:
    return hydra.lib.eithers.either((lambda err: cast(Either[hydra.util.DecodingError, hydra.core.Projection], Left(hydra.util.DecodingError(err)))), (lambda stripped: "inline match expressions are unsupported"), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def float_type(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.core.FloatType]:
    return hydra.lib.eithers.either((lambda err: cast(Either[hydra.util.DecodingError, hydra.core.FloatType], Left(hydra.util.DecodingError(err)))), (lambda stripped: "inline match expressions are unsupported"), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def integer_type(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.core.IntegerType]:
    return hydra.lib.eithers.either((lambda err: cast(Either[hydra.util.DecodingError, hydra.core.IntegerType], Left(hydra.util.DecodingError(err)))), (lambda stripped: "inline match expressions are unsupported"), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def literal_type(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.core.LiteralType]:
    return hydra.lib.eithers.either((lambda err: cast(Either[hydra.util.DecodingError, hydra.core.LiteralType], Left(hydra.util.DecodingError(err)))), (lambda stripped: "inline match expressions are unsupported"), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def type_variable_metadata(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.core.TypeVariableMetadata]:
    return hydra.lib.eithers.either((lambda err: cast(Either[hydra.util.DecodingError, hydra.core.TypeVariableMetadata], Left(hydra.util.DecodingError(err)))), (lambda stripped: "inline match expressions are unsupported"), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def float_value(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.core.FloatValue]:
    return hydra.lib.eithers.either((lambda err: cast(Either[hydra.util.DecodingError, hydra.core.FloatValue], Left(hydra.util.DecodingError(err)))), (lambda stripped: "inline match expressions are unsupported"), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def integer_value(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.core.IntegerValue]:
    return hydra.lib.eithers.either((lambda err: cast(Either[hydra.util.DecodingError, hydra.core.IntegerValue], Left(hydra.util.DecodingError(err)))), (lambda stripped: "inline match expressions are unsupported"), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def literal(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.core.Literal]:
    return hydra.lib.eithers.either((lambda err: cast(Either[hydra.util.DecodingError, hydra.core.Literal], Left(hydra.util.DecodingError(err)))), (lambda stripped: "inline match expressions are unsupported"), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def annotated_term(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.core.AnnotatedTerm]:
    return hydra.lib.eithers.either((lambda err: cast(Either[hydra.util.DecodingError, hydra.core.AnnotatedTerm], Left(hydra.util.DecodingError(err)))), (lambda stripped: "inline match expressions are unsupported"), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def annotated_type(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.core.AnnotatedType]:
    return hydra.lib.eithers.either((lambda err: cast(Either[hydra.util.DecodingError, hydra.core.AnnotatedType], Left(hydra.util.DecodingError(err)))), (lambda stripped: "inline match expressions are unsupported"), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def application(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.core.Application]:
    return hydra.lib.eithers.either((lambda err: cast(Either[hydra.util.DecodingError, hydra.core.Application], Left(hydra.util.DecodingError(err)))), (lambda stripped: "inline match expressions are unsupported"), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def application_type(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.core.ApplicationType]:
    return hydra.lib.eithers.either((lambda err: cast(Either[hydra.util.DecodingError, hydra.core.ApplicationType], Left(hydra.util.DecodingError(err)))), (lambda stripped: "inline match expressions are unsupported"), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def binding(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.core.Binding]:
    return hydra.lib.eithers.either((lambda err: cast(Either[hydra.util.DecodingError, hydra.core.Binding], Left(hydra.util.DecodingError(err)))), (lambda stripped: "inline match expressions are unsupported"), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def case_statement(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.core.CaseStatement]:
    return hydra.lib.eithers.either((lambda err: cast(Either[hydra.util.DecodingError, hydra.core.CaseStatement], Left(hydra.util.DecodingError(err)))), (lambda stripped: "inline match expressions are unsupported"), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def either_type(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.core.EitherType]:
    return hydra.lib.eithers.either((lambda err: cast(Either[hydra.util.DecodingError, hydra.core.EitherType], Left(hydra.util.DecodingError(err)))), (lambda stripped: "inline match expressions are unsupported"), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def elimination(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.core.Elimination]:
    return hydra.lib.eithers.either((lambda err: cast(Either[hydra.util.DecodingError, hydra.core.Elimination], Left(hydra.util.DecodingError(err)))), (lambda stripped: "inline match expressions are unsupported"), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def field(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.core.Field]:
    return hydra.lib.eithers.either((lambda err: cast(Either[hydra.util.DecodingError, hydra.core.Field], Left(hydra.util.DecodingError(err)))), (lambda stripped: "inline match expressions are unsupported"), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def field_type(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.core.FieldType]:
    return hydra.lib.eithers.either((lambda err: cast(Either[hydra.util.DecodingError, hydra.core.FieldType], Left(hydra.util.DecodingError(err)))), (lambda stripped: "inline match expressions are unsupported"), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def forall_type(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.core.ForallType]:
    return hydra.lib.eithers.either((lambda err: cast(Either[hydra.util.DecodingError, hydra.core.ForallType], Left(hydra.util.DecodingError(err)))), (lambda stripped: "inline match expressions are unsupported"), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def function(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.core.Function]:
    return hydra.lib.eithers.either((lambda err: cast(Either[hydra.util.DecodingError, hydra.core.Function], Left(hydra.util.DecodingError(err)))), (lambda stripped: "inline match expressions are unsupported"), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def function_type(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.core.FunctionType]:
    return hydra.lib.eithers.either((lambda err: cast(Either[hydra.util.DecodingError, hydra.core.FunctionType], Left(hydra.util.DecodingError(err)))), (lambda stripped: "inline match expressions are unsupported"), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def injection(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.core.Injection]:
    return hydra.lib.eithers.either((lambda err: cast(Either[hydra.util.DecodingError, hydra.core.Injection], Left(hydra.util.DecodingError(err)))), (lambda stripped: "inline match expressions are unsupported"), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def lambda_(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.core.Lambda]:
    return hydra.lib.eithers.either((lambda err: cast(Either[hydra.util.DecodingError, hydra.core.Lambda], Left(hydra.util.DecodingError(err)))), (lambda stripped: "inline match expressions are unsupported"), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def let(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.core.Let]:
    return hydra.lib.eithers.either((lambda err: cast(Either[hydra.util.DecodingError, hydra.core.Let], Left(hydra.util.DecodingError(err)))), (lambda stripped: "inline match expressions are unsupported"), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def map_type(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.core.MapType]:
    return hydra.lib.eithers.either((lambda err: cast(Either[hydra.util.DecodingError, hydra.core.MapType], Left(hydra.util.DecodingError(err)))), (lambda stripped: "inline match expressions are unsupported"), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def pair_type(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.core.PairType]:
    return hydra.lib.eithers.either((lambda err: cast(Either[hydra.util.DecodingError, hydra.core.PairType], Left(hydra.util.DecodingError(err)))), (lambda stripped: "inline match expressions are unsupported"), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def record(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.core.Record]:
    return hydra.lib.eithers.either((lambda err: cast(Either[hydra.util.DecodingError, hydra.core.Record], Left(hydra.util.DecodingError(err)))), (lambda stripped: "inline match expressions are unsupported"), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def row_type(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.core.RowType]:
    return hydra.lib.eithers.either((lambda err: cast(Either[hydra.util.DecodingError, hydra.core.RowType], Left(hydra.util.DecodingError(err)))), (lambda stripped: "inline match expressions are unsupported"), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def term(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.core.Term]:
    return hydra.lib.eithers.either((lambda err: cast(Either[hydra.util.DecodingError, hydra.core.Term], Left(hydra.util.DecodingError(err)))), (lambda stripped: "inline match expressions are unsupported"), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def type(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.core.Type]:
    return hydra.lib.eithers.either((lambda err: cast(Either[hydra.util.DecodingError, hydra.core.Type], Left(hydra.util.DecodingError(err)))), (lambda stripped: "inline match expressions are unsupported"), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def type_application_term(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.core.TypeApplicationTerm]:
    return hydra.lib.eithers.either((lambda err: cast(Either[hydra.util.DecodingError, hydra.core.TypeApplicationTerm], Left(hydra.util.DecodingError(err)))), (lambda stripped: "inline match expressions are unsupported"), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def type_lambda(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.core.TypeLambda]:
    return hydra.lib.eithers.either((lambda err: cast(Either[hydra.util.DecodingError, hydra.core.TypeLambda], Left(hydra.util.DecodingError(err)))), (lambda stripped: "inline match expressions are unsupported"), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def type_scheme(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.core.TypeScheme]:
    return hydra.lib.eithers.either((lambda err: cast(Either[hydra.util.DecodingError, hydra.core.TypeScheme], Left(hydra.util.DecodingError(err)))), (lambda stripped: "inline match expressions are unsupported"), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def wrapped_term(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.core.WrappedTerm]:
    return hydra.lib.eithers.either((lambda err: cast(Either[hydra.util.DecodingError, hydra.core.WrappedTerm], Left(hydra.util.DecodingError(err)))), (lambda stripped: "inline match expressions are unsupported"), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def wrapped_type(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.core.WrappedType]:
    return hydra.lib.eithers.either((lambda err: cast(Either[hydra.util.DecodingError, hydra.core.WrappedType], Left(hydra.util.DecodingError(err)))), (lambda stripped: "inline match expressions are unsupported"), hydra.lexical.strip_and_dereference_term_either(cx, raw))
