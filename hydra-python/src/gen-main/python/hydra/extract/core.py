# Note: this is an automatically generated file. Do not edit.

r"""Extraction and validation for hydra.core types."""

from __future__ import annotations
from collections.abc import Callable
from decimal import Decimal
from functools import lru_cache
from hydra.dsl.python import Either, FrozenDict, Left, Maybe, Nothing, Right, frozenlist
from typing import TypeVar, cast
import hydra.context
import hydra.core
import hydra.error
import hydra.lexical
import hydra.lib.eithers
import hydra.lib.equality
import hydra.lib.lists
import hydra.lib.literals
import hydra.lib.logic
import hydra.lib.maps
import hydra.lib.maybes
import hydra.lib.pairs
import hydra.lib.strings
import hydra.rewriting
import hydra.show.core

T0 = TypeVar("T0")
T1 = TypeVar("T1")

def bigfloat_value(cx: hydra.context.Context, v: hydra.core.FloatValue) -> Either[hydra.context.InContext[hydra.error.Error], Decimal]:
    r"""Extract a bigfloat value from a FloatValue."""
    
    match v:
        case hydra.core.FloatValueBigfloat(value=f):
            return Right(f)
        
        case _:
            return Left(hydra.context.InContext(cast(hydra.error.Error, hydra.error.ErrorOther(hydra.error.OtherError(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2("expected ", "bigfloat"), " but found "), hydra.show.core.float(v))))), cx))

def float_literal(cx: hydra.context.Context, lit: hydra.core.Literal) -> Either[hydra.context.InContext[hydra.error.Error], hydra.core.FloatValue]:
    r"""Extract a floating-point literal from a Literal value."""
    
    match lit:
        case hydra.core.LiteralFloat(value=v):
            return Right(v)
        
        case _:
            return Left(hydra.context.InContext(cast(hydra.error.Error, hydra.error.ErrorOther(hydra.error.OtherError(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2("expected ", "floating-point value"), " but found "), hydra.show.core.literal(lit))))), cx))

def literal(cx: hydra.context.Context, graph: hydra.graph.Graph, term0: hydra.core.Term):
    def _hoist_hydra_extract_core_literal_1(cx, term, v1):
        match v1:
            case hydra.core.TermLiteral(value=lit):
                return Right(lit)
            
            case _:
                return Left(hydra.context.InContext(cast(hydra.error.Error, hydra.error.ErrorOther(hydra.error.OtherError(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2("expected ", "literal"), " but found "), hydra.show.core.term(term))))), cx))
    return hydra.lib.eithers.bind(hydra.lexical.strip_and_dereference_term(cx, graph, term0), (lambda term: _hoist_hydra_extract_core_literal_1(cx, term, term)))

def bigfloat(cx: hydra.context.Context, graph: hydra.graph.Graph, t: hydra.core.Term) -> Either[hydra.context.InContext[hydra.error.Error], Decimal]:
    r"""Extract an arbitrary-precision floating-point value from a term."""
    
    return hydra.lib.eithers.bind(literal(cx, graph, t), (lambda l: hydra.lib.eithers.bind(float_literal(cx, l), (lambda f: bigfloat_value(cx, f)))))

def bigint_value(cx: hydra.context.Context, v: hydra.core.IntegerValue) -> Either[hydra.context.InContext[hydra.error.Error], int]:
    r"""Extract a bigint value from an IntegerValue."""
    
    match v:
        case hydra.core.IntegerValueBigint(value=i):
            return Right(i)
        
        case _:
            return Left(hydra.context.InContext(cast(hydra.error.Error, hydra.error.ErrorOther(hydra.error.OtherError(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2("expected ", "bigint"), " but found "), hydra.show.core.integer(v))))), cx))

def integer_literal(cx: hydra.context.Context, lit: hydra.core.Literal) -> Either[hydra.context.InContext[hydra.error.Error], hydra.core.IntegerValue]:
    r"""Extract an integer literal from a Literal value."""
    
    match lit:
        case hydra.core.LiteralInteger(value=v):
            return Right(v)
        
        case _:
            return Left(hydra.context.InContext(cast(hydra.error.Error, hydra.error.ErrorOther(hydra.error.OtherError(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2("expected ", "integer value"), " but found "), hydra.show.core.literal(lit))))), cx))

def bigint(cx: hydra.context.Context, graph: hydra.graph.Graph, t: hydra.core.Term) -> Either[hydra.context.InContext[hydra.error.Error], int]:
    r"""Extract an arbitrary-precision integer value from a term."""
    
    return hydra.lib.eithers.bind(literal(cx, graph, t), (lambda l: hydra.lib.eithers.bind(integer_literal(cx, l), (lambda i: bigint_value(cx, i)))))

def binary_literal(cx: hydra.context.Context, v: hydra.core.Literal) -> Either[hydra.context.InContext[hydra.error.Error], bytes]:
    r"""Extract a binary literal from a Literal value."""
    
    match v:
        case hydra.core.LiteralBinary(value=b):
            return Right(b)
        
        case _:
            return Left(hydra.context.InContext(cast(hydra.error.Error, hydra.error.ErrorOther(hydra.error.OtherError(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2("expected ", "binary"), " but found "), hydra.show.core.literal(v))))), cx))

def binary(cx: hydra.context.Context, graph: hydra.graph.Graph, t: hydra.core.Term) -> Either[hydra.context.InContext[hydra.error.Error], bytes]:
    r"""Extract a binary data value from a term."""
    
    return hydra.lib.eithers.bind(literal(cx, graph, t), (lambda l: binary_literal(cx, l)))

def boolean_literal(cx: hydra.context.Context, v: hydra.core.Literal) -> Either[hydra.context.InContext[hydra.error.Error], bool]:
    r"""Extract a boolean literal from a Literal value."""
    
    match v:
        case hydra.core.LiteralBoolean(value=b):
            return Right(b)
        
        case _:
            return Left(hydra.context.InContext(cast(hydra.error.Error, hydra.error.ErrorOther(hydra.error.OtherError(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2("expected ", "boolean"), " but found "), hydra.show.core.literal(v))))), cx))

def boolean(cx: hydra.context.Context, graph: hydra.graph.Graph, t: hydra.core.Term) -> Either[hydra.context.InContext[hydra.error.Error], bool]:
    r"""Extract a boolean value from a term."""
    
    return hydra.lib.eithers.bind(literal(cx, graph, t), (lambda l: boolean_literal(cx, l)))

def cases(cx: hydra.context.Context, name: hydra.core.Name, graph: hydra.graph.Graph, term0: hydra.core.Term):
    def _hoist_hydra_extract_core_cases_1(cx, name, term, v1):
        match v1:
            case hydra.core.EliminationUnion(value=cs):
                return hydra.lib.logic.if_else(hydra.lib.equality.equal(cs.type_name.value, name.value), (lambda : Right(cs)), (lambda : Left(hydra.context.InContext(cast(hydra.error.Error, hydra.error.ErrorOther(hydra.error.OtherError(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2("expected ", hydra.lib.strings.cat2("case statement for type ", name.value)), " but found "), hydra.show.core.term(term))))), cx))))
            
            case _:
                return Left(hydra.context.InContext(cast(hydra.error.Error, hydra.error.ErrorOther(hydra.error.OtherError(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2("expected ", "case statement"), " but found "), hydra.show.core.term(term))))), cx))
    def _hoist_hydra_extract_core_cases_2(cx, name, term, v1):
        match v1:
            case hydra.core.FunctionElimination(value=elimination):
                return _hoist_hydra_extract_core_cases_1(cx, name, term, elimination)
            
            case _:
                return Left(hydra.context.InContext(cast(hydra.error.Error, hydra.error.ErrorOther(hydra.error.OtherError(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2("expected ", "case statement"), " but found "), hydra.show.core.term(term))))), cx))
    def _hoist_hydra_extract_core_cases_3(cx, name, term, v1):
        match v1:
            case hydra.core.TermFunction(value=function):
                return _hoist_hydra_extract_core_cases_2(cx, name, term, function)
            
            case _:
                return Left(hydra.context.InContext(cast(hydra.error.Error, hydra.error.ErrorOther(hydra.error.OtherError(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2("expected ", "case statement"), " but found "), hydra.show.core.term(term))))), cx))
    return hydra.lib.eithers.bind(hydra.lexical.strip_and_dereference_term(cx, graph, term0), (lambda term: _hoist_hydra_extract_core_cases_3(cx, name, term, term)))

def case_field(cx: hydra.context.Context, name: hydra.core.Name, n: str, graph: hydra.graph.Graph, term: hydra.core.Term) -> Either[hydra.context.InContext[hydra.error.Error], hydra.core.Field]:
    r"""Extract a specific case handler from a case statement term."""
    
    field_name = hydra.core.Name(n)
    return hydra.lib.eithers.bind(cases(cx, name, graph, term), (lambda cs: (matching := hydra.lib.lists.filter((lambda f: hydra.lib.equality.equal(f.name.value, field_name.value)), cs.cases), hydra.lib.logic.if_else(hydra.lib.lists.null(matching), (lambda : Left(hydra.context.InContext(cast(hydra.error.Error, hydra.error.ErrorOther(hydra.error.OtherError("not enough cases"))), cx))), (lambda : Right(hydra.lib.lists.head(matching)))))[1]))

def either_term(cx: hydra.context.Context, left_fun: Callable[[hydra.core.Term], Either[hydra.context.InContext[hydra.error.Error], T0]], right_fun: Callable[[hydra.core.Term], Either[hydra.context.InContext[hydra.error.Error], T1]], graph: hydra.graph.Graph, term0: hydra.core.Term):
    def _hoist_hydra_extract_core_either_term_1(cx, left_fun, right_fun, term, v1):
        match v1:
            case hydra.core.TermEither(value=et):
                return hydra.lib.eithers.either((lambda l: hydra.lib.eithers.map((lambda x: Left(x)), left_fun(l))), (lambda r: hydra.lib.eithers.map((lambda x: Right(x)), right_fun(r))), et)
            
            case _:
                return Left(hydra.context.InContext(cast(hydra.error.Error, hydra.error.ErrorOther(hydra.error.OtherError(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2("expected ", "either value"), " but found "), hydra.show.core.term(term))))), cx))
    return hydra.lib.eithers.bind(hydra.lexical.strip_and_dereference_term(cx, graph, term0), (lambda term: _hoist_hydra_extract_core_either_term_1(cx, left_fun, right_fun, term, term)))

def either_type(cx: hydra.context.Context, typ: hydra.core.Type) -> Either[hydra.context.InContext[hydra.error.Error], hydra.core.EitherType]:
    r"""Extract the left and right types from an either type."""
    
    @lru_cache(1)
    def stripped() -> hydra.core.Type:
        return hydra.rewriting.deannotate_type(typ)
    match stripped():
        case hydra.core.TypeEither(value=et):
            return Right(et)
        
        case _:
            return Left(hydra.context.InContext(cast(hydra.error.Error, hydra.error.ErrorOther(hydra.error.OtherError(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2("expected ", "either type"), " but found "), hydra.show.core.type(typ))))), cx))

def field(cx: hydra.context.Context, fname: hydra.core.Name, mapping: Callable[[hydra.core.Term], Either[hydra.context.InContext[hydra.error.Error], T0]], graph: hydra.graph.Graph, fields: frozenlist[hydra.core.Field]) -> Either[hydra.context.InContext[hydra.error.Error], T0]:
    r"""Extract a field value from a list of fields."""
    
    @lru_cache(1)
    def matching_fields() -> frozenlist[hydra.core.Field]:
        return hydra.lib.lists.filter((lambda f: hydra.lib.equality.equal(f.name.value, fname.value)), fields)
    return hydra.lib.logic.if_else(hydra.lib.lists.null(matching_fields()), (lambda : Left(hydra.context.InContext(cast(hydra.error.Error, hydra.error.ErrorOther(hydra.error.OtherError(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2("expected ", hydra.lib.strings.cat2("field ", fname.value)), " but found "), "no matching field")))), cx))), (lambda : hydra.lib.logic.if_else(hydra.lib.equality.equal(hydra.lib.lists.length(matching_fields()), 1), (lambda : hydra.lib.eithers.bind(hydra.lexical.strip_and_dereference_term(cx, graph, hydra.lib.lists.head(matching_fields()).term), (lambda stripped: mapping(stripped)))), (lambda : Left(hydra.context.InContext(cast(hydra.error.Error, hydra.error.ErrorOther(hydra.error.OtherError(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2("expected ", "single field"), " but found "), hydra.lib.strings.cat2("multiple fields named ", fname.value))))), cx))))))

def float32_value(cx: hydra.context.Context, v: hydra.core.FloatValue) -> Either[hydra.context.InContext[hydra.error.Error], float]:
    r"""Extract a float32 value from a FloatValue."""
    
    match v:
        case hydra.core.FloatValueFloat32(value=f):
            return Right(f)
        
        case _:
            return Left(hydra.context.InContext(cast(hydra.error.Error, hydra.error.ErrorOther(hydra.error.OtherError(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2("expected ", "float32"), " but found "), hydra.show.core.float(v))))), cx))

def float32(cx: hydra.context.Context, graph: hydra.graph.Graph, t: hydra.core.Term) -> Either[hydra.context.InContext[hydra.error.Error], float]:
    r"""Extract a 32-bit floating-point value from a term."""
    
    return hydra.lib.eithers.bind(literal(cx, graph, t), (lambda l: hydra.lib.eithers.bind(float_literal(cx, l), (lambda f: float32_value(cx, f)))))

def float64_value(cx: hydra.context.Context, v: hydra.core.FloatValue) -> Either[hydra.context.InContext[hydra.error.Error], float]:
    r"""Extract a float64 value from a FloatValue."""
    
    match v:
        case hydra.core.FloatValueFloat64(value=f):
            return Right(f)
        
        case _:
            return Left(hydra.context.InContext(cast(hydra.error.Error, hydra.error.ErrorOther(hydra.error.OtherError(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2("expected ", "float64"), " but found "), hydra.show.core.float(v))))), cx))

def float64(cx: hydra.context.Context, graph: hydra.graph.Graph, t: hydra.core.Term) -> Either[hydra.context.InContext[hydra.error.Error], float]:
    r"""Extract a 64-bit floating-point value from a term."""
    
    return hydra.lib.eithers.bind(literal(cx, graph, t), (lambda l: hydra.lib.eithers.bind(float_literal(cx, l), (lambda f: float64_value(cx, f)))))

def float_value(cx: hydra.context.Context, graph: hydra.graph.Graph, t: hydra.core.Term) -> Either[hydra.context.InContext[hydra.error.Error], hydra.core.FloatValue]:
    r"""Extract a float value from a term."""
    
    return hydra.lib.eithers.bind(literal(cx, graph, t), (lambda l: float_literal(cx, l)))

def function_type(cx: hydra.context.Context, typ: hydra.core.Type) -> Either[hydra.context.InContext[hydra.error.Error], hydra.core.FunctionType]:
    r"""Extract a function type from a type."""
    
    @lru_cache(1)
    def stripped() -> hydra.core.Type:
        return hydra.rewriting.deannotate_type(typ)
    match stripped():
        case hydra.core.TypeFunction(value=ft):
            return Right(ft)
        
        case _:
            return Left(hydra.context.InContext(cast(hydra.error.Error, hydra.error.ErrorOther(hydra.error.OtherError(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2("expected ", "function type"), " but found "), hydra.show.core.type(typ))))), cx))

def injection(cx: hydra.context.Context, expected: hydra.core.Name, graph: hydra.graph.Graph, term0: hydra.core.Term):
    def _hoist_hydra_extract_core_injection_1(cx, expected, term, v1):
        match v1:
            case hydra.core.TermUnion(value=injection):
                return hydra.lib.logic.if_else(hydra.lib.equality.equal(injection.type_name.value, expected.value), (lambda : Right(injection.field)), (lambda : Left(hydra.context.InContext(cast(hydra.error.Error, hydra.error.ErrorOther(hydra.error.OtherError(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2("expected ", hydra.lib.strings.cat2("injection of type ", expected.value)), " but found "), injection.type_name.value)))), cx))))
            
            case _:
                return Left(hydra.context.InContext(cast(hydra.error.Error, hydra.error.ErrorOther(hydra.error.OtherError(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2("expected ", "injection"), " but found "), hydra.show.core.term(term))))), cx))
    return hydra.lib.eithers.bind(hydra.lexical.strip_and_dereference_term(cx, graph, term0), (lambda term: _hoist_hydra_extract_core_injection_1(cx, expected, term, term)))

def int16_value(cx: hydra.context.Context, v: hydra.core.IntegerValue) -> Either[hydra.context.InContext[hydra.error.Error], int]:
    r"""Extract an int16 value from an IntegerValue."""
    
    match v:
        case hydra.core.IntegerValueInt16(value=i):
            return Right(i)
        
        case _:
            return Left(hydra.context.InContext(cast(hydra.error.Error, hydra.error.ErrorOther(hydra.error.OtherError(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2("expected ", "int16"), " but found "), hydra.show.core.integer(v))))), cx))

def int16(cx: hydra.context.Context, graph: hydra.graph.Graph, t: hydra.core.Term) -> Either[hydra.context.InContext[hydra.error.Error], int]:
    r"""Extract a 16-bit signed integer value from a term."""
    
    return hydra.lib.eithers.bind(literal(cx, graph, t), (lambda l: hydra.lib.eithers.bind(integer_literal(cx, l), (lambda i: int16_value(cx, i)))))

def int32_value(cx: hydra.context.Context, v: hydra.core.IntegerValue) -> Either[hydra.context.InContext[hydra.error.Error], int]:
    r"""Extract an int32 value from an IntegerValue."""
    
    match v:
        case hydra.core.IntegerValueInt32(value=i):
            return Right(i)
        
        case _:
            return Left(hydra.context.InContext(cast(hydra.error.Error, hydra.error.ErrorOther(hydra.error.OtherError(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2("expected ", "int32"), " but found "), hydra.show.core.integer(v))))), cx))

def int32(cx: hydra.context.Context, graph: hydra.graph.Graph, t: hydra.core.Term) -> Either[hydra.context.InContext[hydra.error.Error], int]:
    r"""Extract a 32-bit signed integer value from a term."""
    
    return hydra.lib.eithers.bind(literal(cx, graph, t), (lambda l: hydra.lib.eithers.bind(integer_literal(cx, l), (lambda i: int32_value(cx, i)))))

def int64_value(cx: hydra.context.Context, v: hydra.core.IntegerValue) -> Either[hydra.context.InContext[hydra.error.Error], int]:
    r"""Extract an int64 value from an IntegerValue."""
    
    match v:
        case hydra.core.IntegerValueInt64(value=i):
            return Right(i)
        
        case _:
            return Left(hydra.context.InContext(cast(hydra.error.Error, hydra.error.ErrorOther(hydra.error.OtherError(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2("expected ", "int64"), " but found "), hydra.show.core.integer(v))))), cx))

def int64(cx: hydra.context.Context, graph: hydra.graph.Graph, t: hydra.core.Term) -> Either[hydra.context.InContext[hydra.error.Error], int]:
    r"""Extract a 64-bit signed integer value from a term."""
    
    return hydra.lib.eithers.bind(literal(cx, graph, t), (lambda l: hydra.lib.eithers.bind(integer_literal(cx, l), (lambda i: int64_value(cx, i)))))

def int8_value(cx: hydra.context.Context, v: hydra.core.IntegerValue) -> Either[hydra.context.InContext[hydra.error.Error], int]:
    r"""Extract an int8 value from an IntegerValue."""
    
    match v:
        case hydra.core.IntegerValueInt8(value=i):
            return Right(i)
        
        case _:
            return Left(hydra.context.InContext(cast(hydra.error.Error, hydra.error.ErrorOther(hydra.error.OtherError(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2("expected ", "int8"), " but found "), hydra.show.core.integer(v))))), cx))

def int8(cx: hydra.context.Context, graph: hydra.graph.Graph, t: hydra.core.Term) -> Either[hydra.context.InContext[hydra.error.Error], int]:
    r"""Extract an 8-bit signed integer value from a term."""
    
    return hydra.lib.eithers.bind(literal(cx, graph, t), (lambda l: hydra.lib.eithers.bind(integer_literal(cx, l), (lambda i: int8_value(cx, i)))))

def integer_value(cx: hydra.context.Context, graph: hydra.graph.Graph, t: hydra.core.Term) -> Either[hydra.context.InContext[hydra.error.Error], hydra.core.IntegerValue]:
    r"""Extract an integer value from a term."""
    
    return hydra.lib.eithers.bind(literal(cx, graph, t), (lambda l: integer_literal(cx, l)))

def lambda_(cx: hydra.context.Context, graph: hydra.graph.Graph, term0: hydra.core.Term):
    def _hoist_hydra_extract_core_lambda_1(cx, term, v1):
        match v1:
            case hydra.core.FunctionLambda(value=l):
                return Right(l)
            
            case _:
                return Left(hydra.context.InContext(cast(hydra.error.Error, hydra.error.ErrorOther(hydra.error.OtherError(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2("expected ", "lambda"), " but found "), hydra.show.core.term(term))))), cx))
    def _hoist_hydra_extract_core_lambda_2(cx, term, v1):
        match v1:
            case hydra.core.TermFunction(value=function):
                return _hoist_hydra_extract_core_lambda_1(cx, term, function)
            
            case _:
                return Left(hydra.context.InContext(cast(hydra.error.Error, hydra.error.ErrorOther(hydra.error.OtherError(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2("expected ", "lambda"), " but found "), hydra.show.core.term(term))))), cx))
    return hydra.lib.eithers.bind(hydra.lexical.strip_and_dereference_term(cx, graph, term0), (lambda term: _hoist_hydra_extract_core_lambda_2(cx, term, term)))

def lambda_body(cx: hydra.context.Context, graph: hydra.graph.Graph, term: hydra.core.Term) -> Either[hydra.context.InContext[hydra.error.Error], hydra.core.Term]:
    r"""Extract the body of a lambda term."""
    
    return hydra.lib.eithers.map((lambda v1: v1.body), lambda_(cx, graph, term))

def let(cx: hydra.context.Context, graph: hydra.graph.Graph, term0: hydra.core.Term):
    def _hoist_hydra_extract_core_let_1(cx, term, v1):
        match v1:
            case hydra.core.TermLet(value=lt):
                return Right(lt)
            
            case _:
                return Left(hydra.context.InContext(cast(hydra.error.Error, hydra.error.ErrorOther(hydra.error.OtherError(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2("expected ", "let term"), " but found "), hydra.show.core.term(term))))), cx))
    return hydra.lib.eithers.bind(hydra.lexical.strip_and_dereference_term(cx, graph, term0), (lambda term: _hoist_hydra_extract_core_let_1(cx, term, term)))

def let_binding(cx: hydra.context.Context, n: str, graph: hydra.graph.Graph, term: hydra.core.Term) -> Either[hydra.context.InContext[hydra.error.Error], hydra.core.Term]:
    r"""Extract a binding with the given name from a let term."""
    
    name = hydra.core.Name(n)
    return hydra.lib.eithers.bind(let(cx, graph, term), (lambda let_expr: (matching_bindings := hydra.lib.lists.filter((lambda b: hydra.lib.equality.equal(b.name.value, name.value)), let_expr.bindings), hydra.lib.logic.if_else(hydra.lib.lists.null(matching_bindings), (lambda : Left(hydra.context.InContext(cast(hydra.error.Error, hydra.error.ErrorOther(hydra.error.OtherError(hydra.lib.strings.cat2("no such binding: ", n)))), cx))), (lambda : hydra.lib.logic.if_else(hydra.lib.equality.equal(hydra.lib.lists.length(matching_bindings), 1), (lambda : Right(hydra.lib.lists.head(matching_bindings).term)), (lambda : Left(hydra.context.InContext(cast(hydra.error.Error, hydra.error.ErrorOther(hydra.error.OtherError(hydra.lib.strings.cat2("multiple bindings named ", n)))), cx)))))))[1]))

def list(cx: hydra.context.Context, graph: hydra.graph.Graph, term: hydra.core.Term):
    def _hoist_hydra_extract_core_list_1(cx, stripped, v1):
        match v1:
            case hydra.core.TermList(value=l):
                return Right(l)
            
            case _:
                return Left(hydra.context.InContext(cast(hydra.error.Error, hydra.error.ErrorOther(hydra.error.OtherError(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2("expected ", "list"), " but found "), hydra.show.core.term(stripped))))), cx))
    return hydra.lib.eithers.bind(hydra.lexical.strip_and_dereference_term(cx, graph, term), (lambda stripped: _hoist_hydra_extract_core_list_1(cx, stripped, stripped)))

def list_head(cx: hydra.context.Context, graph: hydra.graph.Graph, term: hydra.core.Term) -> Either[hydra.context.InContext[hydra.error.Error], hydra.core.Term]:
    r"""Extract the first element of a list term."""
    
    return hydra.lib.eithers.bind(list(cx, graph, term), (lambda l: hydra.lib.logic.if_else(hydra.lib.lists.null(l), (lambda : Left(hydra.context.InContext(cast(hydra.error.Error, hydra.error.ErrorOther(hydra.error.OtherError("empty list"))), cx))), (lambda : Right(hydra.lib.lists.head(l))))))

def list_of(cx: hydra.context.Context, f: Callable[[hydra.core.Term], Either[hydra.context.InContext[hydra.error.Error], T0]], graph: hydra.graph.Graph, term: hydra.core.Term) -> Either[hydra.context.InContext[hydra.error.Error], frozenlist[T0]]:
    r"""Extract a list of values from a term, mapping a function over each element."""
    
    return hydra.lib.eithers.bind(list(cx, graph, term), (lambda els: hydra.lib.eithers.map_list(f, els)))

def list_type(cx: hydra.context.Context, typ: hydra.core.Type) -> Either[hydra.context.InContext[hydra.error.Error], hydra.core.Type]:
    r"""Extract the element type from a list type."""
    
    @lru_cache(1)
    def stripped() -> hydra.core.Type:
        return hydra.rewriting.deannotate_type(typ)
    match stripped():
        case hydra.core.TypeList(value=t):
            return Right(t)
        
        case _:
            return Left(hydra.context.InContext(cast(hydra.error.Error, hydra.error.ErrorOther(hydra.error.OtherError(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2("expected ", "list type"), " but found "), hydra.show.core.type(typ))))), cx))

def map(cx: hydra.context.Context, fk: Callable[[hydra.core.Term], Either[hydra.context.InContext[hydra.error.Error], T0]], fv: Callable[[hydra.core.Term], Either[hydra.context.InContext[hydra.error.Error], T1]], graph: hydra.graph.Graph, term0: hydra.core.Term):
    r"""Extract a map of key-value pairs from a term, mapping functions over each key and value."""
    
    def pair(kv_pair: tuple[hydra.core.Term, hydra.core.Term]) -> Either[hydra.context.InContext[hydra.error.Error], tuple[T0, T1]]:
        @lru_cache(1)
        def kterm() -> hydra.core.Term:
            return hydra.lib.pairs.first(kv_pair)
        @lru_cache(1)
        def vterm() -> hydra.core.Term:
            return hydra.lib.pairs.second(kv_pair)
        return hydra.lib.eithers.bind(fk(kterm()), (lambda kval: hydra.lib.eithers.bind(fv(vterm()), (lambda vval: Right((kval, vval))))))
    def _hoist_body_1(term, v1):
        match v1:
            case hydra.core.TermMap(value=m):
                return hydra.lib.eithers.map((lambda x1: hydra.lib.maps.from_list(x1)), hydra.lib.eithers.map_list((lambda x1: pair(x1)), hydra.lib.maps.to_list(m)))
            
            case _:
                return Left(hydra.context.InContext(cast(hydra.error.Error, hydra.error.ErrorOther(hydra.error.OtherError(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2("expected ", "map"), " but found "), hydra.show.core.term(term))))), cx))
    return hydra.lib.eithers.bind(hydra.lexical.strip_and_dereference_term(cx, graph, term0), (lambda term: _hoist_body_1(term, term)))

def map_type(cx: hydra.context.Context, typ: hydra.core.Type) -> Either[hydra.context.InContext[hydra.error.Error], hydra.core.MapType]:
    r"""Extract the key and value types from a map type."""
    
    @lru_cache(1)
    def stripped() -> hydra.core.Type:
        return hydra.rewriting.deannotate_type(typ)
    match stripped():
        case hydra.core.TypeMap(value=mt):
            return Right(mt)
        
        case _:
            return Left(hydra.context.InContext(cast(hydra.error.Error, hydra.error.ErrorOther(hydra.error.OtherError(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2("expected ", "map type"), " but found "), hydra.show.core.type(typ))))), cx))

def maybe_term(cx: hydra.context.Context, f: Callable[[hydra.core.Term], Either[hydra.context.InContext[hydra.error.Error], T0]], graph: hydra.graph.Graph, term0: hydra.core.Term):
    def _hoist_hydra_extract_core_maybe_term_1(cx, f, term, v1):
        match v1:
            case hydra.core.TermMaybe(value=mt):
                return hydra.lib.maybes.maybe((lambda : Right(Nothing())), (lambda t: hydra.lib.eithers.map((lambda x1: hydra.lib.maybes.pure(x1)), f(t))), mt)
            
            case _:
                return Left(hydra.context.InContext(cast(hydra.error.Error, hydra.error.ErrorOther(hydra.error.OtherError(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2("expected ", "maybe value"), " but found "), hydra.show.core.term(term))))), cx))
    return hydra.lib.eithers.bind(hydra.lexical.strip_and_dereference_term(cx, graph, term0), (lambda term: _hoist_hydra_extract_core_maybe_term_1(cx, f, term, term)))

def maybe_type(cx: hydra.context.Context, typ: hydra.core.Type) -> Either[hydra.context.InContext[hydra.error.Error], hydra.core.Type]:
    r"""Extract the base type from an optional type."""
    
    @lru_cache(1)
    def stripped() -> hydra.core.Type:
        return hydra.rewriting.deannotate_type(typ)
    match stripped():
        case hydra.core.TypeMaybe(value=t):
            return Right(t)
        
        case _:
            return Left(hydra.context.InContext(cast(hydra.error.Error, hydra.error.ErrorOther(hydra.error.OtherError(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2("expected ", "maybe type"), " but found "), hydra.show.core.type(typ))))), cx))

def n_args(cx: hydra.context.Context, name: hydra.core.Name, n: int, args: frozenlist[T0]) -> Either[hydra.context.InContext[hydra.error.Error], None]:
    r"""Ensure a function has the expected number of arguments."""
    
    return hydra.lib.logic.if_else(hydra.lib.equality.equal(hydra.lib.lists.length(args), n), (lambda : Right(None)), (lambda : Left(hydra.context.InContext(cast(hydra.error.Error, hydra.error.ErrorOther(hydra.error.OtherError(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2("expected ", hydra.lib.strings.cat((hydra.lib.literals.show_int32(n), " arguments to primitive ", hydra.lib.literals.show_string(name.value)))), " but found "), hydra.lib.literals.show_int32(hydra.lib.lists.length(args)))))), cx))))

def pair(cx: hydra.context.Context, kf: Callable[[hydra.core.Term], Either[hydra.context.InContext[hydra.error.Error], T0]], vf: Callable[[hydra.core.Term], Either[hydra.context.InContext[hydra.error.Error], T1]], graph: hydra.graph.Graph, term0: hydra.core.Term):
    def _hoist_hydra_extract_core_pair_1(cx, kf, term, vf, v1):
        match v1:
            case hydra.core.TermPair(value=p):
                return hydra.lib.eithers.bind(kf(hydra.lib.pairs.first(p)), (lambda k_val: hydra.lib.eithers.bind(vf(hydra.lib.pairs.second(p)), (lambda v_val: Right((k_val, v_val))))))
            
            case _:
                return Left(hydra.context.InContext(cast(hydra.error.Error, hydra.error.ErrorOther(hydra.error.OtherError(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2("expected ", "pair"), " but found "), hydra.show.core.term(term))))), cx))
    return hydra.lib.eithers.bind(hydra.lexical.strip_and_dereference_term(cx, graph, term0), (lambda term: _hoist_hydra_extract_core_pair_1(cx, kf, term, vf, term)))

def term_record(cx: hydra.context.Context, graph: hydra.graph.Graph, term0: hydra.core.Term):
    def _hoist_hydra_extract_core_term_record_1(cx, term, v1):
        match v1:
            case hydra.core.TermRecord(value=record):
                return Right(record)
            
            case _:
                return Left(hydra.context.InContext(cast(hydra.error.Error, hydra.error.ErrorOther(hydra.error.OtherError(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2("expected ", "record"), " but found "), hydra.show.core.term(term))))), cx))
    return hydra.lib.eithers.bind(hydra.lexical.strip_and_dereference_term(cx, graph, term0), (lambda term: _hoist_hydra_extract_core_term_record_1(cx, term, term)))

def record(cx: hydra.context.Context, expected: hydra.core.Name, graph: hydra.graph.Graph, term0: hydra.core.Term) -> Either[hydra.context.InContext[hydra.error.Error], frozenlist[hydra.core.Field]]:
    r"""Extract a record's fields from a term."""
    
    return hydra.lib.eithers.bind(term_record(cx, graph, term0), (lambda record: hydra.lib.logic.if_else(hydra.lib.equality.equal(record.type_name, expected), (lambda : Right(record.fields)), (lambda : Left(hydra.context.InContext(cast(hydra.error.Error, hydra.error.ErrorOther(hydra.error.OtherError(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2("expected ", hydra.lib.strings.cat2("record of type ", expected.value)), " but found "), record.type_name.value)))), cx))))))

def record_type(cx: hydra.context.Context, ename: hydra.core.Name, typ: hydra.core.Type) -> Either[hydra.context.InContext[hydra.error.Error], frozenlist[hydra.core.FieldType]]:
    r"""Extract the field types from a record type."""
    
    @lru_cache(1)
    def stripped() -> hydra.core.Type:
        return hydra.rewriting.deannotate_type(typ)
    match stripped():
        case hydra.core.TypeRecord(value=row_type):
            return hydra.lib.logic.if_else(hydra.lib.equality.equal(row_type.type_name.value, ename.value), (lambda : Right(row_type.fields)), (lambda : Left(hydra.context.InContext(cast(hydra.error.Error, hydra.error.ErrorOther(hydra.error.OtherError(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2("expected ", hydra.lib.strings.cat2("record of type ", ename.value)), " but found "), hydra.lib.strings.cat2("record of type ", row_type.type_name.value))))), cx))))
        
        case _:
            return Left(hydra.context.InContext(cast(hydra.error.Error, hydra.error.ErrorOther(hydra.error.OtherError(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2("expected ", "record type"), " but found "), hydra.show.core.type(typ))))), cx))

def set(cx: hydra.context.Context, graph: hydra.graph.Graph, term: hydra.core.Term):
    def _hoist_hydra_extract_core_set_1(cx, stripped, v1):
        match v1:
            case hydra.core.TermSet(value=s):
                return Right(s)
            
            case _:
                return Left(hydra.context.InContext(cast(hydra.error.Error, hydra.error.ErrorOther(hydra.error.OtherError(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2("expected ", "set"), " but found "), hydra.show.core.term(stripped))))), cx))
    return hydra.lib.eithers.bind(hydra.lexical.strip_and_dereference_term(cx, graph, term), (lambda stripped: _hoist_hydra_extract_core_set_1(cx, stripped, stripped)))

def set_of(cx: hydra.context.Context, f: Callable[[hydra.core.Term], Either[hydra.context.InContext[hydra.error.Error], T0]], graph: hydra.graph.Graph, term: hydra.core.Term) -> Either[hydra.context.InContext[hydra.error.Error], frozenset[T0]]:
    r"""Extract a set of values from a term, mapping a function over each element."""
    
    return hydra.lib.eithers.bind(set(cx, graph, term), (lambda els: hydra.lib.eithers.map_set(f, els)))

def set_type(cx: hydra.context.Context, typ: hydra.core.Type) -> Either[hydra.context.InContext[hydra.error.Error], hydra.core.Type]:
    r"""Extract the element type from a set type."""
    
    @lru_cache(1)
    def stripped() -> hydra.core.Type:
        return hydra.rewriting.deannotate_type(typ)
    match stripped():
        case hydra.core.TypeSet(value=t):
            return Right(t)
        
        case _:
            return Left(hydra.context.InContext(cast(hydra.error.Error, hydra.error.ErrorOther(hydra.error.OtherError(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2("expected ", "set type"), " but found "), hydra.show.core.type(typ))))), cx))

def string_literal(cx: hydra.context.Context, v: hydra.core.Literal) -> Either[hydra.context.InContext[hydra.error.Error], str]:
    r"""Extract a string literal from a Literal value."""
    
    match v:
        case hydra.core.LiteralString(value=s):
            return Right(s)
        
        case _:
            return Left(hydra.context.InContext(cast(hydra.error.Error, hydra.error.ErrorOther(hydra.error.OtherError(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2("expected ", "string"), " but found "), hydra.show.core.literal(v))))), cx))

def string(cx: hydra.context.Context, graph: hydra.graph.Graph, t: hydra.core.Term) -> Either[hydra.context.InContext[hydra.error.Error], str]:
    r"""Extract a string value from a term."""
    
    return hydra.lib.eithers.bind(literal(cx, graph, t), (lambda l: string_literal(cx, l)))

def uint16_value(cx: hydra.context.Context, v: hydra.core.IntegerValue) -> Either[hydra.context.InContext[hydra.error.Error], int]:
    r"""Extract a uint16 value from an IntegerValue."""
    
    match v:
        case hydra.core.IntegerValueUint16(value=i):
            return Right(i)
        
        case _:
            return Left(hydra.context.InContext(cast(hydra.error.Error, hydra.error.ErrorOther(hydra.error.OtherError(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2("expected ", "uint16"), " but found "), hydra.show.core.integer(v))))), cx))

def uint16(cx: hydra.context.Context, graph: hydra.graph.Graph, t: hydra.core.Term) -> Either[hydra.context.InContext[hydra.error.Error], int]:
    r"""Extract a 16-bit unsigned integer value from a term."""
    
    return hydra.lib.eithers.bind(literal(cx, graph, t), (lambda l: hydra.lib.eithers.bind(integer_literal(cx, l), (lambda i: uint16_value(cx, i)))))

def uint32_value(cx: hydra.context.Context, v: hydra.core.IntegerValue) -> Either[hydra.context.InContext[hydra.error.Error], int]:
    r"""Extract a uint32 value from an IntegerValue."""
    
    match v:
        case hydra.core.IntegerValueUint32(value=i):
            return Right(i)
        
        case _:
            return Left(hydra.context.InContext(cast(hydra.error.Error, hydra.error.ErrorOther(hydra.error.OtherError(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2("expected ", "uint32"), " but found "), hydra.show.core.integer(v))))), cx))

def uint32(cx: hydra.context.Context, graph: hydra.graph.Graph, t: hydra.core.Term) -> Either[hydra.context.InContext[hydra.error.Error], int]:
    r"""Extract a 32-bit unsigned integer value from a term."""
    
    return hydra.lib.eithers.bind(literal(cx, graph, t), (lambda l: hydra.lib.eithers.bind(integer_literal(cx, l), (lambda i: uint32_value(cx, i)))))

def uint64_value(cx: hydra.context.Context, v: hydra.core.IntegerValue) -> Either[hydra.context.InContext[hydra.error.Error], int]:
    r"""Extract a uint64 value from an IntegerValue."""
    
    match v:
        case hydra.core.IntegerValueUint64(value=i):
            return Right(i)
        
        case _:
            return Left(hydra.context.InContext(cast(hydra.error.Error, hydra.error.ErrorOther(hydra.error.OtherError(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2("expected ", "uint64"), " but found "), hydra.show.core.integer(v))))), cx))

def uint64(cx: hydra.context.Context, graph: hydra.graph.Graph, t: hydra.core.Term) -> Either[hydra.context.InContext[hydra.error.Error], int]:
    r"""Extract a 64-bit unsigned integer value from a term."""
    
    return hydra.lib.eithers.bind(literal(cx, graph, t), (lambda l: hydra.lib.eithers.bind(integer_literal(cx, l), (lambda i: uint64_value(cx, i)))))

def uint8_value(cx: hydra.context.Context, v: hydra.core.IntegerValue) -> Either[hydra.context.InContext[hydra.error.Error], int]:
    r"""Extract a uint8 value from an IntegerValue."""
    
    match v:
        case hydra.core.IntegerValueUint8(value=i):
            return Right(i)
        
        case _:
            return Left(hydra.context.InContext(cast(hydra.error.Error, hydra.error.ErrorOther(hydra.error.OtherError(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2("expected ", "uint8"), " but found "), hydra.show.core.integer(v))))), cx))

def uint8(cx: hydra.context.Context, graph: hydra.graph.Graph, t: hydra.core.Term) -> Either[hydra.context.InContext[hydra.error.Error], int]:
    r"""Extract an 8-bit unsigned integer value from a term."""
    
    return hydra.lib.eithers.bind(literal(cx, graph, t), (lambda l: hydra.lib.eithers.bind(integer_literal(cx, l), (lambda i: uint8_value(cx, i)))))

def union_type(cx: hydra.context.Context, ename: hydra.core.Name, typ: hydra.core.Type) -> Either[hydra.context.InContext[hydra.error.Error], frozenlist[hydra.core.FieldType]]:
    r"""Extract the field types from a union type."""
    
    @lru_cache(1)
    def stripped() -> hydra.core.Type:
        return hydra.rewriting.deannotate_type(typ)
    match stripped():
        case hydra.core.TypeUnion(value=row_type):
            return hydra.lib.logic.if_else(hydra.lib.equality.equal(row_type.type_name, ename), (lambda : Right(row_type.fields)), (lambda : Left(hydra.context.InContext(cast(hydra.error.Error, hydra.error.ErrorOther(hydra.error.OtherError(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2("expected ", hydra.lib.strings.cat2("union of type ", ename.value)), " but found "), hydra.lib.strings.cat2("union of type ", row_type.type_name.value))))), cx))))
        
        case _:
            return Left(hydra.context.InContext(cast(hydra.error.Error, hydra.error.ErrorOther(hydra.error.OtherError(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2("expected ", "union type"), " but found "), hydra.show.core.type(typ))))), cx))

def unit(cx: hydra.context.Context, term: hydra.core.Term) -> Either[hydra.context.InContext[hydra.error.Error], None]:
    r"""Extract a unit value from a term."""
    
    match term:
        case hydra.core.TermUnit():
            return Right(None)
        
        case _:
            return Left(hydra.context.InContext(cast(hydra.error.Error, hydra.error.ErrorOther(hydra.error.OtherError(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2("expected ", "unit"), " but found "), hydra.show.core.term(term))))), cx))

def unit_variant(cx: hydra.context.Context, tname: hydra.core.Name, graph: hydra.graph.Graph, term: hydra.core.Term) -> Either[hydra.context.InContext[hydra.error.Error], hydra.core.Name]:
    r"""Extract a unit variant (a variant with an empty record value) from a union term."""
    
    return hydra.lib.eithers.bind(injection(cx, tname, graph, term), (lambda field: hydra.lib.eithers.bind(unit(cx, field.term), (lambda ignored: Right(field.name)))))

def wrap(cx: hydra.context.Context, expected: hydra.core.Name, graph: hydra.graph.Graph, term0: hydra.core.Term):
    def _hoist_hydra_extract_core_wrap_1(cx, expected, term, v1):
        match v1:
            case hydra.core.TermWrap(value=wrapped_term):
                return hydra.lib.logic.if_else(hydra.lib.equality.equal(wrapped_term.type_name.value, expected.value), (lambda : Right(wrapped_term.body)), (lambda : Left(hydra.context.InContext(cast(hydra.error.Error, hydra.error.ErrorOther(hydra.error.OtherError(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2("expected ", hydra.lib.strings.cat2("wrapper of type ", expected.value)), " but found "), wrapped_term.type_name.value)))), cx))))
            
            case _:
                return Left(hydra.context.InContext(cast(hydra.error.Error, hydra.error.ErrorOther(hydra.error.OtherError(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2("expected ", hydra.lib.strings.cat2(hydra.lib.strings.cat2("wrap(", expected.value), ")")), " but found "), hydra.show.core.term(term))))), cx))
    return hydra.lib.eithers.bind(hydra.lexical.strip_and_dereference_term(cx, graph, term0), (lambda term: _hoist_hydra_extract_core_wrap_1(cx, expected, term, term)))

def wrapped_type(cx: hydra.context.Context, ename: hydra.core.Name, typ: hydra.core.Type) -> Either[hydra.context.InContext[hydra.error.Error], hydra.core.Type]:
    r"""Extract the wrapped type from a wrapper type."""
    
    @lru_cache(1)
    def stripped() -> hydra.core.Type:
        return hydra.rewriting.deannotate_type(typ)
    match stripped():
        case hydra.core.TypeWrap(value=wrapped_type):
            return hydra.lib.logic.if_else(hydra.lib.equality.equal(wrapped_type.type_name.value, ename.value), (lambda : Right(wrapped_type.body)), (lambda : Left(hydra.context.InContext(cast(hydra.error.Error, hydra.error.ErrorOther(hydra.error.OtherError(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2("expected ", hydra.lib.strings.cat2("wrapped type ", ename.value)), " but found "), hydra.lib.strings.cat2("wrapped type ", wrapped_type.type_name.value))))), cx))))
        
        case _:
            return Left(hydra.context.InContext(cast(hydra.error.Error, hydra.error.ErrorOther(hydra.error.OtherError(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2("expected ", "wrapped type"), " but found "), hydra.show.core.type(typ))))), cx))
