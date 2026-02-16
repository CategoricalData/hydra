# Note: this is an automatically generated file. Do not edit.

r"""Adapter framework for types and terms."""

from __future__ import annotations
from collections.abc import Callable
from functools import lru_cache
from hydra.dsl.python import FrozenDict, Just, Maybe, Nothing, frozenlist
from typing import TypeVar, cast
import hydra.adapt.literals
import hydra.adapt.utils
import hydra.coders
import hydra.compute
import hydra.core
import hydra.extract.core
import hydra.lib.equality
import hydra.lib.flows
import hydra.lib.lists
import hydra.lib.logic
import hydra.lib.maps
import hydra.lib.maybes
import hydra.lib.pairs
import hydra.lib.sets
import hydra.lib.strings
import hydra.monads
import hydra.reflect
import hydra.rewriting
import hydra.schemas
import hydra.show.core
import hydra.variants

T0 = TypeVar("T0")
T1 = TypeVar("T1")
T2 = TypeVar("T2")
T3 = TypeVar("T3")
T4 = TypeVar("T4")
T5 = TypeVar("T5")

def with_graph_context(f: hydra.compute.Flow[hydra.graph.Graph, T0]) -> hydra.compute.Flow[hydra.coders.AdapterContext, T0]:
    return hydra.lib.flows.bind(hydra.monads.get_state(), (lambda cx: hydra.monads.with_state(cx.graph, f)))

function_proxy_name = hydra.core.Name("hydra.core.FunctionProxy")

def pass_literal(t: hydra.core.Type) -> hydra.compute.Flow[hydra.coders.AdapterContext, hydra.compute.Adapter[hydra.coders.AdapterContext, hydra.coders.AdapterContext, hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term]]:
    r"""Pass through literal types with literal adaptation."""
    
    def encdec(ad: hydra.compute.Adapter[hydra.coders.AdapterContext, hydra.coders.AdapterContext, T0, T1, hydra.core.Literal, hydra.core.Literal], dir: hydra.coders.CoderDirection, term: hydra.core.Term) -> hydra.compute.Flow[hydra.coders.AdapterContext, hydra.core.Term]:
        return hydra.lib.flows.bind(with_graph_context(hydra.extract.core.literal(term)), (lambda l: hydra.lib.flows.bind(hydra.adapt.utils.encode_decode(dir, ad.coder, l), (lambda l2: hydra.lib.flows.pure(cast(hydra.core.Term, hydra.core.TermLiteral(l2)))))))
    def for_literal(lt: hydra.core.LiteralType) -> hydra.compute.Flow[hydra.coders.AdapterContext, hydra.compute.Adapter[hydra.coders.AdapterContext, hydra.coders.AdapterContext, hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term]]:
        return hydra.lib.flows.bind(hydra.adapt.literals.literal_adapter(lt), (lambda ad: (step := hydra.adapt.utils.bidirectional((lambda v1, v2: encdec(ad, v1, v2))), hydra.lib.flows.pure(hydra.compute.Adapter(ad.is_lossy, cast(hydra.core.Type, hydra.core.TypeLiteral(ad.source)), cast(hydra.core.Type, hydra.core.TypeLiteral(ad.target)), step)))[1]))
    match t:
        case hydra.core.TypeLiteral(value=lt):
            return for_literal(lt)
        
        case _:
            raise TypeError("Unsupported Type")

def pass_unit(_: T0) -> hydra.compute.Flow[T1, hydra.compute.Adapter[T2, T3, hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term]]:
    return hydra.lib.flows.pure(hydra.compute.Adapter(False, cast(hydra.core.Type, hydra.core.TypeUnit()), cast(hydra.core.Type, hydra.core.TypeUnit()), hydra.compute.Coder((lambda _2: hydra.lib.flows.pure(cast(hydra.core.Term, hydra.core.TermUnit()))), (lambda _2: hydra.lib.flows.pure(cast(hydra.core.Term, hydra.core.TermUnit()))))))

def union_type_to_record_type(rt: hydra.core.RowType) -> hydra.core.RowType:
    r"""Convert a union row type to a record row type."""
    
    def make_optional(f: hydra.core.FieldType) -> hydra.core.FieldType:
        @lru_cache(1)
        def fn() -> hydra.core.Name:
            return f.name
        @lru_cache(1)
        def ft() -> hydra.core.Type:
            return f.type
        return hydra.core.FieldType(fn(), hydra.rewriting.map_beneath_type_annotations((lambda x: cast(hydra.core.Type, hydra.core.TypeMaybe(x))), ft()))
    return hydra.core.RowType(rt.type_name, hydra.lib.lists.map((lambda x1: make_optional(x1)), rt.fields))

def unit_to_record(_: T0) -> hydra.compute.Flow[T1, hydra.compute.Adapter[T2, T3, hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term]]:
    return hydra.lib.flows.pure(hydra.compute.Adapter(False, cast(hydra.core.Type, hydra.core.TypeUnit()), cast(hydra.core.Type, hydra.core.TypeRecord(hydra.core.RowType(hydra.core.Name("_Unit"), ()))), hydra.compute.Coder((lambda _2: hydra.lib.flows.pure(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("_Unit"), ()))))), (lambda _2: hydra.lib.flows.pure(cast(hydra.core.Term, hydra.core.TermUnit()))))))

def field_adapter(ftyp: hydra.core.FieldType) -> hydra.compute.Flow[hydra.coders.AdapterContext, hydra.compute.Adapter[hydra.coders.AdapterContext, hydra.coders.AdapterContext, hydra.core.FieldType, hydra.core.FieldType, hydra.core.Field, hydra.core.Field]]:
    r"""Create an adapter for field types."""
    
    def encdec(ad: hydra.compute.Adapter[T0, T0, T1, T2, hydra.core.Term, hydra.core.Term], dir: hydra.coders.CoderDirection, field: hydra.core.Field) -> hydra.compute.Flow[T0, hydra.core.Field]:
        @lru_cache(1)
        def name() -> hydra.core.Name:
            return field.name
        @lru_cache(1)
        def term() -> hydra.core.Term:
            return field.term
        return hydra.lib.flows.bind(hydra.adapt.utils.encode_decode(dir, ad.coder, term()), (lambda new_term: hydra.lib.flows.pure(hydra.core.Field(name(), new_term))))
    return hydra.lib.flows.bind(term_adapter(ftyp.type), (lambda ad: hydra.lib.flows.pure(hydra.compute.Adapter(ad.is_lossy, ftyp, hydra.core.FieldType(ftyp.name, ad.target), hydra.adapt.utils.bidirectional((lambda v1, v2: encdec(ad, v1, v2)))))))

def for_type_reference(name: hydra.core.Name) -> hydra.compute.Flow[hydra.coders.AdapterContext, hydra.compute.Adapter[hydra.coders.AdapterContext, hydra.coders.AdapterContext, hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term]]:
    r"""This function accounts for recursive type definitions."""
    
    def encdec(name2: hydra.core.Name, dir: hydra.coders.CoderDirection, term: hydra.core.Term) -> hydra.compute.Flow[hydra.coders.AdapterContext, hydra.core.Term]:
        return hydra.lib.flows.bind(hydra.monads.get_state(), (lambda cx: (adapters := cx.adapters, hydra.lib.maybes.maybe(hydra.lib.flows.fail(hydra.lib.strings.cat2("no adapter for reference type ", name2.value)), (lambda ad: hydra.adapt.utils.encode_decode(dir, ad.coder, term)), hydra.lib.maps.lookup(name2, adapters)))[1]))
    def for_type(cx: hydra.coders.AdapterContext, adapters: FrozenDict[hydra.core.Name, hydra.compute.Adapter[hydra.coders.AdapterContext, hydra.coders.AdapterContext, hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term]], t: hydra.core.Type) -> hydra.compute.Flow[hydra.coders.AdapterContext, hydra.compute.Adapter[hydra.coders.AdapterContext, hydra.coders.AdapterContext, hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term]]:
        return hydra.lib.flows.bind(term_adapter(t), (lambda actual: (final_adapters := hydra.lib.maps.insert(name, actual, adapters), final_cx := hydra.coders.AdapterContext(cx.graph, cx.language, final_adapters), hydra.lib.flows.bind(hydra.monads.put_state(final_cx), (lambda ignored2: hydra.lib.flows.pure(actual))))[2]))
    def for_missing_adapter(cx: hydra.coders.AdapterContext, lossy2: bool, adapters: FrozenDict[hydra.core.Name, hydra.compute.Adapter[hydra.coders.AdapterContext, hydra.coders.AdapterContext, hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term]], placeholder2: hydra.compute.Adapter[hydra.coders.AdapterContext, hydra.coders.AdapterContext, hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term]) -> hydra.compute.Flow[hydra.coders.AdapterContext, hydra.compute.Adapter[hydra.coders.AdapterContext, hydra.coders.AdapterContext, hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term]]:
        @lru_cache(1)
        def new_adapters() -> FrozenDict[hydra.core.Name, hydra.compute.Adapter[hydra.coders.AdapterContext, hydra.coders.AdapterContext, hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term]]:
            return hydra.lib.maps.insert(name, placeholder2, adapters)
        @lru_cache(1)
        def new_cx() -> hydra.coders.AdapterContext:
            return hydra.coders.AdapterContext(cx.graph, cx.language, new_adapters())
        return hydra.lib.flows.bind(hydra.monads.put_state(new_cx()), (lambda ignored: hydra.lib.flows.bind(with_graph_context(hydra.schemas.resolve_type(cast(hydra.core.Type, hydra.core.TypeVariable(name)))), (lambda mt: hydra.lib.maybes.maybe(hydra.lib.flows.pure(hydra.compute.Adapter(lossy2, cast(hydra.core.Type, hydra.core.TypeVariable(name)), cast(hydra.core.Type, hydra.core.TypeVariable(name)), hydra.adapt.utils.bidirectional((lambda dir, term: hydra.lib.flows.pure(term))))), (lambda v1: for_type(cx, adapters, v1)), mt)))))
    @lru_cache(1)
    def flow() -> hydra.compute.Flow[hydra.coders.AdapterContext, hydra.compute.Adapter[hydra.coders.AdapterContext, hydra.coders.AdapterContext, hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term]]:
        lossy = False
        @lru_cache(1)
        def placeholder() -> hydra.compute.Adapter[hydra.coders.AdapterContext, hydra.coders.AdapterContext, hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term]:
            return hydra.compute.Adapter(lossy, cast(hydra.core.Type, hydra.core.TypeVariable(name)), cast(hydra.core.Type, hydra.core.TypeVariable(name)), hydra.adapt.utils.bidirectional((lambda v1, v2: encdec(name, v1, v2))))
        return hydra.lib.flows.bind(hydra.monads.get_state(), (lambda cx: (adapters := cx.adapters, hydra.lib.maybes.maybe(for_missing_adapter(cx, lossy, adapters, placeholder()), (lambda x1: hydra.lib.flows.pure(x1)), hydra.lib.maps.lookup(name, adapters)))[1]))
    return hydra.monads.with_trace(hydra.lib.strings.cat2("adapt named type ", name.value), flow())

def function_to_union(t: hydra.core.Type) -> hydra.compute.Flow[hydra.coders.AdapterContext, hydra.compute.Adapter[hydra.coders.AdapterContext, hydra.coders.AdapterContext, hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term]]:
    r"""Convert function types to union types."""
    
    def enc_term(term: hydra.core.Term, stripped_term: hydra.core.Term) -> hydra.core.Term:
        def _hoist_enc_term_1(term: hydra.core.Term, v1: hydra.core.Elimination) -> hydra.core.Term:
            match v1:
                case hydra.core.EliminationWrap(value=name):
                    return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(function_proxy_name, hydra.core.Field(hydra.core.Name("wrap"), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(name.value))))))))
                
                case hydra.core.EliminationRecord():
                    return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(function_proxy_name, hydra.core.Field(hydra.core.Name("record"), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(hydra.show.core.term(term)))))))))
                
                case hydra.core.EliminationUnion():
                    return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(function_proxy_name, hydra.core.Field(hydra.core.Name("union"), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(hydra.show.core.term(term)))))))))
                
                case _:
                    raise AssertionError("Unreachable: all variants handled")
        def _hoist_enc_term_2(term: hydra.core.Term, v1: hydra.core.Function) -> hydra.core.Term:
            match v1:
                case hydra.core.FunctionElimination(value=e):
                    return _hoist_enc_term_1(term, e)
                
                case hydra.core.FunctionLambda():
                    return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(function_proxy_name, hydra.core.Field(hydra.core.Name("lambda"), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(hydra.show.core.term(term)))))))))
                
                case hydra.core.FunctionPrimitive(value=name):
                    return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(function_proxy_name, hydra.core.Field(hydra.core.Name("primitive"), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(name.value))))))))
                
                case _:
                    raise AssertionError("Unreachable: all variants handled")
        match stripped_term:
            case hydra.core.TermFunction(value=f):
                return _hoist_enc_term_2(term, f)
            
            case hydra.core.TermVariable(value=name):
                return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(function_proxy_name, hydra.core.Field(hydra.core.Name("variable"), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(name.value))))))))
            
            case _:
                raise TypeError("Unsupported Term")
    def encode(ad: hydra.compute.Adapter[T0, T1, T2, T3, hydra.core.Term, T4], term: hydra.core.Term) -> hydra.compute.Flow[T0, T4]:
        @lru_cache(1)
        def stripped_term() -> hydra.core.Term:
            return hydra.rewriting.deannotate_term(term)
        return ad.coder.encode(enc_term(term, stripped_term()))
    def read_from_string(term: hydra.core.Term) -> hydra.compute.Flow[hydra.graph.Graph, hydra.core.Term]:
        return hydra.lib.flows.bind(hydra.extract.core.string(term), (lambda s: hydra.lib.maybes.maybe(hydra.lib.flows.fail(hydra.lib.strings.cat2("failed to parse term: ", s)), (lambda x1: hydra.lib.flows.pure(x1)), hydra.show.core.read_term(s))))
    def decode(ad: hydra.compute.Adapter[T0, hydra.coders.AdapterContext, T1, T2, hydra.core.Term, T3], term: T3) -> hydra.compute.Flow[hydra.coders.AdapterContext, hydra.core.Term]:
        def not_found(fname: hydra.core.Name) -> hydra.compute.Flow[T4, T5]:
            return hydra.lib.flows.fail(hydra.lib.strings.cat2("unexpected field: ", fname.value))
        def for_cases(fterm: hydra.core.Term) -> hydra.compute.Flow[hydra.coders.AdapterContext, hydra.core.Term]:
            return with_graph_context(read_from_string(fterm))
        def for_lambda(fterm: hydra.core.Term) -> hydra.compute.Flow[hydra.coders.AdapterContext, hydra.core.Term]:
            return with_graph_context(read_from_string(fterm))
        def for_wrapped(fterm: hydra.core.Term) -> hydra.compute.Flow[hydra.coders.AdapterContext, hydra.core.Term]:
            return with_graph_context(hydra.lib.flows.map((lambda s: cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationWrap(hydra.core.Name(s)))))))), hydra.extract.core.string(fterm)))
        def for_primitive(fterm: hydra.core.Term) -> hydra.compute.Flow[hydra.coders.AdapterContext, hydra.core.Term]:
            return with_graph_context(hydra.lib.flows.map((lambda s: cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionPrimitive(hydra.core.Name(s)))))), hydra.extract.core.string(fterm)))
        def for_projection(fterm: hydra.core.Term) -> hydra.compute.Flow[hydra.coders.AdapterContext, hydra.core.Term]:
            return with_graph_context(read_from_string(fterm))
        def for_variable(fterm: hydra.core.Term) -> hydra.compute.Flow[hydra.coders.AdapterContext, hydra.core.Term]:
            return with_graph_context(hydra.lib.flows.map((lambda s: cast(hydra.core.Term, hydra.core.TermVariable(hydra.core.Name(s)))), hydra.extract.core.string(fterm)))
        return hydra.lib.flows.bind(ad.coder.decode(term), (lambda inj_term: hydra.lib.flows.bind(with_graph_context(hydra.extract.core.injection(function_proxy_name, inj_term)), (lambda field: (fname := field.name, fterm := field.term, hydra.lib.maybes.from_maybe(not_found(fname), hydra.lib.maps.lookup(fname, hydra.lib.maps.from_list(((hydra.core.Name("wrap"), for_wrapped(fterm)), (hydra.core.Name("record"), for_projection(fterm)), (hydra.core.Name("union"), for_cases(fterm)), (hydra.core.Name("lambda"), for_lambda(fterm)), (hydra.core.Name("primitive"), for_primitive(fterm)), (hydra.core.Name("variable"), for_variable(fterm)))))))[2]))))
    match t:
        case hydra.core.TypeFunction(value=ft):
            @lru_cache(1)
            def dom() -> hydra.core.Type:
                return ft.domain
            @lru_cache(1)
            def cod() -> hydra.core.Type:
                return ft.codomain
            @lru_cache(1)
            def union_type() -> hydra.compute.Flow[hydra.coders.AdapterContext, hydra.core.Type]:
                return hydra.lib.flows.bind(term_adapter(dom()), (lambda dom_ad: hydra.lib.flows.pure(cast(hydra.core.Type, hydra.core.TypeUnion(hydra.core.RowType(function_proxy_name, (hydra.core.FieldType(hydra.core.Name("wrap"), cast(hydra.core.Type, hydra.core.TypeLiteral(cast(hydra.core.LiteralType, hydra.core.LiteralTypeString())))), hydra.core.FieldType(hydra.core.Name("record"), cast(hydra.core.Type, hydra.core.TypeLiteral(cast(hydra.core.LiteralType, hydra.core.LiteralTypeString())))), hydra.core.FieldType(hydra.core.Name("union"), cast(hydra.core.Type, hydra.core.TypeLiteral(cast(hydra.core.LiteralType, hydra.core.LiteralTypeString())))), hydra.core.FieldType(hydra.core.Name("lambda"), cast(hydra.core.Type, hydra.core.TypeLiteral(cast(hydra.core.LiteralType, hydra.core.LiteralTypeString())))), hydra.core.FieldType(hydra.core.Name("primitive"), cast(hydra.core.Type, hydra.core.TypeLiteral(cast(hydra.core.LiteralType, hydra.core.LiteralTypeString())))), hydra.core.FieldType(hydra.core.Name("variable"), cast(hydra.core.Type, hydra.core.TypeLiteral(cast(hydra.core.LiteralType, hydra.core.LiteralTypeString())))))))))))
            return hydra.lib.flows.bind(union_type(), (lambda ut: hydra.lib.flows.bind(term_adapter(ut), (lambda ad: hydra.lib.flows.pure(hydra.compute.Adapter(ad.is_lossy, t, ad.target, hydra.compute.Coder((lambda v1: encode(ad, v1)), (lambda v1: decode(ad, v1)))))))))
        
        case _:
            raise TypeError("Unsupported Type")

def lambda_to_monotype(t: hydra.core.Type) -> hydra.compute.Flow[hydra.coders.AdapterContext, hydra.compute.Adapter[hydra.coders.AdapterContext, hydra.coders.AdapterContext, hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term]]:
    r"""Convert forall types to monotypes."""
    
    match t:
        case hydra.core.TypeForall(value=ft):
            @lru_cache(1)
            def body() -> hydra.core.Type:
                return ft.body
            return hydra.lib.flows.bind(term_adapter(body()), (lambda ad: hydra.lib.flows.pure(hydra.compute.Adapter(ad.is_lossy, t, ad.target, ad.coder))))
        
        case _:
            raise TypeError("Unsupported Type")

def maybe_to_list(t: hydra.core.Type) -> hydra.compute.Flow[hydra.coders.AdapterContext, hydra.compute.Adapter[hydra.coders.AdapterContext, hydra.coders.AdapterContext, hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term]]:
    r"""Convert optional types to list types."""
    
    def encode(ad: hydra.compute.Adapter[T0, T1, T2, T3, hydra.core.Term, hydra.core.Term], term: hydra.core.Term) -> hydra.compute.Flow[T0, hydra.core.Term]:
        match term:
            case hydra.core.TermMaybe(value=m):
                return hydra.lib.maybes.maybe(hydra.lib.flows.pure(cast(hydra.core.Term, hydra.core.TermList(()))), (lambda r: hydra.lib.flows.bind(ad.coder.encode(r), (lambda encoded: hydra.lib.flows.pure(cast(hydra.core.Term, hydra.core.TermList((encoded,))))))), m)
            
            case _:
                raise TypeError("Unsupported Term")
    def decode(ad: hydra.compute.Adapter[T0, T1, T2, T3, hydra.core.Term, hydra.core.Term], term: hydra.core.Term) -> hydra.compute.Flow[T1, hydra.core.Term]:
        match term:
            case hydra.core.TermList(value=l):
                return hydra.lib.flows.map((lambda x: cast(hydra.core.Term, hydra.core.TermMaybe(x))), hydra.lib.logic.if_else(hydra.lib.lists.null(l), (lambda : hydra.lib.flows.pure(Nothing())), (lambda : hydra.lib.flows.bind(ad.coder.decode(hydra.lib.lists.head(l)), (lambda decoded: hydra.lib.flows.pure(Just(decoded)))))))
            
            case _:
                raise TypeError("Unsupported Term")
    match t:
        case hydra.core.TypeMaybe(value=ot):
            return hydra.lib.flows.bind(term_adapter(ot), (lambda ad: hydra.lib.flows.pure(hydra.compute.Adapter(False, t, cast(hydra.core.Type, hydra.core.TypeList(ad.target)), hydra.compute.Coder((lambda v1: encode(ad, v1)), (lambda v1: decode(ad, v1)))))))
        
        case _:
            raise TypeError("Unsupported Type")

def pass_application(t: hydra.core.Type) -> hydra.compute.Flow[hydra.coders.AdapterContext, hydra.compute.Adapter[hydra.coders.AdapterContext, hydra.coders.AdapterContext, hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term]]:
    r"""Pass through application types."""
    
    def for_application_type(at: hydra.core.ApplicationType) -> hydra.compute.Flow[hydra.coders.AdapterContext, hydra.compute.Adapter[hydra.coders.AdapterContext, hydra.coders.AdapterContext, hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term]]:
        @lru_cache(1)
        def lhs() -> hydra.core.Type:
            return at.function
        @lru_cache(1)
        def rhs() -> hydra.core.Type:
            return at.argument
        return hydra.lib.flows.bind(term_adapter(lhs()), (lambda lhs_ad: hydra.lib.flows.bind(term_adapter(rhs()), (lambda rhs_ad: hydra.lib.flows.pure(hydra.compute.Adapter(hydra.lib.logic.or_(lhs_ad.is_lossy, rhs_ad.is_lossy), t, cast(hydra.core.Type, hydra.core.TypeApplication(hydra.core.ApplicationType(lhs_ad.target, rhs_ad.target))), hydra.adapt.utils.bidirectional((lambda dir, term: hydra.adapt.utils.encode_decode(dir, lhs_ad.coder, term)))))))))
    match t:
        case hydra.core.TypeApplication(value=at):
            return for_application_type(at)
        
        case _:
            raise TypeError("Unsupported Type")

def pass_either(t: hydra.core.Type) -> hydra.compute.Flow[hydra.coders.AdapterContext, hydra.compute.Adapter[hydra.coders.AdapterContext, hydra.coders.AdapterContext, hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term]]:
    r"""Pass through either types."""
    
    def for_either_type(et: hydra.core.EitherType) -> hydra.compute.Flow[hydra.coders.AdapterContext, hydra.compute.Adapter[hydra.coders.AdapterContext, hydra.coders.AdapterContext, hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term]]:
        @lru_cache(1)
        def left() -> hydra.core.Type:
            return et.left
        @lru_cache(1)
        def right() -> hydra.core.Type:
            return et.right
        return hydra.lib.flows.bind(term_adapter(left()), (lambda left_ad: hydra.lib.flows.bind(term_adapter(right()), (lambda right_ad: hydra.lib.flows.pure(hydra.compute.Adapter(hydra.lib.logic.or_(left_ad.is_lossy, right_ad.is_lossy), t, cast(hydra.core.Type, hydra.core.TypeEither(hydra.core.EitherType(left_ad.target, right_ad.target))), hydra.adapt.utils.bidirectional((lambda dir, term: hydra.adapt.utils.encode_decode(dir, left_ad.coder, term)))))))))
    match t:
        case hydra.core.TypeEither(value=et):
            return for_either_type(et)
        
        case _:
            raise TypeError("Unsupported Type")

def pass_forall(t: hydra.core.Type) -> hydra.compute.Flow[hydra.coders.AdapterContext, hydra.compute.Adapter[hydra.coders.AdapterContext, hydra.coders.AdapterContext, hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term]]:
    r"""Pass through forall types."""
    
    def for_forall_type(ft: hydra.core.ForallType) -> hydra.compute.Flow[hydra.coders.AdapterContext, hydra.compute.Adapter[hydra.coders.AdapterContext, hydra.coders.AdapterContext, hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term]]:
        @lru_cache(1)
        def v() -> hydra.core.Name:
            return ft.parameter
        @lru_cache(1)
        def body() -> hydra.core.Type:
            return ft.body
        return hydra.lib.flows.bind(term_adapter(body()), (lambda ad: hydra.lib.flows.pure(hydra.compute.Adapter(ad.is_lossy, t, cast(hydra.core.Type, hydra.core.TypeForall(hydra.core.ForallType(v(), ad.target))), hydra.adapt.utils.bidirectional((lambda dir, term: hydra.adapt.utils.encode_decode(dir, ad.coder, term)))))))
    match t:
        case hydra.core.TypeForall(value=ft):
            return for_forall_type(ft)
        
        case _:
            raise TypeError("Unsupported Type")

def pass_function(t: hydra.core.Type) -> hydra.compute.Flow[hydra.coders.AdapterContext, hydra.compute.Adapter[hydra.coders.AdapterContext, hydra.coders.AdapterContext, hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term]]:
    r"""Pass through function types with adaptation."""
    
    def to_case_ads(dom: hydra.core.Type, cod: hydra.core.Type) -> hydra.compute.Flow[hydra.coders.AdapterContext, FrozenDict[hydra.core.Name, hydra.compute.Adapter[hydra.coders.AdapterContext, hydra.coders.AdapterContext, hydra.core.FieldType, hydra.core.FieldType, hydra.core.Field, hydra.core.Field]]]:
        match hydra.rewriting.deannotate_type(dom):
            case hydra.core.TypeUnion(value=rt):
                return hydra.lib.flows.bind(hydra.lib.flows.map_list((lambda f: hydra.lib.flows.bind(field_adapter(hydra.core.FieldType(f.name, cast(hydra.core.Type, hydra.core.TypeFunction(hydra.core.FunctionType(f.type, cod))))), (lambda ad: hydra.lib.flows.pure((f.name, ad))))), rt.fields), (lambda pairs: hydra.lib.flows.pure(hydra.lib.maps.from_list(pairs))))
            
            case _:
                return hydra.lib.flows.pure(hydra.lib.maps.empty())
    def to_option_ad(dom: hydra.core.Type, cod: hydra.core.Type) -> hydra.compute.Flow[hydra.coders.AdapterContext, Maybe[hydra.compute.Adapter[hydra.coders.AdapterContext, hydra.coders.AdapterContext, hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term]]]:
        match hydra.rewriting.deannotate_type(dom):
            case hydra.core.TypeMaybe(value=ot):
                return hydra.lib.flows.map((lambda x1: hydra.lib.maybes.pure(x1)), term_adapter(cast(hydra.core.Type, hydra.core.TypeFunction(hydra.core.FunctionType(ot, cod)))))
            
            case _:
                return hydra.lib.flows.pure(Nothing())
    def get_coder(case_ads: FrozenDict[T0, hydra.compute.Adapter[T1, T2, T3, T4, T5, T5]], fname: T0) -> hydra.compute.Coder[T1, T2, T5, T5]:
        return hydra.lib.maybes.maybe(hydra.adapt.utils.id_coder(), (lambda v1: v1.coder), hydra.lib.maps.lookup(fname, case_ads))
    def for_elimination(dir: hydra.coders.CoderDirection, cod_ad: hydra.compute.Adapter[T0, T0, T1, T2, hydra.core.Term, hydra.core.Term], case_ads: FrozenDict[hydra.core.Name, hydra.compute.Adapter[T0, T0, T3, T4, hydra.core.Field, hydra.core.Field]], e: hydra.core.Elimination) -> hydra.compute.Flow[T0, hydra.core.Elimination]:
        match e:
            case hydra.core.EliminationUnion(value=cs):
                @lru_cache(1)
                def n() -> hydra.core.Name:
                    return cs.type_name
                @lru_cache(1)
                def def_() -> Maybe[hydra.core.Term]:
                    return cs.default
                @lru_cache(1)
                def cases() -> frozenlist[hydra.core.Field]:
                    return cs.cases
                return hydra.lib.flows.bind(hydra.lib.flows.map_list((lambda f: hydra.adapt.utils.encode_decode(dir, get_coder(case_ads, f.name), f)), cases()), (lambda rcases: hydra.lib.flows.bind(hydra.lib.maybes.maybe(hydra.lib.flows.pure(Nothing()), (lambda d: hydra.lib.flows.map((lambda x1: hydra.lib.maybes.pure(x1)), hydra.adapt.utils.encode_decode(dir, cod_ad.coder, d))), def_()), (lambda rdef: hydra.lib.flows.pure(cast(hydra.core.Elimination, hydra.core.EliminationUnion(hydra.core.CaseStatement(n(), rdef, rcases))))))))
            
            case _:
                raise TypeError("Unsupported Elimination")
    def for_function(dir: hydra.coders.CoderDirection, cod_ad: hydra.compute.Adapter[T0, T0, T1, T2, hydra.core.Term, hydra.core.Term], case_ads: FrozenDict[hydra.core.Name, hydra.compute.Adapter[T0, T0, T3, T4, hydra.core.Field, hydra.core.Field]], f: hydra.core.Function) -> hydra.compute.Flow[T0, hydra.core.Function]:
        match f:
            case hydra.core.FunctionElimination(value=e):
                return hydra.lib.flows.map((lambda x: cast(hydra.core.Function, hydra.core.FunctionElimination(x))), for_elimination(dir, cod_ad, case_ads, e))
            
            case hydra.core.FunctionLambda(value=l):
                @lru_cache(1)
                def var() -> hydra.core.Name:
                    return l.parameter
                @lru_cache(1)
                def d() -> Maybe[hydra.core.Type]:
                    return l.domain
                @lru_cache(1)
                def body() -> hydra.core.Term:
                    return l.body
                return hydra.lib.flows.bind(hydra.adapt.utils.encode_decode(dir, cod_ad.coder, body()), (lambda new_body: hydra.lib.flows.pure(cast(hydra.core.Function, hydra.core.FunctionLambda(hydra.core.Lambda(var(), d(), new_body))))))
            
            case hydra.core.FunctionPrimitive(value=name):
                return hydra.lib.flows.pure(cast(hydra.core.Function, hydra.core.FunctionPrimitive(name)))
            
            case _:
                raise AssertionError("Unreachable: all variants handled")
    def encdec(cod_ad: hydra.compute.Adapter[T0, T0, T1, T2, hydra.core.Term, hydra.core.Term], case_ads: FrozenDict[hydra.core.Name, hydra.compute.Adapter[T0, T0, T3, T4, hydra.core.Field, hydra.core.Field]], dir: hydra.coders.CoderDirection, term: hydra.core.Term) -> hydra.compute.Flow[T0, hydra.core.Term]:
        match hydra.rewriting.deannotate_term(term):
            case hydra.core.TermFunction(value=f):
                return hydra.lib.flows.map((lambda x: cast(hydra.core.Term, hydra.core.TermFunction(x))), for_function(dir, cod_ad, case_ads, f))
            
            case _:
                return hydra.lib.flows.pure(term)
    def for_function_type(ft: hydra.core.FunctionType) -> hydra.compute.Flow[hydra.coders.AdapterContext, hydra.compute.Adapter[hydra.coders.AdapterContext, hydra.coders.AdapterContext, hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term]]:
        @lru_cache(1)
        def dom() -> hydra.core.Type:
            return ft.domain
        @lru_cache(1)
        def cod() -> hydra.core.Type:
            return ft.codomain
        return hydra.lib.flows.bind(term_adapter(dom()), (lambda dom_ad: hydra.lib.flows.bind(term_adapter(cod()), (lambda cod_ad: hydra.lib.flows.bind(to_case_ads(dom(), cod()), (lambda case_ads: hydra.lib.flows.bind(to_option_ad(dom(), cod()), (lambda option_ad: (lossy := hydra.lib.logic.or_(cod_ad.is_lossy, hydra.lib.lists.foldl(hydra.lib.logic.or_, False, hydra.lib.lists.map((lambda pair: hydra.lib.pairs.second(pair).is_lossy), hydra.lib.maps.to_list(case_ads)))), target := cast(hydra.core.Type, hydra.core.TypeFunction(hydra.core.FunctionType(dom_ad.target, cod_ad.target))), hydra.lib.flows.pure(hydra.compute.Adapter(lossy, t, target, hydra.adapt.utils.bidirectional((lambda v1, v2: encdec(cod_ad, case_ads, v1, v2))))))[2]))))))))
    match t:
        case hydra.core.TypeFunction(value=ft):
            return for_function_type(ft)
        
        case _:
            raise TypeError("Unsupported Type")

def pass_list(t: hydra.core.Type) -> hydra.compute.Flow[hydra.coders.AdapterContext, hydra.compute.Adapter[hydra.coders.AdapterContext, hydra.coders.AdapterContext, hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term]]:
    r"""Pass through list types."""
    
    def encdec(ad: hydra.compute.Adapter[T0, T0, T1, T2, hydra.core.Term, hydra.core.Term], dir: hydra.coders.CoderDirection, term: hydra.core.Term) -> hydra.compute.Flow[T0, hydra.core.Term]:
        match term:
            case hydra.core.TermList(value=terms):
                return hydra.lib.flows.bind(hydra.lib.flows.map_list((lambda v1: hydra.adapt.utils.encode_decode(dir, ad.coder, v1)), terms), (lambda new_terms: hydra.lib.flows.pure(cast(hydra.core.Term, hydra.core.TermList(new_terms)))))
            
            case _:
                raise TypeError("Unsupported Term")
    def for_list_type(lt: hydra.core.Type) -> hydra.compute.Flow[hydra.coders.AdapterContext, hydra.compute.Adapter[hydra.coders.AdapterContext, hydra.coders.AdapterContext, hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term]]:
        return hydra.lib.flows.bind(term_adapter(lt), (lambda ad: hydra.lib.flows.pure(hydra.compute.Adapter(ad.is_lossy, t, cast(hydra.core.Type, hydra.core.TypeList(ad.target)), hydra.adapt.utils.bidirectional((lambda v1, v2: encdec(ad, v1, v2)))))))
    match t:
        case hydra.core.TypeList(value=lt):
            return for_list_type(lt)
        
        case _:
            raise TypeError("Unsupported Type")

def pass_map(t: hydra.core.Type) -> hydra.compute.Flow[hydra.coders.AdapterContext, hydra.compute.Adapter[hydra.coders.AdapterContext, hydra.coders.AdapterContext, hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term]]:
    r"""Pass through map types."""
    
    def encdec(kad: hydra.compute.Adapter[T0, T0, T1, T2, hydra.core.Term, hydra.core.Term], vad: hydra.compute.Adapter[T0, T0, T3, T4, hydra.core.Term, hydra.core.Term], dir: hydra.coders.CoderDirection, term: hydra.core.Term) -> hydra.compute.Flow[T0, hydra.core.Term]:
        match term:
            case hydra.core.TermMap(value=m):
                return hydra.lib.flows.bind(hydra.lib.flows.map_list((lambda pair: (k := hydra.lib.pairs.first(pair), v := hydra.lib.pairs.second(pair), hydra.lib.flows.bind(hydra.adapt.utils.encode_decode(dir, kad.coder, k), (lambda new_k: hydra.lib.flows.bind(hydra.adapt.utils.encode_decode(dir, vad.coder, v), (lambda new_v: hydra.lib.flows.pure((new_k, new_v)))))))[2]), hydra.lib.maps.to_list(m)), (lambda new_pairs: hydra.lib.flows.pure(cast(hydra.core.Term, hydra.core.TermMap(hydra.lib.maps.from_list(new_pairs))))))
            
            case _:
                raise TypeError("Unsupported Term")
    def for_map_type(mt: hydra.core.MapType) -> hydra.compute.Flow[hydra.coders.AdapterContext, hydra.compute.Adapter[hydra.coders.AdapterContext, hydra.coders.AdapterContext, hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term]]:
        @lru_cache(1)
        def kt() -> hydra.core.Type:
            return mt.keys
        @lru_cache(1)
        def vt() -> hydra.core.Type:
            return mt.values
        return hydra.lib.flows.bind(term_adapter(kt()), (lambda kad: hydra.lib.flows.bind(term_adapter(vt()), (lambda vad: hydra.lib.flows.pure(hydra.compute.Adapter(hydra.lib.logic.or_(kad.is_lossy, vad.is_lossy), t, cast(hydra.core.Type, hydra.core.TypeMap(hydra.core.MapType(kad.target, vad.target))), hydra.adapt.utils.bidirectional((lambda v1, v2: encdec(kad, vad, v1, v2)))))))))
    match t:
        case hydra.core.TypeMap(value=mt):
            return for_map_type(mt)
        
        case _:
            raise TypeError("Unsupported Type")

def pass_optional(t: hydra.core.Type) -> hydra.compute.Flow[hydra.coders.AdapterContext, hydra.compute.Adapter[hydra.coders.AdapterContext, hydra.coders.AdapterContext, hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term]]:
    r"""Pass through optional types."""
    
    def map_term(coder: hydra.compute.Coder[hydra.coders.AdapterContext, hydra.coders.AdapterContext, hydra.core.Term, hydra.core.Term], dir: hydra.coders.CoderDirection, term: hydra.core.Term) -> hydra.compute.Flow[hydra.coders.AdapterContext, hydra.core.Term]:
        return hydra.lib.flows.bind(with_graph_context(hydra.extract.core.maybe_term((lambda x1: hydra.lib.flows.pure(x1)), term)), (lambda opt: hydra.lib.flows.bind(hydra.lib.flows.map_maybe((lambda v1: hydra.adapt.utils.encode_decode(dir, coder, v1)), opt), (lambda new_opt: hydra.lib.flows.pure(cast(hydra.core.Term, hydra.core.TermMaybe(new_opt)))))))
    match t:
        case hydra.core.TypeMaybe(value=ot):
            return hydra.lib.flows.bind(term_adapter(ot), (lambda adapter: hydra.lib.flows.pure(hydra.compute.Adapter(adapter.is_lossy, t, cast(hydra.core.Type, hydra.core.TypeMaybe(adapter.target)), hydra.adapt.utils.bidirectional((lambda v1, v2: map_term(adapter.coder, v1, v2)))))))
        
        case _:
            raise TypeError("Unsupported Type")

def pass_record(t: hydra.core.Type) -> hydra.compute.Flow[hydra.coders.AdapterContext, hydra.compute.Adapter[hydra.coders.AdapterContext, hydra.coders.AdapterContext, hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term]]:
    r"""Pass through record types."""
    
    def encdec(rt: hydra.core.RowType, adapters: frozenlist[hydra.compute.Adapter[T0, T0, T1, T2, hydra.core.Field, hydra.core.Field]], dir: hydra.coders.CoderDirection, term: hydra.core.Term) -> hydra.compute.Flow[T0, hydra.core.Term]:
        match term:
            case hydra.core.TermRecord(value=rec):
                @lru_cache(1)
                def dfields() -> frozenlist[hydra.core.Field]:
                    return rec.fields
                return hydra.lib.flows.bind(hydra.lib.flows.sequence(hydra.lib.lists.zip_with((lambda ad, f: hydra.adapt.utils.encode_decode(dir, ad.coder, f)), adapters, dfields())), (lambda new_fields: hydra.lib.flows.pure(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(rt.type_name, new_fields))))))
            
            case _:
                raise TypeError("Unsupported Term")
    def for_record_type(rt: hydra.core.RowType) -> hydra.compute.Flow[hydra.coders.AdapterContext, hydra.compute.Adapter[hydra.coders.AdapterContext, hydra.coders.AdapterContext, hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term]]:
        return hydra.lib.flows.bind(hydra.lib.flows.map_list((lambda x1: field_adapter(x1)), rt.fields), (lambda adapters: (lossy := hydra.lib.lists.foldl(hydra.lib.logic.or_, False, hydra.lib.lists.map((lambda v1: v1.is_lossy), adapters)), sfields_ := hydra.lib.lists.map((lambda v1: v1.target), adapters), hydra.lib.flows.pure(hydra.compute.Adapter(lossy, t, cast(hydra.core.Type, hydra.core.TypeRecord(hydra.core.RowType(rt.type_name, sfields_))), hydra.adapt.utils.bidirectional((lambda v1, v2: encdec(rt, adapters, v1, v2))))))[2]))
    match t:
        case hydra.core.TypeRecord(value=rt):
            return for_record_type(rt)
        
        case _:
            raise TypeError("Unsupported Type")

def pass_set(t: hydra.core.Type) -> hydra.compute.Flow[hydra.coders.AdapterContext, hydra.compute.Adapter[hydra.coders.AdapterContext, hydra.coders.AdapterContext, hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term]]:
    r"""Pass through set types."""
    
    def encdec(ad: hydra.compute.Adapter[T0, T0, T1, T2, hydra.core.Term, hydra.core.Term], dir: hydra.coders.CoderDirection, term: hydra.core.Term) -> hydra.compute.Flow[T0, hydra.core.Term]:
        match term:
            case hydra.core.TermSet(value=terms):
                return hydra.lib.flows.bind(hydra.lib.flows.map_list((lambda v1: hydra.adapt.utils.encode_decode(dir, ad.coder, v1)), hydra.lib.sets.to_list(terms)), (lambda new_terms: hydra.lib.flows.pure(cast(hydra.core.Term, hydra.core.TermSet(hydra.lib.sets.from_list(new_terms))))))
            
            case _:
                raise TypeError("Unsupported Term")
    match t:
        case hydra.core.TypeSet(value=st):
            return hydra.lib.flows.bind(term_adapter(st), (lambda ad: hydra.lib.flows.pure(hydra.compute.Adapter(ad.is_lossy, t, cast(hydra.core.Type, hydra.core.TypeSet(ad.target)), hydra.adapt.utils.bidirectional((lambda v1, v2: encdec(ad, v1, v2)))))))
        
        case _:
            raise TypeError("Unsupported Type")

def pass_union(t: hydra.core.Type) -> hydra.compute.Flow[hydra.coders.AdapterContext, hydra.compute.Adapter[hydra.coders.AdapterContext, hydra.coders.AdapterContext, hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term]]:
    r"""Pass through union types."""
    
    match t:
        case hydra.core.TypeUnion(value=rt):
            @lru_cache(1)
            def sfields() -> frozenlist[hydra.core.FieldType]:
                return rt.fields
            @lru_cache(1)
            def tname() -> hydra.core.Name:
                return rt.type_name
            def get_adapter(adapters_map: FrozenDict[hydra.core.Name, T0], f: hydra.core.Field) -> hydra.compute.Flow[T1, T0]:
                return hydra.lib.maybes.maybe(hydra.lib.flows.fail(hydra.lib.strings.cat2("no such field: ", f.name.value)), (lambda x1: hydra.lib.flows.pure(x1)), hydra.lib.maps.lookup(f.name, adapters_map))
            return hydra.lib.flows.bind(hydra.lib.flows.map_list((lambda f: hydra.lib.flows.bind(field_adapter(f), (lambda ad: hydra.lib.flows.pure((f.name, ad))))), sfields()), (lambda adapters: (adapters_map := hydra.lib.maps.from_list(adapters), lossy := hydra.lib.lists.foldl(hydra.lib.logic.or_, False, hydra.lib.lists.map((lambda pair: hydra.lib.pairs.second(pair).is_lossy), adapters)), sfields_ := hydra.lib.lists.map((lambda pair: hydra.lib.pairs.second(pair).target), adapters), hydra.lib.flows.pure(hydra.compute.Adapter(lossy, t, cast(hydra.core.Type, hydra.core.TypeUnion(hydra.core.RowType(tname(), sfields_))), hydra.adapt.utils.bidirectional((lambda dir, term: hydra.lib.flows.pure(term))))))[3]))
        
        case _:
            raise TypeError("Unsupported Type")

def pass_wrapped(t: hydra.core.Type) -> hydra.compute.Flow[hydra.coders.AdapterContext, hydra.compute.Adapter[hydra.coders.AdapterContext, hydra.coders.AdapterContext, hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term]]:
    r"""Pass through wrapped types."""
    
    match t:
        case hydra.core.TypeWrap(value=wt):
            @lru_cache(1)
            def tname() -> hydra.core.Name:
                return wt.type_name
            @lru_cache(1)
            def ot() -> hydra.core.Type:
                return wt.body
            def map_term(coder: hydra.compute.Coder[hydra.coders.AdapterContext, hydra.coders.AdapterContext, hydra.core.Term, hydra.core.Term], dir: hydra.coders.CoderDirection, term: hydra.core.Term) -> hydra.compute.Flow[hydra.coders.AdapterContext, hydra.core.Term]:
                return hydra.lib.flows.bind(with_graph_context(hydra.extract.core.wrap(tname(), term)), (lambda unwrapped: hydra.lib.flows.bind(hydra.adapt.utils.encode_decode(dir, coder, unwrapped), (lambda new_term: hydra.lib.flows.pure(cast(hydra.core.Term, hydra.core.TermWrap(hydra.core.WrappedTerm(tname(), new_term))))))))
            return hydra.lib.flows.bind(term_adapter(ot()), (lambda adapter: hydra.lib.flows.pure(hydra.compute.Adapter(adapter.is_lossy, t, cast(hydra.core.Type, hydra.core.TypeWrap(hydra.core.WrappedType(tname(), adapter.target))), hydra.adapt.utils.bidirectional((lambda v1, v2: map_term(adapter.coder, v1, v2)))))))
        
        case _:
            raise TypeError("Unsupported Type")

def set_to_list(t: hydra.core.Type) -> hydra.compute.Flow[hydra.coders.AdapterContext, hydra.compute.Adapter[hydra.coders.AdapterContext, hydra.coders.AdapterContext, hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term]]:
    r"""Convert set types to list types."""
    
    def encode(ad: hydra.compute.Adapter[T0, T1, T2, T3, hydra.core.Term, T4], term: hydra.core.Term) -> hydra.compute.Flow[T0, T4]:
        match term:
            case hydra.core.TermSet(value=s):
                return ad.coder.encode(cast(hydra.core.Term, hydra.core.TermList(hydra.lib.sets.to_list(s))))
            
            case _:
                raise TypeError("Unsupported Term")
    def for_list_term(t2: hydra.core.Term) -> hydra.compute.Flow[T0, hydra.core.Term]:
        match t2:
            case hydra.core.TermList(value=l):
                return hydra.lib.flows.pure(cast(hydra.core.Term, hydra.core.TermSet(hydra.lib.sets.from_list(l))))
            
            case _:
                raise TypeError("Unsupported Term")
    def decode(ad: hydra.compute.Adapter[T0, T1, T2, T3, hydra.core.Term, T4], term: T4) -> hydra.compute.Flow[T1, hydra.core.Term]:
        return hydra.lib.flows.bind(ad.coder.decode(term), (lambda list_term: for_list_term(list_term)))
    def for_set_type(st: hydra.core.Type) -> hydra.compute.Flow[hydra.coders.AdapterContext, hydra.compute.Adapter[hydra.coders.AdapterContext, hydra.coders.AdapterContext, hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term]]:
        return hydra.lib.flows.bind(term_adapter(cast(hydra.core.Type, hydra.core.TypeList(st))), (lambda ad: hydra.lib.flows.pure(hydra.compute.Adapter(ad.is_lossy, t, ad.target, hydra.compute.Coder((lambda v1: encode(ad, v1)), (lambda v1: decode(ad, v1)))))))
    match t:
        case hydra.core.TypeSet(value=st):
            return for_set_type(st)
        
        case _:
            raise TypeError("Unsupported Type")

def simplify_application(t: hydra.core.Type) -> hydra.compute.Flow[hydra.coders.AdapterContext, hydra.compute.Adapter[hydra.coders.AdapterContext, hydra.coders.AdapterContext, hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term]]:
    r"""Simplify application types."""
    
    def encdec(ad: hydra.compute.Adapter[T0, T0, T1, T2, T3, T3], dir: hydra.coders.CoderDirection, term: T3) -> hydra.compute.Flow[T0, T3]:
        return hydra.adapt.utils.encode_decode(dir, ad.coder, term)
    def for_application_type(at: hydra.core.ApplicationType) -> hydra.compute.Flow[hydra.coders.AdapterContext, hydra.compute.Adapter[hydra.coders.AdapterContext, hydra.coders.AdapterContext, hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term]]:
        @lru_cache(1)
        def lhs() -> hydra.core.Type:
            return at.function
        return hydra.lib.flows.bind(term_adapter(lhs()), (lambda ad: hydra.lib.flows.pure(hydra.compute.Adapter(False, t, ad.target, hydra.adapt.utils.bidirectional((lambda v1, v2: encdec(ad, v1, v2)))))))
    match t:
        case hydra.core.TypeApplication(value=at):
            return for_application_type(at)
        
        case _:
            raise TypeError("Unsupported Type")

def term_adapter(typ: hydra.core.Type) -> hydra.compute.Flow[hydra.coders.AdapterContext, hydra.compute.Adapter[hydra.coders.AdapterContext, hydra.coders.AdapterContext, hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term]]:
    r"""Create an adapter for any type."""
    
    def constraints(cx: hydra.coders.AdapterContext) -> hydra.coders.LanguageConstraints:
        return cx.language.constraints
    def supported(cx: hydra.coders.AdapterContext, v1: hydra.core.Type) -> bool:
        return hydra.adapt.utils.type_is_supported(constraints(cx), v1)
    def variant_is_supported(cx: hydra.coders.AdapterContext, t: hydra.core.Type) -> bool:
        return hydra.lib.sets.member(hydra.reflect.type_variant(t), constraints(cx).type_variants)
    def supported_at_top_level(cx: hydra.coders.AdapterContext, t: hydra.core.Type) -> bool:
        return hydra.lib.logic.and_(variant_is_supported(cx, t), constraints(cx).types(t))
    def pass_(t: hydra.core.Type) -> frozenlist[Callable[[hydra.core.Type], hydra.compute.Flow[hydra.coders.AdapterContext, hydra.compute.Adapter[hydra.coders.AdapterContext, hydra.coders.AdapterContext, hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term]]]]:
        match hydra.reflect.type_variant(hydra.rewriting.deannotate_type(t)):
            case hydra.variants.TypeVariant.ANNOTATED:
                return ()
            
            case hydra.variants.TypeVariant.APPLICATION:
                return ((lambda x1: pass_application(x1)),)
            
            case hydra.variants.TypeVariant.EITHER:
                return ((lambda x1: pass_either(x1)),)
            
            case hydra.variants.TypeVariant.FORALL:
                return ((lambda x1: pass_forall(x1)),)
            
            case hydra.variants.TypeVariant.FUNCTION:
                return ((lambda x1: pass_function(x1)),)
            
            case hydra.variants.TypeVariant.LIST:
                return ((lambda x1: pass_list(x1)),)
            
            case hydra.variants.TypeVariant.LITERAL:
                return ((lambda x1: pass_literal(x1)),)
            
            case hydra.variants.TypeVariant.MAP:
                return ((lambda x1: pass_map(x1)),)
            
            case hydra.variants.TypeVariant.MAYBE:
                return ((lambda x1: pass_optional(x1)), (lambda x1: maybe_to_list(x1)))
            
            case hydra.variants.TypeVariant.PAIR:
                return ()
            
            case hydra.variants.TypeVariant.RECORD:
                return ((lambda x1: pass_record(x1)),)
            
            case hydra.variants.TypeVariant.SET:
                return ((lambda x1: pass_set(x1)),)
            
            case hydra.variants.TypeVariant.UNION:
                return ((lambda x1: pass_union(x1)),)
            
            case hydra.variants.TypeVariant.UNIT:
                return ((lambda x1: pass_unit(x1)),)
            
            case hydra.variants.TypeVariant.VARIABLE:
                return ()
            
            case hydra.variants.TypeVariant.WRAP:
                return ((lambda x1: pass_wrapped(x1)),)
            
            case _:
                raise AssertionError("Unreachable: all variants handled")
    def try_substitution(t: hydra.core.Type) -> frozenlist[Callable[[hydra.core.Type], hydra.compute.Flow[hydra.coders.AdapterContext, hydra.compute.Adapter[hydra.coders.AdapterContext, hydra.coders.AdapterContext, hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term]]]]:
        match hydra.reflect.type_variant(t):
            case hydra.variants.TypeVariant.ANNOTATED:
                return ()
            
            case hydra.variants.TypeVariant.APPLICATION:
                return ((lambda x1: simplify_application(x1)),)
            
            case hydra.variants.TypeVariant.EITHER:
                return ()
            
            case hydra.variants.TypeVariant.FORALL:
                return ((lambda x1: lambda_to_monotype(x1)),)
            
            case hydra.variants.TypeVariant.FUNCTION:
                return ((lambda x1: function_to_union(x1)),)
            
            case hydra.variants.TypeVariant.LIST:
                return ()
            
            case hydra.variants.TypeVariant.LITERAL:
                return ()
            
            case hydra.variants.TypeVariant.MAP:
                return ()
            
            case hydra.variants.TypeVariant.MAYBE:
                return ((lambda x1: maybe_to_list(x1)),)
            
            case hydra.variants.TypeVariant.PAIR:
                return ()
            
            case hydra.variants.TypeVariant.RECORD:
                return ()
            
            case hydra.variants.TypeVariant.SET:
                return ((lambda x1: set_to_list(x1)),)
            
            case hydra.variants.TypeVariant.UNION:
                return ((lambda x1: union_to_record(x1)),)
            
            case hydra.variants.TypeVariant.UNIT:
                return ((lambda x1: unit_to_record(x1)),)
            
            case hydra.variants.TypeVariant.VARIABLE:
                return ()
            
            case hydra.variants.TypeVariant.WRAP:
                return ((lambda x1: wrap_to_unwrapped(x1)),)
            
            case _:
                raise AssertionError("Unreachable: all variants handled")
    def alts(cx: hydra.coders.AdapterContext, t: hydra.core.Type) -> hydra.compute.Flow[hydra.coders.AdapterContext, frozenlist[hydra.compute.Adapter[hydra.coders.AdapterContext, hydra.coders.AdapterContext, hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term]]]:
        return hydra.lib.flows.map_list((lambda c: c(t)), hydra.lib.logic.if_else(supported_at_top_level(cx, t), (lambda : pass_(t)), (lambda : try_substitution(t))))
    @lru_cache(1)
    def dflt() -> hydra.compute.Flow[hydra.coders.AdapterContext, hydra.compute.Adapter[hydra.coders.AdapterContext, hydra.coders.AdapterContext, hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term]]:
        match typ:
            case hydra.core.TypeVariable(value=name):
                return for_type_reference(name)
            
            case _:
                return hydra.lib.flows.bind(hydra.monads.get_state(), (lambda cx: hydra.adapt.utils.choose_adapter((lambda v1: alts(cx, v1)), (lambda v1: supported(cx, v1)), (lambda x1: hydra.show.core.type(x1)), (lambda x1: hydra.show.core.type(x1)), typ)))
    match typ:
        case hydra.core.TypeAnnotated(value=at):
            return hydra.lib.flows.bind(term_adapter(at.body), (lambda ad: hydra.lib.flows.pure(hydra.compute.Adapter(ad.is_lossy, ad.source, cast(hydra.core.Type, hydra.core.TypeAnnotated(hydra.core.AnnotatedType(ad.target, at.annotation))), ad.coder))))
        
        case _:
            return hydra.monads.with_trace(hydra.lib.strings.cat2("adapter for ", hydra.show.core.type(typ)), dflt())

def union_to_record(t: hydra.core.Type) -> hydra.compute.Flow[hydra.coders.AdapterContext, hydra.compute.Adapter[hydra.coders.AdapterContext, hydra.coders.AdapterContext, hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term]]:
    r"""Convert union types to record types."""
    
    def for_field(field: hydra.core.Field) -> Maybe[hydra.core.Field]:
        @lru_cache(1)
        def fn() -> hydra.core.Name:
            return field.name
        @lru_cache(1)
        def fterm() -> hydra.core.Term:
            return field.term
        match fterm():
            case hydra.core.TermMaybe(value=opt):
                return hydra.lib.maybes.bind(opt, (lambda t2: Just(hydra.core.Field(fn(), t2))))
            
            case _:
                raise TypeError("Unsupported Term")
    def from_record_fields(term: hydra.core.Term, term_: T0, t_: hydra.core.Type, fields: frozenlist[hydra.core.Field]) -> hydra.compute.Flow[T1, hydra.core.Field]:
        @lru_cache(1)
        def matches() -> frozenlist[hydra.core.Field]:
            return hydra.lib.maybes.map_maybe((lambda x1: for_field(x1)), fields)
        return hydra.lib.logic.if_else(hydra.lib.lists.null(matches()), (lambda : hydra.lib.flows.fail(hydra.lib.strings.cat(("cannot convert term back to union: ", hydra.show.core.term(term), " where type = ", hydra.show.core.type(t), "    and target type = ", hydra.show.core.type(t_))))), (lambda : hydra.lib.flows.pure(hydra.lib.lists.head(matches()))))
    def for_rec_term(nm: hydra.core.Name, ad: hydra.compute.Adapter[T0, T1, T2, hydra.core.Type, T3, T4], term: hydra.core.Term, rec_term: hydra.core.Term) -> hydra.compute.Flow[T5, hydra.core.Term]:
        match rec_term:
            case hydra.core.TermRecord(value=rec):
                @lru_cache(1)
                def fields() -> frozenlist[hydra.core.Field]:
                    return rec.fields
                return hydra.lib.flows.bind(from_record_fields(term, cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(nm, fields()))), ad.target, fields()), (lambda result_field: hydra.lib.flows.pure(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(nm, result_field))))))
            
            case _:
                raise TypeError("Unsupported Term")
    match t:
        case hydra.core.TypeUnion(value=rt):
            @lru_cache(1)
            def nm() -> hydra.core.Name:
                return rt.type_name
            @lru_cache(1)
            def sfields() -> frozenlist[hydra.core.FieldType]:
                return rt.fields
            @lru_cache(1)
            def target() -> hydra.core.Type:
                return cast(hydra.core.Type, hydra.core.TypeRecord(union_type_to_record_type(rt)))
            def to_record_field(term: hydra.core.Term, fn: hydra.core.Name, f: hydra.core.FieldType) -> hydra.core.Field:
                @lru_cache(1)
                def fn_() -> hydra.core.Name:
                    return f.name
                return hydra.core.Field(fn_(), cast(hydra.core.Term, hydra.core.TermMaybe(hydra.lib.logic.if_else(hydra.lib.equality.equal(fn_(), fn), (lambda : Just(term)), (lambda : Nothing())))))
            return hydra.lib.flows.bind(term_adapter(target()), (lambda ad: hydra.lib.flows.pure(hydra.compute.Adapter(ad.is_lossy, t, ad.target, hydra.compute.Coder((lambda term_: hydra.lib.flows.bind(with_graph_context(hydra.extract.core.injection(rt.type_name, term_)), (lambda field: (fn := field.name, term := field.term, ad.coder.encode(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(nm(), hydra.lib.lists.map((lambda v1: to_record_field(term, fn, v1)), sfields()))))))[2]))), (lambda term: hydra.lib.flows.bind(ad.coder.decode(term), (lambda rec_term: for_rec_term(nm(), ad, term, rec_term)))))))))
        
        case _:
            raise TypeError("Unsupported Type")

def wrap_to_unwrapped(t: hydra.core.Type) -> hydra.compute.Flow[hydra.coders.AdapterContext, hydra.compute.Adapter[hydra.coders.AdapterContext, hydra.coders.AdapterContext, hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term]]:
    r"""Convert wrapped types to unwrapped types."""
    
    match t:
        case hydra.core.TypeWrap(value=wt):
            @lru_cache(1)
            def tname() -> hydra.core.Name:
                return wt.type_name
            @lru_cache(1)
            def typ() -> hydra.core.Type:
                return wt.body
            def encode(ad: hydra.compute.Adapter[hydra.coders.AdapterContext, T0, T1, T2, hydra.core.Term, T3], term: hydra.core.Term) -> hydra.compute.Flow[hydra.coders.AdapterContext, T3]:
                return hydra.lib.flows.bind(with_graph_context(hydra.extract.core.wrap(tname(), term)), (lambda unwrapped: ad.coder.encode(unwrapped)))
            def decode(ad: hydra.compute.Adapter[T0, T1, T2, T3, hydra.core.Term, T4], term: T4) -> hydra.compute.Flow[T1, hydra.core.Term]:
                return hydra.lib.flows.bind(ad.coder.decode(term), (lambda decoded: hydra.lib.flows.pure(cast(hydra.core.Term, hydra.core.TermWrap(hydra.core.WrappedTerm(tname(), decoded))))))
            return hydra.lib.flows.bind(term_adapter(typ()), (lambda ad: hydra.lib.flows.pure(hydra.compute.Adapter(False, t, ad.target, hydra.compute.Coder((lambda v1: encode(ad, v1)), (lambda v1: decode(ad, v1)))))))
        
        case _:
            raise TypeError("Unsupported Type")

def function_proxy_type(_: T0) -> hydra.core.Type:
    return cast(hydra.core.Type, hydra.core.TypeUnion(hydra.core.RowType(function_proxy_name, (hydra.core.FieldType(hydra.core.Name("wrap"), cast(hydra.core.Type, hydra.core.TypeLiteral(cast(hydra.core.LiteralType, hydra.core.LiteralTypeString())))), hydra.core.FieldType(hydra.core.Name("record"), cast(hydra.core.Type, hydra.core.TypeLiteral(cast(hydra.core.LiteralType, hydra.core.LiteralTypeString())))), hydra.core.FieldType(hydra.core.Name("union"), cast(hydra.core.Type, hydra.core.TypeLiteral(cast(hydra.core.LiteralType, hydra.core.LiteralTypeString())))), hydra.core.FieldType(hydra.core.Name("lambda"), cast(hydra.core.Type, hydra.core.TypeLiteral(cast(hydra.core.LiteralType, hydra.core.LiteralTypeString())))), hydra.core.FieldType(hydra.core.Name("primitive"), cast(hydra.core.Type, hydra.core.TypeLiteral(cast(hydra.core.LiteralType, hydra.core.LiteralTypeString())))), hydra.core.FieldType(hydra.core.Name("variable"), cast(hydra.core.Type, hydra.core.TypeLiteral(cast(hydra.core.LiteralType, hydra.core.LiteralTypeString()))))))))
