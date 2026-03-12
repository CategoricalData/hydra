# Note: this is an automatically generated file. Do not edit.

r"""Adapter framework for types and terms."""

from __future__ import annotations
from collections.abc import Callable
from functools import lru_cache
from hydra.dsl.python import Either, FrozenDict, Just, Left, Maybe, Nothing, Right, frozenlist
from typing import TypeVar, cast
import hydra.adapt.literals
import hydra.adapt.utils
import hydra.coder_utils
import hydra.coders
import hydra.compute
import hydra.context
import hydra.core
import hydra.error
import hydra.extract.core
import hydra.lib.eithers
import hydra.lib.equality
import hydra.lib.lists
import hydra.lib.logic
import hydra.lib.maps
import hydra.lib.maybes
import hydra.lib.pairs
import hydra.lib.sets
import hydra.lib.strings
import hydra.reflect
import hydra.rewriting
import hydra.schemas
import hydra.show.core
import hydra.variants

T0 = TypeVar("T0")
T1 = TypeVar("T1")
T2 = TypeVar("T2")
T3 = TypeVar("T3")

function_proxy_name = hydra.core.Name("hydra.core.FunctionProxy")

def pass_literal(cx: hydra.coders.AdapterContext, t: hydra.core.Type) -> Either[str, hydra.compute.Adapter[hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term]]:
    r"""Pass through literal types with literal adaptation."""
    
    def encdec(graph: hydra.graph.Graph, ad: hydra.compute.Adapter[T0, T1, hydra.core.Literal, hydra.core.Literal], dir: hydra.coders.CoderDirection, cx2: hydra.context.Context, term: hydra.core.Term) -> Either[hydra.context.InContext[hydra.error.OtherError], hydra.core.Term]:
        return hydra.lib.eithers.bind(hydra.extract.core.literal(cx2, graph, term), (lambda l: hydra.lib.eithers.bind(hydra.adapt.utils.encode_decode(dir, ad.coder, cx2, l), (lambda l2: Right(cast(hydra.core.Term, hydra.core.TermLiteral(l2)))))))
    def for_literal(lt: hydra.core.LiteralType) -> Either[str, hydra.compute.Adapter[hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term]]:
        return hydra.lib.eithers.bind(hydra.adapt.literals.literal_adapter(cx, lt), (lambda ad: (step := hydra.adapt.utils.bidirectional((lambda v1, v2, v3: encdec(cx.graph, ad, v1, v2, v3))), Right(hydra.compute.Adapter(ad.is_lossy, cast(hydra.core.Type, hydra.core.TypeLiteral(ad.source)), cast(hydra.core.Type, hydra.core.TypeLiteral(ad.target)), step)))[1]))
    match t:
        case hydra.core.TypeLiteral(value=lt):
            return for_literal(lt)
        
        case _:
            raise TypeError("Unsupported Type")

def pass_unit(_cx: T0, _: T1) -> Either[T2, hydra.compute.Adapter[hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term]]:
    r"""Pass through unit types."""
    
    return Right(hydra.compute.Adapter(False, cast(hydra.core.Type, hydra.core.TypeUnit()), cast(hydra.core.Type, hydra.core.TypeUnit()), hydra.compute.Coder((lambda _cx2, _2: Right(cast(hydra.core.Term, hydra.core.TermUnit()))), (lambda _cx2, _2: Right(cast(hydra.core.Term, hydra.core.TermUnit()))))))

def unit_to_record(_cx: T0, _: T1) -> Either[T2, hydra.compute.Adapter[hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term]]:
    r"""Convert unit terms to records."""
    
    return Right(hydra.compute.Adapter(False, cast(hydra.core.Type, hydra.core.TypeUnit()), cast(hydra.core.Type, hydra.core.TypeRecord(hydra.core.RowType(hydra.core.Name("_Unit"), ()))), hydra.compute.Coder((lambda _cx2, _2: Right(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("_Unit"), ()))))), (lambda _cx2, _2: Right(cast(hydra.core.Term, hydra.core.TermUnit()))))))

def field_adapter(cx: hydra.coders.AdapterContext, ftyp: hydra.core.FieldType) -> Either[str, hydra.compute.Adapter[hydra.core.FieldType, hydra.core.FieldType, hydra.core.Field, hydra.core.Field]]:
    r"""Create an adapter for field types."""
    
    def encdec(ad: hydra.compute.Adapter[T0, T1, hydra.core.Term, hydra.core.Term], dir: hydra.coders.CoderDirection, cx2: hydra.context.Context, field: hydra.core.Field) -> Either[hydra.context.InContext[hydra.error.OtherError], hydra.core.Field]:
        name = field.name
        term = field.term
        return hydra.lib.eithers.bind(hydra.adapt.utils.encode_decode(dir, ad.coder, cx2, term), (lambda new_term: Right(hydra.core.Field(name, new_term))))
    return hydra.lib.eithers.bind(term_adapter(cx, ftyp.type), (lambda ad: Right(hydra.compute.Adapter(ad.is_lossy, ftyp, hydra.core.FieldType(ftyp.name, ad.target), hydra.adapt.utils.bidirectional((lambda v1, v2, v3: encdec(ad, v1, v2, v3)))))))

def for_type_reference(cx: hydra.coders.AdapterContext, name: hydra.core.Name) -> Either[str, hydra.compute.Adapter[hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term]]:
    r"""This function accounts for recursive type definitions."""
    
    def encdec(name2: hydra.core.Name, adapters0: FrozenDict[hydra.core.Name, hydra.compute.Adapter[T0, T1, T2, T2]], dir: hydra.coders.CoderDirection, cx2: hydra.context.Context, term: T2) -> Either[hydra.context.InContext[hydra.error.OtherError], T2]:
        return hydra.lib.maybes.maybe((lambda : Left(hydra.context.InContext(hydra.error.OtherError(hydra.lib.strings.cat2("no adapter for reference type ", name2.value)), cx2))), (lambda ad: hydra.adapt.utils.encode_decode(dir, ad.coder, cx2, term)), hydra.lib.maps.lookup(name2, adapters0))
    def for_type(cx2: hydra.coders.AdapterContext, adapters0: T0, t: hydra.core.Type) -> Either[str, hydra.compute.Adapter[hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term]]:
        return hydra.lib.eithers.bind(term_adapter(cx2, t), (lambda actual: Right(actual)))
    def for_missing_adapter(cx2: hydra.coders.AdapterContext, lossy: bool, adapters0: FrozenDict[hydra.core.Name, hydra.compute.Adapter[hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term]], placeholder: hydra.compute.Adapter[hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term]) -> Either[str, hydra.compute.Adapter[hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term]]:
        @lru_cache(1)
        def new_adapters() -> FrozenDict[hydra.core.Name, hydra.compute.Adapter[hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term]]:
            return hydra.lib.maps.insert(name, placeholder, adapters0)
        new_cx = hydra.coders.AdapterContext(cx2.graph, cx2.language, new_adapters())
        @lru_cache(1)
        def mt() -> Maybe[hydra.core.Type]:
            return hydra.schemas.resolve_type(new_cx.graph, cast(hydra.core.Type, hydra.core.TypeVariable(name)))
        return hydra.lib.maybes.maybe((lambda : Right(hydra.compute.Adapter(lossy, cast(hydra.core.Type, hydra.core.TypeVariable(name)), cast(hydra.core.Type, hydra.core.TypeVariable(name)), hydra.adapt.utils.bidirectional((lambda dir, _cx, term: Right(term)))))), (lambda v1: for_type(cx2, adapters0, v1)), mt())
    lossy = False
    adapters = cx.adapters
    @lru_cache(1)
    def placeholder() -> hydra.compute.Adapter[hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term]:
        return hydra.compute.Adapter(lossy, cast(hydra.core.Type, hydra.core.TypeVariable(name)), cast(hydra.core.Type, hydra.core.TypeVariable(name)), hydra.adapt.utils.bidirectional((lambda v1, v2, v3: encdec(name, adapters, v1, v2, v3))))
    return hydra.lib.maybes.maybe((lambda : for_missing_adapter(cx, lossy, adapters, placeholder())), (lambda x: Right(x)), hydra.lib.maps.lookup(name, adapters))

def function_to_union(cx: hydra.coders.AdapterContext, t: hydra.core.Type) -> Either[str, hydra.compute.Adapter[hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term]]:
    r"""Convert function types to union types."""
    
    def enc_term(term: hydra.core.Term, stripped_term: hydra.core.Term):
        def _hoist_enc_term_1(term, v1):
            match v1:
                case hydra.core.EliminationWrap(value=name):
                    return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(function_proxy_name, hydra.core.Field(hydra.core.Name("wrap"), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(name.value))))))))
                
                case hydra.core.EliminationRecord():
                    return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(function_proxy_name, hydra.core.Field(hydra.core.Name("record"), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(hydra.show.core.term(term)))))))))
                
                case hydra.core.EliminationUnion():
                    return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(function_proxy_name, hydra.core.Field(hydra.core.Name("union"), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(hydra.show.core.term(term)))))))))
                
                case _:
                    raise AssertionError("Unreachable: all variants handled")
        def _hoist_enc_term_2(term, v1):
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
    def encode(ad: hydra.compute.Adapter[T0, T1, hydra.core.Term, T2], cx2: hydra.context.Context, term: hydra.core.Term) -> Either[hydra.context.InContext[hydra.error.OtherError], T2]:
        @lru_cache(1)
        def stripped_term() -> hydra.core.Term:
            return hydra.rewriting.deannotate_term(term)
        return ad.coder.encode(cx2, enc_term(term, stripped_term()))
    def read_from_string(cx2: hydra.context.Context, graph: hydra.graph.Graph, term: hydra.core.Term) -> Either[hydra.context.InContext[hydra.error.OtherError], hydra.core.Term]:
        return hydra.lib.eithers.bind(hydra.extract.core.string(cx2, graph, term), (lambda s: hydra.lib.maybes.maybe((lambda : Left(hydra.context.InContext(hydra.error.OtherError(hydra.lib.strings.cat2("failed to parse term: ", s)), cx2))), (lambda x: Right(x)), hydra.show.core.read_term(s))))
    def decode(graph: hydra.graph.Graph, ad: hydra.compute.Adapter[T0, T1, hydra.core.Term, T2], cx2: hydra.context.Context, term: T2) -> Either[hydra.context.InContext[hydra.error.OtherError], hydra.core.Term]:
        def not_found(fname: hydra.core.Name) -> Either[hydra.context.InContext[hydra.error.OtherError], T3]:
            return Left(hydra.context.InContext(hydra.error.OtherError(hydra.lib.strings.cat2("unexpected field: ", fname.value)), cx2))
        def for_cases(fterm: hydra.core.Term) -> Either[hydra.context.InContext[hydra.error.OtherError], hydra.core.Term]:
            return read_from_string(cx2, graph, fterm)
        def for_lambda(fterm: hydra.core.Term) -> Either[hydra.context.InContext[hydra.error.OtherError], hydra.core.Term]:
            return read_from_string(cx2, graph, fterm)
        def for_wrapped(fterm: hydra.core.Term) -> Either[hydra.context.InContext[hydra.error.OtherError], hydra.core.Term]:
            return hydra.lib.eithers.bind(hydra.extract.core.string(cx2, graph, fterm), (lambda s: Right(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationWrap(hydra.core.Name(s))))))))))
        def for_primitive(fterm: hydra.core.Term) -> Either[hydra.context.InContext[hydra.error.OtherError], hydra.core.Term]:
            return hydra.lib.eithers.bind(hydra.extract.core.string(cx2, graph, fterm), (lambda s: Right(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionPrimitive(hydra.core.Name(s))))))))
        def for_projection(fterm: hydra.core.Term) -> Either[hydra.context.InContext[hydra.error.OtherError], hydra.core.Term]:
            return read_from_string(cx2, graph, fterm)
        def for_variable(fterm: hydra.core.Term) -> Either[hydra.context.InContext[hydra.error.OtherError], hydra.core.Term]:
            return hydra.lib.eithers.bind(hydra.extract.core.string(cx2, graph, fterm), (lambda s: Right(cast(hydra.core.Term, hydra.core.TermVariable(hydra.core.Name(s))))))
        return hydra.lib.eithers.bind(ad.coder.decode(cx2, term), (lambda inj_term: hydra.lib.eithers.bind(hydra.extract.core.injection(cx2, function_proxy_name, graph, inj_term), (lambda field: (fname := field.name, fterm := field.term, hydra.lib.maybes.from_maybe((lambda : not_found(fname)), hydra.lib.maps.lookup(fname, hydra.lib.maps.from_list(((hydra.core.Name("wrap"), for_wrapped(fterm)), (hydra.core.Name("record"), for_projection(fterm)), (hydra.core.Name("union"), for_cases(fterm)), (hydra.core.Name("lambda"), for_lambda(fterm)), (hydra.core.Name("primitive"), for_primitive(fterm)), (hydra.core.Name("variable"), for_variable(fterm)))))))[2]))))
    match t:
        case hydra.core.TypeFunction(value=ft):
            dom = ft.domain
            cod = ft.codomain
            @lru_cache(1)
            def union_type() -> Either[str, hydra.core.Type]:
                return hydra.lib.eithers.bind(term_adapter(cx, dom), (lambda dom_ad: Right(cast(hydra.core.Type, hydra.core.TypeUnion(hydra.core.RowType(function_proxy_name, (hydra.core.FieldType(hydra.core.Name("wrap"), cast(hydra.core.Type, hydra.core.TypeLiteral(cast(hydra.core.LiteralType, hydra.core.LiteralTypeString())))), hydra.core.FieldType(hydra.core.Name("record"), cast(hydra.core.Type, hydra.core.TypeLiteral(cast(hydra.core.LiteralType, hydra.core.LiteralTypeString())))), hydra.core.FieldType(hydra.core.Name("union"), cast(hydra.core.Type, hydra.core.TypeLiteral(cast(hydra.core.LiteralType, hydra.core.LiteralTypeString())))), hydra.core.FieldType(hydra.core.Name("lambda"), cast(hydra.core.Type, hydra.core.TypeLiteral(cast(hydra.core.LiteralType, hydra.core.LiteralTypeString())))), hydra.core.FieldType(hydra.core.Name("primitive"), cast(hydra.core.Type, hydra.core.TypeLiteral(cast(hydra.core.LiteralType, hydra.core.LiteralTypeString())))), hydra.core.FieldType(hydra.core.Name("variable"), cast(hydra.core.Type, hydra.core.TypeLiteral(cast(hydra.core.LiteralType, hydra.core.LiteralTypeString())))))))))))
            return hydra.lib.eithers.bind(union_type(), (lambda ut: hydra.lib.eithers.bind(term_adapter(cx, ut), (lambda ad: (graph := cx.graph, Right(hydra.compute.Adapter(ad.is_lossy, t, ad.target, hydra.compute.Coder((lambda v1, v2: encode(ad, v1, v2)), (lambda v1, v2: decode(graph, ad, v1, v2))))))[1]))))
        
        case _:
            raise TypeError("Unsupported Type")

def lambda_to_monotype(cx: hydra.coders.AdapterContext, t: hydra.core.Type) -> Either[str, hydra.compute.Adapter[hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term]]:
    r"""Convert forall types to monotypes."""
    
    match t:
        case hydra.core.TypeForall(value=ft):
            body = ft.body
            return hydra.lib.eithers.bind(term_adapter(cx, body), (lambda ad: Right(hydra.compute.Adapter(ad.is_lossy, t, ad.target, ad.coder))))
        
        case _:
            raise TypeError("Unsupported Type")

def maybe_to_list(cx: hydra.coders.AdapterContext, t: hydra.core.Type) -> Either[str, hydra.compute.Adapter[hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term]]:
    r"""Convert optional types to list types."""
    
    def encode(ad: hydra.compute.Adapter[T0, T1, hydra.core.Term, hydra.core.Term], cx2: hydra.context.Context, term: hydra.core.Term) -> Either[hydra.context.InContext[hydra.error.OtherError], hydra.core.Term]:
        match term:
            case hydra.core.TermMaybe(value=m):
                return hydra.lib.maybes.maybe((lambda : Right(cast(hydra.core.Term, hydra.core.TermList(())))), (lambda r: hydra.lib.eithers.bind(hydra.adapt.utils.encode_decode(hydra.coders.CoderDirection.ENCODE, ad.coder, cx2, r), (lambda encoded: Right(cast(hydra.core.Term, hydra.core.TermList((encoded,))))))), m)
            
            case _:
                raise TypeError("Unsupported Term")
    def decode(ad: hydra.compute.Adapter[T0, T1, hydra.core.Term, hydra.core.Term], cx2: hydra.context.Context, term: hydra.core.Term) -> Either[hydra.context.InContext[hydra.error.OtherError], hydra.core.Term]:
        match term:
            case hydra.core.TermList(value=l):
                return hydra.lib.eithers.map((lambda x: cast(hydra.core.Term, hydra.core.TermMaybe(x))), hydra.lib.logic.if_else(hydra.lib.lists.null(l), (lambda : Right(Nothing())), (lambda : hydra.lib.eithers.bind(hydra.adapt.utils.encode_decode(hydra.coders.CoderDirection.DECODE, ad.coder, cx2, hydra.lib.lists.head(l)), (lambda decoded: Right(Just(decoded)))))))
            
            case _:
                raise TypeError("Unsupported Term")
    match t:
        case hydra.core.TypeMaybe(value=ot):
            return hydra.lib.eithers.bind(term_adapter(cx, ot), (lambda ad: Right(hydra.compute.Adapter(False, t, cast(hydra.core.Type, hydra.core.TypeList(ad.target)), hydra.compute.Coder((lambda v1, v2: encode(ad, v1, v2)), (lambda v1, v2: decode(ad, v1, v2)))))))
        
        case _:
            raise TypeError("Unsupported Type")

def pass_application(cx: hydra.coders.AdapterContext, t: hydra.core.Type) -> Either[str, hydra.compute.Adapter[hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term]]:
    r"""Pass through application types."""
    
    def for_application_type(at: hydra.core.ApplicationType) -> Either[str, hydra.compute.Adapter[hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term]]:
        lhs = at.function
        rhs = at.argument
        return hydra.lib.eithers.bind(term_adapter(cx, lhs), (lambda lhs_ad: hydra.lib.eithers.bind(term_adapter(cx, rhs), (lambda rhs_ad: Right(hydra.compute.Adapter(hydra.lib.logic.or_(lhs_ad.is_lossy, rhs_ad.is_lossy), t, cast(hydra.core.Type, hydra.core.TypeApplication(hydra.core.ApplicationType(lhs_ad.target, rhs_ad.target))), hydra.adapt.utils.bidirectional((lambda dir, cx2, term: hydra.adapt.utils.encode_decode(dir, lhs_ad.coder, cx2, term)))))))))
    match t:
        case hydra.core.TypeApplication(value=at):
            return for_application_type(at)
        
        case _:
            raise TypeError("Unsupported Type")

def pass_either(cx: hydra.coders.AdapterContext, t: hydra.core.Type) -> Either[str, hydra.compute.Adapter[hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term]]:
    r"""Pass through either types."""
    
    def for_either_type(et: hydra.core.EitherType) -> Either[str, hydra.compute.Adapter[hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term]]:
        left = et.left
        right = et.right
        return hydra.lib.eithers.bind(term_adapter(cx, left), (lambda left_ad: hydra.lib.eithers.bind(term_adapter(cx, right), (lambda right_ad: Right(hydra.compute.Adapter(hydra.lib.logic.or_(left_ad.is_lossy, right_ad.is_lossy), t, cast(hydra.core.Type, hydra.core.TypeEither(hydra.core.EitherType(left_ad.target, right_ad.target))), hydra.adapt.utils.bidirectional((lambda dir, cx2, term: hydra.adapt.utils.encode_decode(dir, left_ad.coder, cx2, term)))))))))
    match t:
        case hydra.core.TypeEither(value=et):
            return for_either_type(et)
        
        case _:
            raise TypeError("Unsupported Type")

def pass_forall(cx: hydra.coders.AdapterContext, t: hydra.core.Type) -> Either[str, hydra.compute.Adapter[hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term]]:
    r"""Pass through forall types."""
    
    def for_forall_type(ft: hydra.core.ForallType) -> Either[str, hydra.compute.Adapter[hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term]]:
        v = ft.parameter
        body = ft.body
        return hydra.lib.eithers.bind(term_adapter(cx, body), (lambda ad: Right(hydra.compute.Adapter(ad.is_lossy, t, cast(hydra.core.Type, hydra.core.TypeForall(hydra.core.ForallType(v, ad.target))), hydra.adapt.utils.bidirectional((lambda dir, cx2, term: hydra.adapt.utils.encode_decode(dir, ad.coder, cx2, term)))))))
    match t:
        case hydra.core.TypeForall(value=ft):
            return for_forall_type(ft)
        
        case _:
            raise TypeError("Unsupported Type")

def pass_function(cx: hydra.coders.AdapterContext, t: hydra.core.Type) -> Either[str, hydra.compute.Adapter[hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term]]:
    r"""Pass through function types with adaptation."""
    
    def to_case_ads(dom: hydra.core.Type, cod: hydra.core.Type) -> Either[str, FrozenDict[hydra.core.Name, hydra.compute.Adapter[hydra.core.FieldType, hydra.core.FieldType, hydra.core.Field, hydra.core.Field]]]:
        match hydra.rewriting.deannotate_type(dom):
            case hydra.core.TypeUnion(value=rt):
                return hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda f: hydra.lib.eithers.bind(field_adapter(cx, hydra.core.FieldType(f.name, cast(hydra.core.Type, hydra.core.TypeFunction(hydra.core.FunctionType(f.type, cod))))), (lambda ad: Right((f.name, ad))))), rt.fields), (lambda pairs: Right(hydra.lib.maps.from_list(pairs))))
            
            case _:
                return Right(hydra.lib.maps.empty())
    def to_option_ad(dom: hydra.core.Type, cod: hydra.core.Type) -> Either[str, Maybe[hydra.compute.Adapter[hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term]]]:
        match hydra.rewriting.deannotate_type(dom):
            case hydra.core.TypeMaybe(value=ot):
                return hydra.lib.eithers.map((lambda x1: hydra.lib.maybes.pure(x1)), term_adapter(cx, cast(hydra.core.Type, hydra.core.TypeFunction(hydra.core.FunctionType(ot, cod)))))
            
            case _:
                return Right(Nothing())
    def get_coder(case_ads: FrozenDict[T0, hydra.compute.Adapter[T1, T2, T3, T3]], fname: T0) -> hydra.compute.Coder[T3, T3]:
        return hydra.lib.maybes.maybe((lambda : hydra.adapt.utils.id_coder()), (lambda v1: v1.coder), hydra.lib.maps.lookup(fname, case_ads))
    def for_elimination(dir: hydra.coders.CoderDirection, cx2: hydra.context.Context, cod_ad: hydra.compute.Adapter[T0, T1, hydra.core.Term, hydra.core.Term], case_ads: FrozenDict[hydra.core.Name, hydra.compute.Adapter[T2, T3, hydra.core.Field, hydra.core.Field]], e: hydra.core.Elimination) -> Either[hydra.context.InContext[hydra.error.OtherError], hydra.core.Elimination]:
        match e:
            case hydra.core.EliminationUnion(value=cs):
                n = cs.type_name
                def_ = cs.default
                cases = cs.cases
                return hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda f: hydra.adapt.utils.encode_decode(dir, get_coder(case_ads, f.name), cx2, f)), cases), (lambda rcases: hydra.lib.eithers.bind(hydra.lib.eithers.map_maybe((lambda d: hydra.adapt.utils.encode_decode(dir, cod_ad.coder, cx2, d)), def_), (lambda rdef: Right(cast(hydra.core.Elimination, hydra.core.EliminationUnion(hydra.core.CaseStatement(n, rdef, rcases))))))))
            
            case _:
                raise TypeError("Unsupported Elimination")
    def for_function(dir: hydra.coders.CoderDirection, cx2: hydra.context.Context, cod_ad: hydra.compute.Adapter[T0, T1, hydra.core.Term, hydra.core.Term], case_ads: FrozenDict[hydra.core.Name, hydra.compute.Adapter[T2, T3, hydra.core.Field, hydra.core.Field]], f: hydra.core.Function) -> Either[hydra.context.InContext[hydra.error.OtherError], hydra.core.Function]:
        match f:
            case hydra.core.FunctionElimination(value=e):
                return hydra.lib.eithers.map((lambda x: cast(hydra.core.Function, hydra.core.FunctionElimination(x))), for_elimination(dir, cx2, cod_ad, case_ads, e))
            
            case hydra.core.FunctionLambda(value=l):
                var = l.parameter
                d = l.domain
                body = l.body
                return hydra.lib.eithers.bind(hydra.adapt.utils.encode_decode(dir, cod_ad.coder, cx2, body), (lambda new_body: Right(cast(hydra.core.Function, hydra.core.FunctionLambda(hydra.core.Lambda(var, d, new_body))))))
            
            case hydra.core.FunctionPrimitive(value=name):
                return Right(cast(hydra.core.Function, hydra.core.FunctionPrimitive(name)))
            
            case _:
                raise AssertionError("Unreachable: all variants handled")
    def encdec(cod_ad: hydra.compute.Adapter[T0, T1, hydra.core.Term, hydra.core.Term], case_ads: FrozenDict[hydra.core.Name, hydra.compute.Adapter[T2, T3, hydra.core.Field, hydra.core.Field]], dir: hydra.coders.CoderDirection, cx2: hydra.context.Context, term: hydra.core.Term) -> Either[hydra.context.InContext[hydra.error.OtherError], hydra.core.Term]:
        match hydra.rewriting.deannotate_term(term):
            case hydra.core.TermFunction(value=f):
                return hydra.lib.eithers.map((lambda x: cast(hydra.core.Term, hydra.core.TermFunction(x))), for_function(dir, cx2, cod_ad, case_ads, f))
            
            case _:
                return Right(term)
    def for_function_type(ft: hydra.core.FunctionType) -> Either[str, hydra.compute.Adapter[hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term]]:
        dom = ft.domain
        cod = ft.codomain
        return hydra.lib.eithers.bind(term_adapter(cx, dom), (lambda dom_ad: hydra.lib.eithers.bind(term_adapter(cx, cod), (lambda cod_ad: hydra.lib.eithers.bind(to_case_ads(dom, cod), (lambda case_ads: hydra.lib.eithers.bind(to_option_ad(dom, cod), (lambda option_ad: (lossy := hydra.lib.logic.or_(cod_ad.is_lossy, hydra.lib.lists.foldl(hydra.lib.logic.or_, False, hydra.lib.lists.map((lambda pair: hydra.lib.pairs.second(pair).is_lossy), hydra.lib.maps.to_list(case_ads)))), target := cast(hydra.core.Type, hydra.core.TypeFunction(hydra.core.FunctionType(dom_ad.target, cod_ad.target))), Right(hydra.compute.Adapter(lossy, t, target, hydra.adapt.utils.bidirectional((lambda v1, v2, v3: encdec(cod_ad, case_ads, v1, v2, v3))))))[2]))))))))
    match t:
        case hydra.core.TypeFunction(value=ft):
            return for_function_type(ft)
        
        case _:
            raise TypeError("Unsupported Type")

def pass_list(cx: hydra.coders.AdapterContext, t: hydra.core.Type) -> Either[str, hydra.compute.Adapter[hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term]]:
    r"""Pass through list types."""
    
    def encdec(ad: hydra.compute.Adapter[T0, T1, hydra.core.Term, hydra.core.Term], dir: hydra.coders.CoderDirection, cx2: hydra.context.Context, term: hydra.core.Term) -> Either[hydra.context.InContext[hydra.error.OtherError], hydra.core.Term]:
        match term:
            case hydra.core.TermList(value=terms):
                return hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda v1: hydra.adapt.utils.encode_decode(dir, ad.coder, cx2, v1)), terms), (lambda new_terms: Right(cast(hydra.core.Term, hydra.core.TermList(new_terms)))))
            
            case _:
                raise TypeError("Unsupported Term")
    def for_list_type(lt: hydra.core.Type) -> Either[str, hydra.compute.Adapter[hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term]]:
        return hydra.lib.eithers.bind(term_adapter(cx, lt), (lambda ad: Right(hydra.compute.Adapter(ad.is_lossy, t, cast(hydra.core.Type, hydra.core.TypeList(ad.target)), hydra.adapt.utils.bidirectional((lambda v1, v2, v3: encdec(ad, v1, v2, v3)))))))
    match t:
        case hydra.core.TypeList(value=lt):
            return for_list_type(lt)
        
        case _:
            raise TypeError("Unsupported Type")

def pass_map(cx: hydra.coders.AdapterContext, t: hydra.core.Type) -> Either[str, hydra.compute.Adapter[hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term]]:
    r"""Pass through map types."""
    
    def encdec(kad: hydra.compute.Adapter[T0, T1, hydra.core.Term, hydra.core.Term], vad: hydra.compute.Adapter[T2, T3, hydra.core.Term, hydra.core.Term], dir: hydra.coders.CoderDirection, cx2: hydra.context.Context, term: hydra.core.Term) -> Either[hydra.context.InContext[hydra.error.OtherError], hydra.core.Term]:
        match term:
            case hydra.core.TermMap(value=m):
                return hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda pair: (k := hydra.lib.pairs.first(pair), v := hydra.lib.pairs.second(pair), hydra.lib.eithers.bind(hydra.adapt.utils.encode_decode(dir, kad.coder, cx2, k), (lambda new_k: hydra.lib.eithers.bind(hydra.adapt.utils.encode_decode(dir, vad.coder, cx2, v), (lambda new_v: Right((new_k, new_v)))))))[2]), hydra.lib.maps.to_list(m)), (lambda new_pairs: Right(cast(hydra.core.Term, hydra.core.TermMap(hydra.lib.maps.from_list(new_pairs))))))
            
            case _:
                raise TypeError("Unsupported Term")
    def for_map_type(mt: hydra.core.MapType) -> Either[str, hydra.compute.Adapter[hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term]]:
        kt = mt.keys
        vt = mt.values
        return hydra.lib.eithers.bind(term_adapter(cx, kt), (lambda kad: hydra.lib.eithers.bind(term_adapter(cx, vt), (lambda vad: Right(hydra.compute.Adapter(hydra.lib.logic.or_(kad.is_lossy, vad.is_lossy), t, cast(hydra.core.Type, hydra.core.TypeMap(hydra.core.MapType(kad.target, vad.target))), hydra.adapt.utils.bidirectional((lambda v1, v2, v3: encdec(kad, vad, v1, v2, v3)))))))))
    match t:
        case hydra.core.TypeMap(value=mt):
            return for_map_type(mt)
        
        case _:
            raise TypeError("Unsupported Type")

def pass_optional(cx: hydra.coders.AdapterContext, t: hydra.core.Type) -> Either[str, hydra.compute.Adapter[hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term]]:
    r"""Pass through optional types."""
    
    def map_term(graph: hydra.graph.Graph, coder: hydra.compute.Coder[hydra.core.Term, hydra.core.Term], dir: hydra.coders.CoderDirection, cx2: hydra.context.Context, term: hydra.core.Term) -> Either[hydra.context.InContext[hydra.error.OtherError], hydra.core.Term]:
        return hydra.lib.eithers.bind(hydra.extract.core.maybe_term(cx2, (lambda x: Right(x)), graph, term), (lambda opt: hydra.lib.eithers.bind(hydra.lib.eithers.map_maybe((lambda v1: hydra.adapt.utils.encode_decode(dir, coder, cx2, v1)), opt), (lambda new_opt: Right(cast(hydra.core.Term, hydra.core.TermMaybe(new_opt)))))))
    match t:
        case hydra.core.TypeMaybe(value=ot):
            return hydra.lib.eithers.bind(term_adapter(cx, ot), (lambda adapter: Right(hydra.compute.Adapter(adapter.is_lossy, t, cast(hydra.core.Type, hydra.core.TypeMaybe(adapter.target)), hydra.adapt.utils.bidirectional((lambda v1, v2, v3: map_term(cx.graph, adapter.coder, v1, v2, v3)))))))
        
        case _:
            raise TypeError("Unsupported Type")

def pass_record(cx: hydra.coders.AdapterContext, t: hydra.core.Type) -> Either[str, hydra.compute.Adapter[hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term]]:
    r"""Pass through record types."""
    
    def encdec(rt: hydra.core.RowType, adapters: frozenlist[hydra.compute.Adapter[T0, T1, hydra.core.Field, hydra.core.Field]], dir: hydra.coders.CoderDirection, cx2: hydra.context.Context, term: hydra.core.Term) -> Either[hydra.context.InContext[hydra.error.OtherError], hydra.core.Term]:
        match term:
            case hydra.core.TermRecord(value=rec):
                dfields = rec.fields
                return hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda p: hydra.adapt.utils.encode_decode(dir, hydra.lib.pairs.first(p).coder, cx2, hydra.lib.pairs.second(p))), hydra.lib.lists.zip(adapters, dfields)), (lambda new_fields: Right(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(rt.type_name, new_fields))))))
            
            case _:
                raise TypeError("Unsupported Term")
    def for_record_type(rt: hydra.core.RowType) -> Either[str, hydra.compute.Adapter[hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term]]:
        return hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda v1: field_adapter(cx, v1)), rt.fields), (lambda adapters: (lossy := hydra.lib.lists.foldl(hydra.lib.logic.or_, False, hydra.lib.lists.map((lambda v1: v1.is_lossy), adapters)), sfields_ := hydra.lib.lists.map((lambda v1: v1.target), adapters), Right(hydra.compute.Adapter(lossy, t, cast(hydra.core.Type, hydra.core.TypeRecord(hydra.core.RowType(rt.type_name, sfields_))), hydra.adapt.utils.bidirectional((lambda v1, v2, v3: encdec(rt, adapters, v1, v2, v3))))))[2]))
    match t:
        case hydra.core.TypeRecord(value=rt):
            return for_record_type(rt)
        
        case _:
            raise TypeError("Unsupported Type")

def pass_set(cx: hydra.coders.AdapterContext, t: hydra.core.Type) -> Either[str, hydra.compute.Adapter[hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term]]:
    r"""Pass through set types."""
    
    def encdec(ad: hydra.compute.Adapter[T0, T1, hydra.core.Term, hydra.core.Term], dir: hydra.coders.CoderDirection, cx2: hydra.context.Context, term: hydra.core.Term) -> Either[hydra.context.InContext[hydra.error.OtherError], hydra.core.Term]:
        match term:
            case hydra.core.TermSet(value=terms):
                return hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda v1: hydra.adapt.utils.encode_decode(dir, ad.coder, cx2, v1)), hydra.lib.sets.to_list(terms)), (lambda new_terms: Right(cast(hydra.core.Term, hydra.core.TermSet(hydra.lib.sets.from_list(new_terms))))))
            
            case _:
                raise TypeError("Unsupported Term")
    match t:
        case hydra.core.TypeSet(value=st):
            return hydra.lib.eithers.bind(term_adapter(cx, st), (lambda ad: Right(hydra.compute.Adapter(ad.is_lossy, t, cast(hydra.core.Type, hydra.core.TypeSet(ad.target)), hydra.adapt.utils.bidirectional((lambda v1, v2, v3: encdec(ad, v1, v2, v3)))))))
        
        case _:
            raise TypeError("Unsupported Type")

def pass_union(cx: hydra.coders.AdapterContext, t: hydra.core.Type) -> Either[str, hydra.compute.Adapter[hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term]]:
    r"""Pass through union types."""
    
    match t:
        case hydra.core.TypeUnion(value=rt):
            sfields = rt.fields
            tname = rt.type_name
            return hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda f: hydra.lib.eithers.bind(field_adapter(cx, f), (lambda ad: Right((f.name, ad))))), sfields), (lambda adapters: (adapters_map := hydra.lib.maps.from_list(adapters), lossy := hydra.lib.lists.foldl(hydra.lib.logic.or_, False, hydra.lib.lists.map((lambda pair: hydra.lib.pairs.second(pair).is_lossy), adapters)), sfields_ := hydra.lib.lists.map((lambda pair: hydra.lib.pairs.second(pair).target), adapters), Right(hydra.compute.Adapter(lossy, t, cast(hydra.core.Type, hydra.core.TypeUnion(hydra.core.RowType(tname, sfields_))), hydra.adapt.utils.bidirectional((lambda dir, _cx, term: Right(term))))))[3]))
        
        case _:
            raise TypeError("Unsupported Type")

def pass_wrapped(cx: hydra.coders.AdapterContext, t: hydra.core.Type) -> Either[str, hydra.compute.Adapter[hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term]]:
    r"""Pass through wrapped types."""
    
    match t:
        case hydra.core.TypeWrap(value=wt):
            tname = wt.type_name
            ot = wt.body
            def map_term(graph: hydra.graph.Graph, coder: hydra.compute.Coder[hydra.core.Term, hydra.core.Term], dir: hydra.coders.CoderDirection, cx2: hydra.context.Context, term: hydra.core.Term) -> Either[hydra.context.InContext[hydra.error.OtherError], hydra.core.Term]:
                return hydra.lib.eithers.bind(hydra.extract.core.wrap(cx2, tname, graph, term), (lambda unwrapped: hydra.lib.eithers.bind(hydra.adapt.utils.encode_decode(dir, coder, cx2, unwrapped), (lambda new_term: Right(cast(hydra.core.Term, hydra.core.TermWrap(hydra.core.WrappedTerm(tname, new_term))))))))
            return hydra.lib.eithers.bind(term_adapter(cx, ot), (lambda adapter: Right(hydra.compute.Adapter(adapter.is_lossy, t, cast(hydra.core.Type, hydra.core.TypeWrap(hydra.core.WrappedType(tname, adapter.target))), hydra.adapt.utils.bidirectional((lambda v1, v2, v3: map_term(cx.graph, adapter.coder, v1, v2, v3)))))))
        
        case _:
            raise TypeError("Unsupported Type")

def set_to_list(cx: hydra.coders.AdapterContext, t: hydra.core.Type) -> Either[str, hydra.compute.Adapter[hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term]]:
    r"""Convert set types to list types."""
    
    def encode(ad: hydra.compute.Adapter[T0, T1, hydra.core.Term, hydra.core.Term], cx2: hydra.context.Context, term: hydra.core.Term) -> Either[hydra.context.InContext[hydra.error.OtherError], hydra.core.Term]:
        match term:
            case hydra.core.TermSet(value=s):
                return hydra.adapt.utils.encode_decode(hydra.coders.CoderDirection.ENCODE, ad.coder, cx2, cast(hydra.core.Term, hydra.core.TermList(hydra.lib.sets.to_list(s))))
            
            case _:
                raise TypeError("Unsupported Term")
    def for_list_term(t2: hydra.core.Term) -> Either[T0, hydra.core.Term]:
        match t2:
            case hydra.core.TermList(value=l):
                return Right(cast(hydra.core.Term, hydra.core.TermSet(hydra.lib.sets.from_list(l))))
            
            case _:
                raise TypeError("Unsupported Term")
    def decode(ad: hydra.compute.Adapter[T0, T1, hydra.core.Term, hydra.core.Term], cx2: hydra.context.Context, term: hydra.core.Term) -> Either[hydra.context.InContext[hydra.error.OtherError], hydra.core.Term]:
        return hydra.lib.eithers.bind(hydra.adapt.utils.encode_decode(hydra.coders.CoderDirection.DECODE, ad.coder, cx2, term), (lambda list_term: for_list_term(list_term)))
    def for_set_type(st: hydra.core.Type) -> Either[str, hydra.compute.Adapter[hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term]]:
        return hydra.lib.eithers.bind(term_adapter(cx, cast(hydra.core.Type, hydra.core.TypeList(st))), (lambda ad: Right(hydra.compute.Adapter(ad.is_lossy, t, ad.target, hydra.compute.Coder((lambda v1, v2: encode(ad, v1, v2)), (lambda v1, v2: decode(ad, v1, v2)))))))
    match t:
        case hydra.core.TypeSet(value=st):
            return for_set_type(st)
        
        case _:
            raise TypeError("Unsupported Type")

def simplify_application(cx: hydra.coders.AdapterContext, t: hydra.core.Type) -> Either[str, hydra.compute.Adapter[hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term]]:
    r"""Simplify application types."""
    
    def encdec(ad: hydra.compute.Adapter[T0, T1, T2, T2], dir: hydra.coders.CoderDirection, cx2: hydra.context.Context, term: T2) -> Either[hydra.context.InContext[hydra.error.OtherError], T2]:
        return hydra.adapt.utils.encode_decode(dir, ad.coder, cx2, term)
    def for_application_type(at: hydra.core.ApplicationType) -> Either[str, hydra.compute.Adapter[hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term]]:
        lhs = at.function
        return hydra.lib.eithers.bind(term_adapter(cx, lhs), (lambda ad: Right(hydra.compute.Adapter(False, t, ad.target, hydra.adapt.utils.bidirectional((lambda v1, v2, v3: encdec(ad, v1, v2, v3)))))))
    match t:
        case hydra.core.TypeApplication(value=at):
            return for_application_type(at)
        
        case _:
            raise TypeError("Unsupported Type")

def term_adapter(cx: hydra.coders.AdapterContext, typ: hydra.core.Type) -> Either[str, hydra.compute.Adapter[hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term]]:
    r"""Create an adapter for any type."""
    
    def constraints(cx2: hydra.coders.AdapterContext) -> hydra.coders.LanguageConstraints:
        return cx2.language.constraints
    def supported(cx2: hydra.coders.AdapterContext, v1: hydra.core.Type) -> bool:
        return hydra.adapt.utils.type_is_supported(constraints(cx2), v1)
    def variant_is_supported(cx2: hydra.coders.AdapterContext, t: hydra.core.Type) -> bool:
        return hydra.lib.sets.member(hydra.reflect.type_variant(t), constraints(cx2).type_variants)
    def supported_at_top_level(cx2: hydra.coders.AdapterContext, t: hydra.core.Type) -> bool:
        return hydra.lib.logic.and_(variant_is_supported(cx2, t), constraints(cx2).types(t))
    def pass_(t: hydra.core.Type) -> frozenlist[Callable[[hydra.coders.AdapterContext, hydra.core.Type], Either[str, hydra.compute.Adapter[hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term]]]]:
        match hydra.reflect.type_variant(hydra.rewriting.deannotate_type(t)):
            case hydra.variants.TypeVariant.ANNOTATED:
                return ()
            
            case hydra.variants.TypeVariant.APPLICATION:
                return ((lambda x1, x2: pass_application(x1, x2)),)
            
            case hydra.variants.TypeVariant.EITHER:
                return ((lambda x1, x2: pass_either(x1, x2)),)
            
            case hydra.variants.TypeVariant.FORALL:
                return ((lambda x1, x2: pass_forall(x1, x2)),)
            
            case hydra.variants.TypeVariant.FUNCTION:
                return ((lambda x1, x2: pass_function(x1, x2)),)
            
            case hydra.variants.TypeVariant.LIST:
                return ((lambda x1, x2: pass_list(x1, x2)),)
            
            case hydra.variants.TypeVariant.LITERAL:
                return ((lambda x1, x2: pass_literal(x1, x2)),)
            
            case hydra.variants.TypeVariant.MAP:
                return ((lambda x1, x2: pass_map(x1, x2)),)
            
            case hydra.variants.TypeVariant.MAYBE:
                return ((lambda x1, x2: pass_optional(x1, x2)), (lambda x1, x2: maybe_to_list(x1, x2)))
            
            case hydra.variants.TypeVariant.PAIR:
                return ()
            
            case hydra.variants.TypeVariant.RECORD:
                return ((lambda x1, x2: pass_record(x1, x2)),)
            
            case hydra.variants.TypeVariant.SET:
                return ((lambda x1, x2: pass_set(x1, x2)),)
            
            case hydra.variants.TypeVariant.UNION:
                return ((lambda x1, x2: pass_union(x1, x2)),)
            
            case hydra.variants.TypeVariant.UNIT:
                return ((lambda x1, x2: pass_unit(x1, x2)),)
            
            case hydra.variants.TypeVariant.VARIABLE:
                return ()
            
            case hydra.variants.TypeVariant.WRAP:
                return ((lambda x1, x2: pass_wrapped(x1, x2)),)
            
            case _:
                raise AssertionError("Unreachable: all variants handled")
    def try_substitution(t: hydra.core.Type) -> frozenlist[Callable[[hydra.coders.AdapterContext, hydra.core.Type], Either[str, hydra.compute.Adapter[hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term]]]]:
        match hydra.reflect.type_variant(t):
            case hydra.variants.TypeVariant.ANNOTATED:
                return ()
            
            case hydra.variants.TypeVariant.APPLICATION:
                return ((lambda x1, x2: simplify_application(x1, x2)),)
            
            case hydra.variants.TypeVariant.EITHER:
                return ()
            
            case hydra.variants.TypeVariant.FORALL:
                return ((lambda x1, x2: lambda_to_monotype(x1, x2)),)
            
            case hydra.variants.TypeVariant.FUNCTION:
                return ((lambda x1, x2: function_to_union(x1, x2)),)
            
            case hydra.variants.TypeVariant.LIST:
                return ()
            
            case hydra.variants.TypeVariant.LITERAL:
                return ()
            
            case hydra.variants.TypeVariant.MAP:
                return ()
            
            case hydra.variants.TypeVariant.MAYBE:
                return ((lambda x1, x2: maybe_to_list(x1, x2)),)
            
            case hydra.variants.TypeVariant.PAIR:
                return ()
            
            case hydra.variants.TypeVariant.RECORD:
                return ()
            
            case hydra.variants.TypeVariant.SET:
                return ((lambda x1, x2: set_to_list(x1, x2)),)
            
            case hydra.variants.TypeVariant.UNION:
                return ((lambda x1, x2: union_to_record(x1, x2)),)
            
            case hydra.variants.TypeVariant.UNIT:
                return ((lambda x1, x2: unit_to_record(x1, x2)),)
            
            case hydra.variants.TypeVariant.VARIABLE:
                return ()
            
            case hydra.variants.TypeVariant.WRAP:
                return ((lambda x1, x2: wrap_to_unwrapped(x1, x2)),)
            
            case _:
                raise AssertionError("Unreachable: all variants handled")
    def alts(cx2: hydra.coders.AdapterContext, t: hydra.core.Type) -> Either[str, frozenlist[hydra.compute.Adapter[hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term]]]:
        return hydra.lib.eithers.map_list((lambda c: c(cx2, t)), hydra.lib.logic.if_else(supported_at_top_level(cx2, t), (lambda : pass_(t)), (lambda : try_substitution(t))))
    @lru_cache(1)
    def dflt() -> Either[str, hydra.compute.Adapter[hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term]]:
        match typ:
            case hydra.core.TypeVariable(value=name):
                return for_type_reference(cx, name)
            
            case _:
                return hydra.adapt.utils.choose_adapter((lambda v1: alts(cx, v1)), (lambda v1: supported(cx, v1)), (lambda x1: hydra.show.core.type(x1)), (lambda x1: hydra.show.core.type(x1)), typ)
    match typ:
        case hydra.core.TypeAnnotated(value=at):
            return hydra.lib.eithers.bind(term_adapter(cx, at.body), (lambda ad: Right(hydra.compute.Adapter(ad.is_lossy, ad.source, cast(hydra.core.Type, hydra.core.TypeAnnotated(hydra.core.AnnotatedType(ad.target, at.annotation))), ad.coder))))
        
        case _:
            return dflt()

def union_to_record(cx: hydra.coders.AdapterContext, t: hydra.core.Type) -> Either[str, hydra.compute.Adapter[hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term]]:
    r"""Convert union types to record types."""
    
    def for_field(field: hydra.core.Field) -> Maybe[hydra.core.Field]:
        fn = field.name
        fterm = field.term
        match fterm:
            case hydra.core.TermMaybe(value=opt):
                return hydra.lib.maybes.bind(opt, (lambda t2: Just(hydra.core.Field(fn, t2))))
            
            case _:
                raise TypeError("Unsupported Term")
    def from_record_fields(cx2: hydra.context.Context, term: hydra.core.Term, term_: T0, t_: hydra.core.Type, fields: frozenlist[hydra.core.Field]) -> Either[hydra.context.InContext[hydra.error.OtherError], hydra.core.Field]:
        @lru_cache(1)
        def matches() -> frozenlist[hydra.core.Field]:
            return hydra.lib.maybes.map_maybe((lambda x1: for_field(x1)), fields)
        return hydra.lib.logic.if_else(hydra.lib.lists.null(matches()), (lambda : Left(hydra.context.InContext(hydra.error.OtherError(hydra.lib.strings.cat(("cannot convert term back to union: ", hydra.show.core.term(term), " where type = ", hydra.show.core.type(t), "    and target type = ", hydra.show.core.type(t_)))), cx2))), (lambda : Right(hydra.lib.lists.head(matches()))))
    def for_rec_term(cx2: hydra.context.Context, nm: hydra.core.Name, ad: hydra.compute.Adapter[T0, hydra.core.Type, T1, T2], term: hydra.core.Term, rec_term: hydra.core.Term) -> Either[hydra.context.InContext[hydra.error.OtherError], hydra.core.Term]:
        match rec_term:
            case hydra.core.TermRecord(value=rec):
                fields = rec.fields
                return hydra.lib.eithers.bind(from_record_fields(cx2, term, cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(nm, fields))), ad.target, fields), (lambda result_field: Right(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(nm, result_field))))))
            
            case _:
                raise TypeError("Unsupported Term")
    match t:
        case hydra.core.TypeUnion(value=rt):
            nm = rt.type_name
            sfields = rt.fields
            @lru_cache(1)
            def target() -> hydra.core.Type:
                return cast(hydra.core.Type, hydra.core.TypeRecord(hydra.coder_utils.union_type_to_record_type(rt)))
            def to_record_field(term: hydra.core.Term, fn: hydra.core.Name, f: hydra.core.FieldType) -> hydra.core.Field:
                fn_ = f.name
                return hydra.core.Field(fn_, cast(hydra.core.Term, hydra.core.TermMaybe(hydra.lib.logic.if_else(hydra.lib.equality.equal(fn_, fn), (lambda : Just(term)), (lambda : Nothing())))))
            return hydra.lib.eithers.bind(term_adapter(cx, target()), (lambda ad: (graph := cx.graph, Right(hydra.compute.Adapter(ad.is_lossy, t, ad.target, hydra.compute.Coder((lambda cx2, term_: hydra.lib.eithers.bind(hydra.extract.core.injection(cx2, rt.type_name, graph, term_), (lambda field: (fn := field.name, term := field.term, ad.coder.encode(cx2, cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(nm, hydra.lib.lists.map((lambda v1: to_record_field(term, fn, v1)), sfields))))))[2]))), (lambda cx2, term: hydra.lib.eithers.bind(ad.coder.decode(cx2, term), (lambda rec_term: for_rec_term(cx2, nm, ad, term, rec_term))))))))[1]))
        
        case _:
            raise TypeError("Unsupported Type")

def wrap_to_unwrapped(cx: hydra.coders.AdapterContext, t: hydra.core.Type) -> Either[str, hydra.compute.Adapter[hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term]]:
    r"""Convert wrapped types to unwrapped types."""
    
    match t:
        case hydra.core.TypeWrap(value=wt):
            tname = wt.type_name
            typ = wt.body
            def encode(graph: hydra.graph.Graph, ad: hydra.compute.Adapter[T0, T1, hydra.core.Term, T2], cx2: hydra.context.Context, term: hydra.core.Term) -> Either[hydra.context.InContext[hydra.error.OtherError], T2]:
                return hydra.lib.eithers.bind(hydra.extract.core.wrap(cx2, tname, graph, term), (lambda unwrapped: ad.coder.encode(cx2, unwrapped)))
            def decode(ad: hydra.compute.Adapter[T0, T1, hydra.core.Term, T2], cx2: hydra.context.Context, term: T2) -> Either[hydra.context.InContext[hydra.error.OtherError], hydra.core.Term]:
                return hydra.lib.eithers.bind(ad.coder.decode(cx2, term), (lambda decoded: Right(cast(hydra.core.Term, hydra.core.TermWrap(hydra.core.WrappedTerm(tname, decoded))))))
            return hydra.lib.eithers.bind(term_adapter(cx, typ), (lambda ad: Right(hydra.compute.Adapter(False, t, ad.target, hydra.compute.Coder((lambda v1, v2: encode(cx.graph, ad, v1, v2)), (lambda v1, v2: decode(ad, v1, v2)))))))
        
        case _:
            raise TypeError("Unsupported Type")

def function_proxy_type(_: T0) -> hydra.core.Type:
    r"""Generate a function proxy type for a given domain type."""
    
    return cast(hydra.core.Type, hydra.core.TypeUnion(hydra.core.RowType(function_proxy_name, (hydra.core.FieldType(hydra.core.Name("wrap"), cast(hydra.core.Type, hydra.core.TypeLiteral(cast(hydra.core.LiteralType, hydra.core.LiteralTypeString())))), hydra.core.FieldType(hydra.core.Name("record"), cast(hydra.core.Type, hydra.core.TypeLiteral(cast(hydra.core.LiteralType, hydra.core.LiteralTypeString())))), hydra.core.FieldType(hydra.core.Name("union"), cast(hydra.core.Type, hydra.core.TypeLiteral(cast(hydra.core.LiteralType, hydra.core.LiteralTypeString())))), hydra.core.FieldType(hydra.core.Name("lambda"), cast(hydra.core.Type, hydra.core.TypeLiteral(cast(hydra.core.LiteralType, hydra.core.LiteralTypeString())))), hydra.core.FieldType(hydra.core.Name("primitive"), cast(hydra.core.Type, hydra.core.TypeLiteral(cast(hydra.core.LiteralType, hydra.core.LiteralTypeString())))), hydra.core.FieldType(hydra.core.Name("variable"), cast(hydra.core.Type, hydra.core.TypeLiteral(cast(hydra.core.LiteralType, hydra.core.LiteralTypeString()))))))))
