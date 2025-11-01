# Note: this is an automatically generated file. Do not edit.

r"""Additional adapter utilities, above and beyond the generated ones."""

from __future__ import annotations
from collections.abc import Callable
from hydra.dsl.python import frozenlist
from typing import cast
import hydra.coders
import hydra.compute
import hydra.core
import hydra.formatting
import hydra.lib.flows
import hydra.lib.lists
import hydra.lib.literals
import hydra.lib.logic
import hydra.lib.optionals
import hydra.lib.sets
import hydra.lib.strings
import hydra.mantle
import hydra.module
import hydra.names
import hydra.rewriting
import hydra.show.core
import hydra.variants

def bidirectional[T0, T1](f: Callable[[hydra.coders.CoderDirection, T0], hydra.compute.Flow[T1, T0]]) -> hydra.compute.Coder[T1, T1, T0, T0]:
    return cast(hydra.compute.Coder[T1, T1, T0, T0], hydra.compute.Coder((lambda v1: f(hydra.coders.CoderDirection.ENCODE, v1)), (lambda v1: f(hydra.coders.CoderDirection.DECODE, v1))))

def id_coder[T0, T1, T2]() -> hydra.compute.Coder[T0, T1, T2, T2]:
    return cast(hydra.compute.Coder[T0, T1, T2, T2], hydra.compute.Coder(cast(Callable[[T2], hydra.compute.Flow[T0, T2]], hydra.lib.flows.pure), cast(Callable[[T2], hydra.compute.Flow[T1, T2]], hydra.lib.flows.pure)))

def choose_adapter[T0, T1, T2, T3, T4](alts: Callable[[T0], hydra.compute.Flow[T1, frozenlist[hydra.compute.Adapter[T2, T3, T0, T0, T4, T4]]]], supported: Callable[[T0], bool], show: Callable[[T0], str], describe: Callable[[T0], str], typ: T0) -> hydra.compute.Flow[T1, hydra.compute.Adapter[T2, T3, T0, T0, T4, T4]]:
    return hydra.lib.logic.if_else(supported(typ), hydra.lib.flows.pure(cast(hydra.compute.Adapter[T2, T3, T0, T0, T4, T4], hydra.compute.Adapter(False, typ, typ, cast(hydra.compute.Coder[T2, T3, T4, T4], id_coder)))), hydra.lib.flows.bind(alts(typ), (lambda raw: (candidates := hydra.lib.lists.filter((lambda adapter: supported(adapter.target)), raw), hydra.lib.logic.if_else(hydra.lib.lists.null(candidates), hydra.lib.flows.fail(hydra.lib.strings.cat(("no adapters found for ", describe(typ), hydra.lib.logic.if_else(hydra.lib.lists.null(raw), "", hydra.lib.strings.cat((" (discarded ", hydra.lib.literals.show_int32(hydra.lib.lists.length(raw)), " unsupported candidate types: ", hydra.show.core.list(show, hydra.lib.lists.map((lambda v1: v1.target), raw)), ")"))), ". Original type: ", show(typ)))), hydra.lib.flows.pure(hydra.lib.lists.head(candidates))))[1])))

def compose_coders[T0, T1, T2, T3, T4](c1: hydra.compute.Coder[T0, T1, T2, T3], c2: hydra.compute.Coder[T0, T1, T3, T4]) -> hydra.compute.Coder[T0, T1, T2, T4]:
    return cast(hydra.compute.Coder[T0, T1, T2, T4], hydra.compute.Coder((lambda a: hydra.lib.flows.bind(c1.encode(a), (lambda b1: c2.encode(b1)))), (lambda c: hydra.lib.flows.bind(c2.decode(c), (lambda b2: c1.decode(b2))))))

def encode_decode[T0, T1](dir: hydra.coders.CoderDirection, coder: hydra.compute.Coder[T0, T0, T1, T1]) -> Callable[[T1], hydra.compute.Flow[T0, T1]]:
    match dir:
        case hydra.coders.CoderDirection.ENCODE:
            return coder.encode
        
        case hydra.coders.CoderDirection.DECODE:
            return coder.decode

def float_type_is_supported(constraints: hydra.coders.LanguageConstraints, ft: hydra.core.FloatType) -> bool:
    r"""Check if float type is supported by language constraints."""
    
    return hydra.lib.sets.member(ft, constraints.float_types)

def id_adapter[T0, T1, T2, T3](t: T0) -> hydra.compute.Adapter[T1, T2, T0, T0, T3, T3]:
    return cast(hydra.compute.Adapter[T1, T2, T0, T0, T3, T3], hydra.compute.Adapter(False, t, t, cast(hydra.compute.Coder[T1, T2, T3, T3], id_coder)))

def integer_type_is_supported(constraints: hydra.coders.LanguageConstraints, it: hydra.core.IntegerType) -> bool:
    r"""Check if integer type is supported by language constraints."""
    
    return hydra.lib.sets.member(it, constraints.integer_types)

def literal_type_is_supported(constraints: hydra.coders.LanguageConstraints, lt: hydra.core.LiteralType) -> bool:
    r"""Check if literal type is supported by language constraints."""
    
    def is_supported(lt2: hydra.core.LiteralType) -> bool:
        match lt2:
            case hydra.core.LiteralTypeFloat(value=ft):
                return float_type_is_supported(constraints, ft)
            
            case hydra.core.LiteralTypeInteger(value=it):
                return integer_type_is_supported(constraints, it)
            
            case _:
                return True
    return hydra.lib.logic.and_(hydra.lib.sets.member(hydra.variants.literal_type_variant(lt), constraints.literal_variants), is_supported(lt))

def name_to_file_path(ns_conv: hydra.mantle.CaseConvention, local_conv: hydra.mantle.CaseConvention, ext: hydra.module.FileExtension, name: hydra.core.Name) -> str:
    r"""Convert a name to file path, given case conventions for namespaces and local names, and assuming '/' as the file path separator."""
    
    qual_name = hydra.names.qualify_name(name)
    ns = qual_name.namespace
    local = qual_name.local
    def ns_to_file_path(ns: hydra.module.Namespace) -> str:
        return hydra.lib.strings.intercalate("/", hydra.lib.lists.map((lambda part: hydra.formatting.convert_case(hydra.mantle.CaseConvention.CAMEL, ns_conv, part)), hydra.lib.strings.split_on(".", ns.value)))
    prefix = hydra.lib.optionals.maybe("", (lambda n: hydra.lib.strings.cat2(ns_to_file_path(n), "/")), ns)
    suffix = hydra.formatting.convert_case(hydra.mantle.CaseConvention.PASCAL, local_conv, local)
    return hydra.lib.strings.cat((prefix, suffix, ".", ext.value))

def type_is_supported(constraints: hydra.coders.LanguageConstraints, t: hydra.core.Type) -> bool:
    r"""Check if type is supported by language constraints."""
    
    base = hydra.rewriting.deannotate_type(t)
    def is_variable(v: hydra.mantle.TypeVariant) -> bool:
        match v:
            case hydra.mantle.TypeVariant.VARIABLE:
                return True
            
            case _:
                return False
    def is_supported_variant(v: hydra.mantle.TypeVariant) -> bool:
        return hydra.lib.logic.or_(is_variable(v), hydra.lib.sets.member(v, constraints.type_variants))
    def is_supported(base: hydra.core.Type) -> bool:
        match base:
            case hydra.core.TypeAnnotated(value=at):
                return type_is_supported(constraints, at.body)
            
            case hydra.core.TypeApplication(value=app):
                return hydra.lib.logic.and_(type_is_supported(constraints, app.function), type_is_supported(constraints, app.argument))
            
            case hydra.core.TypeForall(value=ft):
                return type_is_supported(constraints, ft.body)
            
            case hydra.core.TypeFunction(value=ft2):
                return hydra.lib.logic.and_(type_is_supported(constraints, ft2.domain), type_is_supported(constraints, ft2.codomain))
            
            case hydra.core.TypeList(value=lt):
                return type_is_supported(constraints, lt)
            
            case hydra.core.TypeLiteral(value=at2):
                return literal_type_is_supported(constraints, at2)
            
            case hydra.core.TypeMap(value=mt):
                return hydra.lib.logic.and_(type_is_supported(constraints, mt.keys), type_is_supported(constraints, mt.values))
            
            case hydra.core.TypeOptional(value=ot):
                return type_is_supported(constraints, ot)
            
            case hydra.core.TypeProduct(value=types):
                return hydra.lib.lists.foldl(hydra.lib.logic.and_, True, hydra.lib.lists.map((lambda v1: type_is_supported(constraints, v1)), types))
            
            case hydra.core.TypeRecord(value=rt):
                return hydra.lib.lists.foldl(hydra.lib.logic.and_, True, hydra.lib.lists.map((lambda field: type_is_supported(constraints, field.type)), rt.fields))
            
            case hydra.core.TypeSet(value=st):
                return type_is_supported(constraints, st)
            
            case hydra.core.TypeSum(value=types2):
                return hydra.lib.lists.foldl(hydra.lib.logic.and_, True, hydra.lib.lists.map((lambda v1: type_is_supported(constraints, v1)), types2))
            
            case hydra.core.TypeUnion(value=rt2):
                return hydra.lib.lists.foldl(hydra.lib.logic.and_, True, hydra.lib.lists.map((lambda field: type_is_supported(constraints, field.type)), rt2.fields))
            
            case hydra.core.TypeUnit():
                return True
            
            case hydra.core.TypeWrap(value=wt):
                return type_is_supported(constraints, wt.body)
            
            case hydra.core.TypeVariable():
                return True
    return hydra.lib.logic.and_(constraints.types(base), hydra.lib.logic.and_(is_supported_variant(hydra.variants.type_variant(base)), is_supported(base)))

def unidirectional_coder[T0, T1, T2, T3](m: Callable[[T0], hydra.compute.Flow[T1, T2]]) -> hydra.compute.Coder[T1, T3, T0, T2]:
    return cast(hydra.compute.Coder[T1, T3, T0, T2], hydra.compute.Coder(m, (lambda _: hydra.lib.flows.fail("inbound mapping is unsupported"))))
