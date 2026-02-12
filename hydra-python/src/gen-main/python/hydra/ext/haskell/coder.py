# Note: this is an automatically generated file. Do not edit.

r"""Functions for encoding Hydra modules as Haskell modules."""

from __future__ import annotations
from collections.abc import Callable
from decimal import Decimal
from functools import lru_cache
from hydra.dsl.python import Either, FrozenDict, Just, Maybe, Nothing, frozenlist
from typing import TypeVar, cast
import hydra.adapt.modules
import hydra.annotations
import hydra.classes
import hydra.coders
import hydra.compute
import hydra.constants
import hydra.core
import hydra.encode.core
import hydra.ext.haskell.ast
import hydra.ext.haskell.language
import hydra.ext.haskell.serde
import hydra.ext.haskell.utils
import hydra.formatting
import hydra.graph
import hydra.lexical
import hydra.lib.eithers
import hydra.lib.equality
import hydra.lib.flows
import hydra.lib.lists
import hydra.lib.literals
import hydra.lib.logic
import hydra.lib.maps
import hydra.lib.maybes
import hydra.lib.pairs
import hydra.lib.sets
import hydra.lib.strings
import hydra.module
import hydra.monads
import hydra.names
import hydra.rewriting
import hydra.schemas
import hydra.serialization
import hydra.show.core
import hydra.util

T0 = TypeVar("T0")
T1 = TypeVar("T1")
T2 = TypeVar("T2")

def encode_type(namespaces: hydra.module.Namespaces[hydra.ext.haskell.ast.ModuleName], typ: hydra.core.Type) -> hydra.compute.Flow[T0, hydra.ext.haskell.ast.Type]:
    def encode(v1: hydra.core.Type) -> hydra.compute.Flow[T0, hydra.ext.haskell.ast.Type]:
        return encode_type(namespaces, v1)
    def ref(name: hydra.core.Name) -> hydra.compute.Flow[T1, hydra.ext.haskell.ast.Type]:
        return hydra.lib.flows.pure(cast(hydra.ext.haskell.ast.Type, hydra.ext.haskell.ast.TypeVariable(hydra.ext.haskell.utils.element_reference(namespaces, name))))
    @lru_cache(1)
    def unit_tuple() -> hydra.ext.haskell.ast.Type:
        return cast(hydra.ext.haskell.ast.Type, hydra.ext.haskell.ast.TypeTuple(()))
    def _hoist_body_1(v1: hydra.core.FloatType) -> hydra.compute.Flow[T1, hydra.ext.haskell.ast.Type]:
        match v1:
            case hydra.core.FloatType.FLOAT32:
                return hydra.lib.flows.pure(cast(hydra.ext.haskell.ast.Type, hydra.ext.haskell.ast.TypeVariable(hydra.ext.haskell.utils.raw_name("Float"))))
            
            case hydra.core.FloatType.FLOAT64:
                return hydra.lib.flows.pure(cast(hydra.ext.haskell.ast.Type, hydra.ext.haskell.ast.TypeVariable(hydra.ext.haskell.utils.raw_name("Double"))))
            
            case hydra.core.FloatType.BIGFLOAT:
                return hydra.lib.flows.pure(cast(hydra.ext.haskell.ast.Type, hydra.ext.haskell.ast.TypeVariable(hydra.ext.haskell.utils.raw_name("Double"))))
            
            case _:
                raise AssertionError("Unreachable: all variants handled")
    def _hoist_body_2(it: hydra.core.IntegerType, v1: hydra.core.IntegerType) -> hydra.compute.Flow[T1, hydra.ext.haskell.ast.Type]:
        match v1:
            case hydra.core.IntegerType.BIGINT:
                return hydra.lib.flows.pure(cast(hydra.ext.haskell.ast.Type, hydra.ext.haskell.ast.TypeVariable(hydra.ext.haskell.utils.raw_name("Integer"))))
            
            case hydra.core.IntegerType.INT8:
                return hydra.lib.flows.pure(cast(hydra.ext.haskell.ast.Type, hydra.ext.haskell.ast.TypeVariable(hydra.ext.haskell.utils.raw_name("I.Int8"))))
            
            case hydra.core.IntegerType.INT16:
                return hydra.lib.flows.pure(cast(hydra.ext.haskell.ast.Type, hydra.ext.haskell.ast.TypeVariable(hydra.ext.haskell.utils.raw_name("I.Int16"))))
            
            case hydra.core.IntegerType.INT32:
                return hydra.lib.flows.pure(cast(hydra.ext.haskell.ast.Type, hydra.ext.haskell.ast.TypeVariable(hydra.ext.haskell.utils.raw_name("Int"))))
            
            case hydra.core.IntegerType.INT64:
                return hydra.lib.flows.pure(cast(hydra.ext.haskell.ast.Type, hydra.ext.haskell.ast.TypeVariable(hydra.ext.haskell.utils.raw_name("I.Int64"))))
            
            case _:
                return hydra.lib.flows.fail(hydra.lib.strings.cat2("unexpected integer type: ", hydra.show.core.integer_type(it)))
    def _hoist_body_3(lt: hydra.core.LiteralType, v1: hydra.core.LiteralType) -> hydra.compute.Flow[T1, hydra.ext.haskell.ast.Type]:
        match v1:
            case hydra.core.LiteralTypeBinary():
                return hydra.lib.flows.pure(cast(hydra.ext.haskell.ast.Type, hydra.ext.haskell.ast.TypeVariable(hydra.ext.haskell.utils.raw_name("B.ByteString"))))
            
            case hydra.core.LiteralTypeBoolean():
                return hydra.lib.flows.pure(cast(hydra.ext.haskell.ast.Type, hydra.ext.haskell.ast.TypeVariable(hydra.ext.haskell.utils.raw_name("Bool"))))
            
            case hydra.core.LiteralTypeFloat(value=ft):
                return _hoist_body_1(ft)
            
            case hydra.core.LiteralTypeInteger(value=it):
                return _hoist_body_2(it, it)
            
            case hydra.core.LiteralTypeString():
                return hydra.lib.flows.pure(cast(hydra.ext.haskell.ast.Type, hydra.ext.haskell.ast.TypeVariable(hydra.ext.haskell.utils.raw_name("String"))))
            
            case _:
                return hydra.lib.flows.fail(hydra.lib.strings.cat2("unexpected literal type: ", hydra.show.core.literal_type(lt)))
    def _hoist_body_4(v1: hydra.core.Type) -> hydra.compute.Flow[T0, hydra.ext.haskell.ast.Type]:
        match v1:
            case hydra.core.TypeApplication(value=app):
                @lru_cache(1)
                def lhs() -> hydra.core.Type:
                    return app.function
                @lru_cache(1)
                def rhs() -> hydra.core.Type:
                    return app.argument
                return hydra.lib.flows.bind(encode(lhs()), (lambda hlhs: hydra.lib.flows.bind(encode(rhs()), (lambda hrhs: hydra.lib.flows.pure(hydra.ext.haskell.utils.to_type_application((hlhs, hrhs)))))))
            
            case hydra.core.TypeEither(value=either_type):
                @lru_cache(1)
                def left() -> hydra.core.Type:
                    return either_type.left
                @lru_cache(1)
                def right() -> hydra.core.Type:
                    return either_type.right
                return hydra.lib.flows.map((lambda x1: hydra.ext.haskell.utils.to_type_application(x1)), hydra.lib.flows.sequence((hydra.lib.flows.pure(cast(hydra.ext.haskell.ast.Type, hydra.ext.haskell.ast.TypeVariable(hydra.ext.haskell.utils.raw_name("Either")))), encode(left()), encode(right()))))
            
            case hydra.core.TypeFunction(value=fun_type):
                @lru_cache(1)
                def dom() -> hydra.core.Type:
                    return fun_type.domain
                @lru_cache(1)
                def cod() -> hydra.core.Type:
                    return fun_type.codomain
                return hydra.lib.flows.bind(encode(dom()), (lambda hdom: hydra.lib.flows.bind(encode(cod()), (lambda hcod: hydra.lib.flows.pure(cast(hydra.ext.haskell.ast.Type, hydra.ext.haskell.ast.TypeFunction(hydra.ext.haskell.ast.FunctionType(hdom, hcod))))))))
            
            case hydra.core.TypeForall(value=forall_type):
                @lru_cache(1)
                def v() -> hydra.core.Name:
                    return forall_type.parameter
                @lru_cache(1)
                def body() -> hydra.core.Type:
                    return forall_type.body
                return encode(body())
            
            case hydra.core.TypeList(value=lt):
                return hydra.lib.flows.bind(encode(lt), (lambda hlt: hydra.lib.flows.pure(cast(hydra.ext.haskell.ast.Type, hydra.ext.haskell.ast.TypeList(hlt)))))
            
            case hydra.core.TypeLiteral(value=lt2):
                return _hoist_body_3(lt2, lt2)
            
            case hydra.core.TypeMap(value=map_type):
                @lru_cache(1)
                def kt() -> hydra.core.Type:
                    return map_type.keys
                @lru_cache(1)
                def vt() -> hydra.core.Type:
                    return map_type.values
                return hydra.lib.flows.map((lambda x1: hydra.ext.haskell.utils.to_type_application(x1)), hydra.lib.flows.sequence((hydra.lib.flows.pure(cast(hydra.ext.haskell.ast.Type, hydra.ext.haskell.ast.TypeVariable(hydra.ext.haskell.utils.raw_name("M.Map")))), encode(kt()), encode(vt()))))
            
            case hydra.core.TypeMaybe(value=ot):
                return hydra.lib.flows.map((lambda x1: hydra.ext.haskell.utils.to_type_application(x1)), hydra.lib.flows.sequence((hydra.lib.flows.pure(cast(hydra.ext.haskell.ast.Type, hydra.ext.haskell.ast.TypeVariable(hydra.ext.haskell.utils.raw_name("Maybe")))), encode(ot))))
            
            case hydra.core.TypePair(value=pt):
                return hydra.lib.flows.bind(encode(pt.first), (lambda f: hydra.lib.flows.bind(encode(pt.second), (lambda s: hydra.lib.flows.pure(cast(hydra.ext.haskell.ast.Type, hydra.ext.haskell.ast.TypeTuple((f, s))))))))
            
            case hydra.core.TypeRecord(value=rt):
                return ref(rt.type_name)
            
            case hydra.core.TypeSet(value=st):
                return hydra.lib.flows.map((lambda x1: hydra.ext.haskell.utils.to_type_application(x1)), hydra.lib.flows.sequence((hydra.lib.flows.pure(cast(hydra.ext.haskell.ast.Type, hydra.ext.haskell.ast.TypeVariable(hydra.ext.haskell.utils.raw_name("S.Set")))), encode(st))))
            
            case hydra.core.TypeUnion(value=rt2):
                @lru_cache(1)
                def type_name() -> hydra.core.Name:
                    return rt2.type_name
                return ref(type_name())
            
            case hydra.core.TypeUnit():
                return hydra.lib.flows.pure(unit_tuple())
            
            case hydra.core.TypeVariable(value=v12):
                return ref(v12)
            
            case hydra.core.TypeWrap(value=wrapped):
                @lru_cache(1)
                def name() -> hydra.core.Name:
                    return wrapped.type_name
                return ref(name())
            
            case _:
                return hydra.lib.flows.fail(hydra.lib.strings.cat2("unexpected type: ", hydra.show.core.type(typ)))
    return hydra.monads.with_trace("encode type", _hoist_body_4(hydra.rewriting.deannotate_type(typ)))

def adapt_type_to_haskell_and_encode(namespaces: hydra.module.Namespaces[hydra.ext.haskell.ast.ModuleName], v1: hydra.core.Type) -> hydra.compute.Flow[hydra.graph.Graph, hydra.ext.haskell.ast.Type]:
    r"""Adapt a Hydra type to Haskell's type system and encode it."""
    
    return hydra.adapt.modules.adapt_type_to_language_and_encode(hydra.ext.haskell.language.haskell_language(), (lambda v12: encode_type(namespaces, v12)), v1)

def constant_for_field_name(tname: hydra.core.Name, fname: hydra.core.Name) -> str:
    r"""Generate a constant name for a field (e.g., '_TypeName_fieldName')."""
    
    return hydra.lib.strings.cat(("_", hydra.names.local_name_of(tname), "_", fname.value))

def constant_for_type_name(tname: hydra.core.Name) -> str:
    r"""Generate a constant name for a type (e.g., '_TypeName')."""
    
    return hydra.lib.strings.cat2("_", hydra.names.local_name_of(tname))

# The key used to track Haskell variable depth in annotations.
key_haskell_var = hydra.core.Name("haskellVar")

def encode_literal(l: hydra.core.Literal) -> hydra.compute.Flow[T0, hydra.ext.haskell.ast.Expression]:
    def _hoist_hydra_ext_haskell_coder_encode_literal_1(v1: hydra.core.FloatValue) -> hydra.compute.Flow[T1, hydra.ext.haskell.ast.Expression]:
        match v1:
            case hydra.core.FloatValueFloat32(value=f):
                return hydra.lib.flows.pure(hydra.ext.haskell.utils.hslit(cast(hydra.ext.haskell.ast.Literal, hydra.ext.haskell.ast.LiteralFloat(f))))
            
            case hydra.core.FloatValueFloat64(value=f2):
                return hydra.lib.flows.pure(hydra.ext.haskell.utils.hslit(cast(hydra.ext.haskell.ast.Literal, hydra.ext.haskell.ast.LiteralDouble(f2))))
            
            case hydra.core.FloatValueBigfloat(value=f3):
                return hydra.lib.flows.pure(hydra.ext.haskell.utils.hslit(cast(hydra.ext.haskell.ast.Literal, hydra.ext.haskell.ast.LiteralDouble(hydra.lib.literals.bigfloat_to_float64(f3)))))
            
            case _:
                raise AssertionError("Unreachable: all variants handled")
    def _hoist_hydra_ext_haskell_coder_encode_literal_2(v1: hydra.core.IntegerValue) -> hydra.compute.Flow[T1, hydra.ext.haskell.ast.Expression]:
        match v1:
            case hydra.core.IntegerValueBigint(value=i):
                return hydra.lib.flows.pure(hydra.ext.haskell.utils.hslit(cast(hydra.ext.haskell.ast.Literal, hydra.ext.haskell.ast.LiteralInteger(i))))
            
            case hydra.core.IntegerValueInt8(value=i2):
                return hydra.lib.flows.pure(hydra.ext.haskell.utils.hslit(cast(hydra.ext.haskell.ast.Literal, hydra.ext.haskell.ast.LiteralInteger(hydra.lib.literals.int8_to_bigint(i2)))))
            
            case hydra.core.IntegerValueInt16(value=i3):
                return hydra.lib.flows.pure(hydra.ext.haskell.utils.hslit(cast(hydra.ext.haskell.ast.Literal, hydra.ext.haskell.ast.LiteralInteger(hydra.lib.literals.int16_to_bigint(i3)))))
            
            case hydra.core.IntegerValueInt32(value=i4):
                return hydra.lib.flows.pure(hydra.ext.haskell.utils.hslit(cast(hydra.ext.haskell.ast.Literal, hydra.ext.haskell.ast.LiteralInt(i4))))
            
            case hydra.core.IntegerValueInt64(value=i5):
                return hydra.lib.flows.pure(hydra.ext.haskell.utils.hslit(cast(hydra.ext.haskell.ast.Literal, hydra.ext.haskell.ast.LiteralInteger(hydra.lib.literals.int64_to_bigint(i5)))))
            
            case hydra.core.IntegerValueUint8(value=i6):
                return hydra.lib.flows.pure(hydra.ext.haskell.utils.hslit(cast(hydra.ext.haskell.ast.Literal, hydra.ext.haskell.ast.LiteralInteger(hydra.lib.literals.uint8_to_bigint(i6)))))
            
            case hydra.core.IntegerValueUint16(value=i7):
                return hydra.lib.flows.pure(hydra.ext.haskell.utils.hslit(cast(hydra.ext.haskell.ast.Literal, hydra.ext.haskell.ast.LiteralInteger(hydra.lib.literals.uint16_to_bigint(i7)))))
            
            case hydra.core.IntegerValueUint32(value=i8):
                return hydra.lib.flows.pure(hydra.ext.haskell.utils.hslit(cast(hydra.ext.haskell.ast.Literal, hydra.ext.haskell.ast.LiteralInteger(hydra.lib.literals.uint32_to_bigint(i8)))))
            
            case hydra.core.IntegerValueUint64(value=i9):
                return hydra.lib.flows.pure(hydra.ext.haskell.utils.hslit(cast(hydra.ext.haskell.ast.Literal, hydra.ext.haskell.ast.LiteralInteger(hydra.lib.literals.uint64_to_bigint(i9)))))
            
            case _:
                raise AssertionError("Unreachable: all variants handled")
    match l:
        case hydra.core.LiteralBinary(value=bs):
            return hydra.lib.flows.pure(hydra.ext.haskell.utils.hsapp(hydra.ext.haskell.utils.hsvar("Literals.stringToBinary"), hydra.ext.haskell.utils.hslit(cast(hydra.ext.haskell.ast.Literal, hydra.ext.haskell.ast.LiteralString(hydra.lib.literals.binary_to_string(bs))))))
        
        case hydra.core.LiteralBoolean(value=b):
            return hydra.lib.flows.pure(hydra.ext.haskell.utils.hsvar(hydra.lib.logic.if_else(b, (lambda : "True"), (lambda : "False"))))
        
        case hydra.core.LiteralFloat(value=fv):
            return _hoist_hydra_ext_haskell_coder_encode_literal_1(fv)
        
        case hydra.core.LiteralInteger(value=iv):
            return _hoist_hydra_ext_haskell_coder_encode_literal_2(iv)
        
        case hydra.core.LiteralString(value=s):
            return hydra.lib.flows.pure(hydra.ext.haskell.utils.hslit(cast(hydra.ext.haskell.ast.Literal, hydra.ext.haskell.ast.LiteralString(s))))
        
        case _:
            return hydra.lib.flows.fail(hydra.lib.strings.cat2("literal value ", hydra.show.core.literal(l)))

def encode_function(namespaces: hydra.module.Namespaces[hydra.ext.haskell.ast.ModuleName], fun: hydra.core.Function) -> hydra.compute.Flow[hydra.graph.Graph, hydra.ext.haskell.ast.Expression]:
    def _hoist_hydra_ext_haskell_coder_encode_function_1(namespaces: hydra.module.Namespaces[hydra.ext.haskell.ast.ModuleName], v1: hydra.core.Elimination) -> hydra.compute.Flow[hydra.graph.Graph, hydra.ext.haskell.ast.Expression]:
        match v1:
            case hydra.core.EliminationWrap(value=name):
                return hydra.lib.flows.pure(cast(hydra.ext.haskell.ast.Expression, hydra.ext.haskell.ast.ExpressionVariable(hydra.ext.haskell.utils.element_reference(namespaces, hydra.names.qname(hydra.lib.maybes.from_just(hydra.names.namespace_of(name)), hydra.ext.haskell.utils.newtype_accessor_name(name))))))
            
            case hydra.core.EliminationRecord(value=proj):
                @lru_cache(1)
                def dn() -> hydra.core.Name:
                    return proj.type_name
                @lru_cache(1)
                def fname() -> hydra.core.Name:
                    return proj.field
                return hydra.lib.flows.pure(cast(hydra.ext.haskell.ast.Expression, hydra.ext.haskell.ast.ExpressionVariable(hydra.ext.haskell.utils.record_field_reference(namespaces, dn(), fname()))))
            
            case hydra.core.EliminationUnion(value=stmt):
                @lru_cache(1)
                def dn() -> hydra.core.Name:
                    return stmt.type_name
                @lru_cache(1)
                def def_() -> Maybe[hydra.core.Term]:
                    return stmt.default
                @lru_cache(1)
                def fields() -> frozenlist[hydra.core.Field]:
                    return stmt.cases
                @lru_cache(1)
                def case_expr() -> hydra.compute.Flow[hydra.graph.Graph, hydra.ext.haskell.ast.Expression]:
                    return hydra.lib.flows.bind(hydra.lexical.with_schema_context(hydra.schemas.require_union_type(dn())), (lambda rt: (field_map := hydra.lib.maps.from_list(hydra.lib.lists.map((lambda x1: to_field_map_entry(x1)), rt.fields)), to_field_map_entry := (lambda f: (f.name, f)), hydra.lib.flows.bind(hydra.lib.flows.map_list((lambda v12: to_alt(field_map, v12)), fields()), (lambda ecases: hydra.lib.flows.bind(hydra.lib.maybes.cases(def_(), hydra.lib.flows.pure(()), (lambda d: hydra.lib.flows.bind(hydra.lib.flows.map((lambda x: hydra.ext.haskell.ast.CaseRhs(x)), encode_term(namespaces, d)), (lambda cs: (lhs := cast(hydra.ext.haskell.ast.Pattern, hydra.ext.haskell.ast.PatternName(hydra.ext.haskell.utils.raw_name(hydra.constants.ignored_variable))), alt := hydra.ext.haskell.ast.Alternative(lhs, cs, Nothing()), hydra.lib.flows.pure((alt,)))[2])))), (lambda dcases: hydra.lib.flows.pure(cast(hydra.ext.haskell.ast.Expression, hydra.ext.haskell.ast.ExpressionCase(hydra.ext.haskell.ast.CaseExpression(hydra.ext.haskell.utils.hsvar("x"), hydra.lib.lists.concat2(ecases, dcases))))))))))[2]))
                def to_alt(field_map: FrozenDict[hydra.core.Name, hydra.core.FieldType], field: hydra.core.Field) -> hydra.compute.Flow[hydra.graph.Graph, hydra.ext.haskell.ast.Alternative]:
                    @lru_cache(1)
                    def fn() -> hydra.core.Name:
                        return field.name
                    @lru_cache(1)
                    def fun_() -> hydra.core.Term:
                        return field.term
                    return hydra.annotations.with_depth(key_haskell_var, (lambda depth: (v0 := hydra.lib.strings.cat2("v", hydra.lib.literals.show_int32(depth)), raw := cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(fun_(), cast(hydra.core.Term, hydra.core.TermVariable(hydra.core.Name(v0)))))), rhs_term := hydra.rewriting.simplify_term(raw), v1 := hydra.lib.logic.if_else(hydra.rewriting.is_free_variable_in_term(hydra.core.Name(v0), rhs_term), (lambda : hydra.constants.ignored_variable), (lambda : v0)), hydra.lib.flows.bind(hydra.monads.get_state(), (lambda g_ufr: (hname := hydra.ext.haskell.utils.union_field_reference(g_ufr, namespaces, dn(), fn()), hydra.lib.flows.bind(hydra.lib.maybes.cases(hydra.lib.maps.lookup(fn(), field_map), hydra.lib.flows.fail(hydra.lib.strings.cat(("field ", hydra.lib.literals.show_string(fn().value), " not found in ", hydra.lib.literals.show_string(dn().value)))), (lambda field_type: (ft := field_type.type, no_args := hydra.lib.flows.pure(()), single_arg := hydra.lib.flows.pure((cast(hydra.ext.haskell.ast.Pattern, hydra.ext.haskell.ast.PatternName(hydra.ext.haskell.utils.raw_name(v1))),)), hydra.dsl.python.unsupported("inline match expressions are not yet supported"))[3])), (lambda args: (lhs := hydra.ext.haskell.utils.application_pattern(hname, args), hydra.lib.flows.bind(hydra.lib.flows.map((lambda x: hydra.ext.haskell.ast.CaseRhs(x)), encode_term(namespaces, rhs_term)), (lambda rhs: hydra.lib.flows.pure(hydra.ext.haskell.ast.Alternative(lhs, rhs, Nothing())))))[1])))[1])))[4]))
                return hydra.lib.flows.map((lambda v12: hydra.ext.haskell.utils.hslambda(hydra.ext.haskell.utils.raw_name("x"), v12)), case_expr())
            
            case _:
                raise AssertionError("Unreachable: all variants handled")
    match fun:
        case hydra.core.FunctionElimination(value=e):
            return _hoist_hydra_ext_haskell_coder_encode_function_1(namespaces, e)
        
        case hydra.core.FunctionLambda(value=lam):
            @lru_cache(1)
            def v() -> hydra.core.Name:
                return lam.parameter
            @lru_cache(1)
            def body() -> hydra.core.Term:
                return lam.body
            return hydra.lib.flows.bind(encode_term(namespaces, body()), (lambda hbody: hydra.lib.flows.pure(hydra.ext.haskell.utils.hslambda(hydra.ext.haskell.utils.element_reference(namespaces, v()), hbody))))
        
        case hydra.core.FunctionPrimitive(value=name):
            return hydra.lib.flows.pure(cast(hydra.ext.haskell.ast.Expression, hydra.ext.haskell.ast.ExpressionVariable(hydra.ext.haskell.utils.element_reference(namespaces, name))))
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def encode_term(namespaces: hydra.module.Namespaces[hydra.ext.haskell.ast.ModuleName], term: hydra.core.Term) -> hydra.compute.Flow[hydra.graph.Graph, hydra.ext.haskell.ast.Expression]:
    r"""Encode a Hydra term as a Haskell expression."""
    
    def encode(v1: hydra.core.Term) -> hydra.compute.Flow[hydra.graph.Graph, hydra.ext.haskell.ast.Expression]:
        return encode_term(namespaces, v1)
    def nonempty_map(m: FrozenDict[hydra.core.Term, hydra.core.Term]) -> hydra.compute.Flow[hydra.graph.Graph, hydra.ext.haskell.ast.Expression]:
        lhs = hydra.ext.haskell.utils.hsvar("M.fromList")
        def encode_pair(pair: tuple[hydra.core.Term, hydra.core.Term]) -> hydra.compute.Flow[hydra.graph.Graph, hydra.ext.haskell.ast.Expression]:
            @lru_cache(1)
            def k() -> hydra.core.Term:
                return hydra.lib.pairs.first(pair)
            @lru_cache(1)
            def v() -> hydra.core.Term:
                return hydra.lib.pairs.second(pair)
            @lru_cache(1)
            def hk() -> hydra.compute.Flow[hydra.graph.Graph, hydra.ext.haskell.ast.Expression]:
                return encode(k())
            @lru_cache(1)
            def hv() -> hydra.compute.Flow[hydra.graph.Graph, hydra.ext.haskell.ast.Expression]:
                return encode(v())
            return hydra.lib.flows.map((lambda x: cast(hydra.ext.haskell.ast.Expression, hydra.ext.haskell.ast.ExpressionTuple(x))), hydra.lib.flows.sequence((hk(), hv())))
        return hydra.lib.flows.bind(hydra.lib.flows.map((lambda x: cast(hydra.ext.haskell.ast.Expression, hydra.ext.haskell.ast.ExpressionList(x))), hydra.lib.flows.map_list((lambda x1: encode_pair(x1)), hydra.lib.maps.to_list(m))), (lambda rhs: hydra.lib.flows.pure(hydra.ext.haskell.utils.hsapp(lhs, rhs))))
    def nonempty_set(s: frozenset[hydra.core.Term]) -> hydra.compute.Flow[hydra.graph.Graph, hydra.ext.haskell.ast.Expression]:
        lhs = hydra.ext.haskell.utils.hsvar("S.fromList")
        return hydra.lib.flows.bind(encode_term(namespaces, cast(hydra.core.Term, hydra.core.TermList(hydra.lib.sets.to_list(s)))), (lambda rhs: hydra.lib.flows.pure(hydra.ext.haskell.utils.hsapp(lhs, rhs))))
    match hydra.rewriting.deannotate_term(term):
        case hydra.core.TermApplication(value=app):
            @lru_cache(1)
            def fun() -> hydra.core.Term:
                return app.function
            @lru_cache(1)
            def arg() -> hydra.core.Term:
                return app.argument
            return hydra.lib.flows.bind(encode(fun()), (lambda hfun: hydra.lib.flows.bind(encode(arg()), (lambda harg: hydra.lib.flows.pure(hydra.ext.haskell.utils.hsapp(hfun, harg))))))
        
        case hydra.core.TermEither(value=e):
            return hydra.lib.eithers.either((lambda l: hydra.lib.flows.bind(encode(l), (lambda hl: hydra.lib.flows.pure(hydra.ext.haskell.utils.hsapp(hydra.ext.haskell.utils.hsvar("Left"), hl))))), (lambda r: hydra.lib.flows.bind(encode(r), (lambda hr: hydra.lib.flows.pure(hydra.ext.haskell.utils.hsapp(hydra.ext.haskell.utils.hsvar("Right"), hr))))), e)
        
        case hydra.core.TermFunction(value=f):
            return encode_function(namespaces, f)
        
        case hydra.core.TermLet(value=let_term):
            @lru_cache(1)
            def bindings() -> frozenlist[hydra.core.Binding]:
                return let_term.bindings
            @lru_cache(1)
            def env() -> hydra.core.Term:
                return let_term.body
            def encode_binding(binding: hydra.core.Binding) -> hydra.compute.Flow[hydra.graph.Graph, hydra.ext.haskell.ast.LocalBinding]:
                @lru_cache(1)
                def name() -> hydra.core.Name:
                    return binding.name
                @lru_cache(1)
                def term_() -> hydra.core.Term:
                    return binding.term
                @lru_cache(1)
                def hname() -> hydra.ext.haskell.ast.Name:
                    return hydra.ext.haskell.utils.simple_name(name().value)
                return hydra.lib.flows.bind(encode(term_()), (lambda hexpr: hydra.lib.flows.pure(cast(hydra.ext.haskell.ast.LocalBinding, hydra.ext.haskell.ast.LocalBindingValue(hydra.ext.haskell.utils.simple_value_binding(hname(), hexpr, Nothing()))))))
            return hydra.lib.flows.bind(hydra.lib.flows.map_list((lambda x1: encode_binding(x1)), bindings()), (lambda hbindings: hydra.lib.flows.bind(encode(env()), (lambda hinner: hydra.lib.flows.pure(cast(hydra.ext.haskell.ast.Expression, hydra.ext.haskell.ast.ExpressionLet(hydra.ext.haskell.ast.LetExpression(hbindings, hinner))))))))
        
        case hydra.core.TermList(value=els):
            return hydra.lib.flows.bind(hydra.lib.flows.map_list((lambda x1: encode(x1)), els), (lambda helems: hydra.lib.flows.pure(cast(hydra.ext.haskell.ast.Expression, hydra.ext.haskell.ast.ExpressionList(helems)))))
        
        case hydra.core.TermLiteral(value=v):
            return encode_literal(v)
        
        case hydra.core.TermMap(value=m):
            return hydra.lib.logic.if_else(hydra.lib.maps.null(m), (lambda : hydra.lib.flows.pure(hydra.ext.haskell.utils.hsvar("M.empty"))), (lambda : nonempty_map(m)))
        
        case hydra.core.TermMaybe(value=m2):
            return hydra.lib.maybes.cases(m2, hydra.lib.flows.pure(hydra.ext.haskell.utils.hsvar("Nothing")), (lambda t: hydra.lib.flows.bind(encode(t), (lambda ht: hydra.lib.flows.pure(hydra.ext.haskell.utils.hsapp(hydra.ext.haskell.utils.hsvar("Just"), ht))))))
        
        case hydra.core.TermPair(value=p):
            return hydra.lib.flows.bind(encode(hydra.lib.pairs.first(p)), (lambda f: hydra.lib.flows.bind(encode(hydra.lib.pairs.second(p)), (lambda s: hydra.lib.flows.pure(cast(hydra.ext.haskell.ast.Expression, hydra.ext.haskell.ast.ExpressionTuple((f, s))))))))
        
        case hydra.core.TermRecord(value=record):
            @lru_cache(1)
            def sname() -> hydra.core.Name:
                return record.type_name
            @lru_cache(1)
            def fields() -> frozenlist[hydra.core.Field]:
                return record.fields
            def to_field_update(field: hydra.core.Field) -> hydra.compute.Flow[hydra.graph.Graph, hydra.ext.haskell.ast.FieldUpdate]:
                @lru_cache(1)
                def fn() -> hydra.core.Name:
                    return field.name
                @lru_cache(1)
                def ft() -> hydra.core.Term:
                    return field.term
                @lru_cache(1)
                def field_ref() -> hydra.ext.haskell.ast.Name:
                    return hydra.ext.haskell.utils.record_field_reference(namespaces, sname(), fn())
                return hydra.lib.flows.bind(encode(ft()), (lambda hft: hydra.lib.flows.pure(hydra.ext.haskell.ast.FieldUpdate(field_ref(), hft))))
            @lru_cache(1)
            def type_name() -> hydra.ext.haskell.ast.Name:
                return hydra.ext.haskell.utils.element_reference(namespaces, sname())
            return hydra.lib.flows.bind(hydra.lib.flows.map_list((lambda x1: to_field_update(x1)), fields()), (lambda updates: hydra.lib.flows.pure(cast(hydra.ext.haskell.ast.Expression, hydra.ext.haskell.ast.ExpressionConstructRecord(hydra.ext.haskell.ast.ConstructRecordExpression(type_name(), updates))))))
        
        case hydra.core.TermSet(value=s):
            return hydra.lib.logic.if_else(hydra.lib.sets.null(s), (lambda : hydra.lib.flows.pure(hydra.ext.haskell.utils.hsvar("S.empty"))), (lambda : nonempty_set(s)))
        
        case hydra.core.TermTypeLambda(value=abs):
            @lru_cache(1)
            def term1() -> hydra.core.Term:
                return abs.body
            return encode(term1())
        
        case hydra.core.TermTypeApplication(value=typed):
            @lru_cache(1)
            def term1() -> hydra.core.Term:
                return typed.body
            return encode(term1())
        
        case hydra.core.TermUnion(value=injection):
            @lru_cache(1)
            def sname() -> hydra.core.Name:
                return injection.type_name
            @lru_cache(1)
            def field() -> hydra.core.Field:
                return injection.field
            @lru_cache(1)
            def fn() -> hydra.core.Name:
                return field().name
            @lru_cache(1)
            def ft() -> hydra.core.Term:
                return field().term
            return hydra.lib.flows.bind(hydra.monads.get_state(), (lambda g_ufr2: (lhs := cast(hydra.ext.haskell.ast.Expression, hydra.ext.haskell.ast.ExpressionVariable(hydra.ext.haskell.utils.union_field_reference(g_ufr2, namespaces, sname(), fn()))), dflt := hydra.lib.flows.map((lambda v1: hydra.ext.haskell.utils.hsapp(lhs, v1)), encode(ft())), _hoist_body_1 := (lambda v1: hydra.dsl.python.unsupported("inline match expressions are not yet supported")), hydra.lib.flows.bind(hydra.schemas.require_union_field(sname(), fn()), (lambda ftyp: _hoist_body_1(hydra.rewriting.deannotate_type(ftyp)))))[3]))
        
        case hydra.core.TermUnit():
            return hydra.lib.flows.pure(cast(hydra.ext.haskell.ast.Expression, hydra.ext.haskell.ast.ExpressionTuple(())))
        
        case hydra.core.TermVariable(value=name):
            return hydra.lib.flows.pure(cast(hydra.ext.haskell.ast.Expression, hydra.ext.haskell.ast.ExpressionVariable(hydra.ext.haskell.utils.element_reference(namespaces, name))))
        
        case hydra.core.TermWrap(value=wrapped):
            @lru_cache(1)
            def tname() -> hydra.core.Name:
                return wrapped.type_name
            @lru_cache(1)
            def term_() -> hydra.core.Term:
                return wrapped.body
            @lru_cache(1)
            def lhs() -> hydra.ext.haskell.ast.Expression:
                return cast(hydra.ext.haskell.ast.Expression, hydra.ext.haskell.ast.ExpressionVariable(hydra.ext.haskell.utils.element_reference(namespaces, tname())))
            return hydra.lib.flows.bind(encode(term_()), (lambda rhs: hydra.lib.flows.pure(hydra.ext.haskell.utils.hsapp(lhs(), rhs))))
        
        case _:
            return hydra.lib.flows.fail(hydra.lib.strings.cat2("unexpected term: ", hydra.show.core.term(term)))

def find_ord_variables(typ: hydra.core.Type) -> frozenset[hydra.core.Name]:
    r"""Find type variables that require an Ord constraint (used in maps or sets)."""
    
    def fold(names: frozenset[hydra.core.Name], typ_: hydra.core.Type) -> frozenset[hydra.core.Name]:
        match typ_:
            case hydra.core.TypeMap(value=map_type):
                @lru_cache(1)
                def kt() -> hydra.core.Type:
                    return map_type.keys
                return try_type(names, kt())
            
            case hydra.core.TypeSet(value=et):
                return try_type(names, et)
            
            case _:
                return names
    def is_type_variable(v: hydra.core.Name) -> bool:
        @lru_cache(1)
        def name_str() -> str:
            return v.value
        @lru_cache(1)
        def has_no_namespace() -> bool:
            return hydra.lib.maybes.is_nothing(hydra.names.namespace_of(v))
        @lru_cache(1)
        def starts_with_t() -> bool:
            return hydra.lib.equality.equal(hydra.lib.strings.char_at(0, name_str()), 116)
        return hydra.lib.logic.and_(has_no_namespace(), starts_with_t())
    def try_type(names: frozenset[hydra.core.Name], t: hydra.core.Type) -> frozenset[hydra.core.Name]:
        match hydra.rewriting.deannotate_type(t):
            case hydra.core.TypeVariable(value=v):
                return hydra.lib.logic.if_else(is_type_variable(v), (lambda : hydra.lib.sets.insert(v, names)), (lambda : names))
            
            case _:
                return names
    return hydra.rewriting.fold_over_type(hydra.coders.TraversalOrder.PRE, (lambda x1, x2: fold(x1, x2)), hydra.lib.sets.empty(), typ)

def get_implicit_type_classes(typ: hydra.core.Type) -> FrozenDict[hydra.core.Name, frozenset[hydra.classes.TypeClass]]:
    r"""Get implicit typeclass constraints for type variables that need Ord."""
    
    def to_pair(name: T0) -> tuple[T0, frozenset[hydra.classes.TypeClass]]:
        return (name, hydra.lib.sets.from_list((hydra.classes.TypeClass.ORDERING,)))
    return hydra.lib.maps.from_list(hydra.lib.lists.map((lambda x1: to_pair(x1)), hydra.lib.sets.to_list(find_ord_variables(typ))))

def encode_type_with_class_assertions(namespaces: hydra.module.Namespaces[hydra.ext.haskell.ast.ModuleName], explicit_classes: FrozenDict[hydra.core.Name, frozenset[hydra.classes.TypeClass]], typ: hydra.core.Type) -> hydra.compute.Flow[hydra.graph.Graph, hydra.ext.haskell.ast.Type]:
    r"""Encode a Hydra type as a Haskell type with typeclass assertions."""
    
    @lru_cache(1)
    def classes() -> FrozenDict[hydra.core.Name, frozenset[hydra.classes.TypeClass]]:
        return hydra.lib.maps.union(explicit_classes, get_implicit_type_classes(typ))
    @lru_cache(1)
    def implicit_classes() -> FrozenDict[hydra.core.Name, frozenset[hydra.classes.TypeClass]]:
        return get_implicit_type_classes(typ)
    def encode_assertion(pair: tuple[hydra.core.Name, hydra.classes.TypeClass]) -> hydra.ext.haskell.ast.Assertion:
        @lru_cache(1)
        def name() -> hydra.core.Name:
            return hydra.lib.pairs.first(pair)
        @lru_cache(1)
        def cls() -> hydra.classes.TypeClass:
            return hydra.lib.pairs.second(pair)
        @lru_cache(1)
        def hname() -> hydra.ext.haskell.ast.Name:
            def _hoist_hname_1(v1: hydra.classes.TypeClass) -> str:
                match v1:
                    case hydra.classes.TypeClass.EQUALITY:
                        return "Eq"
                    
                    case hydra.classes.TypeClass.ORDERING:
                        return "Ord"
                    
                    case _:
                        raise AssertionError("Unreachable: all variants handled")
            return hydra.ext.haskell.utils.raw_name(_hoist_hname_1(cls()))
        @lru_cache(1)
        def htype() -> hydra.ext.haskell.ast.Type:
            return cast(hydra.ext.haskell.ast.Type, hydra.ext.haskell.ast.TypeVariable(hydra.ext.haskell.utils.raw_name(name().value)))
        return cast(hydra.ext.haskell.ast.Assertion, hydra.ext.haskell.ast.AssertionClass(hydra.ext.haskell.ast.ClassAssertion(hname(), (htype(),))))
    @lru_cache(1)
    def assert_pairs() -> frozenlist[tuple[hydra.core.Name, hydra.classes.TypeClass]]:
        return hydra.lib.lists.concat(hydra.lib.lists.map(to_pairs, hydra.lib.maps.to_list(classes())))
    def to_pairs(map_entry: tuple[T0, frozenset[T1]]) -> frozenlist[tuple[T0, T1]]:
        @lru_cache(1)
        def name() -> T0:
            return hydra.lib.pairs.first(map_entry)
        @lru_cache(1)
        def cls_set() -> frozenset[T1]:
            return hydra.lib.pairs.second(map_entry)
        def to_pair(c: T2) -> tuple[T0, T2]:
            return (name(), c)
        return hydra.lib.lists.map((lambda x1: to_pair(x1)), hydra.lib.sets.to_list(cls_set()))
    return hydra.monads.with_trace("encode with assertions", hydra.lib.flows.bind(adapt_type_to_haskell_and_encode(namespaces, typ), (lambda htyp: hydra.lib.logic.if_else(hydra.lib.lists.null(assert_pairs()), (lambda : hydra.lib.flows.pure(htyp)), (lambda : (encoded := hydra.lib.lists.map((lambda x1: encode_assertion(x1)), assert_pairs()), hassert := hydra.lib.logic.if_else(hydra.lib.equality.equal(hydra.lib.lists.length(encoded), 1), (lambda : hydra.lib.lists.head(encoded)), (lambda : cast(hydra.ext.haskell.ast.Assertion, hydra.ext.haskell.ast.AssertionTuple(encoded)))), hydra.lib.flows.pure(cast(hydra.ext.haskell.ast.Type, hydra.ext.haskell.ast.TypeCtx(hydra.ext.haskell.ast.ContextType(hassert, htyp)))))[2])))))

def type_scheme_constraints_to_class_map(maybe_constraints: Maybe[FrozenDict[T0, hydra.core.TypeVariableMetadata]]) -> FrozenDict[T0, frozenset[hydra.classes.TypeClass]]:
    def name_to_type_class(class_name: hydra.core.Name) -> Maybe[hydra.classes.TypeClass]:
        @lru_cache(1)
        def class_name_str() -> str:
            return class_name.value
        @lru_cache(1)
        def is_eq() -> bool:
            return hydra.lib.equality.equal(class_name_str(), hydra.core.Name("equality").value)
        @lru_cache(1)
        def is_ord() -> bool:
            return hydra.lib.equality.equal(class_name_str(), hydra.core.Name("ordering").value)
        return hydra.lib.logic.if_else(is_eq(), (lambda : Just(hydra.classes.TypeClass.EQUALITY)), (lambda : hydra.lib.logic.if_else(is_ord(), (lambda : Just(hydra.classes.TypeClass.ORDERING)), (lambda : Nothing()))))
    return hydra.lib.maybes.maybe(hydra.lib.maps.empty(), (lambda constraints: hydra.lib.maps.map((lambda meta: hydra.lib.sets.from_list(hydra.lib.maybes.cat(hydra.lib.lists.map((lambda x1: name_to_type_class(x1)), hydra.lib.sets.to_list(meta.classes))))), constraints)), maybe_constraints)

def to_data_declaration(namespaces: hydra.module.Namespaces[hydra.ext.haskell.ast.ModuleName], def_: hydra.module.TermDefinition) -> hydra.compute.Flow[hydra.graph.Graph, hydra.ext.haskell.ast.DeclarationWithComments]:
    r"""Convert a Hydra term definition to a Haskell declaration with comments."""
    
    @lru_cache(1)
    def name() -> hydra.core.Name:
        return def_.name
    @lru_cache(1)
    def term() -> hydra.core.Term:
        return def_.term
    @lru_cache(1)
    def typ() -> hydra.core.TypeScheme:
        return def_.type
    @lru_cache(1)
    def hname() -> hydra.ext.haskell.ast.Name:
        return hydra.ext.haskell.utils.simple_name(hydra.names.local_name_of(name()))
    def rewrite_value_binding(vb: hydra.ext.haskell.ast.ValueBinding) -> hydra.ext.haskell.ast.ValueBinding:
        match vb:
            case hydra.ext.haskell.ast.ValueBindingSimple(value=simple):
                @lru_cache(1)
                def pattern_() -> hydra.ext.haskell.ast.Pattern:
                    return simple.pattern
                @lru_cache(1)
                def rhs() -> hydra.ext.haskell.ast.RightHandSide:
                    return simple.rhs
                @lru_cache(1)
                def bindings() -> Maybe[hydra.ext.haskell.ast.LocalBindings]:
                    return simple.local_bindings
                match pattern_():
                    case hydra.ext.haskell.ast.PatternApplication(value=app_pat):
                        @lru_cache(1)
                        def name_() -> hydra.ext.haskell.ast.Name:
                            return app_pat.name
                        @lru_cache(1)
                        def args() -> frozenlist[hydra.ext.haskell.ast.Pattern]:
                            return app_pat.args
                        @lru_cache(1)
                        def rhs_expr() -> hydra.ext.haskell.ast.Expression:
                            return rhs().value
                        match rhs_expr():
                            case hydra.ext.haskell.ast.ExpressionLambda(value=lambda_):
                                @lru_cache(1)
                                def vars() -> frozenlist[hydra.ext.haskell.ast.Pattern]:
                                    return lambda_.bindings
                                @lru_cache(1)
                                def body() -> hydra.ext.haskell.ast.Expression:
                                    return lambda_.inner
                                @lru_cache(1)
                                def new_pattern() -> hydra.ext.haskell.ast.Pattern:
                                    return hydra.ext.haskell.utils.application_pattern(name_(), hydra.lib.lists.concat2(args(), vars()))
                                @lru_cache(1)
                                def new_rhs() -> hydra.ext.haskell.ast.RightHandSide:
                                    return hydra.ext.haskell.ast.RightHandSide(body())
                                return rewrite_value_binding(cast(hydra.ext.haskell.ast.ValueBinding, hydra.ext.haskell.ast.ValueBindingSimple(hydra.ext.haskell.ast.SimpleValueBinding(new_pattern(), new_rhs(), bindings()))))
                            
                            case _:
                                return vb
                    
                    case _:
                        return vb
            
            case _:
                raise AssertionError("Unreachable: all variants handled")
    def to_decl(comments: Maybe[str], hname_: hydra.ext.haskell.ast.Name, term_: hydra.core.Term, bindings: Maybe[hydra.ext.haskell.ast.LocalBindings]) -> hydra.compute.Flow[hydra.graph.Graph, hydra.ext.haskell.ast.DeclarationWithComments]:
        match hydra.rewriting.deannotate_term(term_):
            case hydra.core.TermLet(value=let_term):
                @lru_cache(1)
                def lbindings() -> frozenlist[hydra.core.Binding]:
                    return let_term.bindings
                @lru_cache(1)
                def env() -> hydra.core.Term:
                    return let_term.body
                def to_binding(hname_: hydra.ext.haskell.ast.Name, hterm_: hydra.ext.haskell.ast.Expression) -> hydra.ext.haskell.ast.LocalBinding:
                    return cast(hydra.ext.haskell.ast.LocalBinding, hydra.ext.haskell.ast.LocalBindingValue(hydra.ext.haskell.utils.simple_value_binding(hname_, hterm_, Nothing())))
                @lru_cache(1)
                def hnames() -> frozenlist[hydra.ext.haskell.ast.Name]:
                    return hydra.lib.lists.map((lambda binding: hydra.ext.haskell.utils.simple_name(binding.name.value)), lbindings())
                @lru_cache(1)
                def terms() -> frozenlist[hydra.core.Term]:
                    return hydra.lib.lists.map((lambda v1: v1.term), lbindings())
                return hydra.lib.flows.bind(hydra.lib.flows.map_list((lambda v1: encode_term(namespaces, v1)), terms()), (lambda hterms: (hbindings := hydra.lib.lists.zip_with((lambda x1, x2: to_binding(x1, x2)), hnames(), hterms), to_decl(comments, hname_, env(), Just(hydra.ext.haskell.ast.LocalBindings(hbindings))))[1]))
            
            case _:
                return hydra.lib.flows.bind(encode_term(namespaces, term_), (lambda hterm: (vb := hydra.ext.haskell.utils.simple_value_binding(hname_, hterm, bindings), scheme_constraints := typ().constraints, scheme_classes := type_scheme_constraints_to_class_map(scheme_constraints), hydra.lib.flows.bind(hydra.annotations.get_type_classes(hydra.rewriting.remove_types_from_term(term())), (lambda explicit_classes: (combined_classes := hydra.lib.maps.union(scheme_classes, explicit_classes), hydra.lib.flows.bind(encode_type_with_class_assertions(namespaces, combined_classes, typ().type), (lambda htype: (decl := cast(hydra.ext.haskell.ast.Declaration, hydra.ext.haskell.ast.DeclarationTypedBinding(hydra.ext.haskell.ast.TypedBinding(hydra.ext.haskell.ast.TypeSignature(hname_, htype), rewrite_value_binding(vb)))), hydra.lib.flows.pure(hydra.ext.haskell.ast.DeclarationWithComments(decl, comments)))[1])))[1])))[3]))
    return hydra.lib.flows.bind(hydra.annotations.get_term_description(term()), (lambda comments: to_decl(comments, hname(), term(), Nothing())))

# Whether to include type definitions in generated Haskell modules.
include_type_definitions = False

# Whether to use the Hydra core import in generated modules.
use_core_import = True

def name_decls(g: T0, namespaces: hydra.module.Namespaces[hydra.ext.haskell.ast.ModuleName], name: hydra.core.Name, typ: hydra.core.Type) -> frozenlist[hydra.ext.haskell.ast.DeclarationWithComments]:
    @lru_cache(1)
    def nm() -> str:
        return name.value
    def to_decl(n: hydra.core.Name, pair: tuple[str, str]) -> hydra.ext.haskell.ast.DeclarationWithComments:
        @lru_cache(1)
        def k() -> str:
            return hydra.lib.pairs.first(pair)
        @lru_cache(1)
        def v() -> str:
            return hydra.lib.pairs.second(pair)
        @lru_cache(1)
        def decl() -> hydra.ext.haskell.ast.Declaration:
            return cast(hydra.ext.haskell.ast.Declaration, hydra.ext.haskell.ast.DeclarationValueBinding(cast(hydra.ext.haskell.ast.ValueBinding, hydra.ext.haskell.ast.ValueBindingSimple(hydra.ext.haskell.ast.SimpleValueBinding(hydra.ext.haskell.utils.application_pattern(hydra.ext.haskell.utils.simple_name(k()), ()), hydra.ext.haskell.ast.RightHandSide(cast(hydra.ext.haskell.ast.Expression, hydra.ext.haskell.ast.ExpressionApplication(hydra.ext.haskell.ast.ApplicationExpression(cast(hydra.ext.haskell.ast.Expression, hydra.ext.haskell.ast.ExpressionVariable(hydra.ext.haskell.utils.element_reference(namespaces, n))), cast(hydra.ext.haskell.ast.Expression, hydra.ext.haskell.ast.ExpressionLiteral(cast(hydra.ext.haskell.ast.Literal, hydra.ext.haskell.ast.LiteralString(v())))))))), Nothing())))))
        return hydra.ext.haskell.ast.DeclarationWithComments(decl(), Nothing())
    @lru_cache(1)
    def name_decl() -> tuple[str, str]:
        return (constant_for_type_name(name), nm())
    @lru_cache(1)
    def field_decls() -> frozenlist[tuple[str, str]]:
        return hydra.lib.lists.map((lambda x1: to_constant(x1)), hydra.lexical.fields_of(typ))
    def to_constant(field_type: hydra.core.FieldType) -> tuple[str, str]:
        @lru_cache(1)
        def fname() -> hydra.core.Name:
            return field_type.name
        return (constant_for_field_name(name, fname()), fname().value)
    return hydra.lib.logic.if_else(use_core_import, (lambda : hydra.lib.lists.cons(to_decl(hydra.core.Name("hydra.core.Name"), name_decl()), hydra.lib.lists.map((lambda v1: to_decl(hydra.core.Name("hydra.core.Name"), v1)), field_decls()))), (lambda : ()))

def type_decl(namespaces: hydra.module.Namespaces[hydra.ext.haskell.ast.ModuleName], name: hydra.core.Name, typ: hydra.core.Type) -> hydra.compute.Flow[hydra.graph.Graph, hydra.ext.haskell.ast.DeclarationWithComments]:
    def type_name(ns: hydra.module.Namespace, name_: hydra.core.Name) -> hydra.core.Name:
        return hydra.names.qname(ns, type_name_local(name_))
    def type_name_local(name_: hydra.core.Name) -> str:
        return hydra.lib.strings.cat(("_", hydra.names.local_name_of(name_), "_type_"))
    @lru_cache(1)
    def raw_term() -> hydra.core.Term:
        return hydra.encode.core.type(typ)
    def rewrite(recurse: Callable[[hydra.core.Term], hydra.core.Term], term: hydra.core.Term) -> hydra.core.Term:
        @lru_cache(1)
        def variant_result() -> Maybe[hydra.core.Field]:
            match hydra.rewriting.deannotate_term(term):
                case hydra.core.TermUnion(value=inj):
                    return hydra.lib.logic.if_else(hydra.lib.equality.equal(inj.type_name, hydra.core.Name("hydra.core.Type")), (lambda : Just(inj.field)), (lambda : Nothing()))
                
                case _:
                    return Nothing()
        def decode_string(term2: hydra.core.Term) -> Maybe[str]:
            def _hoist_decode_string_1(v1: hydra.core.Literal) -> Maybe[str]:
                match v1:
                    case hydra.core.LiteralString(value=s):
                        return Just(s)
                    
                    case _:
                        return Nothing()
            match hydra.rewriting.deannotate_term(term2):
                case hydra.core.TermLiteral(value=lit):
                    return _hoist_decode_string_1(lit)
                
                case _:
                    return Nothing()
        def decode_name(term2: hydra.core.Term) -> Maybe[hydra.core.Name]:
            match hydra.rewriting.deannotate_term(term2):
                case hydra.core.TermWrap(value=wt):
                    return hydra.lib.logic.if_else(hydra.lib.equality.equal(wt.type_name, hydra.core.Name("hydra.core.Name")), (lambda : hydra.lib.maybes.map((lambda x: hydra.core.Name(x)), decode_string(wt.body))), (lambda : Nothing()))
                
                case _:
                    return Nothing()
        def for_type(field: hydra.core.Field) -> Maybe[hydra.core.Term]:
            @lru_cache(1)
            def fname() -> hydra.core.Name:
                return field.name
            @lru_cache(1)
            def fterm() -> hydra.core.Term:
                return field.term
            return hydra.lib.logic.if_else(hydra.lib.equality.equal(fname(), hydra.core.Name("record")), (lambda : Nothing()), (lambda : hydra.lib.logic.if_else(hydra.lib.equality.equal(fname(), hydra.core.Name("variable")), (lambda : hydra.lib.maybes.bind(decode_name(fterm()), (lambda x1: for_variable_type(x1)))), (lambda : Nothing()))))
        def for_variable_type(vname: hydra.core.Name) -> Maybe[hydra.core.Term]:
            @lru_cache(1)
            def qname() -> hydra.module.QualifiedName:
                return hydra.names.qualify_name(vname)
            @lru_cache(1)
            def mns() -> Maybe[hydra.module.Namespace]:
                return qname().namespace
            @lru_cache(1)
            def local() -> str:
                return qname().local
            return hydra.lib.maybes.map((lambda ns: cast(hydra.core.Term, hydra.core.TermVariable(hydra.names.qname(ns, hydra.lib.strings.cat(("_", local(), "_type_")))))), mns())
        return hydra.lib.maybes.from_maybe(recurse(term), hydra.lib.maybes.bind(variant_result(), (lambda x1: for_type(x1))))
    @lru_cache(1)
    def final_term() -> hydra.core.Term:
        return hydra.rewriting.rewrite_term((lambda x1, x2: rewrite(x1, x2)), raw_term())
    return hydra.lib.flows.bind(hydra.adapt.modules.construct_coder(hydra.ext.haskell.language.haskell_language(), (lambda v1: encode_term(namespaces, v1)), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.core.Type")))), (lambda coder: hydra.lib.flows.bind(coder.encode(final_term()), (lambda expr: (rhs := hydra.ext.haskell.ast.RightHandSide(expr), hname := hydra.ext.haskell.utils.simple_name(type_name_local(name)), pat := hydra.ext.haskell.utils.application_pattern(hname, ()), decl := cast(hydra.ext.haskell.ast.Declaration, hydra.ext.haskell.ast.DeclarationValueBinding(cast(hydra.ext.haskell.ast.ValueBinding, hydra.ext.haskell.ast.ValueBindingSimple(hydra.ext.haskell.ast.SimpleValueBinding(pat, rhs, Nothing()))))), hydra.lib.flows.pure(hydra.ext.haskell.ast.DeclarationWithComments(decl, Nothing())))[4]))))

def to_type_declarations_from(namespaces: hydra.module.Namespaces[hydra.ext.haskell.ast.ModuleName], element_name: hydra.core.Name, typ: hydra.core.Type) -> hydra.compute.Flow[hydra.graph.Graph, frozenlist[hydra.ext.haskell.ast.DeclarationWithComments]]:
    r"""Convert a Hydra type definition to Haskell declarations."""
    
    @lru_cache(1)
    def lname() -> str:
        return hydra.names.local_name_of(element_name)
    @lru_cache(1)
    def hname() -> hydra.ext.haskell.ast.Name:
        return hydra.ext.haskell.utils.simple_name(lname())
    def decl_head(name: hydra.ext.haskell.ast.Name, vars_: frozenlist[hydra.core.Name]) -> hydra.ext.haskell.ast.DeclarationHead:
        return hydra.lib.logic.if_else(hydra.lib.lists.null(vars_), (lambda : cast(hydra.ext.haskell.ast.DeclarationHead, hydra.ext.haskell.ast.DeclarationHeadSimple(name))), (lambda : (h := hydra.lib.lists.head(vars_), rest := hydra.lib.lists.tail(vars_), hvar := hydra.ext.haskell.ast.Variable(hydra.ext.haskell.utils.simple_name(h.value)), cast(hydra.ext.haskell.ast.DeclarationHead, hydra.ext.haskell.ast.DeclarationHeadApplication(hydra.ext.haskell.ast.ApplicationDeclarationHead(decl_head(name, rest), hvar))))[3]))
    def newtype_cons(tname: hydra.core.Name, typ_: hydra.core.Type) -> hydra.compute.Flow[hydra.graph.Graph, hydra.ext.haskell.ast.ConstructorWithComments]:
        @lru_cache(1)
        def hname() -> hydra.ext.haskell.ast.Name:
            return hydra.ext.haskell.utils.simple_name(hydra.ext.haskell.utils.newtype_accessor_name(tname))
        return hydra.lib.flows.bind(adapt_type_to_haskell_and_encode(namespaces, typ_), (lambda htype: (hfield := hydra.ext.haskell.ast.FieldWithComments(hydra.ext.haskell.ast.Field(hname(), htype), Nothing()), constructor_name := hydra.ext.haskell.utils.simple_name(hydra.names.local_name_of(tname)), hydra.lib.flows.pure(hydra.ext.haskell.ast.ConstructorWithComments(cast(hydra.ext.haskell.ast.Constructor, hydra.ext.haskell.ast.ConstructorRecord(hydra.ext.haskell.ast.RecordConstructor(constructor_name, (hfield,)))), Nothing())))[2]))
    def record_cons(lname_: str, fields: frozenlist[hydra.core.FieldType]) -> hydra.compute.Flow[hydra.graph.Graph, hydra.ext.haskell.ast.ConstructorWithComments]:
        def to_field(field_type: hydra.core.FieldType) -> hydra.compute.Flow[hydra.graph.Graph, hydra.ext.haskell.ast.FieldWithComments]:
            @lru_cache(1)
            def fname() -> hydra.core.Name:
                return field_type.name
            @lru_cache(1)
            def ftype() -> hydra.core.Type:
                return field_type.type
            @lru_cache(1)
            def hname_() -> hydra.ext.haskell.ast.Name:
                return hydra.ext.haskell.utils.simple_name(hydra.lib.strings.cat2(hydra.formatting.decapitalize(lname_), hydra.formatting.capitalize(fname().value)))
            return hydra.lib.flows.bind(adapt_type_to_haskell_and_encode(namespaces, ftype()), (lambda htype: hydra.lib.flows.bind(hydra.annotations.get_type_description(ftype()), (lambda comments: hydra.lib.flows.pure(hydra.ext.haskell.ast.FieldWithComments(hydra.ext.haskell.ast.Field(hname_(), htype), comments))))))
        return hydra.lib.flows.bind(hydra.lib.flows.map_list((lambda x1: to_field(x1)), fields), (lambda h_fields: hydra.lib.flows.pure(hydra.ext.haskell.ast.ConstructorWithComments(cast(hydra.ext.haskell.ast.Constructor, hydra.ext.haskell.ast.ConstructorRecord(hydra.ext.haskell.ast.RecordConstructor(hydra.ext.haskell.utils.simple_name(lname_), h_fields))), Nothing()))))
    def union_cons(g_: hydra.graph.Graph, lname_: str, field_type: hydra.core.FieldType) -> hydra.compute.Flow[hydra.graph.Graph, hydra.ext.haskell.ast.ConstructorWithComments]:
        @lru_cache(1)
        def fname() -> hydra.core.Name:
            return field_type.name
        @lru_cache(1)
        def ftype() -> hydra.core.Type:
            return field_type.type
        def deconflict(name: str) -> str:
            @lru_cache(1)
            def tname() -> hydra.core.Name:
                return hydra.names.unqualify_name(hydra.module.QualifiedName(Just(hydra.lib.pairs.first(namespaces.focus)), name))
            return hydra.lib.logic.if_else(hydra.lib.maybes.is_just(hydra.lib.lists.find((lambda b: hydra.lib.equality.equal(b.name, tname())), g_.elements)), (lambda : deconflict(hydra.lib.strings.cat2(name, "_"))), (lambda : name))
        return hydra.lib.flows.bind(hydra.annotations.get_type_description(ftype()), (lambda comments: (nm := deconflict(hydra.lib.strings.cat2(hydra.formatting.capitalize(lname_), hydra.formatting.capitalize(fname().value))), hydra.lib.flows.bind(hydra.lib.logic.if_else(hydra.lib.equality.equal(hydra.rewriting.deannotate_type(ftype()), cast(hydra.core.Type, hydra.core.TypeUnit())), (lambda : hydra.lib.flows.pure(())), (lambda : hydra.lib.flows.bind(adapt_type_to_haskell_and_encode(namespaces, ftype()), (lambda htype: hydra.lib.flows.pure((htype,)))))), (lambda type_list: hydra.lib.flows.pure(hydra.ext.haskell.ast.ConstructorWithComments(cast(hydra.ext.haskell.ast.Constructor, hydra.ext.haskell.ast.ConstructorOrdinary(hydra.ext.haskell.ast.OrdinaryConstructor(hydra.ext.haskell.utils.simple_name(nm), type_list))), comments)))))[1]))
    return hydra.monads.with_trace(hydra.lib.strings.cat2("type definition ", element_name.value), hydra.lib.flows.bind(hydra.monads.get_state(), (lambda g: hydra.lib.flows.bind(hydra.schemas.is_serializable_by_name(element_name), (lambda is_ser: (deriv := hydra.ext.haskell.ast.Deriving(hydra.lib.logic.if_else(is_ser, (lambda : hydra.lib.lists.map((lambda x1: hydra.ext.haskell.utils.raw_name(x1)), ("Eq", "Ord", "Read", "Show"))), (lambda : ()))), unpack_result := hydra.ext.haskell.utils.unpack_forall_type(g, typ), vars := hydra.lib.pairs.first(unpack_result), t_ := hydra.lib.pairs.second(unpack_result), hd := decl_head(hname(), hydra.lib.lists.reverse(vars)), _hoist_body_1 := (lambda v1: hydra.dsl.python.unsupported("inline match expressions are not yet supported")), hydra.lib.flows.bind(_hoist_body_1(hydra.rewriting.deannotate_type(t_)), (lambda decl: hydra.lib.flows.bind(hydra.annotations.get_type_description(typ), (lambda comments: hydra.lib.flows.bind(hydra.lib.logic.if_else(include_type_definitions, (lambda : hydra.lib.flows.bind(type_decl(namespaces, element_name, typ), (lambda decl_: hydra.lib.flows.pure((decl_,))))), (lambda : hydra.lib.flows.pure(()))), (lambda tdecls: (main_decl := hydra.ext.haskell.ast.DeclarationWithComments(decl, comments), name_decls_ := name_decls(g, namespaces, element_name, typ), hydra.lib.flows.pure(hydra.lib.lists.concat(((main_decl,), name_decls_, tdecls))))[2])))))))[6])))))

def construct_module(namespaces: hydra.module.Namespaces[hydra.ext.haskell.ast.ModuleName], mod: hydra.module.Module, defs: frozenlist[hydra.module.Definition]) -> hydra.compute.Flow[hydra.graph.Graph, hydra.ext.haskell.ast.Module]:
    r"""Construct a Haskell module from a Hydra module and its definitions."""
    
    def h(namespace: hydra.module.Namespace) -> str:
        return namespace.value
    def create_declarations(g: T0, def_: hydra.module.Definition) -> hydra.compute.Flow[hydra.graph.Graph, frozenlist[hydra.ext.haskell.ast.DeclarationWithComments]]:
        match def_:
            case hydra.module.DefinitionType(value=type):
                @lru_cache(1)
                def name() -> hydra.core.Name:
                    return type.name
                @lru_cache(1)
                def typ() -> hydra.core.Type:
                    return type.type
                return to_type_declarations_from(namespaces, name(), typ())
            
            case hydra.module.DefinitionTerm(value=term):
                return hydra.lib.flows.bind(to_data_declaration(namespaces, term), (lambda d: hydra.lib.flows.pure((d,))))
            
            case _:
                raise AssertionError("Unreachable: all variants handled")
    def import_name(name: str) -> hydra.ext.haskell.ast.ModuleName:
        return hydra.ext.haskell.ast.ModuleName(hydra.lib.strings.intercalate(".", hydra.lib.lists.map(hydra.formatting.capitalize, hydra.lib.strings.split_on(".", name))))
    @lru_cache(1)
    def imports() -> frozenlist[hydra.ext.haskell.ast.Import]:
        return hydra.lib.lists.concat2(domain_imports(), standard_imports())
    @lru_cache(1)
    def domain_imports() -> frozenlist[hydra.ext.haskell.ast.Import]:
        def to_import(pair: tuple[hydra.module.Namespace, hydra.ext.haskell.ast.ModuleName]) -> hydra.ext.haskell.ast.Import:
            @lru_cache(1)
            def namespace() -> hydra.module.Namespace:
                return hydra.lib.pairs.first(pair)
            @lru_cache(1)
            def alias() -> hydra.ext.haskell.ast.ModuleName:
                return hydra.lib.pairs.second(pair)
            @lru_cache(1)
            def name() -> str:
                return h(namespace())
            return hydra.ext.haskell.ast.Import(True, import_name(name()), Just(alias()), Nothing())
        return hydra.lib.lists.map((lambda x1: to_import(x1)), hydra.lib.maps.to_list(namespaces.mapping))
    @lru_cache(1)
    def standard_imports() -> frozenlist[hydra.ext.haskell.ast.Import]:
        def to_import(triple: tuple[tuple[str, Maybe[str]], frozenlist[str]]) -> hydra.ext.haskell.ast.Import:
            @lru_cache(1)
            def name() -> str:
                return hydra.lib.pairs.first(hydra.lib.pairs.first(triple))
            @lru_cache(1)
            def malias() -> Maybe[str]:
                return hydra.lib.pairs.second(hydra.lib.pairs.first(triple))
            @lru_cache(1)
            def hidden() -> frozenlist[str]:
                return hydra.lib.pairs.second(triple)
            @lru_cache(1)
            def spec() -> Maybe[hydra.ext.haskell.ast.SpecImport]:
                return hydra.lib.logic.if_else(hydra.lib.lists.null(hidden()), (lambda : Nothing()), (lambda : Just(cast(hydra.ext.haskell.ast.SpecImport, hydra.ext.haskell.ast.SpecImportHiding(hydra.lib.lists.map((lambda n: hydra.ext.haskell.ast.ImportExportSpec(Nothing(), hydra.ext.haskell.utils.simple_name(n), Nothing())), hidden()))))))
            return hydra.ext.haskell.ast.Import(hydra.lib.maybes.is_just(malias()), hydra.ext.haskell.ast.ModuleName(name()), hydra.lib.maybes.map((lambda x: hydra.ext.haskell.ast.ModuleName(x)), malias()), spec())
        return hydra.lib.lists.map((lambda x1: to_import(x1)), hydra.lib.lists.concat2(((("Prelude", Nothing()), ("Enum", "Ordering", "decodeFloat", "encodeFloat", "fail", "map", "pure", "sum")), (("Data.ByteString", Just("B")), ()), (("Data.Int", Just("I")), ()), (("Data.List", Just("L")), ()), (("Data.Map", Just("M")), ()), (("Data.Set", Just("S")), ())), hydra.lib.logic.if_else(hydra.schemas.module_contains_binary_literals(mod), (lambda : ((("Hydra.Lib.Literals", Just("Literals")), ()),)), (lambda : ()))))
    return hydra.lib.flows.bind(hydra.monads.get_state(), (lambda g: hydra.lib.flows.bind(hydra.lib.flows.map_list((lambda v1: create_declarations(g, v1)), defs), (lambda decl_lists: (decls := hydra.lib.lists.concat(decl_lists), mc := mod.description, hydra.lib.flows.pure(hydra.ext.haskell.ast.Module(Just(hydra.ext.haskell.ast.ModuleHead(mc, import_name(h(mod.namespace)), ())), imports(), decls)))[2]))))

def module_to_haskell_module(mod: hydra.module.Module, defs: frozenlist[hydra.module.Definition]) -> hydra.compute.Flow[hydra.graph.Graph, hydra.ext.haskell.ast.Module]:
    r"""Convert a Hydra module and definitions to a Haskell module AST."""
    
    return hydra.lib.flows.bind(hydra.ext.haskell.utils.namespaces_for_module(mod), (lambda namespaces: construct_module(namespaces, mod, defs)))

def module_to_haskell(mod: hydra.module.Module, defs: frozenlist[hydra.module.Definition]) -> hydra.compute.Flow[hydra.graph.Graph, FrozenDict[str, str]]:
    r"""Convert a Hydra module to Haskell source code as a filepath-to-content map."""
    
    return hydra.lib.flows.bind(module_to_haskell_module(mod, defs), (lambda hsmod: (s := hydra.serialization.print_expr(hydra.serialization.parenthesize(hydra.ext.haskell.serde.module_to_expr(hsmod))), filepath := hydra.names.namespace_to_file_path(hydra.util.CaseConvention.PASCAL, hydra.module.FileExtension("hs"), mod.namespace), hydra.lib.flows.pure(hydra.lib.maps.singleton(filepath, s)))[2]))
