# Note: this is an automatically generated file. Do not edit.

r"""String representations of hydra.core types."""

from __future__ import annotations
from collections.abc import Callable
from decimal import Decimal
from functools import lru_cache
from hydra.dsl.python import Either, FrozenDict, Just, Maybe, frozenlist
from typing import TypeVar, cast
import hydra.core
import hydra.lib.eithers
import hydra.lib.lists
import hydra.lib.literals
import hydra.lib.logic
import hydra.lib.maps
import hydra.lib.maybes
import hydra.lib.pairs
import hydra.lib.sets
import hydra.lib.strings

T0 = TypeVar("T0")

def float_type(ft: hydra.core.FloatType) -> str:
    r"""Show a float type as a string."""
    
    match ft:
        case hydra.core.FloatType.BIGFLOAT:
            return "bigfloat"
        
        case hydra.core.FloatType.FLOAT32:
            return "float32"
        
        case hydra.core.FloatType.FLOAT64:
            return "float64"
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def integer_type(it: hydra.core.IntegerType) -> str:
    r"""Show an integer type as a string."""
    
    match it:
        case hydra.core.IntegerType.BIGINT:
            return "bigint"
        
        case hydra.core.IntegerType.INT8:
            return "int8"
        
        case hydra.core.IntegerType.INT16:
            return "int16"
        
        case hydra.core.IntegerType.INT32:
            return "int32"
        
        case hydra.core.IntegerType.INT64:
            return "int64"
        
        case hydra.core.IntegerType.UINT8:
            return "uint8"
        
        case hydra.core.IntegerType.UINT16:
            return "uint16"
        
        case hydra.core.IntegerType.UINT32:
            return "uint32"
        
        case hydra.core.IntegerType.UINT64:
            return "uint64"
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def literal_type(lt: hydra.core.LiteralType) -> str:
    r"""Show a literal type as a string."""
    
    match lt:
        case hydra.core.LiteralTypeBinary():
            return "binary"
        
        case hydra.core.LiteralTypeBoolean():
            return "boolean"
        
        case hydra.core.LiteralTypeFloat(value=ft):
            return float_type(ft)
        
        case hydra.core.LiteralTypeInteger(value=it):
            return integer_type(it)
        
        case hydra.core.LiteralTypeString():
            return "string"
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def field_type(ft: hydra.core.FieldType) -> str:
    @lru_cache(1)
    def fname() -> str:
        return ft.name.value
    @lru_cache(1)
    def ftyp() -> hydra.core.Type:
        return ft.type
    return hydra.lib.strings.cat((fname(), ":", type(ftyp())))

def type(typ: hydra.core.Type) -> str:
    r"""Show a type as a string."""
    
    def show_row_type(rt: hydra.core.RowType) -> str:
        @lru_cache(1)
        def flds() -> frozenlist[hydra.core.FieldType]:
            return rt.fields
        @lru_cache(1)
        def field_strs() -> frozenlist[str]:
            return hydra.lib.lists.map((lambda x1: field_type(x1)), flds())
        return hydra.lib.strings.cat(("{", hydra.lib.strings.intercalate(", ", field_strs()), "}"))
    def gather_types(prev: frozenlist[hydra.core.Type], app: hydra.core.ApplicationType) -> frozenlist[hydra.core.Type]:
        @lru_cache(1)
        def lhs() -> hydra.core.Type:
            return app.function
        @lru_cache(1)
        def rhs() -> hydra.core.Type:
            return app.argument
        match lhs():
            case hydra.core.TypeApplication(value=app2):
                return gather_types(hydra.lib.lists.cons(rhs(), prev), app2)
            
            case _:
                return hydra.lib.lists.cons(lhs(), hydra.lib.lists.cons(rhs(), prev))
    def gather_function_types(prev: frozenlist[hydra.core.Type], t: hydra.core.Type) -> frozenlist[hydra.core.Type]:
        match t:
            case hydra.core.TypeFunction(value=ft):
                @lru_cache(1)
                def dom() -> hydra.core.Type:
                    return ft.domain
                @lru_cache(1)
                def cod() -> hydra.core.Type:
                    return ft.codomain
                return gather_function_types(hydra.lib.lists.cons(dom(), prev), cod())
            
            case _:
                return hydra.lib.lists.reverse(hydra.lib.lists.cons(t, prev))
    match typ:
        case hydra.core.TypeAnnotated(value=at):
            return type(at.body)
        
        case hydra.core.TypeApplication(value=app):
            @lru_cache(1)
            def types() -> frozenlist[hydra.core.Type]:
                return gather_types((), app)
            @lru_cache(1)
            def type_strs() -> frozenlist[str]:
                return hydra.lib.lists.map((lambda x1: type(x1)), types())
            return hydra.lib.strings.cat(("(", hydra.lib.strings.intercalate(" @ ", type_strs()), ")"))
        
        case hydra.core.TypeEither(value=et):
            @lru_cache(1)
            def left_typ() -> hydra.core.Type:
                return et.left
            @lru_cache(1)
            def right_typ() -> hydra.core.Type:
                return et.right
            return hydra.lib.strings.cat(("either<", type(left_typ()), ", ", type(right_typ()), ">"))
        
        case hydra.core.TypeForall(value=ft):
            @lru_cache(1)
            def var() -> str:
                return ft.parameter.value
            @lru_cache(1)
            def body() -> hydra.core.Type:
                return ft.body
            return hydra.lib.strings.cat(("(∀", var(), ".", type(body()), ")"))
        
        case hydra.core.TypeFunction():
            @lru_cache(1)
            def types() -> frozenlist[hydra.core.Type]:
                return gather_function_types((), typ)
            @lru_cache(1)
            def type_strs() -> frozenlist[str]:
                return hydra.lib.lists.map((lambda x1: type(x1)), types())
            return hydra.lib.strings.cat(("(", hydra.lib.strings.intercalate(" → ", type_strs()), ")"))
        
        case hydra.core.TypeList(value=etyp):
            return hydra.lib.strings.cat(("list<", type(etyp), ">"))
        
        case hydra.core.TypeLiteral(value=lt):
            return literal_type(lt)
        
        case hydra.core.TypeMap(value=mt):
            @lru_cache(1)
            def key_typ() -> hydra.core.Type:
                return mt.keys
            @lru_cache(1)
            def val_typ() -> hydra.core.Type:
                return mt.values
            return hydra.lib.strings.cat(("map<", type(key_typ()), ", ", type(val_typ()), ">"))
        
        case hydra.core.TypeMaybe(value=etyp2):
            return hydra.lib.strings.cat(("maybe<", type(etyp2), ">"))
        
        case hydra.core.TypePair(value=pt):
            @lru_cache(1)
            def first_typ() -> hydra.core.Type:
                return pt.first
            @lru_cache(1)
            def second_typ() -> hydra.core.Type:
                return pt.second
            return hydra.lib.strings.cat(("(", type(first_typ()), ", ", type(second_typ()), ")"))
        
        case hydra.core.TypeRecord(value=rt):
            return hydra.lib.strings.cat2("record", show_row_type(rt))
        
        case hydra.core.TypeSet(value=etyp3):
            return hydra.lib.strings.cat(("set<", type(etyp3), ">"))
        
        case hydra.core.TypeUnion(value=rt2):
            return hydra.lib.strings.cat2("union", show_row_type(rt2))
        
        case hydra.core.TypeUnit():
            return "unit"
        
        case hydra.core.TypeVariable(value=name):
            return name.value
        
        case hydra.core.TypeWrap(value=wt):
            @lru_cache(1)
            def tname() -> str:
                return wt.type_name.value
            @lru_cache(1)
            def typ1() -> hydra.core.Type:
                return wt.body
            return hydra.lib.strings.cat(("wrap[", tname(), "](", type(typ1()), ")"))
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def float(fv: hydra.core.FloatValue) -> str:
    r"""Show a float value as a string."""
    
    match fv:
        case hydra.core.FloatValueBigfloat(value=v):
            return hydra.lib.strings.cat2(hydra.lib.literals.show_bigfloat(v), ":bigfloat")
        
        case hydra.core.FloatValueFloat32(value=v2):
            return hydra.lib.strings.cat2(hydra.lib.literals.show_float32(v2), ":float32")
        
        case hydra.core.FloatValueFloat64(value=v3):
            return hydra.lib.strings.cat2(hydra.lib.literals.show_float64(v3), ":float64")
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def integer(iv: hydra.core.IntegerValue) -> str:
    r"""Show an integer value as a string."""
    
    match iv:
        case hydra.core.IntegerValueBigint(value=v):
            return hydra.lib.strings.cat2(hydra.lib.literals.show_bigint(v), ":bigint")
        
        case hydra.core.IntegerValueInt8(value=v2):
            return hydra.lib.strings.cat2(hydra.lib.literals.show_int8(v2), ":int8")
        
        case hydra.core.IntegerValueInt16(value=v3):
            return hydra.lib.strings.cat2(hydra.lib.literals.show_int16(v3), ":int16")
        
        case hydra.core.IntegerValueInt32(value=v4):
            return hydra.lib.strings.cat2(hydra.lib.literals.show_int32(v4), ":int32")
        
        case hydra.core.IntegerValueInt64(value=v5):
            return hydra.lib.strings.cat2(hydra.lib.literals.show_int64(v5), ":int64")
        
        case hydra.core.IntegerValueUint8(value=v6):
            return hydra.lib.strings.cat2(hydra.lib.literals.show_uint8(v6), ":uint8")
        
        case hydra.core.IntegerValueUint16(value=v7):
            return hydra.lib.strings.cat2(hydra.lib.literals.show_uint16(v7), ":uint16")
        
        case hydra.core.IntegerValueUint32(value=v8):
            return hydra.lib.strings.cat2(hydra.lib.literals.show_uint32(v8), ":uint32")
        
        case hydra.core.IntegerValueUint64(value=v9):
            return hydra.lib.strings.cat2(hydra.lib.literals.show_uint64(v9), ":uint64")
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def literal(l: hydra.core.Literal) -> str:
    r"""Show a literal as a string."""
    
    match l:
        case hydra.core.LiteralBinary():
            return "[binary]"
        
        case hydra.core.LiteralBoolean(value=b):
            return hydra.lib.logic.if_else(b, (lambda : "true"), (lambda : "false"))
        
        case hydra.core.LiteralFloat(value=fv):
            return float(fv)
        
        case hydra.core.LiteralInteger(value=iv):
            return integer(iv)
        
        case hydra.core.LiteralString(value=s):
            return hydra.lib.literals.show_string(s)
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def type_scheme(ts: hydra.core.TypeScheme) -> str:
    r"""Show a type scheme as a string."""
    
    @lru_cache(1)
    def vars() -> frozenlist[hydra.core.Name]:
        return ts.variables
    @lru_cache(1)
    def body() -> hydra.core.Type:
        return ts.type
    @lru_cache(1)
    def var_names() -> frozenlist[str]:
        return hydra.lib.lists.map((lambda v1: v1.value), vars())
    @lru_cache(1)
    def fa() -> str:
        return hydra.lib.logic.if_else(hydra.lib.lists.null(vars()), (lambda : ""), (lambda : hydra.lib.strings.cat(("∀[", hydra.lib.strings.intercalate(",", var_names()), "]."))))
    return hydra.lib.strings.cat(("(", fa(), type(body()), ")"))

def binding(el: hydra.core.Binding) -> str:
    r"""Show a binding as a string."""
    
    @lru_cache(1)
    def name() -> str:
        return el.name.value
    @lru_cache(1)
    def t() -> hydra.core.Term:
        return el.term
    @lru_cache(1)
    def type_str() -> str:
        return hydra.lib.maybes.maybe("", (lambda ts: hydra.lib.strings.cat((":(", type_scheme(ts), ")"))), el.type)
    return hydra.lib.strings.cat((name(), type_str(), " = ", term(t())))

def elimination(elm: hydra.core.Elimination) -> str:
    r"""Show an elimination as a string."""
    
    match elm:
        case hydra.core.EliminationRecord(value=proj):
            @lru_cache(1)
            def tname() -> str:
                return proj.type_name.value
            @lru_cache(1)
            def fname() -> str:
                return proj.field.value
            return hydra.lib.strings.cat(("project(", tname(), "){", fname(), "}"))
        
        case hydra.core.EliminationUnion(value=cs):
            @lru_cache(1)
            def tname() -> str:
                return cs.type_name.value
            @lru_cache(1)
            def mdef() -> Maybe[hydra.core.Term]:
                return cs.default
            @lru_cache(1)
            def cases() -> frozenlist[hydra.core.Field]:
                return cs.cases
            @lru_cache(1)
            def default_field() -> frozenlist[hydra.core.Field]:
                return hydra.lib.maybes.maybe((), (lambda d: (hydra.core.Field(hydra.core.Name("[default]"), d),)), mdef())
            @lru_cache(1)
            def all_fields() -> frozenlist[hydra.core.Field]:
                return hydra.lib.lists.concat((cases(), default_field()))
            return hydra.lib.strings.cat(("case(", tname(), ")", fields(all_fields())))
        
        case hydra.core.EliminationWrap(value=tname):
            return hydra.lib.strings.cat(("unwrap(", tname.value, ")"))
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def field(field: hydra.core.Field) -> str:
    @lru_cache(1)
    def fname() -> str:
        return field.name.value
    @lru_cache(1)
    def fterm() -> hydra.core.Term:
        return field.term
    return hydra.lib.strings.cat((fname(), "=", term(fterm())))

def fields(flds: frozenlist[hydra.core.Field]) -> str:
    r"""Show a list of fields as a string."""
    
    @lru_cache(1)
    def field_strs() -> frozenlist[str]:
        return hydra.lib.lists.map((lambda x1: field(x1)), flds)
    return hydra.lib.strings.cat(("{", hydra.lib.strings.intercalate(", ", field_strs()), "}"))

def function(f: hydra.core.Function) -> str:
    r"""Show a function as a string."""
    
    match f:
        case hydra.core.FunctionElimination(value=v1):
            return elimination(v1)
        
        case hydra.core.FunctionLambda(value=v12):
            return lambda_(v12)
        
        case hydra.core.FunctionPrimitive(value=name):
            return hydra.lib.strings.cat2(name.value, "!")
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def injection(inj: hydra.core.Injection) -> str:
    r"""Show an injection as a string."""
    
    @lru_cache(1)
    def tname() -> hydra.core.Name:
        return inj.type_name
    @lru_cache(1)
    def f() -> hydra.core.Field:
        return inj.field
    return hydra.lib.strings.cat(("inject(", tname().value, ")", fields((f(),))))

def lambda_(l: hydra.core.Lambda) -> str:
    r"""Show a lambda as a string."""
    
    @lru_cache(1)
    def v() -> str:
        return l.parameter.value
    @lru_cache(1)
    def mt() -> Maybe[hydra.core.Type]:
        return l.domain
    @lru_cache(1)
    def body() -> hydra.core.Term:
        return l.body
    @lru_cache(1)
    def type_str() -> str:
        return hydra.lib.maybes.maybe("", (lambda t: hydra.lib.strings.cat2(":", type(t))), mt())
    return hydra.lib.strings.cat(("λ", v(), type_str(), ".", term(body())))

def let(l: hydra.core.Let) -> str:
    r"""Show a let expression as a string."""
    
    @lru_cache(1)
    def bindings() -> frozenlist[hydra.core.Binding]:
        return l.bindings
    @lru_cache(1)
    def env() -> hydra.core.Term:
        return l.body
    @lru_cache(1)
    def binding_strs() -> frozenlist[str]:
        return hydra.lib.lists.map((lambda x1: binding(x1)), bindings())
    return hydra.lib.strings.cat(("let ", hydra.lib.strings.intercalate(", ", binding_strs()), " in ", term(env())))

def term(t: hydra.core.Term) -> str:
    r"""Show a term as a string."""
    
    def gather_terms(prev: frozenlist[hydra.core.Term], app: hydra.core.Application) -> frozenlist[hydra.core.Term]:
        @lru_cache(1)
        def lhs() -> hydra.core.Term:
            return app.function
        @lru_cache(1)
        def rhs() -> hydra.core.Term:
            return app.argument
        match lhs():
            case hydra.core.TermApplication(value=app2):
                return gather_terms(hydra.lib.lists.cons(rhs(), prev), app2)
            
            case _:
                return hydra.lib.lists.cons(lhs(), hydra.lib.lists.cons(rhs(), prev))
    match t:
        case hydra.core.TermAnnotated(value=at):
            return term(at.body)
        
        case hydra.core.TermApplication(value=app):
            @lru_cache(1)
            def terms() -> frozenlist[hydra.core.Term]:
                return gather_terms((), app)
            @lru_cache(1)
            def term_strs() -> frozenlist[str]:
                return hydra.lib.lists.map((lambda x1: term(x1)), terms())
            return hydra.lib.strings.cat(("(", hydra.lib.strings.intercalate(" @ ", term_strs()), ")"))
        
        case hydra.core.TermEither(value=e):
            return hydra.lib.eithers.either((lambda l: hydra.lib.strings.cat(("left(", term(l), ")"))), (lambda r: hydra.lib.strings.cat(("right(", term(r), ")"))), e)
        
        case hydra.core.TermFunction(value=v1):
            return function(v1)
        
        case hydra.core.TermLet(value=l):
            return let(l)
        
        case hydra.core.TermList(value=els):
            @lru_cache(1)
            def term_strs() -> frozenlist[str]:
                return hydra.lib.lists.map((lambda x1: term(x1)), els)
            return hydra.lib.strings.cat(("[", hydra.lib.strings.intercalate(", ", term_strs()), "]"))
        
        case hydra.core.TermLiteral(value=lit):
            return literal(lit)
        
        case hydra.core.TermMap(value=m):
            def entry(p: tuple[hydra.core.Term, hydra.core.Term]) -> str:
                return hydra.lib.strings.cat((term(hydra.lib.pairs.first(p)), "=", term(hydra.lib.pairs.second(p))))
            return hydra.lib.strings.cat(("{", hydra.lib.strings.intercalate(", ", hydra.lib.lists.map((lambda x1: entry(x1)), hydra.lib.maps.to_list(m))), "}"))
        
        case hydra.core.TermMaybe(value=mt):
            return hydra.lib.maybes.maybe("nothing", (lambda t2: hydra.lib.strings.cat(("just(", term(t2), ")"))), mt)
        
        case hydra.core.TermPair(value=p):
            return hydra.lib.strings.cat(("(", term(hydra.lib.pairs.first(p)), ", ", term(hydra.lib.pairs.second(p)), ")"))
        
        case hydra.core.TermRecord(value=rec):
            @lru_cache(1)
            def tname() -> str:
                return rec.type_name.value
            @lru_cache(1)
            def flds() -> frozenlist[hydra.core.Field]:
                return rec.fields
            return hydra.lib.strings.cat(("record(", tname(), ")", fields(flds())))
        
        case hydra.core.TermSet(value=s):
            return hydra.lib.strings.cat(("{", hydra.lib.strings.intercalate(", ", hydra.lib.lists.map((lambda x1: term(x1)), hydra.lib.sets.to_list(s))), "}"))
        
        case hydra.core.TermTypeLambda(value=ta):
            @lru_cache(1)
            def param() -> str:
                return ta.parameter.value
            @lru_cache(1)
            def body() -> hydra.core.Term:
                return ta.body
            return hydra.lib.strings.cat(("Λ", param(), ".", term(body())))
        
        case hydra.core.TermTypeApplication(value=tt):
            @lru_cache(1)
            def t2() -> hydra.core.Term:
                return tt.body
            @lru_cache(1)
            def typ() -> hydra.core.Type:
                return tt.type
            return hydra.lib.strings.cat((term(t2()), "⟨", type(typ()), "⟩"))
        
        case hydra.core.TermUnion(value=v12):
            return injection(v12)
        
        case hydra.core.TermUnit():
            return "unit"
        
        case hydra.core.TermVariable(value=name):
            return name.value
        
        case hydra.core.TermWrap(value=wt):
            @lru_cache(1)
            def tname() -> str:
                return wt.type_name.value
            @lru_cache(1)
            def term1() -> hydra.core.Term:
                return wt.body
            return hydra.lib.strings.cat(("wrap(", tname(), "){", term(term1()), "}"))
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def list(f: Callable[[T0], str], xs: frozenlist[T0]) -> str:
    @lru_cache(1)
    def element_strs() -> frozenlist[str]:
        return hydra.lib.lists.map(f, xs)
    return hydra.lib.strings.cat(("[", hydra.lib.strings.intercalate(", ", element_strs()), "]"))

def read_term(s: str) -> Maybe[hydra.core.Term]:
    r"""A placeholder for reading terms from their serialized form. Not implemented."""
    
    return Just(cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(s)))))
