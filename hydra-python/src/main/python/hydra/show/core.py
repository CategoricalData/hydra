# Note: this is an automatically generated file. Do not edit.

r"""String representations of hydra.core types."""

from __future__ import annotations
from collections.abc import Callable
from hydra.dsl.python import Just, Maybe, frozenlist
from typing import Tuple, cast
import hydra.core
import hydra.lib.eithers
import hydra.lib.lists
import hydra.lib.literals
import hydra.lib.logic
import hydra.lib.maps
import hydra.lib.maybes
import hydra.lib.sets
import hydra.lib.strings

def float_type(ft: hydra.core.FloatType) -> str:
    r"""Show a float type as a string."""
    
    match ft:
        case hydra.core.FloatType.BIGFLOAT:
            return "bigfloat"
        
        case hydra.core.FloatType.FLOAT32:
            return "float32"
        
        case hydra.core.FloatType.FLOAT64:
            return "float64"

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

def type(typ: hydra.core.Type) -> str:
    r"""Show a type as a string."""
    
    def show_field_type(ft: hydra.core.FieldType) -> str:
        fname = ft.name.value
        ftyp = ft.type
        return hydra.lib.strings.cat((fname, ":", type(ftyp)))
    def show_row_type(rt: hydra.core.RowType) -> str:
        flds = rt.fields
        field_strs = hydra.lib.lists.map(show_field_type, flds)
        return hydra.lib.strings.cat(("{", hydra.lib.strings.intercalate(", ", field_strs), "}"))
    def gather_types(prev: frozenlist[hydra.core.Type], app: hydra.core.ApplicationType) -> frozenlist[hydra.core.Type]:
        lhs = app.function
        rhs = app.argument
        match lhs:
            case hydra.core.TypeApplication(value=app2):
                return gather_types(hydra.lib.lists.cons(rhs, prev), app2)
            
            case _:
                return hydra.lib.lists.cons(lhs, hydra.lib.lists.cons(rhs, prev))
    def gather_function_types(prev: frozenlist[hydra.core.Type], t: hydra.core.Type) -> frozenlist[hydra.core.Type]:
        match t:
            case hydra.core.TypeFunction(value=ft):
                dom = ft.domain
                cod = ft.codomain
                return gather_function_types(hydra.lib.lists.cons(dom, prev), cod)
            
            case _:
                return hydra.lib.lists.reverse(hydra.lib.lists.cons(t, prev))
    match typ:
        case hydra.core.TypeAnnotated(value=at):
            return type(at.body)
        
        case hydra.core.TypeApplication(value=app):
            types = gather_types(cast(frozenlist[hydra.core.Type], ()), app)
            type_strs = hydra.lib.lists.map(type, types)
            return hydra.lib.strings.cat(("(", hydra.lib.strings.intercalate(" @ ", type_strs), ")"))
        
        case hydra.core.TypeEither(value=et):
            left_typ = et.left
            right_typ = et.right
            return hydra.lib.strings.cat(("either<", type(left_typ), ", ", type(right_typ), ">"))
        
        case hydra.core.TypeForall(value=ft):
            var = ft.parameter.value
            body = ft.body
            return hydra.lib.strings.cat(("(∀", var, ".", type(body), ")"))
        
        case hydra.core.TypeFunction():
            types = gather_function_types(cast(frozenlist[hydra.core.Type], ()), typ)
            type_strs = hydra.lib.lists.map(type, types)
            return hydra.lib.strings.cat(("(", hydra.lib.strings.intercalate(" → ", type_strs), ")"))
        
        case hydra.core.TypeList(value=etyp):
            return hydra.lib.strings.cat(("list<", type(etyp), ">"))
        
        case hydra.core.TypeLiteral(value=lt):
            return literal_type(lt)
        
        case hydra.core.TypeMap(value=mt):
            key_typ = mt.keys
            val_typ = mt.values
            return hydra.lib.strings.cat(("map<", type(key_typ), ", ", type(val_typ), ">"))
        
        case hydra.core.TypeMaybe(value=etyp2):
            return hydra.lib.strings.cat(("maybe<", type(etyp2), ">"))
        
        case hydra.core.TypePair(value=pt):
            first_typ = pt.first
            second_typ = pt.second
            return hydra.lib.strings.cat(("(", type(first_typ), ", ", type(second_typ), ")"))
        
        case hydra.core.TypeProduct(value=types):
            type_strs = hydra.lib.lists.map(type, types)
            return hydra.lib.strings.intercalate("×", type_strs)
        
        case hydra.core.TypeRecord(value=rt):
            return hydra.lib.strings.cat2("record", show_row_type(rt))
        
        case hydra.core.TypeSet(value=etyp3):
            return hydra.lib.strings.cat(("set<", type(etyp3), ">"))
        
        case hydra.core.TypeSum(value=types2):
            type_strs = hydra.lib.lists.map(type, types2)
            return hydra.lib.strings.intercalate("+", type_strs)
        
        case hydra.core.TypeUnion(value=rt2):
            return hydra.lib.strings.cat2("union", show_row_type(rt2))
        
        case hydra.core.TypeUnit():
            return "unit"
        
        case hydra.core.TypeVariable(value=name):
            return name.value
        
        case hydra.core.TypeWrap(value=wt):
            tname = wt.type_name.value
            typ1 = wt.body
            return hydra.lib.strings.cat(("wrap[", tname, "](", type(typ1), ")"))

def float(fv: hydra.core.FloatValue) -> str:
    r"""Show a float value as a string."""
    
    match fv:
        case hydra.core.FloatValueBigfloat(value=v):
            return hydra.lib.strings.cat((hydra.lib.literals.show_bigfloat(v), ":bigfloat"))
        
        case hydra.core.FloatValueFloat32(value=v2):
            return hydra.lib.strings.cat((hydra.lib.literals.show_float32(v2), ":float32"))
        
        case hydra.core.FloatValueFloat64(value=v3):
            return hydra.lib.strings.cat((hydra.lib.literals.show_float64(v3), ":float64"))

def integer(iv: hydra.core.IntegerValue) -> str:
    r"""Show an integer value as a string."""
    
    match iv:
        case hydra.core.IntegerValueBigint(value=v):
            return hydra.lib.strings.cat((hydra.lib.literals.show_bigint(v), ":bigint"))
        
        case hydra.core.IntegerValueInt8(value=v2):
            return hydra.lib.strings.cat((hydra.lib.literals.show_int8(v2), ":int8"))
        
        case hydra.core.IntegerValueInt16(value=v3):
            return hydra.lib.strings.cat((hydra.lib.literals.show_int16(v3), ":int16"))
        
        case hydra.core.IntegerValueInt32(value=v4):
            return hydra.lib.strings.cat((hydra.lib.literals.show_int32(v4), ":int32"))
        
        case hydra.core.IntegerValueInt64(value=v5):
            return hydra.lib.strings.cat((hydra.lib.literals.show_int64(v5), ":int64"))
        
        case hydra.core.IntegerValueUint8(value=v6):
            return hydra.lib.strings.cat((hydra.lib.literals.show_uint8(v6), ":uint8"))
        
        case hydra.core.IntegerValueUint16(value=v7):
            return hydra.lib.strings.cat((hydra.lib.literals.show_uint16(v7), ":uint16"))
        
        case hydra.core.IntegerValueUint32(value=v8):
            return hydra.lib.strings.cat((hydra.lib.literals.show_uint32(v8), ":uint32"))
        
        case hydra.core.IntegerValueUint64(value=v9):
            return hydra.lib.strings.cat((hydra.lib.literals.show_uint64(v9), ":uint64"))

def literal(l: hydra.core.Literal) -> str:
    r"""Show a literal as a string."""
    
    match l:
        case hydra.core.LiteralBinary():
            return "[binary]"
        
        case hydra.core.LiteralBoolean(value=b):
            return hydra.lib.logic.if_else(b, "true", "false")
        
        case hydra.core.LiteralFloat(value=fv):
            return float(fv)
        
        case hydra.core.LiteralInteger(value=iv):
            return integer(iv)
        
        case hydra.core.LiteralString(value=s):
            return hydra.lib.literals.show_string(s)

def type_scheme(ts: hydra.core.TypeScheme) -> str:
    r"""Show a type scheme as a string."""
    
    vars = ts.variables
    body = ts.type
    var_names = hydra.lib.lists.map((lambda v1: v1.value), vars)
    fa = hydra.lib.logic.if_else(hydra.lib.lists.null(vars), "", hydra.lib.strings.cat(("∀[", hydra.lib.strings.intercalate(",", var_names), "].")))
    return hydra.lib.strings.cat(("(", fa, type(body), ")"))

def binding(el: hydra.core.Binding) -> str:
    r"""Show a binding as a string."""
    
    name = el.name.value
    t = el.term
    type_str = hydra.lib.maybes.maybe("", (lambda ts: hydra.lib.strings.cat((":(", type_scheme(ts), ")"))), el.type)
    return hydra.lib.strings.cat((name, type_str, " = ", term(t)))

def elimination(elm: hydra.core.Elimination) -> str:
    r"""Show an elimination as a string."""
    
    match elm:
        case hydra.core.EliminationProduct(value=tp):
            arity = tp.arity
            index = tp.index
            return hydra.lib.strings.cat(("[", hydra.lib.literals.show_int32(index), "/", hydra.lib.literals.show_int32(arity), "]"))
        
        case hydra.core.EliminationRecord(value=proj):
            tname = proj.type_name.value
            fname = proj.field.value
            return hydra.lib.strings.cat(("project(", tname, "){", fname, "}"))
        
        case hydra.core.EliminationUnion(value=cs):
            tname = cs.type_name.value
            mdef = cs.default
            cases = cs.cases
            default_field = hydra.lib.maybes.maybe(cast(frozenlist[hydra.core.Field], ()), (lambda d: (hydra.core.Field(hydra.core.Name("[default]"), d),)), mdef)
            all_fields = hydra.lib.lists.concat((cases, default_field))
            return hydra.lib.strings.cat(("case(", tname, ")", fields(all_fields)))
        
        case hydra.core.EliminationWrap(value=tname):
            return hydra.lib.strings.cat(("unwrap(", tname.value, ")"))

def fields(flds: frozenlist[hydra.core.Field]) -> str:
    r"""Show a list of fields as a string."""
    
    def show_field(field: hydra.core.Field) -> str:
        fname = field.name.value
        fterm = field.term
        return hydra.lib.strings.cat2(fname, hydra.lib.strings.cat2("=", term(fterm)))
    field_strs = hydra.lib.lists.map(show_field, flds)
    return hydra.lib.strings.cat(("{", hydra.lib.strings.intercalate(", ", field_strs), "}"))

def function(f: hydra.core.Function) -> str:
    r"""Show a function as a string."""
    
    match f:
        case hydra.core.FunctionElimination(value=v1):
            return elimination(v1)
        
        case hydra.core.FunctionLambda(value=v12):
            return lambda_(v12)
        
        case hydra.core.FunctionPrimitive(value=name):
            return hydra.lib.strings.cat2(name.value, "!")

def injection(inj: hydra.core.Injection) -> str:
    r"""Show an injection as a string."""
    
    tname = inj.type_name
    f = inj.field
    return hydra.lib.strings.cat(("inject(", tname.value, ")", fields((f,))))

def lambda_(l: hydra.core.Lambda) -> str:
    r"""Show a lambda as a string."""
    
    v = l.parameter.value
    mt = l.domain
    body = l.body
    type_str = hydra.lib.maybes.maybe("", (lambda t: hydra.lib.strings.cat2(":", type(t))), mt)
    return hydra.lib.strings.cat(("λ", v, type_str, ".", term(body)))

def term(t: hydra.core.Term) -> str:
    r"""Show a term as a string."""
    
    def gather_terms(prev: frozenlist[hydra.core.Term], app: hydra.core.Application) -> frozenlist[hydra.core.Term]:
        lhs = app.function
        rhs = app.argument
        match lhs:
            case hydra.core.TermApplication(value=app2):
                return gather_terms(hydra.lib.lists.cons(rhs, prev), app2)
            
            case _:
                return hydra.lib.lists.cons(lhs, hydra.lib.lists.cons(rhs, prev))
    match t:
        case hydra.core.TermAnnotated(value=at):
            return term(at.body)
        
        case hydra.core.TermApplication(value=app):
            terms = gather_terms(cast(frozenlist[hydra.core.Term], ()), app)
            term_strs = hydra.lib.lists.map(term, terms)
            return hydra.lib.strings.cat(("(", hydra.lib.strings.intercalate(" @ ", term_strs), ")"))
        
        case hydra.core.TermEither(value=e):
            return hydra.lib.eithers.either((lambda l: hydra.lib.strings.cat(("left(", term(l), ")"))), (lambda r: hydra.lib.strings.cat(("right(", term(r), ")"))), e)
        
        case hydra.core.TermFunction(value=v1):
            return function(v1)
        
        case hydra.core.TermLet(value=l):
            bindings = l.bindings
            env = l.body
            binding_strs = hydra.lib.lists.map(binding, bindings)
            return hydra.lib.strings.cat(("let ", hydra.lib.strings.intercalate(", ", binding_strs), " in ", term(env)))
        
        case hydra.core.TermList(value=els):
            term_strs = hydra.lib.lists.map(term, els)
            return hydra.lib.strings.cat(("[", hydra.lib.strings.intercalate(", ", term_strs), "]"))
        
        case hydra.core.TermLiteral(value=lit):
            return literal(lit)
        
        case hydra.core.TermMap(value=m):
            def entry(p: Tuple[hydra.core.Term, hydra.core.Term]) -> str:
                return hydra.lib.strings.cat((term(p[0]), "=", term(p[1])))
            return hydra.lib.strings.cat(("{", hydra.lib.strings.intercalate(", ", hydra.lib.lists.map(entry, hydra.lib.maps.to_list(m))), "}"))
        
        case hydra.core.TermMaybe(value=mt):
            return hydra.lib.maybes.maybe("nothing", (lambda t2: hydra.lib.strings.cat(("just(", term(t2), ")"))), mt)
        
        case hydra.core.TermPair(value=p):
            return hydra.lib.strings.cat(("(", term(p[0]), ", ", term(p[1]), ")"))
        
        case hydra.core.TermProduct(value=els2):
            term_strs = hydra.lib.lists.map(term, els2)
            return hydra.lib.strings.cat(("(", hydra.lib.strings.intercalate(", ", term_strs), ")"))
        
        case hydra.core.TermRecord(value=rec):
            tname = rec.type_name.value
            flds = rec.fields
            return hydra.lib.strings.cat(("record(", tname, ")", fields(flds)))
        
        case hydra.core.TermSet(value=s):
            return hydra.lib.strings.cat(("{", hydra.lib.strings.intercalate(", ", hydra.lib.lists.map(term, hydra.lib.sets.to_list(s))), "}"))
        
        case hydra.core.TermSum(value=s2):
            index = s2.index
            size = s2.size
            t2 = s2.term
            return hydra.lib.strings.cat(("(", hydra.lib.literals.show_int32(index), "/", hydra.lib.literals.show_int32(size), "=", term(t2), ")"))
        
        case hydra.core.TermTypeLambda(value=ta):
            param = ta.parameter.value
            body = ta.body
            return hydra.lib.strings.cat(("Λ", param, ".", term(body)))
        
        case hydra.core.TermTypeApplication(value=tt):
            t2 = tt.body
            typ = tt.type
            return hydra.lib.strings.cat((term(t2), "⟨", type(typ), "⟩"))
        
        case hydra.core.TermUnion(value=v12):
            return injection(v12)
        
        case hydra.core.TermUnit():
            return "unit"
        
        case hydra.core.TermVariable(value=name):
            return name.value
        
        case hydra.core.TermWrap(value=wt):
            tname = wt.type_name.value
            term1 = wt.body
            return hydra.lib.strings.cat(("wrap(", tname, "){", term(term1), "}"))

def list[T0](f: Callable[[T0], str], xs: frozenlist[T0]) -> str:
    element_strs = hydra.lib.lists.map(f, xs)
    return hydra.lib.strings.cat(("[", hydra.lib.strings.intercalate(", ", element_strs), "]"))

def read_term(s: str) -> Maybe[hydra.core.Term]:
    r"""A placeholder for reading terms from their serialized form. Not implemented."""
    
    return cast(Maybe[hydra.core.Term], Just(cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(s))))))
