# Note: this is an automatically generated file. Do not edit.

r"""Utility functions for constructing Scala AST nodes."""

from __future__ import annotations
from collections.abc import Callable
from functools import lru_cache
from hydra.dsl.python import Just, Maybe, Nothing, frozenlist
from typing import TypeVar, cast
import hydra.core
import hydra.formatting
import hydra.lib.equality
import hydra.lib.lists
import hydra.lib.logic
import hydra.lib.math
import hydra.lib.maybes
import hydra.lib.sets
import hydra.lib.strings
import hydra.names
import hydra.packaging
import hydra.scala.language
import hydra.scala.syntax
import hydra.strip

T0 = TypeVar("T0")

def name_of_type(cx: T0, t: hydra.core.Type) -> Maybe[hydra.core.Name]:
    r"""Extract the name from a type, if it is a named type."""

    while True:
        match hydra.strip.deannotate_type(t):
            case hydra.core.TypeVariable(value=name):
                return Just(name)

            case hydra.core.TypeForall(value=ft):
                cx = cx
                t = ft.body
                continue

            case _:
                return Nothing()

# Reference to scalaReservedWords from the language module.
scala_reserved_words = hydra.scala.language.scala_reserved_words()

def scala_escape_name(s: str) -> str:
    r"""Sanitize a name for Scala: escape reserved words, replace invalid characters."""

    @lru_cache(1)
    def sanitized() -> str:
        return hydra.lib.strings.from_list(hydra.lib.lists.map((lambda c: hydra.lib.logic.if_else(hydra.lib.equality.equal(c, 39), (lambda : 95), (lambda : c))), hydra.lib.strings.to_list(s)))
    @lru_cache(1)
    def sanitized2() -> str:
        return hydra.lib.logic.if_else(hydra.lib.equality.equal(sanitized(), "_"), (lambda : "_x"), (lambda : sanitized()))
    @lru_cache(1)
    def sanitized3() -> str:
        return hydra.lib.logic.if_else(hydra.lib.equality.equal(sanitized2(), "toString"), (lambda : "toString_"), (lambda : sanitized2()))
    @lru_cache(1)
    def needs_backticks() -> bool:
        return hydra.lib.logic.or_(hydra.lib.sets.member(sanitized3(), scala_reserved_words), hydra.lib.logic.and_(hydra.lib.equality.gt(hydra.lib.strings.length(sanitized3()), 0), hydra.lib.equality.equal(hydra.lib.maybes.from_maybe((lambda : 0), hydra.lib.strings.maybe_char_at(hydra.lib.math.sub(hydra.lib.strings.length(sanitized3()), 1), sanitized3())), 95)))
    return hydra.lib.logic.if_else(needs_backticks(), (lambda : hydra.lib.strings.cat(("`", sanitized3(), "`"))), (lambda : sanitized3()))

def scala_type_name(qualify: bool, name: hydra.core.Name) -> str:
    r"""Convert a Hydra name to a Scala type name."""

    return hydra.lib.logic.if_else(hydra.lib.logic.or_(qualify, hydra.lib.sets.member(hydra.names.local_name_of(name), scala_reserved_words)), (lambda : name.value), (lambda : hydra.names.local_name_of(name)))

def qualify_union_field_name(dlft: str, sname: Maybe[hydra.core.Name], fname: hydra.core.Name) -> str:
    r"""Qualify a union field name, optionally prefixing with the Scala type name."""

    return hydra.lib.strings.cat2(hydra.lib.maybes.maybe((lambda : dlft), (lambda n: hydra.lib.strings.cat2(scala_type_name(True, n), ".")), sname), scala_escape_name(fname.value))

def sapply(fun: hydra.scala.syntax.Data, args: frozenlist[hydra.scala.syntax.Data]) -> hydra.scala.syntax.Data:
    r"""Apply a Scala data expression to a list of arguments."""

    return cast(hydra.scala.syntax.Data, hydra.scala.syntax.DataApply(hydra.scala.syntax.Data_Apply(fun, args)))

def sname(s: str) -> hydra.scala.syntax.Data:
    r"""Create a Scala name reference."""

    return cast(hydra.scala.syntax.Data, hydra.scala.syntax.DataRef(cast(hydra.scala.syntax.Data_Ref, hydra.scala.syntax.Data_RefName(hydra.scala.syntax.Data_Name(hydra.scala.syntax.PredefString(s))))))

def type_to_string(t: hydra.scala.syntax.Type):
    def _hoist_hydra_scala_utils_type_to_string_1(v1):
        match v1:
            case hydra.scala.syntax.Type_RefName(value=tn):
                return tn.value

            case _:
                return "Any"
    def _hoist_hydra_scala_utils_type_to_string_2(v1):
        match v1:
            case hydra.scala.syntax.Type_FunctionTypeFunction(value=fn):
                @lru_cache(1)
                def params() -> frozenlist[str]:
                    return hydra.lib.lists.map((lambda x1: type_to_string(x1)), fn.params)
                @lru_cache(1)
                def res() -> str:
                    return type_to_string(fn.res)
                return hydra.lib.strings.cat(("(", hydra.lib.strings.intercalate(", ", params()), ") => ", res()))

            case _:
                return "Any"
    match t:
        case hydra.scala.syntax.TypeRef(value=tr):
            return _hoist_hydra_scala_utils_type_to_string_1(tr)

        case hydra.scala.syntax.TypeVar(value=tv):
            return tv.name.value

        case hydra.scala.syntax.TypeFunctionType(value=ft):
            return _hoist_hydra_scala_utils_type_to_string_2(ft)

        case hydra.scala.syntax.TypeApply(value=ta):
            @lru_cache(1)
            def base() -> str:
                return type_to_string(ta.tpe)
            @lru_cache(1)
            def arg_strs() -> frozenlist[str]:
                return hydra.lib.lists.map((lambda x1: type_to_string(x1)), ta.args)
            return hydra.lib.strings.cat((base(), "[", hydra.lib.strings.intercalate(", ", arg_strs()), "]"))

        case _:
            return "Any"

def sapply_types(fun: hydra.scala.syntax.Data, type_args: frozenlist[hydra.scala.syntax.Type]):
    r"""Apply explicit type parameters to a Scala expression (e.g. f[A, B])."""

    def type_to_str(t: hydra.scala.syntax.Type) -> str:
        return type_to_string(t)
    @lru_cache(1)
    def type_strings() -> frozenlist[str]:
        return hydra.lib.lists.map((lambda x1: type_to_str(x1)), type_args)
    @lru_cache(1)
    def type_arg_str() -> str:
        return hydra.lib.strings.cat(("[", hydra.lib.strings.intercalate(", ", type_strings()), "]"))
    def _hoist_type_to_str_body_1(v1):
        match v1:
            case hydra.scala.syntax.Data_RefName(value=dn):
                name_str = dn.value
                raw_name = name_str.value
                return sname(hydra.lib.strings.cat2(raw_name, type_arg_str()))

            case _:
                return fun
    match fun:
        case hydra.scala.syntax.DataRef(value=ref):
            return _hoist_type_to_str_body_1(ref)

        case _:
            return fun

def sassign(lhs: hydra.scala.syntax.Data, rhs: hydra.scala.syntax.Data) -> hydra.scala.syntax.Data:
    r"""Create a Scala assignment expression."""

    return cast(hydra.scala.syntax.Data, hydra.scala.syntax.DataAssign(hydra.scala.syntax.Data_Assign(lhs, rhs)))

def slambda(v: str, body: hydra.scala.syntax.Data, sdom: Maybe[hydra.scala.syntax.Type]) -> hydra.scala.syntax.Data:
    r"""Create a Scala lambda (function) expression."""

    return cast(hydra.scala.syntax.Data, hydra.scala.syntax.DataFunctionData(cast(hydra.scala.syntax.Data_FunctionData, hydra.scala.syntax.Data_FunctionDataFunction(hydra.scala.syntax.Data_Function((hydra.scala.syntax.Data_Param((), cast(hydra.scala.syntax.Name, hydra.scala.syntax.NameValue(v)), sdom, Nothing()),), body)))))

def sprim(name: hydra.core.Name) -> hydra.scala.syntax.Data:
    r"""Create a Scala primitive reference from a Hydra name."""

    @lru_cache(1)
    def qname() -> hydra.packaging.QualifiedName:
        return hydra.names.qualify_name(name)
    @lru_cache(1)
    def prefix() -> str:
        return hydra.lib.maybes.from_maybe((lambda : hydra.packaging.Namespace("")), qname().namespace).value
    @lru_cache(1)
    def local() -> str:
        return scala_escape_name(qname().local)
    return sname(hydra.lib.strings.cat2(hydra.lib.strings.cat2(prefix(), "."), local()))

def stapply(t: hydra.scala.syntax.Type, args: frozenlist[hydra.scala.syntax.Type]) -> hydra.scala.syntax.Type:
    r"""Apply a Scala type to a list of type arguments."""

    return cast(hydra.scala.syntax.Type, hydra.scala.syntax.TypeApply(hydra.scala.syntax.Type_Apply(t, args)))

def stapply1(t1: hydra.scala.syntax.Type, t2: hydra.scala.syntax.Type) -> hydra.scala.syntax.Type:
    r"""Apply a Scala type to one type argument."""

    return stapply(t1, (t2,))

def stapply2(t1: hydra.scala.syntax.Type, t2: hydra.scala.syntax.Type, t3: hydra.scala.syntax.Type) -> hydra.scala.syntax.Type:
    r"""Apply a Scala type to two type arguments."""

    return stapply(t1, (t2, t3))

def stparam(name: hydra.core.Name) -> hydra.scala.syntax.Type_Param:
    r"""Create a Scala type parameter from a Hydra name, capitalizing to avoid collision with value params."""

    @lru_cache(1)
    def v() -> str:
        return hydra.formatting.capitalize(name.value)
    return hydra.scala.syntax.Type_Param((), cast(hydra.scala.syntax.Name, hydra.scala.syntax.NameValue(v())), (), (), (), ())

def stref(s: str) -> hydra.scala.syntax.Type:
    r"""Create a Scala type reference by name."""

    return cast(hydra.scala.syntax.Type, hydra.scala.syntax.TypeRef(cast(hydra.scala.syntax.Type_Ref, hydra.scala.syntax.Type_RefName(hydra.scala.syntax.Type_Name(s)))))

def svar(name: hydra.core.Name) -> hydra.scala.syntax.Pat:
    r"""Create a Scala pattern variable."""

    v = name.value
    return cast(hydra.scala.syntax.Pat, hydra.scala.syntax.PatVar(hydra.scala.syntax.Pat_Var(hydra.scala.syntax.Data_Name(hydra.scala.syntax.PredefString(v)))))
