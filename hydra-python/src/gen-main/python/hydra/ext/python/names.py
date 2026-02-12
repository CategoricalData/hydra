# Note: this is an automatically generated file. Do not edit.

r"""Python naming utilities: encoding Hydra names as Python names."""

from __future__ import annotations
from collections.abc import Callable
from functools import lru_cache
from hydra.dsl.python import Just, Nothing
from typing import TypeVar, cast
import hydra.core
import hydra.ext.python.helpers
import hydra.ext.python.language
import hydra.ext.python.serde
import hydra.ext.python.syntax
import hydra.formatting
import hydra.lib.equality
import hydra.lib.lists
import hydra.lib.logic
import hydra.lib.maps
import hydra.lib.maybes
import hydra.lib.pairs
import hydra.lib.strings
import hydra.module
import hydra.names
import hydra.util

T0 = TypeVar("T0")

def encode_constant_for_field_name(env: T0, tname: hydra.core.Name, fname: hydra.core.Name) -> hydra.ext.python.syntax.Name:
    return hydra.ext.python.syntax.Name(hydra.lib.strings.cat((hydra.formatting.convert_case(hydra.util.CaseConvention.PASCAL, hydra.util.CaseConvention.UPPER_SNAKE, hydra.names.local_name_of(tname)), "__", hydra.formatting.convert_case(hydra.util.CaseConvention.CAMEL, hydra.util.CaseConvention.UPPER_SNAKE, fname.value), "__NAME")))

def encode_constant_for_type_name(env: T0, tname: hydra.core.Name) -> hydra.ext.python.syntax.Name:
    return hydra.ext.python.syntax.Name(hydra.lib.strings.cat2(hydra.formatting.convert_case(hydra.util.CaseConvention.PASCAL, hydra.util.CaseConvention.UPPER_SNAKE, hydra.names.local_name_of(tname)), "__NAME"))

def sanitize_python_name(v1: str) -> str:
    r"""Sanitize a string to be a valid Python name."""
    
    return hydra.formatting.sanitize_with_underscores(hydra.ext.python.language.python_reserved_words(), v1)

# Whether to use __future__ annotations for forward references.
use_future_annotations = True

def encode_name(is_qualified: bool, conv: hydra.util.CaseConvention, env: hydra.ext.python.helpers.PythonEnvironment, name: hydra.core.Name) -> hydra.ext.python.syntax.Name:
    r"""Encode a Hydra name as a Python name."""
    
    @lru_cache(1)
    def namespaces() -> hydra.module.Namespaces[hydra.ext.python.syntax.DottedName]:
        return env.namespaces
    @lru_cache(1)
    def focus_pair() -> tuple[hydra.module.Namespace, hydra.ext.python.syntax.DottedName]:
        return namespaces().focus
    @lru_cache(1)
    def focus_ns() -> hydra.module.Namespace:
        return hydra.lib.pairs.first(focus_pair())
    @lru_cache(1)
    def bound_vars() -> FrozenDict[hydra.core.Name, hydra.ext.python.syntax.Name]:
        return hydra.lib.pairs.second(env.bound_type_variables)
    @lru_cache(1)
    def qual_name() -> hydra.module.QualifiedName:
        return hydra.names.qualify_name(name)
    @lru_cache(1)
    def mns() -> Maybe[hydra.module.Namespace]:
        return qual_name().namespace
    @lru_cache(1)
    def local() -> str:
        return qual_name().local
    @lru_cache(1)
    def py_local() -> str:
        return sanitize_python_name(hydra.formatting.convert_case(hydra.util.CaseConvention.CAMEL, conv, local()))
    def py_ns(ns_val: hydra.module.Namespace) -> str:
        return hydra.lib.strings.intercalate(".", hydra.lib.lists.map((lambda v1: hydra.formatting.convert_case(hydra.util.CaseConvention.CAMEL, hydra.util.CaseConvention.LOWER_SNAKE, v1)), hydra.lib.strings.split_on(".", ns_val.value)))
    return hydra.lib.logic.if_else(is_qualified, (lambda : hydra.lib.maybes.maybe(hydra.lib.logic.if_else(hydra.lib.equality.equal(mns(), Just(focus_ns())), (lambda : hydra.ext.python.syntax.Name(hydra.lib.logic.if_else(use_future_annotations, (lambda : py_local()), (lambda : hydra.ext.python.serde.escape_python_string(True, py_local()))))), (lambda : hydra.lib.maybes.maybe(hydra.ext.python.syntax.Name(py_local()), (lambda ns_val: hydra.ext.python.syntax.Name(hydra.lib.strings.cat2(py_ns(ns_val), hydra.lib.strings.cat2(".", py_local())))), mns()))), (lambda n: n), hydra.lib.maps.lookup(name, bound_vars()))), (lambda : hydra.ext.python.syntax.Name(py_local())))

def encode_enum_value(v1: hydra.ext.python.helpers.PythonEnvironment, v2: hydra.core.Name) -> hydra.ext.python.syntax.Name:
    r"""Encode a name as a Python enum value (UPPER_SNAKE case)."""
    
    return encode_name(False, hydra.util.CaseConvention.UPPER_SNAKE, v1, v2)

def encode_field_name(env: hydra.ext.python.helpers.PythonEnvironment, fname: hydra.core.Name) -> hydra.ext.python.syntax.Name:
    r"""Encode a name as a Python field name (lower_snake case)."""
    
    return encode_name(False, hydra.util.CaseConvention.LOWER_SNAKE, env, fname)

def encode_name_qualified(env: hydra.ext.python.helpers.PythonEnvironment, name: hydra.core.Name) -> hydra.ext.python.syntax.Name:
    r"""Encode a name as a fully qualified Python name."""
    
    @lru_cache(1)
    def namespaces() -> hydra.module.Namespaces[hydra.ext.python.syntax.DottedName]:
        return env.namespaces
    @lru_cache(1)
    def focus_pair() -> tuple[hydra.module.Namespace, hydra.ext.python.syntax.DottedName]:
        return namespaces().focus
    @lru_cache(1)
    def focus_ns() -> hydra.module.Namespace:
        return hydra.lib.pairs.first(focus_pair())
    @lru_cache(1)
    def bound_vars() -> FrozenDict[hydra.core.Name, hydra.ext.python.syntax.Name]:
        return hydra.lib.pairs.second(env.bound_type_variables)
    @lru_cache(1)
    def qual_name() -> hydra.module.QualifiedName:
        return hydra.names.qualify_name(name)
    @lru_cache(1)
    def mns() -> Maybe[hydra.module.Namespace]:
        return qual_name().namespace
    @lru_cache(1)
    def local() -> str:
        return qual_name().local
    return hydra.lib.maybes.maybe(hydra.lib.logic.if_else(hydra.lib.equality.equal(mns(), Just(focus_ns())), (lambda : hydra.ext.python.syntax.Name(hydra.lib.logic.if_else(use_future_annotations, (lambda : local()), (lambda : hydra.ext.python.serde.escape_python_string(True, local()))))), (lambda : hydra.ext.python.syntax.Name(hydra.lib.strings.intercalate(".", hydra.lib.lists.map(sanitize_python_name, hydra.lib.strings.split_on(".", name.value)))))), (lambda n: n), hydra.lib.maps.lookup(name, bound_vars()))

def encode_namespace(ns_val: hydra.module.Namespace) -> hydra.ext.python.syntax.DottedName:
    r"""Encode a namespace as a Python dotted name."""
    
    return hydra.ext.python.syntax.DottedName(hydra.lib.lists.map((lambda part: hydra.ext.python.syntax.Name(hydra.formatting.convert_case(hydra.util.CaseConvention.CAMEL, hydra.util.CaseConvention.LOWER_SNAKE, part))), hydra.lib.strings.split_on(".", ns_val.value)))

def encode_type_variable(name: hydra.core.Name) -> hydra.ext.python.syntax.Name:
    r"""Encode a type variable name (capitalized)."""
    
    return hydra.ext.python.syntax.Name(hydra.formatting.capitalize(name.value))

def variable_reference(conv: hydra.util.CaseConvention, quoted: bool, env: hydra.ext.python.helpers.PythonEnvironment, name: hydra.core.Name) -> hydra.ext.python.syntax.Expression:
    r"""Reference a variable as a Python expression."""
    
    @lru_cache(1)
    def py_name() -> hydra.ext.python.syntax.Name:
        return encode_name(True, conv, env, name)
    @lru_cache(1)
    def unquoted() -> hydra.ext.python.syntax.Expression:
        return cast(hydra.ext.python.syntax.Expression, hydra.ext.python.syntax.ExpressionSimple(hydra.ext.python.syntax.Disjunction((hydra.ext.python.syntax.Conjunction((cast(hydra.ext.python.syntax.Inversion, hydra.ext.python.syntax.InversionSimple(hydra.ext.python.syntax.Comparison(hydra.ext.python.syntax.BitwiseOr(Nothing(), hydra.ext.python.syntax.BitwiseXor(Nothing(), hydra.ext.python.syntax.BitwiseAnd(Nothing(), hydra.ext.python.syntax.ShiftExpression(Nothing(), hydra.ext.python.syntax.Sum(Nothing(), hydra.ext.python.syntax.Term(Nothing(), cast(hydra.ext.python.syntax.Factor, hydra.ext.python.syntax.FactorSimple(hydra.ext.python.syntax.Power(hydra.ext.python.syntax.AwaitPrimary(False, cast(hydra.ext.python.syntax.Primary, hydra.ext.python.syntax.PrimarySimple(cast(hydra.ext.python.syntax.Atom, hydra.ext.python.syntax.AtomName(py_name()))))), Nothing()))))))))), ()))),)),))))
    @lru_cache(1)
    def namespaces() -> hydra.module.Namespaces[hydra.ext.python.syntax.DottedName]:
        return env.namespaces
    @lru_cache(1)
    def focus_pair() -> tuple[hydra.module.Namespace, hydra.ext.python.syntax.DottedName]:
        return namespaces().focus
    @lru_cache(1)
    def focus_ns() -> hydra.module.Namespace:
        return hydra.lib.pairs.first(focus_pair())
    @lru_cache(1)
    def mns() -> Maybe[hydra.module.Namespace]:
        return hydra.names.namespace_of(name)
    @lru_cache(1)
    def same_namespace() -> bool:
        return hydra.lib.maybes.maybe(False, (lambda ns: hydra.lib.equality.equal(ns, focus_ns())), mns())
    return hydra.lib.logic.if_else(hydra.lib.logic.and_(quoted, same_namespace()), (lambda : cast(hydra.ext.python.syntax.Expression, hydra.ext.python.syntax.ExpressionSimple(hydra.ext.python.syntax.Disjunction((hydra.ext.python.syntax.Conjunction((cast(hydra.ext.python.syntax.Inversion, hydra.ext.python.syntax.InversionSimple(hydra.ext.python.syntax.Comparison(hydra.ext.python.syntax.BitwiseOr(Nothing(), hydra.ext.python.syntax.BitwiseXor(Nothing(), hydra.ext.python.syntax.BitwiseAnd(Nothing(), hydra.ext.python.syntax.ShiftExpression(Nothing(), hydra.ext.python.syntax.Sum(Nothing(), hydra.ext.python.syntax.Term(Nothing(), cast(hydra.ext.python.syntax.Factor, hydra.ext.python.syntax.FactorSimple(hydra.ext.python.syntax.Power(hydra.ext.python.syntax.AwaitPrimary(False, cast(hydra.ext.python.syntax.Primary, hydra.ext.python.syntax.PrimarySimple(cast(hydra.ext.python.syntax.Atom, hydra.ext.python.syntax.AtomString(hydra.ext.python.syntax.String(py_name().value, hydra.ext.python.syntax.QuoteStyle.DOUBLE)))))), Nothing()))))))))), ()))),)),))))), (lambda : unquoted()))

def term_variable_reference(v1: hydra.ext.python.helpers.PythonEnvironment, v2: hydra.core.Name) -> hydra.ext.python.syntax.Expression:
    r"""Reference a term variable as a Python expression."""
    
    return variable_reference(hydra.util.CaseConvention.LOWER_SNAKE, False, v1, v2)

def type_variable_reference(v1: hydra.ext.python.helpers.PythonEnvironment, v2: hydra.core.Name) -> hydra.ext.python.syntax.Expression:
    r"""Reference a type variable as a Python expression."""
    
    return variable_reference(hydra.util.CaseConvention.PASCAL, False, v1, v2)

def variant_name(is_qualified: bool, env: hydra.ext.python.helpers.PythonEnvironment, tname: hydra.core.Name, fname: hydra.core.Name) -> hydra.ext.python.syntax.Name:
    r"""Generate a variant name from type name and field name."""
    
    return encode_name(is_qualified, hydra.util.CaseConvention.PASCAL, env, hydra.core.Name(hydra.lib.strings.cat2(tname.value, hydra.formatting.capitalize(fname.value))))
