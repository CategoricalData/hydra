# Note: this is an automatically generated file. Do not edit.

r"""C++ naming utilities: encoding Hydra names as C++ names."""

from __future__ import annotations
from collections.abc import Callable
from functools import lru_cache
from hydra.dsl.python import Just, frozenlist
from typing import cast
import hydra.core
import hydra.cpp.environment
import hydra.cpp.language
import hydra.cpp.syntax
import hydra.cpp.utils
import hydra.formatting
import hydra.lib.equality
import hydra.lib.lists
import hydra.lib.logic
import hydra.lib.maps
import hydra.lib.maybes
import hydra.lib.pairs
import hydra.lib.strings
import hydra.names
import hydra.packaging
import hydra.util

def sanitize_cpp_name(v1: str) -> str:
    r"""Sanitize a name to be valid in C++."""

    return hydra.formatting.sanitize_with_underscores(hydra.cpp.language.cpp_reserved_words(), v1)

def class_name(name: hydra.core.Name) -> str:
    r"""Get the C++ class name from a Hydra Name."""

    return sanitize_cpp_name(hydra.names.local_name_of(name))

def encode_name(is_qualified: bool, conv: hydra.util.CaseConvention, env: hydra.cpp.environment.CppEnvironment, name: hydra.core.Name) -> str:
    r"""Encode a name with specified convention."""

    @lru_cache(1)
    def focus_ns() -> hydra.packaging.Namespace:
        return hydra.lib.pairs.first(env.namespaces.focus)
    @lru_cache(1)
    def bound_vars() -> FrozenDict[hydra.core.Name, str]:
        return hydra.lib.pairs.second(env.bound_type_variables)
    @lru_cache(1)
    def qual_name() -> hydra.packaging.QualifiedName:
        return hydra.names.qualify_name(name)
    mns = qual_name().namespace
    local = qual_name().local
    @lru_cache(1)
    def cpp_local() -> str:
        return sanitize_cpp_name(hydra.formatting.convert_case(hydra.util.CaseConvention.CAMEL, conv, local))
    def cpp_ns(ns_val: hydra.packaging.Namespace) -> str:
        return hydra.lib.strings.intercalate("::", hydra.lib.lists.map((lambda v1: hydra.formatting.convert_case(hydra.util.CaseConvention.CAMEL, hydra.util.CaseConvention.LOWER_SNAKE, v1)), hydra.lib.strings.split_on(".", ns_val.value)))
    return hydra.lib.logic.if_else(is_qualified, (lambda : hydra.lib.maybes.maybe((lambda : hydra.lib.maybes.maybe((lambda : cpp_local()), (lambda ns_val: hydra.lib.strings.cat2(cpp_ns(ns_val), hydra.lib.strings.cat2("::", cpp_local()))), mns)), (lambda n: n), hydra.lib.maps.lookup(name, bound_vars()))), (lambda : cpp_local()))

def create_type_reference(is_pointer: bool, env: hydra.cpp.environment.CppEnvironment, name: hydra.core.Name) -> hydra.cpp.syntax.TypeExpression:
    r"""Create a type reference, optionally wrapped in shared_ptr."""

    @lru_cache(1)
    def base_type() -> hydra.cpp.syntax.TypeExpression:
        return cast(hydra.cpp.syntax.TypeExpression, hydra.cpp.syntax.TypeExpressionBasic(cast(hydra.cpp.syntax.BasicType, hydra.cpp.syntax.BasicTypeNamed(encode_name(True, hydra.util.CaseConvention.PASCAL, env, name)))))
    return hydra.lib.logic.if_else(is_pointer, (lambda : hydra.cpp.utils.to_const_type(base_type())), (lambda : base_type()))

def encode_enum_value(v1: hydra.cpp.environment.CppEnvironment, v2: hydra.core.Name) -> str:
    r"""Encode an enum value with appropriate naming convention."""

    return encode_name(False, hydra.util.CaseConvention.UPPER_SNAKE, v1, v2)

def encode_field_name(env: hydra.cpp.environment.CppEnvironment, fname: hydra.core.Name) -> str:
    r"""Encode a field name with appropriate naming convention."""

    return encode_name(False, hydra.util.CaseConvention.LOWER_SNAKE, env, fname)

def encode_name_qualified(env: hydra.cpp.environment.CppEnvironment, name: hydra.core.Name) -> str:
    r"""Encode a qualified name with namespace."""

    @lru_cache(1)
    def bound_vars() -> FrozenDict[hydra.core.Name, str]:
        return hydra.lib.pairs.second(env.bound_type_variables)
    @lru_cache(1)
    def focus_ns() -> hydra.packaging.Namespace:
        return hydra.lib.pairs.first(env.namespaces.focus)
    @lru_cache(1)
    def qual_name() -> hydra.packaging.QualifiedName:
        return hydra.names.qualify_name(name)
    mns = qual_name().namespace
    local = qual_name().local
    return hydra.lib.maybes.maybe((lambda : hydra.lib.logic.if_else(hydra.lib.equality.equal(mns, Just(focus_ns())), (lambda : sanitize_cpp_name(local)), (lambda : hydra.lib.strings.intercalate("::", hydra.lib.lists.map(sanitize_cpp_name, hydra.lib.strings.split_on(".", name.value)))))), (lambda n: n), hydra.lib.maps.lookup(name, bound_vars()))

def encode_namespace(ns_val: hydra.packaging.Namespace) -> str:
    r"""Encode a namespace as a C++ namespace string."""

    return hydra.lib.strings.intercalate("::", hydra.lib.lists.map((lambda v1: hydra.formatting.convert_case(hydra.util.CaseConvention.CAMEL, hydra.util.CaseConvention.LOWER_SNAKE, v1)), hydra.lib.strings.split_on(".", ns_val.value)))

def encode_type_variable(name: hydra.core.Name) -> str:
    r"""Encode a type variable name."""

    return hydra.formatting.capitalize(name.value)

def fwd_header_name(ns_val: hydra.packaging.Namespace) -> hydra.core.Name:
    r"""Get the forward header name for a namespace."""

    return hydra.names.unqualify_name(hydra.packaging.QualifiedName(Just(ns_val), "Fwd"))

def namespace_decl(ns_val: hydra.packaging.Namespace, decls: frozenlist[hydra.cpp.syntax.Declaration]) -> hydra.cpp.syntax.Declaration:
    r"""Create a namespace declaration wrapping inner declarations."""

    return cast(hydra.cpp.syntax.Declaration, hydra.cpp.syntax.DeclarationNamespace(hydra.cpp.syntax.NamespaceDeclaration(encode_namespace(ns_val), decls)))

def partial_visitor_name(name: hydra.core.Name) -> str:
    r"""Get the partial visitor name for a type."""

    return sanitize_cpp_name(hydra.lib.strings.cat2(hydra.names.local_name_of(name), "PartialVisitor"))

def variable_reference(conv: hydra.util.CaseConvention, env: hydra.cpp.environment.CppEnvironment, name: hydra.core.Name) -> hydra.cpp.syntax.Expression:
    r"""Create a variable reference expression."""

    return hydra.cpp.utils.create_identifier_expr(encode_name(True, conv, env, name))

def term_variable_reference(v1: hydra.cpp.environment.CppEnvironment, v2: hydra.core.Name) -> hydra.cpp.syntax.Expression:
    r"""Create a reference to a term variable."""

    return variable_reference(hydra.util.CaseConvention.LOWER_SNAKE, v1, v2)

def type_variable_reference(env: hydra.cpp.environment.CppEnvironment, name: hydra.core.Name) -> hydra.cpp.syntax.TypeExpression:
    r"""Create a reference to a type variable."""

    return cast(hydra.cpp.syntax.TypeExpression, hydra.cpp.syntax.TypeExpressionBasic(cast(hydra.cpp.syntax.BasicType, hydra.cpp.syntax.BasicTypeNamed(encode_name(True, hydra.util.CaseConvention.PASCAL, env, name)))))

def variant_name(tname: hydra.core.Name, fname: hydra.core.Name) -> str:
    r"""Get the variant name by combining type name and field name."""

    return sanitize_cpp_name(hydra.lib.strings.cat2(hydra.names.local_name_of(tname), hydra.formatting.capitalize(fname.value)))

def visitor_name(name: hydra.core.Name) -> str:
    r"""Get the visitor name for a type."""

    return sanitize_cpp_name(hydra.lib.strings.cat2(hydra.names.local_name_of(name), "Visitor"))
