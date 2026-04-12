# Note: this is an automatically generated file. Do not edit.

r"""Functions for working with qualified names."""

from __future__ import annotations
from collections.abc import Callable
from functools import lru_cache
from hydra.dsl.python import FrozenDict, Just, Maybe, Nothing, frozenlist
from typing import TypeVar, cast
import hydra.annotations
import hydra.constants
import hydra.core
import hydra.formatting
import hydra.lib.equality
import hydra.lib.lists
import hydra.lib.literals
import hydra.lib.logic
import hydra.lib.maps
import hydra.lib.math
import hydra.lib.maybes
import hydra.lib.pairs
import hydra.lib.sets
import hydra.lib.strings
import hydra.packaging
import hydra.util

T0 = TypeVar("T0")

def qualify_name(name: hydra.core.Name) -> hydra.packaging.QualifiedName:
    r"""Split a dot-separated name into a namespace and local name."""

    @lru_cache(1)
    def parts() -> frozenlist[str]:
        return hydra.lib.lists.reverse(hydra.lib.strings.split_on(".", name.value))
    return hydra.lib.logic.if_else(hydra.lib.equality.equal(1, hydra.lib.lists.length(parts())), (lambda : hydra.packaging.QualifiedName(Nothing(), name.value)), (lambda : hydra.packaging.QualifiedName(Just(hydra.packaging.Namespace(hydra.lib.strings.intercalate(".", hydra.lib.lists.reverse(hydra.lib.lists.tail(parts()))))), hydra.lib.lists.head(parts()))))

def compact_name(namespaces: FrozenDict[hydra.packaging.Namespace, str], name: hydra.core.Name) -> str:
    r"""Given a mapping of namespaces to prefixes, convert a name to a compact string representation."""

    @lru_cache(1)
    def qual_name() -> hydra.packaging.QualifiedName:
        return qualify_name(name)
    mns = qual_name().namespace
    local = qual_name().local
    return hydra.lib.maybes.maybe((lambda : name.value), (lambda ns: hydra.lib.maybes.maybe((lambda : local), (lambda pre: hydra.lib.strings.cat((pre, ":", local))), hydra.lib.maps.lookup(ns, namespaces))), mns)

def normal_type_variable(i: int) -> hydra.core.Name:
    r"""Type variable naming convention follows Haskell: t0, t1, etc."""

    return hydra.core.Name(hydra.lib.strings.cat2("t", hydra.lib.literals.show_int32(i)))

def fresh_name(cx: hydra.context.Context) -> tuple[hydra.core.Name, hydra.context.Context]:
    r"""Generate a fresh type variable name, threading Context."""

    @lru_cache(1)
    def count() -> int:
        return hydra.annotations.get_count(hydra.constants.key_fresh_type_variable_count, cx)
    return (normal_type_variable(count()), hydra.annotations.put_count(hydra.constants.key_fresh_type_variable_count, hydra.lib.math.add(count(), 1), cx))

def fresh_names(n: int, cx: hydra.context.Context) -> tuple[frozenlist[hydra.core.Name], hydra.context.Context]:
    r"""Generate multiple fresh type variable names, threading Context."""

    def go(acc: tuple[frozenlist[hydra.core.Name], hydra.context.Context], _: T0) -> tuple[frozenlist[hydra.core.Name], hydra.context.Context]:
        @lru_cache(1)
        def names() -> frozenlist[hydra.core.Name]:
            return hydra.lib.pairs.first(acc)
        @lru_cache(1)
        def cx0() -> hydra.context.Context:
            return hydra.lib.pairs.second(acc)
        @lru_cache(1)
        def result() -> tuple[hydra.core.Name, hydra.context.Context]:
            return fresh_name(cx0())
        @lru_cache(1)
        def name() -> hydra.core.Name:
            return hydra.lib.pairs.first(result())
        @lru_cache(1)
        def cx1() -> hydra.context.Context:
            return hydra.lib.pairs.second(result())
        return (hydra.lib.lists.concat2(names(), hydra.lib.lists.pure(name())), cx1())
    return hydra.lib.lists.foldl((lambda x1, x2: go(x1, x2)), ((), cx), hydra.lib.lists.replicate(n, None))

def local_name_of(arg_: hydra.core.Name) -> str:
    r"""Extract the local part of a name."""

    return qualify_name(arg_).local

def name_to_file_path(ns_conv: hydra.util.CaseConvention, local_conv: hydra.util.CaseConvention, ext: hydra.packaging.FileExtension, name: hydra.core.Name) -> str:
    r"""Convert a name to file path, given case conventions for namespaces and local names, and assuming '/' as the file path separator."""

    @lru_cache(1)
    def qual_name() -> hydra.packaging.QualifiedName:
        return qualify_name(name)
    ns = qual_name().namespace
    local = qual_name().local
    def ns_to_file_path(ns2: hydra.packaging.Namespace) -> str:
        return hydra.lib.strings.intercalate("/", hydra.lib.lists.map((lambda part: hydra.formatting.convert_case(hydra.util.CaseConvention.CAMEL, ns_conv, part)), hydra.lib.strings.split_on(".", ns2.value)))
    @lru_cache(1)
    def prefix() -> str:
        return hydra.lib.maybes.maybe((lambda : ""), (lambda n: hydra.lib.strings.cat2(ns_to_file_path(n), "/")), ns)
    @lru_cache(1)
    def suffix() -> str:
        return hydra.formatting.convert_case(hydra.util.CaseConvention.PASCAL, local_conv, local)
    return hydra.lib.strings.cat((prefix(), suffix(), ".", ext.value))

def namespace_of(arg_: hydra.core.Name) -> Maybe[hydra.packaging.Namespace]:
    r"""Extract the namespace of a name, if any."""

    return qualify_name(arg_).namespace

def namespace_to_file_path(case_conv: hydra.util.CaseConvention, ext: hydra.packaging.FileExtension, ns: hydra.packaging.Namespace) -> str:
    r"""Convert a namespace to a file path with the given case convention and file extension."""

    @lru_cache(1)
    def parts() -> frozenlist[str]:
        return hydra.lib.lists.map((lambda v1: hydra.formatting.convert_case(hydra.util.CaseConvention.CAMEL, case_conv, v1)), hydra.lib.strings.split_on(".", ns.value))
    return hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.intercalate("/", parts()), "."), ext.value)

def qname(ns: hydra.packaging.Namespace, name: str) -> hydra.core.Name:
    r"""Construct a qualified (dot-separated) name."""

    return hydra.core.Name(hydra.lib.strings.cat((ns.value, ".", name)))

def unique_label(visited: frozenset[str], l: str) -> str:
    r"""Generate a unique label by appending a suffix if the label is already in use."""

    return hydra.lib.logic.if_else(hydra.lib.sets.member(l, visited), (lambda : unique_label(visited, hydra.lib.strings.cat2(l, "'"))), (lambda : l))

def unqualify_name(qname: hydra.packaging.QualifiedName) -> hydra.core.Name:
    r"""Convert a qualified name to a dot-separated name."""

    @lru_cache(1)
    def prefix() -> str:
        return hydra.lib.maybes.maybe((lambda : ""), (lambda n: hydra.lib.strings.cat2(n.value, ".")), qname.namespace)
    return hydra.core.Name(hydra.lib.strings.cat2(prefix(), qname.local))
