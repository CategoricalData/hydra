# Note: this is an automatically generated file. Do not edit.

r"""Functions for working with qualified names."""

from __future__ import annotations
from functools import lru_cache
from hydra.dsl.python import FrozenDict, Just, Maybe, Nothing, frozenlist
import hydra.core
import hydra.formatting
import hydra.lib.equality
import hydra.lib.lists
import hydra.lib.logic
import hydra.lib.maps
import hydra.lib.maybes
import hydra.lib.sets
import hydra.lib.strings
import hydra.module
import hydra.util

def qualify_name(name: hydra.core.Name) -> hydra.core.Type:
    r"""Split a dot-separated name into a namespace and local name."""
    
    @lru_cache(1)
    def parts() -> frozenlist[str]:
        return hydra.lib.lists.reverse(hydra.lib.strings.split_on(".", name.value))
    return hydra.lib.logic.if_else(hydra.lib.equality.equal(1, hydra.lib.lists.length(parts())), (lambda : hydra.module.QualifiedName(Nothing(), name.value)), (lambda : hydra.module.QualifiedName(Just(hydra.module.Namespace(hydra.lib.strings.intercalate(".", hydra.lib.lists.reverse(hydra.lib.lists.tail(parts()))))), hydra.lib.lists.head(parts()))))

def compact_name(namespaces: FrozenDict[hydra.module.Namespace, str], name: hydra.core.Name) -> str:
    r"""Given a mapping of namespaces to prefixes, convert a name to a compact string representation."""
    
    @lru_cache(1)
    def qual_name() -> hydra.core.Type:
        return qualify_name(name)
    @lru_cache(1)
    def mns() -> Maybe[hydra.module.Namespace]:
        return qual_name().namespace
    @lru_cache(1)
    def local() -> str:
        return qual_name().local
    return hydra.lib.maybes.maybe(name.value, (lambda ns: hydra.lib.maybes.maybe(local(), (lambda pre: hydra.lib.strings.cat((pre, ":", local()))), hydra.lib.maps.lookup(ns, namespaces))), mns())

def local_name_of(arg_: hydra.core.Name) -> str:
    r"""Extract the local part of a name."""
    
    return qualify_name(arg_).local

def namespace_of(arg_: hydra.core.Name) -> Maybe[hydra.module.Namespace]:
    r"""Extract the namespace of a name, if any."""
    
    return qualify_name(arg_).namespace

def namespace_to_file_path(case_conv: hydra.util.CaseConvention, ext: hydra.module.FileExtension, ns: hydra.module.Namespace) -> str:
    r"""Convert a namespace to a file path with the given case convention and file extension."""
    
    @lru_cache(1)
    def parts() -> frozenlist[str]:
        return hydra.lib.lists.map((lambda v1: hydra.formatting.convert_case(hydra.util.CaseConvention.CAMEL, case_conv, v1)), hydra.lib.strings.split_on(".", ns.value))
    return hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.intercalate("/", parts()), "."), ext.value)

def qname(ns: hydra.module.Namespace, name: str) -> hydra.core.Type:
    r"""Construct a qualified (dot-separated) name."""
    
    return hydra.core.Name(hydra.lib.strings.cat((ns.value, ".", name)))

def unique_label(visited: frozenset[str], l: str) -> str:
    r"""Generate a unique label by appending a suffix if the label is already in use."""
    
    return hydra.lib.logic.if_else(hydra.lib.sets.member(l, visited), (lambda : unique_label(visited, hydra.lib.strings.cat2(l, "'"))), (lambda : l))

def unqualify_name(qname: hydra.module.QualifiedName) -> hydra.core.Type:
    r"""Convert a qualified name to a dot-separated name."""
    
    @lru_cache(1)
    def prefix() -> str:
        return hydra.lib.maybes.maybe("", (lambda n: hydra.lib.strings.cat2(n.value, ".")), qname.namespace)
    return hydra.core.Name(hydra.lib.strings.cat2(prefix(), qname.local))
