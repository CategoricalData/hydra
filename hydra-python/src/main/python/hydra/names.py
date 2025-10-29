# Note: this is an automatically generated file. Do not edit.

r"""Functions for working with qualified names."""

from __future__ import annotations
from hydra.dsl.python import FrozenDict, Just, Maybe, Nothing
import hydra.core
import hydra.formatting
import hydra.lib.equality
import hydra.lib.lists
import hydra.lib.logic
import hydra.lib.maps
import hydra.lib.optionals
import hydra.lib.sets
import hydra.lib.strings
import hydra.mantle
import hydra.module

def qualify_name(name: hydra.core.Name) -> hydra.module.QualifiedName:
    parts = hydra.lib.lists.reverse(hydra.lib.strings.split_on(".", name.value))
    return hydra.lib.logic.if_else(hydra.lib.equality.equal(1, hydra.lib.lists.length(parts)), hydra.module.QualifiedName(Nothing(), name.value), hydra.module.QualifiedName(Just(hydra.module.Namespace(hydra.lib.strings.intercalate(".", hydra.lib.lists.reverse(hydra.lib.lists.tail(parts))))), hydra.lib.lists.head(parts)))

def compact_name(namespaces: FrozenDict[hydra.module.Namespace, str], name: hydra.core.Name) -> str:
    r"""Given a mapping of namespaces to prefixes, convert a name to a compact string representation."""
    
    qual_name = qualify_name(name)
    mns = qual_name.namespace
    local = qual_name.local
    return hydra.lib.optionals.maybe(name.value, (lambda ns: hydra.lib.optionals.maybe(local, (lambda pre: hydra.lib.strings.cat((pre, ":", local))), hydra.lib.maps.lookup(ns, namespaces))), mns)

def local_name_of(arg_: hydra.core.Name) -> str:
    return qualify_name(arg_).local

def namespace_of(arg_: hydra.core.Name) -> Maybe[hydra.module.Namespace]:
    return qualify_name(arg_).namespace

def namespace_to_file_path(case_conv: hydra.mantle.CaseConvention, ext: hydra.module.FileExtension, ns: hydra.module.Namespace) -> str:
    parts = hydra.lib.lists.map((lambda v1: hydra.formatting.convert_case(hydra.mantle.CaseConvention.CAMEL, case_conv, v1)), hydra.lib.strings.split_on(".", ns.value))
    return hydra.lib.strings.cat((hydra.lib.strings.cat((hydra.lib.strings.intercalate("/", parts), ".")), ext.value))

def qname(ns: hydra.module.Namespace, name: str) -> hydra.core.Name:
    r"""Construct a qualified (dot-separated) name."""
    
    return hydra.core.Name(hydra.lib.strings.cat((ns.value, ".", name)))

def unique_label(visited: frozenset[str], l: str) -> str:
    r"""Generate a unique label by appending a suffix if the label is already in use."""
    
    return hydra.lib.logic.if_else(hydra.lib.sets.member(l, visited), unique_label(visited, hydra.lib.strings.cat2(l, "'")), l)

def unqualify_name(qname: hydra.module.QualifiedName) -> hydra.core.Name:
    r"""Convert a qualified name to a dot-separated name."""
    
    prefix = hydra.lib.optionals.maybe("", (lambda n: hydra.lib.strings.cat((n.value, "."))), qname.namespace)
    return hydra.core.Name(hydra.lib.strings.cat((prefix, qname.local)))
