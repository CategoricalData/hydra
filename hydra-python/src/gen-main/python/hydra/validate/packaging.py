# Note: this is an automatically generated file. Do not edit.

r"""Validation functions for modules and packages."""

from __future__ import annotations
from functools import lru_cache
from hydra.dsl.python import FrozenDict, Just, Maybe, Nothing, frozenlist
from typing import cast
import hydra.core
import hydra.error.packaging
import hydra.formatting
import hydra.lib.equality
import hydra.lib.lists
import hydra.lib.logic
import hydra.lib.maps
import hydra.lib.maybes
import hydra.lib.pairs
import hydra.lib.sets
import hydra.lib.strings
import hydra.names
import hydra.packaging

def check_conflicting_module_namespaces(pkg: hydra.packaging.Package) -> Maybe[hydra.error.packaging.InvalidPackageError]:
    r"""Check for module namespaces that conflict when mapped to target language paths."""

    @lru_cache(1)
    def result() -> tuple[FrozenDict[str, hydra.packaging.Namespace], Maybe[hydra.error.packaging.InvalidPackageError]]:
        return hydra.lib.lists.foldl((lambda acc, mod: (seen := hydra.lib.pairs.first(acc), err := hydra.lib.pairs.second(acc), hydra.lib.maybes.cases(err, (lambda : (ns := mod.namespace, (key := hydra.lib.strings.to_lower(ns.value), (existing := hydra.lib.maps.lookup(key, seen), hydra.lib.maybes.cases(existing, (lambda : (hydra.lib.maps.insert(key, ns, seen), Nothing())), (lambda first: (seen, Just(cast(hydra.error.packaging.InvalidPackageError, hydra.error.packaging.InvalidPackageErrorConflictingModuleNamespace(hydra.error.packaging.ConflictingModuleNamespaceError(first, ns))))))))[1])[1])[1]), (lambda _: acc)))[2]), (hydra.lib.maps.empty(), Nothing()), pkg.modules)
    return hydra.lib.pairs.second(result())

def definition_name(def_: hydra.packaging.Definition) -> hydra.core.Name:
    r"""Extract the name from a definition."""

    match def_:
        case hydra.packaging.DefinitionTerm(value=td):
            return td.name

        case hydra.packaging.DefinitionType(value=td2):
            return td2.name

        case _:
            raise AssertionError("Unreachable: all variants handled")

def check_conflicting_variant_names(mod: hydra.packaging.Module):
    r"""Check for union variant names that, when mapped to constructor names, conflict with other type definitions."""

    ns = mod.namespace
    defs = mod.definitions
    @lru_cache(1)
    def def_names() -> frozenset[str]:
        return hydra.lib.lists.foldl((lambda acc, def_: hydra.lib.sets.insert(hydra.names.local_name_of(definition_name(def_)), acc)), hydra.lib.sets.empty(), defs)
    def _hoist_def_names_body_1(v1):
        match v1:
            case hydra.packaging.DefinitionType(value=td):
                type_name = td.name
                @lru_cache(1)
                def local_type_name() -> str:
                    return hydra.names.local_name_of(type_name)
                typ = td.type.type
                def _hoist_typ_body_1(v12):
                    match v12:
                        case hydra.core.TypeUnion(value=fields):
                            return hydra.lib.lists.foldl((lambda inner_acc, field: hydra.lib.maybes.cases(inner_acc, (lambda : (field_name := field.name, (local_field_name := hydra.names.local_name_of(field_name), (constructor_name := hydra.lib.strings.cat2(hydra.formatting.capitalize(local_type_name()), hydra.formatting.capitalize(local_field_name)), hydra.lib.logic.if_else(hydra.lib.sets.member(constructor_name, def_names()), (lambda : Just(cast(hydra.error.packaging.InvalidModuleError, hydra.error.packaging.InvalidModuleErrorConflictingVariantName(hydra.error.packaging.ConflictingVariantNameError(ns, type_name, field_name, hydra.core.Name(constructor_name)))))), (lambda : Nothing())))[1])[1])[1]), (lambda _: inner_acc))), Nothing(), fields)

                        case _:
                            return Nothing()
                return _hoist_typ_body_1(typ)

            case _:
                return Nothing()
    return hydra.lib.lists.foldl((lambda acc, def_: hydra.lib.maybes.cases(acc, (lambda : _hoist_def_names_body_1(def_)), (lambda _: acc))), Nothing(), defs)

def check_definition_namespaces(mod: hydra.packaging.Module) -> Maybe[hydra.error.packaging.InvalidModuleError]:
    r"""Check that all definition names in a module have the module's namespace as a prefix."""

    ns = mod.namespace
    prefix = hydra.lib.strings.cat2(ns.value, ".")
    prefix_len = hydra.lib.strings.length(prefix)
    return hydra.lib.lists.foldl((lambda acc, def_: hydra.lib.maybes.cases(acc, (lambda : (name := definition_name(def_), (name_str := name.value, (name_prefix := hydra.lib.lists.take(prefix_len, hydra.lib.strings.to_list(name_str)), hydra.lib.logic.if_else(hydra.lib.equality.equal(hydra.lib.strings.from_list(name_prefix), prefix), (lambda : Nothing()), (lambda : Just(cast(hydra.error.packaging.InvalidModuleError, hydra.error.packaging.InvalidModuleErrorDefinitionNotInModuleNamespace(hydra.error.packaging.DefinitionNotInModuleNamespaceError(ns, name)))))))[1])[1])[1]), (lambda _: acc))), Nothing(), mod.definitions)

def check_duplicate_definition_names(mod: hydra.packaging.Module) -> Maybe[hydra.error.packaging.InvalidModuleError]:
    r"""Check for duplicate definition names in a module."""

    ns = mod.namespace
    @lru_cache(1)
    def result() -> tuple[frozenset[hydra.core.Name], Maybe[hydra.error.packaging.InvalidModuleError]]:
        return hydra.lib.lists.foldl((lambda acc, def_: (seen := hydra.lib.pairs.first(acc), err := hydra.lib.pairs.second(acc), hydra.lib.maybes.cases(err, (lambda : (name := definition_name(def_), hydra.lib.logic.if_else(hydra.lib.sets.member(name, seen), (lambda : (seen, Just(cast(hydra.error.packaging.InvalidModuleError, hydra.error.packaging.InvalidModuleErrorDuplicateDefinitionName(hydra.error.packaging.DuplicateDefinitionNameError(ns, name)))))), (lambda : (hydra.lib.sets.insert(name, seen), Nothing()))))[1]), (lambda _: acc)))[2]), (hydra.lib.sets.empty(), Nothing()), mod.definitions)
    return hydra.lib.pairs.second(result())

def check_duplicate_module_namespaces(pkg: hydra.packaging.Package) -> Maybe[hydra.error.packaging.InvalidPackageError]:
    r"""Check for duplicate module namespaces in a package."""

    @lru_cache(1)
    def result() -> tuple[frozenset[hydra.packaging.Namespace], Maybe[hydra.error.packaging.InvalidPackageError]]:
        return hydra.lib.lists.foldl((lambda acc, mod: (seen := hydra.lib.pairs.first(acc), err := hydra.lib.pairs.second(acc), hydra.lib.maybes.cases(err, (lambda : (ns := mod.namespace, hydra.lib.logic.if_else(hydra.lib.sets.member(ns, seen), (lambda : (seen, Just(cast(hydra.error.packaging.InvalidPackageError, hydra.error.packaging.InvalidPackageErrorDuplicateModuleNamespace(hydra.error.packaging.DuplicateModuleNamespaceError(ns)))))), (lambda : (hydra.lib.sets.insert(ns, seen), Nothing()))))[1]), (lambda _: acc)))[2]), (hydra.lib.sets.empty(), Nothing()), pkg.modules)
    return hydra.lib.pairs.second(result())

def module(mod: hydra.packaging.Module) -> Maybe[hydra.error.packaging.InvalidModuleError]:
    r"""Validate a module, returning the first error found or nothing if valid."""

    @lru_cache(1)
    def r1() -> Maybe[hydra.error.packaging.InvalidModuleError]:
        return check_definition_namespaces(mod)
    return hydra.lib.maybes.cases(r1(), (lambda : (r2 := check_duplicate_definition_names(mod), hydra.lib.maybes.cases(r2, (lambda : check_conflicting_variant_names(mod)), (lambda _: r2)))[1]), (lambda _: r1()))

def package(pkg: hydra.packaging.Package) -> Maybe[hydra.error.packaging.InvalidPackageError]:
    r"""Validate a package, returning the first error found or nothing if valid."""

    @lru_cache(1)
    def r1() -> Maybe[hydra.error.packaging.InvalidPackageError]:
        return check_duplicate_module_namespaces(pkg)
    return hydra.lib.maybes.cases(r1(), (lambda : (r2 := check_conflicting_module_namespaces(pkg), hydra.lib.maybes.cases(r2, (lambda : hydra.lib.lists.foldl((lambda acc, mod: hydra.lib.maybes.cases(acc, (lambda : hydra.lib.maybes.map((lambda err: cast(hydra.error.packaging.InvalidPackageError, hydra.error.packaging.InvalidPackageErrorInvalidModule(err))), module(mod))), (lambda _: acc))), Nothing(), pkg.modules)), (lambda _: r2)))[1]), (lambda _: r1()))
