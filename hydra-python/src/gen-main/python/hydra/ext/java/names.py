# Note: this is an automatically generated file. Do not edit.

r"""Java naming constants and package name utilities."""

from __future__ import annotations
from functools import lru_cache
from hydra.dsl.python import Just, Maybe, frozenlist
import hydra.core
import hydra.ext.java.syntax
import hydra.lib.lists

accept_method_name = "accept"

apply_method_name = "apply"

compare_to_method_name = "compareTo"

equals_method_name = "equals"

get_method_name = "get"

hash_code_method_name = "hashCode"

def java_package_name(parts: frozenlist[str]) -> hydra.ext.java.syntax.PackageName:
    r"""Construct a Java package name from a list of string parts."""
    
    return hydra.ext.java.syntax.PackageName(hydra.lib.lists.map((lambda p: hydra.ext.java.syntax.Identifier(p)), parts))

@lru_cache(1)
def hydra_core_package_name() -> Maybe[hydra.ext.java.syntax.PackageName]:
    r"""The hydra.core package name."""
    
    return Just(java_package_name(("hydra", "core")))

@lru_cache(1)
def hydra_util_package_name() -> Maybe[hydra.ext.java.syntax.PackageName]:
    r"""The hydra.util package name."""
    
    return Just(java_package_name(("hydra", "util")))

instance_name = "instance"

@lru_cache(1)
def java_lang_package_name() -> Maybe[hydra.ext.java.syntax.PackageName]:
    r"""The java.lang package name."""
    
    return Just(java_package_name(("java", "lang")))

@lru_cache(1)
def java_util_function_package_name() -> Maybe[hydra.ext.java.syntax.PackageName]:
    r"""The java.util.function package name."""
    
    return Just(java_package_name(("java", "util", "function")))

@lru_cache(1)
def java_util_package_name() -> Maybe[hydra.ext.java.syntax.PackageName]:
    r"""The java.util package name."""
    
    return Just(java_package_name(("java", "util")))

other_instance_name = "other"

otherwise_method_name = "otherwise"

partial_visitor_name = "PartialVisitor"

set_method_name = "set"

value_field_name = "value"

visit_method_name = "visit"

visitor_name = "Visitor"

visitor_return_parameter = "R"
