# Note: this is an automatically generated file. Do not edit.

r"""Python test code generation codec for pytest-based generation tests."""

from __future__ import annotations
from collections.abc import Callable
from functools import lru_cache
from hydra.dsl.python import Either, Right, frozenlist
from typing import TypeVar, cast
import hydra.constants
import hydra.core
import hydra.lib.chars
import hydra.lib.eithers
import hydra.lib.equality
import hydra.lib.lists
import hydra.lib.logic
import hydra.lib.maybes
import hydra.lib.strings
import hydra.packaging
import hydra.testing

T0 = TypeVar("T0")
T1 = TypeVar("T1")

def build_python_test_module(test_module: T0, test_group: hydra.testing.TestGroup, test_body: str) -> str:
    r"""Build the complete Python test module content."""

    group_name_ = test_group.name
    @lru_cache(1)
    def header() -> str:
        return hydra.lib.strings.cat((hydra.lib.strings.cat2("# ", hydra.constants.warning_auto_generated_file), "\n", hydra.lib.strings.cat2("# ", group_name_), "\n\n"))
    return hydra.lib.strings.cat((header(), test_body, "\n"))

def format_python_test_name(name: str) -> str:
    r"""Format a test name for Python (snake_case with test_ prefix)."""

    return hydra.lib.strings.cat2("test_", hydra.lib.strings.from_list(hydra.lib.lists.map((lambda c: hydra.lib.logic.if_else(hydra.lib.chars.is_alpha_num(c), (lambda : hydra.lib.chars.to_lower(c)), (lambda : 95))), hydra.lib.strings.to_list(name))))

def generate_python_test_case(group_path: frozenlist[str], tcm: hydra.testing.TestCaseWithMetadata) -> Either[T0, frozenlist[str]]:
    r"""Generate a single pytest test case from a test case with metadata."""

    name_ = tcm.name
    tcase = tcm.case
    match tcase:
        case hydra.testing.TestCaseUniversal(value=ucase):
            actual_ = ucase.actual
            expected_ = ucase.expected
            @lru_cache(1)
            def full_name() -> str:
                return hydra.lib.logic.if_else(hydra.lib.lists.null(group_path), (lambda : name_), (lambda : hydra.lib.strings.intercalate("__", hydra.lib.lists.concat2(group_path, (name_,)))))
            @lru_cache(1)
            def formatted_name() -> str:
                return format_python_test_name(full_name())
            return Right((hydra.lib.strings.cat(("def ", formatted_name(), "():")), hydra.lib.strings.cat(("    assert (", actual_, ") == (", expected_, ")"))))

        case _:
            raise AssertionError("Unreachable: all variants handled")

def generate_python_test_group_hierarchy(group_path: frozenlist[str], test_group: hydra.testing.TestGroup) -> Either[T0, str]:
    r"""Generate test hierarchy for Python with nested subgroups."""

    cases_ = test_group.cases
    subgroups = test_group.subgroups
    return hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda tc: generate_python_test_case(group_path, tc)), cases_), (lambda test_case_lines: hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda subgroup: (group_name := subgroup.name, header := hydra.lib.strings.cat2("# ", group_name), hydra.lib.eithers.map((lambda content: hydra.lib.strings.cat((header, "\n\n", content))), generate_python_test_group_hierarchy(hydra.lib.lists.concat2(group_path, (group_name,)), subgroup)))[2]), subgroups), (lambda subgroup_blocks: (test_cases_str := hydra.lib.strings.intercalate("\n\n", hydra.lib.lists.concat(test_case_lines)), subgroups_str := hydra.lib.strings.intercalate("\n\n", subgroup_blocks), Right(hydra.lib.strings.cat((test_cases_str, hydra.lib.logic.if_else(hydra.lib.logic.or_(hydra.lib.equality.equal(test_cases_str, ""), hydra.lib.equality.equal(subgroups_str, "")), (lambda : ""), (lambda : "\n\n")), subgroups_str))))[2]))))

def generate_test_file_with_python_codec(test_module: hydra.packaging.Module, test_group: hydra.testing.TestGroup) -> Either[T0, tuple[str, str]]:
    r"""Generate a complete test file for Python."""

    return hydra.lib.eithers.map((lambda test_body: (test_module_content := build_python_test_module(test_module, test_group, test_body), ns_ := test_module.namespace, parts := hydra.lib.strings.split_on(".", ns_.value), dir_parts := hydra.lib.maybes.from_maybe((lambda : ()), hydra.lib.lists.maybe_init(parts)), file_name := hydra.lib.strings.cat(("test_", hydra.lib.maybes.from_maybe((lambda : ""), hydra.lib.lists.maybe_last(parts)), ".py")), file_path := hydra.lib.strings.cat((hydra.lib.strings.intercalate("/", dir_parts), "/", file_name)), (file_path, test_module_content))[6]), generate_python_test_group_hierarchy((), test_group))

def generate_python_test_file(test_module: hydra.packaging.Module, test_group: hydra.testing.TestGroup, _g: T0) -> Either[T1, tuple[str, str]]:
    r"""Generate a Python test file for a test group."""

    return generate_test_file_with_python_codec(test_module, test_group)
