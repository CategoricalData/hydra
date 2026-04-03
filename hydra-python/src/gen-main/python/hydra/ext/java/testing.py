# Note: this is an automatically generated file. Do not edit.

r"""Java test code generation codec for JUnit-based generation tests."""

from __future__ import annotations
from collections.abc import Callable
from functools import lru_cache
from hydra.dsl.python import Either, Right, frozenlist
from typing import TypeVar, cast
import hydra.constants
import hydra.core
import hydra.formatting
import hydra.lib.eithers
import hydra.lib.equality
import hydra.lib.lists
import hydra.lib.logic
import hydra.lib.strings
import hydra.packaging
import hydra.testing
import hydra.util

T0 = TypeVar("T0")
T1 = TypeVar("T1")

def build_java_test_module(test_module: hydra.packaging.Module, test_group: hydra.testing.TestGroup, test_body: str) -> str:
    r"""Build the complete Java test module content."""

    ns_ = test_module.namespace
    parts = hydra.lib.strings.split_on(".", ns_.value)
    @lru_cache(1)
    def package_name() -> str:
        return hydra.lib.strings.intercalate(".", hydra.lib.lists.init(parts))
    @lru_cache(1)
    def class_name_() -> str:
        return hydra.lib.strings.cat2(hydra.formatting.capitalize(hydra.lib.lists.last(parts)), "Test")
    group_name_ = test_group.name
    standard_imports = ("import org.junit.jupiter.api.Test;", "import static org.junit.jupiter.api.Assertions.*;", "import java.util.*;", "import hydra.util.*;")
    @lru_cache(1)
    def header() -> str:
        return hydra.lib.strings.cat((hydra.lib.strings.cat2("// ", hydra.constants.warning_auto_generated_file), "\n", hydra.lib.strings.cat2("// ", group_name_), "\n\n", hydra.lib.strings.cat(("package ", package_name(), ";\n\n")), hydra.lib.strings.intercalate("\n", standard_imports), "\n\n", hydra.lib.strings.cat(("public class ", class_name_(), " {\n\n"))))
    return hydra.lib.strings.cat((header(), test_body, "\n}\n"))

# Standard imports for Java JUnit test files.
find_java_imports = ("import org.junit.jupiter.api.Test;", "import static org.junit.jupiter.api.Assertions.*;", "import java.util.*;")

def format_java_test_name(name: str) -> str:
    r"""Format a test name for Java (PascalCase method name with 'test' prefix)."""

    @lru_cache(1)
    def replaced() -> str:
        return hydra.lib.strings.intercalate(" Neg", hydra.lib.strings.split_on("-", hydra.lib.strings.intercalate("Dot", hydra.lib.strings.split_on(".", hydra.lib.strings.intercalate(" Plus", hydra.lib.strings.split_on("+", hydra.lib.strings.intercalate(" Div", hydra.lib.strings.split_on("/", hydra.lib.strings.intercalate(" Mul", hydra.lib.strings.split_on("*", hydra.lib.strings.intercalate(" Num", hydra.lib.strings.split_on("#", name))))))))))))
    @lru_cache(1)
    def sanitized() -> str:
        return hydra.formatting.non_alnum_to_underscores(replaced())
    @lru_cache(1)
    def pascal_() -> str:
        return hydra.formatting.convert_case(hydra.util.CaseConvention.LOWER_SNAKE, hydra.util.CaseConvention.PASCAL, sanitized())
    return hydra.lib.strings.cat2("test", pascal_())

def generate_java_test_case(group_path: frozenlist[str], tcm: hydra.testing.TestCaseWithMetadata) -> Either[T0, frozenlist[str]]:
    r"""Generate a single JUnit test case from a test case with metadata."""

    name_ = tcm.name
    tcase = tcm.case
    match tcase:
        case hydra.testing.TestCaseUniversal(value=ucase):
            actual_ = ucase.actual
            expected_ = ucase.expected
            @lru_cache(1)
            def full_name() -> str:
                return hydra.lib.logic.if_else(hydra.lib.lists.null(group_path), (lambda : name_), (lambda : hydra.lib.strings.intercalate("_", hydra.lib.lists.concat2(group_path, (name_,)))))
            @lru_cache(1)
            def formatted_name() -> str:
                return format_java_test_name(full_name())
            return Right(("    @Test", hydra.lib.strings.cat(("    public void ", formatted_name(), "() {")), "        assertEquals(", hydra.lib.strings.cat(("            ", expected_, ",")), hydra.lib.strings.cat(("            ", actual_, ");")), "    }"))

        case _:
            raise AssertionError("Unreachable: all variants handled")

def generate_java_test_group_hierarchy(group_path: frozenlist[str], test_group: hydra.testing.TestGroup) -> Either[T0, str]:
    r"""Generate test hierarchy for Java with nested subgroups."""

    cases_ = test_group.cases
    subgroups = test_group.subgroups
    return hydra.lib.eithers.bind(hydra.lib.eithers.map((lambda lines_: hydra.lib.strings.intercalate("\n\n", hydra.lib.lists.concat(lines_))), hydra.lib.eithers.map_list((lambda tc: generate_java_test_case(group_path, tc)), cases_)), (lambda test_cases_str: hydra.lib.eithers.map((lambda subgroups_str: hydra.lib.strings.cat((test_cases_str, hydra.lib.logic.if_else(hydra.lib.logic.or_(hydra.lib.equality.equal(test_cases_str, ""), hydra.lib.equality.equal(subgroups_str, "")), (lambda : ""), (lambda : "\n\n")), subgroups_str))), hydra.lib.eithers.map((lambda blocks: hydra.lib.strings.intercalate("\n\n", blocks)), hydra.lib.eithers.map_list((lambda subgroup: (group_name := subgroup.name, header := hydra.lib.strings.cat2("    // ", group_name), hydra.lib.eithers.map((lambda content: hydra.lib.strings.cat((header, "\n\n", content))), generate_java_test_group_hierarchy(hydra.lib.lists.concat2(group_path, (group_name,)), subgroup)))[2]), subgroups)))))

def generate_test_file_with_java_codec(test_module: hydra.packaging.Module, test_group: hydra.testing.TestGroup) -> Either[T0, tuple[str, str]]:
    r"""Generate a complete test file for Java."""

    return hydra.lib.eithers.map((lambda test_body: (test_module_content := build_java_test_module(test_module, test_group, test_body), ns_ := test_module.namespace, parts := hydra.lib.strings.split_on(".", ns_.value), dir_parts := hydra.lib.lists.drop(1, hydra.lib.lists.init(parts)), class_name_ := hydra.lib.strings.cat2(hydra.formatting.capitalize(hydra.lib.lists.last(parts)), "Test"), file_name := hydra.lib.strings.cat2(class_name_, ".java"), file_path := hydra.lib.strings.cat((hydra.lib.strings.intercalate("/", dir_parts), "/", file_name)), (file_path, test_module_content))[7]), generate_java_test_group_hierarchy((), test_group))

def generate_java_test_file(test_module: hydra.packaging.Module, test_group: hydra.testing.TestGroup, _g: T0) -> Either[T1, tuple[str, str]]:
    r"""Generate a Java test file for a test group."""

    return generate_test_file_with_java_codec(test_module, test_group)

def namespace_to_java_class_name(ns_: hydra.packaging.Namespace) -> str:
    r"""Convert namespace to Java class name."""

    return hydra.lib.strings.intercalate(".", hydra.lib.lists.map(hydra.formatting.capitalize, hydra.lib.strings.split_on(".", ns_.value)))
