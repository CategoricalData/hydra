# Note: this is an automatically generated file. Do not edit.

r"""Python test code generation codec for pytest-based generation tests."""

from __future__ import annotations
from collections.abc import Callable
from functools import lru_cache
from hydra.dsl.python import Either, Right, frozenlist
from typing import TypeVar, cast
import hydra.coders
import hydra.constants
import hydra.context
import hydra.core
import hydra.ext.python.coder
import hydra.ext.python.environment
import hydra.ext.python.names
import hydra.ext.python.serde
import hydra.ext.python.utils
import hydra.lexical
import hydra.lib.chars
import hydra.lib.eithers
import hydra.lib.equality
import hydra.lib.lists
import hydra.lib.logic
import hydra.lib.maps
import hydra.lib.maybes
import hydra.lib.pairs
import hydra.lib.sets
import hydra.lib.strings
import hydra.module
import hydra.serialization
import hydra.show.errors
import hydra.test.utils
import hydra.testing

T0 = TypeVar("T0")
T1 = TypeVar("T1")

def build_python_test_module(codec: hydra.testing.TestCodec, test_module: T0, test_group: hydra.testing.TestGroup, test_body: str, namespaces_: T1) -> str:
    r"""Build the complete Python test module content."""

    group_name_ = test_group.name
    @lru_cache(1)
    def domain_imports() -> frozenlist[str]:
        return codec.find_imports(hydra.lib.sets.empty())
    standard_imports = ("from __future__ import annotations", "from typing import cast", "from decimal import Decimal", "from hydra.dsl.python import FrozenDict, frozenlist, Either, Left, Right, Maybe, Just, Nothing")
    @lru_cache(1)
    def all_imports() -> frozenlist[str]:
        return hydra.lib.lists.concat2(standard_imports, domain_imports())
    @lru_cache(1)
    def header() -> str:
        return hydra.lib.strings.cat((hydra.lib.strings.cat2("# ", hydra.constants.warning_auto_generated_file), "\n", hydra.lib.strings.cat2("# ", group_name_), "\n\n", hydra.lib.strings.intercalate("\n", all_imports()), "\n\n"))
    return hydra.lib.strings.cat((header(), test_body, "\n"))

def empty_python_module_metadata(ns_: hydra.module.Namespace) -> hydra.ext.python.environment.PythonModuleMetadata:
    r"""Create an initial empty PythonModuleMetadata for a given namespace."""

    return hydra.ext.python.environment.PythonModuleMetadata(hydra.module.Namespaces((ns_, hydra.ext.python.names.encode_namespace(ns_)), hydra.lib.maps.empty()), hydra.lib.sets.empty(), False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False)

def find_python_imports(namespaces_: hydra.module.Namespaces[T0], names_: T1) -> frozenlist[str]:
    r"""Determine necessary imports for Python based on referenced namespaces."""

    @lru_cache(1)
    def mapping_() -> FrozenDict[hydra.module.Namespace, T0]:
        return namespaces_.mapping
    @lru_cache(1)
    def filtered() -> FrozenDict[hydra.module.Namespace, T0]:
        return hydra.lib.maps.filter_with_key((lambda ns_, _v: hydra.lib.logic.not_(hydra.lib.equality.equal(hydra.lib.lists.head(hydra.lib.strings.split_on("hydra.test.", ns_.value)), ""))), mapping_())
    return hydra.lib.lists.map((lambda entry: hydra.lib.strings.cat2("import ", hydra.lib.pairs.first(entry).value)), hydra.lib.maps.to_list(filtered()))

def format_python_test_name(name: str) -> str:
    r"""Format a test name for Python (snake_case with test_ prefix)."""

    return hydra.lib.strings.cat2("test_", hydra.lib.strings.from_list(hydra.lib.lists.map((lambda c: hydra.lib.logic.if_else(hydra.lib.chars.is_alpha_num(c), (lambda : hydra.lib.chars.to_lower(c)), (lambda : 95))), hydra.lib.strings.to_list(name))))

def generate_python_test_case(g: hydra.graph.Graph, namespaces_: T0, codec: hydra.testing.TestCodec, group_path: frozenlist[str], tcm: hydra.testing.TestCaseWithMetadata) -> Either[str, frozenlist[str]]:
    r"""Generate a single pytest test case from a test case with metadata."""

    name_ = tcm.name
    tcase = tcm.case
    match tcase:
        case hydra.testing.TestCaseDelegatedEvaluation(value=del_case):
            input_ = del_case.input
            output_ = del_case.output
            @lru_cache(1)
            def full_name() -> str:
                return hydra.lib.logic.if_else(hydra.lib.lists.null(group_path), (lambda : name_), (lambda : hydra.lib.strings.intercalate("__", hydra.lib.lists.concat2(group_path, (name_,)))))
            @lru_cache(1)
            def formatted_name() -> str:
                return codec.format_test_name(full_name())
            return hydra.lib.eithers.bind(codec.encode_term(input_, g), (lambda input_code: hydra.lib.eithers.bind(codec.encode_term(output_, g), (lambda output_code: Right((hydra.lib.strings.cat(("def ", formatted_name(), "():")), hydra.lib.strings.cat(("    assert (", input_code, ") == (", output_code, ")"))))))))

        case _:
            return Right(())

def generate_python_test_group_hierarchy(g: hydra.graph.Graph, namespaces_: T0, codec: hydra.testing.TestCodec, group_path: frozenlist[str], test_group: hydra.testing.TestGroup) -> Either[str, str]:
    r"""Generate test hierarchy for Python with nested subgroups."""

    cases_ = test_group.cases
    subgroups = test_group.subgroups
    return hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda tc: generate_python_test_case(g, namespaces_, codec, group_path, tc)), cases_), (lambda test_case_lines: hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda subgroup: (group_name := subgroup.name, header := hydra.lib.strings.cat2("# ", group_name), hydra.lib.eithers.map((lambda content: hydra.lib.strings.cat((header, "\n\n", content))), generate_python_test_group_hierarchy(g, namespaces_, codec, hydra.lib.lists.concat2(group_path, (group_name,)), subgroup)))[2]), subgroups), (lambda subgroup_blocks: (test_cases_str := hydra.lib.strings.intercalate("\n\n", hydra.lib.lists.concat(test_case_lines)), subgroups_str := hydra.lib.strings.intercalate("\n\n", subgroup_blocks), Right(hydra.lib.strings.cat((test_cases_str, hydra.lib.logic.if_else(hydra.lib.logic.or_(hydra.lib.equality.equal(test_cases_str, ""), hydra.lib.equality.equal(subgroups_str, "")), (lambda : ""), (lambda : "\n\n")), subgroups_str))))[2]))))

def generate_test_file_with_python_codec(codec: hydra.testing.TestCodec, test_module: hydra.module.Module, test_group: hydra.testing.TestGroup, namespaces_: T0, g: hydra.graph.Graph) -> Either[str, tuple[str, str]]:
    r"""Generate a complete test file using the Python codec."""

    return hydra.lib.eithers.map((lambda test_body: (test_module_content := build_python_test_module(codec, test_module, test_group, test_body, namespaces_), ns_ := test_module.namespace, parts := hydra.lib.strings.split_on(".", ns_.value), dir_parts := hydra.lib.lists.init(parts), file_name := hydra.lib.strings.cat(("test_", hydra.lib.lists.last(parts), ".py")), file_path := hydra.lib.strings.cat((hydra.lib.strings.intercalate("/", dir_parts), "/", file_name)), (file_path, test_module_content))[6]), generate_python_test_group_hierarchy(g, namespaces_, codec, (), test_group))

def namespaces_for_python_module(mod: hydra.module.Module, graph_: hydra.graph.Graph) -> Either[T0, hydra.module.Namespaces[hydra.ext.python.syntax.DottedName]]:
    r"""Build namespaces for a Python module, resolving all imports and primitives."""

    @lru_cache(1)
    def bindings() -> frozenlist[hydra.core.Binding]:
        return hydra.lexical.graph_to_bindings(graph_)
    @lru_cache(1)
    def defs() -> frozenlist[hydra.module.Definition]:
        return hydra.lib.maybes.map_maybe((lambda b: hydra.lib.maybes.map((lambda ts: cast(hydra.module.Definition, hydra.module.DefinitionTerm(hydra.module.TermDefinition(b.name, b.term, ts)))), b.type)), bindings())
    return Right(hydra.ext.python.utils.find_namespaces(mod.namespace, defs()))

def namespace_to_python_module_name(ns_: hydra.module.Namespace) -> str:
    r"""Convert namespace to Python module name (dot-separated lowercase)."""

    return ns_.value

# Template for Python import statements.
python_import_template = "import {namespace}"

# Template for Python test module structure.
python_module_template = hydra.lib.strings.intercalate("\n", (hydra.lib.strings.cat2("# ", hydra.constants.warning_auto_generated_file), "", "{imports}", "", "{testGroup}", "", "{testCases}"))

# Template for pytest test case assertions.
python_test_case_template = hydra.lib.strings.intercalate("\n", ("def {name}():", "    assert ({input}) == ({output})"))

# Template for pytest test group comments.
python_test_group_template = "# {groupName}"

def term_to_python_with_context(namespaces_: hydra.module.Namespaces[hydra.ext.python.syntax.DottedName], graph0: hydra.graph.Graph, skip_casts: bool, term: hydra.core.Term, _g: T0) -> Either[str, str]:
    r"""Convert a Hydra term to a Python expression string with a pre-built graph context."""

    return hydra.lib.eithers.bimap((lambda ic: hydra.show.errors.error(ic.object)), (lambda arg_: hydra.serialization.print_expr(hydra.ext.python.serde.encode_expression(arg_))), hydra.ext.python.coder.encode_term_inline(hydra.lexical.empty_context(), hydra.ext.python.environment.PythonEnvironment(namespaces_, ((), hydra.lib.maps.empty()), graph0, hydra.lib.sets.empty(), hydra.ext.python.utils.target_python_version, skip_casts, hydra.lib.sets.empty()), skip_casts, term))

def type_to_python(namespaces_: hydra.module.Namespaces[hydra.ext.python.syntax.DottedName], typ: hydra.core.Type, g: hydra.graph.Graph) -> Either[str, str]:
    r"""Convert a Hydra type to a Python type expression string."""

    return hydra.lib.eithers.bimap((lambda ic: hydra.show.errors.error(ic.object)), (lambda arg_: hydra.serialization.print_expr(hydra.ext.python.serde.encode_expression(arg_))), hydra.ext.python.coder.encode_type(hydra.ext.python.environment.PythonEnvironment(namespaces_, ((), hydra.lib.maps.empty()), g, hydra.lib.sets.empty(), hydra.ext.python.utils.target_python_version, False, hydra.lib.sets.empty()), typ))

def python_test_codec_with_context(namespaces_: hydra.module.Namespaces[hydra.ext.python.syntax.DottedName], tcontext: hydra.graph.Graph) -> hydra.testing.TestCodec:
    r"""Create an efficient Python TestCodec with a pre-built Graph, skipping casts for performance."""

    return hydra.testing.TestCodec(hydra.coders.LanguageName("python"), hydra.module.FileExtension("py"), (lambda v1, v2: term_to_python_with_context(namespaces_, tcontext, True, v1, v2)), (lambda v1, v2: type_to_python(namespaces_, v1, v2)), format_python_test_name, (lambda x1: namespace_to_python_module_name(x1)), python_test_case_template, python_test_group_template, python_module_template, python_import_template, (lambda v1: find_python_imports(namespaces_, v1)))

def generate_python_test_file(test_module: hydra.module.Module, test_group: hydra.testing.TestGroup, g: hydra.graph.Graph) -> Either[str, tuple[str, str]]:
    r"""Generate a Python test file for a test group, with type inference."""

    return hydra.lib.eithers.bind(hydra.test.utils.infer_test_group_terms(g, test_group), (lambda inferred_test_group: hydra.lib.eithers.bind(namespaces_for_python_module(test_module, g), (lambda namespaces_: generate_test_file_with_python_codec(python_test_codec_with_context(namespaces_, g), test_module, inferred_test_group, namespaces_, g)))))

def term_to_python(namespaces_: hydra.module.Namespaces[hydra.ext.python.syntax.DottedName], term: hydra.core.Term, g: hydra.graph.Graph) -> Either[str, str]:
    r"""Convert a Hydra term to a Python expression string."""

    return term_to_python_with_context(namespaces_, g, False, term, g)

def python_test_codec(namespaces_: hydra.module.Namespaces[hydra.ext.python.syntax.DottedName]) -> hydra.testing.TestCodec:
    r"""Create a Python TestCodec for pytest-based test generation."""

    return hydra.testing.TestCodec(hydra.coders.LanguageName("python"), hydra.module.FileExtension("py"), (lambda v1, v2: term_to_python(namespaces_, v1, v2)), (lambda v1, v2: type_to_python(namespaces_, v1, v2)), format_python_test_name, (lambda x1: namespace_to_python_module_name(x1)), python_test_case_template, python_test_group_template, python_module_template, python_import_template, (lambda v1: find_python_imports(namespaces_, v1)))
