# Note: this is an automatically generated file. Do not edit.

r"""Haskell test code generation for HSpec-based generation tests."""

from __future__ import annotations
from collections.abc import Callable
from functools import lru_cache
from hydra.dsl.python import Either, Nothing, Right, frozenlist
from typing import TypeVar, cast
import hydra.coders
import hydra.constants
import hydra.core
import hydra.decode.core
import hydra.dependencies
import hydra.formatting
import hydra.haskell.syntax
import hydra.haskell.utils
import hydra.lexical
import hydra.lib.eithers
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
import hydra.names
import hydra.packaging
import hydra.predicates
import hydra.rewriting
import hydra.show.errors
import hydra.strip
import hydra.testing
import hydra.util

T0 = TypeVar("T0")
T1 = TypeVar("T1")

def add_namespaces_to_namespaces(ns0: hydra.packaging.Namespaces[hydra.haskell.syntax.ModuleName], names: frozenset[hydra.core.Name]) -> hydra.packaging.Namespaces[hydra.haskell.syntax.ModuleName]:
    r"""Add namespaces from a set of names to existing namespaces."""

    @lru_cache(1)
    def new_namespaces() -> frozenset[hydra.packaging.Namespace]:
        return hydra.lib.sets.from_list(hydra.lib.maybes.cat(hydra.lib.lists.map((lambda x1: hydra.names.namespace_of(x1)), hydra.lib.sets.to_list(names))))
    def to_module_name(namespace: hydra.packaging.Namespace) -> hydra.haskell.syntax.ModuleName:
        return hydra.haskell.syntax.ModuleName(hydra.formatting.capitalize(hydra.lib.maybes.from_maybe((lambda : namespace.value), hydra.lib.lists.maybe_last(hydra.lib.strings.split_on(".", namespace.value)))))
    @lru_cache(1)
    def new_mappings() -> FrozenDict[hydra.packaging.Namespace, hydra.haskell.syntax.ModuleName]:
        return hydra.lib.maps.from_list(hydra.lib.lists.map((lambda ns_: (ns_, to_module_name(ns_))), hydra.lib.sets.to_list(new_namespaces())))
    return hydra.packaging.Namespaces(ns0.focus, hydra.lib.maps.union(ns0.mapping, new_mappings()))

def collect_test_cases(tg: hydra.testing.TestGroup) -> frozenlist[hydra.testing.TestCaseWithMetadata]:
    r"""Collect all test cases from a test group recursively."""

    return hydra.lib.lists.concat2(tg.cases, hydra.lib.lists.concat(hydra.lib.lists.map((lambda x1: collect_test_cases(x1)), tg.subgroups)))

def collect_names(graf: hydra.graph.Graph, names: frozenset[hydra.core.Name], t: hydra.core.Term) -> frozenset[hydra.core.Name]:
    r"""Collect variable names from encoded terms within a single term node."""

    return hydra.lib.logic.if_else(hydra.predicates.is_encoded_term(hydra.strip.deannotate_term(t)), (lambda : hydra.lib.eithers.either((lambda _: names), (lambda decoded_term: hydra.lib.sets.union(names, hydra.dependencies.term_dependency_names(True, True, True, decoded_term))), hydra.lib.eithers.bimap((lambda _e: _e), (lambda _a: _a), hydra.decode.core.term(graf, t)))), (lambda : names))

def extract_encoded_term_variable_names(graf: hydra.graph.Graph, term: hydra.core.Term) -> frozenset[hydra.core.Name]:
    r"""Extract all variable names from term-encoded terms in a given term."""

    return hydra.rewriting.fold_over_term(hydra.coders.TraversalOrder.PRE, (lambda v1, v2: collect_names(graf, v1, v2)), hydra.lib.sets.empty(), term)

def extract_test_terms(tcm: T0) -> frozenlist[T1]:
    r"""Extract input and output terms from a test case."""

    return ()

def build_namespaces_for_test_group(mod: hydra.packaging.Module, tgroup: hydra.testing.TestGroup, graph_: hydra.graph.Graph) -> Either[str, hydra.packaging.Namespaces[hydra.haskell.syntax.ModuleName]]:
    r"""Build namespaces for a test group including encoded term references."""

    @lru_cache(1)
    def test_cases_() -> frozenlist[hydra.testing.TestCaseWithMetadata]:
        return collect_test_cases(tgroup)
    @lru_cache(1)
    def test_terms() -> frozenlist[T0]:
        return hydra.lib.lists.concat(hydra.lib.lists.map((lambda x1: extract_test_terms(x1)), test_cases_()))
    @lru_cache(1)
    def test_bindings() -> frozenlist[hydra.core.Binding]:
        return hydra.lib.lists.map((lambda term: hydra.core.Binding(hydra.core.Name("_test_"), term, Nothing())), test_terms())
    @lru_cache(1)
    def temp_module() -> hydra.packaging.Module:
        return hydra.packaging.Module(mod.namespace, hydra.lib.lists.map((lambda b: cast(hydra.packaging.Definition, hydra.packaging.DefinitionTerm(hydra.packaging.TermDefinition(b.name, b.term, b.type)))), test_bindings()), mod.term_dependencies, mod.type_dependencies, mod.description)
    return hydra.lib.eithers.bind(hydra.lib.eithers.bimap((lambda e: hydra.show.errors.error(e)), (lambda a: a), hydra.haskell.utils.namespaces_for_module(temp_module(), hydra.lexical.empty_context(), graph_)), (lambda base_namespaces: (encoded_names := hydra.lib.sets.unions(hydra.lib.lists.map((lambda t: extract_encoded_term_variable_names(graph_, t)), test_terms())), Right(add_namespaces_to_namespaces(base_namespaces, encoded_names)))[1]))

def find_haskell_imports(namespaces: hydra.packaging.Namespaces[hydra.haskell.syntax.ModuleName], names_: T0) -> frozenlist[str]:
    r"""Find necessary imports for Haskell based on referenced names."""

    @lru_cache(1)
    def mapping_() -> FrozenDict[hydra.packaging.Namespace, hydra.haskell.syntax.ModuleName]:
        return namespaces.mapping
    @lru_cache(1)
    def filtered() -> FrozenDict[hydra.packaging.Namespace, hydra.haskell.syntax.ModuleName]:
        return hydra.lib.maps.filter_with_key((lambda ns_, _v: hydra.lib.logic.not_(hydra.lib.equality.equal(hydra.lib.maybes.from_maybe((lambda : ""), hydra.lib.lists.maybe_head(hydra.lib.strings.split_on("hydra.test.", ns_.value))), ""))), mapping_())
    return hydra.lib.lists.map((lambda entry: hydra.lib.strings.cat(("import qualified ", hydra.lib.strings.intercalate(".", hydra.lib.lists.map(hydra.formatting.capitalize, hydra.lib.strings.split_on(".", hydra.lib.pairs.first(entry).value))), " as ", hydra.lib.pairs.second(entry).value))), hydra.lib.maps.to_list(filtered()))

def namespace_to_module_name(ns_: hydra.packaging.Namespace) -> str:
    r"""Convert namespace to Haskell module name."""

    return hydra.lib.strings.intercalate(".", hydra.lib.lists.map(hydra.formatting.capitalize, hydra.lib.strings.split_on(".", ns_.value)))

def build_test_module(test_module: hydra.packaging.Module, test_group: hydra.testing.TestGroup, test_body: str, namespaces: hydra.packaging.Namespaces[hydra.haskell.syntax.ModuleName]) -> str:
    r"""Build the complete test module for Haskell HSpec."""

    ns_ = test_module.namespace
    @lru_cache(1)
    def spec_ns() -> hydra.packaging.Namespace:
        return hydra.packaging.Namespace(hydra.lib.strings.cat2(ns_.value, "Spec"))
    @lru_cache(1)
    def module_name_string() -> str:
        return namespace_to_module_name(spec_ns())
    group_name_ = test_group.name
    @lru_cache(1)
    def domain_imports() -> frozenlist[str]:
        return find_haskell_imports(namespaces, hydra.lib.sets.empty())
    standard_imports = ("import Hydra.Kernel", "import qualified Test.Hspec as H", "import qualified Data.List as L", "import qualified Data.Map as M", "import qualified Data.Set as S", "import qualified Data.Maybe as Y")
    @lru_cache(1)
    def all_imports() -> frozenlist[str]:
        return hydra.lib.lists.concat2(standard_imports, domain_imports())
    @lru_cache(1)
    def header() -> str:
        return hydra.lib.strings.intercalate("\n", hydra.lib.lists.concat(((hydra.lib.strings.cat2("-- ", hydra.constants.warning_auto_generated_file), ""), ("", hydra.lib.strings.cat(("module ", module_name_string(), " where")), ""), all_imports(), ("", "spec :: H.Spec", hydra.lib.strings.cat(("spec = H.describe ", hydra.lib.literals.show_string(group_name_), " $ do"))))))
    return hydra.lib.strings.cat((header(), "\n", test_body, "\n"))

def generate_test_case(depth: T0, tcm: hydra.testing.TestCaseWithMetadata) -> Either[T1, frozenlist[str]]:
    r"""Generate a single HSpec test case from a universal test case."""

    name_ = tcm.name
    tcase = tcm.case
    @lru_cache(1)
    def universal() -> hydra.testing.UniversalTestCase:
        match tcase:
            case hydra.testing.TestCaseUniversal(value=u):
                return u

            case _:
                raise AssertionError("Unreachable: all variants handled")
    actual_ = universal().actual
    expected_ = universal().expected
    return Right((hydra.lib.strings.cat(("H.it ", hydra.lib.literals.show_string(name_), " $ H.shouldBe")), hydra.lib.strings.cat(("  (", actual_, ")")), hydra.lib.strings.cat(("  (", expected_, ")"))))

def generate_test_group_hierarchy(depth: int, test_group: hydra.testing.TestGroup) -> Either[T0, str]:
    r"""Generate test hierarchy preserving the structure with H.describe blocks for subgroups."""

    cases_ = test_group.cases
    subgroups = test_group.subgroups
    @lru_cache(1)
    def indent() -> str:
        return hydra.lib.strings.from_list(hydra.lib.lists.replicate(hydra.lib.math.mul(depth, 2), 32))
    return hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda tc: generate_test_case(depth, tc)), cases_), (lambda test_case_lines_raw: (test_case_lines := hydra.lib.lists.map((lambda lines_: hydra.lib.lists.map((lambda line: hydra.lib.strings.cat2(indent(), line)), lines_)), test_case_lines_raw), test_cases_str := hydra.lib.strings.intercalate("\n", hydra.lib.lists.concat(test_case_lines)), hydra.lib.eithers.map((lambda subgroups_str: hydra.lib.strings.cat((test_cases_str, hydra.lib.logic.if_else(hydra.lib.logic.or_(hydra.lib.equality.equal(test_cases_str, ""), hydra.lib.equality.equal(subgroups_str, "")), (lambda : ""), (lambda : "\n")), subgroups_str))), hydra.lib.eithers.map((lambda blocks: hydra.lib.strings.intercalate("\n", blocks)), hydra.lib.eithers.map_list((lambda subgroup: (group_name_ := subgroup.name, hydra.lib.eithers.map((lambda content: hydra.lib.strings.cat((indent(), "H.describe ", hydra.lib.literals.show_string(group_name_), " $ do\n", content))), generate_test_group_hierarchy(hydra.lib.math.add(depth, 1), subgroup)))[1]), subgroups))))[2]))

def generate_test_file(test_module: hydra.packaging.Module, test_group: hydra.testing.TestGroup, namespaces: hydra.packaging.Namespaces[hydra.haskell.syntax.ModuleName]) -> Either[T0, tuple[str, str]]:
    r"""Generate a complete Haskell test file."""

    return hydra.lib.eithers.map((lambda test_body: (test_module_content := build_test_module(test_module, test_group, test_body, namespaces), ns_ := test_module.namespace, spec_ns := hydra.packaging.Namespace(hydra.lib.strings.cat2(ns_.value, "Spec")), file_path := hydra.names.namespace_to_file_path(hydra.util.CaseConvention.PASCAL, hydra.packaging.FileExtension("hs"), spec_ns), (file_path, test_module_content))[4]), generate_test_group_hierarchy(1, test_group))

def generate_haskell_test_file(test_module: hydra.packaging.Module, test_group: hydra.testing.TestGroup, g: hydra.graph.Graph) -> Either[str, tuple[str, str]]:
    r"""Generate a Haskell test file for a test group, with type inference and namespace building."""

    return hydra.lib.eithers.bind(build_namespaces_for_test_group(test_module, test_group, g), (lambda namespaces: generate_test_file(test_module, test_group, namespaces)))
