# Note: this is an automatically generated file. Do not edit.

r"""Haskell test code generation codec for HSpec-based generation tests."""

from __future__ import annotations
from collections.abc import Callable
from functools import lru_cache
from hydra.dsl.python import Either, FrozenDict, Just, Maybe, Nothing, Right, frozenlist
from typing import TypeVar, cast
import hydra.coders
import hydra.constants
import hydra.context
import hydra.core
import hydra.decode.core
import hydra.ext.haskell.coder
import hydra.ext.haskell.serde
import hydra.ext.haskell.syntax
import hydra.ext.haskell.utils
import hydra.formatting
import hydra.graph
import hydra.inference
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
import hydra.module
import hydra.names
import hydra.rewriting
import hydra.schemas
import hydra.serialization
import hydra.show.errors
import hydra.substitution
import hydra.testing
import hydra.typing
import hydra.util

T0 = TypeVar("T0")

def add_namespaces_to_namespaces(ns0: hydra.module.Namespaces[hydra.ext.haskell.syntax.ModuleName], names: frozenset[hydra.core.Name]) -> hydra.module.Namespaces[hydra.ext.haskell.syntax.ModuleName]:
    r"""Add namespaces from a set of names to existing namespaces."""

    @lru_cache(1)
    def new_namespaces() -> frozenset[hydra.module.Namespace]:
        return hydra.lib.sets.from_list(hydra.lib.maybes.cat(hydra.lib.lists.map((lambda x1: hydra.names.namespace_of(x1)), hydra.lib.sets.to_list(names))))
    def to_module_name(namespace: hydra.module.Namespace) -> hydra.ext.haskell.syntax.ModuleName:
        return hydra.ext.haskell.syntax.ModuleName(hydra.formatting.capitalize(hydra.lib.lists.last(hydra.lib.strings.split_on(".", namespace.value))))
    @lru_cache(1)
    def new_mappings() -> FrozenDict[hydra.module.Namespace, hydra.ext.haskell.syntax.ModuleName]:
        return hydra.lib.maps.from_list(hydra.lib.lists.map((lambda ns_: (ns_, to_module_name(ns_))), hydra.lib.sets.to_list(new_namespaces())))
    return hydra.module.Namespaces(ns0.focus, hydra.lib.maps.union(ns0.mapping, new_mappings()))

def collect_test_cases(tg: hydra.testing.TestGroup) -> frozenlist[hydra.testing.TestCaseWithMetadata]:
    r"""Collect all test cases from a test group recursively."""

    return hydra.lib.lists.concat2(tg.cases, hydra.lib.lists.concat(hydra.lib.lists.map((lambda x1: collect_test_cases(x1)), tg.subgroups)))

def collect_names(graf: hydra.graph.Graph, names: frozenset[hydra.core.Name], t: hydra.core.Term) -> frozenset[hydra.core.Name]:
    r"""Collect variable names from encoded terms within a single term node."""

    return hydra.lib.logic.if_else(hydra.schemas.is_encoded_term(hydra.rewriting.deannotate_term(t)), (lambda : hydra.lib.eithers.either((lambda _: names), (lambda decoded_term: hydra.lib.sets.union(names, hydra.rewriting.term_dependency_names(True, True, True, decoded_term))), hydra.lib.eithers.bimap((lambda _e: _e), (lambda _a: _a), hydra.decode.core.term(graf, t)))), (lambda : names))

def extract_encoded_term_variable_names(graf: hydra.graph.Graph, term: hydra.core.Term) -> frozenset[hydra.core.Name]:
    r"""Extract all variable names from term-encoded terms in a given term."""

    return hydra.rewriting.fold_over_term(hydra.coders.TraversalOrder.PRE, (lambda v1, v2: collect_names(graf, v1, v2)), hydra.lib.sets.empty(), term)

def extract_test_terms(tcm: hydra.testing.TestCaseWithMetadata) -> frozenlist[hydra.core.Term]:
    r"""Extract input and output terms from a test case."""

    match tcm.case:
        case hydra.testing.TestCaseDelegatedEvaluation(value=del_case):
            return (del_case.input, del_case.output)

        case _:
            return ()

def build_namespaces_for_test_group(mod: hydra.module.Module, tgroup: hydra.testing.TestGroup, graph_: hydra.graph.Graph) -> Either[str, hydra.module.Namespaces[hydra.ext.haskell.syntax.ModuleName]]:
    r"""Build namespaces for a test group including encoded term references."""

    @lru_cache(1)
    def test_cases_() -> frozenlist[hydra.testing.TestCaseWithMetadata]:
        return collect_test_cases(tgroup)
    @lru_cache(1)
    def test_terms() -> frozenlist[hydra.core.Term]:
        return hydra.lib.lists.concat(hydra.lib.lists.map((lambda x1: extract_test_terms(x1)), test_cases_()))
    @lru_cache(1)
    def test_bindings() -> frozenlist[hydra.core.Binding]:
        return hydra.lib.lists.map((lambda term: hydra.core.Binding(hydra.core.Name("_test_"), term, Nothing())), test_terms())
    temp_module = hydra.module.Module(mod.namespace, test_bindings(), mod.term_dependencies, mod.type_dependencies, mod.description)
    return hydra.lib.eithers.bind(hydra.lib.eithers.bimap((lambda ic: hydra.show.errors.error(ic.object)), (lambda a: a), hydra.ext.haskell.utils.namespaces_for_module(temp_module, hydra.lexical.empty_context(), graph_)), (lambda base_namespaces: (encoded_names := hydra.lib.sets.unions(hydra.lib.lists.map((lambda t: extract_encoded_term_variable_names(graph_, t)), test_terms())), Right(add_namespaces_to_namespaces(base_namespaces, encoded_names)))[1]))

def build_test_module_with_codec(codec: hydra.testing.TestCodec, test_module: hydra.module.Module, test_group: hydra.testing.TestGroup, test_body: str, namespaces: T0) -> str:
    r"""Build the complete test module using a TestCodec."""

    ns_ = test_module.namespace
    spec_ns = hydra.module.Namespace(hydra.lib.strings.cat2(ns_.value, "Spec"))
    @lru_cache(1)
    def module_name_string() -> str:
        return codec.format_module_name(spec_ns)
    group_name_ = test_group.name
    @lru_cache(1)
    def domain_imports() -> frozenlist[str]:
        return codec.find_imports(hydra.lib.sets.empty())
    standard_imports = ("import Hydra.Kernel", "import qualified Test.Hspec as H", "import qualified Data.List as L", "import qualified Data.Map as M", "import qualified Data.Set as S", "import qualified Data.Maybe as Y")
    @lru_cache(1)
    def all_imports() -> frozenlist[str]:
        return hydra.lib.lists.concat2(standard_imports, domain_imports())
    debug_comments = ("-- DEBUG: Focus namespace = (see generated module)", "-- DEBUG: Namespace mappings: (see generated module)")
    @lru_cache(1)
    def header() -> str:
        return hydra.lib.strings.intercalate("\n", hydra.lib.lists.concat(((hydra.lib.strings.cat2("-- ", hydra.constants.warning_auto_generated_file), ""), debug_comments, ("", hydra.lib.strings.cat(("module ", module_name_string(), " where")), ""), all_imports(), ("", "spec :: H.Spec", hydra.lib.strings.cat(("spec = H.describe ", hydra.lib.literals.show_string(group_name_), " $ do"))))))
    return hydra.lib.strings.cat((header(), "\n", test_body, "\n"))

def contains_trivially_polymorphic(term: hydra.core.Term) -> bool:
    r"""Check if a term contains any trivially polymorphic sub-terms."""

    match term:
        case hydra.core.TermList(value=xs):
            return hydra.lib.logic.or_(hydra.lib.lists.null(xs), hydra.lib.lists.foldl(hydra.lib.logic.or_, False, hydra.lib.lists.map((lambda x1: contains_trivially_polymorphic(x1)), xs)))

        case hydra.core.TermSet(value=s):
            return hydra.lib.logic.or_(hydra.lib.sets.null(s), hydra.lib.lists.foldl(hydra.lib.logic.or_, False, hydra.lib.lists.map((lambda x1: contains_trivially_polymorphic(x1)), hydra.lib.sets.to_list(s))))

        case hydra.core.TermMap(value=m):
            return hydra.lib.logic.or_(hydra.lib.maps.null(m), hydra.lib.logic.or_(hydra.lib.lists.foldl(hydra.lib.logic.or_, False, hydra.lib.lists.map((lambda x1: contains_trivially_polymorphic(x1)), hydra.lib.maps.keys(m))), hydra.lib.lists.foldl(hydra.lib.logic.or_, False, hydra.lib.lists.map((lambda x1: contains_trivially_polymorphic(x1)), hydra.lib.lists.map((lambda p: hydra.lib.pairs.second(p)), hydra.lib.maps.to_list(m))))))

        case hydra.core.TermMaybe(value=mx):
            return hydra.lib.maybes.maybe((lambda : True), (lambda x1: contains_trivially_polymorphic(x1)), mx)

        case hydra.core.TermEither():
            return True

        case hydra.core.TermUnion(value=inj):
            return contains_trivially_polymorphic(inj.field.term)

        case hydra.core.TermPair(value=p):
            return hydra.lib.logic.or_(contains_trivially_polymorphic(hydra.lib.pairs.first(p)), contains_trivially_polymorphic(hydra.lib.pairs.second(p)))

        case hydra.core.TermRecord(value=rec):
            return hydra.lib.lists.foldl(hydra.lib.logic.or_, False, hydra.lib.lists.map((lambda f: contains_trivially_polymorphic(f.term)), rec.fields))

        case hydra.core.TermApplication(value=app):
            return hydra.lib.logic.or_(contains_trivially_polymorphic(app.function), contains_trivially_polymorphic(app.argument))

        case _:
            return False

def find_haskell_imports(namespaces: hydra.module.Namespaces[hydra.ext.haskell.syntax.ModuleName], names_: T0) -> frozenlist[str]:
    r"""Find necessary imports for Haskell based on referenced names."""

    @lru_cache(1)
    def mapping_() -> FrozenDict[hydra.module.Namespace, hydra.ext.haskell.syntax.ModuleName]:
        return namespaces.mapping
    @lru_cache(1)
    def filtered() -> FrozenDict[hydra.module.Namespace, hydra.ext.haskell.syntax.ModuleName]:
        return hydra.lib.maps.filter_with_key((lambda ns_, _v: hydra.lib.logic.not_(hydra.lib.equality.equal(hydra.lib.lists.head(hydra.lib.strings.split_on("hydra.test.", ns_.value)), ""))), mapping_())
    return hydra.lib.lists.map((lambda entry: hydra.lib.strings.cat(("import qualified ", hydra.lib.strings.intercalate(".", hydra.lib.lists.map(hydra.formatting.capitalize, hydra.lib.strings.split_on(".", hydra.lib.pairs.first(entry).value))), " as ", hydra.lib.pairs.second(entry).value))), hydra.lib.maps.to_list(filtered()))

def try_infer_type_of(g: hydra.graph.Graph, term: hydra.core.Term) -> Maybe[tuple[hydra.core.Term, hydra.core.TypeScheme]]:
    r"""Try to infer the type of a term, returning Nothing if inference fails."""

    return hydra.lib.eithers.either((lambda _: Nothing()), (lambda result: Just(hydra.lib.pairs.first(result))), hydra.inference.infer_type_of(hydra.lexical.empty_context(), g, term))

def type_to_haskell(namespaces: hydra.module.Namespaces[hydra.ext.haskell.syntax.ModuleName], typ: hydra.core.Type, g: T0) -> Either[str, str]:
    r"""Convert a Hydra type to a Haskell type expression string."""

    return hydra.lib.eithers.bimap((lambda ic: hydra.show.errors.error(ic.object)), (lambda arg_: hydra.serialization.print_expr(hydra.serialization.parenthesize(hydra.ext.haskell.serde.type_to_expr(arg_)))), hydra.ext.haskell.coder.encode_type(namespaces, typ, hydra.lexical.empty_context(), g))

def generate_type_annotation_for(g: hydra.graph.Graph, namespaces: hydra.module.Namespaces[hydra.ext.haskell.syntax.ModuleName], input_term: hydra.core.Term, output_term: hydra.core.Term) -> Either[str, Maybe[str]]:
    r"""Generate a type annotation for polymorphic output values."""

    return hydra.lib.logic.if_else(hydra.lib.logic.not_(contains_trivially_polymorphic(output_term)), (lambda : Right(Nothing())), (lambda : hydra.lib.maybes.maybe((lambda : Right(Nothing())), (lambda result: (type_scheme := hydra.lib.pairs.second(result), typ := type_scheme.type, schema_vars := hydra.lib.sets.from_list(hydra.lib.maps.keys(g.schema_types)), free_vars := hydra.lib.sets.to_list(hydra.lib.sets.difference(hydra.rewriting.free_variables_in_type(typ), schema_vars)), is_either := (_hoist_is_either_1 := (lambda v1: (lambda _: True)(v1.value) if isinstance(v1, hydra.core.TermEither) else False), _hoist_is_either_1(hydra.rewriting.deannotate_term(output_term)))[1], hydra.lib.logic.if_else(hydra.lib.logic.or_(is_either, hydra.lib.logic.not_(hydra.lib.lists.null(free_vars))), (lambda : (int32_type := cast(hydra.core.Type, hydra.core.TypeLiteral(cast(hydra.core.LiteralType, hydra.core.LiteralTypeInteger(hydra.core.IntegerType.INT32)))), subst := hydra.typing.TypeSubst(hydra.lib.maps.from_list(hydra.lib.lists.map((lambda v: (v, int32_type)), free_vars))), grounded_type := hydra.substitution.subst_in_type(subst, typ), hydra.lib.eithers.map((lambda type_str: Just(hydra.lib.strings.cat2(" :: ", type_str))), type_to_haskell(namespaces, grounded_type, g)))[3]), (lambda : Right(Nothing()))))[5]), try_infer_type_of(g, input_term))))

def indent_continuation_lines(n: int, s: str) -> str:
    r"""Indent continuation lines of a multi-line string."""

    return hydra.lib.strings.intercalate(hydra.lib.strings.cat2("\n", hydra.lib.strings.from_list(hydra.lib.lists.replicate(n, 32))), hydra.lib.strings.split_on("\n", s))

def generate_test_case_with_codec(g: hydra.graph.Graph, namespaces: hydra.module.Namespaces[hydra.ext.haskell.syntax.ModuleName], codec: hydra.testing.TestCodec, depth: int, tcm: hydra.testing.TestCaseWithMetadata) -> Either[str, frozenlist[str]]:
    r"""Generate a single test case using a TestCodec."""

    name_ = tcm.name
    tcase = tcm.case
    match tcase:
        case hydra.testing.TestCaseDelegatedEvaluation(value=del_case):
            input_ = del_case.input
            output_ = del_case.output
            @lru_cache(1)
            def formatted_name() -> str:
                return codec.format_test_name(name_)
            @lru_cache(1)
            def continuation_indent() -> int:
                return hydra.lib.math.add(hydra.lib.math.mul(depth, 2), 4)
            return hydra.lib.eithers.bind(codec.encode_term(input_, g), (lambda input_code: hydra.lib.eithers.bind(codec.encode_term(output_, g), (lambda output_code: hydra.lib.eithers.bind(generate_type_annotation_for(g, namespaces, input_, output_), (lambda type_annotation: (indented_input_code := indent_continuation_lines(continuation_indent(), input_code), indented_output_code := indent_continuation_lines(continuation_indent(), output_code), final_output_code := hydra.lib.maybes.maybe((lambda : indented_output_code), (lambda anno: hydra.lib.strings.cat2(indented_output_code, anno)), type_annotation), Right((hydra.lib.strings.cat(("H.it ", hydra.lib.literals.show_string(formatted_name()), " $ H.shouldBe")), hydra.lib.strings.cat(("  (", indented_input_code, ")")), hydra.lib.strings.cat(("  (", final_output_code, ")")))))[3]))))))

        case _:
            return Right(())

def generate_test_group_hierarchy(g: hydra.graph.Graph, namespaces: hydra.module.Namespaces[hydra.ext.haskell.syntax.ModuleName], codec: hydra.testing.TestCodec, depth: int, test_group: hydra.testing.TestGroup) -> Either[str, str]:
    r"""Generate test hierarchy preserving the structure with H.describe blocks for subgroups."""

    cases_ = test_group.cases
    subgroups = test_group.subgroups
    @lru_cache(1)
    def indent() -> str:
        return hydra.lib.strings.from_list(hydra.lib.lists.replicate(hydra.lib.math.mul(depth, 2), 32))
    return hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda tc: generate_test_case_with_codec(g, namespaces, codec, depth, tc)), cases_), (lambda test_case_lines_raw: (test_case_lines := hydra.lib.lists.map((lambda lines_: hydra.lib.lists.map((lambda line: hydra.lib.strings.cat2(indent(), line)), lines_)), test_case_lines_raw), test_cases_str := hydra.lib.strings.intercalate("\n", hydra.lib.lists.concat(test_case_lines)), hydra.lib.eithers.map((lambda subgroups_str: hydra.lib.strings.cat((test_cases_str, hydra.lib.logic.if_else(hydra.lib.logic.or_(hydra.lib.equality.equal(test_cases_str, ""), hydra.lib.equality.equal(subgroups_str, "")), (lambda : ""), (lambda : "\n")), subgroups_str))), hydra.lib.eithers.map((lambda blocks: hydra.lib.strings.intercalate("\n", blocks)), hydra.lib.eithers.map_list((lambda subgroup: (group_name_ := subgroup.name, hydra.lib.eithers.map((lambda content: hydra.lib.strings.cat((indent(), "H.describe ", hydra.lib.literals.show_string(group_name_), " $ do\n", content))), generate_test_group_hierarchy(g, namespaces, codec, hydra.lib.math.add(depth, 1), subgroup)))[1]), subgroups))))[2]))

def generate_test_file_with_codec(codec: hydra.testing.TestCodec, test_module: hydra.module.Module, test_group: hydra.testing.TestGroup, namespaces: hydra.module.Namespaces[hydra.ext.haskell.syntax.ModuleName], g: hydra.graph.Graph) -> Either[str, tuple[str, str]]:
    r"""Generate a complete test file using a TestCodec."""

    return hydra.lib.eithers.map((lambda test_body: (test_module_content := build_test_module_with_codec(codec, test_module, test_group, test_body, namespaces), ext := codec.file_extension.value, ns_ := test_module.namespace, spec_ns := hydra.module.Namespace(hydra.lib.strings.cat2(ns_.value, "Spec")), file_path := hydra.names.namespace_to_file_path(hydra.util.CaseConvention.PASCAL, hydra.module.FileExtension(ext), spec_ns), (file_path, test_module_content))[5]), generate_test_group_hierarchy(g, namespaces, codec, 1, test_group))

# Template for Haskell import statements.
haskell_import_template = "import qualified {namespace} as {alias}"

# Template for Haskell test module structure.
haskell_module_template = hydra.lib.strings.intercalate("\n", (hydra.lib.strings.cat2("-- ", hydra.constants.warning_auto_generated_file), "", "module {moduleName} where", "", "{imports}", "", "spec :: H.Spec", "{testGroup}", "{testCases}", ""))

# Template for HSpec test case assertions.
haskell_test_case_template = hydra.lib.strings.intercalate("\n", ("  H.it {name} $ H.shouldBe", "    ({input})", "    ({output})", ""))

# Template for HSpec test group description.
haskell_test_group_template = "spec = H.describe {groupName} $ do"

def namespace_to_module_name(ns_: hydra.module.Namespace) -> str:
    r"""Convert namespace to Haskell module name."""

    return hydra.lib.strings.intercalate(".", hydra.lib.lists.map(hydra.formatting.capitalize, hydra.lib.strings.split_on(".", ns_.value)))

def term_to_haskell(namespaces: hydra.module.Namespaces[hydra.ext.haskell.syntax.ModuleName], term: hydra.core.Term, g: hydra.graph.Graph) -> Either[str, str]:
    r"""Convert a Hydra term to a Haskell expression string."""

    return hydra.lib.eithers.bimap((lambda ic: hydra.show.errors.error(ic.object)), (lambda arg_: hydra.serialization.print_expr(hydra.serialization.parenthesize(hydra.ext.haskell.serde.expression_to_expr(arg_)))), hydra.ext.haskell.coder.encode_term(0, namespaces, term, hydra.lexical.empty_context(), g))

def haskell_test_codec(namespaces: hydra.module.Namespaces[hydra.ext.haskell.syntax.ModuleName]) -> hydra.testing.TestCodec:
    r"""Create a Haskell TestCodec that uses the real Haskell coder."""

    return hydra.testing.TestCodec(hydra.coders.LanguageName("haskell"), hydra.module.FileExtension("hs"), (lambda v1, v2: term_to_haskell(namespaces, v1, v2)), (lambda v1, v2: type_to_haskell(namespaces, v1, v2)), (lambda n: n), (lambda x1: namespace_to_module_name(x1)), haskell_test_case_template, haskell_test_group_template, haskell_module_template, haskell_import_template, (lambda v1: find_haskell_imports(namespaces, v1)))

def generate_haskell_test_file(test_module: hydra.module.Module, test_group: hydra.testing.TestGroup, g: hydra.graph.Graph) -> Either[str, tuple[str, str]]:
    r"""Generate a Haskell test file for a test group, with type inference and namespace building."""

    return hydra.lib.eithers.bind(build_namespaces_for_test_group(test_module, test_group, g), (lambda namespaces: generate_test_file_with_codec(haskell_test_codec(namespaces), test_module, test_group, namespaces, g)))
