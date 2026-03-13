# Note: this is an automatically generated file. Do not edit.

r"""Java test code generation codec for JUnit-based generation tests."""

from __future__ import annotations
from collections.abc import Callable
from decimal import Decimal
from functools import lru_cache
from hydra.dsl.python import Either, Nothing, Right, frozenlist
from typing import TypeVar, cast
import hydra.coders
import hydra.constants
import hydra.context
import hydra.core
import hydra.error
import hydra.ext.java.coder
import hydra.ext.java.helpers
import hydra.ext.java.serde
import hydra.formatting
import hydra.inference
import hydra.lexical
import hydra.lib.eithers
import hydra.lib.equality
import hydra.lib.lists
import hydra.lib.logic
import hydra.lib.maps
import hydra.lib.sets
import hydra.lib.strings
import hydra.module
import hydra.rewriting
import hydra.serialization
import hydra.testing
import hydra.typing
import hydra.util

T0 = TypeVar("T0")
T1 = TypeVar("T1")
T2 = TypeVar("T2")

def build_java_test_module(codec: T0, test_module: hydra.module.Module, test_group: hydra.testing.TestGroup, test_body: str) -> str:
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

def generate_assertion(assert_type: str, output_code: str, input_code: str) -> frozenlist[str]:
    r"""Generate assertion code lines based on assertion type, output code, and input code."""
    
    return hydra.lib.logic.if_else(hydra.lib.equality.equal(assert_type, "assertArrayEquals"), (lambda : ("        assertArrayEquals(", hydra.lib.strings.cat(("            ", output_code, ",")), hydra.lib.strings.cat(("            ", input_code, ");")))), (lambda : hydra.lib.logic.if_else(hydra.lib.equality.equal(assert_type, "assertBigDecimalEquals"), (lambda : (hydra.lib.strings.cat(("        assertEquals(0, (", output_code, ").compareTo(", input_code, "));")),)), (lambda : hydra.lib.logic.if_else(hydra.lib.equality.equal(assert_type, "assertDoubleEquals"), (lambda : ("        assertEquals(", hydra.lib.strings.cat(("            ", output_code, ",")), hydra.lib.strings.cat(("            ", input_code, ",")), "            1e-15);")), (lambda : ("        assertEquals(", hydra.lib.strings.cat(("            ", output_code, ",")), hydra.lib.strings.cat(("            ", input_code, ");")))))))))

def get_assertion_type(term: hydra.core.Term):
    def _hoist_hydra_ext_java_test_codec_get_assertion_type_1(v1):
        match v1:
            case hydra.core.FloatValueBigfloat():
                return "assertBigDecimalEquals"
            
            case _:
                return "assertDoubleEquals"
    def _hoist_hydra_ext_java_test_codec_get_assertion_type_2(v1):
        match v1:
            case hydra.core.LiteralBinary():
                return "assertArrayEquals"
            
            case hydra.core.LiteralFloat(value=fv):
                return _hoist_hydra_ext_java_test_codec_get_assertion_type_1(fv)
            
            case _:
                return "assertEquals"
    match hydra.rewriting.deannotate_term(term):
        case hydra.core.TermLiteral(value=lit):
            return _hoist_hydra_ext_java_test_codec_get_assertion_type_2(lit)
        
        case _:
            return "assertEquals"

def is_inference_var(n: hydra.core.Name) -> bool:
    r"""Check if a Name is an unresolved inference variable (matches pattern t followed by digits)."""
    
    s = n.value
    chars = hydra.lib.strings.to_list(s)
    return hydra.lib.logic.and_(hydra.lib.equality.equal(hydra.lib.strings.char_at(0, s), 116), hydra.lib.logic.and_(hydra.lib.logic.not_(hydra.lib.equality.equal(hydra.lib.strings.length(s), 1)), hydra.lib.lists.null(hydra.lib.lists.filter((lambda c: hydra.lib.logic.not_(hydra.lib.logic.and_(hydra.lib.equality.gte(c, 48), hydra.lib.equality.lte(c, 57)))), hydra.lib.lists.drop(1, chars)))))

def generate_java_test_case(g: hydra.graph.Graph, codec: hydra.testing.TestCodec, group_path: frozenlist[str], tcm: hydra.testing.TestCaseWithMetadata) -> Either[str, frozenlist[str]]:
    r"""Generate a single JUnit test case from a test case with metadata."""
    
    name_ = tcm.name
    tcase = tcm.case
    match tcase:
        case hydra.testing.TestCaseDelegatedEvaluation(value=del_case):
            input_ = del_case.input
            output_ = del_case.output
            @lru_cache(1)
            def full_name() -> str:
                return hydra.lib.logic.if_else(hydra.lib.lists.null(group_path), (lambda : name_), (lambda : hydra.lib.strings.intercalate("_", hydra.lib.lists.concat2(group_path, (name_,)))))
            @lru_cache(1)
            def formatted_name() -> str:
                return codec.format_test_name(full_name())
            @lru_cache(1)
            def assert_type() -> str:
                return get_assertion_type(output_)
            @lru_cache(1)
            def type_vars() -> frozenlist[hydra.core.Name]:
                return hydra.lib.lists.sort(hydra.lib.lists.filter((lambda x1: is_inference_var(x1)), hydra.lib.sets.to_list(hydra.lib.sets.union(hydra.rewriting.free_type_variables_in_term(input_), hydra.rewriting.free_type_variables_in_term(output_)))))
            @lru_cache(1)
            def type_params_str() -> str:
                return hydra.lib.logic.if_else(hydra.lib.lists.null(type_vars()), (lambda : ""), (lambda : hydra.lib.strings.cat(("<", hydra.lib.strings.intercalate(", ", hydra.lib.lists.map((lambda n_: hydra.formatting.capitalize(n_.value)), type_vars())), "> "))))
            return hydra.lib.eithers.bind(codec.encode_term(input_, g), (lambda input_code: hydra.lib.eithers.map((lambda output_code: (assertion_lines := generate_assertion(assert_type(), output_code, input_code), hydra.lib.lists.concat2(("    @Test", hydra.lib.strings.cat(("    public ", type_params_str(), "void ", formatted_name(), "() {"))), hydra.lib.lists.concat2(assertion_lines, ("    }",))))[1]), codec.encode_term(output_, g))))
        
        case _:
            return Right(())

def generate_java_test_group_hierarchy(g: hydra.graph.Graph, codec: hydra.testing.TestCodec, group_path: frozenlist[str], test_group: hydra.testing.TestGroup) -> Either[str, str]:
    r"""Generate test hierarchy for Java with nested subgroups."""
    
    cases_ = test_group.cases
    subgroups = test_group.subgroups
    return hydra.lib.eithers.bind(hydra.lib.eithers.map((lambda lines_: hydra.lib.strings.intercalate("\n\n", hydra.lib.lists.concat(lines_))), hydra.lib.eithers.map_list((lambda tc: generate_java_test_case(g, codec, group_path, tc)), cases_)), (lambda test_cases_str: hydra.lib.eithers.map((lambda subgroups_str: hydra.lib.strings.cat((test_cases_str, hydra.lib.logic.if_else(hydra.lib.logic.or_(hydra.lib.equality.equal(test_cases_str, ""), hydra.lib.equality.equal(subgroups_str, "")), (lambda : ""), (lambda : "\n\n")), subgroups_str))), hydra.lib.eithers.map((lambda blocks: hydra.lib.strings.intercalate("\n\n", blocks)), hydra.lib.eithers.map_list((lambda subgroup: (group_name := subgroup.name, header := hydra.lib.strings.cat2("    // ", group_name), hydra.lib.eithers.map((lambda content: hydra.lib.strings.cat((header, "\n\n", content))), generate_java_test_group_hierarchy(g, codec, hydra.lib.lists.concat2(group_path, (group_name,)), subgroup)))[2]), subgroups)))))

def generate_test_file_with_java_codec(codec: hydra.testing.TestCodec, test_module: hydra.module.Module, test_group: hydra.testing.TestGroup, g: hydra.graph.Graph) -> Either[str, tuple[str, str]]:
    r"""Generate a complete test file using the Java codec."""
    
    return hydra.lib.eithers.map((lambda test_body: (test_module_content := build_java_test_module(codec, test_module, test_group, test_body), ns_ := test_module.namespace, parts := hydra.lib.strings.split_on(".", ns_.value), dir_parts := hydra.lib.lists.drop(1, hydra.lib.lists.init(parts)), class_name_ := hydra.lib.strings.cat2(hydra.formatting.capitalize(hydra.lib.lists.last(parts)), "Test"), file_name := hydra.lib.strings.cat2(class_name_, ".java"), file_path := hydra.lib.strings.cat((hydra.lib.strings.intercalate("/", dir_parts), "/", file_name)), (file_path, test_module_content))[7]), generate_java_test_group_hierarchy(g, codec, (), test_group))

def infer_term(g: hydra.graph.Graph, term: hydra.core.Term) -> Either[str, hydra.core.Term]:
    r"""Run type inference on a single term."""
    
    return hydra.lib.eithers.bimap((lambda ic: ic.object.value), (lambda x: x.term), hydra.inference.infer_in_graph_context(hydra.lexical.empty_context(), g, term))

def infer_test_case(g: hydra.graph.Graph, tcm: hydra.testing.TestCaseWithMetadata):
    r"""Run type inference on the terms in a test case."""
    
    name_ = tcm.name
    tcase = tcm.case
    desc = tcm.description
    tags_ = tcm.tags
    def _hoist_body_1(v1):
        match v1:
            case hydra.testing.TestCaseDelegatedEvaluation(value=del_case):
                input_ = del_case.input
                output_ = del_case.output
                return hydra.lib.eithers.bind(infer_term(g, input_), (lambda inferred_input: hydra.lib.eithers.map((lambda inferred_output: cast(hydra.testing.TestCase, hydra.testing.TestCaseDelegatedEvaluation(hydra.testing.DelegatedEvaluationTestCase(inferred_input, inferred_output)))), infer_term(g, output_))))
            
            case _:
                return Right(tcase)
    return hydra.lib.eithers.map((lambda inferred_case: hydra.testing.TestCaseWithMetadata(name_, inferred_case, desc, tags_)), _hoist_body_1(tcase))

def infer_test_group_terms(g: hydra.graph.Graph, tg: hydra.testing.TestGroup) -> Either[str, hydra.testing.TestGroup]:
    r"""Run type inference on all terms in a TestGroup to ensure lambdas have domain types."""
    
    name_ = tg.name
    desc = tg.description
    subgroups = tg.subgroups
    cases_ = tg.cases
    return hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda sg: infer_test_group_terms(g, sg)), subgroups), (lambda inferred_subgroups: hydra.lib.eithers.map((lambda inferred_cases: hydra.testing.TestGroup(name_, desc, inferred_subgroups, inferred_cases)), hydra.lib.eithers.map_list((lambda tc: infer_test_case(g, tc)), cases_))))

# Template for Java import statements.
java_import_template = "import {namespace};"

# Template for JUnit test module structure.
java_module_template = hydra.lib.strings.intercalate("\n", (hydra.lib.strings.cat2("// ", hydra.constants.warning_auto_generated_file), "", "package {package};", "", "{imports}", "", "public class {className} {", "    {testCases}", "}"))

# Template for JUnit test case assertions.
java_test_case_template = hydra.lib.strings.intercalate("\n", ("    @Test", "    public void {name}() {", "        assertEquals({output}, {input});", "    }"))

# Template for JUnit test group comments.
java_test_group_template = "// {groupName}"

def namespace_to_java_class_name(ns_: hydra.module.Namespace) -> str:
    r"""Convert namespace to Java class name."""
    
    return hydra.lib.strings.intercalate(".", hydra.lib.lists.map(hydra.formatting.capitalize, hydra.lib.strings.split_on(".", ns_.value)))

def term_to_java(term: hydra.core.Term, g: hydra.graph.Graph) -> Either[str, str]:
    r"""Convert a Hydra term to a Java expression string."""
    
    return hydra.lib.eithers.bimap((lambda ic: ic.object.value), (lambda arg_: hydra.serialization.print_expr(hydra.serialization.parenthesize(hydra.ext.java.serde.write_expression(arg_)))), hydra.ext.java.coder.encode_term(hydra.ext.java.helpers.JavaEnvironment(hydra.ext.java.helpers.Aliases(hydra.module.Namespace("test"), hydra.lib.maps.empty(), hydra.lib.sets.empty(), hydra.lib.sets.empty(), hydra.lib.sets.empty(), hydra.lib.sets.empty(), hydra.lib.sets.empty(), hydra.lib.maps.empty(), hydra.lib.sets.empty(), hydra.lib.maps.empty(), hydra.lib.sets.empty(), Nothing(), hydra.lib.sets.empty()), g), term, hydra.lexical.empty_context(), g))

def type_to_java(_t: T0, _g: T1) -> Either[T2, str]:
    r"""Convert a Hydra type to a Java type expression string (placeholder returning Object)."""
    
    return Right("Object")

@lru_cache(1)
def java_test_codec() -> hydra.testing.TestCodec:
    r"""Create a Java TestCodec for JUnit-based test generation."""
    
    return hydra.testing.TestCodec(hydra.coders.LanguageName("java"), hydra.module.FileExtension("java"), (lambda x1, x2: term_to_java(x1, x2)), (lambda x1, x2: type_to_java(x1, x2)), format_java_test_name, (lambda x1: namespace_to_java_class_name(x1)), java_test_case_template, java_test_group_template, java_module_template, java_import_template, (lambda _names: find_java_imports))

def generate_java_test_file(test_module: hydra.module.Module, test_group: hydra.testing.TestGroup, g: hydra.graph.Graph) -> Either[str, tuple[str, str]]:
    r"""Generate a Java test file for a test group, with type inference."""
    
    return hydra.lib.eithers.bind(infer_test_group_terms(g, test_group), (lambda inferred_test_group: generate_test_file_with_java_codec(java_test_codec(), test_module, inferred_test_group, g)))
