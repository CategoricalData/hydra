# Note: this is an automatically generated file. Do not edit.

r"""A model for unit testing."""

from __future__ import annotations
from collections.abc import Callable
from dataclasses import dataclass
from enum import Enum
from functools import lru_cache
from hydra.dsl.python import Either, Maybe, Node, frozenlist
from typing import Annotated, Generic, TypeAlias, TypeVar, cast
import hydra.ast
import hydra.coders
import hydra.core
import hydra.error.core
import hydra.graph
import hydra.json.model
import hydra.module
import hydra.parsing
import hydra.typing
import hydra.util

A = TypeVar("A")

@dataclass(frozen=True)
class AlphaConversionTestCase:
    r"""A test case which performs alpha conversion (variable renaming) on a term and compares the result with the expected term."""

    term: Annotated[hydra.core.Term, "The term on which to perform alpha conversion"]
    old_variable: Annotated[hydra.core.Name, "The variable name to replace"]
    new_variable: Annotated[hydra.core.Name, "The new variable name"]
    result: Annotated[hydra.core.Term, "The expected result term after alpha conversion"]

    TYPE_ = hydra.core.Name("hydra.testing.AlphaConversionTestCase")
    TERM = hydra.core.Name("term")
    OLD_VARIABLE = hydra.core.Name("oldVariable")
    NEW_VARIABLE = hydra.core.Name("newVariable")
    RESULT = hydra.core.Name("result")

class EvaluationStyle(Enum):
    r"""One of two evaluation styles: eager or lazy."""

    EAGER = hydra.core.Name("eager")

    LAZY = hydra.core.Name("lazy")

EvaluationStyle.TYPE_ = hydra.core.Name("hydra.testing.EvaluationStyle")

@dataclass(frozen=True)
class CaseConversionTestCase:
    r"""A test case which checks that strings are converted between different case conventions correctly."""

    from_convention: Annotated[hydra.util.CaseConvention, "The source case convention"]
    to_convention: Annotated[hydra.util.CaseConvention, "The target case convention"]
    from_string: Annotated[str, "The input string"]
    to_string: Annotated[str, "The expected output string"]

    TYPE_ = hydra.core.Name("hydra.testing.CaseConversionTestCase")
    FROM_CONVENTION = hydra.core.Name("fromConvention")
    TO_CONVENTION = hydra.core.Name("toConvention")
    FROM_STRING = hydra.core.Name("fromString")
    TO_STRING = hydra.core.Name("toString")

@dataclass(frozen=True)
class DelegatedEvaluationTestCase:
    r"""DEPRECATED: Delegated evaluation test case (to be removed)."""

    input: hydra.core.Term
    output: hydra.core.Term

    TYPE_ = hydra.core.Name("hydra.testing.DelegatedEvaluationTestCase")
    INPUT = hydra.core.Name("input")
    OUTPUT = hydra.core.Name("output")

@dataclass(frozen=True)
class EtaExpansionTestCase:
    r"""A test case which performs eta expansion (adding missing lambda abstractions) on a given term and compares the result with the expected result."""

    input: Annotated[hydra.core.Term, "The term to eta expand"]
    output: Annotated[hydra.core.Term, "The expected result"]

    TYPE_ = hydra.core.Name("hydra.testing.EtaExpansionTestCase")
    INPUT = hydra.core.Name("input")
    OUTPUT = hydra.core.Name("output")

@dataclass(frozen=True)
class DeannotateTermTestCase:
    r"""A test case which strips all annotations from a term and compares the result with the expected term."""

    input: Annotated[hydra.core.Term, "The term to deannotate"]
    output: Annotated[hydra.core.Term, "The expected deannotated term"]

    TYPE_ = hydra.core.Name("hydra.testing.DeannotateTermTestCase")
    INPUT = hydra.core.Name("input")
    OUTPUT = hydra.core.Name("output")

@dataclass(frozen=True)
class DeannotateTypeTestCase:
    r"""A test case which strips all annotations from a type and compares the result with the expected type."""

    input: Annotated[hydra.core.Type, "The type to deannotate"]
    output: Annotated[hydra.core.Type, "The expected deannotated type"]

    TYPE_ = hydra.core.Name("hydra.testing.DeannotateTypeTestCase")
    INPUT = hydra.core.Name("input")
    OUTPUT = hydra.core.Name("output")

@dataclass(frozen=True)
class FlattenLetTermsTestCase:
    r"""A test case which flattens nested let terms, lifting inner bindings to the outer let, and compares the result with the expected term."""

    input: Annotated[hydra.core.Term, "The term to flatten"]
    output: Annotated[hydra.core.Term, "The expected flattened term"]

    TYPE_ = hydra.core.Name("hydra.testing.FlattenLetTermsTestCase")
    INPUT = hydra.core.Name("input")
    OUTPUT = hydra.core.Name("output")

class FoldOperation(Enum):
    r"""A predefined fold operation for testing foldOverTerm."""

    SUM_INT32_LITERALS = hydra.core.Name("sumInt32Literals")
    r"""Sum all Int32 literals in a term"""

    COLLECT_LIST_LENGTHS = hydra.core.Name("collectListLengths")
    r"""Collect the lengths of all list terms (returns list of integers in traversal order)"""

    COLLECT_LABELS = hydra.core.Name("collectLabels")
    r"""Collect labels (first element of pairs where first is a string literal)"""

FoldOperation.TYPE_ = hydra.core.Name("hydra.testing.FoldOperation")

@dataclass(frozen=True)
class FoldOverTermTestCase:
    r"""A test case which applies a fold operation over a term and compares the result."""

    input: Annotated[hydra.core.Term, "The term to fold over"]
    traversal_order: Annotated[hydra.coders.TraversalOrder, "The traversal order (pre or post)"]
    operation: Annotated[FoldOperation, "The fold operation to apply"]
    output: Annotated[hydra.core.Term, "The expected result of the fold"]

    TYPE_ = hydra.core.Name("hydra.testing.FoldOverTermTestCase")
    INPUT = hydra.core.Name("input")
    TRAVERSAL_ORDER = hydra.core.Name("traversalOrder")
    OPERATION = hydra.core.Name("operation")
    OUTPUT = hydra.core.Name("output")

@dataclass(frozen=True)
class FreeVariablesTestCase:
    r"""A test case which computes the free variables of a term and compares the result with an expected set of names."""

    input: Annotated[hydra.core.Term, "The term to analyze"]
    output: Annotated[frozenset[hydra.core.Name], "The expected set of free variable names"]

    TYPE_ = hydra.core.Name("hydra.testing.FreeVariablesTestCase")
    INPUT = hydra.core.Name("input")
    OUTPUT = hydra.core.Name("output")

class HoistPredicate(Enum):
    r"""A predefined predicate for testing hoistSubterms. Each predicate determines which subterms should be hoisted into let bindings."""

    CASE_STATEMENTS = hydra.core.Name("caseStatements")
    r"""Hoist case statements (elimination unions) that appear in non-top-level positions"""

    APPLICATIONS = hydra.core.Name("applications")
    r"""Hoist function applications that appear in non-top-level positions"""

    LISTS = hydra.core.Name("lists")
    r"""Hoist list terms that appear in non-top-level positions"""

    NOTHING = hydra.core.Name("nothing")
    r"""Never hoist anything (identity transformation for let terms)"""

HoistPredicate.TYPE_ = hydra.core.Name("hydra.testing.HoistPredicate")

@dataclass(frozen=True)
class HoistLetBindingsTestCase:
    r"""A test case for hoistLetBindings with hoistAll=True, which hoists ALL nested let bindings to the top level of a let term, not just polymorphic ones. This is used for targets like Java that cannot have let expressions in arbitrary positions."""

    input: Annotated[hydra.core.Let, "The input let term"]
    output: Annotated[hydra.core.Let, "The expected output let term with all nested bindings hoisted to top"]

    TYPE_ = hydra.core.Name("hydra.testing.HoistLetBindingsTestCase")
    INPUT = hydra.core.Name("input")
    OUTPUT = hydra.core.Name("output")

@dataclass(frozen=True)
class HoistPolymorphicLetBindingsTestCase:
    r"""A test case for the hoistPolymorphicLetBindings function, which hoists polymorphic let bindings to the top level of a let term. This is used for targets like Java which don't support polymorphic lambdas."""

    input: Annotated[hydra.core.Let, "The input let term"]
    output: Annotated[hydra.core.Let, "The expected output let term with polymorphic bindings hoisted to top"]

    TYPE_ = hydra.core.Name("hydra.testing.HoistPolymorphicLetBindingsTestCase")
    INPUT = hydra.core.Name("input")
    OUTPUT = hydra.core.Name("output")

@dataclass(frozen=True)
class HoistSubtermsTestCase:
    r"""A test case which hoists subterms into let bindings based on a predicate, and compares the result with the expected term. The predicate decides which subterms at which positions should be extracted into new bindings."""

    predicate: Annotated[HoistPredicate, "The predicate that determines which subterms to hoist"]
    input: Annotated[hydra.core.Term, "The input term (must contain a let expression for hoisting to occur)"]
    output: Annotated[hydra.core.Term, "The expected output term with hoisted subterms as new bindings"]

    TYPE_ = hydra.core.Name("hydra.testing.HoistSubtermsTestCase")
    PREDICATE = hydra.core.Name("predicate")
    INPUT = hydra.core.Name("input")
    OUTPUT = hydra.core.Name("output")

@dataclass(frozen=True)
class HoistCaseStatementsTestCase:
    r"""A test case for the hoistCaseStatements function, which hoists case statements into let bindings, but only when they appear inside a lambda body. This is used for targets like Python which don't support inline match expressions."""

    input: Annotated[hydra.core.Term, "The input term"]
    output: Annotated[hydra.core.Term, "The expected output term with hoisted case statements"]

    TYPE_ = hydra.core.Name("hydra.testing.HoistCaseStatementsTestCase")
    INPUT = hydra.core.Name("input")
    OUTPUT = hydra.core.Name("output")

class TermRewriter(Enum):
    r"""A predefined term rewriter for testing rewriteTerm."""

    REPLACE_FOO_WITH_BAR = hydra.core.Name("replaceFooWithBar")
    r"""Replace all string literal 'foo' with 'bar'"""

    REPLACE_INT32_WITH_INT64 = hydra.core.Name("replaceInt32WithInt64")
    r"""Replace all Int32 literals with Int64 literals of the same value"""

TermRewriter.TYPE_ = hydra.core.Name("hydra.testing.TermRewriter")

@dataclass(frozen=True)
class RewriteTermTestCase:
    r"""A test case which applies a term rewriter and compares the result."""

    input: Annotated[hydra.core.Term, "The term to rewrite"]
    rewriter: Annotated[TermRewriter, "The rewriter to apply"]
    output: Annotated[hydra.core.Term, "The expected rewritten term"]

    TYPE_ = hydra.core.Name("hydra.testing.RewriteTermTestCase")
    INPUT = hydra.core.Name("input")
    REWRITER = hydra.core.Name("rewriter")
    OUTPUT = hydra.core.Name("output")

class TypeRewriter(Enum):
    r"""A predefined type rewriter for testing rewriteType."""

    REPLACE_STRING_WITH_INT32 = hydra.core.Name("replaceStringWithInt32")
    r"""Replace all String types with Int32 types"""

TypeRewriter.TYPE_ = hydra.core.Name("hydra.testing.TypeRewriter")

@dataclass(frozen=True)
class RewriteTypeTestCase:
    r"""A test case which applies a type rewriter and compares the result."""

    input: Annotated[hydra.core.Type, "The type to rewrite"]
    rewriter: Annotated[TypeRewriter, "The rewriter to apply"]
    output: Annotated[hydra.core.Type, "The expected rewritten type"]

    TYPE_ = hydra.core.Name("hydra.testing.RewriteTypeTestCase")
    INPUT = hydra.core.Name("input")
    REWRITER = hydra.core.Name("rewriter")
    OUTPUT = hydra.core.Name("output")

@dataclass(frozen=True)
class EvaluationTestCase:
    r"""A test case which evaluates (reduces) a given term and compares it with the expected result."""

    evaluation_style: Annotated[EvaluationStyle, "The evaluation style (eager or lazy)"]
    input: Annotated[hydra.core.Term, "The term to evaluate"]
    output: Annotated[hydra.core.Term, "The expected result"]

    TYPE_ = hydra.core.Name("hydra.testing.EvaluationTestCase")
    EVALUATION_STYLE = hydra.core.Name("evaluationStyle")
    INPUT = hydra.core.Name("input")
    OUTPUT = hydra.core.Name("output")

@dataclass(frozen=True)
class InferenceFailureTestCase:
    r"""A test case providing a term for which type inference is expected to fail."""

    input: Annotated[hydra.core.Term, "The term for which inference should fail"]

    TYPE_ = hydra.core.Name("hydra.testing.InferenceFailureTestCase")
    INPUT = hydra.core.Name("input")

@dataclass(frozen=True)
class InferenceTestCase:
    r"""A test case which performs type inference on a given term and compares the result with an expected type scheme."""

    input: Annotated[hydra.core.Term, "The term to infer"]
    output: Annotated[hydra.core.TypeScheme, "The expected type scheme"]

    TYPE_ = hydra.core.Name("hydra.testing.InferenceTestCase")
    INPUT = hydra.core.Name("input")
    OUTPUT = hydra.core.Name("output")

@dataclass(frozen=True)
class JsonDecodeTestCase:
    r"""A test case for the Either-based JSON decoder. Takes a type, input JSON, and expected result (Either String Term)."""

    type: Annotated[hydra.core.Type, "The Hydra type to decode into"]
    json: Annotated[hydra.json.model.Value, "The input JSON value"]
    expected: Annotated[Either[str, hydra.core.Term], "The expected result: Left for error, Right for decoded term"]

    TYPE_ = hydra.core.Name("hydra.testing.JsonDecodeTestCase")
    TYPE = hydra.core.Name("type")
    JSON = hydra.core.Name("json")
    EXPECTED = hydra.core.Name("expected")

@dataclass(frozen=True)
class JsonEncodeTestCase:
    r"""A test case for the Either-based JSON encoder. Takes an input term and expected result (Either String Value)."""

    term: Annotated[hydra.core.Term, "The Hydra term to encode"]
    expected: Annotated[Either[str, hydra.json.model.Value], "The expected result: Left for error, Right for encoded JSON"]

    TYPE_ = hydra.core.Name("hydra.testing.JsonEncodeTestCase")
    TERM = hydra.core.Name("term")
    EXPECTED = hydra.core.Name("expected")

# A test case which parses a JSON string and compares the result with an expected JSON value.
JsonParserTestCase: TypeAlias = "ParserTestCase[hydra.json.model.Value]"

@dataclass(frozen=True)
class JsonRoundtripTestCase:
    r"""A test case for round-trip encoding/decoding using the Either-based JSON functions. Encodes a term, then decodes it back, verifying the result equals the original."""

    type: Annotated[hydra.core.Type, "The Hydra type for encoding/decoding"]
    term: Annotated[hydra.core.Term, "The Hydra term to round-trip"]

    TYPE_ = hydra.core.Name("hydra.testing.JsonRoundtripTestCase")
    TYPE = hydra.core.Name("type")
    TERM = hydra.core.Name("term")

@dataclass(frozen=True)
class LiftLambdaAboveLetTestCase:
    r"""A test case which lifts lambda abstractions above let expressions and compares the result with the expected term."""

    input: Annotated[hydra.core.Term, "The term to transform"]
    output: Annotated[hydra.core.Term, "The expected transformed term"]

    TYPE_ = hydra.core.Name("hydra.testing.LiftLambdaAboveLetTestCase")
    INPUT = hydra.core.Name("input")
    OUTPUT = hydra.core.Name("output")

# A test case which serializes a JSON value to a string and compares it to the expected string.
JsonWriterTestCase: TypeAlias = "WriterTestCase[hydra.json.model.Value]"

@dataclass(frozen=True)
class ParserTestCase(Generic[A]):
    r"""A test case which parses an input string and compares the result with an expected value."""

    input: Annotated[str, "The input string to parse"]
    output: Annotated[hydra.parsing.ParseResult[A], "The expected parse result"]

    TYPE_ = hydra.core.Name("hydra.testing.ParserTestCase")
    INPUT = hydra.core.Name("input")
    OUTPUT = hydra.core.Name("output")

class Tag(Node[str]):
    r"""A tag for categorizing test cases."""

Tag.TYPE_ = hydra.core.Name("hydra.testing.Tag")

@dataclass(frozen=True)
class TestCodec:
    r"""A codec for generating compiled test files from test groups into a target programming language."""

    language: Annotated[hydra.coders.LanguageName, "The name of the target programming language"]
    file_extension: Annotated[hydra.module.FileExtension, "The file extension for test files (e.g., 'hs', 'java', 'py')"]
    encode_term: Annotated[Callable[[hydra.core.Term, hydra.graph.Graph], Either[str, str]], "A function for encoding Hydra terms into the target language"]
    encode_type: Annotated[Callable[[hydra.core.Type, hydra.graph.Graph], Either[str, str]], "A function for encoding Hydra types into the target language"]
    format_test_name: Annotated[Callable[[str], str], "A function for formatting test case names according to the target language's conventions"]
    format_module_name: Annotated[Callable[[hydra.module.Namespace], str], "A function for formatting module names according to the target language's conventions"]
    test_case_template: Annotated[str, "A template string for individual test case assertions"]
    test_group_template: Annotated[str, "A template string for wrapping a group of test cases"]
    module_template: Annotated[str, "A template string for the overall test module structure"]
    import_template: Annotated[str, "A template string for import/include statements"]
    find_imports: Annotated[Callable[[frozenset[hydra.core.Name]], frozenlist[str]], "A function that determines the necessary imports for a given set of dependencies"]

    TYPE_ = hydra.core.Name("hydra.testing.TestCodec")
    LANGUAGE = hydra.core.Name("language")
    FILE_EXTENSION = hydra.core.Name("fileExtension")
    ENCODE_TERM = hydra.core.Name("encodeTerm")
    ENCODE_TYPE = hydra.core.Name("encodeType")
    FORMAT_TEST_NAME = hydra.core.Name("formatTestName")
    FORMAT_MODULE_NAME = hydra.core.Name("formatModuleName")
    TEST_CASE_TEMPLATE = hydra.core.Name("testCaseTemplate")
    TEST_GROUP_TEMPLATE = hydra.core.Name("testGroupTemplate")
    MODULE_TEMPLATE = hydra.core.Name("moduleTemplate")
    IMPORT_TEMPLATE = hydra.core.Name("importTemplate")
    FIND_IMPORTS = hydra.core.Name("findImports")

@dataclass(frozen=True)
class TestGenerator(Generic[A]):
    r"""A language-agnostic test generator abstraction, parameterized by the namespace/module name type."""

    namespaces_for_module: Annotated[Callable[[hydra.module.Module, hydra.graph.Graph], Either[str, hydra.module.Namespaces[A]]], "Build namespaces for a module, resolving all imports and primitives"]
    create_codec: Annotated[Callable[[hydra.module.Namespaces[A]], TestCodec], "Create a test codec from resolved namespaces"]
    generate_test_file: Annotated[Callable[[hydra.module.Module, TestGroup, hydra.graph.Graph], Either[str, tuple[str, str]]], "Generate a complete test file for a module and test group"]
    aggregator_file: Annotated[Maybe[Callable[[str, frozenlist[hydra.module.Module]], tuple[str, str]]], "Generate an aggregator file (e.g., Spec.hs for Haskell, conftest.py for Python). Takes base directory and list of modules, returns (filepath, content) or Nothing if not needed"]

    TYPE_ = hydra.core.Name("hydra.testing.TestGenerator")
    NAMESPACES_FOR_MODULE = hydra.core.Name("namespacesForModule")
    CREATE_CODEC = hydra.core.Name("createCodec")
    GENERATE_TEST_FILE = hydra.core.Name("generateTestFile")
    AGGREGATOR_FILE = hydra.core.Name("aggregatorFile")

class TestCaseAlphaConversion(Node["AlphaConversionTestCase"]):
    r"""An alpha conversion test"""

class TestCaseCaseConversion(Node["CaseConversionTestCase"]):
    r"""A case conversion test"""

class TestCaseDeannotateTerm(Node["DeannotateTermTestCase"]):
    r"""A deannotate term test"""

class TestCaseDeannotateType(Node["DeannotateTypeTestCase"]):
    r"""A deannotate type test"""

class TestCaseDelegatedEvaluation(Node["DelegatedEvaluationTestCase"]):
    r"""DEPRECATED: Delegated evaluation test (to be removed)"""

class TestCaseEtaExpansion(Node["EtaExpansionTestCase"]):
    r"""An eta expansion test"""

class TestCaseFlattenLetTerms(Node["FlattenLetTermsTestCase"]):
    r"""A flatten let terms test"""

class TestCaseFreeVariables(Node["FreeVariablesTestCase"]):
    r"""A free variables test"""

class TestCaseEvaluation(Node["EvaluationTestCase"]):
    r"""A term evaluation test"""

class TestCaseInference(Node["InferenceTestCase"]):
    r"""A type inference test"""

class TestCaseInferenceFailure(Node["InferenceFailureTestCase"]):
    r"""A type inference failure test"""

class TestCaseJsonDecode(Node["JsonDecodeTestCase"]):
    r"""A JSON decode test using Either-based decoder"""

class TestCaseJsonEncode(Node["JsonEncodeTestCase"]):
    r"""A JSON encode test using Either-based encoder"""

class TestCaseJsonParser(Node["JsonParserTestCase"]):
    r"""A JSON parser test"""

class TestCaseJsonRoundtrip(Node["JsonRoundtripTestCase"]):
    r"""A JSON round-trip test using Either-based encoder/decoder"""

class TestCaseJsonWriter(Node["JsonWriterTestCase"]):
    r"""A JSON writer test"""

class TestCaseLiftLambdaAboveLet(Node["LiftLambdaAboveLetTestCase"]):
    r"""A lift lambda above let test"""

class TestCaseSerialization(Node["SerializationTestCase"]):
    r"""An AST serialization test"""

class TestCaseSimplifyTerm(Node["SimplifyTermTestCase"]):
    r"""A simplify term test"""

class TestCaseTopologicalSort(Node["TopologicalSortTestCase"]):
    r"""A topological sort test"""

class TestCaseTopologicalSortBindings(Node["TopologicalSortBindingsTestCase"]):
    r"""A topological sort bindings test"""

class TestCaseTopologicalSortSCC(Node["TopologicalSortSCCTestCase"]):
    r"""A topological sort with SCC detection test"""

class TestCaseTypeChecking(Node["TypeCheckingTestCase"]):
    r"""A type checking test"""

class TestCaseTypeCheckingFailure(Node["TypeCheckingFailureTestCase"]):
    r"""A type checking failure test (currently unused)"""

class TestCaseTypeReduction(Node["TypeReductionTestCase"]):
    r"""A type reduction test"""

class TestCaseNormalizeTypeVariables(Node["NormalizeTypeVariablesTestCase"]):
    r"""A normalize type variables test"""

class TestCaseFoldOverTerm(Node["FoldOverTermTestCase"]):
    r"""A fold over term test"""

class TestCaseRewriteTerm(Node["RewriteTermTestCase"]):
    r"""A rewrite term test"""

class TestCaseRewriteType(Node["RewriteTypeTestCase"]):
    r"""A rewrite type test"""

class TestCaseHoistSubterms(Node["HoistSubtermsTestCase"]):
    r"""A hoist subterms test"""

class TestCaseHoistCaseStatements(Node["HoistCaseStatementsTestCase"]):
    r"""A hoist case statements test"""

class TestCaseHoistLetBindings(Node["HoistLetBindingsTestCase"]):
    r"""A hoist all let bindings test (hoistAll=True, for Java)"""

class TestCaseHoistPolymorphicLetBindings(Node["HoistPolymorphicLetBindingsTestCase"]):
    r"""A hoist polymorphic let bindings test"""

class TestCaseSubstInType(Node["SubstInTypeTestCase"]):
    r"""A type substitution test"""

class TestCaseVariableOccursInType(Node["VariableOccursInTypeTestCase"]):
    r"""An occur check test for type unification"""

class TestCaseUnifyTypes(Node["UnifyTypesTestCase"]):
    r"""A type unification test"""

class TestCaseJoinTypes(Node["JoinTypesTestCase"]):
    r"""A join types test (produce type constraints)"""

class TestCaseUnshadowVariables(Node["UnshadowVariablesTestCase"]):
    r"""An unshadow variables test"""

class TestCaseValidateCoreTerm(Node["ValidateCoreTermTestCase"]):
    r"""A core term validation test"""

class TestCaseUniversal(Node["UniversalTestCase"]):
    r"""A universal test case (string comparison)"""

class _TestCaseMeta(type):
    def __getitem__(cls, item):
        return object

# A simple test case with an input and an expected output.
class TestCase(metaclass=_TestCaseMeta):
    r"""TestCaseAlphaConversion | TestCaseCaseConversion | TestCaseDeannotateTerm | TestCaseDeannotateType | TestCaseDelegatedEvaluation | TestCaseEtaExpansion | TestCaseFlattenLetTerms | TestCaseFreeVariables | TestCaseEvaluation | TestCaseInference | TestCaseInferenceFailure | TestCaseJsonDecode | TestCaseJsonEncode | TestCaseJsonParser | TestCaseJsonRoundtrip | TestCaseJsonWriter | TestCaseLiftLambdaAboveLet | TestCaseSerialization | TestCaseSimplifyTerm | TestCaseTopologicalSort | TestCaseTopologicalSortBindings | TestCaseTopologicalSortSCC | TestCaseTypeChecking | TestCaseTypeCheckingFailure | TestCaseTypeReduction | TestCaseNormalizeTypeVariables | TestCaseFoldOverTerm | TestCaseRewriteTerm | TestCaseRewriteType | TestCaseHoistSubterms | TestCaseHoistCaseStatements | TestCaseHoistLetBindings | TestCaseHoistPolymorphicLetBindings | TestCaseSubstInType | TestCaseVariableOccursInType | TestCaseUnifyTypes | TestCaseJoinTypes | TestCaseUnshadowVariables | TestCaseValidateCoreTerm | TestCaseUniversal"""

    TYPE_ = hydra.core.Name("hydra.testing.TestCase")
    ALPHA_CONVERSION = hydra.core.Name("alphaConversion")
    CASE_CONVERSION = hydra.core.Name("caseConversion")
    DEANNOTATE_TERM = hydra.core.Name("deannotateTerm")
    DEANNOTATE_TYPE = hydra.core.Name("deannotateType")
    DELEGATED_EVALUATION = hydra.core.Name("delegatedEvaluation")
    ETA_EXPANSION = hydra.core.Name("etaExpansion")
    FLATTEN_LET_TERMS = hydra.core.Name("flattenLetTerms")
    FREE_VARIABLES = hydra.core.Name("freeVariables")
    EVALUATION = hydra.core.Name("evaluation")
    INFERENCE = hydra.core.Name("inference")
    INFERENCE_FAILURE = hydra.core.Name("inferenceFailure")
    JSON_DECODE = hydra.core.Name("jsonDecode")
    JSON_ENCODE = hydra.core.Name("jsonEncode")
    JSON_PARSER = hydra.core.Name("jsonParser")
    JSON_ROUNDTRIP = hydra.core.Name("jsonRoundtrip")
    JSON_WRITER = hydra.core.Name("jsonWriter")
    LIFT_LAMBDA_ABOVE_LET = hydra.core.Name("liftLambdaAboveLet")
    SERIALIZATION = hydra.core.Name("serialization")
    SIMPLIFY_TERM = hydra.core.Name("simplifyTerm")
    TOPOLOGICAL_SORT = hydra.core.Name("topologicalSort")
    TOPOLOGICAL_SORT_BINDINGS = hydra.core.Name("topologicalSortBindings")
    TOPOLOGICAL_SORT_S_C_C = hydra.core.Name("topologicalSortSCC")
    TYPE_CHECKING = hydra.core.Name("typeChecking")
    TYPE_CHECKING_FAILURE = hydra.core.Name("typeCheckingFailure")
    TYPE_REDUCTION = hydra.core.Name("typeReduction")
    NORMALIZE_TYPE_VARIABLES = hydra.core.Name("normalizeTypeVariables")
    FOLD_OVER_TERM = hydra.core.Name("foldOverTerm")
    REWRITE_TERM = hydra.core.Name("rewriteTerm")
    REWRITE_TYPE = hydra.core.Name("rewriteType")
    HOIST_SUBTERMS = hydra.core.Name("hoistSubterms")
    HOIST_CASE_STATEMENTS = hydra.core.Name("hoistCaseStatements")
    HOIST_LET_BINDINGS = hydra.core.Name("hoistLetBindings")
    HOIST_POLYMORPHIC_LET_BINDINGS = hydra.core.Name("hoistPolymorphicLetBindings")
    SUBST_IN_TYPE = hydra.core.Name("substInType")
    VARIABLE_OCCURS_IN_TYPE = hydra.core.Name("variableOccursInType")
    UNIFY_TYPES = hydra.core.Name("unifyTypes")
    JOIN_TYPES = hydra.core.Name("joinTypes")
    UNSHADOW_VARIABLES = hydra.core.Name("unshadowVariables")
    VALIDATE_CORE_TERM = hydra.core.Name("validateCoreTerm")
    UNIVERSAL = hydra.core.Name("universal")

@dataclass(frozen=True)
class TestCaseWithMetadata:
    r"""One of a number of test case variants, together with metadata including a test name, an optional description, and optional tags."""

    name: Annotated[str, "The name of the test case"]
    case: Annotated[TestCase, "The test case itself"]
    description: Annotated[Maybe[str], "An optional description of the test"]
    tags: Annotated[frozenlist[Tag], "Zero or more tags for categorizing the test"]

    TYPE_ = hydra.core.Name("hydra.testing.TestCaseWithMetadata")
    NAME = hydra.core.Name("name")
    CASE = hydra.core.Name("case")
    DESCRIPTION = hydra.core.Name("description")
    TAGS = hydra.core.Name("tags")

@dataclass(frozen=True)
class TestGroup:
    r"""A collection of test cases with a name and optional description."""

    name: Annotated[str, "The name of the test group"]
    description: Annotated[Maybe[str], "An optional description of the group"]
    subgroups: Annotated[frozenlist[TestGroup], "Nested test groups"]
    cases: Annotated[frozenlist[TestCaseWithMetadata], "The test cases in this group"]

    TYPE_ = hydra.core.Name("hydra.testing.TestGroup")
    NAME = hydra.core.Name("name")
    DESCRIPTION = hydra.core.Name("description")
    SUBGROUPS = hydra.core.Name("subgroups")
    CASES = hydra.core.Name("cases")

@dataclass(frozen=True)
class TypeCheckingTestCase:
    r"""A test case which performs type checking on a given term and compares the result with an expected annotated term and type."""

    input: Annotated[hydra.core.Term, "An untyped term on which to perform inference, then type check"]
    output_term: Annotated[hydra.core.Term, "The expected fully annotated System F term after type inference"]
    output_type: Annotated[hydra.core.Type, "The expected inferred type"]

    TYPE_ = hydra.core.Name("hydra.testing.TypeCheckingTestCase")
    INPUT = hydra.core.Name("input")
    OUTPUT_TERM = hydra.core.Name("outputTerm")
    OUTPUT_TYPE = hydra.core.Name("outputType")

@dataclass(frozen=True)
class TypeCheckingFailureTestCase:
    r"""A test case providing a term for which type checking is expected to fail. Note: there are currently no such test cases."""

    input: Annotated[hydra.core.Term, "The term for which type checking should fail"]

    TYPE_ = hydra.core.Name("hydra.testing.TypeCheckingFailureTestCase")
    INPUT = hydra.core.Name("input")

@dataclass(frozen=True)
class TopologicalSortBindingsTestCase:
    r"""A test case which performs topological sort on a map of bindings (name -> term) and compares the result with expected groups of bindings in topological order."""

    bindings: Annotated[frozenlist[tuple[hydra.core.Name, hydra.core.Term]], "The bindings as a list of (name, term) pairs"]
    expected: Annotated[frozenlist[frozenlist[tuple[hydra.core.Name, hydra.core.Term]]], "The expected groups of bindings in topological order"]

    TYPE_ = hydra.core.Name("hydra.testing.TopologicalSortBindingsTestCase")
    BINDINGS = hydra.core.Name("bindings")
    EXPECTED = hydra.core.Name("expected")

@dataclass(frozen=True)
class TopologicalSortTestCase:
    r"""A test case which performs topological sort on a directed graph and compares the result with either an expected sorted list or expected cycles."""

    adjacency_list: Annotated[frozenlist[tuple[int, frozenlist[int]]], "The directed graph as an adjacency list (node to list of dependencies)"]
    expected: Annotated[Either[frozenlist[frozenlist[int]], frozenlist[int]], "The expected result: Left for cycles, Right for sorted nodes"]

    TYPE_ = hydra.core.Name("hydra.testing.TopologicalSortTestCase")
    ADJACENCY_LIST = hydra.core.Name("adjacencyList")
    EXPECTED = hydra.core.Name("expected")

@dataclass(frozen=True)
class TopologicalSortSCCTestCase:
    r"""A test case which performs topological sort with strongly connected component detection and compares the result with expected components."""

    adjacency_list: Annotated[frozenlist[tuple[int, frozenlist[int]]], "The directed graph as an adjacency list"]
    expected: Annotated[frozenlist[frozenlist[int]], "The expected strongly connected components in topological order"]

    TYPE_ = hydra.core.Name("hydra.testing.TopologicalSortSCCTestCase")
    ADJACENCY_LIST = hydra.core.Name("adjacencyList")
    EXPECTED = hydra.core.Name("expected")

@dataclass(frozen=True)
class SerializationTestCase:
    r"""A test case which serializes an AST expression to a string and compares it with the expected output."""

    input: Annotated[hydra.ast.Expr, "The AST expression to serialize"]
    output: Annotated[str, "The expected serialized string"]

    TYPE_ = hydra.core.Name("hydra.testing.SerializationTestCase")
    INPUT = hydra.core.Name("input")
    OUTPUT = hydra.core.Name("output")

@dataclass(frozen=True)
class SimplifyTermTestCase:
    r"""A test case which performs term simplification (beta reduction and optimization) and compares the result with the expected term."""

    input: Annotated[hydra.core.Term, "The term to simplify"]
    output: Annotated[hydra.core.Term, "The expected simplified term"]

    TYPE_ = hydra.core.Name("hydra.testing.SimplifyTermTestCase")
    INPUT = hydra.core.Name("input")
    OUTPUT = hydra.core.Name("output")

@dataclass(frozen=True)
class NormalizeTypeVariablesTestCase:
    r"""A test case which normalizes type variables in a term (renaming them to t0, t1, t2, etc.) and compares the result with the expected term."""

    input: Annotated[hydra.core.Term, "The term with type annotations to normalize"]
    output: Annotated[hydra.core.Term, "The expected term with normalized type variable names"]

    TYPE_ = hydra.core.Name("hydra.testing.NormalizeTypeVariablesTestCase")
    INPUT = hydra.core.Name("input")
    OUTPUT = hydra.core.Name("output")

@dataclass(frozen=True)
class TypeReductionTestCase:
    r"""A test case which performs beta reduction on a type (reducing type applications) and compares the result with the expected type."""

    input: Annotated[hydra.core.Type, "The type to reduce"]
    output: Annotated[hydra.core.Type, "The expected reduced type"]

    TYPE_ = hydra.core.Name("hydra.testing.TypeReductionTestCase")
    INPUT = hydra.core.Name("input")
    OUTPUT = hydra.core.Name("output")

@dataclass(frozen=True)
class WriterTestCase(Generic[A]):
    r"""A test case which writes a value to a string and compares it to the expected string."""

    input: Annotated[A, "The input value to write"]
    output: Annotated[str, "The expected string"]

    TYPE_ = hydra.core.Name("hydra.testing.WriterTestCase")
    INPUT = hydra.core.Name("input")
    OUTPUT = hydra.core.Name("output")

@dataclass(frozen=True)
class SubstInTypeTestCase:
    r"""A test case which applies a type substitution to a type and compares the result. The substitution is provided as a list of (variable name, replacement type) pairs."""

    substitution: Annotated[frozenlist[tuple[hydra.core.Name, hydra.core.Type]], "The type substitution as a list of (name, type) pairs"]
    input: Annotated[hydra.core.Type, "The type to substitute into"]
    output: Annotated[hydra.core.Type, "The expected result type"]

    TYPE_ = hydra.core.Name("hydra.testing.SubstInTypeTestCase")
    SUBSTITUTION = hydra.core.Name("substitution")
    INPUT = hydra.core.Name("input")
    OUTPUT = hydra.core.Name("output")

@dataclass(frozen=True)
class VariableOccursInTypeTestCase:
    r"""A test case which checks whether a type variable occurs in a type expression. This is the occur check used in type unification."""

    variable: Annotated[hydra.core.Name, "The variable name to search for"]
    type: Annotated[hydra.core.Type, "The type to search within"]
    expected: Annotated[bool, "Whether the variable occurs in the type"]

    TYPE_ = hydra.core.Name("hydra.testing.VariableOccursInTypeTestCase")
    VARIABLE = hydra.core.Name("variable")
    TYPE = hydra.core.Name("type")
    EXPECTED = hydra.core.Name("expected")

@dataclass(frozen=True)
class UnshadowVariablesTestCase:
    r"""A test case which renames shadowed variables in a term and compares the result with the expected term."""

    input: Annotated[hydra.core.Term, "The term with potentially shadowed variables"]
    output: Annotated[hydra.core.Term, "The expected term after unshadowing"]

    TYPE_ = hydra.core.Name("hydra.testing.UnshadowVariablesTestCase")
    INPUT = hydra.core.Name("input")
    OUTPUT = hydra.core.Name("output")

@dataclass(frozen=True)
class UnifyTypesTestCase:
    r"""A test case which attempts to unify two types and compares the result. The expected result is either Left (failure message substring) or Right (substitution)."""

    schema_types: Annotated[frozenlist[hydra.core.Name], "The schema types map (type variable names that should not be bound)"]
    left: Annotated[hydra.core.Type, "The left type to unify"]
    right: Annotated[hydra.core.Type, "The right type to unify"]
    expected: Annotated[Either[str, hydra.typing.TypeSubst], "The expected result: Left for failure (substring of error), Right for substitution"]

    TYPE_ = hydra.core.Name("hydra.testing.UnifyTypesTestCase")
    SCHEMA_TYPES = hydra.core.Name("schemaTypes")
    LEFT = hydra.core.Name("left")
    RIGHT = hydra.core.Name("right")
    EXPECTED = hydra.core.Name("expected")

@dataclass(frozen=True)
class JoinTypesTestCase:
    r"""A test case which joins two types (producing type constraints or failing). The expected result is either Left (failure) or Right (list of constraints)."""

    left: Annotated[hydra.core.Type, "The left type to join"]
    right: Annotated[hydra.core.Type, "The right type to join"]
    expected: Annotated[Either[None, frozenlist[hydra.typing.TypeConstraint]], "The expected result: Left for failure, Right for constraints"]

    TYPE_ = hydra.core.Name("hydra.testing.JoinTypesTestCase")
    LEFT = hydra.core.Name("left")
    RIGHT = hydra.core.Name("right")
    EXPECTED = hydra.core.Name("expected")

@dataclass(frozen=True)
class ValidateCoreTermTestCase:
    r"""A test case which validates a term and compares the result with an expected Maybe InvalidTermError."""

    typed: Annotated[bool, "Whether to expect System F (typed) terms. When true, type variable binding checks and UntypedTermVariableError are active."]
    input: Annotated[hydra.core.Term, "The term to validate"]
    output: Annotated[Maybe[hydra.error.core.InvalidTermError], "The expected validation result (Nothing if valid, Just error if invalid)"]

    TYPE_ = hydra.core.Name("hydra.testing.ValidateCoreTermTestCase")
    TYPED = hydra.core.Name("typed")
    INPUT = hydra.core.Name("input")
    OUTPUT = hydra.core.Name("output")

@dataclass(frozen=True)
class UniversalTestCase:
    r"""A universal test case: the actual and expected values are both strings."""

    actual: Annotated[str, "The actual result (a string-valued expression)"]
    expected: Annotated[str, "The expected result (a string literal)"]

    TYPE_ = hydra.core.Name("hydra.testing.UniversalTestCase")
    ACTUAL = hydra.core.Name("actual")
    EXPECTED = hydra.core.Name("expected")
