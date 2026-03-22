-- | Java test code generation codec in Hydra DSL.
-- This module provides DSL versions of Java test codec functions for JUnit-based generation tests.

module Hydra.Ext.Sources.Java.TestCodec where

-- Standard imports for term-level sources outside of the kernel
import Hydra.Kernel
import Hydra.Sources.Libraries
import           Hydra.Dsl.Meta.Lib.Strings                as Strings
import           Hydra.Dsl.Meta.Phantoms                   as Phantoms
import qualified Hydra.Dsl.Annotations                     as Annotations
import qualified Hydra.Dsl.Bootstrap                       as Bootstrap
import qualified Hydra.Dsl.Grammars                        as Grammars
import qualified Hydra.Dsl.LiteralTypes                    as LiteralTypes
import qualified Hydra.Dsl.Literals                        as Literals
import qualified Hydra.Dsl.Accessors                  as Accessors
import qualified Hydra.Dsl.Ast                        as Ast
import qualified Hydra.Dsl.Meta.Base                       as MetaBase
import qualified Hydra.Dsl.Coders                     as Coders
import qualified Hydra.Dsl.Util                    as Util
import qualified Hydra.Dsl.Meta.Context                    as Ctx
import qualified Hydra.Dsl.Meta.Core                       as Core
import qualified Hydra.Dsl.Error                      as Error
import qualified Hydra.Dsl.Grammar                    as Grammar
import qualified Hydra.Dsl.Meta.Graph                      as Graph
import qualified Hydra.Dsl.Json.Model                       as Json
import qualified Hydra.Dsl.Meta.Lib.Chars                  as Chars
import qualified Hydra.Dsl.Meta.Lib.Eithers                as Eithers
import qualified Hydra.Dsl.Meta.Lib.Equality               as Equality
import qualified Hydra.Dsl.Meta.Lib.Lists                  as Lists
import qualified Hydra.Dsl.Meta.Lib.Literals               as Literals
import qualified Hydra.Dsl.Meta.Lib.Logic                  as Logic
import qualified Hydra.Dsl.Meta.Lib.Maps                   as Maps
import qualified Hydra.Dsl.Meta.Lib.Math                   as Math
import qualified Hydra.Dsl.Meta.Lib.Maybes                 as Maybes
import qualified Hydra.Dsl.Meta.Lib.Pairs                  as Pairs
import qualified Hydra.Dsl.Meta.Lib.Sets                   as Sets
import qualified Hydra.Dsl.Module                     as Module
import qualified Hydra.Dsl.Meta.Terms                      as MetaTerms
import qualified Hydra.Dsl.Meta.Testing                    as Testing
import qualified Hydra.Dsl.Topology                   as Topology
import qualified Hydra.Dsl.Meta.Types                      as MetaTypes
import qualified Hydra.Dsl.Typing                     as Typing
import qualified Hydra.Dsl.Util                       as Util
import qualified Hydra.Dsl.Meta.Variants                   as Variants
import qualified Hydra.Dsl.Prims                           as Prims
import qualified Hydra.Dsl.Meta.Tabular                         as Tabular
import qualified Hydra.Dsl.Terms                           as Terms
import qualified Hydra.Dsl.Tests                           as Tests
import qualified Hydra.Dsl.Types                           as Types
import qualified Hydra.Sources.Decode.Core                 as DecodeCore
import qualified Hydra.Sources.Encode.Core                 as EncodeCore
import qualified Hydra.Sources.Kernel.Terms.Adapt           as Adapt
import qualified Hydra.Sources.Kernel.Terms.All            as KernelTerms
import qualified Hydra.Sources.Kernel.Terms.Annotations    as Annotations
import qualified Hydra.Sources.Kernel.Terms.Arity          as Arity
import qualified Hydra.Sources.Kernel.Terms.Checking       as Checking
import qualified Hydra.Sources.Kernel.Terms.Constants      as Constants
import qualified Hydra.Sources.Kernel.Terms.Extract.Core   as ExtractCore
import qualified Hydra.Sources.Kernel.Terms.Extract.Util   as ExtractUtil
import qualified Hydra.Sources.Kernel.Terms.Formatting     as Formatting
import qualified Hydra.Sources.Kernel.Terms.Grammars       as Grammars
import qualified Hydra.Sources.Kernel.Terms.Inference      as Inference
import qualified Hydra.Sources.Kernel.Terms.Languages      as Languages
import qualified Hydra.Sources.Kernel.Terms.Lexical        as Lexical
import qualified Hydra.Sources.Kernel.Terms.Literals       as Literals
import qualified Hydra.Sources.Kernel.Terms.Names          as Names
import qualified Hydra.Sources.Kernel.Terms.Reduction      as Reduction
import qualified Hydra.Sources.Kernel.Terms.Reflect        as Reflect
import qualified Hydra.Sources.Kernel.Terms.Rewriting      as Rewriting
import qualified Hydra.Sources.Kernel.Terms.Schemas        as Schemas
import qualified Hydra.Sources.Kernel.Terms.Serialization  as Serialization
import qualified Hydra.Sources.Kernel.Terms.Show.Accessors as ShowAccessors
import qualified Hydra.Sources.Kernel.Terms.Show.Core      as ShowCore
import qualified Hydra.Sources.Kernel.Terms.Show.Error     as ShowError
import qualified Hydra.Sources.Kernel.Terms.Show.Graph     as ShowGraph
import qualified Hydra.Sources.Kernel.Terms.Show.Meta      as ShowMeta
import qualified Hydra.Sources.Kernel.Terms.Show.Typing    as ShowTyping
import qualified Hydra.Sources.Kernel.Terms.Sorting        as Sorting
import qualified Hydra.Sources.Kernel.Terms.Substitution   as Substitution
import qualified Hydra.Sources.Kernel.Terms.Templates      as Templates
import qualified Hydra.Sources.Kernel.Terms.Unification    as Unification
import qualified Hydra.Sources.Kernel.Types.All            as KernelTypes
import           Prelude hiding ((++))
import qualified Data.Int                                  as I
import qualified Data.List                                 as L
import qualified Data.Map                                  as M
import qualified Data.Set                                  as S
import qualified Data.Maybe                                as Y

-- Additional imports
import qualified Hydra.Ext.Java.Syntax as Java
import qualified Hydra.Ext.Java.Helpers as JavaHelpers
import qualified Hydra.Ext.Sources.Java.Syntax as JavaSyntax
import qualified Hydra.Ext.Sources.Java.Helpers as JavaHelpersSource
import qualified Hydra.Ext.Sources.Java.Coder as JavaCoderSource
import qualified Hydra.Ext.Sources.Java.Serde as JavaSerdeSource
import qualified Hydra.Sources.Test.Utils as TestUtils
import qualified Hydra.Sources.Kernel.Terms.Serialization  as SerializationSource
import Hydra.Testing (TestCodec(..))
import Hydra.Coders (LanguageName(..))


define :: String -> TTerm a -> TBinding a
define = definitionInModule module_


ns :: Namespace
ns = Namespace "hydra.ext.java.testCodec"

module_ :: Module
module_ = Module ns elements
    [JavaCoderSource.ns, JavaSerdeSource.ns, SerializationSource.ns, TestUtils.ns, Formatting.ns, Names.ns, Inference.ns, Constants.ns, Rewriting.ns, ShowError.ns]
    (JavaHelpersSource.ns:JavaSyntax.ns:KernelTypes.kernelTypesNamespaces) $
    Just "Java test code generation codec for JUnit-based generation tests"
  where
    elements = [
      toBinding termToJava,
      toBinding typeToJava,
      toBinding javaTestCodec,
      toBinding formatJavaTestName,
      toBinding namespaceToJavaClassName,
      toBinding javaTestCaseTemplate,
      toBinding javaTestGroupTemplate,
      toBinding javaModuleTemplate,
      toBinding javaImportTemplate,
      toBinding findJavaImports,
      toBinding generateJavaTestGroupHierarchy,
      toBinding generateJavaTestCase,
      toBinding getAssertionType,
      toBinding generateAssertion,
      toBinding isInferenceVar,
      toBinding generateTestFileWithJavaCodec,
      toBinding buildJavaTestModule,
      toBinding generateJavaTestFile]


-- | Convert a Hydra term to a Java expression string
termToJava :: TBinding (Term -> Graph -> Either String String)
termToJava = define "termToJava" $
  doc "Convert a Hydra term to a Java expression string" $
  lambda "term" $ lambda "g" $
    Eithers.bimap
      ("ic" ~> ShowError.error_ @@ Ctx.inContextObject (var "ic"))
      (Serialization.printExpr <.> Serialization.parenthesize <.> JavaSerdeSource.writeExpression)
      (JavaCoderSource.encodeTerm
        @@ (record JavaHelpers._JavaEnvironment [
              JavaHelpers._JavaEnvironment_aliases>>:
                record JavaHelpers._Aliases [
                  JavaHelpers._Aliases_currentNamespace>>: wrap _Namespace (string "test"),
                  JavaHelpers._Aliases_packages>>: Maps.empty,
                  JavaHelpers._Aliases_branchVars>>: Sets.empty,
                  JavaHelpers._Aliases_recursiveVars>>: Sets.empty,
                  JavaHelpers._Aliases_inScopeTypeParams>>: Sets.empty,
                  JavaHelpers._Aliases_polymorphicLocals>>: Sets.empty,
                  JavaHelpers._Aliases_inScopeJavaVars>>: Sets.empty,
                  JavaHelpers._Aliases_varRenames>>: Maps.empty,
                  JavaHelpers._Aliases_lambdaVars>>: Sets.empty,
                  JavaHelpers._Aliases_typeVarSubst>>: Maps.empty,
                  JavaHelpers._Aliases_trustedTypeVars>>: Sets.empty,
                  JavaHelpers._Aliases_methodCodomain>>: nothing,
                  JavaHelpers._Aliases_thunkedVars>>: Sets.empty],
              JavaHelpers._JavaEnvironment_graph>>: var "g"])
        @@ var "term"
        @@ asTerm Lexical.emptyContext
        @@ var "g")


-- | Convert a Hydra type to a Java type expression string
typeToJava :: TBinding (Type -> Graph -> Either String String)
typeToJava = define "typeToJava" $
  doc "Convert a Hydra type to a Java type expression string (placeholder returning Object)" $
  lambda "_t" $ lambda "_g" $
    Phantoms.right (string "Object")


-- | Create a Java TestCodec
javaTestCodec :: TBinding TestCodec
javaTestCodec = define "javaTestCodec" $
  doc "Create a Java TestCodec for JUnit-based test generation" $
  record _TestCodec [
    _TestCodec_language>>: Coders.languageName_ (string "java"),
    _TestCodec_fileExtension>>: wrap _FileExtension (string "java"),
    _TestCodec_encodeTerm>>: asTerm termToJava,
    _TestCodec_encodeType>>: asTerm typeToJava,
    _TestCodec_formatTestName>>: asTerm formatJavaTestName,
    _TestCodec_formatModuleName>>: asTerm namespaceToJavaClassName,
    _TestCodec_testCaseTemplate>>: asTerm javaTestCaseTemplate,
    _TestCodec_testGroupTemplate>>: asTerm javaTestGroupTemplate,
    _TestCodec_moduleTemplate>>: asTerm javaModuleTemplate,
    _TestCodec_importTemplate>>: asTerm javaImportTemplate,
    _TestCodec_findImports>>: lambda "_names" $ asTerm findJavaImports]


-- | Format a test name for Java (camelCase method name)
-- Special characters are mapped to descriptive words to preserve uniqueness
formatJavaTestName :: TBinding (String -> String)
formatJavaTestName = define "formatJavaTestName" $
  doc "Format a test name for Java (PascalCase method name with 'test' prefix)" $
  lambda "name" $ lets [
    "replaced">: replaceChar (string "-") (string " Neg")
      (replaceChar (string ".") (string "Dot")
        (replaceChar (string "+") (string " Plus")
          (replaceChar (string "/") (string " Div")
            (replaceChar (string "*") (string " Mul")
              (replaceChar (string "#") (string " Num") (var "name")))))),
    "sanitized">: Formatting.nonAlnumToUnderscores @@ var "replaced",
    "pascal_">: Formatting.convertCase @@ Util.caseConventionLowerSnake @@ Util.caseConventionPascal @@ var "sanitized"] $
    Strings.cat2 (string "test") (var "pascal_")
  where
    replaceChar old new s = Strings.intercalate new (Strings.splitOn old s)


-- | Convert namespace to Java class name
namespaceToJavaClassName :: TBinding (Namespace -> String)
namespaceToJavaClassName = define "namespaceToJavaClassName" $
  doc "Convert namespace to Java class name" $
  lambda "ns_" $
    Strings.intercalate (string ".")
      (Lists.map Formatting.capitalize
        (Strings.splitOn (string ".") (unwrap _Namespace @@ var "ns_")))


-- | JUnit test case template
javaTestCaseTemplate :: TBinding String
javaTestCaseTemplate = define "javaTestCaseTemplate" $
  doc "Template for JUnit test case assertions" $
  Strings.intercalate (string "\n") (list [
    string "    @Test",
    string "    public void {name}() {",
    string "        assertEquals({output}, {input});",
    string "    }"])


-- | JUnit test group template
javaTestGroupTemplate :: TBinding String
javaTestGroupTemplate = define "javaTestGroupTemplate" $
  doc "Template for JUnit test group comments" $
  string "// {groupName}"


-- | JUnit module template
javaModuleTemplate :: TBinding String
javaModuleTemplate = define "javaModuleTemplate" $
  doc "Template for JUnit test module structure" $
  Strings.intercalate (string "\n") (list [
    Strings.cat2 (string "// ") (asTerm Constants.warningAutoGeneratedFile),
    string "",
    string "package {package};",
    string "",
    string "{imports}",
    string "",
    string "public class {className} {",
    string "    {testCases}",
    string "}"])


-- | JUnit import template
javaImportTemplate :: TBinding String
javaImportTemplate = define "javaImportTemplate" $
  doc "Template for Java import statements" $
  string "import {namespace};"


-- | Find necessary imports for Java test files
findJavaImports :: TBinding [String]
findJavaImports = define "findJavaImports" $
  doc "Standard imports for Java JUnit test files" $
  list [
    string "import org.junit.jupiter.api.Test;",
    string "import static org.junit.jupiter.api.Assertions.*;",
    string "import java.util.*;"]


-- | Determine the assertion type based on the output term structure
getAssertionType :: TBinding (Term -> String)
getAssertionType = define "getAssertionType" $
  doc "Determine the assertion type based on the output term structure (returns a string tag)" $
  lambda "term" $
    cases _Term (Rewriting.deannotateTerm @@ var "term") (Just (string "assertEquals")) [
      _Term_literal>>: lambda "lit" $
        cases _Literal (var "lit") (Just (string "assertEquals")) [
          _Literal_binary>>: lambda "_b" $ string "assertArrayEquals",
          _Literal_float>>: lambda "fv" $
            cases _FloatValue (var "fv") (Just (string "assertDoubleEquals")) [
              _FloatValue_bigfloat>>: lambda "_bf" $ string "assertBigDecimalEquals"]]]


-- | Generate the assertion code based on the assertion type
generateAssertion :: TBinding (String -> String -> String -> [String])
generateAssertion = define "generateAssertion" $
  doc "Generate assertion code lines based on assertion type, output code, and input code" $
  lambda "assertType" $ lambda "outputCode" $ lambda "inputCode" $
    Logic.ifElse (Equality.equal (var "assertType") (string "assertArrayEquals"))
      (list [
        string "        assertArrayEquals(",
        Strings.cat (list [string "            ", var "outputCode", string ","]),
        Strings.cat (list [string "            ", var "inputCode", string ");"])])
      (Logic.ifElse (Equality.equal (var "assertType") (string "assertBigDecimalEquals"))
        (list [
          Strings.cat (list [string "        assertEquals(0, (", var "outputCode", string ").compareTo(", var "inputCode", string "));"])])
        (Logic.ifElse (Equality.equal (var "assertType") (string "assertDoubleEquals"))
          (list [
            string "        assertEquals(",
            Strings.cat (list [string "            ", var "outputCode", string ","]),
            Strings.cat (list [string "            ", var "inputCode", string ","]),
            string "            1e-15);"])
          (list [
            string "        assertEquals(",
            Strings.cat (list [string "            ", var "outputCode", string ","]),
            Strings.cat (list [string "            ", var "inputCode", string ");"])])))


-- | Check if a Name is an unresolved inference variable (matches pattern t\d+)
isInferenceVar :: TBinding (Name -> Bool)
isInferenceVar = define "isInferenceVar" $
  doc "Check if a Name is an unresolved inference variable (matches pattern t followed by digits)" $
  lambda "n" $ lets [
    "s">: unwrap _Name @@ var "n",
    "chars">: Strings.toList (var "s")] $
    Logic.and
      (Equality.equal (Strings.charAt (int32 0) (var "s")) (int32 116))  -- 't' = 116
      (Logic.and
        (Logic.not (Equality.equal (Strings.length (var "s")) (int32 1)))
        (Lists.null (Lists.filter
          (lambda "c" $ Logic.not (Logic.and (Equality.gte (var "c") (int32 48)) (Equality.lte (var "c") (int32 57))))
          (Lists.drop (int32 1) (var "chars")))))


-- | Generate Java test group hierarchy
generateJavaTestGroupHierarchy :: TBinding (Graph -> TestCodec -> [String] -> TestGroup -> Either String String)
generateJavaTestGroupHierarchy = define "generateJavaTestGroupHierarchy" $
  doc "Generate test hierarchy for Java with nested subgroups" $
  lambda "g" $ lambda "codec" $ lambda "groupPath" $ lambda "testGroup" $ lets [
    "cases_">: project _TestGroup _TestGroup_cases @@ var "testGroup",
    "subgroups">: project _TestGroup _TestGroup_subgroups @@ var "testGroup"] $
    Eithers.bind
      (Eithers.map
        (lambda "lines_" $ Strings.intercalate (string "\n\n") (Lists.concat (var "lines_")))
        (Eithers.mapList
          (lambda "tc" $ generateJavaTestCase @@ var "g" @@ var "codec" @@ var "groupPath" @@ var "tc")
          (var "cases_")))
      (lambda "testCasesStr" $
        Eithers.map
          (lambda "subgroupsStr" $
            Strings.cat (list [
              var "testCasesStr",
              Logic.ifElse (Logic.or (Equality.equal (var "testCasesStr") (string ""))
                                      (Equality.equal (var "subgroupsStr") (string "")))
                (string "")
                (string "\n\n"),
              var "subgroupsStr"]))
          (Eithers.map
            (lambda "blocks" $ Strings.intercalate (string "\n\n") (var "blocks"))
            (Eithers.mapList
              (lambda "subgroup" $ lets [
                "groupName">: project _TestGroup _TestGroup_name @@ var "subgroup",
                "header">: Strings.cat2 (string "    // ") (var "groupName")] $
                Eithers.map
                  (lambda "content" $ Strings.cat (list [var "header", string "\n\n", var "content"]))
                  (generateJavaTestGroupHierarchy @@ var "g" @@ var "codec"
                    @@ (Lists.concat2 (var "groupPath") (list [var "groupName"]))
                    @@ var "subgroup"))
              (var "subgroups"))))


-- | Generate a single test case for Java/JUnit
generateJavaTestCase :: TBinding (Graph -> TestCodec -> [String] -> TestCaseWithMetadata -> Either String [String])
generateJavaTestCase = define "generateJavaTestCase" $
  doc "Generate a single JUnit test case from a test case with metadata" $
  lambda "g" $ lambda "codec" $ lambda "groupPath" $ lambda "tcm" $ lets [
    "name_">: project _TestCaseWithMetadata _TestCaseWithMetadata_name @@ var "tcm",
    "tcase">: project _TestCaseWithMetadata _TestCaseWithMetadata_case @@ var "tcm"] $
    cases _TestCase (var "tcase") (Just (Phantoms.right (list ([] :: [TTerm String])))) [
      _TestCase_delegatedEvaluation>>: lambda "delCase" $ lets [
        "input_">: project _DelegatedEvaluationTestCase _DelegatedEvaluationTestCase_input @@ var "delCase",
        "output_">: project _DelegatedEvaluationTestCase _DelegatedEvaluationTestCase_output @@ var "delCase",
        "fullName">: Logic.ifElse (Lists.null (var "groupPath"))
          (var "name_")
          (Strings.intercalate (string "_") (Lists.concat2 (var "groupPath") (list [var "name_"]))),
        "formattedName">: project _TestCodec _TestCodec_formatTestName @@ var "codec" @@ var "fullName",
        "assertType">: getAssertionType @@ var "output_"] $
        lets [
          "typeVars">: Lists.sort (Lists.filter (asTerm isInferenceVar)
            (Sets.toList (Sets.union
              (Rewriting.freeTypeVariablesInTerm @@ var "input_")
              (Rewriting.freeTypeVariablesInTerm @@ var "output_")))),
          "typeParamsStr">: Logic.ifElse (Lists.null (var "typeVars"))
            (string "")
            (Strings.cat (list [
              string "<",
              Strings.intercalate (string ", ") (Lists.map ("n_" ~> Formatting.capitalize @@ (unwrap _Name @@ var "n_")) (var "typeVars")),
              string "> "]))] $
          Eithers.bind
            (project _TestCodec _TestCodec_encodeTerm @@ var "codec" @@ var "input_" @@ var "g")
            (lambda "inputCode" $
              Eithers.map
                (lambda "outputCode" $ lets [
                  "assertionLines">: generateAssertion @@ var "assertType" @@ var "outputCode" @@ var "inputCode"] $
                  Lists.concat2
                    (list [
                      string "    @Test",
                      Strings.cat (list [string "    public ", var "typeParamsStr", string "void ", var "formattedName", string "() {"])])
                    (Lists.concat2 (var "assertionLines") (list [string "    }"])))
                (project _TestCodec _TestCodec_encodeTerm @@ var "codec" @@ var "output_" @@ var "g"))]


-- | Generate test file using Java codec
generateTestFileWithJavaCodec :: TBinding (TestCodec -> Module -> TestGroup -> Graph -> Either String (String, String))
generateTestFileWithJavaCodec = define "generateTestFileWithJavaCodec" $
  doc "Generate a complete test file using the Java codec" $
  lambda "codec" $ lambda "testModule" $ lambda "testGroup" $ lambda "g" $
    Eithers.map
      (lambda "testBody" $ lets [
        "testModuleContent">: buildJavaTestModule @@ var "codec" @@ var "testModule" @@ var "testGroup" @@ var "testBody",
        "ns_">: Module.moduleNamespace (var "testModule"),
        "parts">: Strings.splitOn (string ".") (unwrap _Namespace @@ var "ns_"),
        "dirParts">: Lists.drop (int32 1) (Lists.init (var "parts")),
        "className_">: Strings.cat2 (Formatting.capitalize @@ (Lists.last (var "parts"))) (string "Test"),
        "fileName">: Strings.cat2 (var "className_") (string ".java"),
        "filePath">: Strings.cat (list [Strings.intercalate (string "/") (var "dirParts"), string "/", var "fileName"])] $
        Phantoms.pair (var "filePath") (var "testModuleContent"))
      (generateJavaTestGroupHierarchy @@ var "g" @@ var "codec" @@ list ([] :: [TTerm String]) @@ var "testGroup")


-- | Build complete Java test module
buildJavaTestModule :: TBinding (TestCodec -> Module -> TestGroup -> String -> String)
buildJavaTestModule = define "buildJavaTestModule" $
  doc "Build the complete Java test module content" $
  lambda "codec" $ lambda "testModule" $ lambda "testGroup" $ lambda "testBody" $ lets [
    "ns_">: Module.moduleNamespace (var "testModule"),
    "parts">: Strings.splitOn (string ".") (unwrap _Namespace @@ var "ns_"),
    "packageName">: Strings.intercalate (string ".") (Lists.init (var "parts")),
    "className_">: Strings.cat2 (Formatting.capitalize @@ (Lists.last (var "parts"))) (string "Test"),
    "groupName_">: project _TestGroup _TestGroup_name @@ var "testGroup",
    "standardImports">: list [
      string "import org.junit.jupiter.api.Test;",
      string "import static org.junit.jupiter.api.Assertions.*;",
      string "import java.util.*;",
      string "import hydra.util.*;"],
    "header">: Strings.cat (list [
      Strings.cat2 (string "// ") (asTerm Constants.warningAutoGeneratedFile),
      string "\n",
      Strings.cat2 (string "// ") (var "groupName_"),
      string "\n\n",
      Strings.cat (list [string "package ", var "packageName", string ";\n\n"]),
      Strings.intercalate (string "\n") (var "standardImports"),
      string "\n\n",
      Strings.cat (list [string "public class ", var "className_", string " {\n\n"])])] $
    Strings.cat (list [var "header", var "testBody", string "\n}\n"])


-- | Generate Java test file for a test group
generateJavaTestFile :: TBinding (Module -> TestGroup -> Graph -> Either String (String, String))
generateJavaTestFile = define "generateJavaTestFile" $
  doc "Generate a Java test file for a test group, with type inference" $
  lambda "testModule" $ lambda "testGroup" $ lambda "g" $
    Eithers.bind
      (TestUtils.inferTestGroupTerms @@ var "g" @@ var "testGroup")
      (lambda "inferredTestGroup" $
        generateTestFileWithJavaCodec @@ (asTerm javaTestCodec) @@ var "testModule" @@ var "inferredTestGroup" @@ var "g")


