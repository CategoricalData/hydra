-- | Java test code generation codec in Hydra DSL.
-- This module provides DSL versions of Java test codec functions for JUnit-based generation tests.

module Hydra.Ext.Sources.Java.Testing where

-- Standard imports for term-level sources outside of the kernel
import Hydra.Kernel
import Hydra.Sources.Libraries
import           Hydra.Dsl.Meta.Lib.Strings                as Strings
import           Hydra.Dsl.Meta.Phantoms                   as Phantoms
import qualified Hydra.Dsl.Annotations                     as Annotations
import qualified Hydra.Dsl.Bootstrap                       as Bootstrap
import qualified Hydra.Dsl.LiteralTypes                    as LiteralTypes
import qualified Hydra.Dsl.Literals                        as Literals
import qualified Hydra.Dsl.Paths                      as Paths
import qualified Hydra.Dsl.Ast                        as Ast
import qualified Hydra.Dsl.Meta.Base                       as MetaBase
import qualified Hydra.Dsl.Coders                     as Coders
import qualified Hydra.Dsl.Util                    as Util
import qualified Hydra.Dsl.Meta.Context                    as Ctx
import qualified Hydra.Dsl.Meta.Core                       as Core
import qualified Hydra.Dsl.Errors                      as Error
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
import qualified Hydra.Sources.Kernel.Terms.Inference      as Inference
import qualified Hydra.Sources.Kernel.Terms.Languages      as Languages
import qualified Hydra.Sources.Kernel.Terms.Lexical        as Lexical
import qualified Hydra.Sources.Kernel.Terms.Literals       as Literals
import qualified Hydra.Sources.Kernel.Terms.Names          as Names
import qualified Hydra.Sources.Kernel.Terms.Reduction      as Reduction
import qualified Hydra.Sources.Kernel.Terms.Reflect        as Reflect
import qualified Hydra.Sources.Kernel.Terms.Serialization  as Serialization
import qualified Hydra.Sources.Kernel.Terms.Show.Paths as ShowPaths
import qualified Hydra.Sources.Kernel.Terms.Show.Core      as ShowCore
import qualified Hydra.Sources.Kernel.Terms.Show.Errors     as ShowError
import qualified Hydra.Sources.Kernel.Terms.Show.Graph     as ShowGraph
import qualified Hydra.Sources.Kernel.Terms.Show.Variants  as ShowVariants
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
import qualified Hydra.Ext.Java.Environment as JavaHelpers
import qualified Hydra.Ext.Sources.Java.Syntax as JavaSyntax
import qualified Hydra.Ext.Sources.Java.Environment as JavaEnvironmentSource
import qualified Hydra.Ext.Sources.Java.Coder as JavaCoderSource
import qualified Hydra.Ext.Sources.Java.Serde as JavaSerdeSource
import qualified Hydra.Sources.Test.Utils as TestUtils
import qualified Hydra.Sources.Kernel.Terms.Serialization  as SerializationSource


define :: String -> TTerm a -> TTermDefinition a
define = definitionInModule module_


ns :: Namespace
ns = Namespace "hydra.ext.java.testing"

module_ :: Module
module_ = Module ns elements
    [SerializationSource.ns, TestUtils.ns, Formatting.ns, Names.ns, Constants.ns]
    (JavaSyntax.ns:KernelTypes.kernelTypesNamespaces) $
    Just "Java test code generation codec for JUnit-based generation tests"
  where
    elements = [
      toDefinition buildJavaTestModule,
      toDefinition findJavaImports,
      toDefinition formatJavaTestName,
      toDefinition generateJavaTestCase,
      toDefinition generateJavaTestFile,
      toDefinition generateJavaTestGroupHierarchy,
      toDefinition generateTestFileWithJavaCodec,
      toDefinition namespaceToJavaClassName]


-- | Build complete Java test module
buildJavaTestModule :: TTermDefinition (Module -> TestGroup -> String -> String)
buildJavaTestModule = define "buildJavaTestModule" $
  doc "Build the complete Java test module content" $
  lambda "testModule" $ lambda "testGroup" $ lambda "testBody" $ lets [
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


-- | Find necessary imports for Java test files
findJavaImports :: TTermDefinition [String]
findJavaImports = define "findJavaImports" $
  doc "Standard imports for Java JUnit test files" $
  list [
    string "import org.junit.jupiter.api.Test;",
    string "import static org.junit.jupiter.api.Assertions.*;",
    string "import java.util.*;"]


-- | Format a test name for Java (camelCase method name)
formatJavaTestName :: TTermDefinition (String -> String)
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


-- | Generate a single test case for Java/JUnit
generateJavaTestCase :: TTermDefinition ([String] -> TestCaseWithMetadata -> Either String [String])
generateJavaTestCase = define "generateJavaTestCase" $
  doc "Generate a single JUnit test case from a test case with metadata" $
  lambda "groupPath" $ lambda "tcm" $ lets [
    "name_">: project _TestCaseWithMetadata _TestCaseWithMetadata_name @@ var "tcm",
    "tcase">: project _TestCaseWithMetadata _TestCaseWithMetadata_case @@ var "tcm"] $
    cases _TestCase (var "tcase") Nothing [
      _TestCase_universal>>: lambda "ucase" $ lets [
        "actual_">: project _UniversalTestCase _UniversalTestCase_actual @@ var "ucase",
        "expected_">: project _UniversalTestCase _UniversalTestCase_expected @@ var "ucase",
        "fullName">: Logic.ifElse (Lists.null (var "groupPath"))
          (var "name_")
          (Strings.intercalate (string "_") (Lists.concat2 (var "groupPath") (list [var "name_"]))),
        "formattedName">: asTerm formatJavaTestName @@ var "fullName"] $
        right (list [
          string "    @Test",
          Strings.cat (list [string "    public void ", var "formattedName", string "() {"]),
          string "        assertEquals(",
          Strings.cat (list [string "            ", var "expected_", string ","]),
          Strings.cat (list [string "            ", var "actual_", string ");"]),
          string "    }"])]


-- | Generate Java test file for a test group
generateJavaTestFile :: TTermDefinition (Module -> TestGroup -> Graph -> Either String (String, String))
generateJavaTestFile = define "generateJavaTestFile" $
  doc "Generate a Java test file for a test group" $
  lambda "testModule" $ lambda "testGroup" $ lambda "_g" $
    generateTestFileWithJavaCodec @@ var "testModule" @@ var "testGroup"


-- | Generate Java test group hierarchy
generateJavaTestGroupHierarchy :: TTermDefinition ([String] -> TestGroup -> Either String String)
generateJavaTestGroupHierarchy = define "generateJavaTestGroupHierarchy" $
  doc "Generate test hierarchy for Java with nested subgroups" $
  lambda "groupPath" $ lambda "testGroup" $ lets [
    "cases_">: project _TestGroup _TestGroup_cases @@ var "testGroup",
    "subgroups">: project _TestGroup _TestGroup_subgroups @@ var "testGroup"] $
    Eithers.bind
      (Eithers.map
        (lambda "lines_" $ Strings.intercalate (string "\n\n") (Lists.concat (var "lines_")))
        (Eithers.mapList
          (lambda "tc" $ generateJavaTestCase @@ var "groupPath" @@ var "tc")
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
                  (generateJavaTestGroupHierarchy
                    @@ (Lists.concat2 (var "groupPath") (list [var "groupName"]))
                    @@ var "subgroup"))
              (var "subgroups"))))


-- | Generate test file using Java codec
generateTestFileWithJavaCodec :: TTermDefinition (Module -> TestGroup -> Either String (String, String))
generateTestFileWithJavaCodec = define "generateTestFileWithJavaCodec" $
  doc "Generate a complete test file for Java" $
  lambda "testModule" $ lambda "testGroup" $
    Eithers.map
      (lambda "testBody" $ lets [
        "testModuleContent">: buildJavaTestModule @@ var "testModule" @@ var "testGroup" @@ var "testBody",
        "ns_">: Module.moduleNamespace (var "testModule"),
        "parts">: Strings.splitOn (string ".") (unwrap _Namespace @@ var "ns_"),
        "dirParts">: Lists.drop (int32 1) (Lists.init (var "parts")),
        "className_">: Strings.cat2 (Formatting.capitalize @@ (Lists.last (var "parts"))) (string "Test"),
        "fileName">: Strings.cat2 (var "className_") (string ".java"),
        "filePath">: Strings.cat (list [Strings.intercalate (string "/") (var "dirParts"), string "/", var "fileName"])] $
        Phantoms.pair (var "filePath") (var "testModuleContent"))
      (generateJavaTestGroupHierarchy @@ list ([] :: [TTerm String]) @@ var "testGroup")


-- | Convert namespace to Java class name
namespaceToJavaClassName :: TTermDefinition (Namespace -> String)
namespaceToJavaClassName = define "namespaceToJavaClassName" $
  doc "Convert namespace to Java class name" $
  lambda "ns_" $
    Strings.intercalate (string ".")
      (Lists.map Formatting.capitalize
        (Strings.splitOn (string ".") (unwrap _Namespace @@ var "ns_")))
