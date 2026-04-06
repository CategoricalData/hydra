-- | Python test code generation codec in Hydra DSL.
-- This module provides DSL versions of Python test codec functions for pytest-based generation tests.

module Hydra.Ext.Sources.Python.Testing where

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
import qualified Hydra.Dsl.Packaging                     as Packaging
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
import qualified Hydra.Ext.Python.Syntax as Py
import qualified Hydra.Ext.Python.Environment as PyHelpers
import qualified Hydra.Ext.Sources.Python.Syntax as PySyntax
import qualified Hydra.Ext.Sources.Python.Environment as PyEnvironmentSource
import qualified Hydra.Ext.Sources.Python.Coder as PyCoderSource
import qualified Hydra.Ext.Sources.Python.Serde as PySerde
import qualified Hydra.Ext.Sources.Python.Names as PyNames
import qualified Hydra.Ext.Sources.Python.Utils as PyUtils
import qualified Hydra.Sources.Test.Utils as TestUtils
import qualified Hydra.Sources.Kernel.Terms.Serialization  as SerializationSource


define :: String -> TTerm a -> TTermDefinition a
define = definitionInModule module_


ns :: Namespace
ns = Namespace "hydra.ext.python.testing"

module_ :: Module
module_ = Module ns definitions
    [SerializationSource.ns, Formatting.ns, Names.ns, TestUtils.ns, Constants.ns]
    (PySyntax.ns:KernelTypes.kernelTypesNamespaces) $
    Just "Python test code generation codec for pytest-based generation tests"
  where
    definitions = [
      toDefinition buildPythonTestModule,
      toDefinition formatPythonTestName,
      toDefinition generatePythonTestCase,
      toDefinition generatePythonTestFile,
      toDefinition generatePythonTestGroupHierarchy,
      toDefinition generateTestFileWithPythonCodec]


-- | Build complete Python test module
buildPythonTestModule :: TTermDefinition (Module -> TestGroup -> String -> String)
buildPythonTestModule = define "buildPythonTestModule" $
  doc "Build the complete Python test module content" $
  lambda "testModule" $ lambda "testGroup" $ lambda "testBody" $ lets [
    "groupName_">: project _TestGroup _TestGroup_name @@ var "testGroup",
    "header">: Strings.cat (list [
      Strings.cat2 (string "# ") (asTerm Constants.warningAutoGeneratedFile),
      string "\n",
      Strings.cat2 (string "# ") (var "groupName_"),
      string "\n\n"])] $
    Strings.cat (list [var "header", var "testBody", string "\n"])


-- | Format a test name for Python (snake_case, valid identifier)
formatPythonTestName :: TTermDefinition (String -> String)
formatPythonTestName = define "formatPythonTestName" $
  doc "Format a test name for Python (snake_case with test_ prefix)" $
  lambda "name" $
    Strings.cat2 (string "test_")
      (Strings.fromList (Lists.map
        (lambda "c" $
          Logic.ifElse (Chars.isAlphaNum (var "c"))
            (Chars.toLower (var "c"))
            (int32 95))  -- underscore
        (Strings.toList (var "name"))))


-- | Generate a single test case for Python/pytest
generatePythonTestCase :: TTermDefinition ([String] -> TestCaseWithMetadata -> Either String [String])
generatePythonTestCase = define "generatePythonTestCase" $
  doc "Generate a single pytest test case from a test case with metadata" $
  lambda "groupPath" $ lambda "tcm" $ lets [
    "name_">: project _TestCaseWithMetadata _TestCaseWithMetadata_name @@ var "tcm",
    "tcase">: project _TestCaseWithMetadata _TestCaseWithMetadata_case @@ var "tcm"] $
    cases _TestCase (var "tcase") Nothing [
      _TestCase_universal>>: lambda "ucase" $ lets [
        "actual_">: project _UniversalTestCase _UniversalTestCase_actual @@ var "ucase",
        "expected_">: project _UniversalTestCase _UniversalTestCase_expected @@ var "ucase",
        "fullName">: Logic.ifElse (Lists.null (var "groupPath"))
          (var "name_")
          (Strings.intercalate (string "__") (Lists.concat2 (var "groupPath") (list [var "name_"]))),
        "formattedName">: asTerm formatPythonTestName @@ var "fullName"] $
        right (list [
              Strings.cat (list [string "def ", var "formattedName", string "():"]),
              Strings.cat (list [string "    assert (", var "actual_", string ") == (", var "expected_", string ")"])])]


-- | Generate Python test file for a test group
generatePythonTestFile :: TTermDefinition (Module -> TestGroup -> Graph -> Either String (String, String))
generatePythonTestFile = define "generatePythonTestFile" $
  doc "Generate a Python test file for a test group" $
  lambda "testModule" $ lambda "testGroup" $ lambda "_g" $
    generateTestFileWithPythonCodec
      @@ var "testModule"
      @@ var "testGroup"


-- | Generate Python test group hierarchy
generatePythonTestGroupHierarchy :: TTermDefinition ([String] -> TestGroup -> Either String String)
generatePythonTestGroupHierarchy = define "generatePythonTestGroupHierarchy" $
  doc "Generate test hierarchy for Python with nested subgroups" $
  lambda "groupPath" $ lambda "testGroup" $ lets [
    "cases_">: project _TestGroup _TestGroup_cases @@ var "testGroup",
    "subgroups">: project _TestGroup _TestGroup_subgroups @@ var "testGroup"] $
    "testCaseLines" <<~ Eithers.mapList
          (lambda "tc" $ generatePythonTestCase @@ var "groupPath" @@ var "tc")
          (var "cases_") $
      "subgroupBlocks" <<~ Eithers.mapList
          (lambda "subgroup" $ lets [
            "groupName">: project _TestGroup _TestGroup_name @@ var "subgroup",
            "header">: Strings.cat2 (string "# ") (var "groupName")] $
            Eithers.map
              (lambda "content" $ Strings.cat (list [var "header", string "\n\n", var "content"]))
              (generatePythonTestGroupHierarchy
                @@ (Lists.concat2 (var "groupPath") (list [var "groupName"]))
                @@ var "subgroup"))
          (var "subgroups") $ lets [
      "testCasesStr">: Strings.intercalate (string "\n\n") (Lists.concat (var "testCaseLines")),
      "subgroupsStr">: Strings.intercalate (string "\n\n") (var "subgroupBlocks")] $
      right (Strings.cat (list [
          var "testCasesStr",
          Logic.ifElse (Logic.or (Equality.equal (var "testCasesStr") (string ""))
                                  (Equality.equal (var "subgroupsStr") (string "")))
            (string "")
            (string "\n\n"),
          var "subgroupsStr"]))


-- | Generate test file using Python codec
generateTestFileWithPythonCodec :: TTermDefinition (Module -> TestGroup -> Either String (String, String))
generateTestFileWithPythonCodec = define "generateTestFileWithPythonCodec" $
  doc "Generate a complete test file for Python" $
  lambda "testModule" $ lambda "testGroup" $
    Eithers.map
      (lambda "testBody" $ lets [
        "testModuleContent">: buildPythonTestModule @@ var "testModule" @@ var "testGroup" @@ var "testBody",
        "ns_">: Packaging.moduleNamespace (var "testModule"),
        "parts">: Strings.splitOn (string ".") (unwrap _Namespace @@ var "ns_"),
        "dirParts">: Lists.init (var "parts"),
        "fileName">: Strings.cat (list [string "test_", Lists.last (var "parts"), string ".py"]),
        "filePath">: Strings.cat (list [Strings.intercalate (string "/") (var "dirParts"), string "/", var "fileName"])] $
        Phantoms.pair (var "filePath") (var "testModuleContent"))
      (generatePythonTestGroupHierarchy @@ list ([] :: [TTerm String]) @@ var "testGroup")
