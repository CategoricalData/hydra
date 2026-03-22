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
import qualified Hydra.Dsl.Errors                      as Error
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
import qualified Hydra.Sources.Kernel.Terms.Show.Errors     as ShowError
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
import qualified Hydra.Ext.Python.Syntax as Py
import qualified Hydra.Ext.Python.Environment as PyHelpers
import qualified Hydra.Ext.Sources.Python.Syntax as PySyntax
import qualified Hydra.Ext.Sources.Python.Environment as PyEnvironmentSource
import qualified Hydra.Ext.Sources.Python.Coder as PyCoderSource
import qualified Hydra.Ext.Sources.Python.Serde as PySerde
import qualified Hydra.Ext.Sources.Python.Names as PyNames
import qualified Hydra.Ext.Sources.Python.Utils as PyUtils
import qualified Hydra.Sources.Test.Utils as TestUtils
import qualified Hydra.Sources.Kernel.Terms.Lexical         as Lexical
import qualified Hydra.Sources.Kernel.Terms.Serialization  as SerializationSource
import Hydra.Testing (TestCodec(..))
import Hydra.Coders (LanguageName(..))


define :: String -> TTerm a -> TBinding a
define = definitionInModule module_


ns :: Namespace
ns = Namespace "hydra.ext.python.testing"

module_ :: Module
module_ = Module ns elements
    [PyCoderSource.ns, PySerde.ns, PyNames.ns, PyUtils.ns, SerializationSource.ns, Formatting.ns, Names.ns, TestUtils.ns, Constants.ns, Rewriting.ns, Lexical.ns, ShowError.ns]
    (PyEnvironmentSource.ns:PySyntax.ns:KernelTypes.kernelTypesNamespaces) $
    Just "Python test code generation codec for pytest-based generation tests"
  where
    elements = [
      -- Python module metadata
      toBinding emptyPythonModuleMetadata,
      -- Term and type encoding
      toBinding termToPythonWithContext,
      toBinding termToPython,
      toBinding typeToPython,
      -- Test codec construction
      toBinding pythonTestCodec,
      toBinding pythonTestCodecWithContext,
      -- Formatting helpers
      toBinding formatPythonTestName,
      toBinding namespaceToPythonModuleName,
      -- Templates
      toBinding pythonTestCaseTemplate,
      toBinding pythonTestGroupTemplate,
      toBinding pythonModuleTemplate,
      toBinding pythonImportTemplate,
      -- Imports
      toBinding findPythonImports,
      -- Namespace helpers
      toBinding namespacesForPythonModule,
      -- Test hierarchy generation
      toBinding generatePythonTestGroupHierarchy,
      toBinding generatePythonTestCase,
      -- Test file generation
      toBinding generateTestFileWithPythonCodec,
      toBinding buildPythonTestModule,
      toBinding generatePythonTestFile]


-- | Initial empty metadata for running encoding
emptyPythonModuleMetadata :: TBinding (Namespace -> PyHelpers.PythonModuleMetadata)
emptyPythonModuleMetadata = define "emptyPythonModuleMetadata" $
  doc "Create an initial empty PythonModuleMetadata for a given namespace" $
  lambda "ns_" $
    record PyHelpers._PythonModuleMetadata [
      PyHelpers._PythonModuleMetadata_namespaces>>:
        record _Namespaces [
          _Namespaces_focus>>: Phantoms.pair (var "ns_") (PyNames.encodeNamespace @@ var "ns_"),
          _Namespaces_mapping>>: Maps.empty],
      PyHelpers._PythonModuleMetadata_typeVariables>>: Sets.empty,
      PyHelpers._PythonModuleMetadata_usesAnnotated>>: boolean False,
      PyHelpers._PythonModuleMetadata_usesCallable>>: boolean False,
      PyHelpers._PythonModuleMetadata_usesCast>>: boolean False,
      PyHelpers._PythonModuleMetadata_usesLruCache>>: boolean False,
      PyHelpers._PythonModuleMetadata_usesTypeAlias>>: boolean False,
      PyHelpers._PythonModuleMetadata_usesDataclass>>: boolean False,
      PyHelpers._PythonModuleMetadata_usesDecimal>>: boolean False,
      PyHelpers._PythonModuleMetadata_usesEither>>: boolean False,
      PyHelpers._PythonModuleMetadata_usesEnum>>: boolean False,
      PyHelpers._PythonModuleMetadata_usesFrozenDict>>: boolean False,
      PyHelpers._PythonModuleMetadata_usesFrozenList>>: boolean False,
      PyHelpers._PythonModuleMetadata_usesGeneric>>: boolean False,
      PyHelpers._PythonModuleMetadata_usesJust>>: boolean False,
      PyHelpers._PythonModuleMetadata_usesLeft>>: boolean False,
      PyHelpers._PythonModuleMetadata_usesMaybe>>: boolean False,
      PyHelpers._PythonModuleMetadata_usesName>>: boolean False,
      PyHelpers._PythonModuleMetadata_usesNode>>: boolean False,
      PyHelpers._PythonModuleMetadata_usesNothing>>: boolean False,
      PyHelpers._PythonModuleMetadata_usesRight>>: boolean False,
      PyHelpers._PythonModuleMetadata_usesTypeVar>>: boolean False]


-- | Convert a Hydra term to a Python expression string with context
termToPythonWithContext :: TBinding (Namespaces Py.DottedName -> Graph -> Bool -> Term -> Graph -> Either String String)
termToPythonWithContext = define "termToPythonWithContext" $
  doc "Convert a Hydra term to a Python expression string with a pre-built graph context" $
  lambda "namespaces_" $ lambda "graph0" $ lambda "skipCasts" $ lambda "term" $ lambda "_g" $
    Eithers.bimap
      ("ic" ~> ShowError.error_ @@ Ctx.inContextObject (var "ic"))
      (Serialization.printExpr <.> PySerde.encodeExpression)
      (PyCoderSource.encodeTermInline
        @@ asTerm Lexical.emptyContext
        @@ (record PyHelpers._PythonEnvironment [
              PyHelpers._PythonEnvironment_namespaces>>: var "namespaces_",
              PyHelpers._PythonEnvironment_boundTypeVariables>>:
                Phantoms.pair (list ([] :: [TTerm String])) (Maps.empty),
              PyHelpers._PythonEnvironment_graph>>: var "graph0",
              PyHelpers._PythonEnvironment_nullaryBindings>>: Sets.empty,
              PyHelpers._PythonEnvironment_version>>: PyUtils.targetPythonVersion,
              PyHelpers._PythonEnvironment_skipCasts>>: var "skipCasts",
              PyHelpers._PythonEnvironment_inlineVariables>>: Sets.empty])
        @@ var "skipCasts"
        @@ var "term")


-- | Legacy wrapper that gets Graph on each call
termToPython :: TBinding (Namespaces Py.DottedName -> Term -> Graph -> Either String String)
termToPython = define "termToPython" $
  doc "Convert a Hydra term to a Python expression string" $
  lambda "namespaces_" $ lambda "term" $ lambda "g" $
    termToPythonWithContext @@ var "namespaces_" @@ var "g" @@ boolean False @@ var "term" @@ var "g"


-- | Convert a Hydra type to a Python type expression string
typeToPython :: TBinding (Namespaces Py.DottedName -> Type -> Graph -> Either String String)
typeToPython = define "typeToPython" $
  doc "Convert a Hydra type to a Python type expression string" $
  lambda "namespaces_" $ lambda "typ" $ lambda "g" $
    Eithers.bimap
      ("ic" ~> ShowError.error_ @@ Ctx.inContextObject (var "ic"))
      (Serialization.printExpr <.> PySerde.encodeExpression)
      (PyCoderSource.encodeType
        @@ (record PyHelpers._PythonEnvironment [
              PyHelpers._PythonEnvironment_namespaces>>: var "namespaces_",
              PyHelpers._PythonEnvironment_boundTypeVariables>>:
                Phantoms.pair (list ([] :: [TTerm String])) (Maps.empty),
              PyHelpers._PythonEnvironment_graph>>: var "g",
              PyHelpers._PythonEnvironment_nullaryBindings>>: Sets.empty,
              PyHelpers._PythonEnvironment_version>>: PyUtils.targetPythonVersion,
              PyHelpers._PythonEnvironment_skipCasts>>: boolean False,
              PyHelpers._PythonEnvironment_inlineVariables>>: Sets.empty])
        @@ var "typ")


-- | Create a Python TestCodec
pythonTestCodec :: TBinding (Namespaces Py.DottedName -> TestCodec)
pythonTestCodec = define "pythonTestCodec" $
  doc "Create a Python TestCodec for pytest-based test generation" $
  lambda "namespaces_" $
    record _TestCodec [
      _TestCodec_language>>: Coders.languageName_ (string "python"),
      _TestCodec_fileExtension>>: wrap _FileExtension (string "py"),
      _TestCodec_encodeTerm>>: termToPython @@ var "namespaces_",
      _TestCodec_encodeType>>: typeToPython @@ var "namespaces_",
      _TestCodec_formatTestName>>: asTerm formatPythonTestName,
      _TestCodec_formatModuleName>>: asTerm namespaceToPythonModuleName,
      _TestCodec_testCaseTemplate>>: asTerm pythonTestCaseTemplate,
      _TestCodec_testGroupTemplate>>: asTerm pythonTestGroupTemplate,
      _TestCodec_moduleTemplate>>: asTerm pythonModuleTemplate,
      _TestCodec_importTemplate>>: asTerm pythonImportTemplate,
      _TestCodec_findImports>>: findPythonImports @@ var "namespaces_"]


-- | Create an efficient Python TestCodec with a pre-built Graph
pythonTestCodecWithContext :: TBinding (Namespaces Py.DottedName -> Graph -> TestCodec)
pythonTestCodecWithContext = define "pythonTestCodecWithContext" $
  doc "Create an efficient Python TestCodec with a pre-built Graph, skipping casts for performance" $
  lambda "namespaces_" $ lambda "tcontext" $
    record _TestCodec [
      _TestCodec_language>>: Coders.languageName_ (string "python"),
      _TestCodec_fileExtension>>: wrap _FileExtension (string "py"),
      _TestCodec_encodeTerm>>: termToPythonWithContext @@ var "namespaces_" @@ var "tcontext" @@ boolean True,
      _TestCodec_encodeType>>: typeToPython @@ var "namespaces_",
      _TestCodec_formatTestName>>: asTerm formatPythonTestName,
      _TestCodec_formatModuleName>>: asTerm namespaceToPythonModuleName,
      _TestCodec_testCaseTemplate>>: asTerm pythonTestCaseTemplate,
      _TestCodec_testGroupTemplate>>: asTerm pythonTestGroupTemplate,
      _TestCodec_moduleTemplate>>: asTerm pythonModuleTemplate,
      _TestCodec_importTemplate>>: asTerm pythonImportTemplate,
      _TestCodec_findImports>>: findPythonImports @@ var "namespaces_"]


-- | Format a test name for Python (snake_case, valid identifier)
-- Each character is individually lowercased; non-alphanumeric chars become underscores (preserving runs)
formatPythonTestName :: TBinding (String -> String)
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


-- | Convert namespace to Python module name
namespaceToPythonModuleName :: TBinding (Namespace -> String)
namespaceToPythonModuleName = define "namespaceToPythonModuleName" $
  doc "Convert namespace to Python module name (dot-separated lowercase)" $
  lambda "ns_" $
    unwrap _Namespace @@ var "ns_"


-- | Pytest test case template
pythonTestCaseTemplate :: TBinding String
pythonTestCaseTemplate = define "pythonTestCaseTemplate" $
  doc "Template for pytest test case assertions" $
  Strings.intercalate (string "\n") (list [
    string "def {name}():",
    string "    assert ({input}) == ({output})"])


-- | Pytest test group template
pythonTestGroupTemplate :: TBinding String
pythonTestGroupTemplate = define "pythonTestGroupTemplate" $
  doc "Template for pytest test group comments" $
  string "# {groupName}"


-- | Python module template
pythonModuleTemplate :: TBinding String
pythonModuleTemplate = define "pythonModuleTemplate" $
  doc "Template for Python test module structure" $
  Strings.intercalate (string "\n") (list [
    Strings.cat2 (string "# ") (asTerm Constants.warningAutoGeneratedFile),
    string "",
    string "{imports}",
    string "",
    string "{testGroup}",
    string "",
    string "{testCases}"])


-- | Python import template
pythonImportTemplate :: TBinding String
pythonImportTemplate = define "pythonImportTemplate" $
  doc "Template for Python import statements" $
  string "import {namespace}"


-- | Find necessary imports for Python based on referenced namespaces
findPythonImports :: TBinding (Namespaces Py.DottedName -> S.Set Name -> [String])
findPythonImports = define "findPythonImports" $
  doc "Determine necessary imports for Python based on referenced namespaces" $
  lambda "namespaces_" $ lambda "names_" $ lets [
    "mapping_">: project _Namespaces _Namespaces_mapping @@ var "namespaces_",
    "filtered">: Maps.filterWithKey
      (lambda "ns_" $ lambda "_v" $
        Logic.not (Equality.equal (Lists.head (Strings.splitOn (string "hydra.test.") (unwrap _Namespace @@ var "ns_"))) (string "")))
      (var "mapping_")] $
    Lists.map
      (lambda "entry" $
        Strings.cat2 (string "import ") (unwrap _Namespace @@ (Pairs.first (var "entry"))))
      (Maps.toList (var "filtered"))


-- | Build namespaces for Python module from graph bindings
namespacesForPythonModule :: TBinding (Module -> Graph -> Either String (Namespaces Py.DottedName))
namespacesForPythonModule = define "namespacesForPythonModule" $
  doc "Build namespaces for a Python module, resolving all imports and primitives" $
  lambda "mod" $ lambda "graph_" $ lets [
    "bindings">: Lexical.graphToBindings @@ var "graph_",
    "defs">: Maybes.mapMaybe
      (lambda "b" $
        Maybes.map
          (lambda "ts" $
            Module.definitionTerm (record _TermDefinition [
              _TermDefinition_name>>: project _Binding _Binding_name @@ var "b",
              _TermDefinition_term>>: project _Binding _Binding_term @@ var "b",
              _TermDefinition_type>>: var "ts"]))
          (project _Binding _Binding_type @@ var "b"))
      (var "bindings")] $
    right (PyUtils.findNamespaces @@ Module.moduleNamespace (var "mod") @@ var "defs")


-- | Generate Python test group hierarchy
generatePythonTestGroupHierarchy :: TBinding (Graph -> Namespaces Py.DottedName -> TestCodec -> [String] -> TestGroup -> Either String String)
generatePythonTestGroupHierarchy = define "generatePythonTestGroupHierarchy" $
  doc "Generate test hierarchy for Python with nested subgroups" $
  lambda "g" $ lambda "namespaces_" $ lambda "codec" $ lambda "groupPath" $ lambda "testGroup" $ lets [
    "cases_">: project _TestGroup _TestGroup_cases @@ var "testGroup",
    "subgroups">: project _TestGroup _TestGroup_subgroups @@ var "testGroup"] $
    "testCaseLines" <<~ Eithers.mapList
          (lambda "tc" $ generatePythonTestCase @@ var "g" @@ var "namespaces_" @@ var "codec" @@ var "groupPath" @@ var "tc")
          (var "cases_") $
      "subgroupBlocks" <<~ Eithers.mapList
          (lambda "subgroup" $ lets [
            "groupName">: project _TestGroup _TestGroup_name @@ var "subgroup",
            "header">: Strings.cat2 (string "# ") (var "groupName")] $
            Eithers.map
              (lambda "content" $ Strings.cat (list [var "header", string "\n\n", var "content"]))
              (generatePythonTestGroupHierarchy @@ var "g" @@ var "namespaces_" @@ var "codec"
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


-- | Generate a single test case for Python/pytest
generatePythonTestCase :: TBinding (Graph -> Namespaces Py.DottedName -> TestCodec -> [String] -> TestCaseWithMetadata -> Either String [String])
generatePythonTestCase = define "generatePythonTestCase" $
  doc "Generate a single pytest test case from a test case with metadata" $
  lambda "g" $ lambda "namespaces_" $ lambda "codec" $ lambda "groupPath" $ lambda "tcm" $ lets [
    "name_">: project _TestCaseWithMetadata _TestCaseWithMetadata_name @@ var "tcm",
    "tcase">: project _TestCaseWithMetadata _TestCaseWithMetadata_case @@ var "tcm"] $
    cases _TestCase (var "tcase") (Just (right (list ([] :: [TTerm String])))) [
      _TestCase_delegatedEvaluation>>: lambda "delCase" $ lets [
        "input_">: project _DelegatedEvaluationTestCase _DelegatedEvaluationTestCase_input @@ var "delCase",
        "output_">: project _DelegatedEvaluationTestCase _DelegatedEvaluationTestCase_output @@ var "delCase",
        "fullName">: Logic.ifElse (Lists.null (var "groupPath"))
          (var "name_")
          (Strings.intercalate (string "__") (Lists.concat2 (var "groupPath") (list [var "name_"]))),
        "formattedName">: project _TestCodec _TestCodec_formatTestName @@ var "codec" @@ var "fullName"] $
        "inputCode" <<~ (project _TestCodec _TestCodec_encodeTerm @@ var "codec" @@ var "input_" @@ var "g") $
        "outputCode" <<~ (project _TestCodec _TestCodec_encodeTerm @@ var "codec" @@ var "output_" @@ var "g") $
        right (list [
              Strings.cat (list [string "def ", var "formattedName", string "():"]),
              Strings.cat (list [string "    assert (", var "inputCode", string ") == (", var "outputCode", string ")"])])]


-- | Generate test file using Python codec
generateTestFileWithPythonCodec :: TBinding (TestCodec -> Module -> TestGroup -> Namespaces Py.DottedName -> Graph -> Either String (String, String))
generateTestFileWithPythonCodec = define "generateTestFileWithPythonCodec" $
  doc "Generate a complete test file using the Python codec" $
  lambda "codec" $ lambda "testModule" $ lambda "testGroup" $ lambda "namespaces_" $ lambda "g" $
    Eithers.map
      (lambda "testBody" $ lets [
        "testModuleContent">: buildPythonTestModule @@ var "codec" @@ var "testModule" @@ var "testGroup" @@ var "testBody" @@ var "namespaces_",
        "ns_">: Module.moduleNamespace (var "testModule"),
        "parts">: Strings.splitOn (string ".") (unwrap _Namespace @@ var "ns_"),
        "dirParts">: Lists.init (var "parts"),
        "fileName">: Strings.cat (list [string "test_", Lists.last (var "parts"), string ".py"]),
        "filePath">: Strings.cat (list [Strings.intercalate (string "/") (var "dirParts"), string "/", var "fileName"])] $
        Phantoms.pair (var "filePath") (var "testModuleContent"))
      (generatePythonTestGroupHierarchy @@ var "g" @@ var "namespaces_" @@ var "codec" @@ list ([] :: [TTerm String]) @@ var "testGroup")


-- | Build complete Python test module
buildPythonTestModule :: TBinding (TestCodec -> Module -> TestGroup -> String -> Namespaces Py.DottedName -> String)
buildPythonTestModule = define "buildPythonTestModule" $
  doc "Build the complete Python test module content" $
  lambda "codec" $ lambda "testModule" $ lambda "testGroup" $ lambda "testBody" $ lambda "namespaces_" $ lets [
    "groupName_">: project _TestGroup _TestGroup_name @@ var "testGroup",
    "domainImports">: project _TestCodec _TestCodec_findImports @@ var "codec" @@ Sets.empty,
    "standardImports">: list [
      string "from __future__ import annotations",
      string "from typing import cast",
      string "from decimal import Decimal",
      string "from hydra.dsl.python import FrozenDict, frozenlist, Either, Left, Right, Maybe, Just, Nothing"],
    "allImports">: Lists.concat2 (var "standardImports") (var "domainImports"),
    "header">: Strings.cat (list [
      Strings.cat2 (string "# ") (asTerm Constants.warningAutoGeneratedFile),
      string "\n",
      Strings.cat2 (string "# ") (var "groupName_"),
      string "\n\n",
      Strings.intercalate (string "\n") (var "allImports"),
      string "\n\n"])] $
    Strings.cat (list [var "header", var "testBody", string "\n"])


-- | Generate Python test file for a test group
generatePythonTestFile :: TBinding (Module -> TestGroup -> Graph -> Either String (String, String))
generatePythonTestFile = define "generatePythonTestFile" $
  doc "Generate a Python test file for a test group, with type inference" $
  lambda "testModule" $ lambda "testGroup" $ lambda "g" $
    Eithers.bind
      (TestUtils.inferTestGroupTerms @@ var "g" @@ var "testGroup")
      (lambda "inferredTestGroup" $
        Eithers.bind
          (namespacesForPythonModule @@ var "testModule" @@ var "g")
          (lambda "namespaces_" $
            generateTestFileWithPythonCodec
              @@ (pythonTestCodecWithContext @@ var "namespaces_" @@ var "g")
              @@ var "testModule"
              @@ var "inferredTestGroup"
              @@ var "namespaces_"
              @@ var "g"))


