-- Note: this is an automatically generated file. Do not edit.

-- | Python test code generation codec for pytest-based generation tests

module Hydra.Ext.Python.TestCodec where

import qualified Hydra.Coders as Coders
import qualified Hydra.Constants as Constants
import qualified Hydra.Context as Context
import qualified Hydra.Core as Core
import qualified Hydra.Ext.Python.Coder as Coder
import qualified Hydra.Ext.Python.Helpers as Helpers
import qualified Hydra.Ext.Python.Names as Names
import qualified Hydra.Ext.Python.Serde as Serde
import qualified Hydra.Ext.Python.Syntax as Syntax
import qualified Hydra.Ext.Python.Utils as Utils
import qualified Hydra.Graph as Graph
import qualified Hydra.Lexical as Lexical
import qualified Hydra.Lib.Chars as Chars
import qualified Hydra.Lib.Eithers as Eithers
import qualified Hydra.Lib.Equality as Equality
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Pairs as Pairs
import qualified Hydra.Lib.Sets as Sets
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Module as Module
import qualified Hydra.Serialization as Serialization
import qualified Hydra.Show.Error as Error
import qualified Hydra.Test.Utils as Utils_
import qualified Hydra.Testing as Testing
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | Create an initial empty PythonModuleMetadata for a given namespace
emptyPythonModuleMetadata :: Module.Namespace -> Helpers.PythonModuleMetadata
emptyPythonModuleMetadata ns_ =
    Helpers.PythonModuleMetadata {
      Helpers.pythonModuleMetadataNamespaces = Module.Namespaces {
        Module.namespacesFocus = (ns_, (Names.encodeNamespace ns_)),
        Module.namespacesMapping = Maps.empty},
      Helpers.pythonModuleMetadataTypeVariables = Sets.empty,
      Helpers.pythonModuleMetadataUsesAnnotated = False,
      Helpers.pythonModuleMetadataUsesCallable = False,
      Helpers.pythonModuleMetadataUsesCast = False,
      Helpers.pythonModuleMetadataUsesLruCache = False,
      Helpers.pythonModuleMetadataUsesTypeAlias = False,
      Helpers.pythonModuleMetadataUsesDataclass = False,
      Helpers.pythonModuleMetadataUsesDecimal = False,
      Helpers.pythonModuleMetadataUsesEither = False,
      Helpers.pythonModuleMetadataUsesEnum = False,
      Helpers.pythonModuleMetadataUsesFrozenDict = False,
      Helpers.pythonModuleMetadataUsesFrozenList = False,
      Helpers.pythonModuleMetadataUsesGeneric = False,
      Helpers.pythonModuleMetadataUsesJust = False,
      Helpers.pythonModuleMetadataUsesLeft = False,
      Helpers.pythonModuleMetadataUsesMaybe = False,
      Helpers.pythonModuleMetadataUsesName = False,
      Helpers.pythonModuleMetadataUsesNode = False,
      Helpers.pythonModuleMetadataUsesNothing = False,
      Helpers.pythonModuleMetadataUsesRight = False,
      Helpers.pythonModuleMetadataUsesTypeVar = False}

-- | Convert a Hydra term to a Python expression string with a pre-built graph context
termToPythonWithContext :: Module.Namespaces Syntax.DottedName -> Graph.Graph -> Bool -> Core.Term -> t0 -> Either String String
termToPythonWithContext namespaces_ graph0 skipCasts term _g =
    Eithers.bimap (\ic -> Error.error (Context.inContextObject ic)) (\arg_ -> Serialization.printExpr (Serde.encodeExpression arg_)) (Coder.encodeTermInline Lexical.emptyContext (Helpers.PythonEnvironment {
      Helpers.pythonEnvironmentNamespaces = namespaces_,
      Helpers.pythonEnvironmentBoundTypeVariables = ([], Maps.empty),
      Helpers.pythonEnvironmentGraph = graph0,
      Helpers.pythonEnvironmentNullaryBindings = Sets.empty,
      Helpers.pythonEnvironmentVersion = Utils.targetPythonVersion,
      Helpers.pythonEnvironmentSkipCasts = skipCasts,
      Helpers.pythonEnvironmentInlineVariables = Sets.empty}) skipCasts term)

-- | Convert a Hydra term to a Python expression string
termToPython :: Module.Namespaces Syntax.DottedName -> Core.Term -> Graph.Graph -> Either String String
termToPython namespaces_ term g = termToPythonWithContext namespaces_ g False term g

-- | Convert a Hydra type to a Python type expression string
typeToPython :: Module.Namespaces Syntax.DottedName -> Core.Type -> Graph.Graph -> Either String String
typeToPython namespaces_ typ g =
    Eithers.bimap (\ic -> Error.error (Context.inContextObject ic)) (\arg_ -> Serialization.printExpr (Serde.encodeExpression arg_)) (Coder.encodeType (Helpers.PythonEnvironment {
      Helpers.pythonEnvironmentNamespaces = namespaces_,
      Helpers.pythonEnvironmentBoundTypeVariables = ([], Maps.empty),
      Helpers.pythonEnvironmentGraph = g,
      Helpers.pythonEnvironmentNullaryBindings = Sets.empty,
      Helpers.pythonEnvironmentVersion = Utils.targetPythonVersion,
      Helpers.pythonEnvironmentSkipCasts = False,
      Helpers.pythonEnvironmentInlineVariables = Sets.empty}) typ)

-- | Create a Python TestCodec for pytest-based test generation
pythonTestCodec :: Module.Namespaces Syntax.DottedName -> Testing.TestCodec
pythonTestCodec namespaces_ =
    Testing.TestCodec {
      Testing.testCodecLanguage = (Coders.LanguageName "python"),
      Testing.testCodecFileExtension = (Module.FileExtension "py"),
      Testing.testCodecEncodeTerm = (termToPython namespaces_),
      Testing.testCodecEncodeType = (typeToPython namespaces_),
      Testing.testCodecFormatTestName = formatPythonTestName,
      Testing.testCodecFormatModuleName = namespaceToPythonModuleName,
      Testing.testCodecTestCaseTemplate = pythonTestCaseTemplate,
      Testing.testCodecTestGroupTemplate = pythonTestGroupTemplate,
      Testing.testCodecModuleTemplate = pythonModuleTemplate,
      Testing.testCodecImportTemplate = pythonImportTemplate,
      Testing.testCodecFindImports = (findPythonImports namespaces_)}

-- | Create an efficient Python TestCodec with a pre-built Graph, skipping casts for performance
pythonTestCodecWithContext :: Module.Namespaces Syntax.DottedName -> Graph.Graph -> Testing.TestCodec
pythonTestCodecWithContext namespaces_ tcontext =
    Testing.TestCodec {
      Testing.testCodecLanguage = (Coders.LanguageName "python"),
      Testing.testCodecFileExtension = (Module.FileExtension "py"),
      Testing.testCodecEncodeTerm = (termToPythonWithContext namespaces_ tcontext True),
      Testing.testCodecEncodeType = (typeToPython namespaces_),
      Testing.testCodecFormatTestName = formatPythonTestName,
      Testing.testCodecFormatModuleName = namespaceToPythonModuleName,
      Testing.testCodecTestCaseTemplate = pythonTestCaseTemplate,
      Testing.testCodecTestGroupTemplate = pythonTestGroupTemplate,
      Testing.testCodecModuleTemplate = pythonModuleTemplate,
      Testing.testCodecImportTemplate = pythonImportTemplate,
      Testing.testCodecFindImports = (findPythonImports namespaces_)}

-- | Format a test name for Python (snake_case with test_ prefix)
formatPythonTestName :: String -> String
formatPythonTestName name =
    Strings.cat2 "test_" (Strings.fromList (Lists.map (\c -> Logic.ifElse (Chars.isAlphaNum c) (Chars.toLower c) 95) (Strings.toList name)))

-- | Convert namespace to Python module name (dot-separated lowercase)
namespaceToPythonModuleName :: Module.Namespace -> String
namespaceToPythonModuleName ns_ = Module.unNamespace ns_

-- | Template for pytest test case assertions
pythonTestCaseTemplate :: String
pythonTestCaseTemplate =
    Strings.intercalate "\n" [
      "def {name}():",
      "    assert ({input}) == ({output})"]

-- | Template for pytest test group comments
pythonTestGroupTemplate :: String
pythonTestGroupTemplate = "# {groupName}"

-- | Template for Python test module structure
pythonModuleTemplate :: String
pythonModuleTemplate =
    Strings.intercalate "\n" [
      Strings.cat2 "# " Constants.warningAutoGeneratedFile,
      "",
      "{imports}",
      "",
      "{testGroup}",
      "",
      "{testCases}"]

-- | Template for Python import statements
pythonImportTemplate :: String
pythonImportTemplate = "import {namespace}"

-- | Determine necessary imports for Python based on referenced namespaces
findPythonImports :: Module.Namespaces t0 -> t1 -> [String]
findPythonImports namespaces_ names_ =
     
      let mapping_ = Module.namespacesMapping namespaces_ 
          filtered =
                  Maps.filterWithKey (\ns_ -> \_v -> Logic.not (Equality.equal (Lists.head (Strings.splitOn "hydra.test." (Module.unNamespace ns_))) "")) mapping_
      in (Lists.map (\entry -> Strings.cat2 "import " (Module.unNamespace (Pairs.first entry))) (Maps.toList filtered))

-- | Build namespaces for a Python module, resolving all imports and primitives
namespacesForPythonModule :: Module.Module -> Graph.Graph -> Either t0 (Module.Namespaces Syntax.DottedName)
namespacesForPythonModule mod graph_ =
     
      let bindings = Lexical.graphToBindings graph_ 
          defs =
                  Maybes.mapMaybe (\b -> Maybes.map (\ts -> Module.DefinitionTerm (Module.TermDefinition {
                    Module.termDefinitionName = (Core.bindingName b),
                    Module.termDefinitionTerm = (Core.bindingTerm b),
                    Module.termDefinitionType = ts})) (Core.bindingType b)) bindings
      in (Right (Utils.findNamespaces (Module.moduleNamespace mod) defs))

-- | Generate test hierarchy for Python with nested subgroups
generatePythonTestGroupHierarchy :: Graph.Graph -> t0 -> Testing.TestCodec -> [String] -> Testing.TestGroup -> Either String String
generatePythonTestGroupHierarchy g namespaces_ codec groupPath testGroup =
     
      let cases_ = Testing.testGroupCases testGroup 
          subgroups = Testing.testGroupSubgroups testGroup
      in (Eithers.bind (Eithers.mapList (\tc -> generatePythonTestCase g namespaces_ codec groupPath tc) cases_) (\testCaseLines -> Eithers.bind (Eithers.mapList (\subgroup ->  
        let groupName = Testing.testGroupName subgroup 
            header = Strings.cat2 "# " groupName
        in (Eithers.map (\content -> Strings.cat [
          header,
          "\n\n",
          content]) (generatePythonTestGroupHierarchy g namespaces_ codec (Lists.concat2 groupPath [
          groupName]) subgroup))) subgroups) (\subgroupBlocks ->  
        let testCasesStr = Strings.intercalate "\n\n" (Lists.concat testCaseLines) 
            subgroupsStr = Strings.intercalate "\n\n" subgroupBlocks
        in (Right (Strings.cat [
          testCasesStr,
          (Logic.ifElse (Logic.or (Equality.equal testCasesStr "") (Equality.equal subgroupsStr "")) "" "\n\n"),
          subgroupsStr])))))

-- | Generate a single pytest test case from a test case with metadata
generatePythonTestCase :: Graph.Graph -> t0 -> Testing.TestCodec -> [String] -> Testing.TestCaseWithMetadata -> Either String [String]
generatePythonTestCase g namespaces_ codec groupPath tcm =
     
      let name_ = Testing.testCaseWithMetadataName tcm 
          tcase = Testing.testCaseWithMetadataCase tcm
      in case tcase of
        Testing.TestCaseDelegatedEvaluation v0 ->  
          let input_ = Testing.delegatedEvaluationTestCaseInput v0 
              output_ = Testing.delegatedEvaluationTestCaseOutput v0
              fullName = Logic.ifElse (Lists.null groupPath) name_ (Strings.intercalate "__" (Lists.concat2 groupPath [
                    name_]))
              formattedName = Testing.testCodecFormatTestName codec fullName
          in (Eithers.bind (Testing.testCodecEncodeTerm codec input_ g) (\inputCode -> Eithers.bind (Testing.testCodecEncodeTerm codec output_ g) (\outputCode -> Right [
            Strings.cat [
              "def ",
              formattedName,
              "():"],
            (Strings.cat [
              "    assert (",
              inputCode,
              ") == (",
              outputCode,
              ")"])])))
        _ -> Right []

-- | Generate a complete test file using the Python codec
generateTestFileWithPythonCodec :: Testing.TestCodec -> Module.Module -> Testing.TestGroup -> t0 -> Graph.Graph -> Either String (String, String)
generateTestFileWithPythonCodec codec testModule testGroup namespaces_ g =
    Eithers.map (\testBody ->  
      let testModuleContent = buildPythonTestModule codec testModule testGroup testBody namespaces_ 
          ns_ = Module.moduleNamespace testModule
          parts = Strings.splitOn "." (Module.unNamespace ns_)
          dirParts = Lists.init parts
          fileName =
                  Strings.cat [
                    "test_",
                    (Lists.last parts),
                    ".py"]
          filePath =
                  Strings.cat [
                    Strings.intercalate "/" dirParts,
                    "/",
                    fileName]
      in (filePath, testModuleContent)) (generatePythonTestGroupHierarchy g namespaces_ codec [] testGroup)

-- | Build the complete Python test module content
buildPythonTestModule :: Testing.TestCodec -> t0 -> Testing.TestGroup -> String -> t1 -> String
buildPythonTestModule codec testModule testGroup testBody namespaces_ =
     
      let groupName_ = Testing.testGroupName testGroup 
          domainImports = Testing.testCodecFindImports codec Sets.empty
          standardImports =
                  [
                    "from __future__ import annotations",
                    "from typing import cast",
                    "from decimal import Decimal",
                    "from hydra.dsl.python import FrozenDict, frozenlist, Either, Left, Right, Maybe, Just, Nothing"]
          allImports = Lists.concat2 standardImports domainImports
          header =
                  Strings.cat [
                    Strings.cat2 "# " Constants.warningAutoGeneratedFile,
                    "\n",
                    (Strings.cat2 "# " groupName_),
                    "\n\n",
                    (Strings.intercalate "\n" allImports),
                    "\n\n"]
      in (Strings.cat [
        header,
        testBody,
        "\n"])

-- | Generate a Python test file for a test group, with type inference
generatePythonTestFile :: Module.Module -> Testing.TestGroup -> Graph.Graph -> Either String (String, String)
generatePythonTestFile testModule testGroup g =
    Eithers.bind (Utils_.inferTestGroupTerms g testGroup) (\inferredTestGroup -> Eithers.bind (namespacesForPythonModule testModule g) (\namespaces_ -> generateTestFileWithPythonCodec (pythonTestCodecWithContext namespaces_ g) testModule inferredTestGroup namespaces_ g))
