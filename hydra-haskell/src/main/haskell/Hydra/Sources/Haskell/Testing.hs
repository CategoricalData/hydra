-- | Haskell test code generation in Hydra DSL.
-- This module generates HSpec-based test files for Haskell from universal test cases.

module Hydra.Sources.Haskell.Testing where

-- Standard imports for term-level sources outside of the kernel
import Hydra.Kernel
import Hydra.Sources.Libraries
import           Hydra.Dsl.Meta.Lib.Strings                as Strings
import           Hydra.Dsl.Meta.Phantoms                   as Phantoms
import qualified Hydra.Dsl.Meta.Context                    as Ctx
import qualified Hydra.Dsl.Meta.Core                       as Core
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
import qualified Hydra.Dsl.Util                       as Util
import qualified Hydra.Sources.Kernel.Terms.Constants      as Constants
import qualified Hydra.Sources.Kernel.Terms.Formatting     as Formatting
import qualified Hydra.Sources.Kernel.Terms.Lexical        as Lexical
import qualified Hydra.Sources.Kernel.Terms.Names          as Names
import qualified Hydra.Sources.Kernel.Terms.Rewriting      as Rewriting
import qualified Hydra.Sources.Kernel.Terms.Schemas        as Schemas
import qualified Hydra.Sources.Kernel.Terms.Show.Errors    as ShowError
import qualified Hydra.Sources.Kernel.Types.All            as KernelTypes
import           Prelude hiding ((++))
import qualified Data.Map                                  as M
import qualified Data.Set                                  as S

-- Additional imports
import qualified Hydra.Ext.Haskell.Syntax as H
import qualified Hydra.Sources.Haskell.Syntax as HaskellSyntax
import qualified Hydra.Sources.Haskell.Utils as HaskellUtilsSource


define :: String -> TTerm a -> TTermDefinition a
define = definitionInModule module_


ns :: Namespace
ns = Namespace "hydra.ext.haskell.testing"

module_ :: Module
module_ = Module ns elements
    [HaskellUtilsSource.ns, Formatting.ns, Names.ns,
     Constants.ns, Rewriting.ns, Schemas.ns, ShowError.ns, Lexical.ns]
    (HaskellSyntax.ns:KernelTypes.kernelTypesNamespaces) $
    Just "Haskell test code generation for HSpec-based generation tests"
  where
    elements = [
      toDefinition addNamespacesToNamespaces,
      toDefinition buildNamespacesForTestGroup,
      toDefinition buildTestModule,
      toDefinition collectNames,
      toDefinition collectTestCases,
      toDefinition extractEncodedTermVariableNames,
      toDefinition extractTestTerms,
      toDefinition findHaskellImports,
      toDefinition generateHaskellTestFile,
      toDefinition generateTestCase,
      toDefinition generateTestFile,
      toDefinition generateTestGroupHierarchy,
      toDefinition namespaceToModuleName]


-- | Add namespaces from a set of names to existing namespaces
addNamespacesToNamespaces :: TTermDefinition (Namespaces H.ModuleName -> S.Set Name -> Namespaces H.ModuleName)
addNamespacesToNamespaces = define "addNamespacesToNamespaces" $
  doc "Add namespaces from a set of names to existing namespaces" $
  lambda "ns0" $ lambda "names" $ lets [
    "newNamespaces">: Sets.fromList (Maybes.cat (Lists.map Names.namespaceOf (Sets.toList (var "names")))),
    "toModuleName">: lambda "namespace" $
      wrap H._ModuleName (Formatting.capitalize @@ Lists.last (Strings.splitOn (string ".") (unwrap _Namespace @@ var "namespace"))),
    "newMappings">: Maps.fromList (Lists.map (lambda "ns_" $ pair (var "ns_") (var "toModuleName" @@ var "ns_")) (Sets.toList (var "newNamespaces")))] $
    record _Namespaces [
      _Namespaces_focus>>: project _Namespaces _Namespaces_focus @@ var "ns0",
      _Namespaces_mapping>>: Maps.union (project _Namespaces _Namespaces_mapping @@ var "ns0") (var "newMappings")]


-- | Build namespaces for a test group including encoded term references
buildNamespacesForTestGroup :: TTermDefinition (Module -> TestGroup -> Graph -> Either String (Namespaces H.ModuleName))
buildNamespacesForTestGroup = define "buildNamespacesForTestGroup" $
  doc "Build namespaces for a test group including encoded term references" $
  lambda "mod" $ lambda "tgroup" $ lambda "graph_" $ lets [
    "testCases_">: collectTestCases @@ var "tgroup",
    "testTerms">: Lists.concat (Lists.map extractTestTerms (var "testCases_")),
    "testBindings">: Lists.map
      (lambda "term" $
        record _Binding [
          _Binding_name>>: wrap _Name (string "_test_"),
          _Binding_term>>: var "term",
          _Binding_type>>: nothing])
      (var "testTerms"),
    "tempModule">: record _Module [
      _Module_namespace>>: Module.moduleNamespace (var "mod"),
      _Module_definitions>>: Lists.map ("b" ~> Module.definitionTerm (Module.termDefinition
        (Core.bindingName $ var "b") (Core.bindingTerm $ var "b")
        (Core.bindingType $ var "b")))
        (var "testBindings"),
      _Module_termDependencies>>: project _Module _Module_termDependencies @@ var "mod",
      _Module_typeDependencies>>: project _Module _Module_typeDependencies @@ var "mod",
      _Module_description>>: project _Module _Module_description @@ var "mod"]] $
    Eithers.bind
      (Eithers.bimap
        (lambda "ic" $ ShowError.error_ @@ Ctx.inContextObject (var "ic"))
        (lambda "a" $ var "a")
        (HaskellUtilsSource.namespacesForModule @@ var "tempModule" @@ asTerm Lexical.emptyContext @@ var "graph_"))
      (lambda "baseNamespaces" $ lets [
        "encodedNames">: Sets.unions (Lists.map (lambda "t" $ extractEncodedTermVariableNames @@ var "graph_" @@ var "t") (var "testTerms"))] $
        right (addNamespacesToNamespaces @@ var "baseNamespaces" @@ var "encodedNames"))


-- | Build the complete test module for Haskell HSpec
buildTestModule :: TTermDefinition (Module -> TestGroup -> String -> Namespaces H.ModuleName -> String)
buildTestModule = define "buildTestModule" $
  doc "Build the complete test module for Haskell HSpec" $
  lambda "testModule" $ lambda "testGroup" $ lambda "testBody" $ lambda "namespaces" $ lets [
    "ns_">: Module.moduleNamespace (var "testModule"),
    "specNs">: wrap _Namespace (Strings.cat2 (unwrap _Namespace @@ var "ns_") (string "Spec")),
    "moduleNameString">: namespaceToModuleName @@ var "specNs",
    "groupName_">: project _TestGroup _TestGroup_name @@ var "testGroup",
    "domainImports">: findHaskellImports @@ var "namespaces" @@ Sets.empty,
    "standardImports">: list [
      string "import Hydra.Kernel",
      string "import qualified Test.Hspec as H",
      string "import qualified Data.List as L",
      string "import qualified Data.Map as M",
      string "import qualified Data.Set as S",
      string "import qualified Data.Maybe as Y"],
    "allImports">: Lists.concat2 (var "standardImports") (var "domainImports"),
    "header">: Strings.intercalate (string "\n") (Lists.concat (list [
      list [
        Strings.cat2 (string "-- ") (asTerm Constants.warningAutoGeneratedFile),
        string ""],
      list [
        string "",
        Strings.cat (list [string "module ", var "moduleNameString", string " where"]),
        string ""],
      var "allImports",
      list [
        string "",
        string "spec :: H.Spec",
        Strings.cat (list [string "spec = H.describe ", Literals.showString (var "groupName_"), string " $ do"])]]))] $
    Strings.cat (list [var "header", string "\n", var "testBody", string "\n"])


-- | Collect variable names from encoded terms within a single term node
collectNames :: TTermDefinition (Graph -> S.Set Name -> Term -> S.Set Name)
collectNames = define "collectNames" $
  doc "Collect variable names from encoded terms within a single term node" $
  lambda "graf" $ lambda "names" $ lambda "t" $
    Logic.ifElse (Schemas.isEncodedTerm @@ (Rewriting.deannotateTerm @@ var "t"))
      (Eithers.either_
        (lambda "_" $ var "names")
        (lambda "decodedTerm" $
          Sets.union (var "names") (Rewriting.termDependencyNames @@ true @@ true @@ true @@ var "decodedTerm"))
        (Eithers.bimap
          (lambda "_e" $ var "_e")
          (lambda "_a" $ var "_a")
          (decoderFor _Term @@ var "graf" @@ var "t")))
      (var "names")


-- | Collect all test cases from a test group (recursively)
collectTestCases :: TTermDefinition (TestGroup -> [TestCaseWithMetadata])
collectTestCases = define "collectTestCases" $
  doc "Collect all test cases from a test group recursively" $
  lambda "tg" $
    Lists.concat2
      (project _TestGroup _TestGroup_cases @@ var "tg")
      (Lists.concat (Lists.map collectTestCases (project _TestGroup _TestGroup_subgroups @@ var "tg")))


-- | Extract all variable names from term-encoded terms in a given term
extractEncodedTermVariableNames :: TTermDefinition (Graph -> Term -> S.Set Name)
extractEncodedTermVariableNames = define "extractEncodedTermVariableNames" $
  doc "Extract all variable names from term-encoded terms in a given term" $
  lambda "graf" $ lambda "term" $
    Rewriting.foldOverTerm @@ inject _TraversalOrder _TraversalOrder_pre unit @@ (collectNames @@ var "graf") @@ Sets.empty @@ var "term"


-- | Extract terms from a test case
extractTestTerms :: TTermDefinition (TestCaseWithMetadata -> [Term])
extractTestTerms = define "extractTestTerms" $
  doc "Extract input and output terms from a test case" $
  lambda "tcm" $
    list ([] :: [TTerm Term])


-- | Find necessary imports for Haskell based on referenced names
findHaskellImports :: TTermDefinition (Namespaces H.ModuleName -> S.Set Name -> [String])
findHaskellImports = define "findHaskellImports" $
  doc "Find necessary imports for Haskell based on referenced names" $
  lambda "namespaces" $ lambda "names_" $ lets [
    "mapping_">: project _Namespaces _Namespaces_mapping @@ var "namespaces",
    "filtered">: Maps.filterWithKey
      (lambda "ns_" $ lambda "_v" $
        Logic.not (Equality.equal
          (Lists.head (Strings.splitOn (string "hydra.test.") (unwrap _Namespace @@ var "ns_")))
          (string "")))
      (var "mapping_")] $
    Lists.map
      (lambda "entry" $ Strings.cat (list [
        string "import qualified ",
        Strings.intercalate (string ".") (Lists.map Formatting.capitalize (Strings.splitOn (string ".") (unwrap _Namespace @@ Pairs.first (var "entry")))),
        string " as ",
        unwrap H._ModuleName @@ Pairs.second (var "entry")]))
      (Maps.toList (var "filtered"))


-- | Generate a Haskell test file for a test group, with type inference and namespace building
generateHaskellTestFile :: TTermDefinition (Module -> TestGroup -> Graph -> Either String (String, String))
generateHaskellTestFile = define "generateHaskellTestFile" $
  doc "Generate a Haskell test file for a test group, with type inference and namespace building" $
  lambda "testModule" $ lambda "testGroup" $ lambda "g" $
    Eithers.bind
      (buildNamespacesForTestGroup @@ var "testModule" @@ var "testGroup" @@ var "g")
      (lambda "namespaces" $
        generateTestFile @@ var "testModule" @@ var "testGroup" @@ var "namespaces")


-- | Generate a single HSpec test case from a universal test case
generateTestCase :: TTermDefinition (Int -> TestCaseWithMetadata -> Either String [String])
generateTestCase = define "generateTestCase" $
  doc "Generate a single HSpec test case from a universal test case" $
  lambda "depth" $ lambda "tcm" $ lets [
    "name_">: project _TestCaseWithMetadata _TestCaseWithMetadata_name @@ var "tcm",
    "tcase">: project _TestCaseWithMetadata _TestCaseWithMetadata_case @@ var "tcm",
    "universal">: match _TestCase Nothing ["universal">: lambda "u" $ var "u"] @@ var "tcase",
    "actual_">: project _UniversalTestCase _UniversalTestCase_actual @@ var "universal",
    "expected_">: project _UniversalTestCase _UniversalTestCase_expected @@ var "universal"] $
    right (list [
      Strings.cat (list [string "H.it ", Literals.showString (var "name_"), string " $ H.shouldBe"]),
      Strings.cat (list [string "  (", var "actual_", string ")"]),
      Strings.cat (list [string "  (", var "expected_", string ")"])])


-- | Generate a complete Haskell test file
generateTestFile :: TTermDefinition (Module -> TestGroup -> Namespaces H.ModuleName -> Either String (String, String))
generateTestFile = define "generateTestFile" $
  doc "Generate a complete Haskell test file" $
  lambda "testModule" $ lambda "testGroup" $ lambda "namespaces" $
    Eithers.map
      (lambda "testBody" $ lets [
        "testModuleContent">: buildTestModule @@ var "testModule" @@ var "testGroup" @@ var "testBody" @@ var "namespaces",
        "ns_">: Module.moduleNamespace (var "testModule"),
        "specNs">: wrap _Namespace (Strings.cat2 (unwrap _Namespace @@ var "ns_") (string "Spec")),
        "filePath">: Names.namespaceToFilePath @@ Util.caseConventionPascal @@ (wrap _FileExtension (string "hs")) @@ var "specNs"] $
        pair (var "filePath") (var "testModuleContent"))
      (generateTestGroupHierarchy @@ int32 1 @@ var "testGroup")


-- | Generate test hierarchy preserving the structure with H.describe blocks for subgroups
generateTestGroupHierarchy :: TTermDefinition (Int -> TestGroup -> Either String String)
generateTestGroupHierarchy = define "generateTestGroupHierarchy" $
  doc "Generate test hierarchy preserving the structure with H.describe blocks for subgroups" $
  lambda "depth" $ lambda "testGroup" $ lets [
    "cases_">: project _TestGroup _TestGroup_cases @@ var "testGroup",
    "subgroups">: project _TestGroup _TestGroup_subgroups @@ var "testGroup",
    "indent">: Strings.fromList (Lists.replicate (Math.mul (var "depth") (int32 2)) (int32 32))] $  -- space char
    Eithers.bind
      (Eithers.mapList
        (lambda "tc" $ generateTestCase @@ var "depth" @@ var "tc")
        (var "cases_"))
      (lambda "testCaseLinesRaw" $ lets [
        "testCaseLines">: Lists.map
          (lambda "lines_" $ Lists.map (lambda "line" $ Strings.cat2 (var "indent") (var "line")) (var "lines_"))
          (var "testCaseLinesRaw"),
        "testCasesStr">: Strings.intercalate (string "\n") (Lists.concat (var "testCaseLines"))] $
        Eithers.map
          (lambda "subgroupsStr" $
            Strings.cat (list [
              var "testCasesStr",
              Logic.ifElse (Logic.or (Equality.equal (var "testCasesStr") (string "")) (Equality.equal (var "subgroupsStr") (string "")))
                (string "")
                (string "\n"),
              var "subgroupsStr"]))
          (Eithers.map
            (lambda "blocks" $ Strings.intercalate (string "\n") (var "blocks"))
            (Eithers.mapList
              (lambda "subgroup" $ lets [
                "groupName_">: project _TestGroup _TestGroup_name @@ var "subgroup"] $
                Eithers.map
                  (lambda "content" $
                    Strings.cat (list [
                      var "indent",
                      string "H.describe ",
                      Literals.showString (var "groupName_"),
                      string " $ do\n",
                      var "content"]))
                  (generateTestGroupHierarchy @@ Math.add (var "depth") (int32 1) @@ var "subgroup"))
              (var "subgroups"))))


-- | Convert namespace to Haskell module name
namespaceToModuleName :: TTermDefinition (Namespace -> String)
namespaceToModuleName = define "namespaceToModuleName" $
  doc "Convert namespace to Haskell module name" $
  lambda "ns_" $
    Strings.intercalate (string ".") (Lists.map Formatting.capitalize (Strings.splitOn (string ".") (unwrap _Namespace @@ var "ns_")))


