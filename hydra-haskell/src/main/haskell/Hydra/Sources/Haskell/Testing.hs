-- | Haskell test code generation codec in Hydra DSL.
-- This module provides DSL versions of Haskell test codec functions for HSpec-based generation tests.

module Hydra.Sources.Haskell.Testing where

-- Standard imports for term-level sources outside of the kernel
import Hydra.Kernel
import Hydra.Sources.Libraries
import           Hydra.Dsl.Meta.Lib.Strings                as Strings
import           Hydra.Dsl.Meta.Phantoms                   as Phantoms
import qualified Hydra.Dsl.Annotations                     as Annotations
import qualified Hydra.Dsl.Bootstrap                       as Bootstrap
import qualified Hydra.Dsl.LiteralTypes                    as LiteralTypes
import qualified Hydra.Dsl.Literals                        as Literals
import qualified Hydra.Dsl.Paths                  as Paths
import qualified Hydra.Dsl.Ast                        as Ast
import qualified Hydra.Dsl.Meta.Base                       as MetaBase
import qualified Hydra.Dsl.Coders                     as Coders
import qualified Hydra.Dsl.Util                    as Util
import qualified Hydra.Dsl.Meta.Context                    as Ctx
import qualified Hydra.Dsl.Meta.Core                       as Core
import qualified Hydra.Dsl.Errors                     as Error
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
import qualified Hydra.Sources.Kernel.Terms.Rewriting      as Rewriting
import qualified Hydra.Sources.Kernel.Terms.Schemas        as Schemas
import qualified Hydra.Sources.Kernel.Terms.Serialization  as Serialization
import qualified Hydra.Sources.Kernel.Terms.Show.Paths as ShowPaths
import qualified Hydra.Sources.Kernel.Terms.Show.Errors    as ShowError
import qualified Hydra.Sources.Kernel.Terms.Show.Core      as ShowCore
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
import qualified Hydra.Ext.Haskell.Syntax as H
import qualified Hydra.Sources.Haskell.Syntax as HaskellSyntax
import qualified Hydra.Sources.Haskell.Coder as HaskellCoderSource
import qualified Hydra.Sources.Haskell.Serde as HaskellSerdeSource
import qualified Hydra.Sources.Haskell.Utils as HaskellUtilsSource
import qualified Hydra.Sources.Test.Utils as TestUtils
import qualified Hydra.Sources.Kernel.Terms.Serialization as SerializationSource


define :: String -> TTerm a -> TBinding a
define = definitionInModule module_


ns :: Namespace
ns = Namespace "hydra.ext.haskell.testing"

module_ :: Module
module_ = Module ns elements
    [Namespace "hydra.ext.haskell.coder", HaskellSerdeSource.ns, HaskellUtilsSource.ns,
     SerializationSource.ns, TestUtils.ns, Formatting.ns, Names.ns,
     Inference.ns, Constants.ns, Rewriting.ns, Substitution.ns, Schemas.ns, ShowError.ns, Lexical.ns]
    (HaskellSyntax.ns:KernelTypes.kernelTypesNamespaces) $
    Just "Haskell test code generation codec for HSpec-based generation tests"
  where
    elements = [
      toTermDefinition addNamespacesToNamespaces,
      toTermDefinition buildNamespacesForTestGroup,
      toTermDefinition buildTestModuleWithCodec,
      toTermDefinition collectNames,
      toTermDefinition collectTestCases,
      toTermDefinition containsTriviallyPolymorphic,
      toTermDefinition extractEncodedTermVariableNames,
      toTermDefinition extractTestTerms,
      toTermDefinition findHaskellImports,
      toTermDefinition generateHaskellTestFile,
      toTermDefinition generateTestCaseWithCodec,
      toTermDefinition generateTestFileWithCodec,
      toTermDefinition generateTestGroupHierarchy,
      toTermDefinition generateTypeAnnotationFor,
      toTermDefinition haskellImportTemplate,
      toTermDefinition haskellModuleTemplate,
      toTermDefinition haskellTestCaseTemplate,
      toTermDefinition haskellTestCodec,
      toTermDefinition haskellTestGroupTemplate,
      toTermDefinition indentContinuationLines,
      toTermDefinition namespaceToModuleName,
      toTermDefinition termToHaskell,
      toTermDefinition tryInferTypeOf,
      toTermDefinition typeToHaskell]


-- | Add namespaces from a set of names to existing namespaces
addNamespacesToNamespaces :: TBinding (Namespaces H.ModuleName -> S.Set Name -> Namespaces H.ModuleName)
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
buildNamespacesForTestGroup :: TBinding (Module -> TestGroup -> Graph -> Either String (Namespaces H.ModuleName))
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


-- | Build the complete test module using a TestCodec
buildTestModuleWithCodec :: TBinding (TestCodec -> Module -> TestGroup -> String -> Namespaces H.ModuleName -> String)
buildTestModuleWithCodec = define "buildTestModuleWithCodec" $
  doc "Build the complete test module using a TestCodec" $
  lambda "codec" $ lambda "testModule" $ lambda "testGroup" $ lambda "testBody" $ lambda "namespaces" $ lets [
    "ns_">: Module.moduleNamespace (var "testModule"),
    "specNs">: wrap _Namespace (Strings.cat2 (unwrap _Namespace @@ var "ns_") (string "Spec")),
    "moduleNameString">: project _TestCodec _TestCodec_formatModuleName @@ var "codec" @@ var "specNs",
    "groupName_">: project _TestGroup _TestGroup_name @@ var "testGroup",
    "domainImports">: project _TestCodec _TestCodec_findImports @@ var "codec" @@ Sets.empty,
    "standardImports">: list [
      string "import Hydra.Kernel",
      string "import qualified Test.Hspec as H",
      string "import qualified Data.List as L",
      string "import qualified Data.Map as M",
      string "import qualified Data.Set as S",
      string "import qualified Data.Maybe as Y"],
    "allImports">: Lists.concat2 (var "standardImports") (var "domainImports"),
    "debugComments">: list [
      string "-- DEBUG: Focus namespace = (see generated module)",
      string "-- DEBUG: Namespace mappings: (see generated module)"],
    "header">: Strings.intercalate (string "\n") (Lists.concat (list [
      list [
        Strings.cat2 (string "-- ") (asTerm Constants.warningAutoGeneratedFile),
        string ""],
      var "debugComments",
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
collectNames :: TBinding (Graph -> S.Set Name -> Term -> S.Set Name)
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
collectTestCases :: TBinding (TestGroup -> [TestCaseWithMetadata])
collectTestCases = define "collectTestCases" $
  doc "Collect all test cases from a test group recursively" $
  lambda "tg" $
    Lists.concat2
      (project _TestGroup _TestGroup_cases @@ var "tg")
      (Lists.concat (Lists.map collectTestCases (project _TestGroup _TestGroup_subgroups @@ var "tg")))


-- | Check if a term contains any trivially polymorphic sub-terms
containsTriviallyPolymorphic :: TBinding (Term -> Bool)
containsTriviallyPolymorphic = define "containsTriviallyPolymorphic" $
  doc "Check if a term contains any trivially polymorphic sub-terms" $
  lambda "term" $
    cases _Term (var "term") (Just false) [
      _Term_list>>: lambda "xs" $
        Logic.or (Lists.null (var "xs"))
          (Lists.foldl (binaryFunction Logic.or) false (Lists.map containsTriviallyPolymorphic (var "xs"))),
      _Term_set>>: lambda "s" $
        Logic.or (Sets.null (var "s"))
          (Lists.foldl (binaryFunction Logic.or) false (Lists.map containsTriviallyPolymorphic (Sets.toList (var "s")))),
      _Term_map>>: lambda "m" $
        Logic.or (Maps.null (var "m"))
          (Logic.or
            (Lists.foldl (binaryFunction Logic.or) false (Lists.map containsTriviallyPolymorphic (Maps.keys (var "m"))))
            (Lists.foldl (binaryFunction Logic.or) false (Lists.map containsTriviallyPolymorphic (Lists.map (lambda "p" $ Pairs.second (var "p")) (Maps.toList (var "m")))))),
      _Term_maybe>>: lambda "mx" $
        Maybes.maybe true containsTriviallyPolymorphic (var "mx"),
      _Term_either>>: lambda "_" $ true,
      _Term_union>>: lambda "inj" $
        containsTriviallyPolymorphic @@ (project _Field _Field_term @@ (project _Injection _Injection_field @@ var "inj")),
      _Term_pair>>: lambda "p" $
        Logic.or
          (containsTriviallyPolymorphic @@ Pairs.first (var "p"))
          (containsTriviallyPolymorphic @@ Pairs.second (var "p")),
      _Term_record>>: lambda "rec" $
        Lists.foldl (binaryFunction Logic.or) false
          (Lists.map (lambda "f" $ containsTriviallyPolymorphic @@ (project _Field _Field_term @@ var "f"))
            (project _Record _Record_fields @@ var "rec")),
      _Term_application>>: lambda "app" $
        Logic.or
          (containsTriviallyPolymorphic @@ (Core.applicationFunction (var "app")))
          (containsTriviallyPolymorphic @@ (Core.applicationArgument (var "app")))]


-- | Extract all variable names from term-encoded terms in a given term
extractEncodedTermVariableNames :: TBinding (Graph -> Term -> S.Set Name)
extractEncodedTermVariableNames = define "extractEncodedTermVariableNames" $
  doc "Extract all variable names from term-encoded terms in a given term" $
  lambda "graf" $ lambda "term" $
    Rewriting.foldOverTerm @@ inject _TraversalOrder _TraversalOrder_pre unit @@ (collectNames @@ var "graf") @@ Sets.empty @@ var "term"


-- | Extract terms from a test case
extractTestTerms :: TBinding (TestCaseWithMetadata -> [Term])
extractTestTerms = define "extractTestTerms" $
  doc "Extract input and output terms from a test case" $
  lambda "tcm" $
    cases _TestCase (project _TestCaseWithMetadata _TestCaseWithMetadata_case @@ var "tcm") (Just (list ([] :: [TTerm Term]))) [
      _TestCase_delegatedEvaluation>>: lambda "delCase" $
        list [
          project _DelegatedEvaluationTestCase _DelegatedEvaluationTestCase_input @@ var "delCase",
          project _DelegatedEvaluationTestCase _DelegatedEvaluationTestCase_output @@ var "delCase"]]


-- | Find necessary imports for Haskell based on referenced names
findHaskellImports :: TBinding (Namespaces H.ModuleName -> S.Set Name -> [String])
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
generateHaskellTestFile :: TBinding (Module -> TestGroup -> Graph -> Either String (String, String))
generateHaskellTestFile = define "generateHaskellTestFile" $
  doc "Generate a Haskell test file for a test group, with type inference and namespace building" $
  lambda "testModule" $ lambda "testGroup" $ lambda "g" $
    Eithers.bind
      (buildNamespacesForTestGroup @@ var "testModule" @@ var "testGroup" @@ var "g")
      (lambda "namespaces" $
        generateTestFileWithCodec @@ (haskellTestCodec @@ var "namespaces") @@ var "testModule" @@ var "testGroup" @@ var "namespaces" @@ var "g")


-- | Generate a single test case using a TestCodec
generateTestCaseWithCodec :: TBinding (Graph -> Namespaces H.ModuleName -> TestCodec -> Int -> TestCaseWithMetadata -> Either String [String])
generateTestCaseWithCodec = define "generateTestCaseWithCodec" $
  doc "Generate a single test case using a TestCodec" $
  lambda "g" $ lambda "namespaces" $ lambda "codec" $ lambda "depth" $ lambda "tcm" $ lets [
    "name_">: project _TestCaseWithMetadata _TestCaseWithMetadata_name @@ var "tcm",
    "tcase">: project _TestCaseWithMetadata _TestCaseWithMetadata_case @@ var "tcm"] $
    cases _TestCase (var "tcase") (Just (right (list ([] :: [TTerm String])))) [
      _TestCase_delegatedEvaluation>>: lambda "delCase" $ lets [
        "input_">: project _DelegatedEvaluationTestCase _DelegatedEvaluationTestCase_input @@ var "delCase",
        "output_">: project _DelegatedEvaluationTestCase _DelegatedEvaluationTestCase_output @@ var "delCase",
        "formattedName">: project _TestCodec _TestCodec_formatTestName @@ var "codec" @@ var "name_",
        "continuationIndent">: Math.add (Math.mul (var "depth") (int32 2)) (int32 4)] $
        Eithers.bind
          (project _TestCodec _TestCodec_encodeTerm @@ var "codec" @@ var "input_" @@ var "g")
          (lambda "inputCode" $
            Eithers.bind
              (project _TestCodec _TestCodec_encodeTerm @@ var "codec" @@ var "output_" @@ var "g")
              (lambda "outputCode" $
                Eithers.bind
                  (generateTypeAnnotationFor @@ var "g" @@ var "namespaces" @@ var "input_" @@ var "output_")
                  (lambda "typeAnnotation" $ lets [
                    "indentedInputCode">: indentContinuationLines @@ var "continuationIndent" @@ var "inputCode",
                    "indentedOutputCode">: indentContinuationLines @@ var "continuationIndent" @@ var "outputCode",
                    "finalOutputCode">: Maybes.maybe (var "indentedOutputCode")
                      (lambda "anno" $ Strings.cat2 (var "indentedOutputCode") (var "anno"))
                      (var "typeAnnotation")] $
                    right (list [
                      Strings.cat (list [string "H.it ", Literals.showString (var "formattedName"), string " $ H.shouldBe"]),
                      Strings.cat (list [string "  (", var "indentedInputCode", string ")"]),
                      Strings.cat (list [string "  (", var "finalOutputCode", string ")"])]))))]


-- | Generate a test file using a TestCodec
generateTestFileWithCodec :: TBinding (TestCodec -> Module -> TestGroup -> Namespaces H.ModuleName -> Graph -> Either String (String, String))
generateTestFileWithCodec = define "generateTestFileWithCodec" $
  doc "Generate a complete test file using a TestCodec" $
  lambda "codec" $ lambda "testModule" $ lambda "testGroup" $ lambda "namespaces" $ lambda "g" $
    Eithers.map
      (lambda "testBody" $ lets [
        "testModuleContent">: buildTestModuleWithCodec @@ var "codec" @@ var "testModule" @@ var "testGroup" @@ var "testBody" @@ var "namespaces",
        "ext">: unwrap _FileExtension @@ (project _TestCodec _TestCodec_fileExtension @@ var "codec"),
        "ns_">: Module.moduleNamespace (var "testModule"),
        "specNs">: wrap _Namespace (Strings.cat2 (unwrap _Namespace @@ var "ns_") (string "Spec")),
        "filePath">: Names.namespaceToFilePath @@ Util.caseConventionPascal @@ (wrap _FileExtension (var "ext")) @@ var "specNs"] $
        pair (var "filePath") (var "testModuleContent"))
      (generateTestGroupHierarchy @@ var "g" @@ var "namespaces" @@ var "codec" @@ int32 1 @@ var "testGroup")


-- | Generate test hierarchy preserving the structure with H.describe blocks for subgroups
generateTestGroupHierarchy :: TBinding (Graph -> Namespaces H.ModuleName -> TestCodec -> Int -> TestGroup -> Either String String)
generateTestGroupHierarchy = define "generateTestGroupHierarchy" $
  doc "Generate test hierarchy preserving the structure with H.describe blocks for subgroups" $
  lambda "g" $ lambda "namespaces" $ lambda "codec" $ lambda "depth" $ lambda "testGroup" $ lets [
    "cases_">: project _TestGroup _TestGroup_cases @@ var "testGroup",
    "subgroups">: project _TestGroup _TestGroup_subgroups @@ var "testGroup",
    "indent">: Strings.fromList (Lists.replicate (Math.mul (var "depth") (int32 2)) (int32 32))] $  -- space char
    Eithers.bind
      (Eithers.mapList
        (lambda "tc" $ generateTestCaseWithCodec @@ var "g" @@ var "namespaces" @@ var "codec" @@ var "depth" @@ var "tc")
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
                  (generateTestGroupHierarchy @@ var "g" @@ var "namespaces" @@ var "codec" @@ Math.add (var "depth") (int32 1) @@ var "subgroup"))
              (var "subgroups"))))


-- | Generate a type annotation for polymorphic output values
generateTypeAnnotationFor :: TBinding (Graph -> Namespaces H.ModuleName -> Term -> Term -> Either String (Maybe String))
generateTypeAnnotationFor = define "generateTypeAnnotationFor" $
  doc "Generate a type annotation for polymorphic output values" $
  lambda "g" $ lambda "namespaces" $ lambda "inputTerm" $ lambda "outputTerm" $
    Logic.ifElse
      (Logic.not (containsTriviallyPolymorphic @@ var "outputTerm"))
      (right nothing)
      (Maybes.maybe
        (right nothing)
        (lambda "result" $ lets [
          "typeScheme">: Pairs.second (var "result"),
          "typ">: project _TypeScheme _TypeScheme_type @@ var "typeScheme",
          "schemaVars">: Sets.fromList (Maps.keys (Graph.graphSchemaTypes (var "g"))),
          "freeVars">: Sets.toList (Sets.difference (Rewriting.freeVariablesInType @@ var "typ") (var "schemaVars")),
          "isEither">: cases _Term (Rewriting.deannotateTerm @@ var "outputTerm") (Just false) [
            _Term_either>>: lambda "_" $ true]] $
          Logic.ifElse
            (Logic.or (var "isEither") (Logic.not (Lists.null (var "freeVars"))))
            (lets [
              "int32Type">: inject _Type _Type_literal (inject _LiteralType _LiteralType_integer (inject _IntegerType _IntegerType_int32 unit)),
              "subst">: Typing.typeSubst (Maps.fromList (Lists.map (lambda "v" $ pair (var "v") (var "int32Type")) (var "freeVars"))),
              "groundedType">: Substitution.substInType @@ var "subst" @@ var "typ"] $
              Eithers.map
                (lambda "typeStr" $ just (Strings.cat2 (string " :: ") (var "typeStr")))
                (typeToHaskell @@ var "namespaces" @@ var "groundedType" @@ var "g"))
            (right nothing))
        (tryInferTypeOf @@ var "g" @@ var "inputTerm"))


-- | Template for Haskell import statements
haskellImportTemplate :: TBinding String
haskellImportTemplate = define "haskellImportTemplate" $
  doc "Template for Haskell import statements" $
  string "import qualified {namespace} as {alias}"


-- | Template for Haskell test module structure
haskellModuleTemplate :: TBinding String
haskellModuleTemplate = define "haskellModuleTemplate" $
  doc "Template for Haskell test module structure" $
  Strings.intercalate (string "\n") (list [
    Strings.cat2 (string "-- ") (asTerm Constants.warningAutoGeneratedFile),
    string "",
    string "module {moduleName} where",
    string "",
    string "{imports}",
    string "",
    string "spec :: H.Spec",
    string "{testGroup}",
    string "{testCases}",
    string ""])


-- | Template for HSpec test case assertions
haskellTestCaseTemplate :: TBinding String
haskellTestCaseTemplate = define "haskellTestCaseTemplate" $
  doc "Template for HSpec test case assertions" $
  Strings.intercalate (string "\n") (list [
    string "  H.it {name} $ H.shouldBe",
    string "    ({input})",
    string "    ({output})",
    string ""])


-- | Create a Haskell TestCodec that uses the real Haskell coder
haskellTestCodec :: TBinding (Namespaces H.ModuleName -> TestCodec)
haskellTestCodec = define "haskellTestCodec" $
  doc "Create a Haskell TestCodec that uses the real Haskell coder" $
  lambda "namespaces" $
    record _TestCodec [
      _TestCodec_language>>: Coders.languageName_ (string "haskell"),
      _TestCodec_fileExtension>>: wrap _FileExtension (string "hs"),
      _TestCodec_encodeTerm>>: termToHaskell @@ var "namespaces",
      _TestCodec_encodeType>>: typeToHaskell @@ var "namespaces",
      _TestCodec_formatTestName>>: lambda "n" $ var "n",  -- identity
      _TestCodec_formatModuleName>>: namespaceToModuleName,
      _TestCodec_testCaseTemplate>>: haskellTestCaseTemplate,
      _TestCodec_testGroupTemplate>>: haskellTestGroupTemplate,
      _TestCodec_moduleTemplate>>: haskellModuleTemplate,
      _TestCodec_importTemplate>>: haskellImportTemplate,
      _TestCodec_findImports>>: findHaskellImports @@ var "namespaces"]


-- | Template for HSpec test group description
haskellTestGroupTemplate :: TBinding String
haskellTestGroupTemplate = define "haskellTestGroupTemplate" $
  doc "Template for HSpec test group description" $
  string "spec = H.describe {groupName} $ do"


-- | Indent continuation lines of a multi-line string
indentContinuationLines :: TBinding (Int -> String -> String)
indentContinuationLines = define "indentContinuationLines" $
  doc "Indent continuation lines of a multi-line string" $
  lambda "n" $ lambda "s" $
    Strings.intercalate
      (Strings.cat2 (string "\n") (Strings.fromList (Lists.replicate (var "n") (int32 32))))
      (Strings.splitOn (string "\n") (var "s"))


-- | Convert namespace to Haskell module name
namespaceToModuleName :: TBinding (Namespace -> String)
namespaceToModuleName = define "namespaceToModuleName" $
  doc "Convert namespace to Haskell module name" $
  lambda "ns_" $
    Strings.intercalate (string ".") (Lists.map Formatting.capitalize (Strings.splitOn (string ".") (unwrap _Namespace @@ var "ns_")))


-- | Convert a Hydra term to a Haskell expression string
termToHaskell :: TBinding (Namespaces H.ModuleName -> Term -> Graph -> Either String String)
termToHaskell = define "termToHaskell" $
  doc "Convert a Hydra term to a Haskell expression string" $
  lambda "namespaces" $ lambda "term" $ lambda "g" $
    Eithers.bimap
      ("ic" ~> ShowError.error_ @@ Ctx.inContextObject (var "ic"))
      (Serialization.printExpr <.> Serialization.parenthesize <.> HaskellSerdeSource.expressionToExpr)
      (HaskellCoderSource.encodeTerm @@ int32 0 @@ var "namespaces" @@ var "term" @@ asTerm Lexical.emptyContext @@ var "g")


-- | Try to infer the type of a term, returning Nothing if inference fails
tryInferTypeOf :: TBinding (Graph -> Term -> Maybe (Term, TypeScheme))
tryInferTypeOf = define "tryInferTypeOf" $
  doc "Try to infer the type of a term, returning Nothing if inference fails" $
  lambda "g" $ lambda "term" $
    Eithers.either_
      (lambda "_" $ nothing)
      (lambda "result" $ just (Pairs.first (var "result")))
      (Inference.inferTypeOf @@ asTerm Lexical.emptyContext @@ var "g" @@ var "term")


-- | Convert a Hydra type to a Haskell type expression string
typeToHaskell :: TBinding (Namespaces H.ModuleName -> Type -> Graph -> Either String String)
typeToHaskell = define "typeToHaskell" $
  doc "Convert a Hydra type to a Haskell type expression string" $
  lambda "namespaces" $ lambda "typ" $ lambda "g" $
    Eithers.bimap
      ("ic" ~> ShowError.error_ @@ Ctx.inContextObject (var "ic"))
      (Serialization.printExpr <.> Serialization.parenthesize <.> HaskellSerdeSource.typeToExpr)
      (HaskellCoderSource.encodeType @@ var "namespaces" @@ var "typ" @@ asTerm Lexical.emptyContext @@ var "g")
