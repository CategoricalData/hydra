-- Note: this is an automatically generated file. Do not edit.

-- | Haskell test code generation for HSpec-based generation tests

module Hydra.Haskell.Testing where

import qualified Hydra.Coders as Coders
import qualified Hydra.Constants as Constants
import qualified Hydra.Core as Core
import qualified Hydra.Decode.Core as DecodeCore
import qualified Hydra.Dependencies as Dependencies
import qualified Hydra.Formatting as Formatting
import qualified Hydra.Graph as Graph
import qualified Hydra.Haskell.Syntax as Syntax
import qualified Hydra.Haskell.Utils as Utils
import qualified Hydra.Lexical as Lexical
import qualified Hydra.Lib.Eithers as Eithers
import qualified Hydra.Lib.Equality as Equality
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Literals as Literals
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Math as Math
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Pairs as Pairs
import qualified Hydra.Lib.Sets as Sets
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Names as Names
import qualified Hydra.Packaging as Packaging
import qualified Hydra.Predicates as Predicates
import qualified Hydra.Rewriting as Rewriting
import qualified Hydra.Show.Errors as Errors
import qualified Hydra.Strip as Strip
import qualified Hydra.Testing as Testing
import qualified Hydra.Util as Util
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Set as S

-- | Add namespaces from a set of names to existing namespaces
addNamespacesToNamespaces :: Packaging.Namespaces Syntax.ModuleName -> S.Set Core.Name -> Packaging.Namespaces Syntax.ModuleName
addNamespacesToNamespaces ns0 names =

      let newNamespaces = Sets.fromList (Maybes.cat (Lists.map Names.namespaceOf (Sets.toList names)))
          toModuleName =
                  \namespace -> Syntax.ModuleName (Formatting.capitalize (Lists.last (Strings.splitOn "." (Packaging.unNamespace namespace))))
          newMappings = Maps.fromList (Lists.map (\ns_ -> (ns_, (toModuleName ns_))) (Sets.toList newNamespaces))
      in Packaging.Namespaces {
        Packaging.namespacesFocus = (Packaging.namespacesFocus ns0),
        Packaging.namespacesMapping = (Maps.union (Packaging.namespacesMapping ns0) newMappings)}

-- | Build namespaces for a test group including encoded term references
buildNamespacesForTestGroup :: Packaging.Module -> Testing.TestGroup -> Graph.Graph -> Either String (Packaging.Namespaces Syntax.ModuleName)
buildNamespacesForTestGroup mod tgroup graph_ =

      let testCases_ = collectTestCases tgroup
          testTerms = Lists.concat (Lists.map extractTestTerms testCases_)
          testBindings =
                  Lists.map (\term -> Core.Binding {
                    Core.bindingName = (Core.Name "_test_"),
                    Core.bindingTerm = term,
                    Core.bindingType = Nothing}) testTerms
          tempModule =
                  Packaging.Module {
                    Packaging.moduleNamespace = (Packaging.moduleNamespace mod),
                    Packaging.moduleDefinitions = (Lists.map (\b -> Packaging.DefinitionTerm (Packaging.TermDefinition {
                      Packaging.termDefinitionName = (Core.bindingName b),
                      Packaging.termDefinitionTerm = (Core.bindingTerm b),
                      Packaging.termDefinitionType = (Core.bindingType b)})) testBindings),
                    Packaging.moduleTermDependencies = (Packaging.moduleTermDependencies mod),
                    Packaging.moduleTypeDependencies = (Packaging.moduleTypeDependencies mod),
                    Packaging.moduleDescription = (Packaging.moduleDescription mod)}
      in (Eithers.bind (Eithers.bimap (\e -> Errors.error e) (\a -> a) (Utils.namespacesForModule tempModule Lexical.emptyContext graph_)) (\baseNamespaces ->
        let encodedNames = Sets.unions (Lists.map (\t -> extractEncodedTermVariableNames graph_ t) testTerms)
        in (Right (addNamespacesToNamespaces baseNamespaces encodedNames))))

-- | Build the complete test module for Haskell HSpec
buildTestModule :: Packaging.Module -> Testing.TestGroup -> String -> Packaging.Namespaces Syntax.ModuleName -> String
buildTestModule testModule testGroup testBody namespaces =

      let ns_ = Packaging.moduleNamespace testModule
          specNs = Packaging.Namespace (Strings.cat2 (Packaging.unNamespace ns_) "Spec")
          moduleNameString = namespaceToModuleName specNs
          groupName_ = Testing.testGroupName testGroup
          domainImports = findHaskellImports namespaces Sets.empty
          standardImports =
                  [
                    "import Hydra.Kernel",
                    "import qualified Test.Hspec as H",
                    "import qualified Data.List as L",
                    "import qualified Data.Map as M",
                    "import qualified Data.Set as S",
                    "import qualified Data.Maybe as Y"]
          allImports = Lists.concat2 standardImports domainImports
          header =
                  Strings.intercalate "\n" (Lists.concat [
                    [
                      Strings.cat2 "-- " Constants.warningAutoGeneratedFile,
                      ""],
                    [
                      "",
                      (Strings.cat [
                        "module ",
                        moduleNameString,
                        " where"]),
                      ""],
                    allImports,
                    [
                      "",
                      "spec :: H.Spec",
                      (Strings.cat [
                        "spec = H.describe ",
                        (Literals.showString groupName_),
                        " $ do"])]])
      in (Strings.cat [
        header,
        "\n",
        testBody,
        "\n"])

-- | Collect variable names from encoded terms within a single term node
collectNames :: Graph.Graph -> S.Set Core.Name -> Core.Term -> S.Set Core.Name
collectNames graf names t =
    Logic.ifElse (Predicates.isEncodedTerm (Strip.deannotateTerm t)) (Eithers.either (\_ -> names) (\decodedTerm -> Sets.union names (Dependencies.termDependencyNames True True True decodedTerm)) (Eithers.bimap (\_e -> _e) (\_a -> _a) (DecodeCore.term graf t))) names

-- | Collect all test cases from a test group recursively
collectTestCases :: Testing.TestGroup -> [Testing.TestCaseWithMetadata]
collectTestCases tg =
    Lists.concat2 (Testing.testGroupCases tg) (Lists.concat (Lists.map collectTestCases (Testing.testGroupSubgroups tg)))

-- | Extract all variable names from term-encoded terms in a given term
extractEncodedTermVariableNames :: Graph.Graph -> Core.Term -> S.Set Core.Name
extractEncodedTermVariableNames graf term =
    Rewriting.foldOverTerm Coders.TraversalOrderPre (collectNames graf) Sets.empty term

-- | Extract input and output terms from a test case
extractTestTerms :: t0 -> [t1]
extractTestTerms tcm = []

-- | Find necessary imports for Haskell based on referenced names
findHaskellImports :: Packaging.Namespaces Syntax.ModuleName -> t0 -> [String]
findHaskellImports namespaces names_ =

      let mapping_ = Packaging.namespacesMapping namespaces
          filtered =
                  Maps.filterWithKey (\ns_ -> \_v -> Logic.not (Equality.equal (Lists.head (Strings.splitOn "hydra.test." (Packaging.unNamespace ns_))) "")) mapping_
      in (Lists.map (\entry -> Strings.cat [
        "import qualified ",
        (Strings.intercalate "." (Lists.map Formatting.capitalize (Strings.splitOn "." (Packaging.unNamespace (Pairs.first entry))))),
        " as ",
        (Syntax.unModuleName (Pairs.second entry))]) (Maps.toList filtered))

-- | Generate a Haskell test file for a test group, with type inference and namespace building
generateHaskellTestFile :: Packaging.Module -> Testing.TestGroup -> Graph.Graph -> Either String (String, String)
generateHaskellTestFile testModule testGroup g =
    Eithers.bind (buildNamespacesForTestGroup testModule testGroup g) (\namespaces -> generateTestFile testModule testGroup namespaces)

-- | Generate a single HSpec test case from a universal test case
generateTestCase :: t0 -> Testing.TestCaseWithMetadata -> Either t1 [String]
generateTestCase depth tcm =

      let name_ = Testing.testCaseWithMetadataName tcm
          tcase = Testing.testCaseWithMetadataCase tcm
          universal =
                  case tcase of
                    Testing.TestCaseUniversal v0 -> v0
          actual_ = Testing.universalTestCaseActual universal
          expected_ = Testing.universalTestCaseExpected universal
      in (Right [
        Strings.cat [
          "H.it ",
          (Literals.showString name_),
          " $ H.shouldBe"],
        (Strings.cat [
          "  (",
          actual_,
          ")"]),
        (Strings.cat [
          "  (",
          expected_,
          ")"])])

-- | Generate a complete Haskell test file
generateTestFile :: Packaging.Module -> Testing.TestGroup -> Packaging.Namespaces Syntax.ModuleName -> Either t0 (String, String)
generateTestFile testModule testGroup namespaces =
    Eithers.map (\testBody ->
      let testModuleContent = buildTestModule testModule testGroup testBody namespaces
          ns_ = Packaging.moduleNamespace testModule
          specNs = Packaging.Namespace (Strings.cat2 (Packaging.unNamespace ns_) "Spec")
          filePath = Names.namespaceToFilePath Util.CaseConventionPascal (Packaging.FileExtension "hs") specNs
      in (filePath, testModuleContent)) (generateTestGroupHierarchy 1 testGroup)

-- | Generate test hierarchy preserving the structure with H.describe blocks for subgroups
generateTestGroupHierarchy :: Int -> Testing.TestGroup -> Either t0 String
generateTestGroupHierarchy depth testGroup =

      let cases_ = Testing.testGroupCases testGroup
          subgroups = Testing.testGroupSubgroups testGroup
          indent = Strings.fromList (Lists.replicate (Math.mul depth 2) 32)
      in (Eithers.bind (Eithers.mapList (\tc -> generateTestCase depth tc) cases_) (\testCaseLinesRaw ->
        let testCaseLines = Lists.map (\lines_ -> Lists.map (\line -> Strings.cat2 indent line) lines_) testCaseLinesRaw
            testCasesStr = Strings.intercalate "\n" (Lists.concat testCaseLines)
        in (Eithers.map (\subgroupsStr -> Strings.cat [
          testCasesStr,
          (Logic.ifElse (Logic.or (Equality.equal testCasesStr "") (Equality.equal subgroupsStr "")) "" "\n"),
          subgroupsStr]) (Eithers.map (\blocks -> Strings.intercalate "\n" blocks) (Eithers.mapList (\subgroup ->
          let groupName_ = Testing.testGroupName subgroup
          in (Eithers.map (\content -> Strings.cat [
            indent,
            "H.describe ",
            (Literals.showString groupName_),
            " $ do\n",
            content]) (generateTestGroupHierarchy (Math.add depth 1) subgroup))) subgroups)))))

-- | Convert namespace to Haskell module name
namespaceToModuleName :: Packaging.Namespace -> String
namespaceToModuleName ns_ =
    Strings.intercalate "." (Lists.map Formatting.capitalize (Strings.splitOn "." (Packaging.unNamespace ns_)))
