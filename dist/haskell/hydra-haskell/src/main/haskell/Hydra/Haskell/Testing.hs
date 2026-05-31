-- Note: this is an automatically generated file. Do not edit.
-- | Haskell test code generation for HSpec-based generation tests

module Hydra.Haskell.Testing where
import qualified Hydra.Ast as Ast
import qualified Hydra.Coders as Coders
import qualified Hydra.Constants as Constants
import qualified Hydra.Core as Core
import qualified Hydra.Decode.Core as DecodeCore
import qualified Hydra.Dependencies as Dependencies
import qualified Hydra.Error.Checking as Checking
import qualified Hydra.Error.Core as ErrorCore
import qualified Hydra.Error.Packaging as ErrorPackaging
import qualified Hydra.Errors as Errors
import qualified Hydra.Formatting as Formatting
import qualified Hydra.Graph as Graph
import qualified Hydra.Haskell.Syntax as Syntax
import qualified Hydra.Haskell.Utils as Utils
import qualified Hydra.Json.Model as Model
import qualified Hydra.Lexical as Lexical
import qualified Hydra.Haskell.Lib.Eithers as Eithers
import qualified Hydra.Haskell.Lib.Equality as Equality
import qualified Hydra.Haskell.Lib.Lists as Lists
import qualified Hydra.Haskell.Lib.Literals as Literals
import qualified Hydra.Haskell.Lib.Logic as Logic
import qualified Hydra.Haskell.Lib.Maps as Maps
import qualified Hydra.Haskell.Lib.Math as Math
import qualified Hydra.Haskell.Lib.Maybes as Maybes
import qualified Hydra.Haskell.Lib.Pairs as Pairs
import qualified Hydra.Haskell.Lib.Sets as Sets
import qualified Hydra.Haskell.Lib.Strings as Strings
import qualified Hydra.Names as Names
import qualified Hydra.Packaging as Packaging
import qualified Hydra.Parsing as Parsing
import qualified Hydra.Paths as Paths
import qualified Hydra.Predicates as Predicates
import qualified Hydra.Query as Query
import qualified Hydra.Relational as Relational
import qualified Hydra.Rewriting as Rewriting
import qualified Hydra.Scoping as Scoping
import qualified Hydra.Show.Errors as ShowErrors
import qualified Hydra.Strip as Strip
import qualified Hydra.Tabular as Tabular
import qualified Hydra.Testing as Testing
import qualified Hydra.Topology as Topology
import qualified Hydra.Typed as Typed
import qualified Hydra.Typing as Typing
import qualified Hydra.Util as Util
import qualified Hydra.Validation as Validation
import qualified Hydra.Variants as Variants
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
import qualified Data.Set as S
-- | Add namespaces from a set of names to existing namespaces
addNamespacesToNamespaces :: Util.ModuleNames Syntax.ModuleName -> S.Set Core.Name -> Util.ModuleNames Syntax.ModuleName
addNamespacesToNamespaces ns0 names =

      let newNamespaces = Sets.fromList (Maybes.cat (Lists.map Names.moduleNameOf (Sets.toList names)))
          toModuleName =
                  \namespace -> Syntax.ModuleName (Formatting.capitalize (Maybes.fromMaybe (Packaging.unModuleName namespace) (Lists.maybeLast (Strings.splitOn "." (Packaging.unModuleName namespace)))))
          newMappings = Maps.fromList (Lists.map (\ns_ -> (ns_, (toModuleName ns_))) (Sets.toList newNamespaces))
      in Util.ModuleNames {
        Util.moduleNamesFocus = (Util.moduleNamesFocus ns0),
        Util.moduleNamesMapping = (Maps.union (Util.moduleNamesMapping ns0) newMappings)}
-- | Build namespaces for a test group including encoded term references
buildNamespacesForTestGroup :: Packaging.Module -> Testing.TestGroup -> Graph.Graph -> Either String (Util.ModuleNames Syntax.ModuleName)
buildNamespacesForTestGroup mod tgroup graph_ =

      let testCases_ = collectTestCases tgroup
          testTerms = Lists.concat (Lists.map extractTestTerms testCases_)
          testBindings =
                  Lists.map (\term -> Core.Binding {
                    Core.bindingName = (Core.Name "_test_"),
                    Core.bindingTerm = term,
                    Core.bindingTypeScheme = Nothing}) testTerms
          tempModule =
                  Packaging.Module {
                    Packaging.moduleName = (Packaging.moduleName mod),
                    Packaging.moduleDescription = (Packaging.moduleDescription mod),
                    Packaging.moduleDependencies = (Packaging.moduleDependencies mod),
                    Packaging.moduleDefinitions = (Lists.map (\b -> Packaging.DefinitionTerm (Packaging.TermDefinition {
                      Packaging.termDefinitionName = (Core.bindingName b),
                      Packaging.termDefinitionTerm = (Core.bindingTerm b),
                      Packaging.termDefinitionSignature = (Maybes.map Scoping.typeSchemeToTermSignature (Core.bindingTypeScheme b))})) testBindings)}
      in (Eithers.bind (Eithers.bimap (\e -> ShowErrors.error e) (\a -> a) (Utils.namespacesForModule tempModule Lexical.emptyInferenceContext graph_)) (\baseNamespaces ->
        let encodedNames = Sets.unions (Lists.map (\t -> extractEncodedTermVariableNames graph_ t) testTerms)
        in (Right (addNamespacesToNamespaces baseNamespaces encodedNames))))
-- | Build the complete test module for Haskell HSpec
buildTestModule :: Packaging.Module -> Testing.TestGroup -> String -> Util.ModuleNames Syntax.ModuleName -> String
buildTestModule testModule testGroup testBody namespaces =

      let ns_ = Packaging.moduleName testModule
          specNs = Packaging.ModuleName (Strings.cat2 (Packaging.unModuleName ns_) "Spec")
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
findHaskellImports :: Util.ModuleNames Syntax.ModuleName -> t0 -> [String]
findHaskellImports namespaces names_ =

      let mapping_ = Util.moduleNamesMapping namespaces
          filtered =
                  Maps.filterWithKey (\ns_ -> \_v -> Logic.not (Equality.equal (Maybes.fromMaybe "" (Lists.maybeHead (Strings.splitOn "hydra.test." (Packaging.unModuleName ns_)))) "")) mapping_
      in (Lists.map (\entry -> Strings.cat [
        "import qualified ",
        (Strings.intercalate "." (Lists.map Formatting.capitalize (Strings.splitOn "." (Packaging.unModuleName (Pairs.first entry))))),
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
          actual_ = Testing.universalTestCaseActual universal ()
          expected_ = Testing.universalTestCaseExpected universal ()
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
generateTestFile :: Packaging.Module -> Testing.TestGroup -> Util.ModuleNames Syntax.ModuleName -> Either t0 (String, String)
generateTestFile testModule testGroup namespaces =
    Eithers.map (\testBody ->
      let testModuleContent = buildTestModule testModule testGroup testBody namespaces
          ns_ = Packaging.moduleName testModule
          specNs = Packaging.ModuleName (Strings.cat2 (Packaging.unModuleName ns_) "Spec")
          filePath = Names.moduleNameToFilePath Util.CaseConventionPascal (Packaging.FileExtension "hs") specNs
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
namespaceToModuleName :: Packaging.ModuleName -> String
namespaceToModuleName ns_ =
    Strings.intercalate "." (Lists.map Formatting.capitalize (Strings.splitOn "." (Packaging.unModuleName ns_)))
