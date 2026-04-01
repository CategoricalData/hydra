-- Note: this is an automatically generated file. Do not edit.

-- | Haskell test code generation codec for HSpec-based generation tests

module Hydra.Ext.Haskell.Testing where

import qualified Hydra.Coders as Coders
import qualified Hydra.Constants as Constants
import qualified Hydra.Context as Context
import qualified Hydra.Core as Core
import qualified Hydra.Decode.Core as Core_
import qualified Hydra.Ext.Haskell.Coder as Coder
import qualified Hydra.Ext.Haskell.Serde as Serde
import qualified Hydra.Ext.Haskell.Syntax as Syntax
import qualified Hydra.Ext.Haskell.Utils as Utils
import qualified Hydra.Formatting as Formatting
import qualified Hydra.Graph as Graph
import qualified Hydra.Inference as Inference
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
import qualified Hydra.Module as Module
import qualified Hydra.Names as Names
import qualified Hydra.Rewriting as Rewriting
import qualified Hydra.Schemas as Schemas
import qualified Hydra.Serialization as Serialization
import qualified Hydra.Show.Errors as Errors
import qualified Hydra.Substitution as Substitution
import qualified Hydra.Testing as Testing
import qualified Hydra.Typing as Typing
import qualified Hydra.Util as Util
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | Add namespaces from a set of names to existing namespaces
addNamespacesToNamespaces :: Module.Namespaces Syntax.ModuleName -> S.Set Core.Name -> Module.Namespaces Syntax.ModuleName
addNamespacesToNamespaces ns0 names =

      let newNamespaces = Sets.fromList (Maybes.cat (Lists.map Names.namespaceOf (Sets.toList names)))
          toModuleName =
                  \namespace -> Syntax.ModuleName (Formatting.capitalize (Lists.last (Strings.splitOn "." (Module.unNamespace namespace))))
          newMappings = Maps.fromList (Lists.map (\ns_ -> (ns_, (toModuleName ns_))) (Sets.toList newNamespaces))
      in Module.Namespaces {
        Module.namespacesFocus = (Module.namespacesFocus ns0),
        Module.namespacesMapping = (Maps.union (Module.namespacesMapping ns0) newMappings)}

-- | Build namespaces for a test group including encoded term references
buildNamespacesForTestGroup :: Module.Module -> Testing.TestGroup -> Graph.Graph -> Either String (Module.Namespaces Syntax.ModuleName)
buildNamespacesForTestGroup mod tgroup graph_ =

      let testCases_ = collectTestCases tgroup
          testTerms = Lists.concat (Lists.map extractTestTerms testCases_)
          testBindings =
                  Lists.map (\term -> Core.Binding {
                    Core.bindingName = (Core.Name "_test_"),
                    Core.bindingTerm = term,
                    Core.bindingType = Nothing}) testTerms
          tempModule =
                  Module.Module {
                    Module.moduleNamespace = (Module.moduleNamespace mod),
                    Module.moduleDefinitions = (Lists.map (\b -> Module.DefinitionTerm (Module.TermDefinition {
                      Module.termDefinitionName = (Core.bindingName b),
                      Module.termDefinitionTerm = (Core.bindingTerm b),
                      Module.termDefinitionType = (Core.bindingType b)})) testBindings),
                    Module.moduleTermDependencies = (Module.moduleTermDependencies mod),
                    Module.moduleTypeDependencies = (Module.moduleTypeDependencies mod),
                    Module.moduleDescription = (Module.moduleDescription mod)}
      in (Eithers.bind (Eithers.bimap (\ic -> Errors.error (Context.inContextObject ic)) (\a -> a) (Utils.namespacesForModule tempModule Lexical.emptyContext graph_)) (\baseNamespaces ->
        let encodedNames = Sets.unions (Lists.map (\t -> extractEncodedTermVariableNames graph_ t) testTerms)
        in (Right (addNamespacesToNamespaces baseNamespaces encodedNames))))

-- | Build the complete test module using a TestCodec
buildTestModuleWithCodec :: Testing.TestCodec -> Module.Module -> Testing.TestGroup -> String -> t0 -> String
buildTestModuleWithCodec codec testModule testGroup testBody namespaces =

      let ns_ = Module.moduleNamespace testModule
          specNs = Module.Namespace (Strings.cat2 (Module.unNamespace ns_) "Spec")
          moduleNameString = Testing.testCodecFormatModuleName codec specNs
          groupName_ = Testing.testGroupName testGroup
          domainImports = Testing.testCodecFindImports codec Sets.empty
          standardImports =
                  [
                    "import Hydra.Kernel",
                    "import qualified Test.Hspec as H",
                    "import qualified Data.List as L",
                    "import qualified Data.Map as M",
                    "import qualified Data.Set as S",
                    "import qualified Data.Maybe as Y"]
          allImports = Lists.concat2 standardImports domainImports
          debugComments =
                  [
                    "-- DEBUG: Focus namespace = (see generated module)",
                    "-- DEBUG: Namespace mappings: (see generated module)"]
          header =
                  Strings.intercalate "\n" (Lists.concat [
                    [
                      Strings.cat2 "-- " Constants.warningAutoGeneratedFile,
                      ""],
                    debugComments,
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
    Logic.ifElse (Schemas.isEncodedTerm (Rewriting.deannotateTerm t)) (Eithers.either (\_ -> names) (\decodedTerm -> Sets.union names (Rewriting.termDependencyNames True True True decodedTerm)) (Eithers.bimap (\_e -> _e) (\_a -> _a) (Core_.term graf t))) names

-- | Collect all test cases from a test group recursively
collectTestCases :: Testing.TestGroup -> [Testing.TestCaseWithMetadata]
collectTestCases tg =
    Lists.concat2 (Testing.testGroupCases tg) (Lists.concat (Lists.map collectTestCases (Testing.testGroupSubgroups tg)))

-- | Check if a term contains any trivially polymorphic sub-terms
containsTriviallyPolymorphic :: Core.Term -> Bool
containsTriviallyPolymorphic term =
    case term of
      Core.TermList v0 -> Logic.or (Lists.null v0) (Lists.foldl Logic.or False (Lists.map containsTriviallyPolymorphic v0))
      Core.TermSet v0 -> Logic.or (Sets.null v0) (Lists.foldl Logic.or False (Lists.map containsTriviallyPolymorphic (Sets.toList v0)))
      Core.TermMap v0 -> Logic.or (Maps.null v0) (Logic.or (Lists.foldl Logic.or False (Lists.map containsTriviallyPolymorphic (Maps.keys v0))) (Lists.foldl Logic.or False (Lists.map containsTriviallyPolymorphic (Lists.map (\p -> Pairs.second p) (Maps.toList v0)))))
      Core.TermMaybe v0 -> Maybes.maybe True containsTriviallyPolymorphic v0
      Core.TermEither _ -> True
      Core.TermUnion v0 -> containsTriviallyPolymorphic (Core.fieldTerm (Core.injectionField v0))
      Core.TermPair v0 -> Logic.or (containsTriviallyPolymorphic (Pairs.first v0)) (containsTriviallyPolymorphic (Pairs.second v0))
      Core.TermRecord v0 -> Lists.foldl Logic.or False (Lists.map (\f -> containsTriviallyPolymorphic (Core.fieldTerm f)) (Core.recordFields v0))
      Core.TermApplication v0 -> Logic.or (containsTriviallyPolymorphic (Core.applicationFunction v0)) (containsTriviallyPolymorphic (Core.applicationArgument v0))
      _ -> False

-- | Extract all variable names from term-encoded terms in a given term
extractEncodedTermVariableNames :: Graph.Graph -> Core.Term -> S.Set Core.Name
extractEncodedTermVariableNames graf term =
    Rewriting.foldOverTerm Coders.TraversalOrderPre (collectNames graf) Sets.empty term

-- | Extract input and output terms from a test case
extractTestTerms :: t0 -> [t1]
extractTestTerms tcm = []

-- | Find necessary imports for Haskell based on referenced names
findHaskellImports :: Module.Namespaces Syntax.ModuleName -> t0 -> [String]
findHaskellImports namespaces names_ =

      let mapping_ = Module.namespacesMapping namespaces
          filtered =
                  Maps.filterWithKey (\ns_ -> \_v -> Logic.not (Equality.equal (Lists.head (Strings.splitOn "hydra.test." (Module.unNamespace ns_))) "")) mapping_
      in (Lists.map (\entry -> Strings.cat [
        "import qualified ",
        (Strings.intercalate "." (Lists.map Formatting.capitalize (Strings.splitOn "." (Module.unNamespace (Pairs.first entry))))),
        " as ",
        (Syntax.unModuleName (Pairs.second entry))]) (Maps.toList filtered))

-- | Generate a Haskell test file for a test group, with type inference and namespace building
generateHaskellTestFile :: Module.Module -> Testing.TestGroup -> Graph.Graph -> Either String (String, String)
generateHaskellTestFile testModule testGroup g =
    Eithers.bind (buildNamespacesForTestGroup testModule testGroup g) (\namespaces -> generateTestFileWithCodec (haskellTestCodec namespaces) testModule testGroup namespaces g)

-- | Generate a single test case using a TestCodec
generateTestCaseWithCodec :: t0 -> t1 -> t2 -> t3 -> Testing.TestCaseWithMetadata -> Either t4 [t5]
generateTestCaseWithCodec g namespaces codec depth tcm =

      let name_ = Testing.testCaseWithMetadataName tcm
          tcase = Testing.testCaseWithMetadataCase tcm
      in (Right [])

-- | Generate a complete test file using a TestCodec
generateTestFileWithCodec :: Testing.TestCodec -> Module.Module -> Testing.TestGroup -> t0 -> t1 -> Either t2 (String, String)
generateTestFileWithCodec codec testModule testGroup namespaces g =
    Eithers.map (\testBody ->
      let testModuleContent = buildTestModuleWithCodec codec testModule testGroup testBody namespaces
          ext = Module.unFileExtension (Testing.testCodecFileExtension codec)
          ns_ = Module.moduleNamespace testModule
          specNs = Module.Namespace (Strings.cat2 (Module.unNamespace ns_) "Spec")
          filePath = Names.namespaceToFilePath Util.CaseConventionPascal (Module.FileExtension ext) specNs
      in (filePath, testModuleContent)) (generateTestGroupHierarchy g namespaces codec 1 testGroup)

-- | Generate test hierarchy preserving the structure with H.describe blocks for subgroups
generateTestGroupHierarchy :: t0 -> t1 -> t2 -> Int -> Testing.TestGroup -> Either t3 String
generateTestGroupHierarchy g namespaces codec depth testGroup =

      let cases_ = Testing.testGroupCases testGroup
          subgroups = Testing.testGroupSubgroups testGroup
          indent = Strings.fromList (Lists.replicate (Math.mul depth 2) 32)
      in (Eithers.bind (Eithers.mapList (\tc -> generateTestCaseWithCodec g namespaces codec depth tc) cases_) (\testCaseLinesRaw ->
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
            content]) (generateTestGroupHierarchy g namespaces codec (Math.add depth 1) subgroup))) subgroups)))))

-- | Generate a type annotation for polymorphic output values
generateTypeAnnotationFor :: Graph.Graph -> Module.Namespaces Syntax.ModuleName -> Core.Term -> Core.Term -> Either String (Maybe String)
generateTypeAnnotationFor g namespaces inputTerm outputTerm =
    Logic.ifElse (Logic.not (containsTriviallyPolymorphic outputTerm)) (Right Nothing) (Maybes.maybe (Right Nothing) (\result ->
      let typeScheme = Pairs.second result
          typ = Core.typeSchemeType typeScheme
          schemaVars = Sets.fromList (Maps.keys (Graph.graphSchemaTypes g))
          freeVars = Sets.toList (Sets.difference (Rewriting.freeVariablesInType typ) schemaVars)
          isEither =
                  case (Rewriting.deannotateTerm outputTerm) of
                    Core.TermEither _ -> True
                    _ -> False
      in (Logic.ifElse (Logic.or isEither (Logic.not (Lists.null freeVars))) (
        let int32Type = Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)
            subst = Typing.TypeSubst (Maps.fromList (Lists.map (\v -> (v, int32Type)) freeVars))
            groundedType = Substitution.substInType subst typ
        in (Eithers.map (\typeStr -> Just (Strings.cat2 " :: " typeStr)) (typeToHaskell namespaces groundedType g))) (Right Nothing))) (tryInferTypeOf g inputTerm))

-- | Template for Haskell import statements
haskellImportTemplate :: String
haskellImportTemplate = "import qualified {namespace} as {alias}"

-- | Template for Haskell test module structure
haskellModuleTemplate :: String
haskellModuleTemplate =
    Strings.intercalate "\n" [
      Strings.cat2 "-- " Constants.warningAutoGeneratedFile,
      "",
      "module {moduleName} where",
      "",
      "{imports}",
      "",
      "spec :: H.Spec",
      "{testGroup}",
      "{testCases}",
      ""]

-- | Template for HSpec test case assertions
haskellTestCaseTemplate :: String
haskellTestCaseTemplate =
    Strings.intercalate "\n" [
      "  H.it {name} $ H.shouldBe",
      "    ({input})",
      "    ({output})",
      ""]

-- | Create a Haskell TestCodec that uses the real Haskell coder
haskellTestCodec :: Module.Namespaces Syntax.ModuleName -> Testing.TestCodec
haskellTestCodec namespaces =
    Testing.TestCodec {
      Testing.testCodecLanguage = (Coders.LanguageName "haskell"),
      Testing.testCodecFileExtension = (Module.FileExtension "hs"),
      Testing.testCodecEncodeTerm = (termToHaskell namespaces),
      Testing.testCodecEncodeType = (typeToHaskell namespaces),
      Testing.testCodecFormatTestName = (\n -> n),
      Testing.testCodecFormatModuleName = namespaceToModuleName,
      Testing.testCodecTestCaseTemplate = haskellTestCaseTemplate,
      Testing.testCodecTestGroupTemplate = haskellTestGroupTemplate,
      Testing.testCodecModuleTemplate = haskellModuleTemplate,
      Testing.testCodecImportTemplate = haskellImportTemplate,
      Testing.testCodecFindImports = (findHaskellImports namespaces)}

-- | Template for HSpec test group description
haskellTestGroupTemplate :: String
haskellTestGroupTemplate = "spec = H.describe {groupName} $ do"

-- | Indent continuation lines of a multi-line string
indentContinuationLines :: Int -> String -> String
indentContinuationLines n s =
    Strings.intercalate (Strings.cat2 "\n" (Strings.fromList (Lists.replicate n 32))) (Strings.splitOn "\n" s)

-- | Convert namespace to Haskell module name
namespaceToModuleName :: Module.Namespace -> String
namespaceToModuleName ns_ =
    Strings.intercalate "." (Lists.map Formatting.capitalize (Strings.splitOn "." (Module.unNamespace ns_)))

-- | Convert a Hydra term to a Haskell expression string
termToHaskell :: Module.Namespaces Syntax.ModuleName -> Core.Term -> Graph.Graph -> Either String String
termToHaskell namespaces term g =
    Eithers.bimap (\ic -> Errors.error (Context.inContextObject ic)) (\arg_ -> (\arg_ -> Serialization.printExpr (Serialization.parenthesize arg_)) (Serde.expressionToExpr arg_)) (Coder.encodeTerm 0 namespaces term Lexical.emptyContext g)

-- | Try to infer the type of a term, returning Nothing if inference fails
tryInferTypeOf :: Graph.Graph -> Core.Term -> Maybe (Core.Term, Core.TypeScheme)
tryInferTypeOf g term =
    Eithers.either (\_ -> Nothing) (\result -> Just (Pairs.first result)) (Inference.inferTypeOf Lexical.emptyContext g term)

-- | Convert a Hydra type to a Haskell type expression string
typeToHaskell :: Module.Namespaces Syntax.ModuleName -> Core.Type -> t0 -> Either String String
typeToHaskell namespaces typ g =
    Eithers.bimap (\ic -> Errors.error (Context.inContextObject ic)) (\arg_ -> (\arg_ -> Serialization.printExpr (Serialization.parenthesize arg_)) (Serde.typeToExpr arg_)) (Coder.encodeType namespaces typ Lexical.emptyContext g)
