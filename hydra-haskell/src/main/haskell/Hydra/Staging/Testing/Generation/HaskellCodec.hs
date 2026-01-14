-- | Haskell-specific test code generation using HSpec

module Hydra.Staging.Testing.Generation.HaskellCodec where

import Hydra.Kernel hiding (map)
import Hydra.Testing
import Hydra.Coders (LanguageName(..))
import Hydra.Staging.Testing.Generation.Transform (collectTestCases, addGenerationPrefix)
import Hydra.Staging.Testing.Generation.Generate (TestGenerator(..), createTestGroupLookup, generateGenerationTestSuite)
import qualified Hydra.Ext.Haskell.Coder as HaskellCoder
import Hydra.Ext.Haskell.Utils (namespacesForModule, sanitizeHaskellName)
import qualified Hydra.Ext.Haskell.Serde as HaskellSerde
import Hydra.Serialization (printExpr, parenthesize)
import qualified Hydra.Ext.Haskell.Ast as H
import qualified Hydra.Names as Names
import qualified Hydra.Util as Util
import qualified Hydra.Show.Core as ShowCore
import qualified Hydra.Formatting as Formatting
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Lib.Lists as Lists

import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.Set as S
import qualified Hydra.Rewriting as Rewriting
import qualified Hydra.Inference as Inference
import qualified Hydra.Substitution as Substitution
import qualified Hydra.Typing as Typing
import qualified Hydra.Decode.Core as DecodeCore
import Data.Char (toUpper)
import qualified System.FilePath as FP


-- | Extract all variable names from term-encoded terms in a given term.
-- This recursively decodes encoded terms and extracts the variable names.
extractEncodedTermVariableNames :: Graph -> Term -> S.Set Name
extractEncodedTermVariableNames graph term =
  Rewriting.foldOverTerm TraversalOrderPre collectNames S.empty term
  where
    collectNames :: S.Set Name -> Term -> S.Set Name
    collectNames names t
      | isEncodedTerm (Rewriting.deannotateTerm t) =
          case DecodeCore.term graph t of
            Right decodedTerm ->
              -- Recursively extract variable names from the decoded term
              S.union names (Rewriting.termDependencyNames True True True decodedTerm)
            Left _ -> names
      | otherwise = names

-- | Add namespaces from a set of names to existing namespaces
addNamespacesToNamespaces :: Namespaces H.ModuleName -> S.Set Name -> Namespaces H.ModuleName
addNamespacesToNamespaces ns0 names =
  let newNamespaces = S.fromList $ mapMaybe Names.namespaceOf $ S.toList names
      toModuleName namespace =
        let parts = Strings.splitOn "." (unNamespace namespace)
            lastPart = L.last parts
        in H.ModuleName (Formatting.capitalize lastPart)
      toPair ns = (ns, toModuleName ns)
      newMappings = M.fromList $ map toPair $ S.toList newNamespaces
  in ns0 { namespacesMapping = M.union (namespacesMapping ns0) newMappings }
  where
    mapMaybe f = foldr (\x acc -> maybe acc (:acc) (f x)) []

-- | Check if a term is a FlowState record.
-- Returns Just (value, state, trace) if it is a FlowState record, Nothing otherwise.
isFlowStateRecord :: Term -> Maybe (Term, Term, Term)
isFlowStateRecord term = case Rewriting.deannotateTerm term of
  TermRecord rec | unName (recordTypeName rec) == "hydra.compute.FlowState" ->
    let fields = recordFields rec
        getValue = findField "value" fields
        getState = findField "state" fields
        getTrace = findField "trace" fields
    in case (getValue, getState, getTrace) of
         (Just v, Just s, Just t) -> Just (v, s, t)
         _ -> Nothing
  _ -> Nothing
  where
    findField :: String -> [Field] -> Maybe Term
    findField name = L.foldr (\f acc -> if unName (fieldName f) == name then Just (fieldTerm f) else acc) Nothing

-- | Preprocess input term to replace TermUnit state with emptyGraph in Flow unrolling patterns.
-- The kernel test generation converts the Flow state argument (e.g. emptyGraph) to TermUnit
-- due to type erasure, but some functions (like annotation functions) specifically require Graph state.
preprocessFlowInput :: Term -> Term
preprocessFlowInput = Rewriting.rewriteTerm rewrite
  where
    emptyGraphVar = TermVariable (Name "hydra.lexical.emptyGraph")

    rewrite :: (Term -> Term) -> Term -> Term
    rewrite recurse t = case Rewriting.deannotateTerm t of
      -- Pattern: (((unFlow flowExpr) state) trace)
      -- Structure: Application { function = Application { function = Application { function = Eliminator, arg = flowExpr }, arg = state }, arg = trace }
      -- We want to replace state=TermUnit with emptyGraph
      TermApplication app ->
        let func = applicationFunction app     -- Application { function = Application { ... }, arg = state }
            traceArg = applicationArgument app -- trace
        in case Rewriting.deannotateTerm func of
          TermApplication stateApp ->
            let unFlowApp = applicationFunction stateApp  -- Application { function = Eliminator, arg = flowExpr }
                stateArg = applicationArgument stateApp   -- state (might be TermUnit)
            in case Rewriting.deannotateTerm unFlowApp of
              TermApplication elimApp ->
                let elimFunc = applicationFunction elimApp  -- Eliminator
                in case Rewriting.deannotateTerm elimFunc of
                  TermFunction (FunctionElimination (EliminationWrap wrapName))
                    | unName wrapName == "hydra.compute.Flow" ->
                      -- Found the pattern! Check if stateArg is TermUnit
                      let newState = case Rewriting.deannotateTerm stateArg of
                            TermUnit -> emptyGraphVar
                            _ -> recurse stateArg
                          -- Rebuild: ((unFlowApp newState) trace)
                          newStateApp = TermApplication $ Application (recurse unFlowApp) newState
                      in TermApplication $ Application newStateApp (recurse traceArg)
                  _ -> recurse t
              _ -> recurse t
          _ -> recurse t
      _ -> recurse t

termToHaskell :: Namespaces H.ModuleName -> Term -> Flow Graph String
termToHaskell namespaces term = do
  result <- HaskellCoder.encodeTerm namespaces term
  return $ printExpr . parenthesize . HaskellSerde.expressionToExpr $ result

typeToHaskell :: Namespaces H.ModuleName -> Type -> Flow Graph String
typeToHaskell namespaces typ = (printExpr . parenthesize . HaskellSerde.typeToExpr) <$>
  HaskellCoder.encodeType namespaces typ

-- | Create a Haskell TestCodec that uses the real Haskell coder
haskellTestCodec :: Namespaces H.ModuleName -> TestCodec
haskellTestCodec namespaces = TestCodec {
    testCodecLanguage = LanguageName "haskell",
    testCodecFileExtension = FileExtension "hs",
    testCodecEncodeTerm = termToHaskell namespaces,
    testCodecEncodeType = typeToHaskell namespaces,
    testCodecFormatTestName = id,  -- Keep test names as-is
    testCodecFormatModuleName = namespaceToModuleName,
    testCodecTestCaseTemplate = haskellTestCaseTemplate,
    testCodecTestGroupTemplate = haskellTestGroupTemplate,
    testCodecModuleTemplate = haskellModuleTemplate,
    testCodecImportTemplate = haskellImportTemplate,
    testCodecFindImports = findHaskellImports namespaces}

-- Templates for Haskell test generation
haskellTestCaseTemplate :: String
haskellTestCaseTemplate = unlines [
  "  H.it {name} $ H.shouldBe",
  "    ({input})",
  "    ({output})"]

haskellTestGroupTemplate :: String
haskellTestGroupTemplate = "spec = H.describe {groupName} $ do"

haskellModuleTemplate :: String
haskellModuleTemplate = unlines [
  "-- " ++ warningAutoGeneratedFile,
  "",
  "module {moduleName} where",
  "",
  "{imports}",
  "",
  "spec :: H.Spec",
  "{testGroup}",
  "{testCases}"]

haskellImportTemplate :: String
haskellImportTemplate = "import qualified {namespace} as {alias}"

-- | Find necessary imports for Haskell based on referenced names
findHaskellImports :: Namespaces H.ModuleName -> S.Set Name -> [String]
findHaskellImports namespaces names = L.map makeImport (M.toList filteredMapping)
  where
    -- Filter out test module namespaces (hydra.test.*) which aren't in main library
    isTestNamespace (Namespace ns) = "hydra.test." `L.isPrefixOf` ns
    filteredMapping = M.filterWithKey (\ns _ -> not (isTestNamespace ns)) (namespacesMapping namespaces)
    makeImport (ns, alias) =
      "import qualified " ++ nsToModuleName ns ++ " as " ++ H.unModuleName alias
    nsToModuleName (Namespace ns) =
      Strings.intercalate "." $ Lists.map Formatting.capitalize (Strings.splitOn "." ns)

-- | Generate test hierarchy preserving the structure with H.describe blocks for subgroups
generateTestGroupHierarchy :: InferenceContext -> Namespaces H.ModuleName -> TestCodec -> Int -> TestGroup -> Flow Graph String
generateTestGroupHierarchy infContext namespaces codec depth testGroup = do
  -- Generate test cases at the current level with proper indentation
  testCaseLinesRaw <- mapM (generateTestCaseWithCodec infContext namespaces codec depth) (testGroupCases testGroup)
  let indent = replicate (depth * 2) ' '
      indentTestCase = L.map (indent ++)
      testCaseLines = fmap indentTestCase testCaseLinesRaw
      testCasesStr = L.intercalate "\n" (concat testCaseLines)

  -- Generate H.describe blocks for each subgroup
  subgroupStrs <- mapM generateSubgroupBlock (testGroupSubgroups testGroup)
  let subgroupsStr = L.intercalate "\n" subgroupStrs

  -- Combine test cases and subgroups
  return $ testCasesStr ++ (if null testCasesStr || null subgroupsStr then "" else "\n") ++ subgroupsStr
  where
    generateSubgroupBlock :: TestGroup -> Flow Graph String
    generateSubgroupBlock subgroup = do
      let indent = replicate (depth * 2) ' '
      -- Recursively generate content for this subgroup at depth+1 (nested inside this H.describe)
      subgroupContent <- generateTestGroupHierarchy infContext namespaces codec (depth + 1) subgroup
      let groupName = testGroupName subgroup
      -- Generate the H.describe block with proper indentation
      return $ indent ++ "H.describe " ++ show groupName ++ " $ do\n" ++ subgroupContent

-- | Generic test file generation using a TestCodec
generateTestFileWithCodec :: TestCodec -> Module -> TestGroup -> Namespaces H.ModuleName -> Flow Graph (FilePath, String)
generateTestFileWithCodec codec testModule testGroup namespaces = do
  -- Note: Type inference is now performed ONCE upfront in generateAllModuleTests (Generate.hs)
  -- This is critical for performance: inferGraphTypes is expensive and should not be called per-module
  g <- getState
  infContext <- graphToInferenceContext g

  -- Generate test hierarchy preserving the structure
  testBody <- generateTestGroupHierarchy infContext namespaces codec 1 testGroup

  -- Build the complete test module
  let testModuleContent = buildTestModuleWithCodec codec testModule testGroup testBody namespaces

  -- Use the codec's file extension for the path
  -- Append "Spec" to the namespace for hspec-discover compatibility
  let FileExtension ext = testCodecFileExtension codec
  let Namespace ns = moduleNamespace testModule
  let specNs = Namespace (ns ++ "Spec")
  let filePath = Names.namespaceToFilePath Util.CaseConventionPascal
                   (FileExtension ext)
                   specNs

  return (filePath, testModuleContent)

-- | Generate a single test case using a TestCodec
generateTestCaseWithCodec :: InferenceContext -> Namespaces H.ModuleName -> TestCodec -> Int -> TestCaseWithMetadata -> Flow Graph [String]
generateTestCaseWithCodec infContext namespaces codec depth (TestCaseWithMetadata name tcase _ _) = case tcase of
  TestCaseDelegatedEvaluation (DelegatedEvaluationTestCase inputRaw output) -> do
    -- Preprocess input to replace TermUnit state with emptyGraph in Flow patterns
    let input = preprocessFlowInput inputRaw

    -- Check if output is a FlowState record - if so, we compare only the flowStateValue
    -- because FlowState Graph a cannot be compared with shouldBe (Graph has no Show instance)
    case isFlowStateRecord output of
      Just (expectedValue, _, _) -> do
        -- Generate test that extracts flowStateValue from the result and compares it
        inputCode <- testCodecEncodeTerm codec input
        expectedValueCode <- testCodecEncodeTerm codec expectedValue

        let formattedName = testCodecFormatTestName codec name
            indentLines n s = L.intercalate ("\n" ++ replicate n ' ') (L.lines s)
            continuationIndent = depth * 2 + 4
            indentedInputCode = indentLines continuationIndent inputCode
            indentedExpectedCode = indentLines continuationIndent expectedValueCode

        -- Check if we need a type annotation for the expected value
        -- Use expectedValue (the unwrapped flowStateValue) for inference, not full FlowState output
        typeAnnotation <- generateTypeAnnotationForFlowStateValue infContext namespaces input expectedValue
        let finalExpectedCode = case typeAnnotation of
              Just anno -> indentedExpectedCode ++ anno
              Nothing -> indentedExpectedCode

        return [
          "H.it " ++ show formattedName ++ " $ H.shouldBe",
          "  (Compute.flowStateValue (" ++ indentedInputCode ++ "))",
          "  (" ++ finalExpectedCode ++ ")"]

      Nothing -> do
        -- Standard comparison - output is not a FlowState
        inputCode <- testCodecEncodeTerm codec input
        outputCode <- testCodecEncodeTerm codec output

        let formattedName = testCodecFormatTestName codec name
            indentLines n s = L.intercalate ("\n" ++ replicate n ' ') (L.lines s)
            continuationIndent = depth * 2 + 4
            indentedInputCode = indentLines continuationIndent inputCode
            indentedOutputCode = indentLines continuationIndent outputCode

        typeAnnotation <- generateTypeAnnotationFor infContext namespaces input output
        let (finalInputCode, finalOutputCode) = case typeAnnotation of
              Just anno -> (indentedInputCode, indentedOutputCode ++ anno)
              Nothing -> (indentedInputCode, indentedOutputCode)

        return [
          "H.it " ++ show formattedName ++ " $ H.shouldBe",
          "  (" ++ finalInputCode ++ ")",
          "  (" ++ finalOutputCode ++ ")"]

  _ -> return []  -- Skip non-delegated tests (shouldn't happen after transform)

-- | Generate a type annotation for FlowState value extraction.
-- Adds annotation when:
-- 1. Bare `Nothing` (always ambiguous) -> `Maybe Int`
-- 2. `Just Nothing` when INPUT also contains bare `Nothing` (polymorphic) -> `Maybe (Maybe Int)`
-- For (2), if input is concrete (like getTermDescription), GHC infers from input.
generateTypeAnnotationForFlowStateValue :: InferenceContext -> Namespaces H.ModuleName -> Term -> Term -> Flow Graph (Maybe String)
generateTypeAnnotationForFlowStateValue _ _ inputTerm expectedValue =
  case Rewriting.deannotateTerm expectedValue of
    TermMaybe Nothing -> return $ Just " :: Maybe Int"
    TermMaybe (Just inner) ->
      case Rewriting.deannotateTerm inner of
        TermMaybe Nothing ->
          -- Check if INPUT is polymorphic (contains bare Nothing)
          if inputContainsNothing inputTerm
            then return $ Just " :: Maybe (Maybe Int)"
            else return Nothing  -- Input is concrete, GHC can infer
        _ -> return Nothing
    _ -> return Nothing
  where
    -- Check if input term contains bare Nothing (indicates polymorphism)
    inputContainsNothing :: Term -> Bool
    inputContainsNothing term = case Rewriting.deannotateTerm term of
      TermMaybe Nothing -> True
      TermMaybe (Just inner) -> inputContainsNothing inner
      TermApplication app ->
        inputContainsNothing (applicationFunction app) ||
        inputContainsNothing (applicationArgument app)
      TermFunction (FunctionLambda lam) -> inputContainsNothing (lambdaBody lam)
      TermRecord rec -> any (inputContainsNothing . fieldTerm) (recordFields rec)
      TermList xs -> any inputContainsNothing xs
      _ -> False

-- | Extract the value type from a FlowState type.
-- FlowState s a has flowStateValue :: Maybe a, so we extract the `Maybe a` type.
-- The type structure is: TypeApplication (ApplicationType (TypeApplication (ApplicationType (TypeRecord "FlowState") s)) a)
extractFlowStateValueType :: Type -> Flow Graph (Maybe Type)
extractFlowStateValueType typ = case Rewriting.deannotateType typ of
  -- FlowState s a -> extract Maybe a
  -- Structure: TypeApplication (TypeApplication FlowState s) a
  TypeApplication (ApplicationType (TypeApplication (ApplicationType base sType)) aType)
    | isFlowStateType base -> return $ Just $ TypeMaybe aType
  -- Check for TypeRecord with FlowState name
  TypeRecord rowType | unName (rowTypeTypeName rowType) == "hydra.compute.FlowState" ->
    -- FlowState as a record - extract the value field type
    let valueField = L.find (\f -> unName (fieldTypeName f) == "value") (rowTypeFields rowType)
    in case valueField of
         Just ft -> return $ Just $ fieldTypeType ft  -- Already Maybe a
         Nothing -> return Nothing
  -- If it's wrapped in Maybe already (shouldn't happen), just return it
  TypeMaybe inner -> return $ Just typ
  -- Otherwise, return Nothing to indicate we couldn't extract the type
  _ -> return Nothing
  where
    isFlowStateType t = case Rewriting.deannotateType t of
      TypeWrap wt -> unName (wrappedTypeTypeName wt) == "hydra.compute.FlowState"
      TypeRecord rt -> unName (rowTypeTypeName rt) == "hydra.compute.FlowState"
      _ -> False

-- | Generate a type annotation for polymorphic output values
-- Adds annotations when BOTH:
-- 1. The inferred type has free type variables that GHC cannot resolve
-- 2. The output has no concrete values to guide type inference (empty list, etc.)
--
-- The annotation is derived from the INPUT term's inferred type, since the input
-- is the expression being evaluated and its result type should match the output type.
-- Free type variables are replaced with Int32.
generateTypeAnnotationFor :: InferenceContext -> Namespaces H.ModuleName -> Term -> Term -> Flow Graph (Maybe String)
generateTypeAnnotationFor infContext namespaces inputTerm outputTerm = do
  -- Only consider annotation if output has no concrete values to guide inference
  if not needsAnnotation
    then return Nothing
    else do
      -- Infer the type of the input expression (which gives us the result/output type)
      -- Use tryInferTypeOf to gracefully handle inference failures (e.g., when schema types
      -- like Graph conflict with polymorphic type variables)
      mresult <- tryInferTypeOf infContext inputTerm
      case mresult of
        Nothing -> return Nothing  -- Inference failed; skip annotation
        Just (_, typeScheme) -> do
          let typ = typeSchemeType typeScheme
              -- Check if there are any free type variables that need grounding
              freeVars = S.toList $ S.difference
                (Rewriting.freeVariablesInType typ)
                schemaVars
          -- Either types ALWAYS need annotations (one branch is always unconstrained),
          -- while other polymorphic types only need annotations if they have free variables.
          if isEitherTerm outputTerm || not (null freeVars)
            then do
              -- Replace free type variables with Int32
              let int32Type = TypeLiteral (LiteralTypeInteger IntegerTypeInt32)
                  subst = Typing.TypeSubst $ M.fromList [(v, int32Type) | v <- freeVars]
                  groundedType = Substitution.substInType subst typ
              -- Encode the type as Haskell
              typeStr <- typeToHaskell namespaces groundedType
              return $ Just (" :: " ++ typeStr)
            else return Nothing
  where
    schemaVars = S.fromList $ M.keys $ inferenceContextSchemaTypes infContext
    needsAnnotation = containsTriviallyPolymorphic outputTerm
    isEitherTerm (TermEither _) = True
    isEitherTerm _ = False

-- | Try to infer the type of a term, returning Nothing if inference fails
-- This allows graceful degradation when type inference encounters issues
-- (e.g., schema types being unified with polymorphic type variables)
tryInferTypeOf :: InferenceContext -> Term -> Flow Graph (Maybe (Term, TypeScheme))
tryInferTypeOf infContext term = Flow $ \s t ->
  let FlowState mval s' t' = unFlow (Inference.inferTypeOf infContext term) s t
  in FlowState (Just mval) s' t'

-- | Check if a term CONTAINS any trivially polymorphic sub-terms (empty list, Nothing, etc.)
-- This recursively searches through the term structure to find any parts that would
-- cause GHC to need type annotations.
containsTriviallyPolymorphic :: Term -> Bool
containsTriviallyPolymorphic term = case term of
  TermList [] -> True  -- Empty list
  TermList xs -> any containsTriviallyPolymorphic xs  -- Check list elements
  TermSet s -> S.null s || any containsTriviallyPolymorphic (S.toList s)  -- Empty set or elements
  TermMap m -> M.null m || any containsTriviallyPolymorphic (M.keys m) || any containsTriviallyPolymorphic (M.elems m)
  TermMaybe Nothing -> True  -- Nothing value
  TermMaybe (Just x) -> containsTriviallyPolymorphic x  -- Check content
  -- Either values ALWAYS need type annotations because one branch is unconstrained.
  -- Even `Right 5` needs an annotation because the Left type is ambiguous.
  TermEither _ -> True
  TermUnion inj -> containsTriviallyPolymorphic (fieldTerm $ injectionField inj)
  TermPair (a, b) -> containsTriviallyPolymorphic a || containsTriviallyPolymorphic b
  TermRecord fields -> any (containsTriviallyPolymorphic . fieldTerm) (recordFields fields)
  TermApplication app -> containsTriviallyPolymorphic (applicationFunction app) ||
                         containsTriviallyPolymorphic (applicationArgument app)
  _ -> False

-- | Build the complete test module using a TestCodec
buildTestModuleWithCodec :: TestCodec -> Module -> TestGroup -> String -> Namespaces H.ModuleName -> String
buildTestModuleWithCodec codec testModule testGroup testBody namespaces = header ++ testBody ++ "\n"
  where
    -- Append "Spec" to module name for hspec-discover compatibility
    Namespace ns = moduleNamespace testModule
    specNs = Namespace (ns ++ "Spec")
    moduleNameString = testCodecFormatModuleName codec specNs
    groupName = testGroupName testGroup

    -- Use the codec's findImports to determine necessary imports
    -- For now, we'll pass an empty set since we're not tracking names yet
    -- TODO: collect names from test cases
    domainImports = testCodecFindImports codec S.empty

    -- Standard imports that are always needed for Haskell
    standardImports = [
      "import Hydra.Kernel",
      "import qualified Test.Hspec as H",
      "import qualified Data.List as L",
      "import qualified Data.Map as M",
      "import qualified Data.Set as S",
      "import qualified Data.Maybe as Y"]

    allImports = standardImports ++ domainImports

    -- Debug comments showing namespace configuration
    debugComments = [
        "-- DEBUG: Focus namespace = " ++ show (namespacesFocus namespaces),
        "-- DEBUG: Namespace mappings:",
        "-- " ++ show (M.toList $ namespacesMapping namespaces)
      ]

    header = unlines ([
        "-- " ++ warningAutoGeneratedFile,
        ""
      ] ++ debugComments ++ [
        "",
        "module " ++ moduleNameString ++ " where",
        ""
      ] ++ allImports ++ [
        "",
        "spec :: H.Spec",
        "spec = H.describe " ++ show groupName ++ " $ do"
      ])

-- | Convert namespace to Haskell module name
-- Uses the same logic as the Haskell coder's importName function
namespaceToModuleName :: Namespace -> String
namespaceToModuleName (Namespace ns) =
  Strings.intercalate "." $ Lists.map Formatting.capitalize (Strings.splitOn "." ns)

-- | Generate generation test file for a test group using the Haskell codec
generateHaskellTestFile :: Module -> TestGroup -> Flow Graph (FilePath, String)
generateHaskellTestFile testModule testGroup = do
  -- Build proper namespaces that include all primitives referenced in test terms
  namespaces <- buildNamespacesForTestGroup testModule testGroup

  -- Generate test file using the codec
  generateTestFileWithCodec (haskellTestCodec namespaces) testModule testGroup namespaces
  where
    buildNamespacesForTestGroup mod tgroup = do
      let testCases = collectTestCases tgroup
          testTerms = concatMap extractTestTerms testCases
          testBindings = zipWith (\i term -> Binding (Name $ "_test_" ++ show i) term Nothing) ([0..] :: [Integer]) testTerms
          tempModule = mod { moduleElements = testBindings }
      -- Get initial namespaces from the module
      baseNamespaces <- namespacesForModule tempModule
      -- Extract additional namespaces from term-encoded variable references
      graph <- getState
      let encodedNames = S.unions $ map (extractEncodedTermVariableNames graph) testTerms
      -- Add the encoded term namespaces to the base namespaces
      -- Also add hydra.lexical explicitly since it's needed for Flow tests (emptyGraph)
      let extraNamespaces = S.fromList [Name "hydra.lexical.emptyGraph"]
      return $ addNamespacesToNamespaces baseNamespaces (S.union encodedNames extraNamespaces)
    extractTestTerms (TestCaseWithMetadata _ tcase _ _) = case tcase of
      TestCaseDelegatedEvaluation (DelegatedEvaluationTestCase input output) -> [input, output]
      _ -> []

-- | Haskell-specific test generator
-- Provides the complete Haskell implementation of the TestGenerator abstraction
haskellTestGenerator :: TestGenerator H.ModuleName
haskellTestGenerator = TestGenerator {
  testGenNamespacesForModule = namespacesForModule,
  testGenCreateCodec = haskellTestCodec,
  testGenGenerateTestFile = generateHaskellTestFile,
  testGenAggregatorFile = Just generateHaskellAggregatorSpec
}

-- | Generate an aggregator spec file that imports all generated test modules (Haskell/HSpec style)
generateHaskellAggregatorSpec :: FilePath -> [Module] -> (FilePath, String)
generateHaskellAggregatorSpec baseDir modules =
  let addSpecSuffix (Namespace ns) = Namespace (ns ++ "Spec")
      modulePaths = map (namespaceToModuleName . addSpecSuffix . addGenerationPrefix . moduleNamespace) modules
      imports = L.intercalate "\n" $ map (\m -> "import qualified " ++ m ++ " as " ++ sanitizeModuleName m) modulePaths
      specs = L.intercalate "\n    " $ map (\m -> sanitizeModuleName m ++ ".spec") modulePaths
      content = unlines [
        "-- Note: this is an automatically generated file. Do not edit.",
        "",
        "module Generation.Spec (spec) where",
        "",
        "import qualified Test.Hspec as H",
        imports,
        "",
        "spec :: H.Spec",
        "spec = do",
        "    " ++ specs
        ]
      filePath = FP.combine baseDir "Generation/Spec.hs"
  in (filePath, content)
  where
    sanitizeModuleName = map (\c -> if c == '.' then '_' else c)
    namespaceToModuleName (Namespace ns) =
      L.intercalate "." $ L.map capitalize (L.filter (not . null) $ Strings.splitOn "." ns)
    capitalize [] = []
    capitalize (x:xs) = toUpper x : xs
