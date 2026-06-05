{-# LANGUAGE FlexibleContexts #-}

-- | Meta-DSL for constructing test-related terms
-- TODO: merge with Hydra.Dsl.Tests

module Hydra.Dsl.Meta.Testing (
  module Hydra.Dsl.Testing,
  module Hydra.Dsl.Meta.Testing,
) where

import Hydra.Dsl.Testing hiding (
  tag, testCaseUniversal, testCaseWithMetadata,
  testCaseWithMetadataCase, testCaseWithMetadataDescription,
  testCaseWithMetadataName, testCaseWithMetadataTags, testGroup,
  universalTestCase, unTag)
import Hydra.Kernel
import Hydra.Error.Core (InvalidTermError)
import Hydra.Error.Packaging (InvalidModuleError, InvalidPackageError)
import Hydra.Testing as Testing
import Hydra.Dsl.Meta.Phantoms as Phantoms hiding ((++))
import qualified Hydra.Dsl.Meta.Lib.Eithers as Eithers
import qualified Hydra.Dsl.Meta.Lib.Lists as Lists
import qualified Hydra.Dsl.Meta.Lib.Maybes as Maybes
import qualified Hydra.Dsl.Meta.Lib.Pairs as Pairs
import qualified Hydra.Dsl.Meta.Lib.Strings as Strings
import qualified Hydra.Dsl.Meta.Terms as MetaTerms
import qualified Hydra.Dsl.Meta.Types as T
import qualified Hydra.Dsl.Validation as Validation

import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M

type Int32 = I.Int32


tag_disabled = Tag "disabled"
tag_disabledForMinimalInference = Tag "disabledForMinimalInference"

alphaConvertRef :: TypedTerm (Name -> Name -> Term -> Term)
alphaConvertRef = TypedTerm $ TermVariable $ Name "hydra.reduction.alphaConvert"

betaReduceTypeRef :: TypedTerm (InferenceContext -> Graph -> Type -> Either Error Type)
betaReduceTypeRef = TypedTerm $ TermVariable $ Name "hydra.reduction.betaReduceType"

-- | Refs for hydra.validate.packaging validators and their show functions.
checkConflictingModuleNamesRef :: TypedTerm (Package -> Maybe InvalidPackageError)
checkConflictingModuleNamesRef = TypedTerm $ TermVariable $ Name "hydra.validate.packaging.checkConflictingModuleNames"

checkConflictingVariantNamesRef :: TypedTerm (Module -> Maybe InvalidModuleError)
checkConflictingVariantNamesRef = TypedTerm $ TermVariable $ Name "hydra.validate.packaging.checkConflictingVariantNames"

checkDefinitionDocumentationRef :: TypedTerm (Module -> Maybe InvalidModuleError)
checkDefinitionDocumentationRef = TypedTerm $ TermVariable $ Name "hydra.validate.packaging.checkDefinitionDocumentation"

checkDefinitionModuleNamesRef :: TypedTerm (Module -> Maybe InvalidModuleError)
checkDefinitionModuleNamesRef = TypedTerm $ TermVariable $ Name "hydra.validate.packaging.checkDefinitionModuleNames"

checkDefinitionNameConventionRef :: TypedTerm (Module -> Maybe InvalidModuleError)
checkDefinitionNameConventionRef = TypedTerm $ TermVariable $ Name "hydra.validate.packaging.checkDefinitionNameConvention"

checkDefinitionOrderingRef :: TypedTerm (Module -> Maybe InvalidModuleError)
checkDefinitionOrderingRef = TypedTerm $ TermVariable $ Name "hydra.validate.packaging.checkDefinitionOrdering"

checkDuplicateDefinitionNamesRef :: TypedTerm (Module -> Maybe InvalidModuleError)
checkDuplicateDefinitionNamesRef = TypedTerm $ TermVariable $ Name "hydra.validate.packaging.checkDuplicateDefinitionNames"

checkDuplicateModuleNamesRef :: TypedTerm (Package -> Maybe InvalidPackageError)
checkDuplicateModuleNamesRef = TypedTerm $ TermVariable $ Name "hydra.validate.packaging.checkDuplicateModuleNames"

checkModuleNameConventionRef :: TypedTerm (Module -> Maybe InvalidModuleError)
checkModuleNameConventionRef = TypedTerm $ TermVariable $ Name "hydra.validate.packaging.checkModuleNameConvention"

checkPackageNameConventionRef :: TypedTerm (Package -> Maybe InvalidPackageError)
checkPackageNameConventionRef = TypedTerm $ TermVariable $ Name "hydra.validate.packaging.checkPackageNameConvention"

-- | Type checking test: infers the type and compares with expected.
checkTest :: String -> [Tag] -> TypedTerm Term -> TypedTerm Term -> TypedTerm Type -> TypedTerm TestCaseWithMetadata
checkTest name tags input _outputTerm outputType = testCaseWithMetadata (Phantoms.string name)
  (testCaseUniversal $ universalTestCase
    (retype $ Eithers.either_
      (Phantoms.lambda "e" (Phantoms.string "<<inference error>>"))
      (Phantoms.lambda "result"
        (showTypeRef @@ (typeSchemeToFTypeRef @@ Pairs.second (Pairs.first (Phantoms.var "result")))))
      (inferTypeOfRef @@ testContextRef @@ testGraphRef @@ input))
    (retype $ showTypeRef @@ outputType))
  nothing (Phantoms.list $ tag . unTag <$> tags)
  where
    retype :: TypedTerm x -> TypedTerm String
    retype (TypedTerm t) = TypedTerm t

evalCase :: String -> TypedTerm Term -> TypedTerm Term -> TypedTerm TestCaseWithMetadata
evalCase cname input output = evalCaseWithTags cname [] input output

-- | Create a universal test case that evaluates a Term via reduceTerm and compares the result.
evalCaseWithTags :: String -> [Tag] -> TypedTerm Term -> TypedTerm Term -> TypedTerm TestCaseWithMetadata
evalCaseWithTags cname tags input output = testCaseWithMetadata (Phantoms.string cname)
  (testCaseUniversal $ universalTestCase
    (retype $ Eithers.either_
      (Phantoms.lambda "e" (Phantoms.string "<<eval error>>"))
      (Phantoms.lambda "t" (showTermRef @@ Phantoms.var "t"))
      (reduceTermRef @@ testContextRef @@ testGraphRef @@ true @@ input))
    (retype $ showTermRef @@ output))
  nothing (Phantoms.list $ tag . unTag <$> tags)
  where
    retype :: TypedTerm x -> TypedTerm String
    retype (TypedTerm t) = TypedTerm t

-- | Create a universal test case for an expression with a show function
evalPair :: String -> TypedTerm (t -> String) -> TypedTerm t -> TypedTerm t -> TypedTerm TestCaseWithMetadata
evalPair cname showFn logicalActual logicalExpected = universalCase cname
  (retype $ showFn @@ logicalActual) (retype $ showFn @@ logicalExpected)
  where
    retype :: TypedTerm x -> TypedTerm String
    retype (TypedTerm t) = TypedTerm t

-- | evalPair with tags
evalPairWithTags :: String -> [Tag] -> TypedTerm (t -> String) -> TypedTerm t -> TypedTerm t -> TypedTerm TestCaseWithMetadata
evalPairWithTags cname tags showFn logicalActual logicalExpected = testCaseWithMetadata (Phantoms.string cname)
  (testCaseUniversal $ universalTestCase (retype $ showFn @@ logicalActual) (retype $ showFn @@ logicalExpected))
  nothing (Phantoms.list $ tag . unTag <$> tags)
  where
    retype :: TypedTerm x -> TypedTerm String
    retype (TypedTerm t) = TypedTerm t

expectFailure :: AsTerm t Term => Int -> [Tag] -> t -> TypedTerm TestCaseWithMetadata
expectFailure i tags term = infFailureTest ("#" ++ show i) tags (asTerm term)

expectMono :: AsTerm t Term => Int -> [Tag] -> t -> TypedTerm Type -> TypedTerm TestCaseWithMetadata
expectMono i tags term typ = infTest ("#" ++ show i) tags (asTerm term) $ T.mono typ

expectPoly :: AsTerm t Term => Int -> [Tag] -> t -> [String] -> TypedTerm Type -> TypedTerm TestCaseWithMetadata
expectPoly i tags term params typ = infTest ("#" ++ show i) tags (asTerm term) $ T.poly params typ

expectPolyConstrained :: AsTerm t Term => Int -> [Tag] -> t -> [String] -> [(String, [String])] -> TypedTerm Type -> TypedTerm TestCaseWithMetadata
expectPolyConstrained i tags term params constraints typ = infTest ("#" ++ show i) tags (asTerm term) $ T.polyConstrained params constraints typ

groupRef = MetaTerms.varNamePhantom . bindingName

-- | Inference failure test: expects inference to fail.
infFailureTest :: String -> [Tag] -> TypedTerm Term -> TypedTerm TestCaseWithMetadata
infFailureTest name tags term = testCaseWithMetadata (Phantoms.string name)
  (testCaseUniversal $ universalTestCase
    (retype $ Eithers.either_
      (Phantoms.lambda "e" (Phantoms.string "FAIL"))
      (Phantoms.lambda "result" (Strings.cat2 (Phantoms.string "unexpected: ")
        (showTypeSchemeRef @@ Pairs.second (Pairs.first (Phantoms.var "result")))))
      (inferTypeOfRef @@ testContextRef @@ testGraphRef @@ term))
    (Phantoms.string "FAIL"))
  nothing (Phantoms.list $ tag . unTag <$> tags)
  where
    retype :: TypedTerm x -> TypedTerm String
    retype (TypedTerm t) = TypedTerm t

-- | Inference test: infer type and compare with expected type scheme.
infTest :: String -> [Tag] -> TypedTerm Term -> TypedTerm TypeScheme -> TypedTerm TestCaseWithMetadata
infTest name tags term ts = testCaseWithMetadata (Phantoms.string name)
  (testCaseUniversal $ universalTestCase
    (retype $ Eithers.either_
      (Phantoms.lambda "e" (Strings.cat2 (Phantoms.string "INFERENCE ERROR: ") (Phantoms.string "failed")))
      (Phantoms.lambda "result"
        (showTypeSchemeRef @@ Pairs.second (Pairs.first (Phantoms.var "result"))))
      (inferTypeOfRef @@ testContextRef @@ testGraphRef @@ term))
    (retype $ showTypeSchemeRef @@ ts))
  nothing (Phantoms.list $ tag . unTag <$> tags)
  where
    retype :: TypedTerm x -> TypedTerm String
    retype (TypedTerm t) = TypedTerm t

isDisabled tcase = tag_disabled `L.elem` Testing.testCaseWithMetadataTags tcase
isDisabledForMinimalInference tcase = tag_disabledForMinimalInference `L.elem` Testing.testCaseWithMetadataTags tcase

inferTypeOfRef :: TypedTerm (InferenceContext -> Graph -> Term -> Either Error ((Term, TypeScheme), InferenceContext))
inferTypeOfRef = TypedTerm $ TermVariable $ Name "hydra.inference.inferTypeOf"

-- | Reference to the kernel-default core validation profile (term + type
-- rules; 'singleVariantUnion' classified as a warning, everything else
-- as an error; maxErrors=1).
kernelDefaultCoreProfileRef :: TypedTerm ValidationProfile
kernelDefaultCoreProfileRef = TypedTerm $ TermVariable $ Name "hydra.validate.core.kernelDefaultCoreProfile"

-- | Reference to the kernel-default packaging validation profile (every
-- per-module and per-package check classified as an error; maxErrors=1).
kernelDefaultPackagingProfileRef :: TypedTerm ValidationProfile
kernelDefaultPackagingProfileRef = TypedTerm $ TermVariable $ Name "hydra.validate.packaging.kernelDefaultPackagingProfile"

-- | Reference to the kernel-strict 'kernelModule' convenience wrapper
-- (Maybe-returning, applies 'kernelDefaultPackagingProfile' internally).
-- Used by the kernelModule orchestrator tests in
-- 'Sources/Test/Validate/Packaging.hs'.
kernelModuleRef :: TypedTerm (Module -> Maybe InvalidModuleError)
kernelModuleRef = TypedTerm $ TermVariable $ Name "hydra.validate.packaging.kernelModule"

-- | Reference to the kernel-strict 'kernelPackage' convenience wrapper.
kernelPackageRef :: TypedTerm (Package -> Maybe InvalidPackageError)
kernelPackageRef = TypedTerm $ TermVariable $ Name "hydra.validate.packaging.kernelPackage"

mapTerm :: [(TypedTerm Term, TypedTerm Term)] -> TypedTerm Term
mapTerm pairs = TypedTerm $ TermInject $ Injection _Term $ Field _Term_map $ TermMap $ M.fromList [(unTypedTerm k, unTypedTerm v) | (k, v) <- pairs]

mapTermEmpty :: TypedTerm (M.Map k v)
mapTermEmpty = TypedTerm $ TermMap M.empty

-- | Type checking test where term doesn't change (just check the inferred type)
noChange :: String -> TypedTerm Term -> TypedTerm Type -> TypedTerm TestCaseWithMetadata
noChange name term typ = checkTest name [] term term typ

noTags :: TypedTerm [Tag]
noTags = Phantoms.list ([] :: [TypedTerm Tag])

primCase :: String -> Name -> [TypedTerm Term] -> TypedTerm Term -> TypedTerm TestCaseWithMetadata
primCase cname primName args output = primCaseWithTags cname [] primName args output

primCaseWithTags :: String -> [Tag] -> Name -> [TypedTerm Term] -> TypedTerm Term -> TypedTerm TestCaseWithMetadata
primCaseWithTags cname tags primName args output = evalCaseWithTags cname tags input output
  where
    input = L.foldl (MetaTerms.@@) (MetaTerms.primitive primName) args

reduceTermRef :: TypedTerm (InferenceContext -> Graph -> Bool -> Term -> Either Error Term)
reduceTermRef = TypedTerm $ TermVariable $ Name "hydra.reduction.reduceTerm"

removeTypesFromTermRef :: TypedTerm (Term -> Term)
removeTypesFromTermRef = TypedTerm $ TermVariable $ Name "hydra.strip.removeTypesFromTerm"

showInvalidModuleErrorRef :: TypedTerm (InvalidModuleError -> String)
showInvalidModuleErrorRef = TypedTerm $ TermVariable $ Name "hydra.show.error.packaging.invalidModuleError"

showInvalidPackageErrorRef :: TypedTerm (InvalidPackageError -> String)
showInvalidPackageErrorRef = TypedTerm $ TermVariable $ Name "hydra.show.error.packaging.invalidPackageError"

showInvalidTermErrorRef :: TypedTerm (InvalidTermError -> String)
showInvalidTermErrorRef = TypedTerm $ TermVariable $ Name "hydra.show.error.core.invalidTermError"

showTermRef :: TypedTerm (Term -> String)
showTermRef = TypedTerm $ TermVariable $ Name "hydra.show.core.term"

showTypeRef :: TypedTerm (Type -> String)
showTypeRef = TypedTerm $ TermVariable $ Name "hydra.show.core.type"

showTypeSchemeRef :: TypedTerm (TypeScheme -> String)
showTypeSchemeRef = TypedTerm $ TermVariable $ Name "hydra.show.core.typeScheme"

-- | evalPair for String-typed expressions (identity show)
stringEvalPair :: String -> TypedTerm String -> TypedTerm String -> TypedTerm TestCaseWithMetadata
stringEvalPair cname = evalPair cname (Phantoms.lambda "s" (Phantoms.var "s"))

subgroup :: AsTerm t TestCaseWithMetadata => String -> [t] -> TypedTerm TestGroup
subgroup name cases = tgroup name Nothing [] (asTerm <$> cases)

supergroup :: AsTerm t TestGroup => String -> [t] -> TypedTerm TestGroup
supergroup name subgroups = tgroup name Nothing (asTerm <$> subgroups) []

tag :: String -> TypedTerm Tag
tag = Phantoms.wrap _Tag . Phantoms.string

testContextRef :: TypedTerm InferenceContext
testContextRef = TypedTerm $ TermVariable $ Name "hydra.test.testGraph.testContext"

-- | References to kernel functions (avoids circular imports)
testGraphRef :: TypedTerm Graph
testGraphRef = TypedTerm $ TermVariable $ Name "hydra.test.testGraph.testGraph"

tgroup :: String -> Maybe String -> [TypedTerm TestGroup] -> [TypedTerm TestCaseWithMetadata] -> TypedTerm TestGroup
tgroup name mdesc subgroups cases = testGroup (Phantoms.string name) (opt (Phantoms.string <$> mdesc)) (Phantoms.list subgroups) (Phantoms.list cases)

typeSchemeToFTypeRef :: TypedTerm (TypeScheme -> Type)
typeSchemeToFTypeRef = TypedTerm $ TermVariable $ Name "hydra.scoping.typeSchemeToFType"

universalCase :: String -> TypedTerm a -> TypedTerm b -> TypedTerm TestCaseWithMetadata
universalCase cname actual expected = testCaseWithMetadata (Phantoms.string cname)
  (testCaseUniversal $ universalTestCase (retype actual) (retype expected))
  nothing noTags
  where
    retype :: TypedTerm x -> TypedTerm String
    retype (TypedTerm t) = TypedTerm t

-- | Reference to the profile-aware core term validator. Used by
-- 'validateCoreTermCase' (with 'kernelDefaultCoreProfileRef'-applied,
-- head-extracted to preserve the legacy 'Maybe E' shape) and by
-- 'validateCoreTermCaseWithProfile' (with an explicit profile).
validateCoreTermProfiledRef :: TypedTerm (ValidationProfile -> Bool -> Graph -> Term -> ValidationResult InvalidTermError)
validateCoreTermProfiledRef = TypedTerm $ TermVariable $ Name "hydra.validate.core.term"

-- | Reference to the profile-aware packaging module validator. Used by
-- 'validatePackagingModuleCaseWithProfile' directly.
validatePackagingModuleProfiledRef :: TypedTerm (ValidationProfile -> ValidationResult InvalidModuleError -> Module -> ValidationResult InvalidModuleError)
validatePackagingModuleProfiledRef = TypedTerm $ TermVariable $ Name "hydra.validate.packaging.module"

-- | Reference to the profile-aware packaging package validator. Returns a
-- 'ValidationResult InvalidPackageError'; used by
-- 'validatePackagingPackageCaseWithProfile'.
validatePackagingPackageProfiledRef :: TypedTerm (ValidationProfile -> ValidationResult InvalidPackageError -> Package -> ValidationResult InvalidPackageError)
validatePackagingPackageProfiledRef = TypedTerm $ TermVariable $ Name "hydra.validate.packaging.package"

----------------------------------------

-- | Convenience function for creating alpha conversion test cases
alphaCase :: String -> TypedTerm Term -> TypedTerm Name -> TypedTerm Name -> TypedTerm Term -> TypedTerm TestCaseWithMetadata
alphaCase cname term oldVar newVar result = universalCase cname
  (retype $ showTermRef @@ (alphaConvertRef @@ oldVar @@ newVar @@ term))
  (retype $ showTermRef @@ result)
  where
    retype :: TypedTerm x -> TypedTerm String
    retype (TypedTerm t) = TypedTerm t

encodedTestGroupToBinding :: ModuleName -> String -> TypedTerm TestGroup -> Binding
encodedTestGroupToBinding ns lname group = Binding name (unTypedTerm group)
    $ Just $ TypeScheme [] typ Nothing
  where
    name = unqualifyName $ QualifiedName (Just ns) lname
    typ = TypeVariable _TestGroup

-- | Render a 'ValidationResult InvalidModuleError' as a string of the form
-- "errors=[s1;s2] warnings=[w1]". Counterpart of 'showValidationResultTerm'
-- for module-level findings.
showValidationResultModule :: TypedTerm (ValidationResult InvalidModuleError) -> TypedTerm String
showValidationResultModule vr = retype $
  Strings.cat2
    (Strings.cat2 (Phantoms.string "errors=[") $
      Strings.cat2 (Strings.intercalate (Phantoms.string ";") $
        Lists.map (Phantoms.lambda "e" $ showInvalidModuleErrorRef @@ Phantoms.var "e")
          (Validation.validationResultErrors vr))
        (Phantoms.string "]"))
    (Strings.cat2 (Phantoms.string " warnings=[") $
      Strings.cat2 (Strings.intercalate (Phantoms.string ";") $
        Lists.map (Phantoms.lambda "w" $ showInvalidModuleErrorRef @@ Phantoms.var "w")
          (Validation.validationResultWarnings vr))
        (Phantoms.string "]"))
  where
    retype :: TypedTerm x -> TypedTerm String
    retype (TypedTerm t) = TypedTerm t

-- | Render a 'ValidationResult InvalidPackageError' as a string. Package-level
-- counterpart of 'showValidationResultTerm'.
showValidationResultPackage :: TypedTerm (ValidationResult InvalidPackageError) -> TypedTerm String
showValidationResultPackage vr = retype $
  Strings.cat2
    (Strings.cat2 (Phantoms.string "errors=[") $
      Strings.cat2 (Strings.intercalate (Phantoms.string ";") $
        Lists.map (Phantoms.lambda "e" $ showInvalidPackageErrorRef @@ Phantoms.var "e")
          (Validation.validationResultErrors vr))
        (Phantoms.string "]"))
    (Strings.cat2 (Phantoms.string " warnings=[") $
      Strings.cat2 (Strings.intercalate (Phantoms.string ";") $
        Lists.map (Phantoms.lambda "w" $ showInvalidPackageErrorRef @@ Phantoms.var "w")
          (Validation.validationResultWarnings vr))
        (Phantoms.string "]"))
  where
    retype :: TypedTerm x -> TypedTerm String
    retype (TypedTerm t) = TypedTerm t

-- | Profile-aware variant of 'validatePackagingModuleCase'. Calls
-- 'hydra.validate.packaging.module'' (the new orchestrator) starting from
-- an empty 'ValidationResult' and compares the full result shape against
-- the expected.
validatePackagingModuleCaseWithProfile
  :: String
  -> TypedTerm ValidationProfile
  -> TypedTerm Module
  -> TypedTerm (ValidationResult InvalidModuleError)
  -> TypedTerm TestCaseWithMetadata
validatePackagingModuleCaseWithProfile cname profile input expected = universalCase cname
  (showValidationResultModule
    (validatePackagingModuleProfiledRef @@ profile @@ emptyResultModule @@ input))
  (showValidationResultModule expected)
  where
    emptyResultModule :: TypedTerm (ValidationResult InvalidModuleError)
    emptyResultModule = Validation.validationResult
      (Phantoms.list ([] :: [TypedTerm InvalidModuleError]))
      (Phantoms.list ([] :: [TypedTerm InvalidModuleError]))

-- | Profile-aware variant of 'validatePackagingPackageCase'.
validatePackagingPackageCaseWithProfile
  :: String
  -> TypedTerm ValidationProfile
  -> TypedTerm Package
  -> TypedTerm (ValidationResult InvalidPackageError)
  -> TypedTerm TestCaseWithMetadata
validatePackagingPackageCaseWithProfile cname profile input expected = universalCase cname
  (showValidationResultPackage
    (validatePackagingPackageProfiledRef @@ profile @@ emptyResultPackage @@ input))
  (showValidationResultPackage expected)
  where
    emptyResultPackage :: TypedTerm (ValidationResult InvalidPackageError)
    emptyResultPackage = Validation.validationResult
      (Phantoms.list ([] :: [TypedTerm InvalidPackageError]))
      (Phantoms.list ([] :: [TypedTerm InvalidPackageError]))

-- | Render a 'ValidationResult InvalidTermError' as a string of the form
-- "errors=[s1;s2] warnings=[w1]". Used to compare actual vs expected
-- 'ValidationResult' values in profile-aware test cases.
showValidationResultTerm :: TypedTerm (ValidationResult InvalidTermError) -> TypedTerm String
showValidationResultTerm vr = retype $
  Strings.cat2
    (Strings.cat2 (Phantoms.string "errors=[") $
      Strings.cat2 (Strings.intercalate (Phantoms.string ";") $
        Lists.map (Phantoms.lambda "e" $ showInvalidTermErrorRef @@ Phantoms.var "e")
          (Validation.validationResultErrors vr))
        (Phantoms.string "]"))
    (Strings.cat2 (Phantoms.string " warnings=[") $
      Strings.cat2 (Strings.intercalate (Phantoms.string ";") $
        Lists.map (Phantoms.lambda "w" $ showInvalidTermErrorRef @@ Phantoms.var "w")
          (Validation.validationResultWarnings vr))
        (Phantoms.string "]"))
  where
    retype :: TypedTerm x -> TypedTerm String
    retype (TypedTerm t) = TypedTerm t

-- | Profile-aware variant of 'validateCoreTermCase'. Compares the full
-- 'ValidationResult' shape — both errors and warnings — against the
-- expected, allowing the caller to specify any profile (not just the
-- kernel default). Use for tests that exercise multi-error
-- accumulation, warning vs error classification, or rule disabling.
validateCoreTermCaseWithProfile
  :: String
  -> TypedTerm ValidationProfile
  -> TypedTerm Bool
  -> TypedTerm Term
  -> TypedTerm (ValidationResult InvalidTermError)
  -> TypedTerm TestCaseWithMetadata
validateCoreTermCaseWithProfile cname profile typed input expected = universalCase cname
  (showValidationResultTerm
    (validateCoreTermProfiledRef @@ profile @@ typed @@ testGraphRef @@ input))
  (showValidationResultTerm expected)

testCaseUniversal :: TypedTerm UniversalTestCase -> TypedTerm TestCase
testCaseUniversal = inject _TestCase _TestCase_universal

testCaseWithMetadata :: TypedTerm String -> TypedTerm TestCase -> TypedTerm (Maybe String) -> TypedTerm [Tag] -> TypedTerm TestCaseWithMetadata
testCaseWithMetadata name tcase description tags = Phantoms.record _TestCaseWithMetadata [
  _TestCaseWithMetadata_name>>: name,
  _TestCaseWithMetadata_case>>: tcase,
  _TestCaseWithMetadata_description>>: description,
  _TestCaseWithMetadata_tags>>: tags]

testCaseWithMetadataCase :: TypedTerm (TestCaseWithMetadata -> TestCase)
testCaseWithMetadataCase = Phantoms.project _TestCaseWithMetadata _TestCaseWithMetadata_case

testCaseWithMetadataDescription :: TypedTerm (TestCaseWithMetadata -> Maybe String)
testCaseWithMetadataDescription = Phantoms.project _TestCaseWithMetadata _TestCaseWithMetadata_description

testCaseWithMetadataName :: TypedTerm (TestCaseWithMetadata -> String)
testCaseWithMetadataName = Phantoms.project _TestCaseWithMetadata _TestCaseWithMetadata_name

testCaseWithMetadataTags :: TypedTerm (TestCaseWithMetadata -> [Tag])
testCaseWithMetadataTags = Phantoms.project _TestCaseWithMetadata _TestCaseWithMetadata_tags

testGroup :: TypedTerm String -> TypedTerm (Maybe String) -> TypedTerm [TestGroup] -> TypedTerm [TestCaseWithMetadata] -> TypedTerm TestGroup
testGroup name description subgroups cases = Phantoms.record _TestGroup [
  _TestGroup_name>>: name,
  _TestGroup_description>>: description,
  _TestGroup_subgroups>>: subgroups,
  _TestGroup_cases>>: cases]

-- | Convenience function for creating type reduction test cases
typeRedCase :: String -> TypedTerm Type -> TypedTerm Type -> TypedTerm TestCaseWithMetadata
typeRedCase cname input output = universalCase cname
  (retype $ Eithers.either_
    (Phantoms.lambda "e" (Phantoms.string "<<type reduction error>>"))
    (Phantoms.lambda "t" (showTypeRef @@ Phantoms.var "t"))
    (betaReduceTypeRef @@ testContextRef @@ testGraphRef @@ input))
  (retype $ showTypeRef @@ output)
  where
    retype :: TypedTerm x -> TypedTerm String
    retype (TypedTerm t) = TypedTerm t

-- | Build a 'UniversalTestCase' from two string-producing expressions.
-- Each expression is wrapped in a unit-lambda so its evaluation is deferred
-- until the runner forces the thunk inside its per-test timing bracket. See
-- issue #311: without thunking, eagerly-evaluated hosts (Scala, the four
-- complete Lisps) compute 'actual'/'expected' at test-data load time, before
-- any timer starts, and report zero elapsed time.
universalTestCase :: TypedTerm String -> TypedTerm String -> TypedTerm UniversalTestCase
universalTestCase actual expected = Phantoms.record _UniversalTestCase [
  _UniversalTestCase_actual Phantoms.>>: thunk actual,
  _UniversalTestCase_expected Phantoms.>>: thunk expected]
  where
    thunk :: TypedTerm String -> TypedTerm (() -> String)
    thunk body = Phantoms.lambda "_" body

-- | Convenience function for creating validation test cases. Drives the
-- profile-aware 'hydra.validate.core.term' with the kernel-default core
-- profile and head-extracts the resulting errors list — preserving the
-- legacy 'Maybe InvalidTermError' shape that existing test data uses.
-- Tests that need to assert multi-error accumulation, warning
-- classification, or rule disabling should use
-- 'validateCoreTermCaseWithProfile' instead.
validateCoreTermCase :: String -> TypedTerm Bool -> TypedTerm Term -> TypedTerm (Maybe InvalidTermError) -> TypedTerm TestCaseWithMetadata
validateCoreTermCase cname typed input expected = universalCase cname
  (retype $ Maybes.cases
    (Lists.maybeHead $ Validation.validationResultErrors $
      validateCoreTermProfiledRef @@ kernelDefaultCoreProfileRef @@ typed @@ testGraphRef @@ input)
    (Phantoms.string "valid")
    (Phantoms.lambda "e" (showInvalidTermErrorRef @@ Phantoms.var "e")))
  (retype $ Maybes.cases
    expected
    (Phantoms.string "valid")
    (Phantoms.lambda "e" (showInvalidTermErrorRef @@ Phantoms.var "e")))
  where
    retype :: TypedTerm x -> TypedTerm String
    retype (TypedTerm t) = TypedTerm t

-- | Convenience function for creating module-validation test cases.
-- Applies a packaging-level Module validator to an input module and compares
-- the result against the expected Maybe InvalidModuleError.
validatePackagingModuleCase :: String -> TypedTerm (Module -> Maybe InvalidModuleError) -> TypedTerm Module -> TypedTerm (Maybe InvalidModuleError) -> TypedTerm TestCaseWithMetadata
validatePackagingModuleCase cname validator input expected = universalCase cname
  (retype $ Maybes.cases
    (validator @@ input)
    (Phantoms.string "valid")
    (Phantoms.lambda "e" (showInvalidModuleErrorRef @@ Phantoms.var "e")))
  (retype $ Maybes.cases
    expected
    (Phantoms.string "valid")
    (Phantoms.lambda "e" (showInvalidModuleErrorRef @@ Phantoms.var "e")))
  where
    retype :: TypedTerm x -> TypedTerm String
    retype (TypedTerm t) = TypedTerm t

-- | Convenience function for creating package-validation test cases.
-- Applies a packaging-level Package validator to an input package and compares
-- the result against the expected Maybe InvalidPackageError.
validatePackagingPackageCase :: String -> TypedTerm (Package -> Maybe InvalidPackageError) -> TypedTerm Package -> TypedTerm (Maybe InvalidPackageError) -> TypedTerm TestCaseWithMetadata
validatePackagingPackageCase cname validator input expected = universalCase cname
  (retype $ Maybes.cases
    (validator @@ input)
    (Phantoms.string "valid")
    (Phantoms.lambda "e" (showInvalidPackageErrorRef @@ Phantoms.var "e")))
  (retype $ Maybes.cases
    expected
    (Phantoms.string "valid")
    (Phantoms.lambda "e" (showInvalidPackageErrorRef @@ Phantoms.var "e")))
  where
    retype :: TypedTerm x -> TypedTerm String
    retype (TypedTerm t) = TypedTerm t
