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

noTags :: TTerm [Tag]
noTags = Phantoms.list ([] :: [TTerm Tag])

expectFailure :: AsTerm t Term => Int -> [Tag] -> t -> TTerm TestCaseWithMetadata
expectFailure i tags term = infFailureTest ("#" ++ show i) tags (asTerm term)

expectMono :: AsTerm t Term => Int -> [Tag] -> t -> TTerm Type -> TTerm TestCaseWithMetadata
expectMono i tags term typ = infTest ("#" ++ show i) tags (asTerm term) $ T.mono typ

expectPoly :: AsTerm t Term => Int -> [Tag] -> t -> [String] -> TTerm Type -> TTerm TestCaseWithMetadata
expectPoly i tags term params typ = infTest ("#" ++ show i) tags (asTerm term) $ T.poly params typ

expectPolyConstrained :: AsTerm t Term => Int -> [Tag] -> t -> [String] -> [(String, [String])] -> TTerm Type -> TTerm TestCaseWithMetadata
expectPolyConstrained i tags term params constraints typ = infTest ("#" ++ show i) tags (asTerm term) $ T.polyConstrained params constraints typ

groupRef = MetaTerms.varNamePhantom . bindingName

primCase :: String -> Name -> [TTerm Term] -> TTerm Term -> TTerm TestCaseWithMetadata
primCase cname primName args output = primCaseWithTags cname [] primName args output

primCaseWithTags :: String -> [Tag] -> Name -> [TTerm Term] -> TTerm Term -> TTerm TestCaseWithMetadata
primCaseWithTags cname tags primName args output = evalCaseWithTags cname tags input output
  where
    input = L.foldl (MetaTerms.@@) (MetaTerms.primitive primName) args

evalCase :: String -> TTerm Term -> TTerm Term -> TTerm TestCaseWithMetadata
evalCase cname input output = evalCaseWithTags cname [] input output

-- | Create a universal test case that evaluates a Term via reduceTerm and compares the result.
evalCaseWithTags :: String -> [Tag] -> TTerm Term -> TTerm Term -> TTerm TestCaseWithMetadata
evalCaseWithTags cname tags input output = testCaseWithMetadata (Phantoms.string cname)
  (testCaseUniversal $ universalTestCase
    (retype $ Eithers.either_
      (Phantoms.lambda "e" (Phantoms.string "<<eval error>>"))
      (Phantoms.lambda "t" (showTermRef @@ Phantoms.var "t"))
      (reduceTermRef @@ testContextRef @@ testGraphRef @@ true @@ input))
    (retype $ showTermRef @@ output))
  nothing (Phantoms.list $ tag . unTag <$> tags)
  where
    retype :: TTerm x -> TTerm String
    retype (TTerm t) = TTerm t

-- | References to kernel functions (avoids circular imports)
testGraphRef :: TTerm Graph
testGraphRef = TTerm $ TermVariable $ Name "hydra.test.testGraph.testGraph"

testContextRef :: TTerm InferenceContext
testContextRef = TTerm $ TermVariable $ Name "hydra.test.testGraph.testContext"

showTermRef :: TTerm (Term -> String)
showTermRef = TTerm $ TermVariable $ Name "hydra.show.core.term"

showTypeRef :: TTerm (Type -> String)
showTypeRef = TTerm $ TermVariable $ Name "hydra.show.core.type"

inferTypeOfRef :: TTerm (InferenceContext -> Graph -> Term -> Either Error ((Term, TypeScheme), InferenceContext))
inferTypeOfRef = TTerm $ TermVariable $ Name "hydra.inference.inferTypeOf"

alphaConvertRef :: TTerm (Name -> Name -> Term -> Term)
alphaConvertRef = TTerm $ TermVariable $ Name "hydra.reduction.alphaConvert"

betaReduceTypeRef :: TTerm (InferenceContext -> Graph -> Type -> Either Error Type)
betaReduceTypeRef = TTerm $ TermVariable $ Name "hydra.reduction.betaReduceType"

-- | Reference to the profile-aware core term validator. Used by
-- 'validateCoreTermCase' (with 'kernelDefaultCoreProfileRef'-applied,
-- head-extracted to preserve the legacy 'Maybe E' shape) and by
-- 'validateCoreTermCaseWithProfile' (with an explicit profile).
validateCoreTermProfiledRef :: TTerm (ValidationProfile -> Bool -> Graph -> Term -> ValidationResult InvalidTermError)
validateCoreTermProfiledRef = TTerm $ TermVariable $ Name "hydra.validate.core.term"

-- | Reference to the kernel-default core validation profile (term + type
-- rules; 'singleVariantUnion' classified as a warning, everything else
-- as an error; maxErrors=1).
kernelDefaultCoreProfileRef :: TTerm ValidationProfile
kernelDefaultCoreProfileRef = TTerm $ TermVariable $ Name "hydra.validate.core.kernelDefaultCoreProfile"

showInvalidTermErrorRef :: TTerm (InvalidTermError -> String)
showInvalidTermErrorRef = TTerm $ TermVariable $ Name "hydra.show.error.core.invalidTermError"

-- | Refs for hydra.validate.packaging validators and their show functions.
checkConflictingModuleNamesRef :: TTerm (Package -> Maybe InvalidPackageError)
checkConflictingModuleNamesRef = TTerm $ TermVariable $ Name "hydra.validate.packaging.checkConflictingModuleNames"

checkConflictingVariantNamesRef :: TTerm (Module -> Maybe InvalidModuleError)
checkConflictingVariantNamesRef = TTerm $ TermVariable $ Name "hydra.validate.packaging.checkConflictingVariantNames"

checkDefinitionDocumentationRef :: TTerm (Module -> Maybe InvalidModuleError)
checkDefinitionDocumentationRef = TTerm $ TermVariable $ Name "hydra.validate.packaging.checkDefinitionDocumentation"

checkDefinitionNameConventionRef :: TTerm (Module -> Maybe InvalidModuleError)
checkDefinitionNameConventionRef = TTerm $ TermVariable $ Name "hydra.validate.packaging.checkDefinitionNameConvention"

checkDefinitionModuleNamesRef :: TTerm (Module -> Maybe InvalidModuleError)
checkDefinitionModuleNamesRef = TTerm $ TermVariable $ Name "hydra.validate.packaging.checkDefinitionModuleNames"

checkDefinitionOrderingRef :: TTerm (Module -> Maybe InvalidModuleError)
checkDefinitionOrderingRef = TTerm $ TermVariable $ Name "hydra.validate.packaging.checkDefinitionOrdering"

checkDuplicateDefinitionNamesRef :: TTerm (Module -> Maybe InvalidModuleError)
checkDuplicateDefinitionNamesRef = TTerm $ TermVariable $ Name "hydra.validate.packaging.checkDuplicateDefinitionNames"

checkDuplicateModuleNamesRef :: TTerm (Package -> Maybe InvalidPackageError)
checkDuplicateModuleNamesRef = TTerm $ TermVariable $ Name "hydra.validate.packaging.checkDuplicateModuleNames"

checkModuleNameConventionRef :: TTerm (Module -> Maybe InvalidModuleError)
checkModuleNameConventionRef = TTerm $ TermVariable $ Name "hydra.validate.packaging.checkModuleNameConvention"

checkPackageNameConventionRef :: TTerm (Package -> Maybe InvalidPackageError)
checkPackageNameConventionRef = TTerm $ TermVariable $ Name "hydra.validate.packaging.checkPackageNameConvention"

-- | Reference to the kernel-strict 'kernelModule' convenience wrapper
-- (Maybe-returning, applies 'kernelDefaultPackagingProfile' internally).
-- Used by the kernelModule orchestrator tests in
-- 'Sources/Test/Validate/Packaging.hs'.
kernelModuleRef :: TTerm (Module -> Maybe InvalidModuleError)
kernelModuleRef = TTerm $ TermVariable $ Name "hydra.validate.packaging.kernelModule"

-- | Reference to the kernel-strict 'kernelPackage' convenience wrapper.
kernelPackageRef :: TTerm (Package -> Maybe InvalidPackageError)
kernelPackageRef = TTerm $ TermVariable $ Name "hydra.validate.packaging.kernelPackage"

-- | Reference to the profile-aware packaging module validator. Used by
-- 'validatePackagingModuleCaseWithProfile' directly.
validatePackagingModuleProfiledRef :: TTerm (ValidationProfile -> ValidationResult InvalidModuleError -> Module -> ValidationResult InvalidModuleError)
validatePackagingModuleProfiledRef = TTerm $ TermVariable $ Name "hydra.validate.packaging.module"

-- | Reference to the profile-aware packaging package validator. Returns a
-- 'ValidationResult InvalidPackageError'; used by
-- 'validatePackagingPackageCaseWithProfile'.
validatePackagingPackageProfiledRef :: TTerm (ValidationProfile -> ValidationResult InvalidPackageError -> Package -> ValidationResult InvalidPackageError)
validatePackagingPackageProfiledRef = TTerm $ TermVariable $ Name "hydra.validate.packaging.package"

-- | Reference to the kernel-default packaging validation profile (every
-- per-module and per-package check classified as an error; maxErrors=1).
kernelDefaultPackagingProfileRef :: TTerm ValidationProfile
kernelDefaultPackagingProfileRef = TTerm $ TermVariable $ Name "hydra.validate.packaging.kernelDefaultPackagingProfile"

showInvalidModuleErrorRef :: TTerm (InvalidModuleError -> String)
showInvalidModuleErrorRef = TTerm $ TermVariable $ Name "hydra.show.error.packaging.invalidModuleError"

showInvalidPackageErrorRef :: TTerm (InvalidPackageError -> String)
showInvalidPackageErrorRef = TTerm $ TermVariable $ Name "hydra.show.error.packaging.invalidPackageError"

showTypeSchemeRef :: TTerm (TypeScheme -> String)
showTypeSchemeRef = TTerm $ TermVariable $ Name "hydra.show.core.typeScheme"

reduceTermRef :: TTerm (InferenceContext -> Graph -> Bool -> Term -> Either Error Term)
reduceTermRef = TTerm $ TermVariable $ Name "hydra.reduction.reduceTerm"

removeTypesFromTermRef :: TTerm (Term -> Term)
removeTypesFromTermRef = TTerm $ TermVariable $ Name "hydra.strip.removeTypesFromTerm"

typeSchemeToFTypeRef :: TTerm (TypeScheme -> Type)
typeSchemeToFTypeRef = TTerm $ TermVariable $ Name "hydra.scoping.typeSchemeToFType"

-- | Create a universal test case for an expression with a show function
evalPair :: String -> TTerm (t -> String) -> TTerm t -> TTerm t -> TTerm TestCaseWithMetadata
evalPair cname showFn logicalActual logicalExpected = universalCase cname
  (retype $ showFn @@ logicalActual) (retype $ showFn @@ logicalExpected)
  where
    retype :: TTerm x -> TTerm String
    retype (TTerm t) = TTerm t

-- | evalPair for String-typed expressions (identity show)
stringEvalPair :: String -> TTerm String -> TTerm String -> TTerm TestCaseWithMetadata
stringEvalPair cname = evalPair cname (Phantoms.lambda "s" (Phantoms.var "s"))

-- | evalPair with tags
evalPairWithTags :: String -> [Tag] -> TTerm (t -> String) -> TTerm t -> TTerm t -> TTerm TestCaseWithMetadata
evalPairWithTags cname tags showFn logicalActual logicalExpected = testCaseWithMetadata (Phantoms.string cname)
  (testCaseUniversal $ universalTestCase (retype $ showFn @@ logicalActual) (retype $ showFn @@ logicalExpected))
  nothing (Phantoms.list $ tag . unTag <$> tags)
  where
    retype :: TTerm x -> TTerm String
    retype (TTerm t) = TTerm t

-- | Type checking test: infers the type and compares with expected.
checkTest :: String -> [Tag] -> TTerm Term -> TTerm Term -> TTerm Type -> TTerm TestCaseWithMetadata
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
    retype :: TTerm x -> TTerm String
    retype (TTerm t) = TTerm t

-- | Type checking test where term doesn't change (just check the inferred type)
noChange :: String -> TTerm Term -> TTerm Type -> TTerm TestCaseWithMetadata
noChange name term typ = checkTest name [] term term typ

universalCase :: String -> TTerm a -> TTerm b -> TTerm TestCaseWithMetadata
universalCase cname actual expected = testCaseWithMetadata (Phantoms.string cname)
  (testCaseUniversal $ universalTestCase (retype actual) (retype expected))
  nothing noTags
  where
    retype :: TTerm x -> TTerm String
    retype (TTerm t) = TTerm t

-- | Inference failure test: expects inference to fail.
infFailureTest :: String -> [Tag] -> TTerm Term -> TTerm TestCaseWithMetadata
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
    retype :: TTerm x -> TTerm String
    retype (TTerm t) = TTerm t

-- | Inference test: infer type and compare with expected type scheme.
infTest :: String -> [Tag] -> TTerm Term -> TTerm TypeScheme -> TTerm TestCaseWithMetadata
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
    retype :: TTerm x -> TTerm String
    retype (TTerm t) = TTerm t

isDisabled tcase = tag_disabled `L.elem` Testing.testCaseWithMetadataTags tcase
isDisabledForMinimalInference tcase = tag_disabledForMinimalInference `L.elem` Testing.testCaseWithMetadataTags tcase

mapTerm :: [(TTerm Term, TTerm Term)] -> TTerm Term
mapTerm pairs = TTerm $ TermInject $ Injection _Term $ Field _Term_map $ TermMap $ M.fromList [(unTTerm k, unTTerm v) | (k, v) <- pairs]

mapTermEmpty :: TTerm (M.Map k v)
mapTermEmpty = TTerm $ TermMap M.empty

subgroup :: AsTerm t TestCaseWithMetadata => String -> [t] -> TTerm TestGroup
subgroup name cases = tgroup name Nothing [] (asTerm <$> cases)

supergroup :: AsTerm t TestGroup => String -> [t] -> TTerm TestGroup
supergroup name subgroups = tgroup name Nothing (asTerm <$> subgroups) []

tag :: String -> TTerm Tag
tag = Phantoms.wrap _Tag . Phantoms.string

tgroup :: String -> Maybe String -> [TTerm TestGroup] -> [TTerm TestCaseWithMetadata] -> TTerm TestGroup
tgroup name mdesc subgroups cases = testGroup (Phantoms.string name) (opt (Phantoms.string <$> mdesc)) (Phantoms.list subgroups) (Phantoms.list cases)

----------------------------------------

encodedTestGroupToBinding :: ModuleName -> String -> TTerm TestGroup -> Binding
encodedTestGroupToBinding ns lname group = Binding name (unTTerm group)
    $ Just $ TypeScheme [] typ Nothing
  where
    name = unqualifyName $ QualifiedName (Just ns) lname
    typ = TypeVariable _TestGroup

testCaseUniversal :: TTerm UniversalTestCase -> TTerm TestCase
testCaseUniversal = inject _TestCase _TestCase_universal

-- | Build a 'UniversalTestCase' from two string-producing expressions.
-- Each expression is wrapped in a unit-lambda so its evaluation is deferred
-- until the runner forces the thunk inside its per-test timing bracket. See
-- issue #311: without thunking, eagerly-evaluated hosts (Scala, the four
-- complete Lisps) compute 'actual'/'expected' at test-data load time, before
-- any timer starts, and report zero elapsed time.
universalTestCase :: TTerm String -> TTerm String -> TTerm UniversalTestCase
universalTestCase actual expected = Phantoms.record _UniversalTestCase [
  _UniversalTestCase_actual Phantoms.>>: thunk actual,
  _UniversalTestCase_expected Phantoms.>>: thunk expected]
  where
    thunk :: TTerm String -> TTerm (() -> String)
    thunk body = Phantoms.lambda "_" body

-- | Convenience function for creating alpha conversion test cases
alphaCase :: String -> TTerm Term -> TTerm Name -> TTerm Name -> TTerm Term -> TTerm TestCaseWithMetadata
alphaCase cname term oldVar newVar result = universalCase cname
  (retype $ showTermRef @@ (alphaConvertRef @@ oldVar @@ newVar @@ term))
  (retype $ showTermRef @@ result)
  where
    retype :: TTerm x -> TTerm String
    retype (TTerm t) = TTerm t

-- | Convenience function for creating type reduction test cases
typeRedCase :: String -> TTerm Type -> TTerm Type -> TTerm TestCaseWithMetadata
typeRedCase cname input output = universalCase cname
  (retype $ Eithers.either_
    (Phantoms.lambda "e" (Phantoms.string "<<type reduction error>>"))
    (Phantoms.lambda "t" (showTypeRef @@ Phantoms.var "t"))
    (betaReduceTypeRef @@ testContextRef @@ testGraphRef @@ input))
  (retype $ showTypeRef @@ output)
  where
    retype :: TTerm x -> TTerm String
    retype (TTerm t) = TTerm t

testCaseWithMetadata :: TTerm String -> TTerm TestCase -> TTerm (Maybe String) -> TTerm [Tag] -> TTerm TestCaseWithMetadata
testCaseWithMetadata name tcase description tags = Phantoms.record _TestCaseWithMetadata [
  _TestCaseWithMetadata_name>>: name,
  _TestCaseWithMetadata_case>>: tcase,
  _TestCaseWithMetadata_description>>: description,
  _TestCaseWithMetadata_tags>>: tags]

testCaseWithMetadataName :: TTerm (TestCaseWithMetadata -> String)
testCaseWithMetadataName = Phantoms.project _TestCaseWithMetadata _TestCaseWithMetadata_name

testCaseWithMetadataCase :: TTerm (TestCaseWithMetadata -> TestCase)
testCaseWithMetadataCase = Phantoms.project _TestCaseWithMetadata _TestCaseWithMetadata_case

testCaseWithMetadataDescription :: TTerm (TestCaseWithMetadata -> Maybe String)
testCaseWithMetadataDescription = Phantoms.project _TestCaseWithMetadata _TestCaseWithMetadata_description

testCaseWithMetadataTags :: TTerm (TestCaseWithMetadata -> [Tag])
testCaseWithMetadataTags = Phantoms.project _TestCaseWithMetadata _TestCaseWithMetadata_tags

testGroup :: TTerm String -> TTerm (Maybe String) -> TTerm [TestGroup] -> TTerm [TestCaseWithMetadata] -> TTerm TestGroup
testGroup name description subgroups cases = Phantoms.record _TestGroup [
  _TestGroup_name>>: name,
  _TestGroup_description>>: description,
  _TestGroup_subgroups>>: subgroups,
  _TestGroup_cases>>: cases]

-- | Convenience function for creating validation test cases. Drives the
-- profile-aware 'hydra.validate.core.term' with the kernel-default core
-- profile and head-extracts the resulting errors list — preserving the
-- legacy 'Maybe InvalidTermError' shape that existing test data uses.
-- Tests that need to assert multi-error accumulation, warning
-- classification, or rule disabling should use
-- 'validateCoreTermCaseWithProfile' instead.
validateCoreTermCase :: String -> TTerm Bool -> TTerm Term -> TTerm (Maybe InvalidTermError) -> TTerm TestCaseWithMetadata
validateCoreTermCase cname typed input expected = universalCase cname
  (retype $ Maybes.maybe
    (Phantoms.string "valid")
    (Phantoms.lambda "e" (showInvalidTermErrorRef @@ Phantoms.var "e"))
    (Lists.maybeHead $ Validation.validationResultErrors $
      validateCoreTermProfiledRef @@ kernelDefaultCoreProfileRef @@ typed @@ testGraphRef @@ input))
  (retype $ Maybes.maybe
    (Phantoms.string "valid")
    (Phantoms.lambda "e" (showInvalidTermErrorRef @@ Phantoms.var "e"))
    expected)
  where
    retype :: TTerm x -> TTerm String
    retype (TTerm t) = TTerm t

-- | Render a 'ValidationResult InvalidTermError' as a string of the form
-- "errors=[s1;s2] warnings=[w1]". Used to compare actual vs expected
-- 'ValidationResult' values in profile-aware test cases.
showValidationResultTerm :: TTerm (ValidationResult InvalidTermError) -> TTerm String
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
    retype :: TTerm x -> TTerm String
    retype (TTerm t) = TTerm t

-- | Profile-aware variant of 'validateCoreTermCase'. Compares the full
-- 'ValidationResult' shape — both errors and warnings — against the
-- expected, allowing the caller to specify any profile (not just the
-- kernel default). Use for tests that exercise multi-error
-- accumulation, warning vs error classification, or rule disabling.
validateCoreTermCaseWithProfile
  :: String
  -> TTerm ValidationProfile
  -> TTerm Bool
  -> TTerm Term
  -> TTerm (ValidationResult InvalidTermError)
  -> TTerm TestCaseWithMetadata
validateCoreTermCaseWithProfile cname profile typed input expected = universalCase cname
  (showValidationResultTerm
    (validateCoreTermProfiledRef @@ profile @@ typed @@ testGraphRef @@ input))
  (showValidationResultTerm expected)

-- | Convenience function for creating module-validation test cases.
-- Applies a packaging-level Module validator to an input module and compares
-- the result against the expected Maybe InvalidModuleError.
validatePackagingModuleCase :: String -> TTerm (Module -> Maybe InvalidModuleError) -> TTerm Module -> TTerm (Maybe InvalidModuleError) -> TTerm TestCaseWithMetadata
validatePackagingModuleCase cname validator input expected = universalCase cname
  (retype $ Maybes.maybe
    (Phantoms.string "valid")
    (Phantoms.lambda "e" (showInvalidModuleErrorRef @@ Phantoms.var "e"))
    (validator @@ input))
  (retype $ Maybes.maybe
    (Phantoms.string "valid")
    (Phantoms.lambda "e" (showInvalidModuleErrorRef @@ Phantoms.var "e"))
    expected)
  where
    retype :: TTerm x -> TTerm String
    retype (TTerm t) = TTerm t

-- | Convenience function for creating package-validation test cases.
-- Applies a packaging-level Package validator to an input package and compares
-- the result against the expected Maybe InvalidPackageError.
validatePackagingPackageCase :: String -> TTerm (Package -> Maybe InvalidPackageError) -> TTerm Package -> TTerm (Maybe InvalidPackageError) -> TTerm TestCaseWithMetadata
validatePackagingPackageCase cname validator input expected = universalCase cname
  (retype $ Maybes.maybe
    (Phantoms.string "valid")
    (Phantoms.lambda "e" (showInvalidPackageErrorRef @@ Phantoms.var "e"))
    (validator @@ input))
  (retype $ Maybes.maybe
    (Phantoms.string "valid")
    (Phantoms.lambda "e" (showInvalidPackageErrorRef @@ Phantoms.var "e"))
    expected)
  where
    retype :: TTerm x -> TTerm String
    retype (TTerm t) = TTerm t

-- | Render a 'ValidationResult InvalidModuleError' as a string of the form
-- "errors=[s1;s2] warnings=[w1]". Counterpart of 'showValidationResultTerm'
-- for module-level findings.
showValidationResultModule :: TTerm (ValidationResult InvalidModuleError) -> TTerm String
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
    retype :: TTerm x -> TTerm String
    retype (TTerm t) = TTerm t

-- | Render a 'ValidationResult InvalidPackageError' as a string. Package-level
-- counterpart of 'showValidationResultTerm'.
showValidationResultPackage :: TTerm (ValidationResult InvalidPackageError) -> TTerm String
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
    retype :: TTerm x -> TTerm String
    retype (TTerm t) = TTerm t

-- | Profile-aware variant of 'validatePackagingModuleCase'. Calls
-- 'hydra.validate.packaging.module'' (the new orchestrator) starting from
-- an empty 'ValidationResult' and compares the full result shape against
-- the expected.
validatePackagingModuleCaseWithProfile
  :: String
  -> TTerm ValidationProfile
  -> TTerm Module
  -> TTerm (ValidationResult InvalidModuleError)
  -> TTerm TestCaseWithMetadata
validatePackagingModuleCaseWithProfile cname profile input expected = universalCase cname
  (showValidationResultModule
    (validatePackagingModuleProfiledRef @@ profile @@ emptyResultModule @@ input))
  (showValidationResultModule expected)
  where
    emptyResultModule :: TTerm (ValidationResult InvalidModuleError)
    emptyResultModule = Validation.validationResult
      (Phantoms.list ([] :: [TTerm InvalidModuleError]))
      (Phantoms.list ([] :: [TTerm InvalidModuleError]))

-- | Profile-aware variant of 'validatePackagingPackageCase'.
validatePackagingPackageCaseWithProfile
  :: String
  -> TTerm ValidationProfile
  -> TTerm Package
  -> TTerm (ValidationResult InvalidPackageError)
  -> TTerm TestCaseWithMetadata
validatePackagingPackageCaseWithProfile cname profile input expected = universalCase cname
  (showValidationResultPackage
    (validatePackagingPackageProfiledRef @@ profile @@ emptyResultPackage @@ input))
  (showValidationResultPackage expected)
  where
    emptyResultPackage :: TTerm (ValidationResult InvalidPackageError)
    emptyResultPackage = Validation.validationResult
      (Phantoms.list ([] :: [TTerm InvalidPackageError]))
      (Phantoms.list ([] :: [TTerm InvalidPackageError]))
