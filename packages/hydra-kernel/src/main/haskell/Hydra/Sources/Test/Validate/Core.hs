
-- | Test cases for core term and type validation
module Hydra.Sources.Test.Validate.Core where

-- Standard imports for term-encoded tests
import Hydra.Kernel
import           Hydra.Dsl.Bootstrap (unqualifiedDep, descriptionMetadata)
import Hydra.Dsl.Meta.Testing                 as Testing
import Hydra.Dsl.Meta.Terms                   as Terms
import Hydra.Sources.Kernel.Types.All
import qualified Hydra.Dsl.Meta.Core          as Core
import qualified Hydra.Dsl.Meta.Lib.Lists     as Lists
import qualified Hydra.Dsl.Meta.Lib.Sets      as Sets
import qualified Hydra.Dsl.Meta.Phantoms      as Phantoms
import qualified Hydra.Dsl.Meta.Types         as T
import qualified Hydra.Dsl.Validation         as Validation
import qualified Hydra.Sources.Test.TestGraph as TestGraph
import qualified Hydra.Sources.Test.TestTerms as TestTerms
import qualified Hydra.Sources.Test.TestTypes as TestTypes
import qualified Data.List                    as L
import qualified Data.Map                     as M

import Hydra.Testing
import Hydra.Sources.Libraries


ns :: ModuleName
ns = ModuleName "hydra.test.validate.core"

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = definitions,
            moduleDependencies = unqualifiedDep <$> ([ModuleName "hydra.validate.core", ModuleName "hydra.show.error.core", ModuleName "hydra.test.testGraph"] ++ kernelTypesModuleNames),
            moduleMetadata = descriptionMetadata ((Just "Test cases for core term and type validation"))}
  where
    definitions = [
      Phantoms.toDefinition allTests,
      Phantoms.toDefinition duplicateBindingsTests,
      Phantoms.toDefinition duplicateFieldsTests,
      Phantoms.toDefinition emptyLetBindingsTests,
      Phantoms.toDefinition identityApplicationTests,
      Phantoms.toDefinition profileBehaviourTests,
      Phantoms.toDefinition variableShadowingTests]
      -- Commented out pending test gen fixes (raw constructors / unresolvable names):
      -- annotationTests, selfApplicationTests, emptyCaseStatementTests,
      -- emptyTypeNameTests, namingConventionTests

define :: String -> TypedTerm a -> TypedTermDefinition a
define = definitionInModule module_

-- ============================================================================
-- Helpers
-- ============================================================================

-- Empty accessor path
emptyPath :: TypedTerm SubtermPath
emptyPath = Phantoms.wrap _SubtermPath (Phantoms.list ([] :: [TypedTerm SubtermStep]))

-- Typed just for InvalidTermError
justError :: TypedTerm InvalidTermError -> TypedTerm (Maybe InvalidTermError)
justError (TypedTerm t) = TypedTerm $ TermOptional $ Just t

-- Helper to build names
nm :: String -> TypedTerm Name
nm s = Core.name $ Phantoms.string s

-- No error expected
noError :: TypedTerm (Maybe InvalidTermError)
noError = TypedTerm $ TermOptional Nothing

-- Shorthand for untyped term test case
untypedCase :: String -> TypedTerm Term -> TypedTerm (Maybe InvalidTermError) -> TypedTerm TestCaseWithMetadata
untypedCase name = validateCoreTermCase name (Phantoms.boolean False)

-- Error constructors

-- SubtermStep helpers
accLambdaBody :: TypedTerm SubtermStep
accLambdaBody = Phantoms.inject _SubtermStep _SubtermStep_lambdaBody Phantoms.unit

accLetBinding :: String -> TypedTerm SubtermStep
accLetBinding name = Phantoms.inject _SubtermStep _SubtermStep_letBinding (nm name)

accLetBody :: TypedTerm SubtermStep
accLetBody = Phantoms.inject _SubtermStep _SubtermStep_letBody Phantoms.unit

dupBinding :: [TypedTerm SubtermStep] -> String -> TypedTerm (Maybe InvalidTermError)
dupBinding path name = justError $
  Phantoms.inject _InvalidTermError _InvalidTermError_duplicateBinding $
    Phantoms.record _DuplicateBindingError [
      unName _DuplicateBindingError_location Phantoms.>: Phantoms.wrap _SubtermPath (Phantoms.list path),
      unName _DuplicateBindingError_name Phantoms.>: nm name]

dupField :: [TypedTerm SubtermStep] -> String -> TypedTerm (Maybe InvalidTermError)
dupField path name = justError $
  Phantoms.inject _InvalidTermError _InvalidTermError_duplicateField $
    Phantoms.record _DuplicateFieldError [
      unName _DuplicateFieldError_location Phantoms.>: Phantoms.wrap _SubtermPath (Phantoms.list path),
      unName _DuplicateFieldError_name Phantoms.>: nm name]

emptyAnnotErr :: TypedTerm (Maybe InvalidTermError)
emptyAnnotErr = justError $
  Phantoms.inject _InvalidTermError _InvalidTermError_emptyTermAnnotation $
    Phantoms.record _EmptyTermAnnotationError [
      unName _EmptyTermAnnotationError_location Phantoms.>: emptyPath]

emptyCaseErr :: String -> TypedTerm (Maybe InvalidTermError)
emptyCaseErr tname = justError $
  Phantoms.inject _InvalidTermError _InvalidTermError_emptyCaseStatement $
    Phantoms.record _EmptyCaseStatementError [
      unName _EmptyCaseStatementError_location Phantoms.>: emptyPath,
      unName _EmptyCaseStatementError_typeName Phantoms.>: nm tname]

emptyLetErr :: TypedTerm (Maybe InvalidTermError)
emptyLetErr = justError $
  Phantoms.inject _InvalidTermError _InvalidTermError_emptyLetBindings $
    Phantoms.record _EmptyLetBindingsError [
      unName _EmptyLetBindingsError_location Phantoms.>: emptyPath]

emptyTypeNameErr :: TypedTerm (Maybe InvalidTermError)
emptyTypeNameErr = justError $
  Phantoms.inject _InvalidTermError _InvalidTermError_emptyTypeNameInTerm $
    Phantoms.record _EmptyTypeNameInTermError [
      unName _EmptyTypeNameInTermError_location Phantoms.>: emptyPath]

identityAppErr :: TypedTerm (Maybe InvalidTermError)
identityAppErr = justError $
  Phantoms.inject _InvalidTermError _InvalidTermError_unnecessaryIdentityApplication $
    Phantoms.record _UnnecessaryIdentityApplicationError [
      unName _UnnecessaryIdentityApplicationError_location Phantoms.>: emptyPath]

invalidLetNameErr :: String -> TypedTerm (Maybe InvalidTermError)
invalidLetNameErr name = justError $
  Phantoms.inject _InvalidTermError _InvalidTermError_invalidLetBindingName $
    Phantoms.record _InvalidLetBindingNameError [
      unName _InvalidLetBindingNameError_location Phantoms.>: emptyPath,
      unName _InvalidLetBindingNameError_name Phantoms.>: nm name]

invalidNameErr :: String -> TypedTerm (Maybe InvalidTermError)
invalidNameErr name = justError $
  Phantoms.inject _InvalidTermError _InvalidTermError_invalidLambdaParameterName $
    Phantoms.record _InvalidLambdaParameterNameError [
      unName _InvalidLambdaParameterNameError_location Phantoms.>: emptyPath,
      unName _InvalidLambdaParameterNameError_name Phantoms.>: nm name]

nestedAnnotErr :: TypedTerm (Maybe InvalidTermError)
nestedAnnotErr = justError $
  Phantoms.inject _InvalidTermError _InvalidTermError_nestedTermAnnotation $
    Phantoms.record _NestedTermAnnotationError [
      unName _NestedTermAnnotationError_location Phantoms.>: emptyPath]

redundantWrapErr :: String -> TypedTerm (Maybe InvalidTermError)
redundantWrapErr tname = justError $
  Phantoms.inject _InvalidTermError _InvalidTermError_redundantWrapUnwrap $
    Phantoms.record _RedundantWrapUnwrapError [
      unName _RedundantWrapUnwrapError_location Phantoms.>: emptyPath,
      unName _RedundantWrapUnwrapError_typeName Phantoms.>: nm tname]

selfAppErr :: String -> TypedTerm (Maybe InvalidTermError)
selfAppErr name = justError $
  Phantoms.inject _InvalidTermError _InvalidTermError_selfApplication $
    Phantoms.record _SelfApplicationError [
      unName _SelfApplicationError_location Phantoms.>: emptyPath,
      unName _SelfApplicationError_name Phantoms.>: nm name]

shadowErr :: String -> TypedTerm (Maybe InvalidTermError)
shadowErr name = justError $
  Phantoms.inject _InvalidTermError _InvalidTermError_termVariableShadowing $
    Phantoms.record _TermVariableShadowingError [
      unName _TermVariableShadowingError_location Phantoms.>: emptyPath,
      unName _TermVariableShadowingError_name Phantoms.>: nm name]

unknownPrimErr :: String -> TypedTerm (Maybe InvalidTermError)
unknownPrimErr name = justError $
  Phantoms.inject _InvalidTermError _InvalidTermError_unknownPrimitiveName $
    Phantoms.record _UnknownPrimitiveNameError [
      unName _UnknownPrimitiveNameError_location Phantoms.>: emptyPath,
      unName _UnknownPrimitiveNameError_name Phantoms.>: nm name]

-- Term construction helpers

-- DSL-based term construction helpers (raw Haskell constructors break test generation)

allTests :: TypedTermDefinition TestGroup
allTests = define "allTests" $
  Phantoms.doc "Test cases for core term and type validation" $
  supergroup "validate.core" [
    duplicateBindingsTests,
    duplicateFieldsTests,
    emptyLetBindingsTests,
    -- annotationTests,            -- needs raw TermAnnotated constructor (no DSL equivalent for empty annotations)
    -- unknownPrimitiveTests,      -- primitive refs cause type unification with Term
    -- selfApplicationTests,       -- needs unbound variables
    identityApplicationTests,
    profileBehaviourTests,
    -- redundantWrapUnwrapTests,   -- uses unresolvable type names TypeA/TypeB/MyType
    variableShadowingTests]
    -- emptyCaseStatementTests,    -- needs unresolvable type name "MyUnion"
    -- emptyTypeNameTests,         -- needs empty string type names
    -- namingConventionTests       -- needs raw Lambda constructor for empty name

-- ============================================================================
-- T2: Duplicate bindings
-- ============================================================================

-- | Construct an annotated term with a single annotation
annotateTerm :: TypedTerm Term -> String -> TypedTerm Term -> TypedTerm Term
annotateTerm body key val = Phantoms.annot (Name key) Nothing body

duplicateBindingsTests :: TypedTermDefinition TestGroup
duplicateBindingsTests = define "duplicateBindingsTests" $
  subgroup "duplicate bindings" [
    untypedCase "no bindings (literal)"
      (int32 42) noError,
    untypedCase "single binding"
      (lets [(nm "x", int32 1)] (var "x")) noError,
    untypedCase "distinct bindings"
      (lets [(nm "x", int32 1), (nm "y", int32 2)] (var "x")) noError,
    untypedCase "duplicate bindings at top level"
      (lets [(nm "x", int32 1), (nm "x", int32 2)] (var "x"))
      (dupBinding [] "x"),
    untypedCase "duplicate bindings in lambda body"
      (lambda "f" (lets [(nm "a", int32 1), (nm "a", int32 2)] (var "a")))
      (dupBinding [accLambdaBody] "a"),
    untypedCase "duplicate bindings in let body"
      (lets [(nm "x", int32 1)]
        (lets [(nm "y", int32 2), (nm "y", int32 3)] (var "y")))
      (dupBinding [accLetBody] "y"),
    untypedCase "duplicate bindings in let binding value"
      (lets [(nm "x", lets [(nm "a", int32 1), (nm "a", int32 2)] (var "a"))] (var "x"))
      (dupBinding [accLetBinding "x"] "a"),
    untypedCase "same name in different scopes is valid"
      (lets [(nm "x", int32 1)]
        (lets [(nm "x", int32 2)] (var "x")))
      noError]

-- ============================================================================
-- T3/T4: Duplicate fields
-- ============================================================================

duplicateFieldsTests :: TypedTermDefinition TestGroup
duplicateFieldsTests = define "duplicateFieldsTests" $
  subgroup "duplicate fields" [
    untypedCase "no fields (literal)"
      (int32 42) noError,
    untypedCase "distinct record fields"
      (record (nm "Point") [(nm "x", int32 1), (nm "y", int32 2)]) noError,
    untypedCase "duplicate record fields at top level"
      (record (nm "Point") [(nm "x", int32 1), (nm "x", int32 2)])
      (dupField [] "x"),
    untypedCase "duplicate fields in record inside lambda"
      (lambda "f"
        (record (nm "Point") [(nm "x", int32 1), (nm "x", int32 2)]))
      (dupField [accLambdaBody] "x"),
    untypedCase "duplicate fields in record inside let body"
      (lets [(nm "r", int32 0)]
        (record (nm "Point") [(nm "x", int32 1), (nm "x", int32 2)]))
      (dupField [accLetBody] "x")]

-- ============================================================================
-- T1: Empty let bindings
-- ============================================================================

-- | The fully qualified rule name for InvalidTermError.emptyLetBindings.
-- Matches the format produced by 'qualifiedRule' in Validate/Core.hs.
emptyLetBindingsRule :: Name
emptyLetBindingsRule = Name "hydra.error.core.InvalidTermError.emptyLetBindings"

emptyLetBindingsTests :: TypedTermDefinition TestGroup
emptyLetBindingsTests = define "emptyLetBindingsTests" $
  subgroup "empty let bindings" [
    untypedCase "let with bindings is valid"
      (lets [(nm "x", int32 1)] (var "x")) noError,
    -- Raw Term constructors cause test gen failures; use DSL helpers instead
    untypedCase "empty let bindings"
      (lets [] (int32 0))
      emptyLetErr]

-- ============================================================================
-- T5: Empty type name
-- ============================================================================

{- emptyTypeNameTests :: TypedTermDefinition TestGroup
emptyTypeNameTests = define "emptyTypeNameTests" $
  subgroup "empty type name" [
    untypedCase "record with valid type name"
      (record (nm "Point") [(nm "x", int32 1)]) noError,
    untypedCase "record with empty type name"
      (record (nm "") [(nm "x", int32 1)])
      emptyTypeNameErr,
    untypedCase "injection with empty type name"
      (TypedTerm $ TermInject $ Injection (Name "") (Field (Name "x") (TermLiteral $ LiteralInteger $ IntegerValueInt32 1)))
      emptyTypeNameErr,
    untypedCase "projection with empty type name"
      (projTerm "" "x")
      emptyTypeNameErr,
    untypedCase "wrap with empty type name"
      (wrapTerm "" (int32 1))
      emptyTypeNameErr]
-}

-- ============================================================================
-- T6: Empty case statement
-- ============================================================================

{- emptyCaseStatementTests :: TypedTermDefinition TestGroup
emptyCaseStatementTests = define "emptyCaseStatementTests" $
  subgroup "empty case statement" [
    untypedCase "case with branches is valid"
      (caseTerm "MyUnion" Nothing [("x", int32 1)])
      noError,
    untypedCase "case with default only is valid"
      (caseTerm "MyUnion" (Just $ int32 0) [])
      noError,
    untypedCase "empty case with no default"
      (caseTerm "MyUnion" Nothing [])
      (emptyCaseErr "MyUnion")]
-}

-- ============================================================================
-- T20/T21: Annotations
-- ============================================================================

{- annotationTests :: TypedTermDefinition TestGroup
annotationTests = define "annotationTests" $
  subgroup "annotations" [
    untypedCase "annotated term is valid"
      (annotateTerm (int32 42) "key" (string "value"))
      noError,
    untypedCase "empty annotation"
      (emptyAnnotation (int32 42))
      emptyAnnotErr,
    untypedCase "nested annotations"
      (annotateTerm (emptyAnnotation (int32 42)) "key" (string "value"))
      emptyAnnotErr]
-}

-- ============================================================================
-- T22: Unknown primitive
-- ============================================================================

-- | Bare InvalidTermError representing an empty-let-bindings violation at
-- a given subterm path. Differs from 'emptyLetErr' in two ways: returns
-- the bare error (not Maybe), and accepts a path so distinct violations
-- in the same input can be distinguished by location.
emptyLetErrAt :: [TypedTerm SubtermStep] -> TypedTerm InvalidTermError
emptyLetErrAt path = Phantoms.inject _InvalidTermError _InvalidTermError_emptyLetBindings $
  Phantoms.record _EmptyLetBindingsError [
    unName _EmptyLetBindingsError_location Phantoms.>:
      Phantoms.wrap _SubtermPath (Phantoms.list path)]

identityApplicationTests :: TypedTermDefinition TestGroup
identityApplicationTests = define "identityApplicationTests" $
  subgroup "identity application" [
    untypedCase "non-identity lambda application is valid"
      (lambda "x" (int32 1) @@ int32 2)
      noError,
    untypedCase "identity lambda application"
      (lambda "x" (var "x") @@ int32 42)
      identityAppErr]

-- ============================================================================
-- T14: Redundant wrap/unwrap
-- ============================================================================

-- | Construct a primitive function reference
primRef :: String -> TypedTerm Term
primRef name = toTermTerm $ Phantoms.primitive (Name name)

profileBehaviourTests :: TypedTermDefinition TestGroup
profileBehaviourTests = define "profileBehaviourTests" $
  subgroup "profile-aware behaviour" [
    -- Multi-error accumulation: nested empty lets at distinct paths produce
    -- two separate findings when maxErrors is high enough to hold both.
    universalCase "multi-error accumulation: two empty-let errors collected"
      (showValidationResultTerm
        ((((validateCoreTermProfiledRef
          Phantoms.@@ profileWith [emptyLetBindingsRule] [] 5 5)
          Phantoms.@@ Phantoms.boolean False)
          Phantoms.@@ testGraphRef)
          Phantoms.@@ (lets [] (lets [] (int32 0)))))
      (showValidationResultTerm $ resultWith
        [emptyLetErrAt [], emptyLetErrAt [accLetBody]]
        []),
    -- Warning classification: empty-let rule in warningRules.
    -- Expect 0 errors, 2 warnings (one per nested empty let).
    universalCase "warning classification: empty-let demoted to warnings"
      (showValidationResultTerm
        ((((validateCoreTermProfiledRef
          Phantoms.@@ profileWith [] [emptyLetBindingsRule] 5 5)
          Phantoms.@@ Phantoms.boolean False)
          Phantoms.@@ testGraphRef)
          Phantoms.@@ (lets [] (lets [] (int32 0)))))
      (showValidationResultTerm $ resultWith
        []
        [emptyLetErrAt [], emptyLetErrAt [accLetBody]]),
    -- Rule disabling: rule is in neither errorRules nor warningRules, so
    -- the check is skipped entirely. Expect 0 errors, 0 warnings.
    universalCase "rule disabling: empty-let omitted from profile"
      (showValidationResultTerm
        ((((validateCoreTermProfiledRef
          Phantoms.@@ profileWith [] [] 5 5)
          Phantoms.@@ Phantoms.boolean False)
          Phantoms.@@ testGraphRef)
          Phantoms.@@ (lets [] (int32 0))))
      (showValidationResultTerm $ resultWith [] []),
    -- maxErrors bound: same nested-empty-lets input, but maxErrors=1.
    -- Expect only the outer error; the recursion is hard-stopped after
    -- the first error is collected.
    universalCase "maxErrors bound: only first error collected when maxErrors=1"
      (showValidationResultTerm
        ((((validateCoreTermProfiledRef
          Phantoms.@@ profileWith [emptyLetBindingsRule] [] 1 5)
          Phantoms.@@ Phantoms.boolean False)
          Phantoms.@@ testGraphRef)
          Phantoms.@@ (lets [] (lets [] (int32 0)))))
      (showValidationResultTerm $ resultWith
        [emptyLetErrAt []]
        [])]

-- | Build a ValidationProfile from explicit error/warning rule lists and bounds.
profileWith :: [Name] -> [Name] -> Int -> Int -> TypedTerm ValidationProfile
profileWith errs warns mE mW = Validation.validationProfile
  (Sets.fromList $ Phantoms.list $ Phantoms.nameLift <$> errs)
  (Sets.fromList $ Phantoms.list $ Phantoms.nameLift <$> warns)
  (Phantoms.int32 mE)
  (Phantoms.int32 mW)

-- | Build a ValidationResult from explicit error and warning lists.
resultWith
  :: [TypedTerm InvalidTermError]
  -> [TypedTerm InvalidTermError]
  -> TypedTerm (ValidationResult InvalidTermError)
resultWith errs warns = Validation.validationResult
  (Phantoms.list errs) (Phantoms.list warns)

-- | Construct a projection
projTerm :: String -> String -> TypedTerm Term
projTerm tname fname = toTermTerm $ Phantoms.project (Name tname) (Name fname)

-- ============================================================================
-- Test groups
-- ============================================================================

redundantWrapUnwrapTests :: TypedTermDefinition TestGroup
redundantWrapUnwrapTests = define "redundantWrapUnwrapTests" $
  subgroup "redundant wrap unwrap" [
    untypedCase "unwrap of different type is valid"
      (unwrapApply "TypeA" (wrapTerm "TypeB" (int32 1)))
      noError,
    untypedCase "unwrap of same type wrap"
      (unwrapApply "MyType" (wrapTerm "MyType" (int32 1)))
      (redundantWrapErr "MyType")]

-- ============================================================================
-- T11: Variable shadowing
-- ============================================================================

-- | Coerce a phantom-typed term to TypedTerm Term
toTermTerm :: TypedTerm a -> TypedTerm Term
toTermTerm (TypedTerm t) = TypedTerm t

unknownPrimitiveTests :: TypedTermDefinition TestGroup
unknownPrimitiveTests = define "unknownPrimitiveTests" $
  subgroup "unknown primitive" [
    untypedCase "known primitive is valid"
      (primRef "hydra.lib.math.add")
      noError,
    untypedCase "unknown primitive"
      (primRef "hydra.lib.nonexistent.foo")
      (unknownPrimErr "hydra.lib.nonexistent.foo")]

-- ============================================================================
-- T15: Self-application
-- ============================================================================

{- selfApplicationTests :: TypedTermDefinition TestGroup
selfApplicationTests = define "selfApplicationTests" $
  subgroup "self application" [
    untypedCase "normal application is valid"
      (app (var "f") (var "x"))
      noError,
    untypedCase "self-application"
      (app (var "x") (var "x"))
      (selfAppErr "x")]
-}

-- ============================================================================
-- T16: Identity application
-- ============================================================================

-- | Construct an unwrap elimination applied to a term
unwrapApply :: String -> TypedTerm Term -> TypedTerm Term
unwrapApply tname (TypedTerm arg) = TypedTerm $ TermApplication $ Application
  (TermUnwrap (Name tname)) arg

variableShadowingTests :: TypedTermDefinition TestGroup
variableShadowingTests = define "variableShadowingTests" $
  subgroup "variable shadowing" [
    untypedCase "lambda with fresh variable is valid"
      (lambda "x" (var "x"))
      noError,
    -- Note: shadowing detection deferred; needs pre-extension graph access
    untypedCase "lambda shadows outer lambda"
      (lambda "x" (lambda "x" (var "x")))
      noError,
    untypedCase "let binding shadows lambda parameter"
      (lambda "x" (lets [(nm "x", int32 1)] (var "x")))
      noError]

-- ============================================================================
-- T17/T18: Naming conventions
-- ============================================================================

{- namingConventionTests :: TypedTermDefinition TestGroup
namingConventionTests = define "namingConventionTests" $
  subgroup "naming conventions" [
    untypedCase "lambda with valid name"
      (lambda "x" (var "x"))
      noError,
    untypedCase "lambda with empty name"
      (TypedTerm $ TermLambda $ Lambda (Name "") Nothing (TermVariable $ Name "x"))
      (invalidNameErr ""),
    untypedCase "let binding with valid name"
      (lets [(nm "x", int32 1)] (var "x"))
      noError,
    untypedCase "let binding with empty name"
      (TypedTerm $ TermLet $ Let [Binding (Name "") (TermLiteral $ LiteralInteger $ IntegerValueInt32 1) Nothing] (TermVariable $ Name "x"))
      (invalidLetNameErr "")]
-}

-- ============================================================================
-- Profile-aware behaviour tests
--
-- These exercise the post-#320 validator API: an explicit ValidationProfile
-- argument and a ValidationResult return shape. The legacy 'Maybe E'-shaped
-- tests above only see the first error per pass, so they can't observe
-- multi-error accumulation, warning vs error classification, or rule
-- disabling. The cases here drive validateCoreTermProfiledRef directly.
-- ============================================================================

-- | Construct a wrap term
wrapTerm :: String -> TypedTerm Term -> TypedTerm Term
wrapTerm tname = Phantoms.wrap (Name tname)
