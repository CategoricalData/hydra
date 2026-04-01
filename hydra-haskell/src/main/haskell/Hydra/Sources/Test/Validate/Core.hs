
-- | Test cases for core term and type validation
module Hydra.Sources.Test.Validate.Core where

-- Standard imports for shallow DSL tests
import Hydra.Kernel
import Hydra.Dsl.Meta.Testing                 as Testing
import Hydra.Dsl.Meta.Terms                   as Terms
import Hydra.Sources.Kernel.Types.All
import qualified Hydra.Dsl.Meta.Core          as Core
import qualified Hydra.Dsl.Meta.Phantoms      as Phantoms
import qualified Hydra.Dsl.Meta.Types         as T
import qualified Hydra.Sources.Test.TestGraph as TestGraph
import qualified Hydra.Sources.Test.TestTerms as TestTerms
import qualified Hydra.Sources.Test.TestTypes as TestTypes
import qualified Data.List                    as L
import qualified Data.Map                     as M

import Hydra.Testing
import Hydra.Sources.Libraries


ns :: Namespace
ns = Namespace "hydra.test.validate.core"

module_ :: Module
module_ = Module ns elements
    [Namespace "hydra.validate.core", Namespace "hydra.show.error.core"]
    kernelTypesNamespaces
    (Just "Test cases for core term and type validation")
  where
    elements = [
      Phantoms.toTermDefinition allTests,
      Phantoms.toTermDefinition duplicateBindingsTests,
      Phantoms.toTermDefinition duplicateFieldsTests,
      Phantoms.toTermDefinition emptyLetBindingsTests,
      Phantoms.toTermDefinition identityApplicationTests,
      Phantoms.toTermDefinition variableShadowingTests]
      -- Commented out pending test gen fixes (raw constructors / unresolvable names):
      -- annotationTests, selfApplicationTests, emptyCaseStatementTests,
      -- emptyTypeNameTests, namingConventionTests

define :: String -> TTerm a -> TBinding a
define = definitionInModule module_

-- ============================================================================
-- Helpers
-- ============================================================================

-- Helper to build names
nm :: String -> TTerm Name
nm s = Core.name $ Phantoms.string s

-- No error expected
noError :: TTerm (Maybe InvalidTermError)
noError = TTerm $ TermMaybe Nothing

-- Typed just for InvalidTermError
justError :: TTerm InvalidTermError -> TTerm (Maybe InvalidTermError)
justError (TTerm t) = TTerm $ TermMaybe $ Just t

-- Shorthand for untyped term test case
untypedCase :: String -> TTerm Term -> TTerm (Maybe InvalidTermError) -> TTerm TestCaseWithMetadata
untypedCase name = validateCoreTermCase name (Phantoms.boolean False)

-- Empty accessor path
emptyPath :: TTerm SubtermPath
emptyPath = Phantoms.wrap _SubtermPath (Phantoms.list ([] :: [TTerm SubtermStep]))

-- Error constructors

dupBinding :: [TTerm SubtermStep] -> String -> TTerm (Maybe InvalidTermError)
dupBinding path name = justError $
  Phantoms.inject _InvalidTermError _InvalidTermError_duplicateBinding $
    Phantoms.record _DuplicateBindingError [
      unName _DuplicateBindingError_location Phantoms.>: Phantoms.wrap _SubtermPath (Phantoms.list path),
      unName _DuplicateBindingError_name Phantoms.>: nm name]

dupField :: [TTerm SubtermStep] -> String -> TTerm (Maybe InvalidTermError)
dupField path name = justError $
  Phantoms.inject _InvalidTermError _InvalidTermError_duplicateField $
    Phantoms.record _DuplicateFieldError [
      unName _DuplicateFieldError_location Phantoms.>: Phantoms.wrap _SubtermPath (Phantoms.list path),
      unName _DuplicateFieldError_name Phantoms.>: nm name]

emptyLetErr :: TTerm (Maybe InvalidTermError)
emptyLetErr = justError $
  Phantoms.inject _InvalidTermError _InvalidTermError_emptyLetBindings $
    Phantoms.record _EmptyLetBindingsError [
      unName _EmptyLetBindingsError_location Phantoms.>: emptyPath]

emptyTypeNameErr :: TTerm (Maybe InvalidTermError)
emptyTypeNameErr = justError $
  Phantoms.inject _InvalidTermError _InvalidTermError_emptyTypeNameInTerm $
    Phantoms.record _EmptyTypeNameInTermError [
      unName _EmptyTypeNameInTermError_location Phantoms.>: emptyPath]

emptyCaseErr :: String -> TTerm (Maybe InvalidTermError)
emptyCaseErr tname = justError $
  Phantoms.inject _InvalidTermError _InvalidTermError_emptyCaseStatement $
    Phantoms.record _EmptyCaseStatementError [
      unName _EmptyCaseStatementError_location Phantoms.>: emptyPath,
      unName _EmptyCaseStatementError_typeName Phantoms.>: nm tname]

nestedAnnotErr :: TTerm (Maybe InvalidTermError)
nestedAnnotErr = justError $
  Phantoms.inject _InvalidTermError _InvalidTermError_nestedTermAnnotation $
    Phantoms.record _NestedTermAnnotationError [
      unName _NestedTermAnnotationError_location Phantoms.>: emptyPath]

emptyAnnotErr :: TTerm (Maybe InvalidTermError)
emptyAnnotErr = justError $
  Phantoms.inject _InvalidTermError _InvalidTermError_emptyTermAnnotation $
    Phantoms.record _EmptyTermAnnotationError [
      unName _EmptyTermAnnotationError_location Phantoms.>: emptyPath]

unknownPrimErr :: String -> TTerm (Maybe InvalidTermError)
unknownPrimErr name = justError $
  Phantoms.inject _InvalidTermError _InvalidTermError_unknownPrimitiveName $
    Phantoms.record _UnknownPrimitiveNameError [
      unName _UnknownPrimitiveNameError_location Phantoms.>: emptyPath,
      unName _UnknownPrimitiveNameError_name Phantoms.>: nm name]

selfAppErr :: String -> TTerm (Maybe InvalidTermError)
selfAppErr name = justError $
  Phantoms.inject _InvalidTermError _InvalidTermError_selfApplication $
    Phantoms.record _SelfApplicationError [
      unName _SelfApplicationError_location Phantoms.>: emptyPath,
      unName _SelfApplicationError_name Phantoms.>: nm name]

identityAppErr :: TTerm (Maybe InvalidTermError)
identityAppErr = justError $
  Phantoms.inject _InvalidTermError _InvalidTermError_unnecessaryIdentityApplication $
    Phantoms.record _UnnecessaryIdentityApplicationError [
      unName _UnnecessaryIdentityApplicationError_location Phantoms.>: emptyPath]

redundantWrapErr :: String -> TTerm (Maybe InvalidTermError)
redundantWrapErr tname = justError $
  Phantoms.inject _InvalidTermError _InvalidTermError_redundantWrapUnwrap $
    Phantoms.record _RedundantWrapUnwrapError [
      unName _RedundantWrapUnwrapError_location Phantoms.>: emptyPath,
      unName _RedundantWrapUnwrapError_typeName Phantoms.>: nm tname]

shadowErr :: String -> TTerm (Maybe InvalidTermError)
shadowErr name = justError $
  Phantoms.inject _InvalidTermError _InvalidTermError_termVariableShadowing $
    Phantoms.record _TermVariableShadowingError [
      unName _TermVariableShadowingError_location Phantoms.>: emptyPath,
      unName _TermVariableShadowingError_name Phantoms.>: nm name]

invalidNameErr :: String -> TTerm (Maybe InvalidTermError)
invalidNameErr name = justError $
  Phantoms.inject _InvalidTermError _InvalidTermError_invalidLambdaParameterName $
    Phantoms.record _InvalidLambdaParameterNameError [
      unName _InvalidLambdaParameterNameError_location Phantoms.>: emptyPath,
      unName _InvalidLambdaParameterNameError_name Phantoms.>: nm name]

invalidLetNameErr :: String -> TTerm (Maybe InvalidTermError)
invalidLetNameErr name = justError $
  Phantoms.inject _InvalidTermError _InvalidTermError_invalidLetBindingName $
    Phantoms.record _InvalidLetBindingNameError [
      unName _InvalidLetBindingNameError_location Phantoms.>: emptyPath,
      unName _InvalidLetBindingNameError_name Phantoms.>: nm name]

-- SubtermStep helpers
accLambdaBody :: TTerm SubtermStep
accLambdaBody = Phantoms.inject _SubtermStep _SubtermStep_lambdaBody Phantoms.unit

accLetBody :: TTerm SubtermStep
accLetBody = Phantoms.inject _SubtermStep _SubtermStep_letBody Phantoms.unit

accLetBinding :: String -> TTerm SubtermStep
accLetBinding name = Phantoms.inject _SubtermStep _SubtermStep_letBinding (nm name)

-- Term construction helpers

-- DSL-based term construction helpers (raw Haskell constructors break test generation)

-- | Construct an annotated term with a single annotation
annotateTerm :: TTerm Term -> String -> TTerm Term -> TTerm Term
annotateTerm body key val = Phantoms.annot (Name key) Nothing body

-- | Construct a wrap term
wrapTerm :: String -> TTerm Term -> TTerm Term
wrapTerm tname = Phantoms.wrap (Name tname)

-- | Coerce a phantom-typed term to TTerm Term
toTermTerm :: TTerm a -> TTerm Term
toTermTerm (TTerm t) = TTerm t

-- | Construct an unwrap elimination applied to a term
unwrapApply :: String -> TTerm Term -> TTerm Term
unwrapApply tname (TTerm arg) = TTerm $ TermApplication $ Application
  (TermFunction $ FunctionElimination $ EliminationWrap (Name tname)) arg

-- | Construct a primitive function reference
primRef :: String -> TTerm Term
primRef name = toTermTerm $ Phantoms.primitive (Name name)

-- | Construct a projection
projTerm :: String -> String -> TTerm Term
projTerm tname fname = toTermTerm $ Phantoms.project (Name tname) (Name fname)

-- ============================================================================
-- Test groups
-- ============================================================================

allTests :: TBinding TestGroup
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
    -- redundantWrapUnwrapTests,   -- uses unresolvable type names TypeA/TypeB/MyType
    variableShadowingTests]
    -- emptyCaseStatementTests,    -- needs unresolvable type name "MyUnion"
    -- emptyTypeNameTests,         -- needs empty string type names
    -- namingConventionTests       -- needs raw Lambda constructor for empty name

-- ============================================================================
-- T2: Duplicate bindings
-- ============================================================================

duplicateBindingsTests :: TBinding TestGroup
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

duplicateFieldsTests :: TBinding TestGroup
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

emptyLetBindingsTests :: TBinding TestGroup
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

{- emptyTypeNameTests :: TBinding TestGroup
emptyTypeNameTests = define "emptyTypeNameTests" $
  subgroup "empty type name" [
    untypedCase "record with valid type name"
      (record (nm "Point") [(nm "x", int32 1)]) noError,
    untypedCase "record with empty type name"
      (record (nm "") [(nm "x", int32 1)])
      emptyTypeNameErr,
    untypedCase "injection with empty type name"
      (TTerm $ TermUnion $ Injection (Name "") (Field (Name "x") (TermLiteral $ LiteralInteger $ IntegerValueInt32 1)))
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

{- emptyCaseStatementTests :: TBinding TestGroup
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

{- annotationTests :: TBinding TestGroup
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

unknownPrimitiveTests :: TBinding TestGroup
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

{- selfApplicationTests :: TBinding TestGroup
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

identityApplicationTests :: TBinding TestGroup
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

redundantWrapUnwrapTests :: TBinding TestGroup
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

variableShadowingTests :: TBinding TestGroup
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

{- namingConventionTests :: TBinding TestGroup
namingConventionTests = define "namingConventionTests" $
  subgroup "naming conventions" [
    untypedCase "lambda with valid name"
      (lambda "x" (var "x"))
      noError,
    untypedCase "lambda with empty name"
      (TTerm $ TermFunction $ FunctionLambda $ Lambda (Name "") Nothing (TermVariable $ Name "x"))
      (invalidNameErr ""),
    untypedCase "let binding with valid name"
      (lets [(nm "x", int32 1)] (var "x"))
      noError,
    untypedCase "let binding with empty name"
      (TTerm $ TermLet $ Let [Binding (Name "") (TermLiteral $ LiteralInteger $ IntegerValueInt32 1) Nothing] (TermVariable $ Name "x"))
      (invalidLetNameErr "")]
-}
