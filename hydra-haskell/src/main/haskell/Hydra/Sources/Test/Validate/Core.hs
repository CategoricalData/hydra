
-- | Test cases for core term validation (duplicate bindings, duplicate fields)
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
    []
    kernelTypesNamespaces
    (Just "Test cases for core term validation")
  where
    elements = [
      Phantoms.toBinding allTests,
      Phantoms.toBinding duplicateBindingsTests,
      Phantoms.toBinding duplicateFieldsTests]

define :: String -> TTerm a -> TBinding a
define = definitionInModule module_

-- Helper to build names
nm :: String -> TTerm Name
nm s = Core.name $ Phantoms.string s

-- No error expected
noError :: TTerm (Maybe InvalidTermError)
noError = TTerm $ TermMaybe Nothing

-- Typed just for InvalidTermError
justError :: TTerm InvalidTermError -> TTerm (Maybe InvalidTermError)
justError (TTerm t) = TTerm $ TermMaybe $ Just t

-- Expected: duplicate binding at path with name
dupBinding :: [TTerm TermAccessor] -> String -> TTerm (Maybe InvalidTermError)
dupBinding path name = justError $
  Phantoms.inject _InvalidTermError _InvalidTermError_duplicateBinding $
    Phantoms.record _DuplicateBindingError [
      unName _DuplicateBindingError_location Phantoms.>: Phantoms.wrap _AccessorPath (Phantoms.list path),
      unName _DuplicateBindingError_name Phantoms.>: nm name]

-- Expected: duplicate field at path with name
dupField :: [TTerm TermAccessor] -> String -> TTerm (Maybe InvalidTermError)
dupField path name = justError $
  Phantoms.inject _InvalidTermError _InvalidTermError_duplicateField $
    Phantoms.record _DuplicateFieldError [
      unName _DuplicateFieldError_location Phantoms.>: Phantoms.wrap _AccessorPath (Phantoms.list path),
      unName _DuplicateFieldError_name Phantoms.>: nm name]

-- TermAccessor helpers
accLambdaBody :: TTerm TermAccessor
accLambdaBody = Phantoms.inject _TermAccessor _TermAccessor_lambdaBody Phantoms.unit

accLetBody :: TTerm TermAccessor
accLetBody = Phantoms.inject _TermAccessor _TermAccessor_letBody Phantoms.unit

accLetBinding :: String -> TTerm TermAccessor
accLetBinding name = Phantoms.inject _TermAccessor _TermAccessor_letBinding (nm name)

allTests :: TBinding TestGroup
allTests = define "allTests" $
  Phantoms.doc "Test cases for core term validation" $
  supergroup "validate.core" [
    duplicateBindingsTests,
    duplicateFieldsTests]

-- | Test cases for duplicate binding detection
duplicateBindingsTests :: TBinding TestGroup
duplicateBindingsTests = define "duplicateBindingsTests" $
  subgroup "duplicate bindings" [

    -- Valid terms: no duplicates
    validateCoreTermCase "no bindings (literal)"
      (int32 42) noError,

    validateCoreTermCase "single binding"
      (lets [(nm "x", int32 1)] (var "x")) noError,

    validateCoreTermCase "distinct bindings"
      (lets [(nm "x", int32 1), (nm "y", int32 2)] (var "x")) noError,

    -- Duplicate bindings at top level
    validateCoreTermCase "duplicate bindings at top level"
      (lets [(nm "x", int32 1), (nm "x", int32 2)] (var "x"))
      (dupBinding [] "x"),

    -- Duplicate bindings nested in lambda body
    validateCoreTermCase "duplicate bindings in lambda body"
      (lambda "f" (lets [(nm "a", int32 1), (nm "a", int32 2)] (var "a")))
      (dupBinding [accLambdaBody] "a"),

    -- Duplicate bindings nested in let body
    validateCoreTermCase "duplicate bindings in let body"
      (lets [(nm "x", int32 1)]
        (lets [(nm "y", int32 2), (nm "y", int32 3)] (var "y")))
      (dupBinding [accLetBody] "y"),
    -- Duplicate bindings in a let binding value
    validateCoreTermCase "duplicate bindings in let binding value"
      (lets [(nm "x", lets [(nm "a", int32 1), (nm "a", int32 2)] (var "a"))] (var "x"))
      (dupBinding [accLetBinding "x"] "a"),
    -- No error: same name in different scopes is fine
    validateCoreTermCase "same name in different scopes is valid"
      (lets [(nm "x", int32 1)]
        (lets [(nm "x", int32 2)] (var "x")))
      noError]

-- | Test cases for duplicate field detection
duplicateFieldsTests :: TBinding TestGroup
duplicateFieldsTests = define "duplicateFieldsTests" $
  subgroup "duplicate fields" [
    -- Valid terms: no duplicate fields
    validateCoreTermCase "no fields (literal)"
      (int32 42) noError,

    validateCoreTermCase "distinct record fields"
      (record (nm "Point") [(nm "x", int32 1), (nm "y", int32 2)]) noError,

    -- Duplicate fields in a record
    validateCoreTermCase "duplicate record fields at top level"
      (record (nm "Point") [(nm "x", int32 1), (nm "x", int32 2)])
      (dupField [] "x"),

    -- Duplicate fields in a nested record
    validateCoreTermCase "duplicate fields in record inside lambda"
      (lambda "f"
        (record (nm "Point") [(nm "x", int32 1), (nm "x", int32 2)]))
      (dupField [accLambdaBody] "x"),

    -- Duplicate fields in record inside let body
    validateCoreTermCase "duplicate fields in record inside let body"
      (lets [(nm "r", int32 0)]
        (record (nm "Point") [(nm "x", int32 1), (nm "x", int32 2)]))
      (dupField [accLetBody] "x")]
