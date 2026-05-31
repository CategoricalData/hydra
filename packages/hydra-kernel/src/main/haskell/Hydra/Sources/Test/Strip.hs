{-# LANGUAGE FlexibleContexts #-}

-- | Test cases for annotation and type stripping operations
module Hydra.Sources.Test.Strip where

-- Standard imports for tests
import Hydra.Kernel
import           Hydra.Dsl.Bootstrap (unqualifiedDep, descriptionMetadata)
import Hydra.Dsl.Meta.Testing                 as Testing
import Hydra.Dsl.Meta.Terms                   as Terms hiding ((@@))
import Hydra.Sources.Kernel.Types.All
import qualified Hydra.Dsl.Meta.Core          as Core
import qualified Hydra.Dsl.Meta.Phantoms      as Phantoms
import           Hydra.Dsl.Meta.Phantoms                ((@@))
import qualified Hydra.Dsl.Meta.Types         as T
import qualified Hydra.Sources.Test.TestGraph as TestGraph
import qualified Hydra.Sources.Test.TestTerms as TestTerms
import qualified Hydra.Sources.Test.TestTypes as TestTypes
import qualified Data.List                    as L
import qualified Data.Map                     as M

import Hydra.Testing
import Hydra.Sources.Libraries

import qualified Hydra.Sources.Kernel.Terms.Show.Core as ShowCore
import qualified Hydra.Sources.Kernel.Terms.Strip as Strip


ns :: ModuleName
ns = ModuleName "hydra.test.strip"

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = definitions,
            moduleDependencies = unqualifiedDep <$> ([ShowCore.ns, Strip.ns, TestGraph.ns] ++ kernelTypesModuleNames),
            moduleMetadata = descriptionMetadata ((Just "Test cases for annotation and type stripping operations"))}
  where
    definitions = [Phantoms.toDefinition allTests]

define :: String -> TypedTerm a -> TypedTermDefinition a
define = definitionInModule module_

allTests :: TypedTermDefinition TestGroup
allTests = define "allTests" $
    Phantoms.doc "Test cases for annotation and type stripping operations" $
    supergroup "strip" [
      deannotateTermGroup,
      deannotateTypeGroup]

-- | Convenience helpers for specific kernel functions
deannotateTermCase :: String -> TypedTerm Term -> TypedTerm Term -> TypedTerm TestCaseWithMetadata
deannotateTermCase cname = termCase cname Strip.deannotateTerm

-- | Test cases for deannotating terms (stripping top-level annotations)
-- Note: deannotateTerm only strips annotations at the top level, not recursively
deannotateTermGroup :: TypedTerm TestGroup
deannotateTermGroup = subgroup "deannotateTerm" [
    deannotateTermCase "unannotated literal unchanged"
      (int32 42)
      (int32 42),

    deannotateTermCase "unannotated variable unchanged"
      (var "x")
      (var "x"),

    deannotateTermCase "unannotated lambda unchanged"
      (lambda "x" (var "x"))
      (lambda "x" (var "x")),

    deannotateTermCase "single annotation stripped"
      (annots emptyAnnMap (int32 42))
      (int32 42),

    deannotateTermCase "nested annotations stripped"
      (annots emptyAnnMap (annots emptyAnnMap (int32 42)))
      (int32 42),

    deannotateTermCase "annotated lambda stripped"
      (annots emptyAnnMap (lambda "x" (var "x")))
      (lambda "x" (var "x")),

    deannotateTermCase "annotated application stripped"
      (annots emptyAnnMap (apply (var "f") (var "x")))
      (apply (var "f") (var "x"))]

deannotateTypeCase :: String -> TypedTerm Type -> TypedTerm Type -> TypedTerm TestCaseWithMetadata
deannotateTypeCase cname = typeCase cname Strip.deannotateType

-- | Test cases for deannotating types (stripping top-level annotations)
-- Note: deannotateType only strips annotations at the top level, not recursively
deannotateTypeGroup :: TypedTerm TestGroup
deannotateTypeGroup = subgroup "deannotateType" [
    deannotateTypeCase "unannotated primitive type unchanged"
      T.int32
      T.int32,

    deannotateTypeCase "unannotated string type unchanged"
      T.string
      T.string,

    deannotateTypeCase "unannotated function type unchanged"
      (T.function T.int32 T.string)
      (T.function T.int32 T.string),

    deannotateTypeCase "single annotation stripped"
      (T.annot emptyAnnMap T.int32)
      T.int32,

    deannotateTypeCase "nested annotations stripped"
      (T.annot emptyAnnMap (T.annot emptyAnnMap T.string))
      T.string,

    deannotateTypeCase "annotated list type stripped"
      (T.annot emptyAnnMap (T.list T.int32))
      (T.list T.int32),

    deannotateTypeCase "annotated function type stripped"
      (T.annot emptyAnnMap (T.function T.int32 T.string))
      (T.function T.int32 T.string)]

-- Helper to build an empty annotation map
emptyAnnMap :: TypedTerm (M.Map Name Term)
emptyAnnMap = Phantoms.map M.empty

-- | Show a term as a string using ShowCore.term
showTerm :: TypedTerm Term -> TypedTerm String
showTerm t = ShowCore.term @@ t

-- | Show a type as a string using ShowCore.type_
showType :: TypedTerm Type -> TypedTerm String
showType t = ShowCore.type_ @@ t

-- | Helper for Term -> Term kernel function test cases
termCase :: String -> TypedTermDefinition (Term -> Term) -> TypedTerm Term -> TypedTerm Term -> TypedTerm TestCaseWithMetadata
termCase cname func input output = universalCase cname (showTerm (func @@ input)) (showTerm output)

-- | Helper for Type -> Type kernel function test cases
typeCase :: String -> TypedTermDefinition (Type -> Type) -> TypedTerm Type -> TypedTerm Type -> TypedTerm TestCaseWithMetadata
typeCase cname func input output = universalCase cname (showType (func @@ input)) (showType output)
