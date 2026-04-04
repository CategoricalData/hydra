{-# LANGUAGE FlexibleContexts #-}

-- | Test cases for annotation and type stripping operations
module Hydra.Sources.Test.Strip where

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

import qualified Hydra.Sources.Kernel.Terms.Show.Core as ShowCore
import qualified Hydra.Sources.Kernel.Terms.Strip as StripModule


ns :: Namespace
ns = Namespace "hydra.test.strip"

module_ :: Module
module_ = Module ns elements
    [ShowCore.ns, StripModule.ns, TestGraph.ns]
    kernelTypesNamespaces
    (Just "Test cases for annotation and type stripping operations")
  where
    elements = [Phantoms.toDefinition allTests]

define :: String -> TTerm a -> TTermDefinition a
define = definitionInModule module_

-- Local alias for polymorphic application (Phantoms.@@ applies TBindings; Terms.@@ only works on TTerm Term)
(#) :: (AsTerm f (a -> b), AsTerm g a) => f -> g -> TTerm b
(#) = (Phantoms.@@)
infixl 1 #

-- | Show a term as a string using ShowCore.term
showTerm :: TTerm Term -> TTerm String
showTerm t = ShowCore.term # t

-- | Show a type as a string using ShowCore.type_
showType :: TTerm Type -> TTerm String
showType t = ShowCore.type_ # t

-- | Helper for Term -> Term kernel function test cases
termCase :: String -> TTermDefinition (Term -> Term) -> TTerm Term -> TTerm Term -> TTerm TestCaseWithMetadata
termCase cname func input output = universalCase cname (showTerm (func # input)) (showTerm output)

-- | Helper for Type -> Type kernel function test cases
typeCase :: String -> TTermDefinition (Type -> Type) -> TTerm Type -> TTerm Type -> TTerm TestCaseWithMetadata
typeCase cname func input output = universalCase cname (showType (func # input)) (showType output)

-- | Convenience helpers for specific kernel functions
deannotateTermCase :: String -> TTerm Term -> TTerm Term -> TTerm TestCaseWithMetadata
deannotateTermCase cname = termCase cname StripModule.deannotateTerm

deannotateTypeCase :: String -> TTerm Type -> TTerm Type -> TTerm TestCaseWithMetadata
deannotateTypeCase cname = typeCase cname StripModule.deannotateType

-- Helper to build an empty annotation map
emptyAnnMap :: TTerm (M.Map Name Term)
emptyAnnMap = Phantoms.map M.empty

-- | Test cases for deannotating terms (stripping top-level annotations)
-- Note: deannotateTerm only strips annotations at the top level, not recursively
deannotateTermGroup :: TTerm TestGroup
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
      (annot emptyAnnMap (int32 42))
      (int32 42),

    deannotateTermCase "nested annotations stripped"
      (annot emptyAnnMap (annot emptyAnnMap (int32 42)))
      (int32 42),

    deannotateTermCase "annotated lambda stripped"
      (annot emptyAnnMap (lambda "x" (var "x")))
      (lambda "x" (var "x")),

    deannotateTermCase "annotated application stripped"
      (annot emptyAnnMap (apply (var "f") (var "x")))
      (apply (var "f") (var "x"))]

-- | Test cases for deannotating types (stripping top-level annotations)
-- Note: deannotateType only strips annotations at the top level, not recursively
deannotateTypeGroup :: TTerm TestGroup
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

allTests :: TTermDefinition TestGroup
allTests = define "allTests" $
    Phantoms.doc "Test cases for annotation and type stripping operations" $
    supergroup "strip" [
      deannotateTermGroup,
      deannotateTypeGroup]
