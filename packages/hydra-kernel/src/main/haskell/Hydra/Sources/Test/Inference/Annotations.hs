module Hydra.Sources.Test.Inference.Annotations where

-- Standard imports for term-encoded tests
import Hydra.Kernel
import           Hydra.Overlay.Haskell.Bootstrap (unqualifiedDep, descriptionMetadata)
import Hydra.Overlay.Haskell.Dsl.Typed.Testing                 as Testing
import Hydra.Overlay.Haskell.Dsl.Typed.Terms                   as Terms
import Hydra.Sources.Kernel.Types.All
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Core          as Core
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Phantoms      as Phantoms
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Types         as T
import qualified Hydra.Sources.Test.TestGraph as TestGraph
import qualified Hydra.Lib.Math as DefMath


ns :: ModuleName
ns = ModuleName "hydra.test.inference.annotations"

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = definitions,
            moduleDependencies = unqualifiedDep <$> ([TestGraph.ns, ModuleName "hydra.inference", ModuleName "hydra.print.core"] ++ kernelTypesModuleNames),
            moduleMetadata = descriptionMetadata (Just "Inference tests for annotated terms")}
  where
    definitions = [
      Phantoms.toDefinition allTests,
      Phantoms.toDefinition transparencyTests,
      Phantoms.toDefinition failureTests]

define :: String -> TypedTerm a -> TypedTermDefinition a
define = definitionInModule module_

-- | Build a raw TermAnnotated (not schema-encoded) so inference sees a real AnnotatedTerm,
-- not a TermInject{annotated=...} which would trigger checkTypeSubst failures.
rawAnnotatedTerm :: TypedTerm Term -> TypedTerm Term -> TypedTerm Term
rawAnnotatedTerm body ann = Core.termAnnotated $ Core.annotatedTerm body ann

allTests :: TypedTermDefinition TestGroup
allTests = define "allTests" $
  Phantoms.doc "Inference tests for annotated terms" $
  supergroup "Annotated terms" [
    transparencyTests,
    failureTests]

-- | The inferred type of an AnnotatedTerm is always the type of its body;
-- the annotation is passed through unchanged and does not affect inference.
-- Tests cover both map-term annotations (the conventional form) and
-- arbitrary term annotations.
transparencyTests :: TypedTermDefinition TestGroup
transparencyTests = define "transparencyTests" $
  supergroup "Annotation is transparent to the inferred type" [
    subgroup "Arbitrary-term annotation" [
      -- The simplest case: annotation is an unrelated literal
      -- rawAnnotatedTerm (int32 42) (string "hello") :: int32
      expectMono 1 []
        (rawAnnotatedTerm (int32 42) (string "hello"))
        T.int32,
      -- rawAnnotatedTerm (string "foo") (int32 0) :: string
      expectMono 2 []
        (rawAnnotatedTerm (string "foo") (int32 0))
        T.string,
      -- Polymorphic body is unaffected by annotation
      -- rawAnnotatedTerm (\x -> x) (int32 0) :: forall t0. t0 -> t0
      expectPoly 3 []
        (rawAnnotatedTerm (lambda "x" $ var "x") (int32 0))
        ["t0"] (T.function (T.var "t0") (T.var "t0"))],
    subgroup "Map-term annotation (conventional form)" [
      -- Empty map annotation on a literal: the conventional annotation encoding
      -- rawAnnotatedTerm (int32 42) (map {}) :: int32
      expectMono 1 []
        (rawAnnotatedTerm (int32 42) (mapTerm []))
        T.int32,
      -- Non-empty map annotation: a string-keyed map with a string value
      -- rawAnnotatedTerm (int32 42) (map {"description": "the answer"}) :: int32
      expectMono 2 []
        (rawAnnotatedTerm (int32 42) (mapTerm [(string "description", string "the answer")]))
        T.int32],
    subgroup "Nested annotations" [
      -- rawAnnotatedTerm (rawAnnotatedTerm (int32 42) (string "inner")) (string "outer") :: int32
      expectMono 1 []
        (rawAnnotatedTerm (rawAnnotatedTerm (int32 42) (string "inner")) (string "outer"))
        T.int32,
      -- Nested with empty-map annotations (conventional form)
      expectMono 2 []
        (rawAnnotatedTerm (rawAnnotatedTerm (int32 42) (mapTerm [])) (mapTerm []))
        T.int32]]

-- | Inference errors in the body propagate; errors in the annotation are
-- not checked (the annotation is passed through unchanged).
failureTests :: TypedTermDefinition TestGroup
failureTests = define "failureTests" $
  supergroup "Errors in the body propagate" [
    subgroup "Type error in body propagates" [
      -- rawAnnotatedTerm (add 1 "x") 0 -> failure (body: int32 vs. string)
      expectFailure 1 []
        (rawAnnotatedTerm (primitive DefMath.add @@ int32 1 @@ string "x") (int32 0))]]
