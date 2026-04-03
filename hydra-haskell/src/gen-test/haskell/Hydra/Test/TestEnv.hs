-- | Test environment providing the test graph and context with primitives.
-- This is a hand-written bridge module referenced by the generated TestGraph.
-- It should NOT be overwritten by code generation.

module Hydra.Test.TestEnv (testGraph, testContext) where

import Hydra.Kernel
import Hydra.Sources.Libraries (standardLibraries)
import Hydra.Json.Bootstrap (typesByName)
import qualified Hydra.Sources.Kernel.Terms.Annotations as TermAnnotations
import qualified Hydra.Sources.Kernel.Terms.Constants as TermConstants
import qualified Hydra.Sources.Kernel.Terms.Dependencies as TermDependencies
import qualified Hydra.Sources.Kernel.Terms.Extract.Core as TermExtractCore
import qualified Hydra.Sources.Kernel.Terms.Lexical as TermLexical
import qualified Hydra.Sources.Kernel.Terms.Rewriting as TermRewriting
import qualified Hydra.Sources.Kernel.Terms.Scoping as TermScoping
import qualified Hydra.Sources.Kernel.Terms.Show.Core as TermShowCore
import qualified Hydra.Sources.Kernel.Terms.Strip as TermStrip
import qualified Hydra.Sources.Kernel.Terms.Variables as TermVariables
import qualified Hydra.Decode.Core as TermDecodeCore
import qualified Hydra.Encode.Core as TermEncodeCore
import qualified Data.List as L
import qualified Data.Map as M

-- | Convert a Type to a TypeScheme, extracting forall-bound variables.
-- Also strips top-level annotations.
typeToScheme :: Type -> TypeScheme
typeToScheme = go []
  where
    go vars (TypeForall (ForallType var body)) = go (vars ++ [var]) body
    go vars (TypeAnnotated (AnnotatedType body _)) = go vars body
    go vars body = TypeScheme vars body Nothing

-- | The test graph with primitives, schema types, and kernel term bindings.
testGraph :: M.Map Name Type -> Graph
testGraph testTypes = let
    allPrims = L.concatMap libraryPrimitives standardLibraries
    primsMap = M.fromList $ fmap (\p -> (primitiveName p, p)) allPrims
    kernelSchemas = M.map typeToScheme typesByName
    testSchemas = M.map typeToScheme testTypes
    allSchemas = M.union kernelSchemas testSchemas
    -- Kernel term bindings needed for reduceTerm to evaluate kernel functions
    kernelTermBindings = L.concat $ fmap moduleBindings
      [ TermAnnotations.module_
      , TermConstants.module_
      , TermDependencies.module_
      , TermExtractCore.module_
      , TermLexical.module_
      , TermRewriting.module_
      , TermScoping.module_
      , TermShowCore.module_
      , TermStrip.module_
      , TermVariables.module_
      ]
    boundTerms = M.fromList $ fmap (\b -> (bindingName b, bindingTerm b)) kernelTermBindings
    boundTypes = M.fromList
      [ (bindingName b, ts) | b <- kernelTermBindings, Just ts <- [bindingType b] ]
    base = emptyGraph
  in base {
    graphPrimitives = primsMap,
    graphSchemaTypes = allSchemas,
    graphBoundTerms = boundTerms,
    graphBoundTypes = boundTypes }

-- | The test context (empty).
testContext :: Context
testContext = emptyContext
