-- | A variant of the test graph that exercises default primitive implementations.
-- For each primitive whose PrimitiveDefinition carries a defaultImplementation term,
-- the native Haskell implementation is replaced by a reducer-based fallback that
-- applies the default implementation term to its arguments via reduceTerm.
-- Primitives without a defaultImplementation retain their native implementations.
--
-- This module is hand-written (not generated) and should NOT be overwritten by
-- code generation.

module Hydra.Test.DefaultImplGraph (defaultImplGraph, defaultImplPrimCount) where

import Hydra.Kernel
import Hydra.Reduction (reduceTerm)
import Hydra.Overlay.Haskell.Libraries (standardLibraries)
import qualified Hydra.Test.TestEnv as TestEnv

import qualified Hydra.Sources.Kernel.Lib.Chars     as LibChars
import qualified Hydra.Sources.Kernel.Lib.Effects   as LibEffects
import qualified Hydra.Sources.Kernel.Lib.Eithers   as LibEithers
import qualified Hydra.Sources.Kernel.Lib.Equality  as LibEquality
import qualified Hydra.Sources.Kernel.Lib.Files     as LibFiles
import qualified Hydra.Sources.Kernel.Lib.Lists     as LibLists
import qualified Hydra.Sources.Kernel.Lib.Literals  as LibLiterals
import qualified Hydra.Sources.Kernel.Lib.Logic     as LibLogic
import qualified Hydra.Sources.Kernel.Lib.Maps      as LibMaps
import qualified Hydra.Sources.Kernel.Lib.Math      as LibMath
import qualified Hydra.Sources.Kernel.Lib.Optionals as LibOptionals
import qualified Hydra.Sources.Kernel.Lib.Pairs     as LibPairs
import qualified Hydra.Sources.Kernel.Lib.Regex     as LibRegex
import qualified Hydra.Sources.Kernel.Lib.Sets      as LibSets
import qualified Hydra.Sources.Kernel.Lib.Strings   as LibStrings
import qualified Hydra.Sources.Kernel.Lib.System    as LibSystem
import qualified Hydra.Sources.Kernel.Lib.Text      as LibText

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Maybe as Y


-- | Build a map from primitive Name to PrimitiveDefinition from all DSL lib modules.
-- Only definitions with a Just defaultImplementation are relevant, but we build
-- the full map to keep it simple.
dslPrimDefsMap :: M.Map Name PrimitiveDefinition
dslPrimDefsMap = M.fromList
    [ (primitiveDefinitionName pd, pd)
    | mod_ <- allLibModules
    , DefinitionPrimitive pd <- moduleDefinitions mod_
    ]
  where
    allLibModules =
      [ LibChars.module_
      , LibEffects.module_
      , LibEithers.module_
      , LibEquality.module_
      , LibFiles.module_
      , LibLists.module_
      , LibLiterals.module_
      , LibLogic.module_
      , LibMaps.module_
      , LibMath.module_
      , LibOptionals.module_
      , LibPairs.module_
      , LibRegex.module_
      , LibSets.module_
      , LibStrings.module_
      , LibSystem.module_
      , LibText.module_
      ]

-- | Given a Graph (the standard test graph, used as the native-primitive fallback),
-- build the default-impl implementation function for a primitive whose
-- defaultImplementation term is `implTerm`.
--
-- Strategy: apply `implTerm` to each argument using TermApplication, then reduce
-- the whole expression via reduceTerm with the provided graph.
defaultImplFunc :: Graph -> Term -> Graph -> [Term] -> Either Error Term
defaultImplFunc fallbackGraph implTerm _g args =
    reduceTerm emptyInferenceContext fallbackGraph True applied
  where
    applied = L.foldl (\f a -> TermApplication (Application f a)) implTerm args

-- | Count of primitives that have a defaultImplementation in the DSL sources.
defaultImplPrimCount :: Int
defaultImplPrimCount = M.size $ M.filter (Y.isJust . primitiveDefinitionDefaultImplementation) dslPrimDefsMap

-- | Build the default-impl test graph. Replaces the implementation of every
-- primitive that carries a defaultImplementation in the DSL source with a
-- reducer-based fallback. Primitives without a defaultImplementation keep
-- their native Haskell implementations (so the graph is complete and can
-- resolve cross-primitive calls).
--
-- The testTypes and testTerms arguments match the TestEnv.testGraph signature.
defaultImplGraph :: M.Map Name Type -> M.Map Name Term -> Graph
defaultImplGraph testTypes testTerms =
    nativeGraph { graphPrimitives = patchedPrims }
  where
    nativeGraph = TestEnv.testGraph testTypes testTerms
    allPrims = M.elems (graphPrimitives nativeGraph)

    -- For each native Primitive, check if the DSL has a defaultImplementation.
    -- If so, replace the implementation; otherwise keep the native one.
    patchedPrims = M.fromList
      [ (primitiveDefinitionName (primitiveDefinition p), patchPrim p)
      | p <- allPrims
      ]

    patchPrim :: Primitive -> Primitive
    patchPrim p =
      let name = primitiveDefinitionName (primitiveDefinition p)
      in case M.lookup name dslPrimDefsMap >>= primitiveDefinitionDefaultImplementation of
        Nothing -> p
        Just implTerm ->
          p { primitiveImplementation = defaultImplFunc nativeGraph implTerm }
