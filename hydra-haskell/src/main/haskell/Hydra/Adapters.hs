-- | Entry point for Hydra's adapter (type/term rewriting) framework.
--   An adapter takes a type expression which is supported in a source language, and rewrites it to a type which is supported by a target language.
--   In parallel, terms conforming to the original type are rewritten. Both levels of the transformation are bidirectional.

module Hydra.Adapters where

import Hydra.TermAdapters
import Hydra.Printing
import Hydra.Coders
import Hydra.Compute
import Hydra.Core
import Hydra.CoreDecoding
import Hydra.CoreLanguage
import Hydra.Graph
import Hydra.Lexical
import Hydra.Mantle
import Hydra.Module
import Hydra.Flows
import Hydra.TermAdapters
import Hydra.AdapterUtils
import Hydra.Reduction
import Hydra.Tier1
import Hydra.Tier2

import qualified Control.Monad as CM
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S


-- | Given a target language and a source type, find the target type to which the latter will be adapted.
adaptType :: (Ord a, Read a, Show a) => Language a -> Type a -> GraphFlow a (Type a)
adaptType lang typ = adapterTarget <$> languageAdapter lang typ

-- | Given a target language, a unidirectional last-mile encoding, and a source type,
--   construct a unidirectional adapting coder for terms of that type. Terms will be rewritten according to the type and
--   according to the constraints of the target language, then carried by the last mile into the final representation
constructCoder :: (Ord a, Read a, Show a)
  => Language a
  -> (Term a -> GraphFlow a c)
  -> Type a
  -> GraphFlow a (Coder (Graph a) (Graph a) (Term a) c)
constructCoder lang encodeTerm typ = withTrace ("coder for " ++ describeType typ) $ do
    adapter <- languageAdapter lang typ
    return $ composeCoders (adapterCoder adapter) (unidirectionalCoder encodeTerm)

-- | Given a target language and a source type, produce an adapter,
--   which rewrites the type and its terms according to the language's constraints
languageAdapter :: (Ord a, Read a, Show a)
  => Language a -> Type a -> GraphFlow a (SymmetricAdapter (Graph a) (Type a) (Term a))
languageAdapter lang typ0 = do
  -- TODO: rather than beta-reducing types all at once, we should incrementally extend the environment when application types are adapted
  -- typ <- betaReduceType typ0
  let typ = typ0

  g  <- getState
  -- Provide an initial adapter context
  let cx0 = AdapterContext g lang M.empty
  -- Construct the term adapter, and capture the populated adapter context
  (adapter, cx) <- withState cx0 $ do
    ad <- termAdapter typ
    cx <- getState -- The state has been mutated to hold adapters for type elements
    return (ad, cx)
  -- Wrap terms in the adapter context as they pass through the adapter coder
  let ac = Coder encode decode
        where
          encode = withState cx . coderEncode (adapterCoder adapter)
          decode = withState cx . coderDecode (adapterCoder adapter)
  return $ adapter {adapterCoder = ac}

-- | Given a target language, a unidirectional last mile encoding, and an intermediate helper function,
--   transform a given module into a target representation
transformModule :: (Ord a, Read a, Show a)
  => Language a
  -> (Term a -> GraphFlow a e)
  -> (Module a -> M.Map (Type a) (Coder (Graph a) (Graph a) (Term a) e) -> [(Element a, TypedTerm a)] -> GraphFlow a d)
  -> Module a -> GraphFlow a d
transformModule lang encodeTerm createModule mod = withTrace ("transform module " ++ unNamespace (moduleNamespace mod)) $ do
    pairs <- withSchemaContext $ CM.mapM elementAsTypedTerm els
    let types = L.nub (typedTermType <$> pairs)
    coders <- codersFor types
    createModule mod coders $ L.zip els pairs
  where
    els = moduleElements mod

    codersFor types = do
      cdrs <- CM.mapM (constructCoder lang encodeTerm) types
      return $ M.fromList $ L.zip types cdrs
