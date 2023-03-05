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
import Hydra.Monads
import Hydra.TermAdapters
import Hydra.AdapterUtils

import qualified Control.Monad as CM
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S


adaptType :: (Ord a, Read a, Show a) => Language a -> Type a -> GraphFlow a (Type a)
adaptType targetLang t = do
    g <- getState
    let acx = AdapterContext g hydraCoreLanguage targetLang
    ad <- withState acx $ termAdapter t
    return $ adapterTarget ad

constructCoder :: (Ord a, Read a, Show a)
  => Language a
  -> (Term a -> GraphFlow a c)
  -> Type a
  -> GraphFlow a (Coder (Graph a) (Graph a) (Term a) c)
constructCoder lang encodeTerm typ = withTrace ("coder for " ++ describeType typ) $ do
    g <- getState
    let acx = AdapterContext g hydraCoreLanguage lang
    adapter <- withState acx $ termAdapter typ
    coder <- termCoder $ adapterTarget adapter
    return $ composeCoders (adapterCoder adapter) coder
  where
    termCoder _ = pure $ unidirectionalCoder encodeTerm

transformModule :: (Ord a, Read a, Show a)
  => Language a
  -> (Term a -> GraphFlow a e)
  -> (Module a -> M.Map (Type a) (Coder (Graph a) (Graph a) (Term a) e) -> [(Element a, TypedTerm a)] -> GraphFlow a d)
  -> Module a -> GraphFlow a d
transformModule lang encodeTerm createModule mod = do
    pairs <- withSchemaContext $ CM.mapM elementAsTypedTerm els
    let types = L.nub (typedTermType <$> pairs)
    coders <- codersFor types
    createModule mod coders $ L.zip els pairs
  where
    els = moduleElements mod

    codersFor types = do
      cdrs <- CM.mapM (constructCoder lang encodeTerm) types
      return $ M.fromList $ L.zip types cdrs
