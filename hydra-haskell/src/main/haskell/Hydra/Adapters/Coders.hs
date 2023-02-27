-- | Entry point for Hydra's adapter (type/term rewriting) framework.
--   An adapter takes a type expression which is supported in a source language, and rewrites it to a type which is supported by a target language.
--   In parallel, terms conforming to the original type are rewritten. Both levels of the transformation are bidirectional.

module Hydra.Adapters.Coders where

import Hydra.Kernel
import Hydra.Adapters.Term
import Hydra.Adapters.UtilsEtc

import qualified Control.Monad as CM
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S


adaptType :: (Ord m, Read m, Show m) => Language m -> Type m -> GraphFlow m (Type m)
adaptType targetLang t = do
    cx <- getState
    let acx = AdapterContext cx hydraCoreLanguage targetLang
    ad <- withState acx $ termAdapter t
    return $ adapterTarget ad

constructCoder :: (Ord m, Read m, Show m)
  => Language m
  -> (Term m -> GraphFlow m c)
  -> Type m
  -> GraphFlow m (Coder (Context m) (Context m) (Term m) c)
constructCoder lang encodeTerm typ = withTrace ("coder for " ++ describeType typ) $ do
    cx <- getState
    let acx = AdapterContext cx hydraCoreLanguage lang
    adapter <- withState acx $ termAdapter typ
    coder <- termCoder $ adapterTarget adapter
    return $ composeCoders (adapterCoder adapter) coder
  where
    termCoder _ = pure $ unidirectionalCoder encodeTerm

transformModule :: (Ord m, Read m, Show m)
  => Language m
  -> (Term m -> GraphFlow m e)
  -> (Module m -> M.Map (Type m) (Coder (Context m) (Context m) (Term m) e) -> [(Element m, TypedTerm m)] -> GraphFlow m d)
  -> Module m -> GraphFlow m d
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
