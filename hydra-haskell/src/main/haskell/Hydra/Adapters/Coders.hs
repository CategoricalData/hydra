module Hydra.Adapters.Coders where

import Hydra.Core
import Hydra.Compute
import Hydra.Module
import Hydra.Monads
import Hydra.Lexical
import Hydra.Rewriting
import Hydra.Adapters.Term
import Hydra.CoreLanguage
import Hydra.Adapters.UtilsEtc
import Hydra.Lexical
import Hydra.CoreDecoding

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
  where

transformModule :: (Ord m, Read m, Show m)
  => Language m
  -> (Term m -> GraphFlow m e)
  -> (Module m -> M.Map (Type m) (Coder (Context m) (Term m) e) -> [(Element m, TypedTerm m)] -> GraphFlow m d)
  -> Module m -> GraphFlow m d
transformModule lang encodeTerm createModule mod = do
    pairs <- withSchemaContext $ CM.mapM elementAsTypedTerm els
    coders <- codersFor $ L.nub (typedTermType <$> pairs)
    createModule mod coders $ L.zip els pairs
  where
    els = moduleElements mod

    codersFor types = do
      cdrs <- CM.mapM constructCoder types
      return $ M.fromList $ L.zip types cdrs

    constructCoder typ = do
        cx <- getState
        let acx = AdapterContext cx hydraCoreLanguage lang
        adapter <- withState acx $ termAdapter typ
        coder <- termCoder $ adapterTarget adapter
        return $ composeCoders (adapterCoder adapter) coder
      where
        termCoder _ = pure $ unidirectionalCoder encodeTerm
