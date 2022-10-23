module Hydra.Adapters.Coders where

import Hydra.All
import Hydra.CoreDecoding
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
  where

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
      cdrs <- CM.mapM constructCoder types
      return $ M.fromList $ L.zip types cdrs

    constructCoder typ = withTrace ("coder for " ++ describeType typ) $ do
        cx <- getState
        let acx = AdapterContext cx hydraCoreLanguage lang
        adapter <- withState acx $ termAdapter typ
        coder <- termCoder $ adapterTarget adapter
        return $ composeCoders (adapterCoder adapter) coder
      where
        termCoder _ = pure $ unidirectionalCoder encodeTerm
