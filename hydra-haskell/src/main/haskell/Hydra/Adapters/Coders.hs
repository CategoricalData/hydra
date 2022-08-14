module Hydra.Adapters.Coders where

import Hydra.Adapter
import Hydra.Core
import Hydra.Evaluation
import Hydra.Graph
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


adaptType :: (Ord m, Read m, Show m) => Context m -> Language m -> Type m -> Result (Type m)
adaptType cx targetLang t = do
    ad <- qualifiedToResult $ termAdapter ac t
    return $ adapterTarget ad
  where
    ac = AdapterContext cx hydraCoreLanguage targetLang

graphToExternalModule :: (Ord m, Read m, Show m)
  => Language m
  -> (Context m -> Term m -> Result e)
  -> (Context m -> Graph m -> M.Map (Type m) (Coder (Term m) e) -> [(Element m, TypedTerm m)] -> Result d)
  -> Context m -> Graph m -> Qualified d
graphToExternalModule lang encodeTerm createModule cx g = do
    let scx = schemaContext cx
    pairs <- resultToQualified $ CM.mapM (elementAsTypedTerm scx) els
    coders <- codersFor $ L.nub (typedTermType <$> pairs)
    resultToQualified $ createModule cx g coders $ L.zip els pairs
  where
    els = graphElements g

    codersFor types = do
      cdrs <- CM.mapM constructCoder types
      return $ M.fromList $ L.zip types cdrs

    constructCoder typ = do
        adapter <- termAdapter adContext typ
        coder <- termCoder $ adapterTarget adapter
        return $ composeCoders (adapterCoder adapter) coder
      where
        adContext = AdapterContext cx hydraCoreLanguage lang
        termCoder _ = pure $ unidirectionalCoder (encodeTerm cx)
