module Hydra.Util.Coders where

import Hydra.Adapter
import Hydra.Core
import Hydra.Evaluation
import Hydra.Graph
import Hydra.Impl.Haskell.Extras
import qualified Hydra.Lib.Strings as Strings
import Hydra.Prototyping.Primitives
import Hydra.Prototyping.Rewriting
import qualified Control.Monad as CM
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import Hydra.Prototyping.Adapters.Term
import Hydra.Prototyping.CoreLanguage 
import Hydra.Prototyping.Steps


dataGraphDependencies :: Show m => Graph m -> S.Set GraphName
dataGraphDependencies g = S.delete (graphName g) allDeps
  where
    allDeps = L.foldl (\s t -> S.union s $ depsOf t) S.empty $
      (elementData <$> graphElements g) ++ (elementSchema <$> graphElements g)
    depsOf term = foldOverTerm TraversalOrderPre addNames S.empty term
    addNames names term = case termData term of
      ExpressionElement name -> S.insert (graphNameOf name) names
      ExpressionFunction (FunctionPrimitive name) -> S.insert (graphNameOf name) names
      ExpressionNominal (NominalTerm name _) -> S.insert name names
      _ -> names
    graphNameOf = L.head . Strings.splitOn "."

dataGraphToExternalModule :: (Default m, Ord m, Read m, Show m)
  => Language
  -> (Context m -> Term m -> Result e)
  -> (Context m -> Graph m -> M.Map Type (Step (Term m) e) -> [(Element m, TypedTerm m)] -> Result d)
  -> Context m -> Graph m -> Qualified d
dataGraphToExternalModule lang encodeTerm createModule cx g = do
    scx <- resultToQualified $ schemaContext cx
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
        return $ composeSteps (adapterStep adapter) coder
      where
        adContext = AdapterContext cx hydraCoreLanguage lang
        termCoder _ = pure $ unidirectionalStep (encodeTerm cx)
