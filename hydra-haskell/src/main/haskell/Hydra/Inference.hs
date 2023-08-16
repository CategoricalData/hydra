-- | Entry point for Hydra type inference, which is a variation on on Hindley-Milner

module Hydra.Inference (
  annotateTermWithTypes,
  inferGraphTypes,
  inferType,
  inferTypeScheme,
  inferTypeAndConstraints,
  Constraint,
) where

import Hydra.Compute
import Hydra.Core
import Hydra.CoreEncoding
import Hydra.Graph
import Hydra.Lexical
import Hydra.Mantle
import Hydra.Kv
import Hydra.Rewriting
import Hydra.Substitution
import Hydra.Unification
import Hydra.Rules
import Hydra.Tier1
import Hydra.Tier2
import Hydra.Tools.Sorting
import qualified Hydra.Dsl.Terms as Terms
import qualified Hydra.Dsl.Types as Types

import qualified Control.Monad as CM
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Maybe as Y


annotateElements :: (Ord a, Show a) => Graph a -> [Element a] -> Flow (Graph a) [Element a]
annotateElements g sortedEls = withInferenceContext $ do
    iels <- annotate sortedEls []

    -- Note: inference occurs over the entire graph at once,
    --       but unification and substitution occur within elements in isolation
    let constraints = termConstraints . elementData <$> iels
    subst <- withGraphContext $ withSchemaContext $ CM.mapM solveConstraints constraints
    r <- CM.zipWithM rewriteElement subst iels

    return r
  where
    rewriteElement subst el = do
        let itm = rewriteDataType (substituteInType subst) $ elementData el
        term <- rewriteTermMetaM annotType itm
        return el {
          elementData = term}
      where
        annotType (ann, typ, _) = do
          let anns = graphAnnotations g
          mtyp <- withGraphContext $ annotationClassTypeOf anns ann
          let typ' = Y.fromMaybe typ mtyp
          return $ annotationClassSetTypeOf anns (Just typ') ann

    annotate original annotated = case original of
      [] -> pure $ L.reverse annotated
      (el:r) -> do
        iel <- inferElementType el
        withBinding (elementName el) (termTypeScheme $ elementData iel) $ annotate r (iel:annotated)

annotateTermWithTypes :: (Ord a, Show a) => Term a -> Flow (Graph a) (Term a)
annotateTermWithTypes term0 = do
  (term1, _) <- inferTypeAndConstraints term0

  g <- getState
  let anns = graphAnnotations g
  let annotType (ann, typ, _) = annotationClassSetTypeOf anns (Just typ) ann
  let term2 = rewriteTermMeta annotType term1
  return term2

inferElementType :: (Ord a, Show a) => Element a -> Flow (InferenceContext a) (Element (InfAnn a))
inferElementType el = withTrace ("infer type of " ++ unName (elementName el)) $ do
  iterm <- infer $ elementData el
  return $ el {elementData = iterm}

inferGraphTypes :: (Ord a, Show a) => Flow (Graph a) (Graph a)
inferGraphTypes = getState >>= annotateGraph
  where
    annotateGraph g = withTrace ("infer graph types") $ do
        sorted <- sortGraphElements g
        els <- sortGraphElements g >>= annotateElements g
        return g {graphElements = M.fromList (toPair <$> els)}
      where
        toPair el = (elementName el, el)

-- TODO: deprecated
inferType :: (Ord a, Show a) => Term a -> Flow (Graph a) (Type a)
inferType term = typeSchemeType <$> inferTypeScheme term

-- TODO: deprecated
-- | Solve for the top-level type of an expression in a given environment
inferTypeAndConstraints :: (Ord a, Show a) => Term a -> Flow (Graph a) (Term (a, Type a, [Constraint a]), TypeScheme a)
inferTypeAndConstraints term = withTrace ("infer type") $ withInferenceContext $ do
    iterm <- infer term
    subst <- withGraphContext $ withSchemaContext $ solveConstraints (termConstraints iterm)
    let term2 = rewriteDataType (substituteInType subst) iterm
    return (term2, closeOver $ termType term2)
  where
    -- | Canonicalize and return the polymorphic top-level type.
    closeOver = normalizeScheme . generalize M.empty . reduceType

-- TODO: deprecated
inferTypeScheme :: (Ord a, Show a) => Term a -> Flow (Graph a) (TypeScheme a)
inferTypeScheme term = snd <$> inferTypeAndConstraints term

rewriteDataType :: Ord a => (Type a -> Type a) -> Term (a, Type a, [Constraint a]) -> Term (a, Type a, [Constraint a])
rewriteDataType f = rewriteTermMeta rewrite
  where
    rewrite (x, typ, c) = (x, f typ, c)

sortGraphElements :: (Ord a, Show a) => Graph a -> Flow (Graph a) [Element a]
sortGraphElements g = do
    annotated <- S.fromList . Y.catMaybes <$> (CM.mapM ifAnnotated $ M.elems els)
    adjList <- CM.mapM (toAdj annotated) $ M.elems els
    case topologicalSort adjList of
      Left comps -> fail $ "cyclical dependency not resolved through annotations: " ++ L.intercalate ", " (unName <$> L.head comps)
      Right names -> return $ Y.catMaybes ((\n -> M.lookup n els) <$> names)
  where
    els = graphElements g
    ifAnnotated el = do
      mtyp <- annotationClassTermType (graphAnnotations g) $ elementData el
      return $ case mtyp of
        Nothing -> Nothing
        Just _ -> Just $ elementName el
    toAdj annotated el = do
        let deps = L.filter isNotAnnotated $ L.filter isElName $ S.toList $ freeVariablesInTerm $ elementData el

        return (elementName el, deps)
      where
        -- Ignore free variables which are not valid element references
        isElName name = M.member name els
        -- No need for an inference dependency on an element which is already annotated with a type
        isNotAnnotated name = not $ S.member name annotated

withInferenceContext flow = do
    g <- getState
    env <- initialEnv g $ graphAnnotations g
    withState (InferenceContext g env) flow
  where
    initialEnv g anns = M.fromList . Y.catMaybes <$> (CM.mapM toPair $ M.elems $ graphElements g)
      where
        toPair el = do
          mt <- annotationClassTermType anns $ elementData el
          return $ (\t -> (elementName el, monotype t)) <$> mt
