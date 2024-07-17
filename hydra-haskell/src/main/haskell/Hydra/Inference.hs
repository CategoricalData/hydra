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


annotateElements :: Graph Kv -> [Element Kv] -> Flow (Graph Kv) [Element Kv]
annotateElements g sortedEls = withInferenceContext $ do
    iels' <- annotate sortedEls ([])
    let iels = fst <$> iels'
    let constraints = snd <$> iels'

    -- Note: inference occurs over the entire graph at once,
    --       but unification and substitution occur within elements in isolation
    subst <- withGraphContext $ withSchemaContext $ CM.mapM solveConstraints constraints
    return $ L.zipWith rewriteElement subst iels
  where
    rewriteElement subst el = el {
          elementData = rewriteDataType (substituteInType subst) $ elementData el}

    annotate :: [Element Kv] -> [(Element Kv, [Constraint])] -> Flow (InferenceContext) [(Element Kv, [Constraint])]
    annotate original annotated = case original of
      [] -> pure $ L.reverse annotated
      (el:r) -> do
        (iel, c1) <- inferElementType el
        withBinding (elementName el) (termTypeScheme $ elementData iel) $ annotate r ((iel, c1):annotated)

annotateTermWithTypes :: Term Kv -> Flow (Graph Kv) (Term Kv)
annotateTermWithTypes term0 = do
  (term1, _) <- inferTypeAndConstraints term0
  return term1

inferElementType :: Element Kv -> Flow (InferenceContext) (Element Kv, [Constraint])
inferElementType el = withTrace ("infer type of " ++ unName (elementName el)) $ do
  (iterm, c) <- infer $ elementData el
  return (el {elementData = iterm}, c)

inferGraphTypes :: Flow (Graph Kv) (Graph Kv)
inferGraphTypes = getState >>= annotateGraph
  where
    annotateGraph g = withTrace ("infer graph types") $ do
        sorted <- sortGraphElements g
        els <- sortGraphElements g >>= annotateElements g
        return g {graphElements = M.fromList (toPair <$> els)}
      where
        toPair el = (elementName el, el)

-- TODO: deprecated
inferType :: Term Kv -> Flow (Graph Kv) (Type Kv)
inferType term = typeSchemeType <$> inferTypeScheme term

-- TODO: deprecated
-- | Solve for the top-level type of an expression in a given environment
inferTypeAndConstraints :: Term Kv -> Flow (Graph Kv) (Term Kv, TypeScheme Kv)
inferTypeAndConstraints term = withTrace ("infer type") $ withInferenceContext $ do
    (iterm, constraints) <- infer term
    subst <- withGraphContext $ withSchemaContext $ solveConstraints constraints
    let term2 = rewriteDataType (substituteInType subst) iterm
    return (term2, closeOver $ termType term2)
  where
    -- | Canonicalize and return the polymorphic top-level type.
    closeOver = normalizeScheme . generalize M.empty . reduceType

-- TODO: deprecated
inferTypeScheme :: Term Kv -> Flow (Graph Kv) (TypeScheme Kv)
inferTypeScheme term = snd <$> inferTypeAndConstraints term

rewriteDataType :: (Type Kv -> Type Kv) -> Term Kv -> Term Kv
rewriteDataType f = rewriteTerm ff id
  where
    ff recurse term = case recurse term of
      TermTyped (TermWithType term1 type1) -> TermTyped $ TermWithType term1 (f type1)
      t -> t

sortGraphElements :: Graph Kv -> Flow (Graph Kv) [Element Kv]
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
