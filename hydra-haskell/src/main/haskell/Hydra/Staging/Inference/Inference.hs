-- | Entry point for Hydra type inference, which is a variation on on Hindley-Milner

module Hydra.Staging.Inference.Inference where

import Hydra.Annotations
import Hydra.Compute
import Hydra.Core
import Hydra.CoreEncoding
import Hydra.Graph
import Hydra.Staging.Lexical
import Hydra.Mantle
import Hydra.Staging.Annotations
import Hydra.Staging.Rewriting
import Hydra.Staging.Inference.Substitution
import Hydra.Staging.Inference.Unification
import Hydra.Staging.Inference.Rules
import Hydra.Flows
import Hydra.Rewriting
import Hydra.Errors
import Hydra.Lexical
import Hydra.Staging.Sorting
import qualified Hydra.Dsl.Terms as Terms
import qualified Hydra.Dsl.Types as Types

import qualified Control.Monad as CM
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Maybe as Y


--annotateElements :: Graph -> [Element] -> Flow Graph [Element]
--annotateElements g sortedEls = withInferenceContext $ do
--    iels' <- annotate sortedEls ([])
--    let iels = fst <$> iels'
--    let constraints = snd <$> iels'
--
--    -- Note: inference occurs over the entire graph at once,
--    --       but unification and substitution occur within elements in isolation
--    subst <- withSchemaContext $ CM.mapM solveConstraints constraints
--    return $ L.zipWith rewriteElement subst iels
--  where
--    -- Note: the following defaults to user-provided type annotations where provided.
--    --       In the future, we should trust unification to perform this defaulting, and not override the inferred type.
--    rewriteElement subst el = el { elementTerm = setTermType (Just typ) term1 }
--      where
--        term0 = elementTerm el
--        term1 = rewriteDataType (substituteInType subst) term0
--        typ = Y.fromMaybe (termType term1) $ getTermType term0
--
--    annotate :: [Element] -> [(Element, [TypeConstraint])] -> Flow Graph [(Element, [TypeConstraint])]
--    annotate original annotated = case original of
--      [] -> pure $ L.reverse annotated
--      (el:r) -> do
--        (Inferred iel t c1) <- inferElementType el
--        withBinding (elementName el) (monotype t) $ annotate r ((iel, c1):annotated)

inferElementType :: Element -> Flow Graph (Inferred Element)
inferElementType el = withTrace ("infer type of " ++ unName (elementName el)) $ do
  (Inferred iterm t c) <- infer $ elementTerm el
  return (Inferred (el {elementTerm = iterm}) t c)

--inferGraphTypes :: Flow Graph Graph
--inferGraphTypes = getState >>= annotateGraph
--  where
--    annotateGraph g = withTrace ("infer graph types") $ do
--        sorted <- sortGraphElements g
--        els <- sortGraphElements g >>= annotateElements g
--        return g {graphElements = M.fromList (toPair <$> els)}
--      where
--        toPair el = (elementName el, el)

--inferTermType :: Term -> Flow Graph Term
--inferTermType term0 = do
--  (term1, _) <- inferTypeAndConstraints term0
--  return term1

---- TODO: deprecated
--inferredTypeOf :: Term -> Flow Graph Type
--inferredTypeOf term = typeSchemeType <$> inferTypeScheme term

---- TODO: deprecated
---- | Solve for the top-level type of an expression in a given environment
--inferTypeAndConstraints :: Term -> Flow Graph (Term, TypeScheme)
--inferTypeAndConstraints term = withTrace ("infer type") $ withInferenceContext $ do
--    (Inferred iterm _ constraints) <- infer term
--    subst <- withSchemaContext $ solveConstraints constraints
--    let term2 = rewriteDataType (substituteInType subst) iterm
----    let typ = Y.fromMaybe (termType term2) $ getTermType term
----    return (setTermType (Just typ) term2, closeOver $ termType term2)
--    return (term2, closeOver $ termType term2)
--  where
--    -- | Canonicalize and return the polymorphic top-level type.
--    closeOver = normalizeScheme . generalize M.empty . reduceType

---- TODO: deprecated
--inferTypeScheme :: Term -> Flow Graph TypeScheme
--inferTypeScheme term = snd <$> inferTypeAndConstraints term

rewriteDataType :: (Type -> Type) -> Term -> Term
rewriteDataType f = rewriteTerm ff
  where
    ff recurse term = case recurse term of
      TermTyped (TypedTerm term1 type1) -> TermTyped $ TypedTerm term1 (f type1)
      t -> t

--sortGraphElements :: Graph -> Flow Graph [Element]
--sortGraphElements g = do
--    let annotated = S.fromList $ Y.catMaybes (ifAnnotated <$> M.elems els)
--    adjList <- CM.mapM (toAdj annotated) $ M.elems els
--    case topologicalSort adjList of
--      Left comps -> fail $ "cyclical dependency not resolved through annotations: " ++ L.intercalate ", " (unName <$> L.head comps)
--      Right names -> return $ Y.catMaybes ((\n -> M.lookup n els) <$> names)
--  where
--    els = graphElements g
--    ifAnnotated el = case (getTermType $ elementTerm el) of
--      Nothing -> Nothing
--      Just _ -> Just $ elementName el
--    toAdj annotated el = do
--        let deps = L.filter isNotAnnotated $ L.filter isElName $ S.toList $ freeVariablesInTerm $ elementTerm el
--
--        return (elementName el, deps)
--      where
--        -- Ignore free variables which are not valid element references
--        isElName name = M.member name els
--        -- No need for an inference dependency on an element which is already annotated with a type
--        isNotAnnotated name = not $ S.member name annotated

--withInferenceContext flow = do
--    g <- getState
--    let env = initialEnv g
--    withState (g {graphTypes = env}) flow
--  where
--    initialEnv g = M.fromList $ Y.catMaybes (toPair <$> (M.elems $ graphElements g))
--      where
--        toPair el = (\t -> (elementName el, monotype t)) <$> (getTermType $ elementTerm el)
