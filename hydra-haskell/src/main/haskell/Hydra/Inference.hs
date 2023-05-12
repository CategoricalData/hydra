-- | Entry point for Hydra type inference, which is a variation on on Hindley-Milner

module Hydra.Inference (
  annotateElementWithTypes,
  annotateGraphWithTypes,
  annotateTermWithTypes,
  inferType,
  inferTypeAndConstraints,
  inferTypeScheme,
  Constraint,
) where

import Hydra.Compute
import Hydra.Core
import Hydra.CoreEncoding
import Hydra.Graph
import Hydra.Lexical
import Hydra.Mantle
import Hydra.Flows
import Hydra.Kv
import Hydra.Rewriting
import Hydra.Substitution
import Hydra.Unification
import Hydra.Rules
import Hydra.Tools.Sorting
import qualified Hydra.Dsl.Types as Types

import qualified Control.Monad as CM
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Maybe as Y


annotateElementWithTypes :: (Ord a, Show a) => Element a -> GraphFlow a (Element a)
annotateElementWithTypes el = do
    withTrace ("annotate element " ++ unName (elementName el)) $ do
      term <- annotateTermWithTypes $ elementData el
      typ <- findType term
      return $ el {
        elementData = term,
        elementSchema = epsilonEncodeType typ}
  where
    findType term = do
      cx <- getState
      mt <- annotationClassTermType (graphAnnotations cx) term
      case mt of
        Just t -> return t
        Nothing -> fail "expected a type annotation"

annotateGraphWithTypes :: (Ord a, Show a) => GraphFlow a (Graph a)
annotateGraphWithTypes = getState >>= annotateGraph
  where
    annotateGraph g = do
        adjList <- CM.mapM toAdj $ M.toList els
        case topologicalSort adjList of
          Nothing -> fail $ "cyclical dependency not resolved through annotations"
          Just names -> do
            let sortedEls = Y.catMaybes ((\n -> M.lookup n els) <$> names)
            annotatedEls <- CM.mapM annotateElementWithTypes sortedEls
            return g {graphElements = M.fromList (toPair <$> annotatedEls)}
              where
                toPair el = (elementName el, el)
      where
        els = graphElements g
        isElName name = M.member name els
        toAdj (name, el) = do
          let term = elementData el
          mtyp <- annotationClassTermType (graphAnnotations g) term
          let deps = case mtyp of
                Nothing -> L.filter isElName $ S.toList $ freeVariablesInTerm term
                Just _ -> []
          return (name, deps)

annotateTermWithTypes :: (Ord a, Show a) => Term a -> GraphFlow a (Term a)
annotateTermWithTypes term0 = do
  (term1, _) <- inferTypeAndConstraints term0

  g <- getState
  let anns = graphAnnotations g
  let annotType (ann, typ, _) = annotationClassSetTypeOf anns (Just typ) ann
  let term2 = rewriteTermMeta annotType term1

  useProvidedTypeAnnotation g anns term0 term2
 where
   -- TODO: this merely compensates for a shortcoming of type inference. Fix that shortcoming and get rid of this step.
   useProvidedTypeAnnotation g anns term0 term1 = do
     mt <- annotationClassTermType anns term0
     case mt of
       Nothing -> pure term1
       Just t -> pure $ annotationClassSetTermType anns g (Just t) term1

inferType :: (Ord a, Show a) => Term a -> GraphFlow a (Type a)
inferType term = typeSchemeType <$> inferTypeScheme term

-- | Solve for the top-level type of an expression in a given environment
inferTypeAndConstraints :: (Ord a, Show a) => Term a -> GraphFlow a (Term (a, Type a, [Constraint a]), TypeScheme a)
inferTypeAndConstraints term = do
    withTrace ("infer type") $ do
      cx <- getState
      withState (startContext cx) $ do
        term1 <- infer term
        subst <- withGraphContext $ withSchemaContext $ solveConstraints (termConstraints term1)
        let term2 = rewriteDataType (substituteInType subst) term1
        return (term2, closeOver $ termType term2)
  where
    -- | Canonicalize and return the polymorphic top-level type.
    closeOver = normalizeScheme . generalize M.empty . reduceType

inferTypeScheme :: (Ord a, Show a) => Term a -> GraphFlow a (TypeScheme a)
inferTypeScheme term = snd <$> inferTypeAndConstraints term

rewriteDataType :: Ord a => (Type a -> Type a) -> Term (a, Type a, [Constraint a]) -> Term (a, Type a, [Constraint a])
rewriteDataType f = rewriteTermMeta rewrite
  where
    rewrite (x, typ, c) = (x, f typ, c)

startContext :: Graph a -> InferenceContext a
startContext cx = InferenceContext cx 0 M.empty
