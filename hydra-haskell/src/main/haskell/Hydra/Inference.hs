-- | Entry point for Hydra type inference, which is a variation on on Hindley-Milner

module Hydra.Inference (
  annotateElementWithTypes,
  annotateTermWithTypes,
  inferType,
  inferTypeAndConstraints,
  Constraint,
) where

import Hydra.Compute
import Hydra.Core
import Hydra.CoreEncoding
import Hydra.Graph
import Hydra.Lexical
import Hydra.Mantle
import Hydra.Monads
import Hydra.Rewriting
import Hydra.Substitution
import Hydra.Unification
import Hydra.Rules
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

annotateTermWithTypes :: (Ord a, Show a) => Term a -> GraphFlow a (Term a)
annotateTermWithTypes term0 = do
  (term1, _) <- inferTypeAndConstraints term0
  anns <- graphAnnotations <$> getState
  mt <- annotationClassTermType anns term0 -- Use a provided type, if available, rather than the inferred type
  let annotType (ann, typ, _) = annotationClassSetTypeOf anns (Just $ Y.fromMaybe typ mt) ann
  return $ rewriteTermMeta annotType term1

inferType :: (Ord a, Show a) => Term a -> GraphFlow a (TypeScheme a)
inferType term = snd <$> inferTypeAndConstraints term

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

rewriteDataType :: Ord a => (Type a -> Type a) -> Term (a, Type a, [Constraint a]) -> Term (a, Type a, [Constraint a])
rewriteDataType f = rewriteTermMeta rewrite
  where
    rewrite (x, typ, c) = (x, f typ, c)

startContext :: Graph a -> InferenceGraph a
startContext cx = InferenceContext cx 0 M.empty
