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


annotateElementWithTypes :: (Ord m, Show m) => Element m -> GraphFlow m (Element m)
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

annotateTermWithTypes :: (Ord m, Show m) => Term m -> GraphFlow m (Term m)
annotateTermWithTypes term0 = do
  (term1, _) <- inferTypeAndConstraints term0
  anns <- graphAnnotations <$> getState
  mt <- annotationClassTermType anns term0 -- Use a provided type, if available, rather than the inferred type
  let annotType (ann, typ, _) = annotationClassSetTypeOf anns (Just $ Y.fromMaybe typ mt) ann
  return $ rewriteTermMeta annotType term1

inferType :: (Ord m, Show m) => Term m -> GraphFlow m (TypeScheme m)
inferType term = snd <$> inferTypeAndConstraints term

-- | Solve for the top-level type of an expression in a given environment
inferTypeAndConstraints :: (Ord m, Show m) => Term m -> GraphFlow m (Term (m, Type m, [Constraint m]), TypeScheme m)
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

rewriteDataType :: Ord m => (Type m -> Type m) -> Term (m, Type m, [Constraint m]) -> Term (m, Type m, [Constraint m])
rewriteDataType f = rewriteTermMeta rewrite
  where
    rewrite (x, typ, c) = (x, f typ, c)

startContext :: Graph m -> InferenceGraph m
startContext cx = InferenceContext cx 0 M.empty
