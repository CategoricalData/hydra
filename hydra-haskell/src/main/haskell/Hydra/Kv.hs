-- | Functions for working with Kv, the default annotation type in Hydra

module Hydra.Kv where

import Hydra.Core
import Hydra.Compute
import Hydra.CoreDecoding
import Hydra.CoreEncoding
import Hydra.Common
import Hydra.Monads
import Hydra.Mantle
import qualified Hydra.Dsl.Terms as Terms

import qualified Data.Map as M
import qualified Data.Maybe as Y


aggregateAnnotations :: (a -> Maybe (Annotated a Kv)) -> a -> Kv
aggregateAnnotations getAnn t = Kv $ M.fromList $ addKv [] t
  where
    addKv m t = case getAnn t of
      Nothing -> m
      Just (Annotated t' (Kv other)) -> addKv (m ++ M.toList other) t'

emptyKv :: Kv
emptyKv = Kv M.empty

getAnnotation :: String -> Kv -> Maybe (Term Kv)
getAnnotation key (Kv m) = M.lookup key m

getAttr :: String -> Flow s (Maybe (Term Kv))
getAttr key = Flow q
  where
    q s0 t0 = FlowState (Just $ M.lookup key $ traceOther t0) s0 t0

getAttrWithDefault :: String -> Term Kv -> Flow s (Term Kv)
getAttrWithDefault key def = Y.fromMaybe def <$> getAttr key

getDescription :: Kv -> GraphFlow Kv (Y.Maybe String)
getDescription kv = case getAnnotation kvDescription kv of
  Nothing -> pure Nothing
  Just term -> case term of
    TermLiteral (LiteralString s) -> pure $ Just s
    _ -> fail $ "unexpected value for " ++ show kvDescription ++ ": " ++ show term

getTermAnnotation :: Context Kv -> String -> Term Kv -> Y.Maybe (Term Kv)
getTermAnnotation cx key = getAnnotation key . termAnnotationInternal

getTermDescription :: Term Kv -> GraphFlow Kv (Y.Maybe String)
getTermDescription = getDescription . termAnnotationInternal

getType :: Kv -> GraphFlow Kv (Y.Maybe (Type Kv))
getType kv = case getAnnotation kvType kv of
  Nothing -> pure Nothing
  Just dat -> Just <$> decodeType dat

getTypeDescription :: Type Kv -> GraphFlow Kv (Y.Maybe String)
getTypeDescription = getDescription . typeAnnotationInternal

kvAnnotationClass :: AnnotationClass Kv
kvAnnotationClass = AnnotationClass {
    annotationClassDefault = Kv M.empty,
    annotationClassEqual = (==),
    annotationClassCompare = \m1 m2 -> toComparison $ m1 `compare` m2,
    annotationClassShow = show,
    annotationClassRead = read,

    -- TODO: simplify
    annotationClassTermAnnotation = termAnnotationInternal,
    annotationClassTypeAnnotation = typeAnnotationInternal,
    annotationClassTermDescription = getTermDescription,
    annotationClassTypeDescription = getTypeDescription,
    annotationClassTermType = getType . termAnnotationInternal,
    annotationClassSetTermDescription = setTermDescription,
    annotationClassSetTermType = setTermType,
    annotationClassTypeOf = getType,
    annotationClassSetTypeOf = setType}
  where
    toComparison c = case c of
      LT -> ComparisonLessThan
      EQ -> ComparisonEqualTo
      GT -> ComparisonGreaterThan

kvDescription :: String
kvDescription = "description"

kvType :: String
kvType = "type"

nextCount :: String -> Flow s Int
nextCount attrName = do
  count <- getAttrWithDefault attrName (Terms.int32 0) >>= Terms.expectInt32
  putAttr attrName (Terms.int32 $ count + 1)
  return count

putAttr :: String -> Term Kv -> Flow s ()
putAttr key val = Flow q
  where
    q s0 t0 = FlowState (Just ()) s0 (t0 {traceOther = M.insert key val $ traceOther t0})

setAnnotation :: String -> Y.Maybe (Term Kv) -> Kv -> Kv
setAnnotation key val (Kv m) = Kv $ M.alter (const val) key m

setDescription :: Y.Maybe String -> Kv -> Kv
setDescription d = setAnnotation kvDescription (Terms.string <$> d)

setTermAnnotation :: Context Kv -> String -> Y.Maybe (Term Kv) -> Term Kv -> Term Kv
setTermAnnotation cx key val term = if kv == annotationClassDefault (contextAnnotations cx)
    then term'
    else TermAnnotated $ Annotated term' kv
  where
    term' = stripTerm term
    kv = setAnnotation key val $ termAnnotationInternal term

setTermDescription :: Context Kv -> Y.Maybe String -> Term Kv -> Term Kv
setTermDescription cx d = setTermAnnotation cx kvDescription (Terms.string <$> d)

setTermType :: Context Kv -> Y.Maybe (Type Kv) -> Term Kv -> Term Kv
setTermType cx d = setTermAnnotation cx kvType (encodeType <$> d)

setType :: Y.Maybe (Type Kv) -> Kv -> Kv
setType mt = setAnnotation kvType (encodeType <$> mt)

setTypeAnnotation :: Context Kv -> String -> Y.Maybe (Term Kv) -> Type Kv -> Type Kv
setTypeAnnotation cx key val typ = if kv == annotationClassDefault (contextAnnotations cx)
    then typ'
    else TypeAnnotated $ Annotated typ' kv
  where
    typ' = stripType typ
    kv = setAnnotation key val $ typeAnnotationInternal typ

setTypeDescription :: Context Kv -> Y.Maybe String -> Type Kv -> Type Kv
setTypeDescription cx d = setTypeAnnotation cx kvDescription (Terms.string <$> d)

termAnnotationInternal :: Term Kv -> Kv
termAnnotationInternal = aggregateAnnotations $ \t -> case t of
  TermAnnotated a -> Just a
  _ -> Nothing

typeAnnotationInternal :: Type Kv -> Kv
typeAnnotationInternal = aggregateAnnotations $ \t -> case t of
  TypeAnnotated a -> Just a
  _ -> Nothing
