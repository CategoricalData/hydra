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
getDescription meta = case getAnnotation metaDescription meta of
  Nothing -> pure Nothing
  Just term -> case term of
    TermLiteral (LiteralString s) -> pure $ Just s
    _ -> fail $ "unexpected value for " ++ show metaDescription ++ ": " ++ show term

getTermAnnotation :: Context Kv -> String -> Term Kv -> Y.Maybe (Term Kv)
getTermAnnotation cx key = getAnnotation key . termMetaInternal

getTermDescription :: Term Kv -> GraphFlow Kv (Y.Maybe String)
getTermDescription = getDescription . termMetaInternal

getType :: Kv -> GraphFlow Kv (Y.Maybe (Type Kv))
getType meta = case getAnnotation metaType meta of
  Nothing -> pure Nothing
  Just dat -> Just <$> decodeType dat

getTypeDescription :: Type Kv -> GraphFlow Kv (Y.Maybe String)
getTypeDescription = getDescription . typeMetaInternal

metaAnnotationClass :: AnnotationClass Kv
metaAnnotationClass = AnnotationClass {
    annotationClassDefault = Kv M.empty,
    annotationClassEqual = (==),
    annotationClassCompare = \m1 m2 -> toComparison $ m1 `compare` m2,
    annotationClassShow = show,
    annotationClassRead = read,

    -- TODO: simplify
    annotationClassTermAnnotation = termMetaInternal,
    annotationClassTypeAnnotation = typeMetaInternal,
    annotationClassTermDescription = getTermDescription,
    annotationClassTypeDescription = getTypeDescription,
    annotationClassTermType = getType . termMetaInternal,
    annotationClassSetTermDescription = setTermDescription,
    annotationClassSetTermType = setTermType,
    annotationClassTypeOf = getType,
    annotationClassSetTypeOf = setType}
  where
    toComparison c = case c of
      LT -> ComparisonLessThan
      EQ -> ComparisonEqualTo
      GT -> ComparisonGreaterThan

metaDescription :: String
metaDescription = "description"

metaType :: String
metaType = "type"

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
setDescription d = setAnnotation metaDescription (Terms.string <$> d)

setTermAnnotation :: Context Kv -> String -> Y.Maybe (Term Kv) -> Term Kv -> Term Kv
setTermAnnotation cx key val term = if meta == annotationClassDefault (contextAnnotations cx)
    then term'
    else TermAnnotated $ Annotated term' meta
  where
    term' = stripTerm term
    meta = setAnnotation key val $ termMetaInternal term

setTermDescription :: Context Kv -> Y.Maybe String -> Term Kv -> Term Kv
setTermDescription cx d = setTermAnnotation cx metaDescription (Terms.string <$> d)

setTermType :: Context Kv -> Y.Maybe (Type Kv) -> Term Kv -> Term Kv
setTermType cx d = setTermAnnotation cx metaType (encodeType <$> d)

setType :: Y.Maybe (Type Kv) -> Kv -> Kv
setType mt = setAnnotation metaType (encodeType <$> mt)

setTypeAnnotation :: Context Kv -> String -> Y.Maybe (Term Kv) -> Type Kv -> Type Kv
setTypeAnnotation cx key val typ = if meta == annotationClassDefault (contextAnnotations cx)
    then typ'
    else TypeAnnotated $ Annotated typ' meta
  where
    typ' = stripType typ
    meta = setAnnotation key val $ typeMetaInternal typ

setTypeDescription :: Context Kv -> Y.Maybe String -> Type Kv -> Type Kv
setTypeDescription cx d = setTypeAnnotation cx metaDescription (Terms.string <$> d)

termMetaInternal :: Term Kv -> Kv
termMetaInternal = aggregateAnnotations $ \t -> case t of
  TermAnnotated a -> Just a
  _ -> Nothing

typeMetaInternal :: Type Kv -> Kv
typeMetaInternal = aggregateAnnotations $ \t -> case t of
  TypeAnnotated a -> Just a
  _ -> Nothing
