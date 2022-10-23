module Hydra.Meta where

import Hydra.Core
import Hydra.Compute
import Hydra.CoreDecoding
import Hydra.CoreEncoding
import Hydra.Common
import Hydra.Monads
import Hydra.Mantle
import Hydra.Impl.Haskell.Dsl.Terms

import qualified Data.Map as M
import qualified Data.Maybe as Y


aggregateAnnotations :: (a -> Maybe (Annotated a Meta)) -> a -> Meta
aggregateAnnotations getAnn t = Meta $ M.fromList $ addMeta [] t
  where
    addMeta m t = case getAnn t of
      Nothing -> m
      Just (Annotated t' (Meta other)) -> addMeta (m ++ M.toList other) t'

getAnnotation :: String -> Meta -> Maybe (Term Meta)
getAnnotation key (Meta m) = M.lookup key m

getDescription :: Meta -> GraphFlow Meta (Y.Maybe String)
getDescription meta = case getAnnotation metaDescription meta of
  Nothing -> pure Nothing
  Just term -> case term of
    TermLiteral (LiteralString s) -> pure $ Just s
    _ -> fail $ "unexpected value for " ++ show metaDescription ++ ": " ++ show term

getTermAnnotation :: Context Meta -> String -> Term Meta -> Y.Maybe (Term Meta)
getTermAnnotation cx key = getAnnotation key . termMetaInternal

getTermDescription :: Term Meta -> GraphFlow Meta (Y.Maybe String)
getTermDescription = getDescription . termMetaInternal

getType :: Meta -> GraphFlow Meta (Y.Maybe (Type Meta))
getType meta = case getAnnotation metaType meta of
  Nothing -> pure Nothing
  Just dat -> Just <$> decodeType dat

getTypeDescription :: Type Meta -> GraphFlow Meta (Y.Maybe String)
getTypeDescription = getDescription . typeMetaInternal

metaAnnotationClass :: AnnotationClass Meta
metaAnnotationClass = AnnotationClass {
    annotationClassDefault = Meta M.empty,
    annotationClassEqual = (==),
    annotationClassCompare = \m1 m2 -> toComparison $ m1 `compare` m2,
    annotationClassShow = show,
    annotationClassRead = read,

    -- TODO: simplify
    annotationClassTermMeta = termMetaInternal,
    annotationClassTypeMeta = typeMetaInternal,
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

setAnnotation :: String -> Y.Maybe (Term Meta) -> Meta -> Meta
setAnnotation key val (Meta m) = Meta $ M.alter (const val) key m

setDescription :: Y.Maybe String -> Meta -> Meta
setDescription d = setAnnotation metaDescription (string <$> d)

setTermAnnotation :: Context Meta -> String -> Y.Maybe (Term Meta) -> Term Meta -> Term Meta
setTermAnnotation cx key val term = if meta == annotationClassDefault (contextAnnotations cx)
    then term'
    else TermAnnotated $ Annotated term' meta
  where
    term' = stripTerm term
    meta = setAnnotation key val $ termMetaInternal term

setTermDescription :: Context Meta -> Y.Maybe String -> Term Meta -> Term Meta
setTermDescription cx d = setTermAnnotation cx metaDescription (string <$> d)

setTermType :: Context Meta -> Y.Maybe (Type Meta) -> Term Meta -> Term Meta
setTermType cx d = setTermAnnotation cx metaType (encodeType <$> d)

setType :: Y.Maybe (Type Meta) -> Meta -> Meta
setType mt = setAnnotation metaType (encodeType <$> mt)

setTypeAnnotation :: Context Meta -> String -> Y.Maybe (Term Meta) -> Type Meta -> Type Meta
setTypeAnnotation cx key val typ = if meta == annotationClassDefault (contextAnnotations cx)
    then typ'
    else TypeAnnotated $ Annotated typ' meta
  where
    typ' = stripType typ
    meta = setAnnotation key val $ typeMetaInternal typ

setTypeDescription :: Context Meta -> Y.Maybe String -> Type Meta -> Type Meta
setTypeDescription cx d = setTypeAnnotation cx metaDescription (string <$> d)

termMetaInternal :: Term Meta -> Meta
termMetaInternal = aggregateAnnotations $ \t -> case t of
  TermAnnotated a -> Just a
  _ -> Nothing

typeMetaInternal :: Type Meta -> Meta
typeMetaInternal = aggregateAnnotations $ \t -> case t of
  TypeAnnotated a -> Just a
  _ -> Nothing
