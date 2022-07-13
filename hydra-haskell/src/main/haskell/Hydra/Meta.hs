module Hydra.Meta where

import Hydra.Core
import Hydra.Evaluation
import Hydra.CoreDecoding
import Hydra.CoreEncoding
import Hydra.Impl.Haskell.Dsl.Terms
import Hydra.Common

import qualified Data.Map as M
import qualified Data.Maybe as Y


getAnnotation :: String -> Meta -> Maybe (Term Meta)
getAnnotation key (Meta m) = M.lookup key m

getDescription :: Meta -> Result (Y.Maybe String)
getDescription meta = case getAnnotation metaDescription meta of
  Nothing -> pure Nothing
  Just term -> case term of
    TermLiteral (LiteralString s) -> pure $ Just s
    _ -> fail $ "unexpected value for " ++ show metaDescription ++ ": " ++ show term

getTermAnnotation :: Context Meta -> String -> Term Meta -> Y.Maybe (Term Meta)
getTermAnnotation cx key = getAnnotation key . termMeta cx

getTermDescription :: Context Meta -> Term Meta -> Result (Y.Maybe String)
getTermDescription cx = getDescription . termMeta cx

getType :: Context Meta -> Meta -> Result (Y.Maybe (Type Meta))
getType cx meta = case getAnnotation metaType meta of
  Nothing -> pure Nothing
  Just dat -> Just <$> decodeType cx dat

getTypeDescription :: Context Meta -> Type Meta -> Result (Y.Maybe String)
getTypeDescription cx = getDescription . typeMeta cx

metaAnnotationClass :: AnnotationClass Meta
metaAnnotationClass = AnnotationClass {
    annotationClassDefault = Meta M.empty,
    annotationClassEqual = (==),
    annotationClassCompare = \m1 m2 -> toComparison $ m1 `compare` m2,
    annotationClassShow = show,
    annotationClassRead = read,
    
    -- TODO: simplify
    annotationClassTermDescription = getTermDescription,
    annotationClassTypeDescription = getTypeDescription,
    annotationClassTermType = \cx t -> getType cx $ termMeta cx t,
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
    term' = termExpr term
    meta = setAnnotation key val $ termMeta cx term

setTermDescription :: Context Meta -> Y.Maybe String -> Term Meta -> Term Meta
setTermDescription cx d = setTermAnnotation cx metaDescription (string <$> d)

setTermType :: Context Meta -> Y.Maybe (Type Meta) -> Term Meta -> Term Meta
setTermType cx d = setTermAnnotation cx metaType (encodeType cx <$> d)

setType :: Context Meta -> Y.Maybe (Type Meta) -> Meta -> Meta
setType cx d = setAnnotation metaType (encodeType cx <$> d)

setTypeAnnotation :: Context Meta -> String -> Y.Maybe (Term Meta) -> Type Meta -> Type Meta
setTypeAnnotation cx key val typ = if meta == annotationClassDefault (contextAnnotations cx)
    then typ'
    else TypeAnnotated $ Annotated typ' meta
  where
    typ' = typeExpr typ
    meta = setAnnotation key val $ typeMeta cx typ

setTypeDescription :: Context Meta -> Y.Maybe String -> Type Meta -> Type Meta
setTypeDescription cx d = setTypeAnnotation cx metaDescription (string <$> d)
