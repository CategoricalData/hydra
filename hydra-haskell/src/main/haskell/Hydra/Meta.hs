module Hydra.Meta where

import Hydra.Core
import Hydra.Evaluation
import Hydra.CoreDecoding
import Hydra.CoreEncoding
import Hydra.Impl.Haskell.Dsl.Terms
import Hydra.Impl.Haskell.Default
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

getTermAnnotation :: String -> Term Meta -> Y.Maybe (Term Meta)
getTermAnnotation key = getAnnotation key . termMeta

getTermDescription :: Term Meta -> Result (Y.Maybe String)
getTermDescription = getDescription . termMeta

getType :: Context Meta -> Meta -> Result (Y.Maybe (Type Meta))
getType cx meta = case getAnnotation metaType meta of
  Nothing -> pure Nothing
  Just dat -> Just <$> decodeType cx dat

getTypeDescription :: Type Meta -> Result (Y.Maybe String)
getTypeDescription = getDescription . typeMeta

metaDescription :: String
metaDescription = "description"

metaType :: String
metaType = "type"

setAnnotation :: String -> Y.Maybe (Term Meta) -> Meta -> Meta
setAnnotation key val (Meta m) = Meta $ M.alter (const val) key m

setDescription :: Y.Maybe String -> Meta -> Meta
setDescription d = setAnnotation metaDescription (string <$> d)

setTermAnnotation :: String -> Y.Maybe (Term Meta) -> Term Meta -> Term Meta
setTermAnnotation key val term = if meta == dflt
    then term'
    else TermAnnotated $ Annotated term' meta
  where
    term' = termExpr term
    meta = setAnnotation key val $ termMeta term

setTermDescription :: Y.Maybe String -> Term Meta -> Term Meta
setTermDescription d = setTermAnnotation metaDescription (string <$> d)

setTermType :: Context Meta -> Y.Maybe (Type Meta) -> Term Meta -> Term Meta
setTermType cx d = setTermAnnotation metaType (encodeType cx <$> d)

setType :: Context Meta -> Y.Maybe (Type Meta) -> Meta -> Meta
setType cx d = setAnnotation metaType (encodeType cx <$> d)

setTypeAnnotation :: String -> Y.Maybe (Term Meta) -> Type Meta -> Type Meta
setTypeAnnotation key val typ = if meta == dflt
    then typ'
    else TypeAnnotated $ Annotated typ' meta
  where
    typ' = typeExpr typ
    meta = setAnnotation key val $ typeMeta typ

setTypeDescription :: Y.Maybe String -> Type Meta -> Type Meta
setTypeDescription d = setTypeAnnotation metaDescription (string <$> d)
