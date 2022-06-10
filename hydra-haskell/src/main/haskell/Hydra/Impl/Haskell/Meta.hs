module Hydra.Impl.Haskell.Meta where

import Hydra.Core
import Hydra.Evaluation
import Hydra.CoreDecoding
import Hydra.CoreEncoding
import Hydra.Impl.Haskell.Dsl.Terms

import qualified Data.Map as M
import qualified Data.Maybe as Y


metaDescription :: String
metaDescription = "description"

metaType :: String
metaType = "type"

getAnnotation :: String -> Meta -> Maybe (Term Meta)
getAnnotation key (Meta m) = M.lookup key m

getDescription :: Meta -> Result (Y.Maybe String)
getDescription meta = case getAnnotation metaDescription meta of
  Nothing -> pure Nothing
  Just (Term term _) -> case term of
    TermExprLiteral (LiteralString s) -> pure $ Just s
    _ -> fail $ "unexpected value for " ++ show metaDescription ++ ": " ++ show term

getType :: Context Meta -> Meta -> Result (Y.Maybe (Type Meta))
getType cx meta = case getAnnotation metaType meta of
  Nothing -> pure Nothing
  Just dat -> Just <$> decodeType cx dat

setAnnotation :: String -> Y.Maybe (Term Meta) -> Meta -> Meta
setAnnotation key val (Meta m) = Meta $ M.alter (const val) key m

setDescription :: Y.Maybe String -> Meta -> Meta
setDescription d = setAnnotation metaDescription (stringValue <$> d)

setType :: Context Meta -> Y.Maybe (Type Meta) -> Meta -> Meta
setType cx d = setAnnotation metaType (encodeType cx <$> d)
