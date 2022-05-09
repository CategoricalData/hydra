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

getDescription :: Meta -> Result (Y.Maybe String)
getDescription (Meta m) = case M.lookup metaDescription m of
  Nothing -> pure Nothing
  Just (Data term _) -> case term of
    DataTermLiteral (LiteralString s) -> pure $ Just s
    _ -> fail $ "unexpected value for " ++ show metaDescription ++ ": " ++ show term

setDescription :: Y.Maybe String -> Meta -> Meta
setDescription d (Meta m) = Meta $ M.alter (\_ -> stringValue <$> d) metaDescription m

getType :: Context Meta -> Meta -> Result (Y.Maybe (Type Meta))
getType cx (Meta m) = case M.lookup metaType m of
  Nothing -> pure Nothing
  Just dat -> Just <$> decodeType cx dat

setType :: Context Meta -> Y.Maybe (Type Meta) -> Meta -> Meta
setType cx d (Meta m) = Meta $ M.alter (\_ -> fmap (\t -> encodeType cx t) d) metaType m
