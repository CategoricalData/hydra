-- | Haskell implementations of hydra/lib/io primitives

module Hydra.Lib.Io (
  showTerm,
  showType,
) where

import Hydra.Kernel
import Hydra.Langs.Json.Coder
import Hydra.Dsl.Annotations
import Hydra.Langs.Json.Serde
import Hydra.Sources.Core
import qualified Hydra.Langs.Json.Model as Json
import qualified Hydra.Dsl.Types as Types

import qualified Data.Map as M
import qualified Data.Maybe as Y


showTerm :: Term a -> String
showTerm term = fromFlow "fail" hydraCore $ do
    coder <- termStringCoder
    coderEncode coder encoded
  where
    encoded = coreEncodeTerm $ rewriteTermMeta (const $ Kv M.empty) term

termStringCoder :: Flow (Graph Kv) (Coder (Graph Kv) (Graph Kv) (Term Kv) String)
termStringCoder = do
    termJsonCoder <- jsonCoder $ TypeVariable _Term
    return $ Coder (mout termJsonCoder) (min termJsonCoder)
  where
    mout termJsonCoder term = jsonValueToString <$> coderEncode termJsonCoder term
    min termJsonCoder s = case stringToJsonValue s of
      Left msg -> fail $ "failed to parse JSON value: " ++ msg
      Right v -> coderDecode termJsonCoder v

showType :: Type a -> String
showType typ = fromFlow "fail" hydraCore $ do
    coder <- typeStringCoder
    coderEncode coder encoded
  where
    encoded = coreEncodeType $ rewriteTypeMeta (const $ Kv M.empty) typ

typeStringCoder :: Flow (Graph Kv) (Coder (Graph Kv) (Graph Kv) (Term Kv) String)
typeStringCoder = do
    typeJsonCoder <- jsonCoder $ TypeVariable _Type
    return $ Coder (mout typeJsonCoder) (min typeJsonCoder)
  where
    mout typeJsonCoder term = jsonValueToString <$> coderEncode typeJsonCoder term
    min typeJsonCoder s = case stringToJsonValue s of
      Left msg -> fail $ "failed to parse as JSON value: " ++ msg
      Right v -> coderDecode typeJsonCoder v
