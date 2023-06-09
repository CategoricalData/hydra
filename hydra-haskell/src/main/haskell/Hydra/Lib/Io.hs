-- | Haskell implementations of hydra/lib/io primitives

module Hydra.Lib.Io (
  showTerm,
  showType,
  hydraCore,
  termJsonCoder,
  termStringCoder
) where

import Hydra.Kernel
import Hydra.Langs.Json.Coder
import Hydra.Dsl.Annotations
import Hydra.Langs.Json.Serde
import qualified Hydra.Langs.Json.Model as Json
import qualified Hydra.Dsl.Types as Types

import qualified Data.Map as M
import qualified Data.Maybe as Y


showTerm :: Term a -> String
showTerm term = fromFlow hydraCore $ coderEncode termStringCoder encoded
  where
    encoded = sigmaEncodeTerm $ rewriteTermMeta (const $ Kv M.empty) term

termJsonCoder :: Coder (Graph Kv) (Graph Kv) (Term Kv) Json.Value
termJsonCoder = fromFlow hydraCore $ jsonCoder $ TypeVariable _Term

termStringCoder :: Coder (Graph Kv) (Graph Kv) (Term Kv) String
termStringCoder = Coder mout min
  where
    mout term = jsonValueToString <$> coderEncode termJsonCoder term
    min s = case stringToJsonValue s of
      Left msg -> fail $ "failed to parse JSON value: " ++ msg
      Right v -> coderDecode termJsonCoder v

showType :: Ord a => Type a -> String
showType typ = fromFlow hydraCore $ coderEncode typeStringCoder encoded
  where
    encoded = epsilonEncodeType $ rewriteTypeMeta (const $ Kv M.empty) typ

typeJsonCoder :: Coder (Graph Kv) (Graph Kv) (Term Kv) Json.Value
typeJsonCoder = fromFlow hydraCore $ jsonCoder $ TypeVariable _Type

typeStringCoder :: Coder (Graph Kv) (Graph Kv) (Term Kv) String
typeStringCoder = Coder mout min
  where
    mout term = jsonValueToString <$> coderEncode typeJsonCoder term
    min s = case stringToJsonValue s of
      Left msg -> fail $ "failed to parse as JSON value: " ++ msg
      Right v -> coderDecode typeJsonCoder v
