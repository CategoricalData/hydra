-- | Haskell implementations of hydra/lib/io primitives

module Hydra.Lib.Io (
  showTerm,
  showType,
  coreContext,
) where

import Hydra.Kernel
import Hydra.Ext.Json.Coder
import Hydra.Dsl.Standard
import Hydra.Ext.Json.Serde
import qualified Hydra.Ext.Json.Model as Json
import qualified Hydra.Dsl.Types as Types

import qualified Data.Map as M
import qualified Data.Maybe as Y


showTerm :: Ord m => Term m -> String
showTerm term = fromFlow coreContext $ coderEncode termStringCoder encoded
  where
    encoded = sigmaEncodeTerm $ rewriteTermMeta (const $ Kv M.empty) term

termJsonCoder :: Coder (Context Kv) (Context Kv) (Term Kv) Json.Value
termJsonCoder = fromFlow coreContext $ jsonCoder $ Types.wrap _Term

termStringCoder :: Coder (Context Kv) (Context Kv) (Term Kv) String
termStringCoder = Coder mout min
  where
    mout term = valueToString <$> coderEncode termJsonCoder term
    min s = case stringToValue s of
      Left msg -> fail $ "failed to parse JSON value: " ++ msg
      Right v -> coderDecode termJsonCoder v

showType :: Ord m => Type m -> String
showType typ = fromFlow coreContext $ coderEncode typeStringCoder encoded
  where
    encoded = epsilonEncodeType $ rewriteTypeMeta (const $ Kv M.empty) typ

typeJsonCoder :: Coder (Context Kv) (Context Kv) (Term Kv) Json.Value
typeJsonCoder = fromFlow coreContext $ jsonCoder $ Types.wrap _Type

typeStringCoder :: Coder (Context Kv) (Context Kv) (Term Kv) String
typeStringCoder = Coder mout min
  where
    mout term = valueToString <$> coderEncode typeJsonCoder term
    min s = case stringToValue s of
      Left msg -> fail $ "failed to parse as JSON value: " ++ msg
      Right v -> coderDecode typeJsonCoder v
