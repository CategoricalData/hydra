-- | Haskell implementations of hydra/lib/io primitives

module Hydra.Lib.Io (
  showTerm,
  showType,
  coreContext,
) where

import Hydra.Kernel
import Hydra.Ext.Json.Coder
import qualified Hydra.Ext.Json.Model as Json
import Hydra.Dsl.Standard
import qualified Hydra.Dsl.Types as Types
import Hydra.Ext.Json.Serde
import Hydra.CoreEncoding

import qualified Data.Map as M
import qualified Data.Maybe as Y


showTerm :: Ord m => Term m -> String
showTerm term = fromFlow coreContext $ coderEncode termStringCoder encoded
  where
    encoded = encodeTerm $ rewriteTermMeta (const $ Meta M.empty) term

termJsonCoder :: Coder (Context Meta) (Context Meta) (Term Meta) Json.Value
termJsonCoder = fromFlow coreContext $ jsonCoder $ Types.wrap _Term

termStringCoder :: Coder (Context Meta) (Context Meta) (Term Meta) String
termStringCoder = Coder mout min
  where
    mout term = valueToString <$> coderEncode termJsonCoder term
    min s = case stringToValue s of
      Left msg -> fail $ "failed to parse JSON value: " ++ msg
      Right v -> coderDecode termJsonCoder v

showType :: Ord m => Type m -> String
showType typ = fromFlow coreContext $ coderEncode typeStringCoder encoded
  where
    encoded = encodeType $ rewriteTypeMeta (const $ Meta M.empty) typ

typeJsonCoder :: Coder (Context Meta) (Context Meta) (Term Meta) Json.Value
typeJsonCoder = fromFlow coreContext $ jsonCoder $ Types.wrap _Type

typeStringCoder :: Coder (Context Meta) (Context Meta) (Term Meta) String
typeStringCoder = Coder mout min
  where
    mout term = valueToString <$> coderEncode typeJsonCoder term
    min s = case stringToValue s of
      Left msg -> fail $ "failed to parse as JSON value: " ++ msg
      Right v -> coderDecode typeJsonCoder v
