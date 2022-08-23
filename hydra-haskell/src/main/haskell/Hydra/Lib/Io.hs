module Hydra.Lib.Io (
  showTerm,
  showType,
  coreContext,
) where

import Hydra.Core
import Hydra.Evaluation
import Hydra.Ext.Json.Coder
import qualified Hydra.Ext.Json.Model as Json
import Hydra.Impl.Haskell.Dsl.Standard
import qualified Hydra.Impl.Haskell.Dsl.Types as Types
import Hydra.Impl.Haskell.Ext.Json.Serde
import Hydra.Rewriting
import Hydra.CoreEncoding
import Hydra.Monads

import qualified Data.Map as M
import qualified Data.Maybe as Y


showTerm :: Ord m => Term m -> String
showTerm term = fromFlow coreContext $ coderEncode termStringCoder encoded
  where
    encoded = encodeTerm $ rewriteTermMeta (const $ Meta M.empty) term

termJsonCoder :: Coder (Context Meta) (Term Meta) Json.Value
termJsonCoder = fromFlow coreContext $ jsonCoder $ Types.nominal _Term

termStringCoder :: Coder (Context Meta) (Term Meta) String
termStringCoder = Coder mout min
  where
    mout term = valueToString <$> coderEncode termJsonCoder term
    min s = case stringToValue s of
      Nothing -> fail $ "failed to parse as JSON value: " ++ s
      Just v -> coderDecode termJsonCoder v

showType :: Ord m => Type m -> String
showType typ = fromFlow coreContext $ coderEncode typeStringCoder encoded
  where
    encoded = encodeType $ rewriteTypeMeta (const $ Meta M.empty) typ

typeJsonCoder :: Coder (Context Meta) (Term Meta) Json.Value
typeJsonCoder = fromFlow coreContext $ jsonCoder $ Types.nominal _Type

typeStringCoder :: Coder (Context Meta) (Term Meta) String
typeStringCoder = Coder mout min
  where
    mout term = valueToString <$> coderEncode typeJsonCoder term
    min s = case stringToValue s of
      Nothing -> fail $ "failed to parse as JSON value: " ++ s
      Just v -> coderDecode typeJsonCoder v
