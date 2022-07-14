module Hydra.Lib.Io (
  showTerm,
  showType,
  coreContext,
) where

import Hydra.Core
import Hydra.Graph
import Hydra.Evaluation
import Hydra.Ext.Json.Coder
import qualified Hydra.Ext.Json.Model as Json
import Hydra.Impl.Haskell.Dsl.Standard
import qualified Hydra.Impl.Haskell.Dsl.Types as Types
import Hydra.Errors
import Hydra.Impl.Haskell.Ext.Json.Serde
import Hydra.Rewriting
import Hydra.CoreEncoding
import Hydra.Impl.Haskell.Sources.Core
import Hydra.Primitives
import Hydra.Impl.Haskell.Sources.Libraries

import qualified Data.Map as M
import qualified Data.Maybe as Y


coreContext :: Context Meta
coreContext = standardContext {
  contextGraphs = GraphSet {
    graphSetGraphs = M.fromList [
      (hydraCoreName, hydraCore)],
    graphSetRoot = hydraCoreName},
  contextElements = graphElementsMap hydraCore}

showTerm :: Ord m => Term m -> String
showTerm term = case coderEncode termStringCoder encoded of
    ResultSuccess s -> s
  where
    encoded = encodeTerm coreContext $ rewriteTermMeta (const $ Meta M.empty) term

termJsonCoder :: Coder (Term Meta) Json.Value
termJsonCoder = Y.fromJust $ qualifiedValue $ jsonCoder coreContext $ Types.nominal _Term

termStringCoder :: Coder (Term Meta) String
termStringCoder = Coder mout min
  where
    mout term = valueToString <$> coderEncode termJsonCoder term
    min s = case stringToValue s of
      Nothing -> fail $ "failed to parse as JSON value: " ++ s
      Just v -> coderDecode termJsonCoder v

showType :: Ord m => Type m -> String
showType typ = case coderEncode typeStringCoder encoded of
  ResultSuccess s -> s
  where
    encoded = encodeType coreContext $ rewriteTypeMeta (const $ Meta M.empty) typ

typeJsonCoder :: Coder (Term Meta) Json.Value
typeJsonCoder = Y.fromJust $ qualifiedValue $ jsonCoder coreContext $ Types.nominal _Type

typeStringCoder :: Coder (Term Meta) String
typeStringCoder = Coder mout min
  where
    mout term = valueToString <$> coderEncode typeJsonCoder term
    min s = case stringToValue s of
      Nothing -> fail $ "failed to parse as JSON value: " ++ s
      Just v -> coderDecode typeJsonCoder v
