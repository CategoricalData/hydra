module Hydra.Ext.Yaml.Modules (printModule) where

import Hydra.All
import Hydra.Adapters.Coders
import Hydra.Impl.Haskell.Ext.Yaml.Serde
import Hydra.Ext.Yaml.Coder
import Hydra.Ext.Yaml.Language
import qualified Hydra.Ext.Yaml.Model as YM
import qualified Hydra.Impl.Haskell.Dsl.Types as Types

import qualified Control.Monad as CM
import qualified Data.List as L
import qualified Data.Map as M


constructModule :: (Ord m, Read m, Show m)
  => Module m
  -> M.Map (Type m) (Coder (Context m) (Context m) (Term m) YM.Node)
  -> [(Element m, TypedTerm m)]
  -> GraphFlow m YM.Node
constructModule mod coders pairs = do
    keyvals <- withTrace "encoding terms" (CM.mapM toYaml pairs)
    return $ YM.NodeMapping $ M.fromList keyvals
  where
    toYaml (el, (TypedTerm typ term)) = withTrace ("element " ++ unName (elementName el)) $ do
      encode <- case M.lookup typ coders of
        Nothing -> fail $ "no coder found for type " ++ show typ
        Just coder -> pure $ coderEncode coder
      node <- encode term
      return (YM.NodeScalar $ YM.ScalarStr $ localNameOf $ elementName el, node)
    ns = unNamespace $ moduleNamespace mod
    localNameOf name = L.drop (1 + L.length ns) $ unName name

printModule :: (Ord m, Read m, Show m) => Module m -> GraphFlow m (M.Map FilePath String)
printModule mod = withTrace ("print module " ++ (unNamespace $ moduleNamespace mod)) $ do
    node <- transformModule language encodeTerm constructModule mod
    return $ M.fromList [(path, hydraYamlToString node)]
  where
    path = namespaceToFilePath False (FileExtension "yaml") $ moduleNamespace mod
    encodeTerm _ = fail $ "only type definitions are expected in this mapping to YAML"
