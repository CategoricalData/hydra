-- | Module-level YAML generation for Hydra terms

module Hydra.Staging.Yaml.Modules (moduleToYaml) where

import Hydra.Kernel
import Hydra.Staging.Yaml.Coder
import Hydra.Staging.Yaml.Serde
import Hydra.Staging.Yaml.Language
import qualified Hydra.Ext.Org.Yaml.Model as YM
import qualified Hydra.Dsl.Types as Types

import qualified Control.Monad as CM
import qualified Data.List as L
import qualified Data.Map as M


-- | New simple adapter version that works with definitions directly
moduleToYaml :: Module -> [Definition] -> Flow Graph (M.Map FilePath String)
moduleToYaml mod defs = withTrace ("print module " ++ (unNamespace $ moduleNamespace mod)) $ do
    let termDefs = [td | DefinitionTerm td <- defs]
    node <- constructModule mod termDefs
    return $ M.fromList [(path, hydraYamlToString node)]
  where
    path = namespaceToFilePath CaseConventionCamel (FileExtension "yaml") $ moduleNamespace mod

constructModule :: Module -> [TermDefinition] -> Flow Graph YM.Node
constructModule mod termDefs = do
    keyvals <- withTrace "encoding terms" (CM.mapM toYaml termDefs)
    return $ YM.NodeMapping $ M.fromList keyvals
  where
    toYaml (TermDefinition name term typeScheme) = withTrace ("element " ++ unName name) $ do
      coder <- yamlCoder (typeSchemeType typeScheme)
      node <- coderEncode coder term
      return (YM.NodeScalar $ YM.ScalarStr $ localNameOf name, node)
    ns = unNamespace $ moduleNamespace mod
    localNameOf name = L.drop (1 + L.length ns) $ unName name
