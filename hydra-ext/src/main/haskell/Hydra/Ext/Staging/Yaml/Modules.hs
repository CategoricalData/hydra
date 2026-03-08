-- | Module-level YAML generation for Hydra terms

module Hydra.Ext.Staging.Yaml.Modules (moduleToYaml) where

import Hydra.Kernel
import Hydra.Ext.Org.Yaml.Coder
import Hydra.Ext.Staging.Yaml.Serde
import qualified Hydra.Ext.Org.Yaml.Model as YM

import qualified Data.List as L
import qualified Data.Map as M


-- | New simple adapter version that works with definitions directly
moduleToYaml :: Module -> [Definition] -> Context -> Graph -> Either (InContext OtherError) (M.Map FilePath String)
moduleToYaml mod defs cx g = do
    let termDefs = [td | DefinitionTerm td <- defs]
    node <- constructModule mod termDefs cx g
    Right $ M.fromList [(path, hydraYamlToString node)]
  where
    path = namespaceToFilePath CaseConventionCamel (FileExtension "yaml") $ moduleNamespace mod

constructModule :: Module -> [TermDefinition] -> Context -> Graph -> Either (InContext OtherError) YM.Node
constructModule mod termDefs cx g = do
    keyvals <- mapM toYaml termDefs
    Right $ YM.NodeMapping $ M.fromList keyvals
  where
    toYaml (TermDefinition name term typeScheme) = do
      coder <- yamlCoder (typeSchemeType typeScheme) cx g
      node <- coderEncode coder cx term
      Right (YM.NodeScalar $ YM.ScalarStr $ localNameOf name, node)
    ns = unNamespace $ moduleNamespace mod
    localNameOf name = L.drop (1 + L.length ns) $ unName name
