module Hydra.Impl.Haskell.Ext.Yaml.Serde (
  yamlSerde,
  yamlSerdeStr,
) where

import Hydra.Core
import Hydra.Errors
import Hydra.Evaluation
import Hydra.Ext.Yaml.Coder
import Hydra.Impl.Haskell.Dsl.Terms
import Hydra.Impl.Haskell.Extras
import qualified Hydra.Ext.Yaml.Model as YM

import qualified Data.ByteString.Lazy as BS
import qualified Control.Monad as CM
import qualified Data.YAML as DY
import qualified Data.YAML.Event as DYE
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as LB

bytesToHsYaml :: BS.ByteString -> Result (DY.Node DY.Pos)
bytesToHsYaml bs = case DY.decodeNode bs of
    Left (pos, msg) -> fail $ "YAML parser failure at " ++ show pos ++ ": " ++ msg
    Right docs -> if L.null docs
      then fail "no YAML document"
      else if L.length docs > 1
      then fail "multiple YAML documents"
      else case L.head docs of
        (DY.Doc node) -> pure node

bytesToHydraYaml :: BS.ByteString -> Result YM.Node
bytesToHydraYaml = bytesToHsYaml CM.>=> hsYamlToHydraYaml

hsYamlToBytes :: DY.Node () -> BS.ByteString
hsYamlToBytes node = DY.encodeNode [DY.Doc node]

hsYamlToHydraYaml :: DY.Node a -> Result YM.Node
hsYamlToHydraYaml hs = case hs of
  DY.Scalar _ s -> YM.NodeScalar <$> case s of
     DY.SNull -> pure YM.ScalarNull
     DY.SBool b -> pure $ YM.ScalarBool b
     DY.SFloat f -> pure $ YM.ScalarFloat f
     DY.SInt i -> pure $ YM.ScalarInt i
     DY.SStr t -> pure $ YM.ScalarStr $ T.unpack t
     DY.SUnknown _ _ -> fail "YAML unknown scalars are unsupported"
  DY.Mapping _ _ m -> YM.NodeMapping . M.fromList <$> CM.mapM mapPair (M.toList m)
    where
      mapPair (k, v) = (,) <$> hsYamlToHydraYaml k <*> hsYamlToHydraYaml v
  DY.Sequence _ _ s -> YM.NodeSequence <$> CM.mapM hsYamlToHydraYaml s
  DY.Anchor {} -> fail "YAML anchors are unsupported"

hydraYamlToBytes :: YM.Node -> BS.ByteString
hydraYamlToBytes = hsYamlToBytes . hydraYamlToHsYaml

hydraYamlToHsYaml :: YM.Node -> DY.Node ()
hydraYamlToHsYaml hy = case hy of
  YM.NodeMapping m -> DY.Mapping () DYE.untagged $ M.fromList $ mapPair <$> M.toList m
    where
      mapPair (k, v) = (,) (hydraYamlToHsYaml k) (hydraYamlToHsYaml v)
  YM.NodeScalar s -> DY.Scalar () $ case s of
    YM.ScalarBool b -> DY.SBool b
    YM.ScalarFloat f -> DY.SFloat f
    YM.ScalarInt i -> DY.SInt i
    YM.ScalarNull -> DY.SNull
    YM.ScalarStr s -> DY.SStr $ T.pack s
  YM.NodeSequence s -> DY.Sequence () DYE.untagged $ hydraYamlToHsYaml <$> s

yamlSerde :: (Default m, Eq m, Ord m, Read m, Show m) => Context m -> Type m -> Qualified (Step (Data m) BS.ByteString)
yamlSerde context typ = do
  coder <- yamlCoder context typ
  return Step {
    stepOut = fmap hydraYamlToBytes . stepOut coder,
    stepIn = bytesToHydraYaml CM.>=> stepIn coder}

yamlSerdeStr :: (Default m, Eq m, Ord m, Read m, Show m) => Context m -> Type m -> Qualified (Step (Data m) String)
yamlSerdeStr context typ = do
  serde <- yamlSerde context typ
  return Step {
    stepOut = fmap LB.unpack . stepOut serde,
    stepIn = stepIn serde . LB.pack}
