-- | YAML serialization and deserialization using the HsYAML library

module Hydra.Staging.Yaml.Serde where

import Hydra.Kernel
import Hydra.Staging.Yaml.Coder
import Hydra.Tools.Bytestrings
import qualified Hydra.Ext.Org.Yaml.Model as YM

import qualified Data.ByteString.Lazy as BS
import qualified Data.YAML as DY
import qualified Data.YAML.Event as DYE
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as LB


bytesToHsYaml :: BS.ByteString -> Either String (DY.Node DY.Pos)
bytesToHsYaml bs = case DY.decodeNode bs of
    Left (pos, msg) -> Left $ "YAML parser failure at " ++ show pos ++ ": " ++ msg
    Right docs -> if L.null docs
      then Left "no YAML document"
      else if L.length docs > 1
      then Left "multiple YAML documents"
      else case L.head docs of
        (DY.Doc node) -> Right node

bytesToHydraYaml :: BS.ByteString -> Either String YM.Node
bytesToHydraYaml bs = bytesToHsYaml bs >>= hsYamlToHydraYaml

hsYamlToBytes :: DY.Node () -> BS.ByteString
hsYamlToBytes node = DY.encodeNode [DY.Doc node]

hsYamlToHydraYaml :: DY.Node x -> Either String YM.Node
hsYamlToHydraYaml hs = case hs of
  DY.Scalar _ s -> YM.NodeScalar <$> case s of
     DY.SNull -> Right YM.ScalarNull
     DY.SBool b -> Right $ YM.ScalarBool b
     DY.SFloat f -> Right $ YM.ScalarFloat f
     DY.SInt i -> Right $ YM.ScalarInt i
     DY.SStr t -> Right $ YM.ScalarStr $ T.unpack t
     DY.SUnknown _ _ -> Left "YAML unknown scalars are unsupported"
  DY.Mapping _ _ m -> YM.NodeMapping . M.fromList <$> mapM mapPair (M.toList m)
    where
      mapPair (k, v) = (,) <$> hsYamlToHydraYaml k <*> hsYamlToHydraYaml v
  DY.Sequence _ _ s -> YM.NodeSequence <$> mapM hsYamlToHydraYaml s
  DY.Anchor {} -> Left "YAML anchors are unsupported"

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

hydraYamlToString :: YM.Node -> String
hydraYamlToString = bytesToString . hydraYamlToBytes

-- | YAML byte string coder
yamlByteStringCoder :: Context -> Graph -> Type -> Either (InContext OtherError) (Coder (Term) BS.ByteString)
yamlByteStringCoder cx g typ = do
  coder <- yamlCoder cx g typ
  Right $ Coder
    (\cx' term -> do
      node <- coderEncode coder cx' term
      Right $ hydraYamlToBytes node)
    (\cx' bs -> case bytesToHydraYaml bs of
      Left err -> Left $ InContext (OtherError err) cx'
      Right node -> coderDecode coder cx' node)

-- | YAML string coder
yamlStringCoder :: Context -> Graph -> Type -> Either (InContext OtherError) (Coder (Term) String)
yamlStringCoder cx g typ = do
  coder <- yamlCoder cx g typ
  Right $ Coder
    (\cx' term -> do
      node <- coderEncode coder cx' term
      Right $ hydraYamlToString node)
    (\cx' s -> case bytesToHydraYaml (LB.pack s) of
      Left err -> Left $ InContext (OtherError err) cx'
      Right node -> coderDecode coder cx' node)
