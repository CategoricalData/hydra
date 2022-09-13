module Hydra.Ext.Shacl.Coder where

import Hydra.Common
import Hydra.Core
import Hydra.Evaluation
import Hydra.Graph
import Hydra.Monads
import Hydra.CoreDecoding
import Hydra.Lexical
import qualified Hydra.Ext.Rdf.Syntax as Rdf
import qualified Hydra.Ext.Shacl.Model as Shacl

import qualified Control.Monad as CM
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S


data Ann a = Ann a (Maybe String)

shaclCoder :: (Eq m, Show m) => Graph m -> GraphFlow m (Shacl.ShapesGraph, Graph m -> GraphFlow m Rdf.Graph)
shaclCoder sg = do
    cx <- getState
    let typeEls = L.filter (isEncodedType cx . elementSchema) $ graphElements sg
    pairs <- CM.mapM decode typeEls
    fail "TODO"
  where
    mapGraph g = fail "TODO"
    decode el = do
      typ <- decodeType $ elementData el
      return (el, typ)

encodeType :: Show m => (Element m, Type m) -> GraphFlow m Shacl.Shape
encodeType (el, typ) = case stripType typ of
--  TypeElement et ->
--  TypeList lt ->
  TypeLiteral lt -> encodeLiteralType lt
--  TypeMap (MapType kt vt) ->
--  TypeNominal name ->
--  TypeOptional ot ->
--  TypeRecord fields ->
--  TypeSet st ->
--  TypeUnion fields ->
  _ -> unexpected "type" typ

encodeLiteralType :: LiteralType -> GraphFlow m Shacl.Shape
encodeLiteralType lt = Shacl.ShapeNode . Shacl.NodeShape <$> case lt of
    LiteralTypeBinary -> xsd "base64Binary"
    LiteralTypeBoolean -> xsd "boolean"
    LiteralTypeFloat ft -> case ft of
      FloatTypeBigfloat -> xsd "decimal"
      FloatTypeFloat32 -> xsd "float"
      FloatTypeFloat64 -> xsd "double"
    LiteralTypeInteger it -> case it of
      IntegerTypeBigint -> xsd "integer"
      IntegerTypeInt8 -> xsd "byte"
      IntegerTypeInt16 -> xsd "short"
      IntegerTypeInt32 -> xsd "int"
      IntegerTypeInt64 -> xsd "long"
      IntegerTypeUint8 -> xsd "unsignedByte"
      IntegerTypeUint16 -> xsd "unsignedShort"
      IntegerTypeUint32 -> xsd "unsignedInt"
      IntegerTypeUint64 -> xsd "unsignedLong"
    LiteralTypeString -> xsd "string"
  where
    xsd local = pure $ defaultCommonProperties {
      Shacl.commonPropertiesConstraints = [Shacl.CommonConstraintDatatype $ xmlSchemaDatatypeIri local]}

defaultCommonProperties :: Shacl.CommonProperties
defaultCommonProperties = Shacl.CommonProperties {
  Shacl.commonPropertiesConstraints = [],
  Shacl.commonPropertiesDeactivated = Nothing,
  Shacl.commonPropertiesMessage = defaultLangStrings,
  Shacl.commonPropertiesSeverity = Shacl.SeverityInfo,
  Shacl.commonPropertiesTargetClass = S.empty,
  Shacl.commonPropertiesTargetNode = S.empty,
  Shacl.commonPropertiesTargetObjectsOf = S.empty,
  Shacl.commonPropertiesTargetSubjectsOf = S.empty}

defaultLangStrings :: Rdf.LangStrings
defaultLangStrings = Rdf.LangStrings M.empty

xmlSchemaDatatypeIri :: String -> Rdf.Iri
xmlSchemaDatatypeIri local = Rdf.Iri $ "http://www.w3.org/2001/XMLSchema#" ++ local
