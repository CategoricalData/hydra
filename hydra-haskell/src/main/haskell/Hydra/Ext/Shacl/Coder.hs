module Hydra.Ext.Shacl.Coder where

import Hydra.Common
import Hydra.Core
import Hydra.Evaluation
import Hydra.Graph
import Hydra.Impl.Haskell.Extras
import Hydra.CoreDecoding
import Hydra.Ext.Shacl.Language
import qualified Hydra.Impl.Haskell.Dsl.Types as Types
import qualified Hydra.Ext.Rdf.Syntax as Rdf
import qualified Hydra.Ext.Shacl.Model as Shacl
import Hydra.Util.Coders

import qualified Control.Monad as CM
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S


data Ann a = Ann a (Maybe String)

shaclCoder :: (Default m, Eq m, Show m) => Context m -> Graph m -> Qualified (Shacl.ShapesGraph, Graph m -> Result Rdf.Graph)
shaclCoder cx sg = do
    pairs <- resultToQualified $ CM.mapM decode typeEls
    fail "FOO"
  where
    mapGraph g = fail "TODO"
    typeEls = L.filter (isEncodedType . elementSchema) $ graphElements sg
    decode el = do
      typ <- decodeType cx $ elementData el
      return (el, typ)

encodeType :: Show m => (Element m, Type m) -> Result Shacl.Shape
encodeType (el, typ) = case typeExpr typ of
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


encodeLiteralType :: LiteralType -> Result Shacl.Shape
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
      Shacl.commonPropertiesConstraints = defaultCommonConstraints {
        Shacl.commonConstraintsDatatype = S.fromList [xmlSchemaDatatypeIri local]}}

defaultCommonConstraints :: Shacl.CommonConstraints
defaultCommonConstraints = Shacl.CommonConstraints {
  Shacl.commonConstraintsAnd = [],
  Shacl.commonConstraintsClosed = False,
  Shacl.commonConstraintsDatatype = S.empty,
  Shacl.commonConstraintsHasValue = S.empty,
  Shacl.commonConstraintsIgnoredProperties = [],
  Shacl.commonConstraintsIn = [],
  Shacl.commonConstraintsNode = S.empty,
  Shacl.commonConstraintsNot = S.empty,
  Shacl.commonConstraintsProperty = S.empty,
  Shacl.commonConstraintsOr = [],
  Shacl.commonConstraintsXone = []}

defaultCommonProperties :: Shacl.CommonProperties
defaultCommonProperties = Shacl.CommonProperties {
  Shacl.commonPropertiesConstraints = defaultCommonConstraints,
  Shacl.commonPropertiesDeactivated = False,
  Shacl.commonPropertiesMessage = defaultLangStrings,
  Shacl.commonPropertiesSeverity = Shacl.SeverityInfo,
  Shacl.commonPropertiesTargetClass = S.empty,
  Shacl.commonPropertiesTargetNode = Nothing,
  Shacl.commonPropertiesTargetObjectsOf = S.empty,
  Shacl.commonPropertiesTargetSubjectsOf = S.empty}

defaultLangStrings :: Rdf.LangStrings
defaultLangStrings = Rdf.LangStrings M.empty

xmlSchemaDatatypeIri :: String -> Rdf.Iri
xmlSchemaDatatypeIri local = Rdf.Iri $ "http://www.w3.org/2001/XMLSchema#" ++ local
