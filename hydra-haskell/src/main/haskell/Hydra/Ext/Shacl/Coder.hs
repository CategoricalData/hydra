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
import qualified Data.Maybe as Y


shaclCoder :: (Eq m, Show m) => Graph m -> GraphFlow m (Shacl.ShapesGraph, Graph m -> GraphFlow m Rdf.Graph)
shaclCoder sg = do
    cx <- getState
    let typeEls = L.filter (isEncodedType cx . elementSchema) $ graphElements sg
    shapes <- CM.mapM toShape typeEls
    let sg = Shacl.ShapesGraph $ S.fromList shapes
    let termFlow = \g -> fail "not implemented"
    return (sg, termFlow)
  where
    toShape el = do
      typ <- decodeType $ elementData el
      common <- encodeType typ
      return $ Shacl.Definition (elementIri el) $ Shacl.ShapeNode $ Shacl.NodeShape common

common :: [Shacl.CommonConstraint] -> Shacl.CommonProperties
common constraints = defaultCommonProperties {
  Shacl.commonPropertiesConstraints = S.fromList constraints}

defaultCommonProperties :: Shacl.CommonProperties
defaultCommonProperties = Shacl.CommonProperties {
  Shacl.commonPropertiesConstraints = S.empty,
  Shacl.commonPropertiesDeactivated = Nothing,
  Shacl.commonPropertiesMessage = emptyLangStrings,
  Shacl.commonPropertiesSeverity = Shacl.SeverityInfo,
  Shacl.commonPropertiesTargetClass = S.empty,
  Shacl.commonPropertiesTargetNode = S.empty,
  Shacl.commonPropertiesTargetObjectsOf = S.empty,
  Shacl.commonPropertiesTargetSubjectsOf = S.empty}

elementIri :: Element m -> Rdf.Iri
elementIri = nameToIri . elementName

emptyDescription :: Rdf.Node -> Rdf.Description
emptyDescription node = Rdf.Description node emptyGraph

emptyGraph :: Rdf.Graph
emptyGraph = Rdf.Graph S.empty

emptyLangStrings :: Rdf.LangStrings
emptyLangStrings = Rdf.LangStrings M.empty

encodeType :: Show m => Type m -> GraphFlow m Shacl.CommonProperties
encodeType typ = case stripType typ of
    TypeElement et -> encodeType et
    TypeList _ -> any
    TypeLiteral lt -> pure $ encodeLiteralType lt
    TypeMap _ -> any
    TypeNominal name -> any -- TODO: include name
    TypeRecord (RowType rname fields) -> do
      props <- CM.zipWithM (encodeFieldType rname) (Just <$> [0..]) fields
      return $ common [Shacl.CommonConstraintProperty $ S.fromList (Shacl.ReferenceDefinition <$> props)]
    TypeSet _ -> any
    TypeUnion (RowType rname fields) -> do
        props <- CM.mapM (encodeFieldType rname Nothing) fields
        let shapes = (Shacl.ReferenceAnonymous . toShape) <$> props
        return $ common [Shacl.CommonConstraintXone $ S.fromList shapes]
      where
        toShape prop = node [Shacl.CommonConstraintProperty $ S.fromList [Shacl.ReferenceDefinition prop]]
    _ -> unexpected "type" typ
  where
    -- SHACL's built-in vocabulary is less expressive than Hydra's type system, so for now, SHACL validation simply ends
    -- when inexpressible types are encountered. However, certain constructs such as lists can be validated using
    -- secondary structures. For example, see shsh:ListShape in the SHACL documentation. TODO: explore these constructions.
    any = pure $ common []

encodeFieldType :: Show m => Name -> Maybe Integer -> FieldType m -> GraphFlow m (Shacl.Definition Shacl.PropertyShape)
encodeFieldType rname order (FieldType fname ft) = do
    shape <- forType (Just 1) (Just 1) ft
    return $ Shacl.Definition iri shape
  where
    iri = propertyIri rname fname
    forType mn mx t = case stripType t of
      TypeOptional ot -> forType (Just 0) mx ot
      TypeSet st -> forType mn Nothing st
      _ -> do
        cp <- encodeType t
        let baseProp = property iri
        return $ baseProp {
          Shacl.propertyShapeCommon = cp,
          Shacl.propertyShapeConstraints = S.fromList $ Y.catMaybes [
            Shacl.PropertyShapeConstraintMinCount <$> mn,
            Shacl.PropertyShapeConstraintMaxCount <$> mx],
          Shacl.propertyShapeOrder = order}

encodeLiteral :: Literal -> GraphFlow m Rdf.Node
encodeLiteral lit = Rdf.NodeLiteral <$> case lit of
    LiteralBinary s -> fail "base 64 encoding not yet implemented"
    LiteralBoolean b -> pure $ xsd (\b -> if b then "true" else "false") b "boolean"
    LiteralFloat f -> pure $ case f of
      FloatValueBigfloat v -> xsd show v "decimal"
      FloatValueFloat32 v -> xsd show v "float"
      FloatValueFloat64 v -> xsd show v "double"
    LiteralInteger i -> pure $ case i of
      IntegerValueBigint v -> xsd show v "integer"
      IntegerValueInt8 v   -> xsd show v "byte"
      IntegerValueInt16 v  -> xsd show v "short"
      IntegerValueInt32 v  -> xsd show v "int"
      IntegerValueInt64 v  -> xsd show v "long"
      IntegerValueUint8 v  -> xsd show v "unsignedByte"
      IntegerValueUint16 v -> xsd show v "unsignedShort"
      IntegerValueUint32 v -> xsd show v "unsignedInt"
      IntegerValueUint64 v -> xsd show v "unsignedLong"
    LiteralString s -> pure $ xsd id s "string"
  where
    -- TODO: using Haskell's built-in show function is a cheat, and may not be correct/optimal in all cases
    xsd ser x local = Rdf.Literal (ser x) (xmlSchemaDatatypeIri local) Nothing

encodeLiteralType :: LiteralType -> Shacl.CommonProperties
encodeLiteralType lt = case lt of
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
    xsd local = common [Shacl.CommonConstraintDatatype $ xmlSchemaDatatypeIri local]

--encodeTerm :: Term m -> GraphFlow m Rdf.Description
--encodeTerm term = case term of
--  TermAnnotated (Annotated term' ann) -> encodeTerm term' -- TODO: extract an rdfs:comment
----  TermApplication
--  TermElement name -> pure $ emptyDescription $ Rdf.NodeIri $ nameToIri name
----  TermFunction
----  TermLet
----  TermList
--  TermLiteral lit -> emptyDescription <$> encodeLiteral lit
----  TermMap
----  TermNominal
----  TermOptional
--  TermRecord (Record rname fields) ->
----  TermSet
----  TermUnion
----  TermVariable

nameToIri :: Name -> Rdf.Iri
nameToIri = Rdf.Iri . unName

node :: [Shacl.CommonConstraint] -> Shacl.Shape
node = Shacl.ShapeNode . Shacl.NodeShape . common

property :: Rdf.Iri -> Shacl.PropertyShape
property iri = Shacl.PropertyShape {
    Shacl.propertyShapeCommon = defaultCommonProperties,
    Shacl.propertyShapeConstraints = S.empty,
    Shacl.propertyShapeDefaultValue = Nothing,
    Shacl.propertyShapeDescription = emptyLangStrings,
    Shacl.propertyShapeName = emptyLangStrings,
    Shacl.propertyShapeOrder = Nothing,
    Shacl.propertyShapePath = iri}

propertyIri :: Name -> FieldName -> Rdf.Iri
propertyIri rname fname = Rdf.Iri $ (unName rname) ++ "#" ++ (unFieldName fname)

xmlSchemaDatatypeIri :: String -> Rdf.Iri
xmlSchemaDatatypeIri local = Rdf.Iri $ "http://www.w3.org/2001/XMLSchema#" ++ local
