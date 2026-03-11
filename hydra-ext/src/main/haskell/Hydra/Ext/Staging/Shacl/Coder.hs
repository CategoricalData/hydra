module Hydra.Ext.Staging.Shacl.Coder where

import Hydra.Kernel
import Hydra.Ext.Rdf.Utils
import qualified Hydra.Ext.Org.W3.Rdf.Syntax as Rdf
import qualified Hydra.Ext.Org.W3.Shacl.Model as Shacl
import qualified Hydra.Decode.Core as DecodeCore
import qualified Hydra.Extract.Core as ExtractCore

import qualified Control.Monad as CM
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Maybe as Y


type Result a = Either (InContext OtherError) a

err :: Context -> String -> Result a
err cx msg = Left (InContext (OtherError msg) cx)

unexpectedE :: Context -> String -> String -> Result a
unexpectedE cx expected found = err cx $ "Expected " ++ expected ++ ", found: " ++ found

shaclCoder :: Module -> Context -> Graph -> Result (Shacl.ShapesGraph, Context)
shaclCoder mod cx g = do
    -- Note: untested since deprecation of element schemas
    let typeEls = L.filter isNativeType $ moduleElements mod
    shapes <- CM.mapM (toShape g) typeEls
    let sg = Shacl.ShapesGraph $ S.fromList shapes
    return (sg, cx)
  where
    toShape g el = do
      typ <- case DecodeCore.type_ g $ bindingTerm el of
        Left de -> err cx (unDecodingError de)
        Right t -> Right t
      cp <- encodeType typ cx
      return $ Shacl.Definition (elementIri el) $ Shacl.ShapeNode $ Shacl.NodeShape cp

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

elementIri :: Binding -> Rdf.Iri
elementIri = nameToIri . bindingName

encodeField :: Name -> Rdf.Resource -> Field -> Context -> Graph -> Either (InContext OtherError) ([Rdf.Triple], Context)
encodeField rname subject field cx g = do
  let (node, cx1) = nextBlankNode cx
  (descs, cx2) <- encodeTerm node (fieldTerm field) cx1 g
  return (triplesOf descs ++
    forObjects subject (propertyIri rname $ fieldName field) (subjectsOf descs), cx2)

encodeFieldType :: Name -> Maybe Integer -> FieldType -> Context -> Result (Shacl.Definition Shacl.PropertyShape)
encodeFieldType rname order (FieldType fname ft) cx = do
    shape <- forType (Just 1) (Just 1) ft
    return $ Shacl.Definition iri shape
  where
    iri = propertyIri rname fname
    forType mn mx t = case deannotateType t of
      TypeMaybe ot -> forType (Just 0) mx ot
      TypeSet st -> forType mn Nothing st
      _ -> do
        cp <- encodeType t cx
        let baseProp = property iri
        return $ baseProp {
          Shacl.propertyShapeCommon = cp,
          Shacl.propertyShapeConstraints = S.fromList $ Y.catMaybes [
            Shacl.PropertyShapeConstraintMinCount <$> mn,
            Shacl.PropertyShapeConstraintMaxCount <$> mx],
          Shacl.propertyShapeOrder = order}

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

encodeTerm :: Rdf.Resource -> Term -> Context -> Graph -> Either (InContext OtherError) ([Rdf.Description], Context)
encodeTerm subject term cx g = case term of
  TermAnnotated (AnnotatedTerm inner _ann) -> encodeTerm subject inner cx g -- TODO: extract an rdfs:comment
  TermList terms -> encodeList subject terms cx
    where
      encodeList subj terms cx0 = if L.null terms
        then Right ([emptyDescription $ (Rdf.NodeIri $ rdfIri "nil")], cx0)
          else do
            let (node, cx1) = nextBlankNode cx0
            (fdescs, cx2) <- encodeTerm node (L.head terms) cx1 g
            let firstTriples = triplesOf fdescs ++
                  forObjects subj (rdfIri "first") (subjectsOf fdescs)
            let (next, cx3) = nextBlankNode cx2
            (rdescs, cx4) <- encodeList next (L.tail terms) cx3
            let restTriples = triplesOf rdescs ++
                  forObjects subj (rdfIri "rest") (subjectsOf rdescs)
            return ([Rdf.Description (resourceToNode subj) (Rdf.Graph $ S.fromList $ firstTriples ++ restTriples)], cx4)
  TermLiteral lit -> do
    let node = Rdf.NodeLiteral $ encodeLiteral lit
    return ([emptyDescription node], cx)
  TermMap m -> do
      (tripless, cxFinal) <- foldAccumResult (\cx0 kv -> forKeyVal subject kv cx0) cx (M.toList m)
      return ([Rdf.Description (resourceToNode subject) $ Rdf.Graph $ S.fromList $ L.concat tripless], cxFinal)
    where
      forKeyVal subj (k, v) cx0 = do
        -- Note: only string-valued keys are supported
        ks <- ExtractCore.string cx0 g $ deannotateTerm k
        let (node, cx1) = nextBlankNode cx0
        (descs, cx2) <- encodeTerm node v cx1 g
        let pred = keyIri ks
        let objs = subjectsOf descs
        let triples = forObjects subj pred objs
        return (triples ++ triplesOf descs, cx2)
  TermWrap (WrappedTerm name inner) -> do
    (descs, cx1) <- encodeTerm subject inner cx g
    return ((withType name $ L.head descs):(L.tail descs), cx1)
  TermMaybe mterm -> case mterm of
    Nothing -> Right ([], cx)
    Just inner -> encodeTerm subject inner cx g
  TermRecord (Record rname fields) -> do
    (tripless, cxFinal) <- foldAccumResult (\cx0 field -> encodeField rname subject field cx0 g) cx fields
    return ([withType rname $ Rdf.Description (resourceToNode subject) (Rdf.Graph $ S.fromList $ L.concat tripless)], cxFinal)
  TermSet terms -> do
    (descss, cxFinal) <- foldAccumResult (\cx0 t -> do
      let (node, cx1) = nextBlankNode cx0
      encodeTerm node t cx1 g) cx (S.toList terms)
    return (L.concat descss, cxFinal)
  TermUnion (Injection rname field) -> do
    (triples, cx1) <- encodeField rname subject field cx g
    return ([withType rname $ Rdf.Description (resourceToNode subject) (Rdf.Graph $ S.fromList triples)], cx1)
  _ -> unexpectedE cx "RDF-compatible term" $ show term

-- | Fold over a list, accumulating results and threading context
foldAccumResult :: (Context -> a -> Either (InContext OtherError) (b, Context)) -> Context -> [a] -> Either (InContext OtherError) ([b], Context)
foldAccumResult _ cx [] = Right ([], cx)
foldAccumResult f cx (x:xs) = do
  (b, cx1) <- f cx x
  (bs, cx2) <- foldAccumResult f cx1 xs
  return (b:bs, cx2)

encodeType :: Type -> Context -> Result Shacl.CommonProperties
encodeType typ cx = case deannotateType typ of
    TypeList _ -> any
    TypeLiteral lt -> pure $ encodeLiteralType lt
    TypeMap _ -> any
    TypeWrap name -> any -- TODO: include name
    TypeRecord (RowType rname fields) -> do
      props <- CM.zipWithM (\order ft -> encodeFieldType rname order ft cx) (Just <$> [0..]) fields
      return $ common [Shacl.CommonConstraintProperty $ S.fromList (Shacl.ReferenceDefinition <$> props)]
    TypeSet _ -> any
    TypeUnion (RowType rname fields) -> do
        props <- CM.mapM (\ft -> encodeFieldType rname Nothing ft cx) fields
        let shapes = (Shacl.ReferenceAnonymous . toShape) <$> props
        return $ common [Shacl.CommonConstraintXone $ S.fromList shapes]
      where
        toShape prop = node [Shacl.CommonConstraintProperty $ S.fromList [Shacl.ReferenceDefinition prop]]
    _ -> unexpectedE cx "type" $ show typ
  where
    -- SHACL's built-in vocabulary is less expressive than Hydra's type system, so for now, SHACL validation simply ends
    -- when inexpressible types are encountered. However, certain constructs such as lists can be validated using
    -- secondary structures. For example, see shsh:ListShape in the SHACL documentation. TODO: explore these constructions.
    any = pure $ common []

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

withType :: Name -> Rdf.Description -> Rdf.Description
withType name (Rdf.Description subj (Rdf.Graph triples)) = Rdf.Description subj (Rdf.Graph $ S.insert triple triples)
  where
    subjRes = case subj of
      Rdf.NodeIri iri -> Rdf.ResourceIri iri
      Rdf.NodeBnode bnode -> Rdf.ResourceBnode bnode
    triple = Rdf.Triple subjRes (rdfIri "type") (Rdf.NodeIri $ nameToIri name)
