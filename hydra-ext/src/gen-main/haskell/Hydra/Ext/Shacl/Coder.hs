-- Note: this is an automatically generated file. Do not edit.

-- | SHACL coder: converts Hydra types and terms to SHACL shapes and RDF descriptions

module Hydra.Ext.Shacl.Coder where

import qualified Hydra.Annotations as Annotations
import qualified Hydra.Context as Context
import qualified Hydra.Core as Core
import qualified Hydra.Decode.Core as Core_
import qualified Hydra.Errors as Errors
import qualified Hydra.Ext.Org.W3.Rdf.Syntax as Syntax
import qualified Hydra.Ext.Org.W3.Shacl.Model as Model
import qualified Hydra.Ext.Rdf.Utils as Utils
import qualified Hydra.Extract.Core as Core__
import qualified Hydra.Graph as Graph
import qualified Hydra.Lib.Eithers as Eithers
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Literals as Literals
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Math as Math
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Pairs as Pairs
import qualified Hydra.Lib.Sets as Sets
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Module as Module
import qualified Hydra.Strip as Strip
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)

-- | Construct CommonProperties from a list of constraints, using defaults for other fields
common :: [Model.CommonConstraint] -> Model.CommonProperties
common constraints =
    Model.CommonProperties {
      Model.commonPropertiesConstraints = (Sets.fromList constraints),
      Model.commonPropertiesDeactivated = Nothing,
      Model.commonPropertiesMessage = (Syntax.LangStrings Maps.empty),
      Model.commonPropertiesSeverity = Model.SeverityInfo,
      Model.commonPropertiesTargetClass = Sets.empty,
      Model.commonPropertiesTargetNode = Sets.empty,
      Model.commonPropertiesTargetObjectsOf = Sets.empty,
      Model.commonPropertiesTargetSubjectsOf = Sets.empty}

-- | Default CommonProperties with empty constraints and default severity
defaultCommonProperties :: Model.CommonProperties
defaultCommonProperties = common []

-- | Convert a binding's name to an RDF IRI
elementIri :: Core.Binding -> Syntax.Iri
elementIri el = Utils.nameToIri (Core.bindingName el)

-- | Encode a record field as RDF triples with a given subject
encodeField :: Core.Name -> Syntax.Resource -> Core.Field -> Context.Context -> Graph.Graph -> Either (Context.InContext Errors.Error) ([Syntax.Triple], Context.Context)
encodeField rname subject field cx g =

      let pair1 = Utils.nextBlankNode cx
          node = Pairs.first pair1
          cx1 = Pairs.second pair1
      in (Eithers.bind (encodeTerm node (Core.fieldTerm field) cx1 g) (\_r1 ->
        let descs = Pairs.first _r1
            cx2 = Pairs.second _r1
        in (Right (Lists.concat2 (Utils.triplesOf descs) (Utils.forObjects subject (Utils.propertyIri rname (Core.fieldName field)) (Utils.subjectsOf descs)), cx2))))

-- | Encode a FieldType as a SHACL property shape Definition
encodeFieldType :: Core.Name -> Maybe Integer -> Core.FieldType -> Context.Context -> Either (Context.InContext Errors.Error) (Model.Definition Model.PropertyShape)
encodeFieldType rname order ft cx =

      let fname = Core.fieldTypeName ft
          ftype = Core.fieldTypeType ft
          iri = Utils.propertyIri rname fname
          forType =
                  \mn -> \mx -> \t -> case (Strip.deannotateType t) of
                    Core.TypeMaybe v0 -> forType (Just 0) mx v0
                    Core.TypeSet v0 -> forType mn Nothing v0
                    _ -> forTypeDefault mn mx t
          forTypeDefault =
                  \mn -> \mx -> \t -> Eithers.map (\_cp ->
                    let baseProp = property iri
                        minC = Maybes.map (\_n -> Model.PropertyShapeConstraintMinCount _n) mn
                        maxC = Maybes.map (\_n -> Model.PropertyShapeConstraintMaxCount _n) mx
                    in Model.Definition {
                      Model.definitionIri = iri,
                      Model.definitionTarget = Model.PropertyShape {
                        Model.propertyShapeCommon = _cp,
                        Model.propertyShapeConstraints = (Sets.fromList (Maybes.cat [
                          minC,
                          maxC])),
                        Model.propertyShapeDefaultValue = Nothing,
                        Model.propertyShapeDescription = (Syntax.LangStrings Maps.empty),
                        Model.propertyShapeName = (Syntax.LangStrings Maps.empty),
                        Model.propertyShapeOrder = order,
                        Model.propertyShapePath = iri}}) (encodeType rname t cx)
      in (forType (Just 1) (Just 1) ftype)

-- | Encode a list of terms as RDF list structure
encodeList :: Syntax.Resource -> [Core.Term] -> Context.Context -> Graph.Graph -> Either (Context.InContext Errors.Error) ([Syntax.Description], Context.Context)
encodeList subj terms cx0 g =
    Logic.ifElse (Lists.null terms) (Right ([
      Syntax.Description {
        Syntax.descriptionSubject = (Syntax.NodeIri (Syntax.Iri "http://www.w3.org/1999/02/22-rdf-syntax-ns#nil")),
        Syntax.descriptionGraph = (Syntax.Graph Sets.empty)}], cx0)) (
      let pair1 = Utils.nextBlankNode cx0
          node1 = Pairs.first pair1
          cx1 = Pairs.second pair1
      in (Eithers.bind (encodeTerm node1 (Lists.head terms) cx1 g) (\_r1 ->
        let fdescs = Pairs.first _r1
            cx2 = Pairs.second _r1
            firstTriples = Lists.concat2 (Utils.triplesOf fdescs) (Utils.forObjects subj (Utils.rdfIri "first") (Utils.subjectsOf fdescs))
            pair2 = Utils.nextBlankNode cx2
            next = Pairs.first pair2
            cx3 = Pairs.second pair2
        in (Eithers.map (\_r2 ->
          let rdescs = Pairs.first _r2
              cx4 = Pairs.second _r2
              restTriples = Lists.concat2 (Utils.triplesOf rdescs) (Utils.forObjects subj (Utils.rdfIri "rest") (Utils.subjectsOf rdescs))
          in ([
            Syntax.Description {
              Syntax.descriptionSubject = (Utils.resourceToNode subj),
              Syntax.descriptionGraph = (Syntax.Graph (Sets.fromList (Lists.concat2 firstTriples restTriples)))}], cx4)) (encodeList next (Lists.tail terms) cx3 g)))))

-- | Encode a LiteralType as SHACL CommonProperties with an XSD datatype constraint
encodeLiteralType :: Core.LiteralType -> Model.CommonProperties
encodeLiteralType lt =

      let xsd = \local -> common [
            Model.CommonConstraintDatatype (Utils.xmlSchemaDatatypeIri local)]
      in case lt of
        Core.LiteralTypeBinary -> xsd "base64Binary"
        Core.LiteralTypeBoolean -> xsd "boolean"
        Core.LiteralTypeFloat v0 -> case v0 of
          Core.FloatTypeBigfloat -> xsd "decimal"
          Core.FloatTypeFloat32 -> xsd "float"
          Core.FloatTypeFloat64 -> xsd "double"
        Core.LiteralTypeInteger v0 -> case v0 of
          Core.IntegerTypeBigint -> xsd "integer"
          Core.IntegerTypeInt8 -> xsd "byte"
          Core.IntegerTypeInt16 -> xsd "short"
          Core.IntegerTypeInt32 -> xsd "int"
          Core.IntegerTypeInt64 -> xsd "long"
          Core.IntegerTypeUint8 -> xsd "unsignedByte"
          Core.IntegerTypeUint16 -> xsd "unsignedShort"
          Core.IntegerTypeUint32 -> xsd "unsignedInt"
          Core.IntegerTypeUint64 -> xsd "unsignedLong"
        Core.LiteralTypeString -> xsd "string"

-- | Encode a Hydra term as a list of RDF Descriptions
encodeTerm :: Syntax.Resource -> Core.Term -> Context.Context -> Graph.Graph -> Either (Context.InContext Errors.Error) ([Syntax.Description], Context.Context)
encodeTerm subject term cx g =
    case term of
      Core.TermAnnotated v0 -> encodeTerm subject (Core.annotatedTermBody v0) cx g
      Core.TermList v0 -> encodeList subject v0 cx g
      Core.TermLiteral v0 -> Right ([
        Syntax.Description {
          Syntax.descriptionSubject = (Syntax.NodeLiteral (Utils.encodeLiteral v0)),
          Syntax.descriptionGraph = (Syntax.Graph Sets.empty)}], cx)
      Core.TermMap v0 -> Eithers.map (\_r -> ([
        Syntax.Description {
          Syntax.descriptionSubject = (Utils.resourceToNode subject),
          Syntax.descriptionGraph = (Syntax.Graph (Sets.fromList (Lists.concat (Pairs.first _r))))}], (Pairs.second _r))) (foldAccumResult (\_cx0 -> \kv -> Eithers.bind (Core__.string _cx0 g (Strip.deannotateTerm (Pairs.first kv))) (\_ks ->
        let pair2 = Utils.nextBlankNode _cx0
            node2 = Pairs.first pair2
            cx2 = Pairs.second pair2
        in (Eithers.map (\_dr -> (Lists.concat2 (Utils.forObjects subject (Utils.keyIri _ks) (Utils.subjectsOf (Pairs.first _dr))) (Utils.triplesOf (Pairs.first _dr)), (Pairs.second _dr))) (encodeTerm node2 (Pairs.second kv) cx2 g)))) cx (Maps.toList v0))
      Core.TermWrap v0 -> Eithers.map (\_dr ->
        let descs = Pairs.first _dr
            cx1 = Pairs.second _dr
        in (Lists.cons (withType (Core.wrappedTermTypeName v0) (Lists.head descs)) (Lists.tail descs), cx1)) (encodeTerm subject (Core.wrappedTermBody v0) cx g)
      Core.TermMaybe v0 -> Maybes.maybe (Right ([], cx)) (\_inner -> encodeTerm subject _inner cx g) v0
      Core.TermRecord v0 ->
        let rname = Core.recordTypeName v0
            fields = Core.recordFields v0
        in (Eithers.map (\_r -> ([
          withType rname (Syntax.Description {
            Syntax.descriptionSubject = (Utils.resourceToNode subject),
            Syntax.descriptionGraph = (Syntax.Graph (Sets.fromList (Lists.concat (Pairs.first _r))))})], (Pairs.second _r))) (foldAccumResult (\_cx0 -> \field -> encodeField rname subject field _cx0 g) cx fields))
      Core.TermSet v0 -> Eithers.map (\_r -> (Lists.concat (Pairs.first _r), (Pairs.second _r))) (foldAccumResult (\_cx0 -> \t ->
        let pair3 = Utils.nextBlankNode _cx0
            node3 = Pairs.first pair3
            cx3 = Pairs.second pair3
        in (encodeTerm node3 t cx3 g)) cx (Sets.toList v0))
      Core.TermUnion v0 ->
        let rname = Core.injectionTypeName v0
            field = Core.injectionField v0
        in (Eithers.map (\_r -> ([
          withType rname (Syntax.Description {
            Syntax.descriptionSubject = (Utils.resourceToNode subject),
            Syntax.descriptionGraph = (Syntax.Graph (Sets.fromList (Pairs.first _r)))})], (Pairs.second _r))) (encodeField rname subject field cx g))
      _ -> unexpectedE cx "RDF-compatible term" "unsupported term variant"

-- | Encode a Hydra type as SHACL CommonProperties
encodeType :: Core.Name -> Core.Type -> Context.Context -> Either (Context.InContext Errors.Error) Model.CommonProperties
encodeType tname typ cx =

      let any = Right (common [])
      in case (Strip.deannotateType typ) of
        Core.TypeEither _ -> any
        Core.TypeList _ -> any
        Core.TypeLiteral v0 -> Right (encodeLiteralType v0)
        Core.TypeMap _ -> any
        Core.TypePair _ -> any
        Core.TypeWrap _ -> any
        Core.TypeRecord v0 -> Eithers.map (\_props -> common [
          Model.CommonConstraintProperty (Sets.fromList (Lists.map (\_p -> Model.ReferenceDefinition _p) _props))]) (Eithers.mapList (\_pair -> encodeFieldType tname (Just (Pairs.first _pair)) (Pairs.second _pair) cx) (Lists.zip (Lists.map (\_i -> Literals.int32ToBigint _i) (Math.range 0 (Lists.length v0))) v0))
        Core.TypeSet _ -> any
        Core.TypeUnion v0 -> Eithers.map (\_props -> common [
          Model.CommonConstraintXone (Sets.fromList (Lists.map (\_p -> Model.ReferenceAnonymous (node [
            Model.CommonConstraintProperty (Sets.fromList [
              Model.ReferenceDefinition _p])])) _props))]) (Eithers.mapList (\_ft -> encodeFieldType tname Nothing _ft cx) v0)
        Core.TypeUnit -> any
        Core.TypeVariable v0 -> Right (common [
          Model.CommonConstraintNode (Sets.fromList [
            Model.ReferenceNamed (Utils.nameToIri v0)])])
        _ -> unexpectedE cx "type" "unsupported type variant"

-- | Construct an error result with a context and message
err :: Context.Context -> String -> Either (Context.InContext Errors.Error) t0
err cx msg =
    Left (Context.InContext {
      Context.inContextObject = (Errors.ErrorOther (Errors.OtherError msg)),
      Context.inContextContext = cx})

-- | Fold over a list, accumulating results and threading context through each step
foldAccumResult :: (t0 -> t1 -> Either t2 (t3, t0)) -> t0 -> [t1] -> Either t2 ([t3], t0)
foldAccumResult f cx xs =
    Logic.ifElse (Lists.null xs) (Right ([], cx)) (Eithers.bind (f cx (Lists.head xs)) (\_r -> Eithers.map (\_rest -> (Lists.cons (Pairs.first _r) (Pairs.first _rest), (Pairs.second _rest))) (foldAccumResult f (Pairs.second _r) (Lists.tail xs))))

-- | Construct a SHACL node shape from a list of common constraints
node :: [Model.CommonConstraint] -> Model.Shape
node constraints = Model.ShapeNode (Model.NodeShape {
  Model.nodeShapeCommon = (common constraints)})

-- | Construct a default property shape with the given IRI as its path
property :: Syntax.Iri -> Model.PropertyShape
property iri =
    Model.PropertyShape {
      Model.propertyShapeCommon = defaultCommonProperties,
      Model.propertyShapeConstraints = Sets.empty,
      Model.propertyShapeDefaultValue = Nothing,
      Model.propertyShapeDescription = (Syntax.LangStrings Maps.empty),
      Model.propertyShapeName = (Syntax.LangStrings Maps.empty),
      Model.propertyShapeOrder = Nothing,
      Model.propertyShapePath = iri}

-- | Encode a module's type elements as a SHACL ShapesGraph
shaclCoder :: Module.Module -> Context.Context -> Graph.Graph -> Either (Context.InContext Errors.Error) (Model.ShapesGraph, Context.Context)
shaclCoder mod cx g =

      let typeEls =
              Maybes.cat (Lists.map (\d -> case d of
                Module.DefinitionType v0 -> Just (Annotations.typeElement (Module.typeDefinitionName v0) (Module.typeDefinitionType v0))
                _ -> Nothing) (Module.moduleDefinitions mod))
          toShape =
                  \el -> Eithers.bind (Eithers.bimap (\_de -> Context.InContext {
                    Context.inContextObject = (Errors.ErrorOther (Errors.OtherError (Errors.unDecodingError _de))),
                    Context.inContextContext = cx}) (\_t -> _t) (Core_.type_ g (Core.bindingTerm el))) (\_typ -> Eithers.map (\_cp -> Model.Definition {
                    Model.definitionIri = (elementIri el),
                    Model.definitionTarget = (Model.ShapeNode (Model.NodeShape {
                      Model.nodeShapeCommon = _cp}))}) (encodeType (Core.bindingName el) _typ cx))
      in (Eithers.map (\_shapes -> (Model.ShapesGraph (Sets.fromList _shapes), cx)) (Eithers.mapList toShape typeEls))

-- | Construct an error for unexpected input, given expected and found descriptions
unexpectedE :: Context.Context -> String -> String -> Either (Context.InContext Errors.Error) t0
unexpectedE cx expected found =
    err cx (Strings.cat [
      "Expected ",
      expected,
      ", found: ",
      found])

-- | Add an rdf:type triple to an RDF Description
withType :: Core.Name -> Syntax.Description -> Syntax.Description
withType name desc =

      let subj = Syntax.descriptionSubject desc
          triples = Syntax.unGraph (Syntax.descriptionGraph desc)
          subjRes =
                  case subj of
                    Syntax.NodeIri v0 -> Syntax.ResourceIri v0
                    Syntax.NodeBnode v0 -> Syntax.ResourceBnode v0
          triple =
                  Syntax.Triple {
                    Syntax.tripleSubject = subjRes,
                    Syntax.triplePredicate = (Utils.rdfIri "type"),
                    Syntax.tripleObject = (Syntax.NodeIri (Utils.nameToIri name))}
      in Syntax.Description {
        Syntax.descriptionSubject = subj,
        Syntax.descriptionGraph = (Syntax.Graph (Sets.insert triple triples))}
