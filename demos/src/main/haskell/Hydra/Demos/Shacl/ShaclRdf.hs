-- | Serialize a SHACL ShapesGraph to RDF triples using the SHACL vocabulary.
--
-- Converts Hydra's SHACL model types (ShapesGraph, Definition, Shape,
-- NodeShape, PropertyShape, etc.) into RDF triples using the sh: namespace.

module Hydra.Demos.Shacl.ShaclRdf (
  shapesGraphToTriples,
  shapesGraphToNtriples,
) where

import qualified Hydra.Rdf.Syntax as Rdf
import qualified Hydra.Shacl.Model as Shacl
import qualified Hydra.Rdf.Serde as Serde
import qualified Hydra.Rdf.Utils as RdfUtils

import qualified Data.Set as S


-- | SHACL namespace
sh :: String -> Rdf.Iri
sh local = Rdf.Iri ("http://www.w3.org/ns/shacl#" ++ local)

-- | RDF namespace
rdf :: String -> Rdf.Iri
rdf local = Rdf.Iri ("http://www.w3.org/1999/02/22-rdf-syntax-ns#" ++ local)

-- | XSD namespace
xsd :: String -> Rdf.Iri
xsd local = Rdf.Iri ("http://www.w3.org/2001/XMLSchema#" ++ local)

-- | Create an IRI node
iriNode :: Rdf.Iri -> Rdf.Node
iriNode = Rdf.NodeIri

-- | Create an IRI resource
iriResource :: Rdf.Iri -> Rdf.Resource
iriResource = Rdf.ResourceIri

-- | Create a triple from IRI subject, IRI predicate, and IRI object
tripleIII :: Rdf.Iri -> Rdf.Iri -> Rdf.Iri -> Rdf.Triple
tripleIII s p o = Rdf.Triple (iriResource s) p (iriNode o)

-- | Create a triple from IRI subject, IRI predicate, and node object
tripleIIN :: Rdf.Iri -> Rdf.Iri -> Rdf.Node -> Rdf.Triple
tripleIIN s p o = Rdf.Triple (iriResource s) p o

-- | Create a triple from resource subject, IRI predicate, and node object
tripleRIN :: Rdf.Resource -> Rdf.Iri -> Rdf.Node -> Rdf.Triple
tripleRIN s p o = Rdf.Triple s p o

-- | Create a triple from resource subject, IRI predicate, and IRI object
tripleRII :: Rdf.Resource -> Rdf.Iri -> Rdf.Iri -> Rdf.Triple
tripleRII s p o = Rdf.Triple s p (iriNode o)

-- | Create an integer literal node
integerNode :: Integer -> Rdf.Node
integerNode n = Rdf.NodeLiteral $ Rdf.Literal {
  Rdf.literalLexicalForm = show n,
  Rdf.literalDatatypeIri = xsd "integer",
  Rdf.literalLanguageTag = Nothing }

-- | Create a boolean literal node
booleanNode :: Bool -> Rdf.Node
booleanNode b = Rdf.NodeLiteral $ Rdf.Literal {
  Rdf.literalLexicalForm = if b then "true" else "false",
  Rdf.literalDatatypeIri = xsd "boolean",
  Rdf.literalLanguageTag = Nothing }

-- | Create a string literal node
stringNode :: String -> Rdf.Node
stringNode s = Rdf.NodeLiteral $ Rdf.Literal {
  Rdf.literalLexicalForm = s,
  Rdf.literalDatatypeIri = xsd "string",
  Rdf.literalLanguageTag = Nothing }

-- | Convert a ShapesGraph to RDF triples
shapesGraphToTriples :: Shacl.ShapesGraph -> [Rdf.Triple]
shapesGraphToTriples (Shacl.ShapesGraph defs) =
  concatMap definitionToTriples (S.toList defs)

-- | Convert a ShapesGraph to N-Triples string
shapesGraphToNtriples :: Shacl.ShapesGraph -> String
shapesGraphToNtriples sg =
  Serde.rdfGraphToNtriples $ RdfUtils.descriptionsToGraph descs
  where
    triples = shapesGraphToTriples sg
    descs = map tripleToDescription triples

-- | Convert a single triple to a description (one triple per description)
tripleToDescription :: Rdf.Triple -> Rdf.Description
tripleToDescription t@(Rdf.Triple subj _ _) =
  Rdf.Description {
    Rdf.descriptionSubject = resourceToNode subj,
    Rdf.descriptionGraph = Rdf.Graph (S.singleton t) }

-- | Convert a resource to a node
resourceToNode :: Rdf.Resource -> Rdf.Node
resourceToNode (Rdf.ResourceIri i) = Rdf.NodeIri i
resourceToNode (Rdf.ResourceBnode b) = Rdf.NodeBnode b

-- | Serialize a Definition<Shape> to triples
definitionToTriples :: Shacl.Definition Shacl.Shape -> [Rdf.Triple]
definitionToTriples (Shacl.Definition iri shape) =
  case shape of
    Shacl.ShapeNode ns -> nodeShapeToTriples iri ns
    Shacl.ShapeProperty ps -> propertyShapeToTriples (iriResource iri) ps

-- | Serialize a NodeShape to triples
nodeShapeToTriples :: Rdf.Iri -> Shacl.NodeShape -> [Rdf.Triple]
nodeShapeToTriples iri (Shacl.NodeShape common) =
  [ tripleIII iri (rdf "type") (sh "NodeShape"),
    tripleIII iri (sh "targetClass") iri ]
  ++ commonPropertiesToTriples (iriResource iri) common

-- | Serialize a PropertyShape to triples, given a subject resource (may be blank node)
propertyShapeToTriples :: Rdf.Resource -> Shacl.PropertyShape -> [Rdf.Triple]
propertyShapeToTriples subj ps =
  [ tripleRII subj (rdf "type") (sh "PropertyShape"),
    tripleRII subj (sh "path") (Shacl.propertyShapePath ps) ]
  ++ commonPropertiesToTriples subj (Shacl.propertyShapeCommon ps)
  ++ concatMap (propertyShapeConstraintToTriples subj) (S.toList $ Shacl.propertyShapeConstraints ps)
  ++ maybe [] (\n -> [tripleRIN subj (sh "order") (integerNode n)]) (Shacl.propertyShapeOrder ps)

-- | Serialize CommonProperties to triples
commonPropertiesToTriples :: Rdf.Resource -> Shacl.CommonProperties -> [Rdf.Triple]
commonPropertiesToTriples subj cp =
  concatMap (commonConstraintToTriples subj) (S.toList $ Shacl.commonPropertiesConstraints cp)
  ++ maybe [] (\b -> [tripleRIN subj (sh "deactivated") (booleanNode b)]) (Shacl.commonPropertiesDeactivated cp)
  -- Note: targetClass, targetNode, targetObjectsOf, targetSubjectsOf are typically empty
  -- for shapes generated by the Hydra SHACL coder. The RDF model's RdfsClass and Property
  -- types are stand-ins (not IRIs), so we skip them if non-empty.
  ++ concatMap (\n -> [tripleRIN subj (sh "targetNode") (iriOrLiteralToNode n)]) (S.toList $ Shacl.commonPropertiesTargetNode cp)

-- | Serialize a CommonConstraint to triples
commonConstraintToTriples :: Rdf.Resource -> Shacl.CommonConstraint -> [Rdf.Triple]
commonConstraintToTriples subj cc = case cc of
  Shacl.CommonConstraintDatatype iri ->
    [tripleRII subj (sh "datatype") iri]
  Shacl.CommonConstraintClass _classes ->
    [] -- RdfsClass is a stand-in type (wraps ()), not usable as an IRI
  Shacl.CommonConstraintNodeKind nk ->
    [tripleRII subj (sh "nodeKind") (nodeKindToIri nk)]
  Shacl.CommonConstraintProperty refs ->
    concatMap (propertyRefToTriples subj) (S.toList refs)
  Shacl.CommonConstraintMaxInclusive lit ->
    [tripleRIN subj (sh "maxInclusive") (Rdf.NodeLiteral lit)]
  Shacl.CommonConstraintMinInclusive lit ->
    [tripleRIN subj (sh "minInclusive") (Rdf.NodeLiteral lit)]
  Shacl.CommonConstraintMaxExclusive lit ->
    [tripleRIN subj (sh "maxExclusive") (Rdf.NodeLiteral lit)]
  Shacl.CommonConstraintMinExclusive lit ->
    [tripleRIN subj (sh "minExclusive") (Rdf.NodeLiteral lit)]
  Shacl.CommonConstraintMaxLength n ->
    [tripleRIN subj (sh "maxLength") (integerNode n)]
  Shacl.CommonConstraintMinLength n ->
    [tripleRIN subj (sh "minLength") (integerNode n)]
  Shacl.CommonConstraintPattern (Shacl.Pattern regex flags) ->
    [tripleRIN subj (sh "pattern") (stringNode regex)]
    ++ maybe [] (\f -> [tripleRIN subj (sh "flags") (stringNode f)]) flags
  Shacl.CommonConstraintClosed (Shacl.Closed isClosed _ignored) ->
    [tripleRIN subj (sh "closed") (booleanNode isClosed)]
  Shacl.CommonConstraintHasValue nodes ->
    concatMap (\n -> [tripleRIN subj (sh "hasValue") n]) (S.toList nodes)
  Shacl.CommonConstraintIn nodes ->
    -- sh:in is an RDF list; simplified here as individual triples
    concatMap (\n -> [tripleRIN subj (sh "in") n]) nodes
  Shacl.CommonConstraintNode refs ->
    concatMap (\ref -> case ref of
      Shacl.ReferenceNamed iri -> [tripleRII subj (sh "node") iri]
      _ -> []) (S.toList refs)
  Shacl.CommonConstraintNot refs ->
    concatMap (\ref -> case ref of
      Shacl.ReferenceNamed iri -> [tripleRII subj (sh "not") iri]
      _ -> []) (S.toList refs)
  Shacl.CommonConstraintAnd refs ->
    concatMap (\ref -> case ref of
      Shacl.ReferenceNamed iri -> [tripleRII subj (sh "and") iri]
      _ -> []) (S.toList refs)
  Shacl.CommonConstraintOr refs ->
    concatMap (\ref -> case ref of
      Shacl.ReferenceNamed iri -> [tripleRII subj (sh "or") iri]
      _ -> []) (S.toList refs)
  Shacl.CommonConstraintXone refs ->
    concatMap (\ref -> case ref of
      Shacl.ReferenceNamed iri -> [tripleRII subj (sh "xone") iri]
      _ -> []) (S.toList refs)
  Shacl.CommonConstraintEquals _props ->
    [] -- Property is a stand-in type, not usable as an IRI
  Shacl.CommonConstraintDisjoint _props ->
    [] -- Property is a stand-in type, not usable as an IRI
  Shacl.CommonConstraintLanguageIn _tags ->
    [] -- Language-in requires RDF list encoding; omit for now

-- | Serialize a property shape reference (may be inline or named)
propertyRefToTriples :: Rdf.Resource -> Shacl.Reference Shacl.PropertyShape -> [Rdf.Triple]
propertyRefToTriples subj ref = case ref of
  Shacl.ReferenceNamed iri ->
    [tripleRII subj (sh "property") iri]
  Shacl.ReferenceDefinition (Shacl.Definition iri ps) ->
    [tripleRII subj (sh "property") iri]
    ++ propertyShapeToTriples (iriResource iri) ps
  Shacl.ReferenceAnonymous _ps ->
    -- Anonymous property shapes would need blank node allocation; skip for now
    []

-- | Serialize a PropertyShapeConstraint to triples
propertyShapeConstraintToTriples :: Rdf.Resource -> Shacl.PropertyShapeConstraint -> [Rdf.Triple]
propertyShapeConstraintToTriples subj psc = case psc of
  Shacl.PropertyShapeConstraintMinCount n ->
    [tripleRIN subj (sh "minCount") (integerNode n)]
  Shacl.PropertyShapeConstraintMaxCount n ->
    [tripleRIN subj (sh "maxCount") (integerNode n)]
  Shacl.PropertyShapeConstraintLessThan _props ->
    [] -- Property is a stand-in type, not usable as an IRI
  Shacl.PropertyShapeConstraintLessThanOrEquals _props ->
    [] -- Property is a stand-in type, not usable as an IRI
  Shacl.PropertyShapeConstraintUniqueLang b ->
    [tripleRIN subj (sh "uniqueLang") (booleanNode b)]
  Shacl.PropertyShapeConstraintQualifiedValueShape _ ->
    [] -- Complex; omit for now

-- | Convert a NodeKind to its SHACL IRI
nodeKindToIri :: Shacl.NodeKind -> Rdf.Iri
nodeKindToIri nk = case nk of
  Shacl.NodeKindBlankNode -> sh "BlankNode"
  Shacl.NodeKindIri -> sh "IRI"
  Shacl.NodeKindLiteral -> sh "Literal"
  Shacl.NodeKindBlankNodeOrIri -> sh "BlankNodeOrIRI"
  Shacl.NodeKindBlankNodeOrLiteral -> sh "BlankNodeOrLiteral"
  Shacl.NodeKindIriOrLiteral -> sh "IRIOrLiteral"

-- | Convert an IriOrLiteral to an RDF Node
iriOrLiteralToNode :: Rdf.IriOrLiteral -> Rdf.Node
iriOrLiteralToNode (Rdf.IriOrLiteralIri iri) = Rdf.NodeIri iri
iriOrLiteralToNode (Rdf.IriOrLiteralLiteral lit) = Rdf.NodeLiteral lit
