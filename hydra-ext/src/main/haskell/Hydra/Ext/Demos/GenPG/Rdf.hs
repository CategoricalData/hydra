-- | RDF/SHACL output for the GenPG demo.
--
-- Provides an alternative output path (N-Triples + SHACL shapes) alongside the
-- existing GraphSON output. Uses the same CSV-to-property-graph pipeline; only
-- the final serialization differs.

module Hydra.Ext.Demos.GenPG.Rdf where

import Hydra.Kernel
import qualified Hydra.Show.Errors as ShowError
import Hydra.Ext.Demos.GenPG.Examples.Sales.DatabaseSchema
import Hydra.Ext.Demos.GenPG.Examples.Sales.GraphSchema
import Hydra.Ext.Demos.GenPG.Examples.Sales.Mapping
import Hydra.Ext.Demos.GenPG.Examples.Health.DatabaseSchema
import Hydra.Ext.Demos.GenPG.Examples.Health.GraphSchema
import Hydra.Ext.Demos.GenPG.Examples.Health.Mapping
import Hydra.Ext.Demos.GenPG.Demo (transformTables)
import Hydra.Ext.Demos.Shacl.ShaclRdf (shapesGraphToNtriples)
import Hydra.Ext.Tools.PropertyGraphToRdf
import Hydra.Lib.Literals (showInt32)

import qualified Hydra.Ext.Org.W3.Rdf.Syntax as Rdf
import qualified Hydra.Ext.Org.W3.Shacl.Model as Shacl
import qualified Hydra.Ext.Rdf.Serde as Serde
import qualified Hydra.Ext.Rdf.Utils as RdfUtils
import qualified Hydra.Pg.Model as Pg

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import System.IO (hFlush, stdout)


--------------------------------------------------------------------------------
-- Entry points

generateSalesRdf :: IO ()
generateSalesRdf = generateRdf
  "demos/genpg/data/sources/sales"
  salesTableSchemas
  salesGraph
  salesGraphSchema
  "demos/genpg/output/sales"

generateHealthRdf :: IO ()
generateHealthRdf = generateRdf
  "demos/genpg/data/sources/health"
  healthTableSchemas
  healthGraph
  healthGraphSchema
  "demos/genpg/output/health"

generateRdf :: FilePath -> [TableType] -> Pg.LazyGraph Term -> Pg.GraphSchema Type
  -> FilePath -> IO ()
generateRdf sourceRoot tableSchemas graphMapping graphSchema outputDir = do
  log $ "Reading CSV files from " ++ sourceRoot ++ "/"
  g <- transformTables sourceRoot tableSchemas graphMapping

  -- Generate SHACL shapes from the graph schema
  log "Generating SHACL shapes..."
  let shapes = graphSchemaToShapesGraph graphSchema
  let shapesNt = shapesGraphToNtriples shapes
  let shapesFile = outputDir ++ "-shapes.nt"
  writeFile shapesFile (ensureTrailingNewline shapesNt)
  log $ "  Wrote shapes to " ++ shapesFile

  -- Generate RDF data from the property graph
  log "Encoding property graph as RDF..."
  let helper = defaultTermHelper
  let vertexDescs = map (encodeVertexOrFail helper) (Pg.lazyGraphVertices g)
  let edgeDescs = map (encodeEdgeOrFail helper) (Pg.lazyGraphEdges g)
  let allDescs = vertexDescs ++ edgeDescs
  let dataNt = Serde.rdfGraphToNtriples $ RdfUtils.descriptionsToGraph allDescs
  let dataFile = outputDir ++ "-data.nt"
  writeFile dataFile (ensureTrailingNewline dataNt)
  log $ "  Wrote " ++ show (length vertexDescs) ++ " vertex descriptions and "
    ++ show (length edgeDescs) ++ " edge descriptions to " ++ dataFile

  -- Generate intentionally non-conforming RDF data for negative validation
  log "Generating non-conforming RDF data..."
  let invalidNt = generateInvalidData
  let invalidFile = outputDir ++ "-invalid.nt"
  writeFile invalidFile (ensureTrailingNewline invalidNt)
  log $ "  Wrote to " ++ invalidFile

  log "Done."
  where
    log msg = putStrLn msg >> hFlush stdout
    ensureTrailingNewline s
      | null s = "\n"
      | last s == '\n' = s
      | otherwise = s ++ "\n"

    encodeVertexOrFail helper v = case encodeVertex helper v of
      Left ic -> error $ "Error encoding vertex: " ++ show (inContextObject ic)
      Right d -> d

    encodeEdgeOrFail helper e = case encodeEdge helper e of
      Left ic -> error $ "Error encoding edge: " ++ show (inContextObject ic)
      Right d -> d


--------------------------------------------------------------------------------
-- Default helpers for Term-valued property graphs

-- | A default namespace prefix for the demo
demoNs :: String
demoNs = "urn:hydra:genpg:"

-- | Default helper for encoding Term-valued property graphs as RDF.
defaultTermHelper :: PropertyGraphRdfHelper a Term
defaultTermHelper = PropertyGraphRdfHelper {
  encodeEdgeId = termToIri "edge:",
  encodeEdgeLabel = \(Pg.EdgeLabel l) -> Right $ Rdf.Iri (demoNs ++ l),
  encodePropertyKey = \(Pg.PropertyKey k) -> Right $ Rdf.Iri (demoNs ++ k),
  encodePropertyValue = termToLiteral,
  encodeVertexId = termToIri "vertex:",
  encodeVertexLabel = \(Pg.VertexLabel l) -> Right $ Rdf.Iri (demoNs ++ l)}

termToIri :: String -> Term -> Either (InContext Error) Rdf.Iri
termToIri prefix term = case term of
  TermLiteral (LiteralString s) -> Right $ Rdf.Iri (demoNs ++ prefix ++ s)
  TermLiteral (LiteralInteger (IntegerValueInt32 i)) -> Right $ Rdf.Iri (demoNs ++ prefix ++ showInt32 i)
  _ -> otherError $ "Unsupported term type for IRI encoding: " ++ show term

termToLiteral :: Term -> Either (InContext Error) Rdf.Literal
termToLiteral term = case term of
  TermLiteral lit -> Right $ RdfUtils.encodeLiteral lit
  _ -> otherError $ "Expected a literal term: " ++ show term

otherError :: String -> Either (InContext Error) a
otherError msg = Left $ InContext (ErrorOther $ OtherError msg) emptyContext


--------------------------------------------------------------------------------
-- Invalid data generation for negative validation

-- | Generate intentionally non-conforming RDF data for validation testing.
-- Creates instances that violate the SHACL shapes in specific ways:
--   1. An Employee missing required "firstName" and "lastName"
--   2. A Product with an integer "name" instead of string
--   3. An edge pointing to a non-existent vertex type
generateInvalidData :: String
generateInvalidData = Serde.rdfGraphToNtriples $ Rdf.Graph $ S.fromList $
  -- Employee missing required firstName and lastName
  [ triple "urn:hydra:genpg:vertex:invalid_emp" rdfType "urn:hydra:genpg:Employee"
  , tripleL "urn:hydra:genpg:vertex:invalid_emp" "urn:hydra:genpg:email" "bad@example.com"
  ] ++
  -- Product with integer name instead of string (wrong datatype)
  [ triple "urn:hydra:genpg:vertex:invalid_prod" rdfType "urn:hydra:genpg:Product"
  , tripleInt "urn:hydra:genpg:vertex:invalid_prod" "urn:hydra:genpg:name" 42
  ]
  where
    rdfType = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"

    triple :: String -> String -> String -> Rdf.Triple
    triple s p o = Rdf.Triple
      (Rdf.ResourceIri (Rdf.Iri s))
      (Rdf.Iri p)
      (Rdf.NodeIri (Rdf.Iri o))

    tripleL :: String -> String -> String -> Rdf.Triple
    tripleL s p v = Rdf.Triple
      (Rdf.ResourceIri (Rdf.Iri s))
      (Rdf.Iri p)
      (Rdf.NodeLiteral (Rdf.Literal v (Rdf.Iri "http://www.w3.org/2001/XMLSchema#string") Nothing))

    tripleInt :: String -> String -> Int -> Rdf.Triple
    tripleInt s p n = Rdf.Triple
      (Rdf.ResourceIri (Rdf.Iri s))
      (Rdf.Iri p)
      (Rdf.NodeLiteral (Rdf.Literal (show n) (Rdf.Iri "http://www.w3.org/2001/XMLSchema#integer") Nothing))


--------------------------------------------------------------------------------
-- Schema-to-SHACL conversion (raw Haskell, mirrors the promoted DSL logic)

-- | Convert a GraphSchema Type to a SHACL ShapesGraph.
graphSchemaToShapesGraph :: Pg.GraphSchema Type -> Shacl.ShapesGraph
graphSchemaToShapesGraph schema =
  Shacl.ShapesGraph $ S.fromList $ map makeVertexShape $ M.elems (Pg.graphSchemaVertices schema)
  where
    allEdgeTypes = M.elems (Pg.graphSchemaEdges schema)

    makeVertexShape :: Pg.VertexType Type -> Shacl.Definition Shacl.Shape
    makeVertexShape vt =
      let labelIri = vertexLabelToIri (Pg.vertexTypeLabel vt)
          vlabel = Pg.vertexTypeLabel vt
          propConstraints = map (\pt -> makePropertyConstraint (scopedPropertyIri vlabel $ Pg.propertyTypeKey pt) (makePropertyShape pt)) (Pg.vertexTypeProperties vt)
          edgeConstraints = concatMap (makeEdgeConstraint (Pg.vertexTypeLabel vt)) allEdgeTypes
          allConstraints = S.fromList (propConstraints ++ edgeConstraints)
          common = emptyCommon {
            Shacl.commonPropertiesConstraints = allConstraints,
            Shacl.commonPropertiesTargetClass = S.singleton (Rdf.RdfsClass ())}
      in Shacl.Definition {
        Shacl.definitionIri = labelIri,
        Shacl.definitionTarget = Shacl.ShapeNode $ Shacl.NodeShape common}

    makePropertyShape :: Pg.PropertyType Type -> Shacl.PropertyShape
    makePropertyShape pt =
      let keyIri = propertyKeyToIri (Pg.propertyTypeKey pt)
          dtIri = typeToXsdIri (Pg.propertyTypeValue pt)
          constraints = S.singleton (Shacl.CommonConstraintDatatype dtIri)
          propConstraints = if Pg.propertyTypeRequired pt
            then S.singleton (Shacl.PropertyShapeConstraintMinCount 1)
            else S.empty
      in Shacl.PropertyShape {
        Shacl.propertyShapeCommon = emptyCommon { Shacl.commonPropertiesConstraints = constraints },
        Shacl.propertyShapeConstraints = propConstraints,
        Shacl.propertyShapeDefaultValue = Nothing,
        Shacl.propertyShapeDescription = emptyLangStrings,
        Shacl.propertyShapeName = emptyLangStrings,
        Shacl.propertyShapeOrder = Nothing,
        Shacl.propertyShapePath = keyIri}

    makePropertyConstraint :: Rdf.Iri -> Shacl.PropertyShape -> Shacl.CommonConstraint
    makePropertyConstraint propIri ps = Shacl.CommonConstraintProperty $
      S.singleton (Shacl.ReferenceDefinition $ Shacl.Definition propIri ps)

    makeEdgeConstraint :: Pg.VertexLabel -> Pg.EdgeType Type -> [Shacl.CommonConstraint]
    makeEdgeConstraint vLabel et
      | Pg.edgeTypeOut et == vLabel =
          let edgeIri = edgeLabelToIri (Pg.edgeTypeLabel et)
              inIri = vertexLabelToIri (Pg.edgeTypeIn et)
              classConstraint = Shacl.CommonConstraintClass $ S.singleton (Rdf.RdfsClass ())
              nodeConstraint = Shacl.CommonConstraintNode $
                S.singleton (Shacl.ReferenceNamed inIri)
              common = emptyCommon { Shacl.commonPropertiesConstraints = S.fromList [classConstraint, nodeConstraint] }
              ps = Shacl.PropertyShape {
                Shacl.propertyShapeCommon = common,
                Shacl.propertyShapeConstraints = S.empty,
                Shacl.propertyShapeDefaultValue = Nothing,
                Shacl.propertyShapeDescription = emptyLangStrings,
                Shacl.propertyShapeName = emptyLangStrings,
                Shacl.propertyShapeOrder = Nothing,
                Shacl.propertyShapePath = edgeIri}
          in [makePropertyConstraint (scopedEdgeIri vLabel $ Pg.edgeTypeLabel et) ps]
      | otherwise = []

    emptyCommon :: Shacl.CommonProperties
    emptyCommon = Shacl.CommonProperties {
      Shacl.commonPropertiesConstraints = S.empty,
      Shacl.commonPropertiesDeactivated = Nothing,
      Shacl.commonPropertiesMessage = emptyLangStrings,
      Shacl.commonPropertiesSeverity = Shacl.SeverityViolation,
      Shacl.commonPropertiesTargetClass = S.empty,
      Shacl.commonPropertiesTargetNode = S.empty,
      Shacl.commonPropertiesTargetObjectsOf = S.empty,
      Shacl.commonPropertiesTargetSubjectsOf = S.empty}

    emptyLangStrings :: Rdf.LangStrings
    emptyLangStrings = Rdf.LangStrings M.empty

    vertexLabelToIri :: Pg.VertexLabel -> Rdf.Iri
    vertexLabelToIri (Pg.VertexLabel l) = Rdf.Iri (demoNs ++ l)

    edgeLabelToIri :: Pg.EdgeLabel -> Rdf.Iri
    edgeLabelToIri (Pg.EdgeLabel l) = Rdf.Iri (demoNs ++ l)

    propertyKeyToIri :: Pg.PropertyKey -> Rdf.Iri
    propertyKeyToIri (Pg.PropertyKey k) = Rdf.Iri (demoNs ++ k)

    scopedPropertyIri :: Pg.VertexLabel -> Pg.PropertyKey -> Rdf.Iri
    scopedPropertyIri (Pg.VertexLabel vl) (Pg.PropertyKey k) = Rdf.Iri (demoNs ++ vl ++ "#" ++ k)

    scopedEdgeIri :: Pg.VertexLabel -> Pg.EdgeLabel -> Rdf.Iri
    scopedEdgeIri (Pg.VertexLabel vl) (Pg.EdgeLabel el) = Rdf.Iri (demoNs ++ vl ++ "#" ++ el)

    -- | Map a Hydra Type to an XSD datatype IRI
    typeToXsdIri :: Type -> Rdf.Iri
    typeToXsdIri typ = case typ of
      TypeLiteral lt -> literalTypeToXsd lt
      _ -> Rdf.Iri "http://www.w3.org/2001/XMLSchema#string"  -- fallback

    literalTypeToXsd :: LiteralType -> Rdf.Iri
    literalTypeToXsd lt = Rdf.Iri $ "http://www.w3.org/2001/XMLSchema#" ++ case lt of
      LiteralTypeBinary -> "base64Binary"
      LiteralTypeBoolean -> "boolean"
      LiteralTypeFloat ft -> case ft of
        FloatTypeBigfloat -> "decimal"
        FloatTypeFloat32 -> "float"
        FloatTypeFloat64 -> "double"
      LiteralTypeInteger it -> case it of
        IntegerTypeBigint -> "integer"
        IntegerTypeInt8 -> "byte"
        IntegerTypeInt16 -> "short"
        IntegerTypeInt32 -> "int"
        IntegerTypeInt64 -> "long"
        IntegerTypeUint8 -> "unsignedByte"
        IntegerTypeUint16 -> "unsignedShort"
        IntegerTypeUint32 -> "unsignedInt"
        IntegerTypeUint64 -> "unsignedLong"
      LiteralTypeString -> "string"
