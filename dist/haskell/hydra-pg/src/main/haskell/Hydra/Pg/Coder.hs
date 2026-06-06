-- Note: this is an automatically generated file. Do not edit.
-- | Property graph element coders for mapping Hydra terms to property graph elements

module Hydra.Pg.Coder where
import qualified Hydra.Annotations as Annotations
import qualified Hydra.Ast as Ast
import qualified Hydra.Coders as Coders
import qualified Hydra.Core as Core
import qualified Hydra.Error.Checking as Checking
import qualified Hydra.Error.Core as ErrorCore
import qualified Hydra.Error.Packaging as ErrorPackaging
import qualified Hydra.Errors as Errors
import qualified Hydra.Extract.Core as ExtractCore
import qualified Hydra.Graph as Graph
import qualified Hydra.Json.Model as JsonModel
import qualified Hydra.Haskell.Lib.Eithers as Eithers
import qualified Hydra.Haskell.Lib.Equality as Equality
import qualified Hydra.Haskell.Lib.Lists as Lists
import qualified Hydra.Haskell.Lib.Logic as Logic
import qualified Hydra.Haskell.Lib.Maps as Maps
import qualified Hydra.Haskell.Lib.Optionals as Optionals
import qualified Hydra.Haskell.Lib.Pairs as Pairs
import qualified Hydra.Haskell.Lib.Sets as Sets
import qualified Hydra.Haskell.Lib.Strings as Strings
import qualified Hydra.Packaging as Packaging
import qualified Hydra.Parsing as Parsing
import qualified Hydra.Paths as Paths
import qualified Hydra.Pg.Mapping as Mapping
import qualified Hydra.Pg.Model as PgModel
import qualified Hydra.Pg.TermsToElements as TermsToElements
import qualified Hydra.Query as Query
import qualified Hydra.Relational as Relational
import qualified Hydra.Resolution as Resolution
import qualified Hydra.Strip as Strip
import qualified Hydra.Tabular as Tabular
import qualified Hydra.Testing as Testing
import qualified Hydra.Topology as Topology
import qualified Hydra.Typed as Typed
import qualified Hydra.Typing as Typing
import qualified Hydra.Util as Util
import qualified Hydra.Validation as Validation
import qualified Hydra.Variants as Variants
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
import qualified Data.Map as M
-- | Check a condition, returning an error if false
check :: t0 -> Bool -> Either t1 () -> Either t1 ()
check _cx b e = Logic.ifElse b (Right ()) e
-- | Check that a record name matches the expected name
checkRecordName :: t0 -> Core.Name -> Core.Name -> Either Errors.Error ()
checkRecordName cx expected actual =
    check cx (Logic.or (Equality.equal (Core.unName expected) "placeholder") (Equality.equal (Core.unName actual) (Core.unName expected))) (Left (Errors.ErrorOther (Errors.OtherError (Strings.cat2 (Strings.cat2 (Strings.cat2 "Expected record of type " (Core.unName expected)) ", found record of type ") (Core.unName actual)))))
-- | Construct an edge coder from components
constructEdgeCoder :: Typing.InferenceContext -> Graph.Graph -> PgModel.VertexLabel -> Mapping.Schema t0 t1 t2 -> Core.Type -> t1 -> t1 -> PgModel.Direction -> Core.Name -> [Core.FieldType] -> [Coders.Adapter Core.FieldType (PgModel.PropertyType t1) Core.Field (PgModel.Property t2)] -> Maybe (Core.FieldType, (Mapping.ValueSpec, (Maybe String))) -> Maybe (Core.FieldType, (Mapping.ValueSpec, (Maybe String))) -> Either Errors.Error (Coders.Adapter Core.Type (PgModel.ElementTypeTree t1) Core.Term (PgModel.ElementTree t2))
constructEdgeCoder cx g parentLabel schema source vidType eidType dir name fields propAdapters mOutSpec mInSpec =
    Eithers.bind (findLabelString cx g source name (Core.Name (Mapping.annotationSchemaEdgeLabel (Mapping.schemaAnnotations schema)))) (\labelStr ->
      let label = PgModel.EdgeLabel labelStr
          vertexIdsSchema = Mapping.schemaVertexIds schema
      in (Eithers.bind (edgeIdAdapter cx g schema eidType name (Core.Name (Mapping.annotationSchemaEdgeId (Mapping.schemaAnnotations schema))) fields) (\idAdapter -> Eithers.bind (Optionals.cases mOutSpec (Right Nothing) (\s -> Eithers.map (\x -> Just x) (projectionAdapter cx g vidType vertexIdsSchema s "out"))) (\outIdAdapter -> Eithers.bind (Optionals.cases mInSpec (Right Nothing) (\s -> Eithers.map (\x -> Just x) (projectionAdapter cx g vidType vertexIdsSchema s "in"))) (\inIdAdapter -> Eithers.bind (Optionals.cases mOutSpec (Right Nothing) (\s -> Eithers.map (\x -> Just x) (findIncidentVertexAdapter cx g schema vidType eidType s))) (\outVertexAdapter -> Eithers.bind (Optionals.cases mInSpec (Right Nothing) (\s -> Eithers.map (\x -> Just x) (findIncidentVertexAdapter cx g schema vidType eidType s))) (\inVertexAdapter ->
        let vertexAdapters =
                Optionals.cat [
                  outVertexAdapter,
                  inVertexAdapter]
        in (Eithers.bind (Optionals.cases mOutSpec (Right parentLabel) (\spec -> Optionals.cases (Pairs.second (Pairs.second spec)) (Left (Errors.ErrorOther (Errors.OtherError "no out-vertex label"))) (\a -> Right (PgModel.VertexLabel a)))) (\outLabel -> Eithers.bind (Optionals.cases mInSpec (Right parentLabel) (\spec -> Optionals.cases (Pairs.second (Pairs.second spec)) (Left (Errors.ErrorOther (Errors.OtherError "no in-vertex label"))) (\a -> Right (PgModel.VertexLabel a)))) (\inLabel -> Right (edgeCoder g dir schema source eidType name label outLabel inLabel idAdapter outIdAdapter inIdAdapter propAdapters vertexAdapters)))))))))))
-- | Construct a vertex coder from components
constructVertexCoder :: Typing.InferenceContext -> Graph.Graph -> Mapping.Schema t0 t1 t2 -> Core.Type -> t1 -> t1 -> Core.Name -> [Core.FieldType] -> [Coders.Adapter Core.FieldType (PgModel.PropertyType t1) Core.Field (PgModel.Property t2)] -> Either Errors.Error (Coders.Adapter Core.Type (PgModel.ElementTypeTree t1) Core.Term (PgModel.ElementTree t2))
constructVertexCoder cx g schema source vidType eidType name fields propAdapters =
    Eithers.bind (findLabelString cx g source name (Core.Name (Mapping.annotationSchemaVertexLabel (Mapping.schemaAnnotations schema)))) (\labelStr ->
      let label = PgModel.VertexLabel labelStr
      in (Eithers.bind (vertexIdAdapter cx g schema vidType name (Core.Name (Mapping.annotationSchemaVertexId (Mapping.schemaAnnotations schema))) fields) (\idAdapter -> Eithers.bind (findAdjacenEdgeAdapters cx g schema vidType eidType label PgModel.DirectionOut fields) (\outEdgeAdapters -> Eithers.bind (findAdjacenEdgeAdapters cx g schema vidType eidType label PgModel.DirectionIn fields) (\inEdgeAdapters -> Right (vertexCoder g schema source vidType name label idAdapter propAdapters (Lists.concat2 outEdgeAdapters inEdgeAdapters)))))))
-- | Create an edge coder given all components
edgeCoder :: t0 -> PgModel.Direction -> Mapping.Schema t1 t2 t3 -> t4 -> t5 -> Core.Name -> PgModel.EdgeLabel -> PgModel.VertexLabel -> PgModel.VertexLabel -> Maybe (Core.Name, (Coders.Adapter t6 t7 Core.Term t3)) -> Maybe (Core.Name, (Coders.Adapter t8 t9 Core.Term t3)) -> Maybe (Core.Name, (Coders.Adapter t10 t11 Core.Term t3)) -> [Coders.Adapter Core.FieldType (PgModel.PropertyType t5) Core.Field (PgModel.Property t3)] -> [(Core.Name, (Coders.Adapter t12 t13 Core.Term (PgModel.ElementTree t3)))] -> Coders.Adapter t4 (PgModel.ElementTypeTree t5) Core.Term (PgModel.ElementTree t3)
edgeCoder g dir schema source eidType tname label outLabel inLabel mIdAdapter outAdapter inAdapter propAdapters vertexAdapters =

      let et =
              PgModel.EdgeType {
                PgModel.edgeTypeLabel = label,
                PgModel.edgeTypeId = eidType,
                PgModel.edgeTypeOut = outLabel,
                PgModel.edgeTypeIn = inLabel,
                PgModel.edgeTypeProperties = (propertyTypes propAdapters)}
      in Coders.Adapter {
        Coders.adapterIsLossy = True,
        Coders.adapterSource = source,
        Coders.adapterTarget = (elementTypeTreeEdge et []),
        Coders.adapterCoder = Coders.Coder {
          Coders.coderEncode = (\cx -> \term ->
            let deannot = Strip.deannotateTerm term
                unwrapped =
                        case deannot of
                          Core.TermOptional v0 -> Optionals.fromOptional deannot v0
                          _ -> deannot
                rec =
                        case unwrapped of
                          Core.TermRecord v0 -> v0
            in (Eithers.bind (checkRecordName cx tname (Core.recordTypeName rec)) (\_chk ->
              let fieldsm = Resolution.fieldMap (Core.recordFields rec)
              in (Eithers.bind (Optionals.cases mIdAdapter (Right (Mapping.schemaDefaultEdgeId schema)) (selectEdgeId cx fieldsm)) (\edgeId -> Eithers.bind (encodeProperties cx fieldsm propAdapters) (\props ->
                let getVertexId =
                        \dirCheck -> \adapter -> Optionals.cases (Logic.ifElse (Equality.equal dir dirCheck) Nothing adapter) (Right (Mapping.schemaDefaultVertexId schema)) (selectVertexId cx fieldsm)
                in (Eithers.bind (getVertexId PgModel.DirectionOut outAdapter) (\outId -> Eithers.bind (getVertexId PgModel.DirectionIn inAdapter) (\inId -> Eithers.bind (Eithers.map (\xs -> Optionals.cat xs) (Eithers.mapList (\va ->
                  let fname = Pairs.first va
                      ad = Pairs.second va
                  in (Optionals.cases (Maps.lookup fname fieldsm) (Right Nothing) (\fterm -> Eithers.map (\x -> Just x) (Coders.coderEncode (Coders.adapterCoder ad) cx fterm)))) vertexAdapters)) (\deps -> Right (elementTreeEdge (PgModel.Edge {
                  PgModel.edgeLabel = label,
                  PgModel.edgeId = edgeId,
                  PgModel.edgeOut = outId,
                  PgModel.edgeIn = inId,
                  PgModel.edgeProperties = props}) deps))))))))))),
          Coders.coderDecode = (\cx -> \_ -> Left (Errors.ErrorOther (Errors.OtherError "edge decoding is not yet supported")))}}
-- | Create an edge id adapter
edgeIdAdapter :: t0 -> t1 -> Mapping.Schema t2 t3 t4 -> t5 -> Core.Name -> Core.Name -> [Core.FieldType] -> Either Errors.Error (Maybe (Core.Name, (Coders.Adapter Core.Type t5 Core.Term t4)))
edgeIdAdapter cx g schema eidType name idKey fields =
    Eithers.bind (findIdProjectionSpec cx False name idKey fields) (\mIdSpec -> Optionals.cases mIdSpec (Right Nothing) (\idSpec -> Eithers.map (\x -> Just x) (projectionAdapter cx g eidType (Mapping.schemaEdgeIds schema) idSpec "id")))
-- | Construct an element adapter for a given type, interpreting it either as a vertex specification or an edge specification
elementCoder :: Maybe (PgModel.Direction, PgModel.VertexLabel) -> Mapping.Schema t0 t1 t2 -> Core.Type -> t1 -> t1 -> Typing.InferenceContext -> Graph.Graph -> Either Errors.Error (Coders.Adapter Core.Type (PgModel.ElementTypeTree t1) Core.Term (PgModel.ElementTree t2))
elementCoder mparent schema source vidType eidType cx g =

      let dir = Optionals.cases mparent PgModel.DirectionBoth (\p -> Pairs.first p)
          parentLabel = Optionals.cases mparent (PgModel.VertexLabel "NOLABEL") (\p -> Pairs.second p)
      in case (Strip.deannotateType source) of
        Core.TypeOptional v0 -> elementCoder mparent schema v0 vidType eidType cx g
        Core.TypeRecord v0 ->
          let name = Core.Name "placeholder"
              outVertexKey = Core.Name (Mapping.annotationSchemaOutVertex (Mapping.schemaAnnotations schema))
              outVertexLabelKey = Core.Name (Mapping.annotationSchemaOutVertexLabel (Mapping.schemaAnnotations schema))
              inVertexKey = Core.Name (Mapping.annotationSchemaInVertex (Mapping.schemaAnnotations schema))
              inVertexLabelKey = Core.Name (Mapping.annotationSchemaInVertexLabel (Mapping.schemaAnnotations schema))
          in (Eithers.bind (findProjectionSpec cx g name outVertexKey outVertexLabelKey v0) (\mOutSpec -> Eithers.bind (findProjectionSpec cx g name inVertexKey inVertexLabelKey v0) (\mInSpec ->
            let kind = Logic.ifElse (hasVertexAdapters dir mOutSpec mInSpec) PgModel.ElementKindEdge PgModel.ElementKindVertex
            in (Eithers.bind (findPropertySpecs cx g schema kind v0) (\propSpecs -> Eithers.bind (Eithers.mapList (propertyAdapter cx g schema) propSpecs) (\propAdapters -> case kind of
              PgModel.ElementKindVertex -> constructVertexCoder cx g schema source vidType eidType name v0 propAdapters
              PgModel.ElementKindEdge -> constructEdgeCoder cx g parentLabel schema source vidType eidType dir name v0 propAdapters mOutSpec mInSpec))))))
        _ -> Left (Errors.ErrorOther (Errors.OtherError (Strings.cat2 (Strings.cat2 (Strings.cat2 "Expected " "record type") ", found: ") "other type")))
-- | Create an element tree for an edge
elementTreeEdge :: PgModel.Edge t0 -> [PgModel.ElementTree t0] -> PgModel.ElementTree t0
elementTreeEdge edge deps =
    PgModel.ElementTree {
      PgModel.elementTreeSelf = (PgModel.ElementEdge edge),
      PgModel.elementTreeDependencies = deps}
-- | Create an element tree for a vertex
elementTreeVertex :: PgModel.Vertex t0 -> [PgModel.ElementTree t0] -> PgModel.ElementTree t0
elementTreeVertex vertex deps =
    PgModel.ElementTree {
      PgModel.elementTreeSelf = (PgModel.ElementVertex vertex),
      PgModel.elementTreeDependencies = deps}
-- | Create an element type tree for an edge type
elementTypeTreeEdge :: PgModel.EdgeType t0 -> [PgModel.ElementTypeTree t0] -> PgModel.ElementTypeTree t0
elementTypeTreeEdge etype deps =
    PgModel.ElementTypeTree {
      PgModel.elementTypeTreeSelf = (PgModel.ElementTypeEdge etype),
      PgModel.elementTypeTreeDependencies = deps}
-- | Create an element type tree for a vertex type
elementTypeTreeVertex :: PgModel.VertexType t0 -> [PgModel.ElementTypeTree t0] -> PgModel.ElementTypeTree t0
elementTypeTreeVertex vtype deps =
    PgModel.ElementTypeTree {
      PgModel.elementTypeTreeSelf = (PgModel.ElementTypeVertex vtype),
      PgModel.elementTypeTreeDependencies = deps}
-- | Encode all properties from a field map using property adapters
encodeProperties :: Typing.InferenceContext -> M.Map Core.Name Core.Term -> [Coders.Adapter Core.FieldType t0 Core.Field (PgModel.Property t1)] -> Either Errors.Error (M.Map PgModel.PropertyKey t1)
encodeProperties cx fields adapters =
    Eithers.map (\props -> Maps.fromList (Lists.map (\prop -> (PgModel.propertyKey prop, (PgModel.propertyValue prop))) props)) (Eithers.map (\xs -> Optionals.cat xs) (Eithers.mapList (encodeProperty cx fields) adapters))
-- | Encode a single property from a field map using a property adapter
encodeProperty :: Typing.InferenceContext -> M.Map Core.Name Core.Term -> Coders.Adapter Core.FieldType t0 Core.Field t1 -> Either Errors.Error (Maybe t1)
encodeProperty cx fields adapter =

      let fname = Core.fieldTypeName (Coders.adapterSource adapter)
          ftyp = Strip.deannotateType (Core.fieldTypeType (Coders.adapterSource adapter))
          isMaybe =
                  case ftyp of
                    Core.TypeOptional _ -> True
                    _ -> False
          encodeValue =
                  \v -> Eithers.map (\x -> Just x) (Coders.coderEncode (Coders.adapterCoder adapter) cx (Core.Field {
                    Core.fieldName = fname,
                    Core.fieldTerm = v}))
      in (Optionals.cases (Maps.lookup fname fields) (Logic.ifElse isMaybe (Right Nothing) (Left (Errors.ErrorOther (Errors.OtherError (Strings.cat2 "expected field not found in record: " (Core.unName fname)))))) (\value -> Logic.ifElse isMaybe (case (Strip.deannotateTerm value) of
        Core.TermOptional v0 -> Optionals.cases v0 (Right Nothing) encodeValue
        _ -> encodeValue value) (encodeValue value)))
-- | Extract a string from a term
extractString :: t0 -> Graph.Graph -> Core.Term -> Either Errors.Error String
extractString cx g t = ExtractCore.string g t
-- | Find adjacent edge adapters for a given direction
findAdjacenEdgeAdapters :: Typing.InferenceContext -> Graph.Graph -> Mapping.Schema t0 t1 t2 -> t1 -> t1 -> PgModel.VertexLabel -> PgModel.Direction -> [Core.FieldType] -> Either Errors.Error [(
  PgModel.Direction,
  (
    Core.FieldType,
    (PgModel.EdgeLabel, (Coders.Adapter Core.Type (PgModel.ElementTypeTree t1) Core.Term (PgModel.ElementTree t2)))))]
findAdjacenEdgeAdapters cx g schema vidType eidType parentLabel dir fields =
    Eithers.map (\xs -> Optionals.cat xs) (Eithers.mapList (\field ->
      let key =
              Core.Name (case dir of
                PgModel.DirectionOut -> Mapping.annotationSchemaOutEdgeLabel (Mapping.schemaAnnotations schema)
                PgModel.DirectionIn -> Mapping.annotationSchemaInEdgeLabel (Mapping.schemaAnnotations schema))
      in (Optionals.cases (Annotations.getTypeAnnotation key (Core.fieldTypeType field)) (Right Nothing) (\a -> Eithers.bind (extractString cx g a) (\labelStr -> Eithers.bind (elementCoder (Just (dir, parentLabel)) schema (Core.fieldTypeType field) vidType eidType cx g) (\elad -> Right (Just (dir, (field, (PgModel.EdgeLabel labelStr, elad))))))))) fields)
-- | Find an id projection spec for a field
findIdProjectionSpec :: t0 -> Bool -> Core.Name -> Core.Name -> [Core.FieldType] -> Either Errors.Error (Maybe (Core.FieldType, (Mapping.ValueSpec, (Maybe String))))
findIdProjectionSpec cx required tname idKey fields =
    Eithers.bind (findSingleFieldWithAnnotationKey cx tname idKey fields) (\mid -> Optionals.cases mid (Logic.ifElse required (Left (Errors.ErrorOther (Errors.OtherError (Strings.cat2 (Strings.cat2 "no " (Core.unName idKey)) " field")))) (Right Nothing)) (\mi -> Eithers.map (\spec -> Just (mi, (spec, (Optionals.map (\s -> Strings.toUpper s) Nothing)))) (Optionals.cases (Annotations.getTypeAnnotation idKey (Core.fieldTypeType mi)) (Right Mapping.ValueSpecValue) (TermsToElements.decodeValueSpec cx (Graph.Graph {
      Graph.graphBoundTerms = Maps.empty,
      Graph.graphBoundTypes = Maps.empty,
      Graph.graphClassConstraints = Maps.empty,
      Graph.graphLambdaVariables = Sets.empty,
      Graph.graphMetadata = Maps.empty,
      Graph.graphPrimitives = Maps.empty,
      Graph.graphSchemaTypes = Maps.empty,
      Graph.graphTypeVariables = Sets.empty})))))
-- | Find an incident vertex adapter for a projection spec
findIncidentVertexAdapter :: Typing.InferenceContext -> Graph.Graph -> Mapping.Schema t0 t1 t2 -> t1 -> t1 -> (Core.FieldType, (Mapping.ValueSpec, (Maybe String))) -> Either Errors.Error (Core.Name, (Coders.Adapter Core.Type (PgModel.ElementTypeTree t1) Core.Term (PgModel.ElementTree t2)))
findIncidentVertexAdapter cx g schema vidType eidType spec =

      let field = Pairs.first spec
      in (Eithers.bind (elementCoder Nothing schema (Core.fieldTypeType field) vidType eidType cx g) (\adapter -> Right (Core.fieldTypeName field, adapter)))
-- | Find a label string from annotations or the type name
findLabelString :: t0 -> Graph.Graph -> Core.Type -> Core.Name -> Core.Name -> Either Errors.Error String
findLabelString cx g source tname labelKey =
    Optionals.cases (Annotations.getTypeAnnotation labelKey source) (Right (Core.unName tname)) (extractString cx g)
-- | Find a projection spec for a field
findProjectionSpec :: t0 -> Graph.Graph -> Core.Name -> Core.Name -> Core.Name -> [Core.FieldType] -> Either Errors.Error (Maybe (Core.FieldType, (Mapping.ValueSpec, (Maybe String))))
findProjectionSpec cx g tname key aliasKey fields =
    Eithers.bind (findSingleFieldWithAnnotationKey cx tname key fields) (\mfield -> Optionals.cases mfield (Right Nothing) (\field -> Optionals.cases (Annotations.getTypeAnnotation key (Core.fieldTypeType field)) (Left (Errors.ErrorOther (Errors.OtherError "findProjectionSpec: missing type annotation for key"))) (\annot -> Eithers.bind (TermsToElements.decodeValueSpec cx g annot) (\spec -> Eithers.bind (Optionals.cases (Annotations.getTypeAnnotation aliasKey (Core.fieldTypeType field)) (Right Nothing) (\t -> Eithers.map (\x -> Just x) (extractString cx g t))) (\alias -> Right (Just (field, (spec, alias))))))))
-- | Find property specs for element fields
findPropertySpecs :: t0 -> Graph.Graph -> Mapping.Schema t1 t2 t3 -> PgModel.ElementKind -> [Core.FieldType] -> Either Errors.Error [(Core.FieldType, (Mapping.ValueSpec, (Maybe String)))]
findPropertySpecs cx g schema kind fields =
    Eithers.mapList (\field ->
      let propKeyKey = Core.Name (Mapping.annotationSchemaPropertyKey (Mapping.schemaAnnotations schema))
          propValueKey = Core.Name (Mapping.annotationSchemaPropertyValue (Mapping.schemaAnnotations schema))
      in (Eithers.bind (Optionals.cases (Annotations.getTypeAnnotation propKeyKey (Core.fieldTypeType field)) (Right Nothing) (\a -> Eithers.map (\x -> Just x) (extractString cx g a))) (\alias -> Eithers.bind (Optionals.cases (Annotations.getTypeAnnotation propValueKey (Core.fieldTypeType field)) (Right Mapping.ValueSpecValue) (TermsToElements.decodeValueSpec cx g)) (\values -> Right (field, (values, alias)))))) (Lists.filter (\field ->
      let annots = Mapping.schemaAnnotations schema
          ignoreKey = Core.Name (Mapping.annotationSchemaIgnore annots)
          specialKeys =
                  case kind of
                    PgModel.ElementKindVertex -> [
                      Core.Name (Mapping.annotationSchemaVertexId annots),
                      (Core.Name (Mapping.annotationSchemaOutEdgeLabel annots)),
                      (Core.Name (Mapping.annotationSchemaInEdgeLabel annots))]
                    PgModel.ElementKindEdge -> [
                      Core.Name (Mapping.annotationSchemaEdgeId annots),
                      (Core.Name (Mapping.annotationSchemaOutVertex annots)),
                      (Core.Name (Mapping.annotationSchemaInVertex annots))]
          allKeys =
                  Lists.concat [
                    [
                      ignoreKey],
                    specialKeys]
          hasSpecialAnnotation =
                  Lists.foldl (\b -> \k -> Logic.or b (Optionals.isGiven (Annotations.getTypeAnnotation k (Core.fieldTypeType field)))) False allKeys
          hasSpecialFieldName = Lists.foldl (\b -> \k -> Logic.or b (Equality.equal (Core.fieldTypeName field) k)) False specialKeys
      in (Logic.not (Logic.or hasSpecialAnnotation hasSpecialFieldName))) fields)
-- | Find a single field with a given annotation key
findSingleFieldWithAnnotationKey :: t0 -> Core.Name -> Core.Name -> [Core.FieldType] -> Either Errors.Error (Maybe Core.FieldType)
findSingleFieldWithAnnotationKey cx tname key fields =

      let matches = Lists.filter (\f -> Optionals.isGiven (Annotations.getTypeAnnotation key (Core.fieldTypeType f))) fields
      in (Logic.ifElse (Equality.gt (Lists.length matches) 1) (Left (Errors.ErrorOther (Errors.OtherError (Strings.cat2 (Strings.cat2 (Strings.cat2 "Multiple fields marked as '" (Core.unName key)) "' in record type ") (Core.unName tname))))) (Right (Lists.maybeHead matches)))
-- | Determine whether the spec has vertex adapters based on direction and out/in specs
hasVertexAdapters :: PgModel.Direction -> Maybe t0 -> Maybe t1 -> Bool
hasVertexAdapters dir mOutSpec mInSpec =
    case dir of
      PgModel.DirectionOut -> Optionals.isGiven mInSpec
      PgModel.DirectionIn -> Optionals.isGiven mOutSpec
      PgModel.DirectionBoth -> Logic.and (Optionals.isGiven mOutSpec) (Optionals.isGiven mInSpec)
-- | Create a projection adapter from a projection spec
projectionAdapter :: t0 -> t1 -> t2 -> Coders.Coder Core.Term t3 -> (Core.FieldType, (Mapping.ValueSpec, t4)) -> String -> Either t5 (Core.Name, (Coders.Adapter Core.Type t2 Core.Term t3))
projectionAdapter cx g idtype coder spec key =

      let field = Pairs.first spec
          values = Pairs.first (Pairs.second spec)
      in (Eithers.bind (TermsToElements.parseValueSpec cx g values) (\traversal -> Right (
        Core.fieldTypeName field,
        Coders.Adapter {
          Coders.adapterIsLossy = True,
          Coders.adapterSource = (Core.fieldTypeType field),
          Coders.adapterTarget = idtype,
          Coders.adapterCoder = Coders.Coder {
            Coders.coderEncode = (\cx_ -> \typ -> Eithers.bind (traverseToSingleTerm cx_ (Strings.cat2 key "-projection") (traversal cx_) typ) (\t -> Coders.coderEncode coder cx_ t)),
            Coders.coderDecode = (\cx_ -> \_ -> Left (Errors.ErrorOther (Errors.OtherError (Strings.cat2 (Strings.cat2 "edge '" key) "' decoding is not yet supported"))))}})))
-- | Create a property adapter from a property spec
propertyAdapter :: Typing.InferenceContext -> t0 -> Mapping.Schema t1 t2 t3 -> (Core.FieldType, (Mapping.ValueSpec, (Maybe String))) -> Either Errors.Error (Coders.Adapter Core.FieldType (PgModel.PropertyType t2) Core.Field (PgModel.Property t3))
propertyAdapter cx g schema spec =

      let tfield = Pairs.first spec
          values = Pairs.first (Pairs.second spec)
          alias = Pairs.second (Pairs.second spec)
          key = PgModel.PropertyKey (Optionals.fromOptional (Core.unName (Core.fieldTypeName tfield)) alias)
      in (Eithers.bind (Coders.coderEncode (Mapping.schemaPropertyTypes schema) cx (Core.fieldTypeType tfield)) (\pt -> Eithers.bind (TermsToElements.parseValueSpec cx g values) (\traversal -> Right (Coders.Adapter {
        Coders.adapterIsLossy = True,
        Coders.adapterSource = tfield,
        Coders.adapterTarget = PgModel.PropertyType {
          PgModel.propertyTypeKey = key,
          PgModel.propertyTypeValue = pt,
          PgModel.propertyTypeRequired = True},
        Coders.adapterCoder = Coders.Coder {
          Coders.coderEncode = (\cx_ -> \dfield -> Eithers.bind (traverseToSingleTerm cx_ "property traversal" (traversal cx_) (Core.fieldTerm dfield)) (\result -> Eithers.bind (Coders.coderEncode (Mapping.schemaPropertyValues schema) cx_ result) (\value -> Right (PgModel.Property {
            PgModel.propertyKey = key,
            PgModel.propertyValue = value})))),
          Coders.coderDecode = (\cx_ -> \_ -> Left (Errors.ErrorOther (Errors.OtherError "property decoding is not yet supported")))}}))))
-- | Extract property types from property adapters
propertyTypes :: [Coders.Adapter t0 (PgModel.PropertyType t1) t2 t3] -> [PgModel.PropertyType t1]
propertyTypes propAdapters =
    Lists.map (\a -> PgModel.PropertyType {
      PgModel.propertyTypeKey = (PgModel.propertyTypeKey (Coders.adapterTarget a)),
      PgModel.propertyTypeValue = (PgModel.propertyTypeValue (Coders.adapterTarget a)),
      PgModel.propertyTypeRequired = True}) propAdapters
-- | Select an edge id from record fields using an id adapter
selectEdgeId :: Typing.InferenceContext -> M.Map Core.Name t0 -> (Core.Name, (Coders.Adapter t1 t2 t0 t3)) -> Either Errors.Error t3
selectEdgeId cx fields ad =

      let fname = Pairs.first ad
          adapter = Pairs.second ad
      in (Optionals.cases (Maps.lookup fname fields) (Left (Errors.ErrorOther (Errors.OtherError (Strings.cat2 (Strings.cat2 "no " (Core.unName fname)) " in record")))) (\t -> Coders.coderEncode (Coders.adapterCoder adapter) cx t))
-- | Select a vertex id from record fields using an id adapter
selectVertexId :: Typing.InferenceContext -> M.Map Core.Name t0 -> (Core.Name, (Coders.Adapter t1 t2 t0 t3)) -> Either Errors.Error t3
selectVertexId cx fields ad =

      let fname = Pairs.first ad
          adapter = Pairs.second ad
      in (Optionals.cases (Maps.lookup fname fields) (Left (Errors.ErrorOther (Errors.OtherError (Strings.cat2 (Strings.cat2 "no " (Core.unName fname)) " in record")))) (\t -> Coders.coderEncode (Coders.adapterCoder adapter) cx t))
-- | Traverse to a single term, failing if zero or multiple terms are found
traverseToSingleTerm :: t0 -> String -> (t1 -> Either Errors.Error [t2]) -> t1 -> Either Errors.Error t2
traverseToSingleTerm cx desc traversal term =
    Eithers.bind (traversal term) (\terms -> Logic.ifElse (Lists.null terms) (Left (Errors.ErrorOther (Errors.OtherError (Strings.cat2 desc " did not resolve to a term")))) (Logic.ifElse (Equality.equal (Lists.length terms) 1) (Optionals.cases (Lists.maybeHead terms) (Left (Errors.ErrorOther (Errors.OtherError (Strings.cat2 desc " resolved to multiple terms")))) (\x -> Right x)) (Left (Errors.ErrorOther (Errors.OtherError (Strings.cat2 desc " resolved to multiple terms"))))))
-- | Create a vertex coder given all components
vertexCoder :: t0 -> Mapping.Schema t1 t2 t3 -> t4 -> t5 -> t6 -> PgModel.VertexLabel -> (Core.Name, (Coders.Adapter t7 t8 Core.Term t3)) -> [Coders.Adapter Core.FieldType (PgModel.PropertyType t5) Core.Field (PgModel.Property t3)] -> [(
  PgModel.Direction,
  (Core.FieldType, (PgModel.EdgeLabel, (Coders.Adapter t9 (PgModel.ElementTypeTree t5) Core.Term (PgModel.ElementTree t3)))))] -> Coders.Adapter t4 (PgModel.ElementTypeTree t5) Core.Term (PgModel.ElementTree t3)
vertexCoder g schema source vidType tname vlabel idAdapter propAdapters edgeAdapters =

      let vtype =
              PgModel.VertexType {
                PgModel.vertexTypeLabel = vlabel,
                PgModel.vertexTypeId = vidType,
                PgModel.vertexTypeProperties = (propertyTypes propAdapters)}
          depTypes = Lists.map (\ea -> Coders.adapterTarget (Pairs.second (Pairs.second (Pairs.second ea)))) edgeAdapters
          target = elementTypeTreeVertex vtype depTypes
      in Coders.Adapter {
        Coders.adapterIsLossy = True,
        Coders.adapterSource = source,
        Coders.adapterTarget = target,
        Coders.adapterCoder = Coders.Coder {
          Coders.coderEncode = (\cx -> \term ->
            let deannot = Strip.deannotateTerm term
                unwrapped =
                        case deannot of
                          Core.TermOptional v0 -> Optionals.fromOptional deannot v0
                          _ -> deannot
                rec =
                        case unwrapped of
                          Core.TermRecord v0 -> v0
                fmap = Resolution.fieldMap (Core.recordFields rec)
            in (Eithers.bind (selectVertexId cx fmap idAdapter) (\vid -> Eithers.bind (encodeProperties cx fmap propAdapters) (\props -> Eithers.bind (Eithers.map (\xs -> Lists.concat xs) (Eithers.mapList (\ea ->
              let eaDir = Pairs.first ea
                  eaField = Pairs.first (Pairs.second ea)
                  eaLabel = Pairs.first (Pairs.second (Pairs.second ea))
                  eaAdapter = Pairs.second (Pairs.second (Pairs.second ea))
              in (Optionals.cases (Maps.lookup (Core.fieldTypeName eaField) fmap) (Right []) (\fterm -> Eithers.map (\tree -> (\x -> case x of
                PgModel.ElementVertex v0 ->
                  let otherid = PgModel.vertexId v0
                      edgeid = Mapping.schemaDefaultEdgeId schema
                      outId =
                              case eaDir of
                                PgModel.DirectionOut -> vid
                                PgModel.DirectionIn -> otherid
                      inId =
                              case eaDir of
                                PgModel.DirectionOut -> otherid
                                PgModel.DirectionIn -> vid
                      edge =
                              PgModel.ElementEdge (PgModel.Edge {
                                PgModel.edgeLabel = eaLabel,
                                PgModel.edgeId = edgeid,
                                PgModel.edgeOut = outId,
                                PgModel.edgeIn = inId,
                                PgModel.edgeProperties = Maps.empty})
                  in [
                    PgModel.ElementTree {
                      PgModel.elementTreeSelf = edge,
                      PgModel.elementTreeDependencies = [
                        tree]}]
                PgModel.ElementEdge v0 ->
                  let fixedEdge =
                          case eaDir of
                            PgModel.DirectionOut -> PgModel.Edge {
                              PgModel.edgeLabel = (PgModel.edgeLabel v0),
                              PgModel.edgeId = (PgModel.edgeId v0),
                              PgModel.edgeOut = vid,
                              PgModel.edgeIn = (PgModel.edgeIn v0),
                              PgModel.edgeProperties = (PgModel.edgeProperties v0)}
                            PgModel.DirectionIn -> PgModel.Edge {
                              PgModel.edgeLabel = (PgModel.edgeLabel v0),
                              PgModel.edgeId = (PgModel.edgeId v0),
                              PgModel.edgeOut = (PgModel.edgeOut v0),
                              PgModel.edgeIn = vid,
                              PgModel.edgeProperties = (PgModel.edgeProperties v0)}
                  in [
                    PgModel.ElementTree {
                      PgModel.elementTreeSelf = (PgModel.ElementEdge fixedEdge),
                      PgModel.elementTreeDependencies = (PgModel.elementTreeDependencies tree)}]) (PgModel.elementTreeSelf tree)) (Coders.coderEncode (Coders.adapterCoder eaAdapter) cx fterm)))) edgeAdapters)) (\deps -> Right (elementTreeVertex (PgModel.Vertex {
              PgModel.vertexLabel = vlabel,
              PgModel.vertexId = vid,
              PgModel.vertexProperties = props}) deps)))))),
          Coders.coderDecode = (\cx -> \_ -> Left (Errors.ErrorOther (Errors.OtherError "vertex decoding is not yet supported")))}}
-- | Create a vertex id adapter
vertexIdAdapter :: t0 -> t1 -> Mapping.Schema t2 t3 t4 -> t5 -> Core.Name -> Core.Name -> [Core.FieldType] -> Either Errors.Error (Core.Name, (Coders.Adapter Core.Type t5 Core.Term t4))
vertexIdAdapter cx g schema vidType name idKey fields =
    Eithers.bind (findIdProjectionSpec cx True name idKey fields) (\mIdSpec -> Optionals.cases mIdSpec (Left (Errors.ErrorOther (Errors.OtherError "vertexIdAdapter: no id projection spec"))) (\idSpec -> projectionAdapter cx g vidType (Mapping.schemaVertexIds schema) idSpec "id"))
