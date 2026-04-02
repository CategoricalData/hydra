-- Note: this is an automatically generated file. Do not edit.

-- | Property graph element coders for mapping Hydra terms to property graph elements

module Hydra.Pg.Coder where

import qualified Hydra.Annotations as Annotations
import qualified Hydra.Coders as Coders
import qualified Hydra.Context as Context
import qualified Hydra.Core as Core
import qualified Hydra.Errors as Errors
import qualified Hydra.Extract.Core as Core_
import qualified Hydra.Graph as Graph
import qualified Hydra.Lib.Eithers as Eithers
import qualified Hydra.Lib.Equality as Equality
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Pairs as Pairs
import qualified Hydra.Lib.Sets as Sets
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Pg.Mapping as Mapping
import qualified Hydra.Pg.Model as Model
import qualified Hydra.Pg.TermsToElements as TermsToElements
import qualified Hydra.Resolution as Resolution
import qualified Hydra.Strip as Strip
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | Check a condition, returning an error if false
check :: t0 -> Bool -> Either t1 () -> Either t1 ()
check _cx b e = Logic.ifElse b (Right ()) e

-- | Check that a record name matches the expected name
checkRecordName :: Context.Context -> Core.Name -> Core.Name -> Either (Context.InContext Errors.Error) ()
checkRecordName cx expected actual =
    check cx (Logic.or (Equality.equal (Core.unName expected) "placeholder") (Equality.equal (Core.unName actual) (Core.unName expected))) (Left (Context.InContext {
      Context.inContextObject = (Errors.ErrorOther (Errors.OtherError (Strings.cat2 (Strings.cat2 (Strings.cat2 "Expected record of type " (Core.unName expected)) ", found record of type ") (Core.unName actual)))),
      Context.inContextContext = cx}))

-- | Construct an edge coder from components
constructEdgeCoder :: Context.Context -> Graph.Graph -> Model.VertexLabel -> Mapping.Schema t0 t1 t2 -> Core.Type -> t1 -> t1 -> Model.Direction -> Core.Name -> [Core.FieldType] -> [Coders.Adapter Core.FieldType (Model.PropertyType t1) Core.Field (Model.Property t2)] -> Maybe (Core.FieldType, (Mapping.ValueSpec, (Maybe String))) -> Maybe (Core.FieldType, (Mapping.ValueSpec, (Maybe String))) -> Either (Context.InContext Errors.Error) (Coders.Adapter Core.Type (Model.ElementTypeTree t1) Core.Term (Model.ElementTree t2))
constructEdgeCoder cx g parentLabel schema source vidType eidType dir name fields propAdapters mOutSpec mInSpec =
    Eithers.bind (findLabelString cx g source name (Core.Name (Mapping.annotationSchemaEdgeLabel (Mapping.schemaAnnotations schema)))) (\labelStr ->
      let label = Model.EdgeLabel labelStr
          vertexIdsSchema = Mapping.schemaVertexIds schema
      in (Eithers.bind (edgeIdAdapter cx g schema eidType name (Core.Name (Mapping.annotationSchemaEdgeId (Mapping.schemaAnnotations schema))) fields) (\idAdapter -> Eithers.bind (Maybes.maybe (Right Nothing) (\s -> Eithers.map (\x -> Just x) (projectionAdapter cx g vidType vertexIdsSchema s "out")) mOutSpec) (\outIdAdapter -> Eithers.bind (Maybes.maybe (Right Nothing) (\s -> Eithers.map (\x -> Just x) (projectionAdapter cx g vidType vertexIdsSchema s "in")) mInSpec) (\inIdAdapter -> Eithers.bind (Maybes.maybe (Right Nothing) (\s -> Eithers.map (\x -> Just x) (findIncidentVertexAdapter cx g schema vidType eidType s)) mOutSpec) (\outVertexAdapter -> Eithers.bind (Maybes.maybe (Right Nothing) (\s -> Eithers.map (\x -> Just x) (findIncidentVertexAdapter cx g schema vidType eidType s)) mInSpec) (\inVertexAdapter ->
        let vertexAdapters =
                Maybes.cat [
                  outVertexAdapter,
                  inVertexAdapter]
        in (Eithers.bind (Maybes.maybe (Right parentLabel) (\spec -> Maybes.maybe (Left (Context.InContext {
          Context.inContextObject = (Errors.ErrorOther (Errors.OtherError "no out-vertex label")),
          Context.inContextContext = cx})) (\a -> Right (Model.VertexLabel a)) (Pairs.second (Pairs.second spec))) mOutSpec) (\outLabel -> Eithers.bind (Maybes.maybe (Right parentLabel) (\spec -> Maybes.maybe (Left (Context.InContext {
          Context.inContextObject = (Errors.ErrorOther (Errors.OtherError "no in-vertex label")),
          Context.inContextContext = cx})) (\a -> Right (Model.VertexLabel a)) (Pairs.second (Pairs.second spec))) mInSpec) (\inLabel -> Right (edgeCoder g dir schema source eidType name label outLabel inLabel idAdapter outIdAdapter inIdAdapter propAdapters vertexAdapters)))))))))))

-- | Construct a vertex coder from components
constructVertexCoder :: Context.Context -> Graph.Graph -> Mapping.Schema t0 t1 t2 -> Core.Type -> t1 -> t1 -> Core.Name -> [Core.FieldType] -> [Coders.Adapter Core.FieldType (Model.PropertyType t1) Core.Field (Model.Property t2)] -> Either (Context.InContext Errors.Error) (Coders.Adapter Core.Type (Model.ElementTypeTree t1) Core.Term (Model.ElementTree t2))
constructVertexCoder cx g schema source vidType eidType name fields propAdapters =
    Eithers.bind (findLabelString cx g source name (Core.Name (Mapping.annotationSchemaVertexLabel (Mapping.schemaAnnotations schema)))) (\labelStr ->
      let label = Model.VertexLabel labelStr
      in (Eithers.bind (vertexIdAdapter cx g schema vidType name (Core.Name (Mapping.annotationSchemaVertexId (Mapping.schemaAnnotations schema))) fields) (\idAdapter -> Eithers.bind (findAdjacenEdgeAdapters cx g schema vidType eidType label Model.DirectionOut fields) (\outEdgeAdapters -> Eithers.bind (findAdjacenEdgeAdapters cx g schema vidType eidType label Model.DirectionIn fields) (\inEdgeAdapters -> Right (vertexCoder g schema source vidType name label idAdapter propAdapters (Lists.concat2 outEdgeAdapters inEdgeAdapters)))))))

-- | Create an edge coder given all components
edgeCoder :: t0 -> Model.Direction -> Mapping.Schema t1 t2 t3 -> t4 -> t5 -> Core.Name -> Model.EdgeLabel -> Model.VertexLabel -> Model.VertexLabel -> Maybe (Core.Name, (Coders.Adapter t6 t7 Core.Term t3)) -> Maybe (Core.Name, (Coders.Adapter t8 t9 Core.Term t3)) -> Maybe (Core.Name, (Coders.Adapter t10 t11 Core.Term t3)) -> [Coders.Adapter Core.FieldType (Model.PropertyType t5) Core.Field (Model.Property t3)] -> [(Core.Name, (Coders.Adapter t12 t13 Core.Term (Model.ElementTree t3)))] -> Coders.Adapter t4 (Model.ElementTypeTree t5) Core.Term (Model.ElementTree t3)
edgeCoder g dir schema source eidType tname label outLabel inLabel mIdAdapter outAdapter inAdapter propAdapters vertexAdapters =

      let et =
              Model.EdgeType {
                Model.edgeTypeLabel = label,
                Model.edgeTypeId = eidType,
                Model.edgeTypeOut = outLabel,
                Model.edgeTypeIn = inLabel,
                Model.edgeTypeProperties = (propertyTypes propAdapters)}
      in Coders.Adapter {
        Coders.adapterIsLossy = True,
        Coders.adapterSource = source,
        Coders.adapterTarget = (elementTypeTreeEdge et []),
        Coders.adapterCoder = Coders.Coder {
          Coders.coderEncode = (\cx -> \term ->
            let deannot = Strip.deannotateTerm term
                unwrapped =
                        case deannot of
                          Core.TermMaybe v0 -> Maybes.fromMaybe deannot v0
                          _ -> deannot
                rec =
                        case unwrapped of
                          Core.TermRecord v0 -> v0
            in (Eithers.bind (checkRecordName cx tname (Core.recordTypeName rec)) (\_chk ->
              let fieldsm = Resolution.fieldMap (Core.recordFields rec)
              in (Eithers.bind (Maybes.maybe (Right (Mapping.schemaDefaultEdgeId schema)) (selectEdgeId cx fieldsm) mIdAdapter) (\edgeId -> Eithers.bind (encodeProperties cx fieldsm propAdapters) (\props ->
                let getVertexId =
                        \dirCheck -> \adapter -> Maybes.maybe (Right (Mapping.schemaDefaultVertexId schema)) (selectVertexId cx fieldsm) (Logic.ifElse (Equality.equal dir dirCheck) Nothing adapter)
                in (Eithers.bind (getVertexId Model.DirectionOut outAdapter) (\outId -> Eithers.bind (getVertexId Model.DirectionIn inAdapter) (\inId -> Eithers.bind (Eithers.map (\xs -> Maybes.cat xs) (Eithers.mapList (\va ->
                  let fname = Pairs.first va
                      ad = Pairs.second va
                  in (Maybes.maybe (Right Nothing) (\fterm -> Eithers.map (\x -> Just x) (Coders.coderEncode (Coders.adapterCoder ad) cx fterm)) (Maps.lookup fname fieldsm))) vertexAdapters)) (\deps -> Right (elementTreeEdge (Model.Edge {
                  Model.edgeLabel = label,
                  Model.edgeId = edgeId,
                  Model.edgeOut = outId,
                  Model.edgeIn = inId,
                  Model.edgeProperties = props}) deps))))))))))),
          Coders.coderDecode = (\cx -> \_ -> Left (Context.InContext {
            Context.inContextObject = (Errors.ErrorOther (Errors.OtherError "edge decoding is not yet supported")),
            Context.inContextContext = cx}))}}

-- | Create an edge id adapter
edgeIdAdapter :: Context.Context -> t0 -> Mapping.Schema t1 t2 t3 -> t4 -> Core.Name -> Core.Name -> [Core.FieldType] -> Either (Context.InContext Errors.Error) (Maybe (Core.Name, (Coders.Adapter Core.Type t4 Core.Term t3)))
edgeIdAdapter cx g schema eidType name idKey fields =
    Eithers.bind (findIdProjectionSpec cx False name idKey fields) (\mIdSpec -> Maybes.maybe (Right Nothing) (\idSpec -> Eithers.map (\x -> Just x) (projectionAdapter cx g eidType (Mapping.schemaEdgeIds schema) idSpec "id")) mIdSpec)

-- | Construct an element adapter for a given type, interpreting it either as a vertex specification or an edge specification
elementCoder :: Maybe (Model.Direction, Model.VertexLabel) -> Mapping.Schema t0 t1 t2 -> Core.Type -> t1 -> t1 -> Context.Context -> Graph.Graph -> Either (Context.InContext Errors.Error) (Coders.Adapter Core.Type (Model.ElementTypeTree t1) Core.Term (Model.ElementTree t2))
elementCoder mparent schema source vidType eidType cx g =

      let dir = Maybes.maybe Model.DirectionBoth (\p -> Pairs.first p) mparent
          parentLabel = Maybes.maybe (Model.VertexLabel "NOLABEL") (\p -> Pairs.second p) mparent
      in case (Strip.deannotateType source) of
        Core.TypeMaybe v0 -> elementCoder mparent schema v0 vidType eidType cx g
        Core.TypeRecord v0 ->
          let name = Core.Name "placeholder"
              outVertexKey = Core.Name (Mapping.annotationSchemaOutVertex (Mapping.schemaAnnotations schema))
              outVertexLabelKey = Core.Name (Mapping.annotationSchemaOutVertexLabel (Mapping.schemaAnnotations schema))
              inVertexKey = Core.Name (Mapping.annotationSchemaInVertex (Mapping.schemaAnnotations schema))
              inVertexLabelKey = Core.Name (Mapping.annotationSchemaInVertexLabel (Mapping.schemaAnnotations schema))
          in (Eithers.bind (findProjectionSpec cx g name outVertexKey outVertexLabelKey v0) (\mOutSpec -> Eithers.bind (findProjectionSpec cx g name inVertexKey inVertexLabelKey v0) (\mInSpec ->
            let kind = Logic.ifElse (hasVertexAdapters dir mOutSpec mInSpec) Model.ElementKindEdge Model.ElementKindVertex
            in (Eithers.bind (findPropertySpecs cx g schema kind v0) (\propSpecs -> Eithers.bind (Eithers.mapList (propertyAdapter cx g schema) propSpecs) (\propAdapters -> case kind of
              Model.ElementKindVertex -> constructVertexCoder cx g schema source vidType eidType name v0 propAdapters
              Model.ElementKindEdge -> constructEdgeCoder cx g parentLabel schema source vidType eidType dir name v0 propAdapters mOutSpec mInSpec))))))
        _ -> Left (Context.InContext {
          Context.inContextObject = (Errors.ErrorOther (Errors.OtherError (Strings.cat2 (Strings.cat2 (Strings.cat2 "Expected " "record type") ", found: ") "other type"))),
          Context.inContextContext = cx})

-- | Create an element tree for an edge
elementTreeEdge :: Model.Edge t0 -> [Model.ElementTree t0] -> Model.ElementTree t0
elementTreeEdge edge deps =
    Model.ElementTree {
      Model.elementTreeSelf = (Model.ElementEdge edge),
      Model.elementTreeDependencies = deps}

-- | Create an element tree for a vertex
elementTreeVertex :: Model.Vertex t0 -> [Model.ElementTree t0] -> Model.ElementTree t0
elementTreeVertex vertex deps =
    Model.ElementTree {
      Model.elementTreeSelf = (Model.ElementVertex vertex),
      Model.elementTreeDependencies = deps}

-- | Create an element type tree for an edge type
elementTypeTreeEdge :: Model.EdgeType t0 -> [Model.ElementTypeTree t0] -> Model.ElementTypeTree t0
elementTypeTreeEdge etype deps =
    Model.ElementTypeTree {
      Model.elementTypeTreeSelf = (Model.ElementTypeEdge etype),
      Model.elementTypeTreeDependencies = deps}

-- | Create an element type tree for a vertex type
elementTypeTreeVertex :: Model.VertexType t0 -> [Model.ElementTypeTree t0] -> Model.ElementTypeTree t0
elementTypeTreeVertex vtype deps =
    Model.ElementTypeTree {
      Model.elementTypeTreeSelf = (Model.ElementTypeVertex vtype),
      Model.elementTypeTreeDependencies = deps}

-- | Encode all properties from a field map using property adapters
encodeProperties :: Context.Context -> M.Map Core.Name Core.Term -> [Coders.Adapter Core.FieldType t0 Core.Field (Model.Property t1)] -> Either (Context.InContext Errors.Error) (M.Map Model.PropertyKey t1)
encodeProperties cx fields adapters =
    Eithers.map (\props -> Maps.fromList (Lists.map (\prop -> (Model.propertyKey prop, (Model.propertyValue prop))) props)) (Eithers.map (\xs -> Maybes.cat xs) (Eithers.mapList (encodeProperty cx fields) adapters))

-- | Encode a single property from a field map using a property adapter
encodeProperty :: Context.Context -> M.Map Core.Name Core.Term -> Coders.Adapter Core.FieldType t0 Core.Field t1 -> Either (Context.InContext Errors.Error) (Maybe t1)
encodeProperty cx fields adapter =

      let fname = Core.fieldTypeName (Coders.adapterSource adapter)
          ftyp = Strip.deannotateType (Core.fieldTypeType (Coders.adapterSource adapter))
          isMaybe =
                  case ftyp of
                    Core.TypeMaybe _ -> True
                    _ -> False
          encodeValue =
                  \v -> Eithers.map (\x -> Just x) (Coders.coderEncode (Coders.adapterCoder adapter) cx (Core.Field {
                    Core.fieldName = fname,
                    Core.fieldTerm = v}))
      in (Maybes.maybe (Logic.ifElse isMaybe (Right Nothing) (Left (Context.InContext {
        Context.inContextObject = (Errors.ErrorOther (Errors.OtherError (Strings.cat2 "expected field not found in record: " (Core.unName fname)))),
        Context.inContextContext = cx}))) (\value -> Logic.ifElse isMaybe (case (Strip.deannotateTerm value) of
        Core.TermMaybe v0 -> Maybes.maybe (Right Nothing) encodeValue v0
        _ -> encodeValue value) (encodeValue value)) (Maps.lookup fname fields))

-- | Extract a string from a term
extractString :: Context.Context -> Graph.Graph -> Core.Term -> Either (Context.InContext Errors.Error) String
extractString cx g t = Core_.string cx g t

-- | Find adjacent edge adapters for a given direction
findAdjacenEdgeAdapters :: Context.Context -> Graph.Graph -> Mapping.Schema t0 t1 t2 -> t1 -> t1 -> Model.VertexLabel -> Model.Direction -> [Core.FieldType] -> Either (Context.InContext Errors.Error) [(Model.Direction, (Core.FieldType, (Model.EdgeLabel, (Coders.Adapter Core.Type (Model.ElementTypeTree t1) Core.Term (Model.ElementTree t2)))))]
findAdjacenEdgeAdapters cx g schema vidType eidType parentLabel dir fields =
    Eithers.map (\xs -> Maybes.cat xs) (Eithers.mapList (\field ->
      let key =
              Core.Name (case dir of
                Model.DirectionOut -> Mapping.annotationSchemaOutEdgeLabel (Mapping.schemaAnnotations schema)
                Model.DirectionIn -> Mapping.annotationSchemaInEdgeLabel (Mapping.schemaAnnotations schema))
      in (Maybes.maybe (Right Nothing) (\a -> Eithers.bind (extractString cx g a) (\labelStr -> Eithers.bind (elementCoder (Just (dir, parentLabel)) schema (Core.fieldTypeType field) vidType eidType cx g) (\elad -> Right (Just (dir, (field, (Model.EdgeLabel labelStr, elad))))))) (Annotations.getTypeAnnotation key (Core.fieldTypeType field)))) fields)

-- | Find an id projection spec for a field
findIdProjectionSpec :: Context.Context -> Bool -> Core.Name -> Core.Name -> [Core.FieldType] -> Either (Context.InContext Errors.Error) (Maybe (Core.FieldType, (Mapping.ValueSpec, (Maybe String))))
findIdProjectionSpec cx required tname idKey fields =
    Eithers.bind (findSingleFieldWithAnnotationKey cx tname idKey fields) (\mid -> Maybes.maybe (Logic.ifElse required (Left (Context.InContext {
      Context.inContextObject = (Errors.ErrorOther (Errors.OtherError (Strings.cat2 (Strings.cat2 "no " (Core.unName idKey)) " field"))),
      Context.inContextContext = cx})) (Right Nothing)) (\mi -> Eithers.map (\spec -> Just (mi, (spec, (Maybes.map (\s -> Strings.toUpper s) Nothing)))) (Maybes.maybe (Right Mapping.ValueSpecValue) (TermsToElements.decodeValueSpec cx (Graph.Graph {
      Graph.graphBoundTerms = Maps.empty,
      Graph.graphBoundTypes = Maps.empty,
      Graph.graphClassConstraints = Maps.empty,
      Graph.graphLambdaVariables = Sets.empty,
      Graph.graphMetadata = Maps.empty,
      Graph.graphPrimitives = Maps.empty,
      Graph.graphSchemaTypes = Maps.empty,
      Graph.graphTypeVariables = Sets.empty})) (Annotations.getTypeAnnotation idKey (Core.fieldTypeType mi)))) mid)

-- | Find an incident vertex adapter for a projection spec
findIncidentVertexAdapter :: Context.Context -> Graph.Graph -> Mapping.Schema t0 t1 t2 -> t1 -> t1 -> (Core.FieldType, (Mapping.ValueSpec, (Maybe String))) -> Either (Context.InContext Errors.Error) (Core.Name, (Coders.Adapter Core.Type (Model.ElementTypeTree t1) Core.Term (Model.ElementTree t2)))
findIncidentVertexAdapter cx g schema vidType eidType spec =

      let field = Pairs.first spec
      in (Eithers.bind (elementCoder Nothing schema (Core.fieldTypeType field) vidType eidType cx g) (\adapter -> Right (Core.fieldTypeName field, adapter)))

-- | Find a label string from annotations or the type name
findLabelString :: Context.Context -> Graph.Graph -> Core.Type -> Core.Name -> Core.Name -> Either (Context.InContext Errors.Error) String
findLabelString cx g source tname labelKey =
    Maybes.maybe (Right (Core.unName tname)) (extractString cx g) (Annotations.getTypeAnnotation labelKey source)

-- | Find a projection spec for a field
findProjectionSpec :: Context.Context -> Graph.Graph -> Core.Name -> Core.Name -> Core.Name -> [Core.FieldType] -> Either (Context.InContext Errors.Error) (Maybe (Core.FieldType, (Mapping.ValueSpec, (Maybe String))))
findProjectionSpec cx g tname key aliasKey fields =
    Eithers.bind (findSingleFieldWithAnnotationKey cx tname key fields) (\mfield -> Maybes.maybe (Right Nothing) (\field -> Eithers.bind (TermsToElements.decodeValueSpec cx g (Maybes.fromJust (Annotations.getTypeAnnotation key (Core.fieldTypeType field)))) (\spec -> Eithers.bind (Maybes.maybe (Right Nothing) (\t -> Eithers.map (\x -> Just x) (extractString cx g t)) (Annotations.getTypeAnnotation aliasKey (Core.fieldTypeType field))) (\alias -> Right (Just (field, (spec, alias)))))) mfield)

-- | Find property specs for element fields
findPropertySpecs :: Context.Context -> Graph.Graph -> Mapping.Schema t0 t1 t2 -> Model.ElementKind -> [Core.FieldType] -> Either (Context.InContext Errors.Error) [(Core.FieldType, (Mapping.ValueSpec, (Maybe String)))]
findPropertySpecs cx g schema kind fields =
    Eithers.mapList (\field ->
      let propKeyKey = Core.Name (Mapping.annotationSchemaPropertyKey (Mapping.schemaAnnotations schema))
          propValueKey = Core.Name (Mapping.annotationSchemaPropertyValue (Mapping.schemaAnnotations schema))
      in (Eithers.bind (Maybes.maybe (Right Nothing) (\a -> Eithers.map (\x -> Just x) (extractString cx g a)) (Annotations.getTypeAnnotation propKeyKey (Core.fieldTypeType field))) (\alias -> Eithers.bind (Maybes.maybe (Right Mapping.ValueSpecValue) (TermsToElements.decodeValueSpec cx g) (Annotations.getTypeAnnotation propValueKey (Core.fieldTypeType field))) (\values -> Right (field, (values, alias)))))) (Lists.filter (\field ->
      let annots = Mapping.schemaAnnotations schema
          ignoreKey = Core.Name (Mapping.annotationSchemaIgnore annots)
          specialKeys =
                  case kind of
                    Model.ElementKindVertex -> [
                      Core.Name (Mapping.annotationSchemaVertexId annots),
                      (Core.Name (Mapping.annotationSchemaOutEdgeLabel annots)),
                      (Core.Name (Mapping.annotationSchemaInEdgeLabel annots))]
                    Model.ElementKindEdge -> [
                      Core.Name (Mapping.annotationSchemaEdgeId annots),
                      (Core.Name (Mapping.annotationSchemaOutVertex annots)),
                      (Core.Name (Mapping.annotationSchemaInVertex annots))]
          allKeys =
                  Lists.concat [
                    [
                      ignoreKey],
                    specialKeys]
          hasSpecialAnnotation =
                  Lists.foldl (\b -> \k -> Logic.or b (Maybes.isJust (Annotations.getTypeAnnotation k (Core.fieldTypeType field)))) False allKeys
          hasSpecialFieldName = Lists.foldl (\b -> \k -> Logic.or b (Equality.equal (Core.fieldTypeName field) k)) False specialKeys
      in (Logic.not (Logic.or hasSpecialAnnotation hasSpecialFieldName))) fields)

-- | Find a single field with a given annotation key
findSingleFieldWithAnnotationKey :: Context.Context -> Core.Name -> Core.Name -> [Core.FieldType] -> Either (Context.InContext Errors.Error) (Maybe Core.FieldType)
findSingleFieldWithAnnotationKey cx tname key fields =

      let matches = Lists.filter (\f -> Maybes.isJust (Annotations.getTypeAnnotation key (Core.fieldTypeType f))) fields
      in (Logic.ifElse (Equality.gt (Lists.length matches) 1) (Left (Context.InContext {
        Context.inContextObject = (Errors.ErrorOther (Errors.OtherError (Strings.cat2 (Strings.cat2 (Strings.cat2 "Multiple fields marked as '" (Core.unName key)) "' in record type ") (Core.unName tname)))),
        Context.inContextContext = cx})) (Right (Lists.safeHead matches)))

-- | Determine whether the spec has vertex adapters based on direction and out/in specs
hasVertexAdapters :: Model.Direction -> Maybe t0 -> Maybe t1 -> Bool
hasVertexAdapters dir mOutSpec mInSpec =
    case dir of
      Model.DirectionOut -> Maybes.isJust mInSpec
      Model.DirectionIn -> Maybes.isJust mOutSpec
      Model.DirectionBoth -> Logic.and (Maybes.isJust mOutSpec) (Maybes.isJust mInSpec)

-- | Create a projection adapter from a projection spec
projectionAdapter :: t0 -> t1 -> t2 -> Coders.Coder Core.Term t3 -> (Core.FieldType, (Mapping.ValueSpec, t4)) -> String -> Either t5 (Core.Name, (Coders.Adapter Core.Type t2 Core.Term t3))
projectionAdapter cx g idtype coder spec key =

      let field = Pairs.first spec
          values = Pairs.first (Pairs.second spec)
      in (Eithers.bind (TermsToElements.parseValueSpec cx g values) (\traversal -> Right (Core.fieldTypeName field, Coders.Adapter {
        Coders.adapterIsLossy = True,
        Coders.adapterSource = (Core.fieldTypeType field),
        Coders.adapterTarget = idtype,
        Coders.adapterCoder = Coders.Coder {
          Coders.coderEncode = (\cx_ -> \typ -> Eithers.bind (traverseToSingleTerm cx_ (Strings.cat2 key "-projection") (traversal cx_) typ) (\t -> Coders.coderEncode coder cx_ t)),
          Coders.coderDecode = (\cx_ -> \_ -> Left (Context.InContext {
            Context.inContextObject = (Errors.ErrorOther (Errors.OtherError (Strings.cat2 (Strings.cat2 "edge '" key) "' decoding is not yet supported"))),
            Context.inContextContext = cx_}))}})))

-- | Create a property adapter from a property spec
propertyAdapter :: Context.Context -> t0 -> Mapping.Schema t1 t2 t3 -> (Core.FieldType, (Mapping.ValueSpec, (Maybe String))) -> Either (Context.InContext Errors.Error) (Coders.Adapter Core.FieldType (Model.PropertyType t2) Core.Field (Model.Property t3))
propertyAdapter cx g schema spec =

      let tfield = Pairs.first spec
          values = Pairs.first (Pairs.second spec)
          alias = Pairs.second (Pairs.second spec)
          key = Model.PropertyKey (Maybes.fromMaybe (Core.unName (Core.fieldTypeName tfield)) alias)
      in (Eithers.bind (Coders.coderEncode (Mapping.schemaPropertyTypes schema) cx (Core.fieldTypeType tfield)) (\pt -> Eithers.bind (TermsToElements.parseValueSpec cx g values) (\traversal -> Right (Coders.Adapter {
        Coders.adapterIsLossy = True,
        Coders.adapterSource = tfield,
        Coders.adapterTarget = Model.PropertyType {
          Model.propertyTypeKey = key,
          Model.propertyTypeValue = pt,
          Model.propertyTypeRequired = True},
        Coders.adapterCoder = Coders.Coder {
          Coders.coderEncode = (\cx_ -> \dfield -> Eithers.bind (traverseToSingleTerm cx_ "property traversal" (traversal cx_) (Core.fieldTerm dfield)) (\result -> Eithers.bind (Coders.coderEncode (Mapping.schemaPropertyValues schema) cx_ result) (\value -> Right (Model.Property {
            Model.propertyKey = key,
            Model.propertyValue = value})))),
          Coders.coderDecode = (\cx_ -> \_ -> Left (Context.InContext {
            Context.inContextObject = (Errors.ErrorOther (Errors.OtherError "property decoding is not yet supported")),
            Context.inContextContext = cx_}))}}))))

-- | Extract property types from property adapters
propertyTypes :: [Coders.Adapter t0 (Model.PropertyType t1) t2 t3] -> [Model.PropertyType t1]
propertyTypes propAdapters =
    Lists.map (\a -> Model.PropertyType {
      Model.propertyTypeKey = (Model.propertyTypeKey (Coders.adapterTarget a)),
      Model.propertyTypeValue = (Model.propertyTypeValue (Coders.adapterTarget a)),
      Model.propertyTypeRequired = True}) propAdapters

-- | Select an edge id from record fields using an id adapter
selectEdgeId :: Context.Context -> M.Map Core.Name t0 -> (Core.Name, (Coders.Adapter t1 t2 t0 t3)) -> Either (Context.InContext Errors.Error) t3
selectEdgeId cx fields ad =

      let fname = Pairs.first ad
          adapter = Pairs.second ad
      in (Maybes.maybe (Left (Context.InContext {
        Context.inContextObject = (Errors.ErrorOther (Errors.OtherError (Strings.cat2 (Strings.cat2 "no " (Core.unName fname)) " in record"))),
        Context.inContextContext = cx})) (\t -> Coders.coderEncode (Coders.adapterCoder adapter) cx t) (Maps.lookup fname fields))

-- | Select a vertex id from record fields using an id adapter
selectVertexId :: Context.Context -> M.Map Core.Name t0 -> (Core.Name, (Coders.Adapter t1 t2 t0 t3)) -> Either (Context.InContext Errors.Error) t3
selectVertexId cx fields ad =

      let fname = Pairs.first ad
          adapter = Pairs.second ad
      in (Maybes.maybe (Left (Context.InContext {
        Context.inContextObject = (Errors.ErrorOther (Errors.OtherError (Strings.cat2 (Strings.cat2 "no " (Core.unName fname)) " in record"))),
        Context.inContextContext = cx})) (\t -> Coders.coderEncode (Coders.adapterCoder adapter) cx t) (Maps.lookup fname fields))

-- | Traverse to a single term, failing if zero or multiple terms are found
traverseToSingleTerm :: Context.Context -> String -> (t0 -> Either (Context.InContext Errors.Error) [t1]) -> t0 -> Either (Context.InContext Errors.Error) t1
traverseToSingleTerm cx desc traversal term =
    Eithers.bind (traversal term) (\terms -> Logic.ifElse (Lists.null terms) (Left (Context.InContext {
      Context.inContextObject = (Errors.ErrorOther (Errors.OtherError (Strings.cat2 desc " did not resolve to a term"))),
      Context.inContextContext = cx})) (Logic.ifElse (Equality.equal (Lists.length terms) 1) (Right (Lists.head terms)) (Left (Context.InContext {
      Context.inContextObject = (Errors.ErrorOther (Errors.OtherError (Strings.cat2 desc " resolved to multiple terms"))),
      Context.inContextContext = cx}))))

-- | Create a vertex coder given all components
vertexCoder :: t0 -> Mapping.Schema t1 t2 t3 -> t4 -> t5 -> t6 -> Model.VertexLabel -> (Core.Name, (Coders.Adapter t7 t8 Core.Term t3)) -> [Coders.Adapter Core.FieldType (Model.PropertyType t5) Core.Field (Model.Property t3)] -> [(Model.Direction, (Core.FieldType, (Model.EdgeLabel, (Coders.Adapter t9 (Model.ElementTypeTree t5) Core.Term (Model.ElementTree t3)))))] -> Coders.Adapter t4 (Model.ElementTypeTree t5) Core.Term (Model.ElementTree t3)
vertexCoder g schema source vidType tname vlabel idAdapter propAdapters edgeAdapters =

      let vtype =
              Model.VertexType {
                Model.vertexTypeLabel = vlabel,
                Model.vertexTypeId = vidType,
                Model.vertexTypeProperties = (propertyTypes propAdapters)}
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
                          Core.TermMaybe v0 -> Maybes.fromMaybe deannot v0
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
              in (Maybes.maybe (Right []) (\fterm -> Eithers.map (\tree -> (\x -> case x of
                Model.ElementVertex v0 ->
                  let otherid = Model.vertexId v0
                      edgeid = Mapping.schemaDefaultEdgeId schema
                      outId =
                              case eaDir of
                                Model.DirectionOut -> vid
                                Model.DirectionIn -> otherid
                      inId =
                              case eaDir of
                                Model.DirectionOut -> otherid
                                Model.DirectionIn -> vid
                      edge =
                              Model.ElementEdge (Model.Edge {
                                Model.edgeLabel = eaLabel,
                                Model.edgeId = edgeid,
                                Model.edgeOut = outId,
                                Model.edgeIn = inId,
                                Model.edgeProperties = Maps.empty})
                  in [
                    Model.ElementTree {
                      Model.elementTreeSelf = edge,
                      Model.elementTreeDependencies = [
                        tree]}]
                Model.ElementEdge v0 ->
                  let fixedEdge =
                          case eaDir of
                            Model.DirectionOut -> Model.Edge {
                              Model.edgeLabel = (Model.edgeLabel v0),
                              Model.edgeId = (Model.edgeId v0),
                              Model.edgeOut = vid,
                              Model.edgeIn = (Model.edgeIn v0),
                              Model.edgeProperties = (Model.edgeProperties v0)}
                            Model.DirectionIn -> Model.Edge {
                              Model.edgeLabel = (Model.edgeLabel v0),
                              Model.edgeId = (Model.edgeId v0),
                              Model.edgeOut = (Model.edgeOut v0),
                              Model.edgeIn = vid,
                              Model.edgeProperties = (Model.edgeProperties v0)}
                  in [
                    Model.ElementTree {
                      Model.elementTreeSelf = (Model.ElementEdge fixedEdge),
                      Model.elementTreeDependencies = (Model.elementTreeDependencies tree)}]) (Model.elementTreeSelf tree)) (Coders.coderEncode (Coders.adapterCoder eaAdapter) cx fterm)) (Maps.lookup (Core.fieldTypeName eaField) fmap))) edgeAdapters)) (\deps -> Right (elementTreeVertex (Model.Vertex {
              Model.vertexLabel = vlabel,
              Model.vertexId = vid,
              Model.vertexProperties = props}) deps)))))),
          Coders.coderDecode = (\cx -> \_ -> Left (Context.InContext {
            Context.inContextObject = (Errors.ErrorOther (Errors.OtherError "vertex decoding is not yet supported")),
            Context.inContextContext = cx}))}}

-- | Create a vertex id adapter
vertexIdAdapter :: Context.Context -> t0 -> Mapping.Schema t1 t2 t3 -> t4 -> Core.Name -> Core.Name -> [Core.FieldType] -> Either (Context.InContext Errors.Error) (Core.Name, (Coders.Adapter Core.Type t4 Core.Term t3))
vertexIdAdapter cx g schema vidType name idKey fields =
    Eithers.bind (findIdProjectionSpec cx True name idKey fields) (\mIdSpec -> Eithers.bind (Right (Maybes.fromJust mIdSpec)) (\idSpec -> projectionAdapter cx g vidType (Mapping.schemaVertexIds schema) idSpec "id"))
