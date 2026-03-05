module Hydra.Ext.Staging.Pg.Coder (
  elementCoder,
) where

import Hydra.Kernel
import Hydra.Pg.Mapping
import Hydra.Ext.Staging.Pg.TermsToElements
import qualified Hydra.Pg.Model as PG
import qualified Hydra.Extract.Core as ExtractCore
import qualified Hydra.Dsl.Terms as Terms

import qualified Control.Monad as CM
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Maybe as Y


type Result a = Either (InContext OtherError) a

err :: Context -> String -> Result a
err cx msg = Left (InContext (OtherError msg) cx)

unexpectedE :: Context -> String -> String -> Result a
unexpectedE cx expected found = err cx $ "Expected " ++ expected ++ ", found: " ++ found

-- | Bridge an ExtractCore function into Result
extractString :: Context -> Graph -> Term -> Result String
extractString cx g t = ExtractCore.string cx g t

type ElementAdapter t v = Adapter Type (PG.ElementTypeTree t) Term (PG.ElementTree v)

type PropertyAdapter t v = Adapter (FieldType) (PG.PropertyType t) Field (PG.Property v)

type IdAdapter t v = (Name, Adapter Type t Term v)

type IncidentElementAdapter t v = (Name, ElementAdapter t v)

data AdjacentEdgeAdapter a t v = AdjacentEdgeAdapter {
  adjacentEdgeAdapterDirection :: PG.Direction,
  adjacentEdgeAdapterField :: FieldType,
  adjacentEdgeAdapterLabel :: PG.EdgeLabel,
  adjacentEdgeAdapterAdapter :: ElementAdapter t v}

data ProjectionSpec a = ProjectionSpec {
  projectionSpecField :: FieldType,
  projectionSpecValues :: ValueSpec,
  projectionSpecAlias :: Maybe String}

vertexLabelKey = Name . annotationSchemaVertexLabel . schemaAnnotations
edgeLabelKey = Name . annotationSchemaEdgeLabel . schemaAnnotations
vertexIdKey = Name . annotationSchemaVertexId . schemaAnnotations
edgeIdKey = Name . annotationSchemaEdgeId . schemaAnnotations
propertyKeyKey = Name . annotationSchemaPropertyKey . schemaAnnotations
propertyValueKey = Name . annotationSchemaPropertyValue . schemaAnnotations
outVertexKey = Name . annotationSchemaOutVertex . schemaAnnotations
outVertexLabelKey = Name . annotationSchemaOutVertexLabel . schemaAnnotations
inVertexKey = Name . annotationSchemaInVertex . schemaAnnotations
inVertexLabelKey = Name . annotationSchemaInVertexLabel . schemaAnnotations
outEdgeLabelKey = Name . annotationSchemaOutEdgeLabel . schemaAnnotations
inEdgeLabelKey = Name . annotationSchemaInEdgeLabel . schemaAnnotations
ignoreKey = Name . annotationSchemaIgnore . schemaAnnotations

check :: Context -> Bool -> Result () -> Result ()
check _ b e = if b then pure () else e

checkRecordName :: Context -> Name -> Name -> Result ()
checkRecordName cx expected actual = check cx (actual == expected) $
  unexpectedE cx ("record of type " ++ unName expected) ("record of type " ++ unName actual)

constructEdgeCoder :: (Show t, Show v)
                   => Context -> Graph -> PG.VertexLabel -> Schema Graph t v -> Type -> t -> t -> PG.Direction -> Name -> [FieldType]
                   -> [PropertyAdapter t v] -> Y.Maybe (ProjectionSpec v) -> Y.Maybe (ProjectionSpec v)
                   -> Result (ElementAdapter t v)
constructEdgeCoder cx g parentLabel schema source vidType eidType dir name fields propAdapters mOutSpec mInSpec = do
  label <- PG.EdgeLabel <$> findLabelString cx g source name (edgeLabelKey schema)
  idAdapter <- edgeIdAdapter cx g schema eidType name (edgeIdKey schema) fields

  outIdAdapter <- traverse (\s -> projectionAdapter cx g vidType (schemaVertexIds schema) s "out") mOutSpec
  inIdAdapter <- traverse (\s -> projectionAdapter cx g vidType (schemaVertexIds schema) s "in") mInSpec

  outVertexAdapter <- traverse (findIncidentVertexAdapter cx g schema vidType eidType) mOutSpec
  inVertexAdapter <- traverse (findIncidentVertexAdapter cx g schema vidType eidType) mInSpec
  let vertexAdapters = Y.catMaybes [outVertexAdapter, inVertexAdapter]

  outLabel <- case mOutSpec of
    Nothing -> pure parentLabel
    Just spec -> Y.maybe (err cx "no out-vertex label") (pure . PG.VertexLabel) $ projectionSpecAlias spec
  inLabel <- case mInSpec of
    Nothing -> pure parentLabel
    Just spec -> Y.maybe (err cx "no in-vertex label") (pure . PG.VertexLabel) $ projectionSpecAlias spec
  return $ edgeCoder g dir schema source eidType name label outLabel inLabel idAdapter outIdAdapter inIdAdapter propAdapters vertexAdapters

constructVertexCoder :: (Show t, Show v)
                     => Context -> Graph -> Schema Graph t v -> Type -> t -> t -> Name -> [FieldType] -> [PropertyAdapter t v]
                     -> Result (ElementAdapter t v)
constructVertexCoder cx g schema source vidType eidType name fields propAdapters = do
  label <- PG.VertexLabel <$> findLabelString cx g source name (vertexLabelKey schema)
  idAdapter <- vertexIdAdapter cx g schema vidType name (vertexIdKey schema) fields
  outEdgeAdapters <- findAdjacenEdgeAdapters cx g schema vidType eidType label PG.DirectionOut fields
  inEdgeAdapters <- findAdjacenEdgeAdapters cx g schema vidType eidType label PG.DirectionIn fields
  return $ vertexCoder g schema source vidType name label idAdapter propAdapters (outEdgeAdapters ++ inEdgeAdapters)

edgeCoder
  :: Graph
  -> PG.Direction
  -> Schema Graph t v
  -> Type
  -> t
  -> Name
  -> PG.EdgeLabel -> PG.VertexLabel -> PG.VertexLabel
  -> Maybe (IdAdapter t v) -> Maybe (IdAdapter t v) -> Maybe (IdAdapter t v) -> [PropertyAdapter t v]
  -> [IncidentElementAdapter t v]
  -> ElementAdapter t v
edgeCoder g dir schema source eidType tname label outLabel inLabel mIdAdapter outAdapter inAdapter propAdapters vertexAdapters
    = Adapter lossy source (elementTypeTreeEdge et []) coder
  where
    et = PG.EdgeType label eidType outLabel inLabel $ propertyTypes propAdapters
    coder = Coder encode decode
      where
        encode cx term = case deannotateTerm term of
          TermMaybe (Just ot) -> encode cx ot
          TermRecord (Record tname' fields) -> do
              checkRecordName cx tname tname'
              let fieldsm = fieldMap fields
              id <- case mIdAdapter of
                Nothing -> pure $ schemaDefaultEdgeId schema
                Just ad -> selectEdgeId cx fieldsm ad
              props <- encodeProperties cx fieldsm propAdapters
              outId <- getVertexId cx PG.DirectionOut fieldsm outAdapter
              inId <- getVertexId cx PG.DirectionIn fieldsm inAdapter

              deps <- Y.catMaybes <$> CM.mapM (findDependency cx fieldsm) vertexAdapters

              return $ elementTreeEdge (PG.Edge label id outId inId props) deps
            where
              findDependency cx' fieldsm (fname, ad) = case M.lookup fname fieldsm of
                  Nothing -> pure Nothing
                  Just fterm -> Just <$> coderEncode (adapterCoder ad) cx' fterm
          _ -> unexpectedE cx "record (1)" $ show term
        decode cx _ = err cx "edge decoding is not yet supported"
        getVertexId cx dir1 fieldsm adapter = if dir1 == dir
          then pure $ schemaDefaultVertexId schema
          else case adapter of
            Nothing -> err cx $ "no adapter for " ++ show dir1 ++ " with " ++ show dir
            Just ad -> selectVertexId cx fieldsm ad

edgeIdAdapter :: Context -> Graph -> Schema Graph t v -> t -> Name -> Name -> [FieldType] -> Result (Y.Maybe (IdAdapter t v))
edgeIdAdapter cx g schema eidType name idKey fields = do
  mIdSpec <- findIdProjectionSpec cx False name idKey fields
  case mIdSpec of
    Nothing -> pure Nothing
    Just idSpec -> Just <$> projectionAdapter cx g eidType (schemaEdgeIds schema) idSpec "id"

-- | Constructs an element adapter for a given type, interpreting it either as a vertex specification or an edge specification
--   The adapter maps terms of the given type to property graph element trees (rooted at a vertex or an edge)
elementCoder :: (Show t, Show v)
  => Y.Maybe (PG.Direction, PG.VertexLabel) -- For edge specs, their direction and parent vertex label
  -> Schema Graph t v
  -> Type
  -> t -> t
  -> Context -> Graph
  -> Result (ElementAdapter t v)
elementCoder mparent schema source vidType eidType cx g = case deannotateType source of
    TypeMaybe ot -> elementCoder mparent schema ot vidType eidType cx g

    TypeRecord (RowType name fields) -> do

      -- Construct out- and in-vertex specs (only in edge specs)
      mOutSpec <- findProjectionSpec cx g name (outVertexKey schema) (outVertexLabelKey schema) fields
      mInSpec <- findProjectionSpec cx g name (inVertexKey schema) (inVertexLabelKey schema) fields

      -- Decide whether this is a vertex spec or edge spec
      let kind = if hasVertexAdapters dir mOutSpec mInSpec then PG.ElementKindEdge else PG.ElementKindVertex

      -- Construct property adapters
      propAdapters <- findPropertySpecs cx g schema kind fields >>= CM.mapM (propertyAdapter cx g schema)

      -- Construct either a vertex adapter or an edge adapter
      case kind of
        PG.ElementKindVertex -> constructVertexCoder cx g schema source vidType eidType name fields propAdapters
        PG.ElementKindEdge -> constructEdgeCoder cx g parentLabel schema source vidType eidType dir name fields propAdapters mOutSpec mInSpec

    _ -> unexpectedE cx "record type" $ show source
  where
    dir = Y.maybe PG.DirectionBoth fst mparent
    parentLabel = Y.maybe (PG.VertexLabel "NOLABEL") snd mparent

elementTreeEdge :: PG.Edge v -> [PG.ElementTree v] -> PG.ElementTree v
elementTreeEdge edge = PG.ElementTree (PG.ElementEdge edge)

elementTreeVertex :: PG.Vertex v -> [PG.ElementTree v] -> PG.ElementTree v
elementTreeVertex vertex = PG.ElementTree (PG.ElementVertex vertex)

elementTypeTreeEdge :: PG.EdgeType t -> [PG.ElementTypeTree t] -> PG.ElementTypeTree t
elementTypeTreeEdge etype = PG.ElementTypeTree (PG.ElementTypeEdge etype)

elementTypeTreeVertex :: PG.VertexType t -> [PG.ElementTypeTree t] -> PG.ElementTypeTree t
elementTypeTreeVertex vtype = PG.ElementTypeTree (PG.ElementTypeVertex vtype)

encodeProperties :: Context -> M.Map Name Term -> [PropertyAdapter t v] -> Result (M.Map PG.PropertyKey v)
encodeProperties cx fields adapters = do
  props <- Y.catMaybes <$> CM.mapM (encodeProperty cx fields) adapters
  return $ M.fromList $ fmap (\(PG.Property key val) -> (key, val)) props

encodeProperty :: Context -> M.Map Name Term -> PropertyAdapter t v -> Result (Maybe (PG.Property v))
encodeProperty cx fields adapter = do
  case M.lookup fname fields of
    Nothing -> case ftyp of
      TypeMaybe _ -> pure Nothing
      _ -> err cx $ "expected field not found in record: " ++ unName fname
    Just value -> case ftyp of
      TypeMaybe _ -> case deannotateTerm value of
        TermMaybe ov -> case ov of
          Nothing -> pure Nothing
          Just v -> Just <$> encodeValue v
        _ -> unexpectedE cx "optional term" $ show value
      _ -> Just <$> encodeValue value
  where
    fname = fieldTypeName $ adapterSource adapter
    ftyp = deannotateType (fieldTypeType $ adapterSource adapter)
    encodeValue v = coderEncode (adapterCoder adapter) cx (Field fname v)

findAdjacenEdgeAdapters :: (Show t, Show v)
  => Context -> Graph -> Schema Graph t v -> t -> t -> PG.VertexLabel -> PG.Direction -> [FieldType] -> Result [AdjacentEdgeAdapter a t v]
findAdjacenEdgeAdapters cx g schema vidType eidType parentLabel dir fields = Y.catMaybes <$> CM.mapM toSpec fields
  where
    toSpec field = case getTypeAnnotation key (fieldTypeType field) of
      Nothing -> pure Nothing
      Just a -> do
        label <- PG.EdgeLabel <$> extractString cx g a
        elad <- elementCoder (Just (dir, parentLabel)) schema (fieldTypeType field) vidType eidType cx g
        return $ Just $ AdjacentEdgeAdapter dir field label elad
    key = case dir of
      PG.DirectionOut -> outEdgeLabelKey schema
      PG.DirectionIn -> inEdgeLabelKey schema

findIdProjectionSpec :: Context -> Bool -> Name -> Name -> [FieldType] -> Result (Y.Maybe (ProjectionSpec a))
findIdProjectionSpec cx required tname idKey fields = do
  mid <- findSingleFieldWithAnnotationKey cx tname idKey fields
  case mid of
    Nothing -> if required
      then err cx $ "no " ++ unName idKey ++ " field"
      else pure Nothing
    Just mi -> do
      spec <- case getTypeAnnotation idKey (fieldTypeType mi) of
        Nothing -> pure ValueSpecValue
        Just t -> decodeValueSpec cx emptyGraph t
      return $ Just $ ProjectionSpec mi spec Nothing

findIncidentVertexAdapter :: (Show t, Show v) => Context -> Graph -> Schema Graph t v -> t -> t -> ProjectionSpec v -> Result (Name, ElementAdapter t v)
findIncidentVertexAdapter cx g schema vidType eidType spec = do
    adapter <- elementCoder Nothing schema (fieldTypeType field) vidType eidType cx g
    return (fieldTypeName field, adapter)
  where
    field = projectionSpecField spec

findLabelString :: Context -> Graph -> Type -> Name -> Name -> Result String
findLabelString cx g source tname labelKey = case getTypeAnnotation labelKey source of
  Nothing -> pure $ unName tname
  Just labelTerm -> extractString cx g labelTerm

findProjectionSpec :: Context -> Graph -> Name -> Name -> Name -> [FieldType] -> Result (Y.Maybe (ProjectionSpec a))
findProjectionSpec cx g tname key aliasKey fields = do
  mfield <- findSingleFieldWithAnnotationKey cx tname key fields
  case mfield of
    Nothing -> pure Nothing
    Just field -> do
      spec <- decodeValueSpec cx g $ Y.fromJust $ getTypeAnnotation key $ fieldTypeType field
      alias <- case getTypeAnnotation aliasKey $ fieldTypeType field of
        Nothing -> pure Nothing
        Just t -> Just <$> extractString cx g t
      return $ Just $ ProjectionSpec field spec alias

findPropertySpecs :: Context -> Graph -> Schema Graph t v -> PG.ElementKind -> [FieldType] -> Result [ProjectionSpec a]
findPropertySpecs cx g schema kind fields = CM.mapM toSpec $ L.filter isPropField fields
  where
    isPropField field = not (hasSpecialAnnotation || hasSpecialFieldName)
      where
        hasSpecialAnnotation = L.foldl (\b k -> b || hasAnnotation k) False ((ignoreKey schema):specialKeys)
        hasSpecialFieldName = L.foldl (\b n -> b || hasName n) False specialKeys
        specialKeys = case kind of
          PG.ElementKindVertex -> [vertexIdKey schema, outEdgeLabelKey schema, inEdgeLabelKey schema]
          PG.ElementKindEdge -> [edgeIdKey schema, outVertexKey schema, inVertexKey schema]
        hasAnnotation key = Y.isJust $ getTypeAnnotation key $ fieldTypeType field
        hasName fname = fieldTypeName field == fname
    toSpec field = do
      alias <- case (getTypeAnnotation (propertyKeyKey schema) $ fieldTypeType field) of
        Nothing -> pure Nothing
        Just a -> Just <$> extractString cx g a
      values <- case (getTypeAnnotation (propertyValueKey schema) $ fieldTypeType field) of
        Nothing -> pure ValueSpecValue
        Just sp -> decodeValueSpec cx g sp
      return $ ProjectionSpec field values alias

findSingleFieldWithAnnotationKey :: Context -> Name -> Name -> [FieldType] -> Result (Y.Maybe FieldType)
findSingleFieldWithAnnotationKey cx tname key fields = do
  let matches = L.filter (\f -> Y.isJust $ getTypeAnnotation key $ fieldTypeType f) fields
  if L.length matches > 1
    then err cx $ "Multiple fields marked as '" ++ unName key ++ "' in record type " ++ unName tname ++ ": "
      ++ (L.intercalate ", " (unName . fieldTypeName <$> matches))
    else return $ if L.null matches then Nothing else Just $ L.head matches

hasVertexAdapters :: PG.Direction -> Y.Maybe (ProjectionSpec a) -> Y.Maybe (ProjectionSpec a) -> Bool
hasVertexAdapters dir mOutSpec mInSpec = case dir of
  PG.DirectionOut -> Y.isJust mInSpec
  PG.DirectionIn -> Y.isJust mOutSpec
  PG.DirectionBoth -> Y.isJust mOutSpec && Y.isJust mInSpec

-- TODO; infer lossiness
lossy = True

projectionAdapter :: Context -> Graph -> t
  -> Coder Term v
  -> ProjectionSpec a
  -> String
  -> Result (IdAdapter t v)
projectionAdapter cx g idtype coder spec key = do
    traversal <- parseValueSpec cx g $ projectionSpecValues spec
    let field = projectionSpecField spec
    let encode cx' typ = do
          t <- traverseToSingleTerm cx' (key ++ "-projection") (traversal cx') typ
          coderEncode coder cx' t
    return (fieldTypeName field, Adapter lossy (fieldTypeType field) idtype $ Coder encode (\cx' _ -> err cx' $ "edge '" ++ key ++ "' decoding is not yet supported"))

propertyAdapter :: Context -> Graph -> Schema Graph t v -> ProjectionSpec a -> Result (PropertyAdapter t v)
propertyAdapter cx g schema (ProjectionSpec tfield values alias) = do
  let key = PG.PropertyKey $ case alias of
        Nothing -> unName $ fieldTypeName tfield
        Just k -> k
  pt <- coderEncode (schemaPropertyTypes schema) cx $ fieldTypeType tfield
  traversal <- parseValueSpec cx g values
  let coder = Coder encode decode
        where
          encode cx' dfield =
            if fieldName dfield /= fieldTypeName tfield
              then unexpectedE cx' ("field '" ++ unName (fieldTypeName tfield) ++ "'") $ show dfield
              else do
                result <- traverseToSingleTerm cx' "property traversal" (traversal cx') $ fieldTerm dfield
                value <- coderEncode (schemaPropertyValues schema) cx' result
                return $ PG.Property key value
          decode cx' _ = err cx' "property decoding is not yet supported"
  return $ Adapter lossy tfield (PG.PropertyType key pt True) coder

propertyTypes propAdapters = toPropertyType <$> propAdapters
  where
    toPropertyType a = PG.PropertyType (PG.propertyTypeKey $ adapterTarget a) (PG.propertyTypeValue $ adapterTarget a) True

selectEdgeId :: Context -> M.Map Name Term -> IdAdapter t v -> Result v
selectEdgeId cx fields (fname, ad) = case M.lookup fname fields of
  Nothing -> err cx $ "no " ++ unName fname ++ " in record"
  Just t -> coderEncode (adapterCoder ad) cx t

selectVertexId :: Context -> M.Map Name Term -> IdAdapter t v -> Result v
selectVertexId cx fields (fname, ad) = case M.lookup fname fields of
  Nothing -> err cx $ "no " ++ unName fname ++ " in record"
  Just t -> coderEncode (adapterCoder ad) cx t

traverseToSingleTerm :: Context -> String -> (Term -> Result [Term]) -> Term -> Result Term
traverseToSingleTerm cx desc traversal term = do
  terms <- traversal term
  case terms of
    [] -> err cx $ desc ++ " did not resolve to a term"
    [t] -> pure t
    _ -> err cx $ desc ++ " resolved to multiple terms"

vertexCoder :: (Show t, Show v)
  => Graph
  -> Schema Graph t v
  -> Type
  -> t
  -> Name
  -> PG.VertexLabel -> IdAdapter t v -> [PropertyAdapter t v]
  -> [AdjacentEdgeAdapter a t v]
  -> ElementAdapter t v
vertexCoder g schema source vidType tname vlabel idAdapter propAdapters edgeAdapters = Adapter lossy source target coder
  where
    target = elementTypeTreeVertex vtype depTypes
    vtype = PG.VertexType vlabel vidType $ propertyTypes propAdapters
    depTypes = adapterTarget . adjacentEdgeAdapterAdapter <$> edgeAdapters
    coder = Coder encode decode
      where
        encode cx term = case deannotateTerm term of
            TermMaybe (Just ot) -> encode cx ot
            TermRecord (Record tname' fields) -> do
              checkRecordName cx tname tname'
              let fieldsm = fieldMap fields
              vid <- selectVertexId cx fieldsm idAdapter
              props <- encodeProperties cx (fieldMap fields) propAdapters
              deps <- L.concat <$> CM.mapM (findDependency vid fieldsm) edgeAdapters
              return $ elementTreeVertex (PG.Vertex vlabel vid props) deps
            _ -> unexpectedE cx "record (2)" $ show term
          where
            findDependency vid fieldsm (AdjacentEdgeAdapter dir field elabel ad) = do
                case M.lookup (fieldTypeName field) fieldsm of
                  Nothing -> pure []
                  Just fterm -> fixTree <$> coderEncode (adapterCoder ad) cx fterm
              where
                fixTree tree = case PG.elementTreeSelf tree of
                  -- If the child element is a vertex, we also need an edge
                  PG.ElementVertex v -> do
                      [PG.ElementTree edge [tree]]
                    where
                      edgeid = schemaDefaultEdgeId schema
                      edge = PG.ElementEdge $ PG.Edge elabel edgeid (proj True) (proj False) M.empty
                      otherid = PG.vertexId v
                      proj out = case dir of
                        PG.DirectionOut -> if out then vid else otherid
                        PG.DirectionIn -> if out then otherid else vid

                  -- If the child element is an edge, populate the in/out vertex
                  PG.ElementEdge e -> [tree {PG.elementTreeSelf = PG.ElementEdge $ fixEdge e}]
                fixEdge e = case dir of
                  PG.DirectionOut -> e {PG.edgeOut = vid}
                  PG.DirectionIn -> e {PG.edgeIn = vid}
        decode cx _ = err cx "vertex decoding is not yet supported"

vertexIdAdapter :: Context -> Graph -> Schema Graph t v -> t -> Name -> Name -> [FieldType] -> Result (IdAdapter t v)
vertexIdAdapter cx g schema vidType name idKey fields = do
  idSpec <- Y.fromJust <$> findIdProjectionSpec cx True name idKey fields
  idAdapter <- projectionAdapter cx g vidType (schemaVertexIds schema) idSpec "id"
  return idAdapter
