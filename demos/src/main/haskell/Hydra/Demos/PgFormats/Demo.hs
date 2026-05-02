-- | Driver for the pg-formats demo. Produces seven Hydra-generated artifacts
--   for the TinkerPop "Modern" property graph and the GraphSON wire format:
--
--     * modern_graph.graphson.jsonl  -- GraphSON 3.0, vertex-centric, JSON Lines
--     * Graph.schema.json            -- JSON Schema for hydra.pg.model.Graph
--     * GraphSchema.schema.json      -- JSON Schema for hydra.pg.model.GraphSchema
--     * GraphsonVertex.schema.json   -- JSON Schema for hydra.pg.graphson.syntax.Vertex
--                                        (one GraphSON document = one Vertex object)
--     * pg-model.proto               -- Protobuf v3 definitions for the whole
--                                        hydra.pg.model namespace (Graph, GraphSchema,
--                                        and their dependencies)
--     * Graph.avsc                   -- Avro schema for hydra.pg.model.Graph
--     * GraphSchema.avsc             -- Avro schema for hydra.pg.model.GraphSchema
--
-- The companion artifacts modern_graph.json and schema.json (the Hydra-PG JSON
-- form of the same graph and its schema) are produced by HydraPop's Java side
-- via net.fortytwo.hydra.hydrapop.GenerateExampleData and copied in from
-- HydraPop/src/gen-main/json/ by bin/generate.sh; they are not produced here.
--
-- See bin/generate.ghci for the GHCi entry point.

module Hydra.Demos.PgFormats.Demo (
  generatePgFormats,
  modernGraph,
  Instantiations,
  monomorphizeModuleStrict,
  monomorphizeModuleAllString,
) where

import qualified Hydra.Core as Core
import qualified Hydra.Pg.Model as Pg
import Hydra.Pg.Graphson.Utils (pgElementsToGraphson, encodeTermValue)
import Hydra.Pg.Utils (propertyGraphElements)
import qualified Hydra.Json.Writer as JsonWriter
import qualified Hydra.Show.Errors as ShowError
import qualified Hydra.Sources.Pg.Model as PgModelSource
import qualified Hydra.Sources.Pg.Graphson.Syntax as GraphsonSyntaxSource
import qualified Hydra.Sources.All as SourcesAll
import qualified Hydra.Packaging as Packaging
import qualified Hydra.Variables as Variables
import Hydra.ExtGeneration (writeJsonSchema, writeProtobuf)
import qualified Hydra.Avro.Encoder as AvroEncoder
import qualified Hydra.Avro.SchemaJson as AvroSchemaJson
import qualified Hydra.Coders as Coders
import qualified Hydra.Lexical as Lexical

import qualified Data.List as L
import qualified Data.Map as M
import System.Directory (createDirectoryIfMissing, doesFileExist, removeDirectoryRecursive)
import System.FilePath ((</>))
import System.IO (hFlush, stdout)


-- | Run the full pg-formats generation. Writes outputs under the given directory.
generatePgFormats :: FilePath -> IO ()
generatePgFormats outDir = do
  createDirectoryIfMissing True outDir
  log_ $ "Writing pg-formats artifacts to " ++ outDir

  -- 1. GraphSON 3.0
  let graphsonPath = outDir </> "modern_graph.graphson.jsonl"
  let elements = propertyGraphElements modernGraph
  jsonValues <- case pgElementsToGraphson encodeTermValue elements of
    Left ic -> fail $ "GraphSON encode failed: " ++ ShowError.error ic
    Right v -> return v
  writeFile graphsonPath (jsonValuesToString jsonValues)
  log_ $ "  wrote " ++ graphsonPath

  -- 2. JSON Schema documents for the hydra.pg.model module.
  --    writeJsonSchema emits one document per type definition under
  --    <staging>/hydra/pg/model/<TypeName>.json. We promote Graph and GraphSchema
  --    to flat names alongside the other artifacts.
  let jsStaging = outDir </> "_jsonschema-staging"
  _ <- writeJsonSchema jsStaging SourcesAll.kernelModules [PgModelSource.module_]
  promoteJsonSchema jsStaging outDir "hydra/pg/model/Graph.json"            "Graph.schema.json"
  promoteJsonSchema jsStaging outDir "hydra/pg/model/GraphSchema.json"      "GraphSchema.schema.json"

  -- 3. JSON Schema for the GraphSON document root: hydra.pg.graphson.syntax.Vertex.
  _ <- writeJsonSchema jsStaging SourcesAll.kernelModules [GraphsonSyntaxSource.module_]
  promoteJsonSchema jsStaging outDir "hydra/pg/graphson/syntax/Vertex.json" "GraphsonVertex.schema.json"
  removeDirectoryRecursive jsStaging

  -- 4. Protobuf v3 definitions for the hydra.pg.model namespace.
  --    writeProtobuf emits to <outDir>/<namespace-as-path>.proto, i.e.
  --    <outDir>/hydra/pg/model.proto. Lift it to the flat output dir as pg-model.proto.
  --
  -- Use the convenience all-string monomorphization for the demo. The
  -- correct design (monomorphizeModuleStrict, requiring per-binding
  -- instantiations) is also exported from this module; switch the demo to
  -- it once the proper kernel-side utility lands.
  let pgModelMonomorphic = monomorphizeModuleAllString PgModelSource.module_
  let protoStaging = outDir </> "_proto-staging"
  _ <- writeProtobuf protoStaging SourcesAll.kernelModules [pgModelMonomorphic]
  let protoSrc = protoStaging </> "hydra" </> "pg" </> "model.proto"
  let protoDst = outDir </> "pg-model.proto"
  protoExists <- doesFileExist protoSrc
  if protoExists
    then do
      contents <- readFile protoSrc
      length contents `seq` writeFile protoDst contents
      removeDirectoryRecursive protoStaging
      log_ $ "  wrote " ++ protoDst
    else fail $ "writeProtobuf did not produce expected file " ++ protoSrc

  -- 5. Avro schemas for hydra.pg.model.{Graph, GraphSchema}.
  --    The Avro encoder (Hydra.Avro.Encoder.encodeType) takes a typeMap
  --    M.Map Name Type plus a target Name and returns a HydraAvroAdapter
  --    whose adapterTarget is the Avro Schema. We use the same monomorphic
  --    module as the Protobuf path, since Avro likewise has no notion of
  --    type variables.
  --
  -- Two extra adjustments over the Protobuf input are needed:
  --   * Avro requires map keys to be a literal string. Pg.Model's property
  --     maps key on PropertyKey (a newtype around string), and the Avro
  --     encoder follows the type reference and finds a TypeWrap, not a
  --     TypeLiteral, and refuses it. So inline newtype-around-string
  --     bindings in the typeMap (PropertyKey, VertexLabel, EdgeLabel ->
  --     TypeLiteral string).
  let stringNewtypes = stringNewtypeNames pgModelMonomorphic
  let typeMap = M.fromList
        [ (Packaging.typeDefinitionName td,
           inlineRefs stringNewtypes
                      (Core.typeSchemeBody (Packaging.typeDefinitionTypeScheme td)))
        | Packaging.DefinitionType td <- Packaging.moduleDefinitions pgModelMonomorphic ]
  writeAvroSchema outDir typeMap "hydra.pg.model.Graph" "Graph.avsc"
  writeAvroSchema outDir typeMap "hydra.pg.model.GraphSchema" "GraphSchema.avsc"
  where
    log_ msg = putStrLn msg >> hFlush stdout
    jsonValuesToString vs = L.intercalate "\n" (fmap JsonWriter.printJson vs) ++ "\n"
    promoteJsonSchema staging dir relSrc fname = do
      let src = staging </> relSrc
      let dst = dir </> fname
      exists <- doesFileExist src
      if exists
        then do
          contents <- readFile src
          length contents `seq` writeFile dst contents
          putStrLn $ "  wrote " ++ dst
        else fail $ "writeJsonSchema did not produce expected file " ++ src
    writeAvroSchema dir tmap typeNameStr fname =
      let typeName = Core.Name typeNameStr
          cx = Lexical.emptyContext
      in case AvroEncoder.encodeType cx tmap typeName of
        Left err -> fail $ "Avro encodeType failed for " ++ typeNameStr
                            ++ ": " ++ ShowError.error err
        Right adapter -> do
          let avroSchema = Coders.adapterTarget adapter
          let jsonValue = AvroSchemaJson.encodeSchema avroSchema
          let p = dir </> fname
          writeFile p (JsonWriter.printJson jsonValue ++ "\n")
          putStrLn $ "  wrote " ++ p

--------------------------------------------------------------------------------
-- The TinkerPop Modern graph, encoded as Hydra.Pg.Model.Graph Term.
--
-- Vertex/edge ids are int32. Vertex labels are person/software. Edge labels
-- are knows/created. Property keys are name/age/lang/weight. The data is the
-- TinkerPop reference graph; it agrees with HydraPop/src/gen-main/json/modern_graph.json
-- and HydraPop/src/gen-main/json/schema.json.

modernGraph :: Pg.Graph Core.Term
modernGraph = Pg.Graph {
    Pg.graphVertices = M.fromList [(Pg.vertexId v, v) | v <- vertices],
    Pg.graphEdges    = M.fromList [(Pg.edgeId   e, e) | e <- edges]}
  where
    vertices =
      [ vert 1 "person"   [("name", strLit "marko"),  ("age", int32Lit 29)]
      , vert 2 "person"   [("name", strLit "vadas"),  ("age", int32Lit 27)]
      , vert 3 "software" [("name", strLit "lop"),    ("lang", strLit "java")]
      , vert 4 "person"   [("name", strLit "josh"),   ("age", int32Lit 32)]
      , vert 5 "software" [("name", strLit "ripple"), ("lang", strLit "java")]
      , vert 6 "person"   [("name", strLit "peter"),  ("age", int32Lit 35)]
      ]
    edges =
      [ edge 7  "knows"   1 2 [("weight", float32Lit 0.5)]
      , edge 8  "knows"   1 4 [("weight", float32Lit 1.0)]
      , edge 9  "created" 1 3 [("weight", float32Lit 0.4)]
      , edge 10 "created" 4 5 [("weight", float32Lit 1.0)]
      , edge 11 "created" 4 3 [("weight", float32Lit 0.4)]
      , edge 12 "created" 6 3 [("weight", float32Lit 0.2)]
      ]

vert :: Int -> String -> [(String, Core.Term)] -> Pg.Vertex Core.Term
vert i lbl props = Pg.Vertex {
    Pg.vertexLabel      = Pg.VertexLabel lbl,
    Pg.vertexId         = int32Lit i,
    Pg.vertexProperties = M.fromList [(Pg.PropertyKey k, v) | (k, v) <- props]}

edge :: Int -> String -> Int -> Int -> [(String, Core.Term)] -> Pg.Edge Core.Term
edge i lbl outV inV props = Pg.Edge {
    Pg.edgeLabel      = Pg.EdgeLabel lbl,
    Pg.edgeId         = int32Lit i,
    Pg.edgeOut        = int32Lit outV,
    Pg.edgeIn         = int32Lit inV,
    Pg.edgeProperties = M.fromList [(Pg.PropertyKey k, v) | (k, v) <- props]}

--------------------------------------------------------------------------------
-- Module monomorphization for the Protobuf path.
--
-- Most non-Hydra schema languages (Protobuf, PDL, Avro, JSON Schema) lack
-- a notion of type variables, so a polymorphic Hydra type like
--   Graph v = { vertices: Map<v, Vertex<v>>, edges: Map<v, Edge<v>> }
-- has to be instantiated to a concrete shape before emission. The right
-- design is for the *caller* to supply one Type per type variable, in
-- declaration order, for every polymorphic binding it wants emitted; a
-- missing instantiation is a hard error, not a silent default.
--
-- For convenience there's also a "default everything to string" variant
-- (monomorphizeModuleAllString), which is what the demo currently uses
-- to produce pg-model.proto. That is the same hack the Protobuf coder
-- itself does as a last-resort fallback (see line 345 of
-- dist/.../Hydra/Protobuf/Coder.hs); it wallpapers over real modeling
-- choices and should not be used in production code.
--
-- TODO: promote this into a proper hydra-ext utility (analogous to
-- Hydra.Adapt) that every non-generic coder can share, with a structured
-- error type rather than String. For the demo it lives here.

-- | For each polymorphic type definition in a module, a list of concrete
--   types to substitute, in the same order as the binding's type variables.
type Instantiations = M.Map Core.Name [Core.Type]

-- | Walk a module, replacing every TypeForall in every TypeDefinition's body
--   with the caller-supplied concrete types. Returns Left if a polymorphic
--   binding is missing from the map, or if the number of supplied types does
--   not match the number of type variables.
monomorphizeModuleStrict :: Instantiations -> Packaging.Module
                         -> Either String Packaging.Module
monomorphizeModuleStrict insts mod = do
    defs' <- mapM monoDef (Packaging.moduleDefinitions mod)
    return $ mod { Packaging.moduleDefinitions = defs' }
  where
    monoDef (Packaging.DefinitionType td) = do
      let name = Packaging.typeDefinitionName td
      ts' <- monoScheme name (Packaging.typeDefinitionTypeScheme td)
      return $ Packaging.DefinitionType $ td { Packaging.typeDefinitionTypeScheme = ts' }
    monoDef d = Right d
    monoScheme name (Core.TypeScheme _ body cs) = do
      body' <- monoType name body
      return $ Core.TypeScheme [] body' cs
    monoType name t = case t of
      Core.TypeAnnotated (Core.AnnotatedType inner anns) -> do
        inner' <- monoType name inner
        return $ Core.TypeAnnotated (Core.AnnotatedType inner' anns)
      _ -> case peelForalls t of
        ([],  _)    -> Right t
        (vars, body) -> case M.lookup name insts of
          Nothing -> Left $ "monomorphizeModule: no instantiation provided for "
                             ++ Core.unName name
                             ++ " (needs " ++ show (length vars) ++ " type argument(s): "
                             ++ L.intercalate ", " (map Core.unName vars) ++ ")"
          Just args
            | length args /= length vars ->
                Left $ "monomorphizeModule: " ++ Core.unName name
                        ++ " has " ++ show (length vars)
                        ++ " type variable(s) but " ++ show (length args)
                        ++ " instantiation(s) provided"
            | otherwise ->
                Right $ L.foldl' (\b (v, a) -> Variables.replaceFreeTypeVariable v a b)
                                 body
                                 (zip vars args)

-- | Convenience: instantiate every type variable to hydra.core.string AND
--   strip every TypeApplication down to its head. This is a hack -- it gives
--   the Protobuf coder a fully ground module to encode without forcing the
--   caller to spell out per-binding instantiations, but the resulting schema
--   flattens all id types, all property values, all parametric carriers to
--   "string", and replaces e.g. `PropertyType<t>` with bare `PropertyType`
--   (the same flattening Hydra.Protobuf.Coder.flattenType does internally
--   as a last-resort fallback). Use monomorphizeModuleStrict for any case
--   where the type choice actually matters.
monomorphizeModuleAllString :: Packaging.Module -> Packaging.Module
monomorphizeModuleAllString mod = case monomorphizeModuleStrict allString mod of
    Right m  -> m { Packaging.moduleDefinitions = map flattenDef (Packaging.moduleDefinitions m) }
    Left err -> error $ "monomorphizeModuleAllString: " ++ err
                        ++ " (this should never happen since allString covers every binding)"
  where
    allString = M.fromList
      [ (Packaging.typeDefinitionName td, replicate (numVars td) stringType)
      | Packaging.DefinitionType td <- Packaging.moduleDefinitions mod
      , let n = numVars td, n > 0 ]
    numVars td = length . fst . peelForalls . Core.typeSchemeBody
                       . Packaging.typeDefinitionTypeScheme $ td
    stringType = Core.TypeLiteral Core.LiteralTypeString
    flattenDef (Packaging.DefinitionType td) = Packaging.DefinitionType $
      td { Packaging.typeDefinitionTypeScheme = flattenScheme (Packaging.typeDefinitionTypeScheme td) }
    flattenDef d = d
    flattenScheme (Core.TypeScheme vs body cs) = Core.TypeScheme vs (flattenAppsAndForalls body) cs

-- | Eliminate residual TypeForall and TypeApplication wrappers anywhere in
--   the type. TypeForall body becomes its body (binders dropped); TypeApplication
--   becomes its function head (arguments dropped). Same shape as
--   Hydra.Protobuf.Coder.flattenType, lifted out so the demo can apply it
--   before handing the module to writeProtobuf.
--
-- Also unwraps TypeWrap (newtype) nodes when they appear as the KEY of a
-- TypeMap. The Avro encoder rejects non-literal-string keys, but newtypes
-- around `string` (like hydra.pg.model.PropertyKey) are semantically
-- equivalent for encoding purposes.
flattenAppsAndForalls :: Core.Type -> Core.Type
flattenAppsAndForalls = go
  where
    go t = case t of
      Core.TypeAnnotated (Core.AnnotatedType inner anns) ->
        Core.TypeAnnotated (Core.AnnotatedType (go inner) anns)
      Core.TypeForall (Core.ForallType v body) ->
        go (Variables.replaceFreeTypeVariable v (Core.TypeLiteral Core.LiteralTypeString) body)
      Core.TypeApplication (Core.ApplicationType f _) -> go f
      Core.TypeList l -> Core.TypeList (go l)
      Core.TypeSet s -> Core.TypeSet (go s)
      Core.TypeMaybe m -> Core.TypeMaybe (go m)
      Core.TypeMap (Core.MapType k v) ->
        Core.TypeMap (Core.MapType (go (unwrapToScalar k)) (go v))
      Core.TypePair (Core.PairType a b) ->
        Core.TypePair (Core.PairType (go a) (go b))
      Core.TypeEither (Core.EitherType a b) ->
        Core.TypeEither (Core.EitherType (go a) (go b))
      Core.TypeFunction (Core.FunctionType a b) ->
        Core.TypeFunction (Core.FunctionType (go a) (go b))
      Core.TypeRecord fields -> Core.TypeRecord (map goField fields)
      Core.TypeUnion fields  -> Core.TypeUnion  (map goField fields)
      Core.TypeWrap inner -> Core.TypeWrap (go inner)
      _ -> t
    goField (Core.FieldType n ft) = Core.FieldType n (go ft)
    -- Repeatedly peel TypeAnnotated and TypeWrap to expose the underlying
    -- scalar key type. Used only on map keys, where Avro requires a literal
    -- string and won't accept a newtype around it.
    unwrapToScalar t = case t of
      Core.TypeAnnotated (Core.AnnotatedType inner _) -> unwrapToScalar inner
      Core.TypeWrap inner -> unwrapToScalar inner
      _ -> t

-- | Peel off any leading TypeForall layers (looking through TypeAnnotated),
--   returning the bound variable names in declaration order and the inner
--   non-forall body.
peelForalls :: Core.Type -> ([Core.Name], Core.Type)
peelForalls t = case t of
  Core.TypeAnnotated (Core.AnnotatedType inner _) -> peelForalls inner
  Core.TypeForall (Core.ForallType v body) ->
    let (vs, b) = peelForalls body in (v : vs, b)
  _ -> ([], t)

-- | Find every type definition whose body (after peeling annotations) is a
--   TypeWrap around a TypeLiteral String. Pg.Model's PropertyKey, VertexLabel,
--   and EdgeLabel all match this pattern.
stringNewtypeNames :: Packaging.Module -> [Core.Name]
stringNewtypeNames mod =
  [ Packaging.typeDefinitionName td
  | Packaging.DefinitionType td <- Packaging.moduleDefinitions mod
  , isStringNewtype (Core.typeSchemeBody (Packaging.typeDefinitionTypeScheme td)) ]
  where
    isStringNewtype t = case t of
      Core.TypeAnnotated (Core.AnnotatedType inner _) -> isStringNewtype inner
      Core.TypeWrap inner -> case inner of
        Core.TypeLiteral Core.LiteralTypeString -> True
        Core.TypeAnnotated (Core.AnnotatedType (Core.TypeLiteral Core.LiteralTypeString) _) -> True
        _ -> False
      _ -> False

-- | Replace every TypeVariable reference whose name is in the given list
--   with TypeLiteral String. Used to inline string-newtype references at
--   the typeMap level for the Avro encoder.
inlineRefs :: [Core.Name] -> Core.Type -> Core.Type
inlineRefs names = go
  where
    nameSet = names
    go t = case t of
      Core.TypeAnnotated (Core.AnnotatedType inner anns) ->
        Core.TypeAnnotated (Core.AnnotatedType (go inner) anns)
      Core.TypeVariable n
        | n `elem` nameSet -> Core.TypeLiteral Core.LiteralTypeString
        | otherwise        -> t
      Core.TypeList l -> Core.TypeList (go l)
      Core.TypeSet s -> Core.TypeSet (go s)
      Core.TypeMaybe m -> Core.TypeMaybe (go m)
      Core.TypeMap (Core.MapType k v) ->
        Core.TypeMap (Core.MapType (go k) (go v))
      Core.TypePair (Core.PairType a b) ->
        Core.TypePair (Core.PairType (go a) (go b))
      Core.TypeEither (Core.EitherType a b) ->
        Core.TypeEither (Core.EitherType (go a) (go b))
      Core.TypeFunction (Core.FunctionType a b) ->
        Core.TypeFunction (Core.FunctionType (go a) (go b))
      Core.TypeRecord fs -> Core.TypeRecord (map goField fs)
      Core.TypeUnion fs  -> Core.TypeUnion  (map goField fs)
      Core.TypeWrap inner -> Core.TypeWrap (go inner)
      Core.TypeApplication (Core.ApplicationType f a) ->
        Core.TypeApplication (Core.ApplicationType (go f) (go a))
      Core.TypeForall (Core.ForallType v b) ->
        Core.TypeForall (Core.ForallType v (go b))
      _ -> t
    goField (Core.FieldType n ft) = Core.FieldType n (go ft)

--------------------------------------------------------------------------------

strLit :: String -> Core.Term
strLit = Core.TermLiteral . Core.LiteralString

int32Lit :: Int -> Core.Term
int32Lit i = Core.TermLiteral $ Core.LiteralInteger $ Core.IntegerValueInt32 i

float32Lit :: Float -> Core.Term
float32Lit f = Core.TermLiteral $ Core.LiteralFloat $ Core.FloatValueFloat32 f
