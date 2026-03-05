module Hydra.Ext.Staging.Avro.Coder (
  AvroEnvironment(..),
  AvroHydraAdapter(..),
  AvroQualifiedName(..),
  avroHydraAdapter,
  emptyAvroEnvironment,
) where

import Hydra.Kernel
import qualified Hydra.Lib.Literals as Literals
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Extract.Core as ExtractCore
import qualified Hydra.Dsl.Types as Types
import qualified Hydra.Dsl.Terms as Terms
import qualified Hydra.Ext.Org.Apache.Avro.Schema as Avro
import qualified Hydra.Json.Model as Json

import qualified Text.Read as TR
import qualified Control.Monad as CM
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Maybe as Y


type Result a = Either (InContext OtherError) a

err :: Context -> String -> Result a
err cx msg = Left (InContext (OtherError msg) cx)

unexpectedE :: Context -> String -> String -> Result a
unexpectedE cx expected found = err cx $ "Expected " ++ expected ++ ", found: " ++ found

-- | Either-based JSON extraction helpers

expectArrayE :: Context -> Json.Value -> Result [Json.Value]
expectArrayE cx value = case value of
  Json.ValueArray v -> Right v
  _ -> unexpectedE cx "JSON array" (show value)

expectObjectE :: Context -> Json.Value -> Result (M.Map String Json.Value)
expectObjectE cx value = case value of
  Json.ValueObject v -> Right v
  _ -> unexpectedE cx "JSON object" (show value)

expectStringE :: Context -> Json.Value -> Result String
expectStringE cx value = case value of
  Json.ValueString v -> Right v
  _ -> unexpectedE cx "JSON string" (show value)

requireStringE :: Context -> String -> M.Map String Json.Value -> Result String
requireStringE cx fname m = case M.lookup fname m of
  Nothing -> err cx $ "required attribute " ++ show fname ++ " not found"
  Just v -> expectStringE cx v

optStringE :: Context -> String -> M.Map String Json.Value -> Result (Maybe String)
optStringE cx fname m = case M.lookup fname m of
  Nothing -> Right Nothing
  Just v -> Just <$> expectStringE cx v


data AvroEnvironment = AvroEnvironment {
  avroEnvironmentNamedAdapters :: M.Map AvroQualifiedName AvroHydraAdapter,
  avroEnvironmentNamespace :: Maybe String,
  avroEnvironmentElements :: M.Map Name Binding} -- note: only used in the term coders
type AvroHydraAdapter = Adapter Avro.Schema Type Json.Value Term

data AvroQualifiedName = AvroQualifiedName (Maybe String) String deriving (Eq, Ord, Show)

data AvroForeignKey = AvroForeignKey Name (String -> Name)

data AvroPrimaryKey = AvroPrimaryKey Name (String -> Name)

emptyAvroEnvironment = AvroEnvironment M.empty Nothing M.empty

avro_foreignKey = "@foreignKey"
avro_primaryKey = "@primaryKey"

avroHydraAdapter :: Context -> Avro.Schema -> AvroEnvironment -> Result (AvroHydraAdapter, AvroEnvironment)
avroHydraAdapter cx schema env0 = case schema of
    Avro.SchemaArray (Avro.Array s) -> do
      (ad, env1) <- avroHydraAdapter cx s env0
      let coder = Coder {
            coderEncode = \cx (Json.ValueArray vals) -> Terms.list <$> (CM.mapM (coderEncode (adapterCoder ad) cx) vals),
            coderDecode = \cx (TermList vals) -> Json.ValueArray <$> (CM.mapM (coderDecode (adapterCoder ad) cx) vals)}
      return (Adapter (adapterIsLossy ad) schema (Types.list $ adapterTarget ad) coder, env1)
    Avro.SchemaMap (Avro.Map s) -> do
      (ad, env1) <- avroHydraAdapter cx s env0
      let pairToHydra cx (k, v) = do
            v' <- coderEncode (adapterCoder ad) cx v
            return (Terms.string k, v')
      let coder = Coder {
            coderEncode = \cx (Json.ValueObject m) -> Terms.map . M.fromList <$> (CM.mapM (pairToHydra cx) $ M.toList m),
            coderDecode = \cx m -> do
              mp <- ExtractCore.map cx (ExtractCore.string cx emptyGraph) (\t -> coderDecode (adapterCoder ad) cx t) emptyGraph m
              return $ Json.ValueObject mp}
      return (Adapter (adapterIsLossy ad) schema (Types.map Types.string $ adapterTarget ad) coder, env1)
    Avro.SchemaNamed n -> do
        let ns = Avro.namedNamespace n

        let manns = namedAnnotationsToCore n
        let ann = if M.null manns then Nothing else (Just manns)

        let lastNs = avroEnvironmentNamespace env0
        let nextNs = Y.maybe lastNs Just ns
        let env1 = env0 {avroEnvironmentNamespace = nextNs}

        let qname = AvroQualifiedName nextNs (Avro.namedName n)
        let hydraName = avroNameToHydraName qname

        -- Note: if a named type is redefined (an illegal state for which the Avro spec does not provide a resolution),
        --       we just take the first definition and ignore the second.
        (ad, env2) <- case getAvroHydraAdapter qname env1 of
          Just ad -> err cx $ "Avro named type defined more than once: " ++ show qname
          Nothing -> do
            (ad, env2) <- case Avro.namedType n of
              Avro.NamedTypeEnum (Avro.Enum syms mdefault) -> simpleAdapter env1 typ encode decode  -- TODO: use default value
                where
                  typ = TypeUnion (RowType hydraName $ toField <$> syms)
                    where
                      toField s = FieldType (Name s) Types.unit
                  encode _cx (Json.ValueString s) = Right $ TermUnion (Injection hydraName $ Field (Name s) Terms.unit)
                  -- Note: we simply trust that data coming from the Hydra side is correct
                  decode _cx (TermUnion (Injection _ (Field fn _))) = Right $ Json.ValueString $ unName fn
              Avro.NamedTypeFixed (Avro.Fixed size) -> simpleAdapter env1 Types.binary encode decode
                where
                  encode _cx (Json.ValueString s) = Right $ Terms.binary (Literals.stringToBinary s)
                  decode cx term = Json.ValueString . Literals.binaryToStringBS <$> ExtractCore.binary cx emptyGraph term
              Avro.NamedTypeRecord r -> do
                  let avroFields = Avro.recordFields r
                  (adaptersByFieldName, env2) <- prepareFields cx env1 avroFields
                  pk <- findAvroPrimaryKeyField cx qname avroFields
                  -- TODO: Nothing values for optional fields
                  let encodePair cx (k, v) = case M.lookup k adaptersByFieldName of
                        Nothing -> Left $ InContext (OtherError $ "unrecognized field for " ++ showQname qname ++ ": " ++ show k) cx
                        Just (f, ad) -> do
                          v' <- coderEncode (adapterCoder ad) cx v
                          return $ Field (Name k) v'
                  let decodeField cx (Field (Name k) v) = case M.lookup k adaptersByFieldName of
                        Nothing -> Left $ InContext (OtherError $ "unrecognized field for " ++ showQname qname ++ ": " ++ show k) cx
                        Just (f, ad) -> do
                          v' <- coderDecode (adapterCoder ad) cx v
                          return (k, v')
                  let lossy = L.foldl (\b (_, ad) -> b || adapterIsLossy ad) False $ M.elems adaptersByFieldName
                  let hfields = toHydraField <$> M.elems adaptersByFieldName
                  let target = TypeRecord $ RowType hydraName hfields
                  let coder = Coder {
                    -- Note: the order of the fields is changed
                    coderEncode = \cx (Json.ValueObject m) -> do
                      fields <- CM.mapM (encodePair cx) $ M.toList m
                      let term = TermRecord $ Record hydraName fields
                      -- TODO: addElement (primary key element collection) requires state; skipped in Either-based coder
                      return term,
                    coderDecode = \cx (TermRecord (Record _ fields)) -> Json.ValueObject . M.fromList <$> (CM.mapM (decodeField cx) fields)}
                  return (Adapter lossy schema target coder, env2)
                where
                  toHydraField (f, ad) = FieldType (Name $ Avro.fieldName f) $ adapterTarget ad
            let env3 = putAvroHydraAdapter qname ad env2
            return (annotate ann ad, env3)

        let env3 = env2 {avroEnvironmentNamespace = lastNs}
        return (ad, env3)
      where
        addElement term typ pk fields env = case pk of
          Nothing -> Right env
          Just (AvroPrimaryKey fname constr) -> case L.filter isPkField fields of
              [] -> Right env
              [field] -> do
                  s <- termToStringE cx $ fieldTerm field
                  let name = constr s
                  let el = Binding name term Nothing
                  return $ env {avroEnvironmentElements = M.insert name el (avroEnvironmentElements env)}
              _ -> err cx $ "multiple fields named " ++ unName fname
            where
              isPkField field = fieldName field == fname
    Avro.SchemaPrimitive p -> case p of
        Avro.PrimitiveNull -> simpleAdapter env0 Types.unit encode decode
          where
            encode _cx (Json.ValueString s) = Right $ Terms.string s
            decode cx term = Json.ValueString <$> ExtractCore.string cx emptyGraph term
        Avro.PrimitiveBoolean -> simpleAdapter env0 Types.boolean encode decode
          where
            encode _cx (Json.ValueBoolean b) = Right $ Terms.boolean b
            decode cx term = Json.ValueBoolean <$> ExtractCore.boolean cx emptyGraph term
        Avro.PrimitiveInt -> simpleAdapter env0 Types.int32 encode decode
          where
            encode _cx (Json.ValueNumber d) = Right $ Terms.int32 $ doubleToInt d
            decode cx term = Json.ValueNumber . fromIntegral <$> ExtractCore.int32 cx emptyGraph term
        Avro.PrimitiveLong -> simpleAdapter env0 Types.int64 encode decode
          where
            encode _cx (Json.ValueNumber d) = Right $ Terms.int64 $ doubleToInt d
            decode cx term = Json.ValueNumber . fromIntegral <$> ExtractCore.int64 cx emptyGraph term
        Avro.PrimitiveFloat -> simpleAdapter env0 Types.float32 encode decode
          where
            encode _cx (Json.ValueNumber d) = Right $ Terms.float32 $ realToFrac d
            decode cx term = Json.ValueNumber . realToFrac <$> ExtractCore.float32 cx emptyGraph term
        Avro.PrimitiveDouble -> simpleAdapter env0 Types.float64 encode decode
          where
            encode _cx (Json.ValueNumber d) = Right $ Terms.float64 d
            decode cx term = Json.ValueNumber <$> ExtractCore.float64 cx emptyGraph term
        Avro.PrimitiveBytes -> simpleAdapter env0 Types.binary encode decode
          where
            encode _cx (Json.ValueString s) = Right $ Terms.binary (Literals.stringToBinary s)
            decode cx term = Json.ValueString . Literals.binaryToStringBS <$> ExtractCore.binary cx emptyGraph term
        Avro.PrimitiveString -> simpleAdapter env0 Types.string encode decode
          where
            encode _cx (Json.ValueString s) = Right $ Terms.string s
            decode cx term = Json.ValueString <$> ExtractCore.string cx emptyGraph term
      where
        doubleToInt d = if d < 0 then ceiling d else floor d
    Avro.SchemaReference name -> do
      let qname = parseAvroName (avroEnvironmentNamespace env0) name
      case getAvroHydraAdapter qname env0 of
        Nothing -> err cx $ "Referenced Avro type has not been defined: " ++ show qname
         ++ ". Defined types: " ++ show (M.keys $ avroEnvironmentNamedAdapters env0)
        Just ad -> Right (ad, env0)
    Avro.SchemaUnion (Avro.Union schemas) -> if L.length nonNulls > 1
        then err cx $ "general-purpose unions are not yet supported: " ++ show schema
        else if L.null nonNulls
        then err cx $ "cannot generate the empty type"
        else if hasNull
        then forOptional $ L.head nonNulls
        else do
          (ad, env1) <- avroHydraAdapter cx (L.head nonNulls) env0
          return (Adapter (adapterIsLossy ad) schema (adapterTarget ad) (adapterCoder ad), env1)
      where
        hasNull = (not . L.null . L.filter isNull) schemas
        nonNulls = L.filter (not . isNull) schemas
        isNull schema = case schema of
          Avro.SchemaPrimitive Avro.PrimitiveNull -> True
          _ -> False
        forOptional s = do
          (ad, env1) <- avroHydraAdapter cx s env0
          let coder = Coder {
                coderDecode = \cx (TermMaybe ot) -> case ot of
                  Nothing -> Right Json.ValueNull
                  Just term -> coderDecode (adapterCoder ad) cx term,
                coderEncode = \cx v -> case v of
                  Json.ValueNull -> Right $ TermMaybe Nothing
                  _ -> TermMaybe . Just <$> coderEncode (adapterCoder ad) cx v}
          return (Adapter (adapterIsLossy ad) schema (Types.optional $ adapterTarget ad) coder, env1)
  where
    simpleAdapter env typ encode decode = Right (Adapter False schema typ $ Coder encode decode, env)
    annotate ann ad = case ann of
      Nothing -> ad
      Just n -> ad {adapterTarget = Types.annot n (adapterTarget ad)}

-- | Thread AvroEnvironment through preparing multiple fields
prepareFields :: Context -> AvroEnvironment -> [Avro.Field] -> Result (M.Map String (Avro.Field, AvroHydraAdapter), AvroEnvironment)
prepareFields cx env [] = Right (M.empty, env)
prepareFields cx env (f:fs) = do
  (pair, env1) <- prepareField cx env f
  (rest, env2) <- prepareFields cx env1 fs
  return (M.insert (fst pair) (snd pair) rest, env2)

prepareField :: Context -> AvroEnvironment -> Avro.Field -> Result ((String, (Avro.Field, AvroHydraAdapter)), AvroEnvironment)
prepareField cx env f = do
  fk <- foreignKeyE cx f

  let manns = fieldAnnotationsToCore f
  let ann = if M.null manns then Nothing else (Just manns) :: Y.Maybe (M.Map Name Term)

  (ad, env1) <- case fk of
    Nothing -> avroHydraAdapter cx (Avro.fieldType f) env
    Just (AvroForeignKey name constr) -> do
        (ad, env1) <- avroHydraAdapter cx (Avro.fieldType f) env
        let decodeTerm = \cx (TermVariable name) -> do -- TODO: not symmetrical
              term <- stringToTermE cx (adapterTarget ad) $ unName name
              coderDecode (adapterCoder ad) cx term
        let encodeValue cx v = do
              s <- coderEncode (adapterCoder ad) cx v >>= termToStringE cx
              return $ TermVariable $ constr s
        -- Support three special cases of foreign key types: plain, optional, and list
        case deannotateType (adapterTarget ad) of
          TypeMaybe (TypeLiteral lit) -> forTypeAndCoder env1 ad (Types.optional elTyp) coder
            where
              coder = Coder
                (\cx json -> (TermMaybe . Just) <$> encodeValue cx json)
                decodeTerm
          TypeList (TypeLiteral lit) -> forTypeAndCoder env1 ad (Types.list elTyp) coder
            where
              coder = Coder
                (\cx json -> case json of
                  Json.ValueArray vals -> TermList <$> CM.mapM (encodeValue cx) vals
                  _ -> err cx $ "Expected JSON array")
                decodeTerm
          TypeLiteral lit -> forTypeAndCoder env1 ad elTyp coder
            where
              coder = Coder (\cx v -> encodeValue cx v) decodeTerm
          _ -> err cx $ "unsupported type annotated as foreign key: " ++ (show $ typeVariant $ adapterTarget ad)
      where
        forTypeAndCoder env ad typ coder = Right (Adapter (adapterIsLossy ad) (Avro.fieldType f) typ coder, env)
        elTyp = TypeVariable name
  return ((Avro.fieldName f, (f, annotateAdapter ann ad)), env1)

annotateAdapter :: Maybe (M.Map Name Term) -> AvroHydraAdapter -> AvroHydraAdapter
annotateAdapter ann ad = case ann of
  Nothing -> ad
  Just n -> ad {adapterTarget = Types.annot n (adapterTarget ad)}

findAvroPrimaryKeyField :: Context -> AvroQualifiedName -> [Avro.Field] -> Result (Maybe AvroPrimaryKey)
findAvroPrimaryKeyField cx qname avroFields = do
  let keys = Y.catMaybes $ fmap (primaryKeyE cx) avroFields
  case keys of
    [] -> Right Nothing
    [k] -> Right $ Just k
    _ -> err cx $ "multiple primary key fields for " ++ show qname

avroNameToHydraName :: AvroQualifiedName -> Name
avroNameToHydraName (AvroQualifiedName mns local) = unqualifyName $ QualifiedName (Namespace <$> mns) local

-- TODO: use me (for encoding annotations for which the type is not know) or lose me
--       A more robust solution would use jsonCoder together with an expected type
encodeAnnotationValue :: Json.Value -> Term
encodeAnnotationValue v = case v of
  Json.ValueArray vals -> Terms.list (encodeAnnotationValue <$> vals)
  Json.ValueBoolean b -> Terms.boolean b
  Json.ValueNull -> Terms.tuple []
  Json.ValueNumber d -> Terms.float64 d
  -- Note: JSON objects are untyped maps, not records, in that fields are unordered
  Json.ValueObject m -> Terms.map $ M.fromList (toEntry <$> M.toList m)
    where
      toEntry (k, v) = (Terms.string k, encodeAnnotationValue v)
  Json.ValueString s -> Terms.string s

fieldAnnotationsToCore :: Avro.Field -> M.Map Name Term
fieldAnnotationsToCore f = M.fromList (toCore <$> (M.toList $ Avro.fieldAnnotations f))
  where
    toCore (k, v) = (Name k, encodeAnnotationValue v)

namedAnnotationsToCore :: Avro.Named -> M.Map Name Term
namedAnnotationsToCore n = M.fromList (toCore <$> (M.toList $ Avro.namedAnnotations n))
  where
    toCore (k, v) = (Name k, encodeAnnotationValue v)

getAvroHydraAdapter :: AvroQualifiedName -> AvroEnvironment -> Y.Maybe AvroHydraAdapter
getAvroHydraAdapter qname = M.lookup qname . avroEnvironmentNamedAdapters

foreignKeyE :: Context -> Avro.Field -> Result (Maybe AvroForeignKey)
foreignKeyE cx f = case M.lookup avro_foreignKey (Avro.fieldAnnotations f) of
    Nothing -> Right Nothing
    Just v -> do
      m <- expectObjectE cx v
      tname <- Name <$> requireStringE cx "type" m
      pattern <- optStringE cx "pattern" m
      let constr = case pattern of
            Nothing -> Name
            Just pat -> patternToNameConstructor pat
      return $ Just $ AvroForeignKey tname constr

patternToNameConstructor :: String -> String -> Name
patternToNameConstructor pat = \s -> Name $ L.intercalate s $ Strings.splitOn "${}" pat

primaryKeyE :: Context -> Avro.Field -> Maybe AvroPrimaryKey
primaryKeyE cx f = case M.lookup avro_primaryKey $ Avro.fieldAnnotations f of
    Nothing -> Nothing
    Just v -> case expectStringE cx v of
      Left _ -> Nothing
      Right s -> Just $ AvroPrimaryKey (Name $ Avro.fieldName f) $ patternToNameConstructor s

parseAvroName :: Maybe String -> String -> AvroQualifiedName
parseAvroName mns name = case L.reverse $ Strings.splitOn "." name of
  [local] -> AvroQualifiedName mns local
  (local:rest) -> AvroQualifiedName (Just $ L.intercalate "." $ L.reverse rest) local

putAvroHydraAdapter :: AvroQualifiedName -> AvroHydraAdapter -> AvroEnvironment -> AvroEnvironment
putAvroHydraAdapter qname ad env = env {avroEnvironmentNamedAdapters = M.insert qname ad $ avroEnvironmentNamedAdapters env}

rewriteAvroSchemaM :: ((Avro.Schema -> Result Avro.Schema) -> Avro.Schema -> Result Avro.Schema) -> Avro.Schema -> Result Avro.Schema
rewriteAvroSchemaM f = recurse
  where
    recurse = f (fsub recurse) -- TODO: restore global Rewriting.rewrite/fix instead of the local definition
    fsub recurse schema = case schema of
        Avro.SchemaArray (Avro.Array els) -> Avro.SchemaArray <$> (Avro.Array <$> recurse els)
        Avro.SchemaMap (Avro.Map vschema) -> Avro.SchemaMap <$> (Avro.Map <$> recurse vschema)
        Avro.SchemaNamed n -> do
          nt <- case Avro.namedType n of
            Avro.NamedTypeRecord (Avro.Record fields) -> Avro.NamedTypeRecord <$> (Avro.Record <$> (CM.mapM forField fields))
            t -> Right t
          return $ Avro.SchemaNamed $ n {Avro.namedType = nt}
        Avro.SchemaUnion (Avro.Union schemas) -> Avro.SchemaUnion <$> (Avro.Union <$> (CM.mapM recurse schemas))
        _ -> Right schema
      where
        forField f = do
          t <- recurse $ Avro.fieldType f
          return f {Avro.fieldType = t}

jsonToStringE :: Context -> Json.Value -> Result String
jsonToStringE cx v = case v of
  Json.ValueBoolean b -> Right $ if b then "true" else "false"
  Json.ValueString s -> Right s
  Json.ValueNumber d -> Right $ if fromIntegral (round d) == d
    then show (round d)
    else show d
  _ -> unexpectedE cx "string, number, or boolean" $ show v

showQname :: AvroQualifiedName -> String
showQname (AvroQualifiedName mns local) = (Y.maybe "" (\ns -> ns ++ ".") mns) ++ local

stringToTermE :: Context -> Type -> String -> Result Term
stringToTermE cx typ s = case deannotateType typ of
    TypeLiteral lt -> TermLiteral <$> case lt of
      LiteralTypeBoolean -> LiteralBoolean <$> doRead s
      LiteralTypeInteger it -> LiteralInteger <$> case it of
        IntegerTypeBigint -> IntegerValueBigint <$> doRead s
        IntegerTypeInt8 -> IntegerValueInt8 <$> doRead s
        IntegerTypeInt16 -> IntegerValueInt16 <$> doRead s
        IntegerTypeInt32 -> IntegerValueInt32 <$> doRead s
        IntegerTypeInt64 -> IntegerValueInt64 <$> doRead s
        IntegerTypeUint8 -> IntegerValueUint8 <$> doRead s
        IntegerTypeUint16 -> IntegerValueUint16 <$> doRead s
        IntegerTypeUint32 -> IntegerValueUint32 <$> doRead s
        IntegerTypeUint64 -> IntegerValueUint64 <$> doRead s
      LiteralTypeString -> LiteralString <$> Right s
      _ -> unexpectedE cx "literal type" $ show lt
    _ -> unexpectedE cx "literal type" $ show (typeVariant typ)
  where
    doRead s = case TR.readEither s of
      Left msg -> err cx $ "failed to read value: " ++ msg
      Right term -> Right term

termToStringE :: Context -> Term -> Result String
termToStringE cx term = case deannotateTerm term of
  TermLiteral l -> case l of
    LiteralBoolean b -> Right $ show b
    LiteralInteger iv -> Right $ case iv of
      IntegerValueBigint i -> show i
      IntegerValueInt8 i -> show i
      IntegerValueInt16 i -> show i
      IntegerValueInt32 i -> show i
      IntegerValueInt64 i -> show i
      IntegerValueUint8 i -> show i
      IntegerValueUint16 i -> show i
      IntegerValueUint32 i -> show i
      IntegerValueUint64 i -> show i
    LiteralString s -> Right s
    _ -> unexpectedE cx "boolean, integer, or string" $ show l
  TermMaybe (Just term') -> termToStringE cx term'
  _ -> unexpectedE cx "literal value" $ show term
