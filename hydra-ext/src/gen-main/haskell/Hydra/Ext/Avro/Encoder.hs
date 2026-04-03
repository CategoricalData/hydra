-- Note: this is an automatically generated file. Do not edit.

-- | Hydra-to-Avro encoder: converts Hydra types and terms to Avro schemas and JSON values

module Hydra.Ext.Avro.Encoder where

import qualified Hydra.Coders as Coders
import qualified Hydra.Context as Context
import qualified Hydra.Core as Core
import qualified Hydra.Errors as Errors
import qualified Hydra.Ext.Avro.Environment as Environment
import qualified Hydra.Ext.Org.Apache.Avro.Schema as Schema
import qualified Hydra.Extract.Core as Core_
import qualified Hydra.Graph as Graph
import qualified Hydra.Json.Model as Model
import qualified Hydra.Lib.Eithers as Eithers
import qualified Hydra.Lib.Equality as Equality
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Literals as Literals
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Math as Math
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Pairs as Pairs
import qualified Hydra.Lib.Sets as Sets
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Strip as Strip
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Map as M

-- | Build an Avro field from a name-adapter pair
buildAvroField :: (Core.Name, (Coders.Adapter t0 Schema.Schema t1 t2)) -> Schema.Field
buildAvroField nameAd =

      let name_ = Pairs.first nameAd
          ad = Pairs.second nameAd
      in Schema.Field {
        Schema.fieldName = (localName name_),
        Schema.fieldDoc = Nothing,
        Schema.fieldType = (Coders.adapterTarget ad),
        Schema.fieldDefault = Nothing,
        Schema.fieldOrder = Nothing,
        Schema.fieldAliases = Nothing,
        Schema.fieldAnnotations = Maps.empty}

-- | Create an empty encode environment with the given type map
emptyEncodeEnvironment :: M.Map Core.Name Core.Type -> Environment.EncodeEnvironment
emptyEncodeEnvironment typeMap =
    Environment.EncodeEnvironment {
      Environment.encodeEnvironmentTypeMap = typeMap,
      Environment.encodeEnvironmentEmitted = Maps.empty}

-- | Encode a Hydra type to an Avro schema adapter, given the type map and a root name
encodeType :: Context.Context -> M.Map Core.Name Core.Type -> Core.Name -> Either (Context.InContext Errors.Error) (Coders.Adapter Core.Type Schema.Schema Core.Term Model.Value)
encodeType cx typeMap name_ =
    Eithers.map (\adEnv -> Pairs.first adEnv) (encodeTypeWithEnv cx name_ (emptyEncodeEnvironment typeMap))

-- | Core encoding logic: recursively encode a Hydra type to an Avro schema
encodeTypeInner :: Context.Context -> Maybe Core.Name -> Core.Type -> Environment.EncodeEnvironment -> Either (Context.InContext Errors.Error) (Coders.Adapter Core.Type Schema.Schema Core.Term Model.Value, Environment.EncodeEnvironment)
encodeTypeInner cx mName typ env =

      let annResult = extractAnnotations typ
          annotations = Pairs.first annResult
          bareType = Pairs.second annResult
          simpleAdapter =
                  \target -> \lossy -> \encode -> \decode -> Right (Coders.Adapter {
                    Coders.adapterIsLossy = lossy,
                    Coders.adapterSource = typ,
                    Coders.adapterTarget = target,
                    Coders.adapterCoder = Coders.Coder {
                      Coders.coderEncode = encode,
                      Coders.coderDecode = decode}}, env)
      in case bareType of
        Core.TypeUnit -> simpleAdapter (Schema.SchemaPrimitive Schema.PrimitiveNull) False (\_cx -> \_t -> Right Model.ValueNull) (\_cx -> \_j -> Right Core.TermUnit)
        Core.TypeLiteral v0 -> Eithers.map (\ad -> (ad, env)) (literalAdapter cx typ v0)
        Core.TypeList v0 -> Eithers.bind (encodeTypeInner cx Nothing v0 env) (\adEnv ->
          let innerAd = Pairs.first adEnv
              env1 = Pairs.second adEnv
          in (Right (Coders.Adapter {
            Coders.adapterIsLossy = (Coders.adapterIsLossy innerAd),
            Coders.adapterSource = typ,
            Coders.adapterTarget = (Schema.SchemaArray (Schema.Array {
              Schema.arrayItems = (Coders.adapterTarget innerAd)})),
            Coders.adapterCoder = Coders.Coder {
              Coders.coderEncode = (\cx1 -> \t -> case t of
                Core.TermList v1 -> Eithers.map (\jvs -> Model.ValueArray jvs) (Eithers.mapList (\el -> Coders.coderEncode (Coders.adapterCoder innerAd) cx1 el) v1)),
              Coders.coderDecode = (\cx1 -> \j -> case j of
                Model.ValueArray v1 -> Eithers.map (\ts -> Core.TermList ts) (Eithers.mapList (\el -> Coders.coderDecode (Coders.adapterCoder innerAd) cx1 el) v1))}}, env1)))
        Core.TypeMap v0 ->
          let keyType = Core.mapTypeKeys v0
              valType = Core.mapTypeValues v0
          in case (Strip.deannotateType keyType) of
            Core.TypeLiteral v1 -> case v1 of
              Core.LiteralTypeString -> Eithers.bind (encodeTypeInner cx Nothing valType env) (\adEnv ->
                let valAd = Pairs.first adEnv
                    env1 = Pairs.second adEnv
                in (Right (Coders.Adapter {
                  Coders.adapterIsLossy = (Coders.adapterIsLossy valAd),
                  Coders.adapterSource = typ,
                  Coders.adapterTarget = (Schema.SchemaMap (Schema.Map {
                    Schema.mapValues = (Coders.adapterTarget valAd)})),
                  Coders.adapterCoder = Coders.Coder {
                    Coders.coderEncode = (\cx1 -> \t -> case t of
                      Core.TermMap v3 ->
                        let encodeEntry =
                                \entry ->
                                  let k = Pairs.first entry
                                      v = Pairs.second entry
                                  in (Eithers.bind (Core_.string cx (Graph.Graph {
                                    Graph.graphBoundTerms = Maps.empty,
                                    Graph.graphBoundTypes = Maps.empty,
                                    Graph.graphClassConstraints = Maps.empty,
                                    Graph.graphLambdaVariables = Sets.empty,
                                    Graph.graphMetadata = Maps.empty,
                                    Graph.graphPrimitives = Maps.empty,
                                    Graph.graphSchemaTypes = Maps.empty,
                                    Graph.graphTypeVariables = Sets.empty}) k) (\kStr -> Eithers.map (\vJson -> (kStr, vJson)) (Coders.coderEncode (Coders.adapterCoder valAd) cx1 v)))
                        in (Eithers.map (\pairs -> Model.ValueObject (Maps.fromList pairs)) (Eithers.mapList encodeEntry (Maps.toList v3)))),
                    Coders.coderDecode = (\cx1 -> \j -> case j of
                      Model.ValueObject v3 ->
                        let decodeEntry =
                                \entry ->
                                  let k = Pairs.first entry
                                      v = Pairs.second entry
                                  in (Eithers.map (\vTerm -> (Core.TermLiteral (Core.LiteralString k), vTerm)) (Coders.coderDecode (Coders.adapterCoder valAd) cx1 v))
                        in (Eithers.map (\pairs -> Core.TermMap (Maps.fromList pairs)) (Eithers.mapList decodeEntry (Maps.toList v3))))}}, env1)))
              _ -> err cx "Avro maps require string keys"
            _ -> err cx "Avro maps require string keys"
        Core.TypeRecord v0 -> namedTypeAdapter cx typ mName annotations v0 env (\avroFields -> Schema.NamedTypeRecord (Schema.Record {
          Schema.recordFields = avroFields})) recordTermCoder
        Core.TypeUnion v0 ->
          let allUnit =
                  Lists.foldl (\b -> \ft -> Logic.and b (case (Core.fieldTypeType ft) of
                    Core.TypeUnit -> True
                    _ -> False)) True v0
          in (Logic.ifElse allUnit (enumAdapter cx typ mName annotations v0 env) (unionAsRecordAdapter cx typ mName annotations v0 env))
        Core.TypeMaybe v0 -> Eithers.bind (encodeTypeInner cx Nothing v0 env) (\adEnv ->
          let innerAd = Pairs.first adEnv
              env1 = Pairs.second adEnv
          in (Right (Coders.Adapter {
            Coders.adapterIsLossy = (Coders.adapterIsLossy innerAd),
            Coders.adapterSource = typ,
            Coders.adapterTarget = (Schema.SchemaUnion (Schema.Union [
              Schema.SchemaPrimitive Schema.PrimitiveNull,
              (Coders.adapterTarget innerAd)])),
            Coders.adapterCoder = Coders.Coder {
              Coders.coderEncode = (\cx1 -> \t -> case t of
                Core.TermMaybe v1 -> Maybes.maybe (Right Model.ValueNull) (\inner -> Coders.coderEncode (Coders.adapterCoder innerAd) cx1 inner) v1),
              Coders.coderDecode = (\cx1 -> \j -> case j of
                Model.ValueNull -> Right (Core.TermMaybe Nothing)
                _ -> Eithers.map (\t -> Core.TermMaybe (Just t)) (Coders.coderDecode (Coders.adapterCoder innerAd) cx1 j))}}, env1)))
        Core.TypeWrap v0 -> encodeTypeInner cx mName v0 env
        Core.TypeVariable v0 -> Maybes.maybe (Maybes.maybe (err cx (Strings.cat2 "referenced type not found: " (Core.unName v0))) (\refType -> encodeTypeInner cx (Just v0) refType env) (Maps.lookup v0 (Environment.encodeEnvironmentTypeMap env))) (\existingAd -> Right (Coders.Adapter {
          Coders.adapterIsLossy = (Coders.adapterIsLossy existingAd),
          Coders.adapterSource = (Coders.adapterSource existingAd),
          Coders.adapterTarget = (Schema.SchemaReference (localName v0)),
          Coders.adapterCoder = (Coders.adapterCoder existingAd)}, env)) (Maps.lookup v0 (Environment.encodeEnvironmentEmitted env))
        _ -> err cx "unsupported Hydra type for Avro encoding"

-- | Encode with full environment threading. Returns the adapter and updated environment
encodeTypeWithEnv :: Context.Context -> Core.Name -> Environment.EncodeEnvironment -> Either (Context.InContext Errors.Error) (Coders.Adapter Core.Type Schema.Schema Core.Term Model.Value, Environment.EncodeEnvironment)
encodeTypeWithEnv cx name_ env =
    Maybes.maybe (err cx (Strings.cat2 "type not found in type map: " (Literals.showString (Core.unName name_)))) (\typ -> encodeTypeInner cx (Just name_) typ env) (Maps.lookup name_ (Environment.encodeEnvironmentTypeMap env))

-- | Adapter for all-unit union types (enums)
enumAdapter :: t0 -> Core.Type -> Maybe Core.Name -> M.Map Core.Name Core.Term -> [Core.FieldType] -> Environment.EncodeEnvironment -> Either t1 (Coders.Adapter Core.Type Schema.Schema Core.Term Model.Value, Environment.EncodeEnvironment)
enumAdapter cx typ mName annotations fieldTypes env0 =

      let symbols = Lists.map (\ft -> localName (Core.fieldTypeName ft)) fieldTypes
          typeName = Maybes.fromMaybe (typeToName typ) mName
          avroAnnotations = hydraAnnotationsToAvro annotations
          avroSchema =
                  Schema.SchemaNamed (Schema.Named {
                    Schema.namedName = (localName typeName),
                    Schema.namedNamespace = (nameNamespace typeName),
                    Schema.namedAliases = Nothing,
                    Schema.namedDoc = Nothing,
                    Schema.namedType = (Schema.NamedTypeEnum (Schema.Enum {
                      Schema.enumSymbols = symbols,
                      Schema.enumDefault = Nothing})),
                    Schema.namedAnnotations = avroAnnotations})
          adapter_ =
                  Coders.Adapter {
                    Coders.adapterIsLossy = False,
                    Coders.adapterSource = typ,
                    Coders.adapterTarget = avroSchema,
                    Coders.adapterCoder = Coders.Coder {
                      Coders.coderEncode = (\cx1 -> \t -> case t of
                        Core.TermUnion v0 ->
                          let fname = Core.injectionField v0
                          in (Right (Model.ValueString (localName (Core.fieldName fname))))
                        _ -> err cx1 "expected union term for enum"),
                      Coders.coderDecode = (\_cx -> \j -> case j of
                        Model.ValueString v0 -> Right (Core.TermUnion (Core.Injection {
                          Core.injectionTypeName = typeName,
                          Core.injectionField = Core.Field {
                            Core.fieldName = (Core.Name v0),
                            Core.fieldTerm = Core.TermUnit}})))}}
          env1 =
                  Environment.EncodeEnvironment {
                    Environment.encodeEnvironmentTypeMap = (Environment.encodeEnvironmentTypeMap env0),
                    Environment.encodeEnvironmentEmitted = (Maps.insert typeName adapter_ (Environment.encodeEnvironmentEmitted env0))}
      in (Right (adapter_, env1))

-- | Construct an error result with a message in context
err :: Context.Context -> String -> Either (Context.InContext Errors.Error) t0
err cx msg =
    Left (Context.InContext {
      Context.inContextObject = (Errors.ErrorOther (Errors.OtherError msg)),
      Context.inContextContext = cx})

-- | Extract annotations from a potentially annotated type
extractAnnotations :: Core.Type -> (M.Map Core.Name Core.Term, Core.Type)
extractAnnotations typ =
    case typ of
      Core.TypeAnnotated v0 ->
        let inner = Core.annotatedTypeBody v0
            anns = Core.annotatedTypeAnnotation v0
            innerResult = extractAnnotations inner
            innerAnns = Pairs.first innerResult
            bareType = Pairs.second innerResult
        in (Maps.union anns innerAnns, bareType)
      _ -> (Maps.empty, typ)

-- | Create an adapter for float types
floatAdapter :: Context.Context -> t0 -> Core.FloatType -> Either t1 (Coders.Adapter t0 Schema.Schema Core.Term Model.Value)
floatAdapter cx typ ft =

      let simple =
              \target -> \lossy -> \encode -> \decode -> Right (Coders.Adapter {
                Coders.adapterIsLossy = lossy,
                Coders.adapterSource = typ,
                Coders.adapterTarget = target,
                Coders.adapterCoder = Coders.Coder {
                  Coders.coderEncode = encode,
                  Coders.coderDecode = decode}})
      in case ft of
        Core.FloatTypeFloat32 -> simple (Schema.SchemaPrimitive Schema.PrimitiveFloat) False (\_cx -> \t -> Eithers.map (\f -> Model.ValueNumber (Literals.float32ToBigfloat f)) (Core_.float32 cx (Graph.Graph {
          Graph.graphBoundTerms = Maps.empty,
          Graph.graphBoundTypes = Maps.empty,
          Graph.graphClassConstraints = Maps.empty,
          Graph.graphLambdaVariables = Sets.empty,
          Graph.graphMetadata = Maps.empty,
          Graph.graphPrimitives = Maps.empty,
          Graph.graphSchemaTypes = Maps.empty,
          Graph.graphTypeVariables = Sets.empty}) t)) (\_cx -> \j -> case j of
          Model.ValueNumber v1 -> Right (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat32 (Literals.bigfloatToFloat32 v1)))))
        Core.FloatTypeFloat64 -> simple (Schema.SchemaPrimitive Schema.PrimitiveDouble) False (\_cx -> \t -> Eithers.map (\d -> Model.ValueNumber (Literals.float64ToBigfloat d)) (Core_.float64 cx (Graph.Graph {
          Graph.graphBoundTerms = Maps.empty,
          Graph.graphBoundTypes = Maps.empty,
          Graph.graphClassConstraints = Maps.empty,
          Graph.graphLambdaVariables = Sets.empty,
          Graph.graphMetadata = Maps.empty,
          Graph.graphPrimitives = Maps.empty,
          Graph.graphSchemaTypes = Maps.empty,
          Graph.graphTypeVariables = Sets.empty}) t)) (\_cx -> \j -> case j of
          Model.ValueNumber v1 -> Right (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 (Literals.bigfloatToFloat64 v1)))))
        _ -> simple (Schema.SchemaPrimitive Schema.PrimitiveDouble) True (\_cx -> \t -> case t of
          Core.TermLiteral v0 -> case v0 of
            Core.LiteralFloat v1 -> Right (Model.ValueNumber (floatValueToDouble v1))) (\_cx -> \j -> case j of
          Model.ValueNumber v0 -> Right (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 (Literals.bigfloatToFloat64 v0)))))

-- | Convert any float value to a double (bigfloat)
floatValueToDouble :: Core.FloatValue -> Double
floatValueToDouble fv =
    case fv of
      Core.FloatValueBigfloat v0 -> v0
      Core.FloatValueFloat32 v0 -> Literals.float32ToBigfloat v0
      Core.FloatValueFloat64 v0 -> Literals.float64ToBigfloat v0

-- | Fold over field types, building adapters and threading the environment
foldFieldAdapters :: Context.Context -> [Core.FieldType] -> Environment.EncodeEnvironment -> Either (Context.InContext Errors.Error) ([(Core.Name, (Coders.Adapter Core.Type Schema.Schema Core.Term Model.Value))], Environment.EncodeEnvironment)
foldFieldAdapters cx fieldTypes env0 =
    Lists.foldl (\acc -> \ft -> Eithers.bind acc (\accPair ->
      let soFar = Pairs.first accPair
          env1 = Pairs.second accPair
          fname = Core.fieldTypeName ft
          ftype = Core.fieldTypeType ft
      in (Eithers.bind (encodeTypeInner cx Nothing ftype env1) (\adEnv ->
        let ad = Pairs.first adEnv
            env2 = Pairs.second adEnv
        in (Right (Lists.concat2 soFar [
          (fname, ad)], env2)))))) (Right ([], env0)) fieldTypes

-- | Convert Hydra annotations to Avro annotation map
hydraAnnotationsToAvro :: M.Map Core.Name Core.Term -> M.Map String Model.Value
hydraAnnotationsToAvro anns =
    Maps.fromList (Lists.map (\entry ->
      let k = Pairs.first entry
          v = Pairs.second entry
      in (Core.unName k, (termToJsonValue v))) (Maps.toList anns))

-- | Encode a single type without a type map (for simple/anonymous types)
hydraAvroAdapter :: Context.Context -> M.Map Core.Name Core.Type -> Core.Type -> Either (Context.InContext Errors.Error) (Coders.Adapter Core.Type Schema.Schema Core.Term Model.Value)
hydraAvroAdapter cx typeMap typ =
    Eithers.map (\adEnv -> Pairs.first adEnv) (encodeTypeInner cx Nothing typ (emptyEncodeEnvironment typeMap))

-- | Convert a Hydra Name to an Avro qualified name (local name, optional namespace)
hydraNameToAvroName :: Core.Name -> (String, (Maybe String))
hydraNameToAvroName name_ = (localName name_, (nameNamespace name_))

-- | Create an adapter for integer types
integerAdapter :: Context.Context -> t0 -> Core.IntegerType -> Either t1 (Coders.Adapter t0 Schema.Schema Core.Term Model.Value)
integerAdapter cx typ it =

      let simple =
              \target -> \lossy -> \encode -> \decode -> Right (Coders.Adapter {
                Coders.adapterIsLossy = lossy,
                Coders.adapterSource = typ,
                Coders.adapterTarget = target,
                Coders.adapterCoder = Coders.Coder {
                  Coders.coderEncode = encode,
                  Coders.coderDecode = decode}})
      in case it of
        Core.IntegerTypeInt32 -> simple (Schema.SchemaPrimitive Schema.PrimitiveInt) False (\_cx -> \t -> Eithers.map (\i -> Model.ValueNumber (Literals.bigintToBigfloat (Literals.int32ToBigint i))) (Core_.int32 cx (Graph.Graph {
          Graph.graphBoundTerms = Maps.empty,
          Graph.graphBoundTypes = Maps.empty,
          Graph.graphClassConstraints = Maps.empty,
          Graph.graphLambdaVariables = Sets.empty,
          Graph.graphMetadata = Maps.empty,
          Graph.graphPrimitives = Maps.empty,
          Graph.graphSchemaTypes = Maps.empty,
          Graph.graphTypeVariables = Sets.empty}) t)) (\_cx -> \j -> case j of
          Model.ValueNumber v1 -> Right (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 (Literals.bigintToInt32 (Math.truncate (Literals.bigfloatToFloat64 v1)))))))
        Core.IntegerTypeInt64 -> simple (Schema.SchemaPrimitive Schema.PrimitiveLong) False (\_cx -> \t -> Eithers.map (\i -> Model.ValueNumber (Literals.bigintToBigfloat (Literals.int64ToBigint i))) (Core_.int64 cx (Graph.Graph {
          Graph.graphBoundTerms = Maps.empty,
          Graph.graphBoundTypes = Maps.empty,
          Graph.graphClassConstraints = Maps.empty,
          Graph.graphLambdaVariables = Sets.empty,
          Graph.graphMetadata = Maps.empty,
          Graph.graphPrimitives = Maps.empty,
          Graph.graphSchemaTypes = Maps.empty,
          Graph.graphTypeVariables = Sets.empty}) t)) (\_cx -> \j -> case j of
          Model.ValueNumber v1 -> Right (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt64 (Literals.bigintToInt64 (Math.truncate (Literals.bigfloatToFloat64 v1)))))))
        _ -> simple (Schema.SchemaPrimitive Schema.PrimitiveLong) True (\_cx -> \t -> case t of
          Core.TermLiteral v0 -> case v0 of
            Core.LiteralInteger v1 -> Right (Model.ValueNumber (integerValueToDouble v1))) (\_cx -> \j -> case j of
          Model.ValueNumber v0 -> Right (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt64 (Literals.bigintToInt64 (Math.truncate (Literals.bigfloatToFloat64 v0)))))))

-- | Convert any integer value to a double (bigfloat)
integerValueToDouble :: Core.IntegerValue -> Double
integerValueToDouble iv =
    case iv of
      Core.IntegerValueBigint v0 -> Literals.bigintToBigfloat v0
      Core.IntegerValueInt8 v0 -> Literals.bigintToBigfloat (Literals.int8ToBigint v0)
      Core.IntegerValueInt16 v0 -> Literals.bigintToBigfloat (Literals.int16ToBigint v0)
      Core.IntegerValueInt32 v0 -> Literals.bigintToBigfloat (Literals.int32ToBigint v0)
      Core.IntegerValueInt64 v0 -> Literals.bigintToBigfloat (Literals.int64ToBigint v0)
      Core.IntegerValueUint8 v0 -> Literals.bigintToBigfloat (Literals.uint8ToBigint v0)
      Core.IntegerValueUint16 v0 -> Literals.bigintToBigfloat (Literals.uint16ToBigint v0)
      Core.IntegerValueUint32 v0 -> Literals.bigintToBigfloat (Literals.uint32ToBigint v0)
      Core.IntegerValueUint64 v0 -> Literals.bigintToBigfloat (Literals.uint64ToBigint v0)

-- | Create an adapter for literal types
literalAdapter :: Context.Context -> t0 -> Core.LiteralType -> Either t1 (Coders.Adapter t0 Schema.Schema Core.Term Model.Value)
literalAdapter cx typ lt =

      let simple =
              \target -> \lossy -> \encode -> \decode -> Right (Coders.Adapter {
                Coders.adapterIsLossy = lossy,
                Coders.adapterSource = typ,
                Coders.adapterTarget = target,
                Coders.adapterCoder = Coders.Coder {
                  Coders.coderEncode = encode,
                  Coders.coderDecode = decode}})
      in case lt of
        Core.LiteralTypeBoolean -> simple (Schema.SchemaPrimitive Schema.PrimitiveBoolean) False (\_cx -> \t -> case t of
          Core.TermLiteral v1 -> case v1 of
            Core.LiteralBoolean v2 -> Right (Model.ValueBoolean v2)) (\_cx -> \j -> case j of
          Model.ValueBoolean v1 -> Right (Core.TermLiteral (Core.LiteralBoolean v1)))
        Core.LiteralTypeString -> simple (Schema.SchemaPrimitive Schema.PrimitiveString) False (\_cx -> \t -> case t of
          Core.TermLiteral v1 -> case v1 of
            Core.LiteralString v2 -> Right (Model.ValueString v2)) (\_cx -> \j -> case j of
          Model.ValueString v1 -> Right (Core.TermLiteral (Core.LiteralString v1)))
        Core.LiteralTypeBinary -> simple (Schema.SchemaPrimitive Schema.PrimitiveBytes) False (\_cx -> \t -> case t of
          Core.TermLiteral v1 -> case v1 of
            Core.LiteralBinary v2 -> Right (Model.ValueString (Literals.binaryToString v2))) (\_cx -> \j -> case j of
          Model.ValueString v1 -> Right (Core.TermLiteral (Core.LiteralBinary (Literals.stringToBinary v1))))
        Core.LiteralTypeInteger v0 -> integerAdapter cx typ v0
        Core.LiteralTypeFloat v0 -> floatAdapter cx typ v0

-- | Extract the local part of a qualified name
localName :: Core.Name -> String
localName name_ =

      let s = Core.unName name_
          parts = Strings.splitOn "." s
      in (Lists.last parts)

-- | Extract the namespace from a qualified name, if any
nameNamespace :: Core.Name -> Maybe String
nameNamespace name_ =

      let s = Core.unName name_
          parts = Strings.splitOn "." s
      in (Logic.ifElse (Equality.equal (Lists.length parts) 1) Nothing (Just (Strings.intercalate "." (Lists.init parts))))

-- | Build a named type adapter (shared between record and union-as-record)
namedTypeAdapter :: Context.Context -> Core.Type -> Maybe Core.Name -> M.Map Core.Name Core.Term -> [Core.FieldType] -> Environment.EncodeEnvironment -> ([Schema.Field] -> Schema.NamedType) -> (Context.Context -> Core.Name -> [(Core.Name, (Coders.Adapter Core.Type Schema.Schema Core.Term Model.Value))] -> ((Context.Context -> Core.Term -> Either (Context.InContext Errors.Error) Model.Value), (Context.Context -> Model.Value -> Either (Context.InContext Errors.Error) Core.Term))) -> Either (Context.InContext Errors.Error) (Coders.Adapter Core.Type Schema.Schema Core.Term Model.Value, Environment.EncodeEnvironment)
namedTypeAdapter cx typ mName annotations fieldTypes env0 mkNamedType mkCoder =

      let typeName = Maybes.fromMaybe (typeToName typ) mName
      in (Maybes.maybe (Eithers.bind (foldFieldAdapters cx fieldTypes env0) (\faResult ->
        let fieldAdapters = Pairs.first faResult
            env1 = Pairs.second faResult
            avroFields = Lists.map buildAvroField fieldAdapters
            avroAnnotations = hydraAnnotationsToAvro annotations
            avroSchema =
                    Schema.SchemaNamed (Schema.Named {
                      Schema.namedName = (localName typeName),
                      Schema.namedNamespace = (nameNamespace typeName),
                      Schema.namedAliases = Nothing,
                      Schema.namedDoc = Nothing,
                      Schema.namedType = (mkNamedType avroFields),
                      Schema.namedAnnotations = avroAnnotations})
            lossy = Lists.foldl (\b -> \fa -> Logic.or b (Coders.adapterIsLossy (Pairs.second fa))) False fieldAdapters
            coderPair = mkCoder cx typeName fieldAdapters
            encodeFn = Pairs.first coderPair
            decodeFn = Pairs.second coderPair
            adapter_ =
                    Coders.Adapter {
                      Coders.adapterIsLossy = lossy,
                      Coders.adapterSource = typ,
                      Coders.adapterTarget = avroSchema,
                      Coders.adapterCoder = Coders.Coder {
                        Coders.coderEncode = encodeFn,
                        Coders.coderDecode = decodeFn}}
            env2 =
                    Environment.EncodeEnvironment {
                      Environment.encodeEnvironmentTypeMap = (Environment.encodeEnvironmentTypeMap env1),
                      Environment.encodeEnvironmentEmitted = (Maps.insert typeName adapter_ (Environment.encodeEnvironmentEmitted env1))}
        in (Right (adapter_, env2)))) (\existingAd -> Right (existingAd, env0)) (Maps.lookup typeName (Environment.encodeEnvironmentEmitted env0)))

-- | Build a record term coder from field adapters
recordTermCoder :: Context.Context -> Core.Name -> [(Core.Name, (Coders.Adapter t0 t1 Core.Term Model.Value))] -> ((Context.Context -> Core.Term -> Either (Context.InContext Errors.Error) Model.Value), (Context.Context -> Model.Value -> Either (Context.InContext Errors.Error) Core.Term))
recordTermCoder cx typeName fieldAdapters =

      let encode =
              \cx1 -> \term -> case term of
                Core.TermRecord v0 ->
                  let fields = Core.recordFields v0
                      fieldMap = Maps.fromList (Lists.map (\f -> (Core.fieldName f, (Core.fieldTerm f))) fields)
                      encodeField =
                              \nameAd ->
                                let fname = Pairs.first nameAd
                                    ad = Pairs.second nameAd
                                    fTerm = Maybes.fromMaybe Core.TermUnit (Maps.lookup fname fieldMap)
                                in (Eithers.map (\jv -> (localName fname, jv)) (Coders.coderEncode (Coders.adapterCoder ad) cx1 fTerm))
                  in (Eithers.map (\pairs -> Model.ValueObject (Maps.fromList pairs)) (Eithers.mapList encodeField fieldAdapters))
                _ -> err cx "expected record term"
          decode =
                  \cx1 -> \json -> case json of
                    Model.ValueObject v0 ->
                      let decodeField =
                              \nameAd ->
                                let fname = Pairs.first nameAd
                                    ad = Pairs.second nameAd
                                    jv = Maybes.fromMaybe Model.ValueNull (Maps.lookup (localName fname) v0)
                                in (Eithers.map (\t -> Core.Field {
                                  Core.fieldName = fname,
                                  Core.fieldTerm = t}) (Coders.coderDecode (Coders.adapterCoder ad) cx1 jv))
                      in (Eithers.map (\fields -> Core.TermRecord (Core.Record {
                        Core.recordTypeName = typeName,
                        Core.recordFields = fields})) (Eithers.mapList decodeField fieldAdapters))
                    _ -> err cx "expected JSON object"
      in (encode, decode)

-- | Convert a Hydra term to a JSON value (for annotation values)
termToJsonValue :: Core.Term -> Model.Value
termToJsonValue term =
    case term of
      Core.TermLiteral v0 -> case v0 of
        Core.LiteralString v1 -> Model.ValueString v1
        Core.LiteralBoolean v1 -> Model.ValueBoolean v1
        Core.LiteralInteger v1 -> Model.ValueNumber (integerValueToDouble v1)
        Core.LiteralFloat v1 -> Model.ValueNumber (floatValueToDouble v1)
        Core.LiteralBinary v1 -> Model.ValueString (Literals.binaryToString v1)
      Core.TermList v0 -> Model.ValueArray (Lists.map termToJsonValue v0)
      Core.TermMap v0 -> Model.ValueObject (Maps.fromList (Lists.map (\entry ->
        let k = Pairs.first entry
            v = Pairs.second entry
        in (case k of
          Core.TermLiteral v1 -> case v1 of
            Core.LiteralString v2 -> v2
            _ -> "<key>"
          _ -> "<key>", (termToJsonValue v))) (Maps.toList v0)))
      Core.TermRecord v0 -> Logic.ifElse (Lists.null (Core.recordFields v0)) Model.ValueNull (Model.ValueString "<record>")
      _ -> Model.ValueString "<term>"

-- | Generate a default name for an anonymous type
typeToName :: Core.Type -> Core.Name
typeToName t =
    case (Strip.deannotateType t) of
      Core.TypeRecord _ -> Core.Name "Record"
      Core.TypeUnion _ -> Core.Name "Union"
      _ -> Core.Name "Unknown"

-- | Adapter for general unions (encoded as records with optional fields)
unionAsRecordAdapter :: Context.Context -> Core.Type -> Maybe Core.Name -> M.Map Core.Name Core.Term -> [Core.FieldType] -> Environment.EncodeEnvironment -> Either (Context.InContext Errors.Error) (Coders.Adapter Core.Type Schema.Schema Core.Term Model.Value, Environment.EncodeEnvironment)
unionAsRecordAdapter cx typ mName annotations fieldTypes env0 =
    Eithers.bind (foldFieldAdapters cx fieldTypes env0) (\faResult ->
      let fieldAdapters = Pairs.first faResult
          env1 = Pairs.second faResult
          avroFields =
                  Lists.map (\nameAd ->
                    let fname = Pairs.first nameAd
                        ad = Pairs.second nameAd
                    in Schema.Field {
                      Schema.fieldName = (localName fname),
                      Schema.fieldDoc = Nothing,
                      Schema.fieldType = (Schema.SchemaUnion (Schema.Union [
                        Schema.SchemaPrimitive Schema.PrimitiveNull,
                        (Coders.adapterTarget ad)])),
                      Schema.fieldDefault = (Just Model.ValueNull),
                      Schema.fieldOrder = Nothing,
                      Schema.fieldAliases = Nothing,
                      Schema.fieldAnnotations = Maps.empty}) fieldAdapters
          typeName = Maybes.fromMaybe (typeToName typ) mName
          avroAnnotations = hydraAnnotationsToAvro annotations
          avroSchema =
                  Schema.SchemaNamed (Schema.Named {
                    Schema.namedName = (localName typeName),
                    Schema.namedNamespace = (nameNamespace typeName),
                    Schema.namedAliases = Nothing,
                    Schema.namedDoc = Nothing,
                    Schema.namedType = (Schema.NamedTypeRecord (Schema.Record {
                      Schema.recordFields = avroFields})),
                    Schema.namedAnnotations = avroAnnotations})
          adapter_ =
                  Coders.Adapter {
                    Coders.adapterIsLossy = True,
                    Coders.adapterSource = typ,
                    Coders.adapterTarget = avroSchema,
                    Coders.adapterCoder = Coders.Coder {
                      Coders.coderEncode = (\cx1 -> \t -> case t of
                        Core.TermUnion v0 ->
                          let activeName = Core.fieldName (Core.injectionField v0)
                              activeValue = Core.fieldTerm (Core.injectionField v0)
                              encodePair =
                                      \nameAd ->
                                        let fname = Pairs.first nameAd
                                            ad = Pairs.second nameAd
                                        in (Logic.ifElse (Equality.equal (Core.unName fname) (Core.unName activeName)) (Eithers.map (\jv -> (localName fname, jv)) (Coders.coderEncode (Coders.adapterCoder ad) cx1 activeValue)) (Right (localName fname, Model.ValueNull)))
                          in (Eithers.map (\pairs -> Model.ValueObject (Maps.fromList pairs)) (Eithers.mapList encodePair fieldAdapters))
                        _ -> err cx1 "expected union term"),
                      Coders.coderDecode = (\cx1 -> \j -> case j of
                        Model.ValueObject v0 ->
                          let findActive =
                                  \remaining -> Logic.ifElse (Lists.null remaining) (err cx1 "no non-null field in union record") (
                                    let head_ = Lists.head remaining
                                        rest_ = Lists.tail remaining
                                        fname = Pairs.first head_
                                        ad = Pairs.second head_
                                        mjv = Maps.lookup (localName fname) v0
                                    in (Maybes.maybe (findActive rest_) (\jv -> case jv of
                                      Model.ValueNull -> findActive rest_
                                      _ -> Eithers.map (\t -> Core.TermUnion (Core.Injection {
                                        Core.injectionTypeName = typeName,
                                        Core.injectionField = Core.Field {
                                          Core.fieldName = fname,
                                          Core.fieldTerm = t}})) (Coders.coderDecode (Coders.adapterCoder ad) cx1 jv)) mjv))
                          in (findActive fieldAdapters)
                        _ -> err cx1 "expected JSON object for union-as-record")}}
          env2 =
                  Environment.EncodeEnvironment {
                    Environment.encodeEnvironmentTypeMap = (Environment.encodeEnvironmentTypeMap env1),
                    Environment.encodeEnvironmentEmitted = (Maps.insert typeName adapter_ (Environment.encodeEnvironmentEmitted env1))}
      in (Right (adapter_, env2)))
