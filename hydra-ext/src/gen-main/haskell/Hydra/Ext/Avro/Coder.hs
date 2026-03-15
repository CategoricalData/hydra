-- Note: this is an automatically generated file. Do not edit.

-- | Avro-to-Hydra adapter for converting Avro schemas and data to Hydra types and terms

module Hydra.Ext.Avro.Coder where

import qualified Hydra.Context as Context
import qualified Hydra.Core as Core
import qualified Hydra.Error as Error
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
import qualified Hydra.Module as Module
import qualified Hydra.Names as Names
import qualified Hydra.Rewriting as Rewriting
import qualified Hydra.Util as Util
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

avro_foreignKey :: String
avro_foreignKey = "@foreignKey"

avro_primaryKey :: String
avro_primaryKey = "@primaryKey"

-- | An empty Avro environment with no named adapters, no namespace, and no elements
emptyAvroEnvironment :: Environment.AvroEnvironment
emptyAvroEnvironment = Environment.AvroEnvironment {
  Environment.avroEnvironmentNamedAdapters = Maps.empty,
  Environment.avroEnvironmentNamespace = Nothing,
  Environment.avroEnvironmentElements = Maps.empty}

-- | Create an adapter between Avro schemas and Hydra types/terms
avroHydraAdapter :: (Context.Context -> Schema.Schema -> Environment.AvroEnvironment -> Either (Context.InContext Error.Error) (Util.Adapter Schema.Schema Core.Type Model.Value Core.Term, Environment.AvroEnvironment))
avroHydraAdapter cx schema env0 =  
  let simpleAdapter = (\env -> \typ -> \encode -> \decode -> Right (Util.Adapter {
          Util.adapterIsLossy = False,
          Util.adapterSource = schema,
          Util.adapterTarget = typ,
          Util.adapterCoder = Util.Coder {
            Util.coderEncode = encode,
            Util.coderDecode = decode}}, env)) 
      doubleToInt = (\d -> Literals.bigintToInt32 (Math.truncate (Literals.bigfloatToFloat64 d)))
      doubleToLong = (\d -> Literals.bigintToInt64 (Math.truncate (Literals.bigfloatToFloat64 d)))
  in ((\x -> case x of
    Schema.SchemaArray v0 -> (Eithers.bind (avroHydraAdapter cx (Schema.arrayItems v0) env0) (\adEnv ->  
      let ad = (Pairs.first adEnv) 
          env1 = (Pairs.second adEnv)
      in (Right (Util.Adapter {
        Util.adapterIsLossy = (Util.adapterIsLossy ad),
        Util.adapterSource = schema,
        Util.adapterTarget = (Core.TypeList (Util.adapterTarget ad)),
        Util.adapterCoder = Util.Coder {
          Util.coderEncode = (\cx1 -> \v -> (\x -> case x of
            Model.ValueArray v1 -> (Eithers.map (\ts -> Core.TermList ts) (Eithers.mapList (\jv -> Util.coderEncode (Util.adapterCoder ad) cx1 jv) v1))) v),
          Util.coderDecode = (\cx1 -> \t -> (\x -> case x of
            Core.TermList v1 -> (Eithers.map (\jvs -> Model.ValueArray jvs) (Eithers.mapList (\tv -> Util.coderDecode (Util.adapterCoder ad) cx1 tv) v1))) t)}}, env1))))
    Schema.SchemaMap v0 -> (Eithers.bind (avroHydraAdapter cx (Schema.mapValues v0) env0) (\adEnv ->  
      let ad = (Pairs.first adEnv) 
          env1 = (Pairs.second adEnv)
          pairToHydra = (\cx1 -> \entry ->  
                  let k = (Pairs.first entry) 
                      v = (Pairs.second entry)
                  in (Eithers.map (\v_ -> (Core.TermLiteral (Core.LiteralString k), v_)) (Util.coderEncode (Util.adapterCoder ad) cx1 v)))
      in (Right (Util.Adapter {
        Util.adapterIsLossy = (Util.adapterIsLossy ad),
        Util.adapterSource = schema,
        Util.adapterTarget = (Core.TypeMap (Core.MapType {
          Core.mapTypeKeys = (Core.TypeLiteral Core.LiteralTypeString),
          Core.mapTypeValues = (Util.adapterTarget ad)})),
        Util.adapterCoder = Util.Coder {
          Util.coderEncode = (\cx1 -> \v -> (\x -> case x of
            Model.ValueObject v1 -> (Eithers.map (\pairs -> Core.TermMap (Maps.fromList pairs)) (Eithers.mapList (\e -> pairToHydra cx1 e) (Maps.toList v1)))) v),
          Util.coderDecode = (\cx1 -> \m -> Eithers.map (\mp_ -> Model.ValueObject mp_) (Core_.map cx (\t -> Core_.string cx (Graph.Graph {
            Graph.graphBoundTerms = Maps.empty,
            Graph.graphBoundTypes = Maps.empty,
            Graph.graphClassConstraints = Maps.empty,
            Graph.graphLambdaVariables = Sets.empty,
            Graph.graphMetadata = Maps.empty,
            Graph.graphPrimitives = Maps.empty,
            Graph.graphSchemaTypes = Maps.empty,
            Graph.graphTypeVariables = Sets.empty}) t) (\t -> Util.coderDecode (Util.adapterCoder ad) cx1 t) (Graph.Graph {
            Graph.graphBoundTerms = Maps.empty,
            Graph.graphBoundTypes = Maps.empty,
            Graph.graphClassConstraints = Maps.empty,
            Graph.graphLambdaVariables = Sets.empty,
            Graph.graphMetadata = Maps.empty,
            Graph.graphPrimitives = Maps.empty,
            Graph.graphSchemaTypes = Maps.empty,
            Graph.graphTypeVariables = Sets.empty}) m))}}, env1))))
    Schema.SchemaNamed v0 ->  
      let ns = (Schema.namedNamespace v0) 
          manns = (namedAnnotationsToCore v0)
          ann = (Logic.ifElse (Maps.null manns) Nothing (Just manns))
          lastNs = (Environment.avroEnvironmentNamespace env0)
          nextNs = (Maybes.maybe lastNs (\s -> Just s) ns)
          env1 = Environment.AvroEnvironment {
                  Environment.avroEnvironmentNamedAdapters = (Environment.avroEnvironmentNamedAdapters env0),
                  Environment.avroEnvironmentNamespace = nextNs,
                  Environment.avroEnvironmentElements = (Environment.avroEnvironmentElements env0)}
          qname = Environment.AvroQualifiedName {
                  Environment.avroQualifiedNameNamespace = nextNs,
                  Environment.avroQualifiedNameName = (Schema.namedName v0)}
          hydraName = (avroNameToHydraName qname)
      in (Maybes.maybe (Eithers.bind ((\x -> case x of
        Schema.NamedTypeEnum v1 ->  
          let syms = (Schema.enumSymbols v1) 
              typ = (Core.TypeUnion (Core.RowType {
                      Core.rowTypeTypeName = hydraName,
                      Core.rowTypeFields = (Lists.map (\s -> Core.FieldType {
                        Core.fieldTypeName = (Core.Name s),
                        Core.fieldTypeType = Core.TypeUnit}) syms)}))
          in (simpleAdapter env1 typ (\_cx -> \jv -> (\x -> case x of
            Model.ValueString v2 -> (Right (Core.TermUnion (Core.Injection {
              Core.injectionTypeName = hydraName,
              Core.injectionField = Core.Field {
                Core.fieldName = (Core.Name v2),
                Core.fieldTerm = Core.TermUnit}})))) jv) (\_cx -> \t -> (\x -> case x of
            Core.TermUnion v2 ->  
              let fld = (Core.injectionField v2) 
                  fn = (Core.fieldName fld)
              in (Right (Model.ValueString (Core.unName fn)))) t))
        Schema.NamedTypeFixed _ -> (simpleAdapter env1 (Core.TypeLiteral Core.LiteralTypeBinary) (\_cx -> \jv -> (\x -> case x of
          Model.ValueString v2 -> (Right (Core.TermLiteral (Core.LiteralBinary (Literals.stringToBinary v2))))) jv) (\cx1 -> \t -> Eithers.map (\b -> Model.ValueString (Literals.binaryToString b)) (Core_.binary cx1 (Graph.Graph {
          Graph.graphBoundTerms = Maps.empty,
          Graph.graphBoundTypes = Maps.empty,
          Graph.graphClassConstraints = Maps.empty,
          Graph.graphLambdaVariables = Sets.empty,
          Graph.graphMetadata = Maps.empty,
          Graph.graphPrimitives = Maps.empty,
          Graph.graphSchemaTypes = Maps.empty,
          Graph.graphTypeVariables = Sets.empty}) t)))
        Schema.NamedTypeRecord v1 ->  
          let avroFields = (Schema.recordFields v1)
          in (Eithers.bind (prepareFields cx env1 avroFields) (\prepResult ->  
            let adaptersByFieldName = (Pairs.first prepResult) 
                env2 = (Pairs.second prepResult)
            in (Eithers.bind (findAvroPrimaryKeyField cx qname avroFields) (\pk ->  
              let encodePair = (\cx1 -> \entry ->  
                      let k = (Pairs.first entry) 
                          v = (Pairs.second entry)
                      in (Maybes.maybe (err cx1 (Strings.cat [
                        "unrecognized field for ",
                        (showQname qname),
                        ": ",
                        k])) (\fad -> Eithers.map (\v_ -> Core.Field {
                        Core.fieldName = (Core.Name k),
                        Core.fieldTerm = v_}) (Util.coderEncode (Util.adapterCoder (Pairs.second fad)) cx1 v)) (Maps.lookup k adaptersByFieldName))) 
                  decodeField = (\cx1 -> \fld ->  
                          let k = (Core.unName (Core.fieldName fld)) 
                              v = (Core.fieldTerm fld)
                          in (Maybes.maybe (err cx1 (Strings.cat [
                            "unrecognized field for ",
                            (showQname qname),
                            ": ",
                            k])) (\fad -> Eithers.map (\v_ -> (k, v_)) (Util.coderDecode (Util.adapterCoder (Pairs.second fad)) cx1 v)) (Maps.lookup k adaptersByFieldName)))
                  lossy = (Lists.foldl (\b -> \fad -> Logic.or b (Util.adapterIsLossy (Pairs.second fad))) False (Maps.elems adaptersByFieldName))
                  hfields = (Lists.map (\fad -> Core.FieldType {
                          Core.fieldTypeName = (Core.Name (Schema.fieldName (Pairs.first fad))),
                          Core.fieldTypeType = (Util.adapterTarget (Pairs.second fad))}) (Maps.elems adaptersByFieldName))
                  target = (Core.TypeRecord (Core.RowType {
                          Core.rowTypeTypeName = hydraName,
                          Core.rowTypeFields = hfields}))
              in (Right (Util.Adapter {
                Util.adapterIsLossy = lossy,
                Util.adapterSource = schema,
                Util.adapterTarget = target,
                Util.adapterCoder = Util.Coder {
                  Util.coderEncode = (\cx1 -> \jv -> (\x -> case x of
                    Model.ValueObject v2 -> (Eithers.map (\fields -> Core.TermRecord (Core.Record {
                      Core.recordTypeName = hydraName,
                      Core.recordFields = fields})) (Eithers.mapList (\e -> encodePair cx1 e) (Maps.toList v2)))) jv),
                  Util.coderDecode = (\cx1 -> \t -> (\x -> case x of
                    Core.TermRecord v2 -> (Eithers.map (\kvs -> Model.ValueObject (Maps.fromList kvs)) (Eithers.mapList (\fld -> decodeField cx1 fld) (Core.recordFields v2)))) t)}}, env2))))))) (Schema.namedType v0)) (\adEnv2 ->  
        let ad = (Pairs.first adEnv2) 
            env2 = (Pairs.second adEnv2)
            env3 = (putAvroHydraAdapter qname ad env2)
            env4 = Environment.AvroEnvironment {
                    Environment.avroEnvironmentNamedAdapters = (Environment.avroEnvironmentNamedAdapters env3),
                    Environment.avroEnvironmentNamespace = lastNs,
                    Environment.avroEnvironmentElements = (Environment.avroEnvironmentElements env3)}
        in (Right (annotateAdapter ann ad, env4)))) (\_ad -> err cx (Strings.cat2 "Avro named type defined more than once: " (showQname qname))) (getAvroHydraAdapter qname env1))
    Schema.SchemaPrimitive v0 -> ((\x -> case x of
      Schema.PrimitiveNull -> (simpleAdapter env0 Core.TypeUnit (\_cx -> \jv -> (\x -> case x of
        Model.ValueString v2 -> (Right (Core.TermLiteral (Core.LiteralString v2)))) jv) (\cx1 -> \t -> Eithers.map (\s -> Model.ValueString s) (Core_.string cx1 (Graph.Graph {
        Graph.graphBoundTerms = Maps.empty,
        Graph.graphBoundTypes = Maps.empty,
        Graph.graphClassConstraints = Maps.empty,
        Graph.graphLambdaVariables = Sets.empty,
        Graph.graphMetadata = Maps.empty,
        Graph.graphPrimitives = Maps.empty,
        Graph.graphSchemaTypes = Maps.empty,
        Graph.graphTypeVariables = Sets.empty}) t)))
      Schema.PrimitiveBoolean -> (simpleAdapter env0 (Core.TypeLiteral Core.LiteralTypeBoolean) (\_cx -> \jv -> (\x -> case x of
        Model.ValueBoolean v2 -> (Right (Core.TermLiteral (Core.LiteralBoolean v2)))) jv) (\cx1 -> \t -> Eithers.map (\b -> Model.ValueBoolean b) (Core_.boolean cx1 (Graph.Graph {
        Graph.graphBoundTerms = Maps.empty,
        Graph.graphBoundTypes = Maps.empty,
        Graph.graphClassConstraints = Maps.empty,
        Graph.graphLambdaVariables = Sets.empty,
        Graph.graphMetadata = Maps.empty,
        Graph.graphPrimitives = Maps.empty,
        Graph.graphSchemaTypes = Maps.empty,
        Graph.graphTypeVariables = Sets.empty}) t)))
      Schema.PrimitiveInt -> (simpleAdapter env0 (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)) (\_cx -> \jv -> (\x -> case x of
        Model.ValueNumber v2 -> (Right (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 (doubleToInt v2)))))) jv) (\cx1 -> \t -> Eithers.map (\i -> Model.ValueNumber (Literals.bigintToBigfloat (Literals.int32ToBigint i))) (Core_.int32 cx1 (Graph.Graph {
        Graph.graphBoundTerms = Maps.empty,
        Graph.graphBoundTypes = Maps.empty,
        Graph.graphClassConstraints = Maps.empty,
        Graph.graphLambdaVariables = Sets.empty,
        Graph.graphMetadata = Maps.empty,
        Graph.graphPrimitives = Maps.empty,
        Graph.graphSchemaTypes = Maps.empty,
        Graph.graphTypeVariables = Sets.empty}) t)))
      Schema.PrimitiveLong -> (simpleAdapter env0 (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt64)) (\_cx -> \jv -> (\x -> case x of
        Model.ValueNumber v2 -> (Right (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt64 (doubleToLong v2)))))) jv) (\cx1 -> \t -> Eithers.map (\i -> Model.ValueNumber (Literals.bigintToBigfloat (Literals.int64ToBigint i))) (Core_.int64 cx1 (Graph.Graph {
        Graph.graphBoundTerms = Maps.empty,
        Graph.graphBoundTypes = Maps.empty,
        Graph.graphClassConstraints = Maps.empty,
        Graph.graphLambdaVariables = Sets.empty,
        Graph.graphMetadata = Maps.empty,
        Graph.graphPrimitives = Maps.empty,
        Graph.graphSchemaTypes = Maps.empty,
        Graph.graphTypeVariables = Sets.empty}) t)))
      Schema.PrimitiveFloat -> (simpleAdapter env0 (Core.TypeLiteral (Core.LiteralTypeFloat Core.FloatTypeFloat32)) (\_cx -> \jv -> (\x -> case x of
        Model.ValueNumber v2 -> (Right (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat32 (Literals.bigfloatToFloat32 v2)))))) jv) (\cx1 -> \t -> Eithers.map (\f -> Model.ValueNumber (Literals.float32ToBigfloat f)) (Core_.float32 cx1 (Graph.Graph {
        Graph.graphBoundTerms = Maps.empty,
        Graph.graphBoundTypes = Maps.empty,
        Graph.graphClassConstraints = Maps.empty,
        Graph.graphLambdaVariables = Sets.empty,
        Graph.graphMetadata = Maps.empty,
        Graph.graphPrimitives = Maps.empty,
        Graph.graphSchemaTypes = Maps.empty,
        Graph.graphTypeVariables = Sets.empty}) t)))
      Schema.PrimitiveDouble -> (simpleAdapter env0 (Core.TypeLiteral (Core.LiteralTypeFloat Core.FloatTypeFloat64)) (\_cx -> \jv -> (\x -> case x of
        Model.ValueNumber v2 -> (Right (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 (Literals.bigfloatToFloat64 v2)))))) jv) (\cx1 -> \t -> Eithers.map (\d -> Model.ValueNumber (Literals.float64ToBigfloat d)) (Core_.float64 cx1 (Graph.Graph {
        Graph.graphBoundTerms = Maps.empty,
        Graph.graphBoundTypes = Maps.empty,
        Graph.graphClassConstraints = Maps.empty,
        Graph.graphLambdaVariables = Sets.empty,
        Graph.graphMetadata = Maps.empty,
        Graph.graphPrimitives = Maps.empty,
        Graph.graphSchemaTypes = Maps.empty,
        Graph.graphTypeVariables = Sets.empty}) t)))
      Schema.PrimitiveBytes -> (simpleAdapter env0 (Core.TypeLiteral Core.LiteralTypeBinary) (\_cx -> \jv -> (\x -> case x of
        Model.ValueString v2 -> (Right (Core.TermLiteral (Core.LiteralBinary (Literals.stringToBinary v2))))) jv) (\cx1 -> \t -> Eithers.map (\b -> Model.ValueString (Literals.binaryToString b)) (Core_.binary cx1 (Graph.Graph {
        Graph.graphBoundTerms = Maps.empty,
        Graph.graphBoundTypes = Maps.empty,
        Graph.graphClassConstraints = Maps.empty,
        Graph.graphLambdaVariables = Sets.empty,
        Graph.graphMetadata = Maps.empty,
        Graph.graphPrimitives = Maps.empty,
        Graph.graphSchemaTypes = Maps.empty,
        Graph.graphTypeVariables = Sets.empty}) t)))
      Schema.PrimitiveString -> (simpleAdapter env0 (Core.TypeLiteral Core.LiteralTypeString) (\_cx -> \jv -> (\x -> case x of
        Model.ValueString v2 -> (Right (Core.TermLiteral (Core.LiteralString v2)))) jv) (\cx1 -> \t -> Eithers.map (\s -> Model.ValueString s) (Core_.string cx1 (Graph.Graph {
        Graph.graphBoundTerms = Maps.empty,
        Graph.graphBoundTypes = Maps.empty,
        Graph.graphClassConstraints = Maps.empty,
        Graph.graphLambdaVariables = Sets.empty,
        Graph.graphMetadata = Maps.empty,
        Graph.graphPrimitives = Maps.empty,
        Graph.graphSchemaTypes = Maps.empty,
        Graph.graphTypeVariables = Sets.empty}) t)))) v0)
    Schema.SchemaReference v0 ->  
      let qname = (parseAvroName (Environment.avroEnvironmentNamespace env0) v0)
      in (Maybes.maybe (err cx (Strings.cat2 "Referenced Avro type has not been defined: " (showQname qname))) (\ad -> Right (ad, env0)) (getAvroHydraAdapter qname env0))
    Schema.SchemaUnion v0 ->  
      let schemas = (Schema.unUnion v0) 
          isNull = (\s -> (\x -> case x of
                  Schema.SchemaPrimitive v1 -> ((\x -> case x of
                    Schema.PrimitiveNull -> True
                    _ -> False) v1)
                  _ -> False) s)
          hasNull = (Logic.not (Lists.null (Lists.filter isNull schemas)))
          nonNulls = (Lists.filter (\s -> Logic.not (isNull s)) schemas)
          forOptional = (\s -> Eithers.bind (avroHydraAdapter cx s env0) (\adEnv ->  
                  let ad = (Pairs.first adEnv) 
                      env1 = (Pairs.second adEnv)
                  in (Right (Util.Adapter {
                    Util.adapterIsLossy = (Util.adapterIsLossy ad),
                    Util.adapterSource = schema,
                    Util.adapterTarget = (Core.TypeMaybe (Util.adapterTarget ad)),
                    Util.adapterCoder = Util.Coder {
                      Util.coderEncode = (\cx1 -> \v -> (\x -> case x of
                        Model.ValueNull -> (Right (Core.TermMaybe Nothing))
                        _ -> (Eithers.map (\t -> Core.TermMaybe (Just t)) (Util.coderEncode (Util.adapterCoder ad) cx1 v))) v),
                      Util.coderDecode = (\cx1 -> \t -> (\x -> case x of
                        Core.TermMaybe v1 -> (Maybes.maybe (Right Model.ValueNull) (\term_ -> Util.coderDecode (Util.adapterCoder ad) cx1 term_) v1)) t)}}, env1))))
      in (Logic.ifElse (Equality.gt (Lists.length nonNulls) 1) (err cx "general-purpose unions are not yet supported") (Logic.ifElse (Lists.null nonNulls) (err cx "cannot generate the empty type") (Logic.ifElse hasNull (forOptional (Lists.head nonNulls)) (Eithers.bind (avroHydraAdapter cx (Lists.head nonNulls) env0) (\adEnv ->  
        let ad = (Pairs.first adEnv) 
            env1 = (Pairs.second adEnv)
        in (Right (Util.Adapter {
          Util.adapterIsLossy = (Util.adapterIsLossy ad),
          Util.adapterSource = schema,
          Util.adapterTarget = (Util.adapterTarget ad),
          Util.adapterCoder = (Util.adapterCoder ad)}, env1)))))))) schema)

-- | Thread AvroEnvironment through preparing multiple fields
prepareFields :: (Context.Context -> Environment.AvroEnvironment -> [Schema.Field] -> Either (Context.InContext Error.Error) (M.Map String (Schema.Field, (Util.Adapter Schema.Schema Core.Type Model.Value Core.Term)), Environment.AvroEnvironment))
prepareFields cx env fields = (Lists.foldl (\acc -> \f -> Eithers.bind acc (\accPair ->  
  let m = (Pairs.first accPair) 
      env1 = (Pairs.second accPair)
  in (Eithers.bind (prepareField cx env1 f) (\result ->  
    let kv = (Pairs.first result) 
        env2 = (Pairs.second result)
        k = (Pairs.first kv)
        v = (Pairs.second kv)
    in (Right (Maps.insert k v m, env2)))))) (Right (Maps.empty, env)) fields)

-- | Prepare a single field, producing an adapter and updated environment
prepareField :: (Context.Context -> Environment.AvroEnvironment -> Schema.Field -> Either (Context.InContext Error.Error) ((String, (Schema.Field, (Util.Adapter Schema.Schema Core.Type Model.Value Core.Term))), Environment.AvroEnvironment))
prepareField cx env f =  
  let manns = (fieldAnnotationsToCore f) 
      ann = (Logic.ifElse (Maps.null manns) Nothing (Just manns))
  in (Eithers.bind (foreignKeyE cx f) (\fk -> Eithers.bind (Maybes.maybe (avroHydraAdapter cx (Schema.fieldType f) env) (\fkVal ->  
    let fkName = (Environment.avroForeignKeyTypeName fkVal) 
        fkConstr = (Environment.avroForeignKeyConstructor fkVal)
    in (Eithers.bind (avroHydraAdapter cx (Schema.fieldType f) env) (\adEnvPair ->  
      let ad0 = (Pairs.first adEnvPair) 
          env1 = (Pairs.second adEnvPair)
          elTyp = (Core.TypeVariable fkName)
          encodeValue = (\cx1 -> \v -> Eithers.bind (Util.coderEncode (Util.adapterCoder ad0) cx1 v) (\encoded -> Eithers.bind (termToStringE cx1 encoded) (\s -> Right (Core.TermVariable (fkConstr s)))))
          decodeTerm = (\cx1 -> \t -> (\x -> case x of
                  Core.TermVariable v0 -> (Eithers.bind (stringToTermE cx1 (Util.adapterTarget ad0) (Core.unName v0)) (\term_ -> Util.coderDecode (Util.adapterCoder ad0) cx1 term_))
                  _ -> (err cx1 "expected variable")) t)
          forTypeAndCoder = (\env2 -> \ad1 -> \typ -> \cdr -> Right (Util.Adapter {
                  Util.adapterIsLossy = (Util.adapterIsLossy ad1),
                  Util.adapterSource = (Schema.fieldType f),
                  Util.adapterTarget = typ,
                  Util.adapterCoder = cdr}, env2))
      in ((\x -> case x of
        Core.TypeMaybe v0 -> ((\x -> case x of
          Core.TypeLiteral _ -> (forTypeAndCoder env1 ad0 (Core.TypeMaybe elTyp) (Util.Coder {
            Util.coderEncode = (\cx2 -> \json -> Eithers.map (\v_ -> Core.TermMaybe (Just v_)) (encodeValue cx2 json)),
            Util.coderDecode = decodeTerm}))
          _ -> (err cx "expected literal type inside optional foreign key")) v0)
        Core.TypeList v0 -> ((\x -> case x of
          Core.TypeLiteral _ -> (forTypeAndCoder env1 ad0 (Core.TypeList elTyp) (Util.Coder {
            Util.coderEncode = (\cx2 -> \json -> (\x -> case x of
              Model.ValueArray v2 -> (Eithers.map (\terms -> Core.TermList terms) (Eithers.mapList (\jv -> encodeValue cx2 jv) v2))
              _ -> (err cx2 "Expected JSON array")) json),
            Util.coderDecode = decodeTerm}))
          _ -> (err cx "expected literal type inside list foreign key")) v0)
        Core.TypeLiteral _ -> (forTypeAndCoder env1 ad0 elTyp (Util.Coder {
          Util.coderEncode = encodeValue,
          Util.coderDecode = decodeTerm}))
        _ -> (err cx (Strings.cat2 "unsupported type annotated as foreign key: " "unknown"))) (Rewriting.deannotateType (Util.adapterTarget ad0)))))) fk) (\adEnv ->  
    let ad = (Pairs.first adEnv) 
        env1 = (Pairs.second adEnv)
    in (Right ((Schema.fieldName f, (f, (annotateAdapter ann ad))), env1)))))

-- | Annotate an adapter's target type with optional annotations
annotateAdapter :: (Maybe (M.Map Core.Name Core.Term) -> Util.Adapter t0 Core.Type t1 t2 -> Util.Adapter t0 Core.Type t1 t2)
annotateAdapter ann ad = (Maybes.maybe ad (\n -> Util.Adapter {
  Util.adapterIsLossy = (Util.adapterIsLossy ad),
  Util.adapterSource = (Util.adapterSource ad),
  Util.adapterTarget = (Core.TypeAnnotated (Core.AnnotatedType {
    Core.annotatedTypeBody = (Util.adapterTarget ad),
    Core.annotatedTypeAnnotation = n})),
  Util.adapterCoder = (Util.adapterCoder ad)}) ann)

-- | Find the primary key field among a list of Avro fields
findAvroPrimaryKeyField :: (Context.Context -> Environment.AvroQualifiedName -> [Schema.Field] -> Either (Context.InContext Error.Error) (Maybe Environment.AvroPrimaryKey))
findAvroPrimaryKeyField cx qname avroFields =  
  let keys = (Maybes.cat (Lists.map (\f -> primaryKeyE cx f) avroFields))
  in (Logic.ifElse (Lists.null keys) (Right Nothing) (Logic.ifElse (Equality.equal (Lists.length keys) 1) (Right (Just (Lists.head keys))) (err cx (Strings.cat2 "multiple primary key fields for " (showQname qname)))))

-- | Convert an Avro qualified name to a Hydra name
avroNameToHydraName :: (Environment.AvroQualifiedName -> Core.Name)
avroNameToHydraName qname =  
  let mns = (Environment.avroQualifiedNameNamespace qname) 
      local = (Environment.avroQualifiedNameName qname)
  in (Names.unqualifyName (Module.QualifiedName {
    Module.qualifiedNameNamespace = (Maybes.map (\s -> Module.Namespace s) mns),
    Module.qualifiedNameLocal = local}))

-- | Encode a JSON value as a Hydra term for annotation purposes
encodeAnnotationValue :: (Model.Value -> Core.Term)
encodeAnnotationValue v = ((\x -> case x of
  Model.ValueArray v0 -> (Core.TermList (Lists.map encodeAnnotationValue v0))
  Model.ValueBoolean v0 -> (Core.TermLiteral (Core.LiteralBoolean v0))
  Model.ValueNull -> Core.TermUnit
  Model.ValueNumber v0 -> (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueBigfloat v0)))
  Model.ValueObject v0 -> (Core.TermMap (Maps.fromList (Lists.map (\entry ->  
    let k = (Pairs.first entry) 
        v_ = (Pairs.second entry)
    in (Core.TermLiteral (Core.LiteralString k), (encodeAnnotationValue v_))) (Maps.toList v0))))
  Model.ValueString v0 -> (Core.TermLiteral (Core.LiteralString v0))) v)

-- | Extract field annotations and convert them to core Name/Term pairs
fieldAnnotationsToCore :: (Schema.Field -> M.Map Core.Name Core.Term)
fieldAnnotationsToCore f = (Maps.fromList (Lists.map (\entry ->  
  let k = (Pairs.first entry) 
      v = (Pairs.second entry)
  in (Core.Name k, (encodeAnnotationValue v))) (Maps.toList (Schema.fieldAnnotations f))))

-- | Extract named type annotations and convert them to core Name/Term pairs
namedAnnotationsToCore :: (Schema.Named -> M.Map Core.Name Core.Term)
namedAnnotationsToCore n = (Maps.fromList (Lists.map (\entry ->  
  let k = (Pairs.first entry) 
      v = (Pairs.second entry)
  in (Core.Name k, (encodeAnnotationValue v))) (Maps.toList (Schema.namedAnnotations n))))

-- | Look up an adapter by qualified name in the environment
getAvroHydraAdapter :: (Environment.AvroQualifiedName -> Environment.AvroEnvironment -> Maybe (Util.Adapter Schema.Schema Core.Type Model.Value Core.Term))
getAvroHydraAdapter qname env = (Maps.lookup qname (Environment.avroEnvironmentNamedAdapters env))

-- | Extract a foreign key annotation from a field, if present
foreignKeyE :: (Context.Context -> Schema.Field -> Either (Context.InContext Error.Error) (Maybe Environment.AvroForeignKey))
foreignKeyE cx f = (Maybes.maybe (Right Nothing) (\v -> Eithers.bind (expectObjectE cx v) (\m -> Eithers.bind (Eithers.map (\s -> Core.Name s) (requireStringE cx "type" m)) (\tname -> Eithers.bind (optStringE cx "pattern" m) (\pattern_ ->  
  let constr = (Maybes.maybe (\s -> Core.Name s) (\pat -> patternToNameConstructor pat) pattern_)
  in (Right (Just (Environment.AvroForeignKey {
    Environment.avroForeignKeyTypeName = tname,
    Environment.avroForeignKeyConstructor = constr}))))))) (Maps.lookup avro_foreignKey (Schema.fieldAnnotations f)))

-- | Create a name constructor from a pattern string
patternToNameConstructor :: (String -> String -> Core.Name)
patternToNameConstructor pat s = (Core.Name (Strings.intercalate s (Strings.splitOn "${}" pat)))

-- | Extract a primary key annotation from a field, if present
primaryKeyE :: (t0 -> Schema.Field -> Maybe Environment.AvroPrimaryKey)
primaryKeyE cx f = (Maybes.maybe Nothing (\v -> Eithers.either (\_ -> Nothing) (\s -> Just (Environment.AvroPrimaryKey {
  Environment.avroPrimaryKeyFieldName = (Core.Name (Schema.fieldName f)),
  Environment.avroPrimaryKeyConstructor = (patternToNameConstructor s)})) (expectStringE cx v)) (Maps.lookup avro_primaryKey (Schema.fieldAnnotations f)))

-- | Parse a dotted Avro name into a qualified name
parseAvroName :: (Maybe String -> String -> Environment.AvroQualifiedName)
parseAvroName mns name_ =  
  let parts = (Strings.splitOn "." name_) 
      local = (Lists.last parts)
  in (Logic.ifElse (Equality.equal (Lists.length parts) 1) (Environment.AvroQualifiedName {
    Environment.avroQualifiedNameNamespace = mns,
    Environment.avroQualifiedNameName = local}) (Environment.AvroQualifiedName {
    Environment.avroQualifiedNameNamespace = (Just (Strings.intercalate "." (Lists.init parts))),
    Environment.avroQualifiedNameName = local}))

-- | Store an adapter in the environment by qualified name
putAvroHydraAdapter :: (Environment.AvroQualifiedName -> Util.Adapter Schema.Schema Core.Type Model.Value Core.Term -> Environment.AvroEnvironment -> Environment.AvroEnvironment)
putAvroHydraAdapter qname ad env = Environment.AvroEnvironment {
  Environment.avroEnvironmentNamedAdapters = (Maps.insert qname ad (Environment.avroEnvironmentNamedAdapters env)),
  Environment.avroEnvironmentNamespace = (Environment.avroEnvironmentNamespace env),
  Environment.avroEnvironmentElements = (Environment.avroEnvironmentElements env)}

-- | Recursively rewrite an Avro schema using a monadic transformation function
rewriteAvroSchemaM :: (((Schema.Schema -> Either t0 Schema.Schema) -> Schema.Schema -> Either t0 Schema.Schema) -> Schema.Schema -> Either t0 Schema.Schema)
rewriteAvroSchemaM f schema =  
  let recurse = (rewriteAvroSchemaM f) 
      fsub = (\s -> (\x -> case x of
              Schema.SchemaArray v0 -> (Eithers.map (\els_ -> Schema.SchemaArray (Schema.Array {
                Schema.arrayItems = els_})) (recurse (Schema.arrayItems v0)))
              Schema.SchemaMap v0 -> (Eithers.map (\vs_ -> Schema.SchemaMap (Schema.Map {
                Schema.mapValues = vs_})) (recurse (Schema.mapValues v0)))
              Schema.SchemaNamed v0 -> (Eithers.map (\nt_ -> Schema.SchemaNamed (Schema.Named {
                Schema.namedName = (Schema.namedName v0),
                Schema.namedNamespace = (Schema.namedNamespace v0),
                Schema.namedAliases = (Schema.namedAliases v0),
                Schema.namedDoc = (Schema.namedDoc v0),
                Schema.namedType = nt_,
                Schema.namedAnnotations = (Schema.namedAnnotations v0)})) ((\x -> case x of
                Schema.NamedTypeRecord v1 -> (Eithers.map (\fields_ -> Schema.NamedTypeRecord (Schema.Record {
                  Schema.recordFields = fields_})) (Eithers.mapList (\fld -> Eithers.map (\t_ -> Schema.Field {
                  Schema.fieldName = (Schema.fieldName fld),
                  Schema.fieldDoc = (Schema.fieldDoc fld),
                  Schema.fieldType = t_,
                  Schema.fieldDefault = (Schema.fieldDefault fld),
                  Schema.fieldOrder = (Schema.fieldOrder fld),
                  Schema.fieldAliases = (Schema.fieldAliases fld),
                  Schema.fieldAnnotations = (Schema.fieldAnnotations fld)}) (recurse (Schema.fieldType fld))) (Schema.recordFields v1)))
                _ -> (Right (Schema.namedType v0))) (Schema.namedType v0)))
              Schema.SchemaUnion v0 -> (Eithers.map (\schemas_ -> Schema.SchemaUnion (Schema.Union schemas_)) (Eithers.mapList (\us -> recurse us) (Schema.unUnion v0)))
              _ -> (Right s)) s)
  in (f fsub schema)

-- | Convert a JSON value to a string, supporting booleans, strings, and numbers
jsonToStringE :: (Context.Context -> Model.Value -> Either (Context.InContext Error.Error) String)
jsonToStringE cx v = ((\x -> case x of
  Model.ValueBoolean v0 -> (Right (Logic.ifElse v0 "true" "false"))
  Model.ValueString v0 -> (Right v0)
  Model.ValueNumber v0 -> (Right (Literals.showBigfloat v0))
  _ -> (unexpectedE cx "string, number, or boolean" "other")) v)

-- | Convert an Avro qualified name to a display string
showQname :: (Environment.AvroQualifiedName -> String)
showQname qname =  
  let mns = (Environment.avroQualifiedNameNamespace qname) 
      local = (Environment.avroQualifiedNameName qname)
  in (Strings.cat2 (Maybes.maybe "" (\ns -> Strings.cat2 ns ".") mns) local)

-- | Parse a string into a term of the expected type
stringToTermE :: (Context.Context -> Core.Type -> String -> Either (Context.InContext Error.Error) Core.Term)
stringToTermE cx typ s =  
  let readErr = (err cx "failed to read value") 
      readAndWrap = (\reader -> \wrapper -> Maybes.maybe readErr (\v -> Right (Core.TermLiteral (wrapper v))) (reader s))
  in ((\x -> case x of
    Core.TypeLiteral v0 -> ((\x -> case x of
      Core.LiteralTypeBoolean -> (readAndWrap (\x -> Literals.readBoolean x) (\b -> Core.LiteralBoolean b))
      Core.LiteralTypeInteger v1 -> ((\x -> case x of
        Core.IntegerTypeBigint -> (readAndWrap (\x -> Literals.readBigint x) (\i -> Core.LiteralInteger (Core.IntegerValueBigint i)))
        Core.IntegerTypeInt8 -> (readAndWrap (\x -> Literals.readInt8 x) (\i -> Core.LiteralInteger (Core.IntegerValueInt8 i)))
        Core.IntegerTypeInt16 -> (readAndWrap (\x -> Literals.readInt16 x) (\i -> Core.LiteralInteger (Core.IntegerValueInt16 i)))
        Core.IntegerTypeInt32 -> (readAndWrap (\x -> Literals.readInt32 x) (\i -> Core.LiteralInteger (Core.IntegerValueInt32 i)))
        Core.IntegerTypeInt64 -> (readAndWrap (\x -> Literals.readInt64 x) (\i -> Core.LiteralInteger (Core.IntegerValueInt64 i)))
        Core.IntegerTypeUint8 -> (readAndWrap (\x -> Literals.readUint8 x) (\i -> Core.LiteralInteger (Core.IntegerValueUint8 i)))
        Core.IntegerTypeUint16 -> (readAndWrap (\x -> Literals.readUint16 x) (\i -> Core.LiteralInteger (Core.IntegerValueUint16 i)))
        Core.IntegerTypeUint32 -> (readAndWrap (\x -> Literals.readUint32 x) (\i -> Core.LiteralInteger (Core.IntegerValueUint32 i)))
        Core.IntegerTypeUint64 -> (readAndWrap (\x -> Literals.readUint64 x) (\i -> Core.LiteralInteger (Core.IntegerValueUint64 i)))) v1)
      Core.LiteralTypeString -> (Right (Core.TermLiteral (Core.LiteralString s)))
      _ -> (unexpectedE cx "literal type" "other literal type")) v0)
    _ -> (unexpectedE cx "literal type" "other")) (Rewriting.deannotateType typ))

-- | Convert a literal term to its string representation
termToStringE :: (Context.Context -> Core.Term -> Either (Context.InContext Error.Error) String)
termToStringE cx term = ((\x -> case x of
  Core.TermLiteral v0 -> ((\x -> case x of
    Core.LiteralBoolean v1 -> (Right (Literals.showBoolean v1))
    Core.LiteralInteger v1 -> (Right ((\x -> case x of
      Core.IntegerValueBigint v2 -> (Literals.showBigint v2)
      Core.IntegerValueInt8 v2 -> (Literals.showInt8 v2)
      Core.IntegerValueInt16 v2 -> (Literals.showInt16 v2)
      Core.IntegerValueInt32 v2 -> (Literals.showInt32 v2)
      Core.IntegerValueInt64 v2 -> (Literals.showInt64 v2)
      Core.IntegerValueUint8 v2 -> (Literals.showUint8 v2)
      Core.IntegerValueUint16 v2 -> (Literals.showUint16 v2)
      Core.IntegerValueUint32 v2 -> (Literals.showUint32 v2)
      Core.IntegerValueUint64 v2 -> (Literals.showUint64 v2)) v1))
    Core.LiteralString v1 -> (Right v1)
    _ -> (unexpectedE cx "boolean, integer, or string" "other literal")) v0)
  Core.TermMaybe v0 -> (Maybes.maybe (unexpectedE cx "literal value" "Nothing") (\term_ -> termToStringE cx term_) v0)
  _ -> (unexpectedE cx "literal value" "other")) (Rewriting.deannotateTerm term))

-- | Construct an error result with a message in context
err :: (Context.Context -> String -> Either (Context.InContext Error.Error) t0)
err cx msg = (Left (Context.InContext {
  Context.inContextObject = (Error.ErrorOther (Error.OtherError msg)),
  Context.inContextContext = cx}))

-- | Construct an error for unexpected values
unexpectedE :: (Context.Context -> String -> String -> Either (Context.InContext Error.Error) t0)
unexpectedE cx expected found = (err cx (Strings.cat [
  "Expected ",
  expected,
  ", found: ",
  found]))

-- | Extract a JSON array or return an error
expectArrayE :: (t0 -> Model.Value -> Either t1 [Model.Value])
expectArrayE cx value = ((\x -> case x of
  Model.ValueArray v0 -> (Right v0)) value)

-- | Extract a JSON object or return an error
expectObjectE :: (t0 -> Model.Value -> Either t1 (M.Map String Model.Value))
expectObjectE cx value = ((\x -> case x of
  Model.ValueObject v0 -> (Right v0)) value)

-- | Extract a JSON string or return an error
expectStringE :: (t0 -> Model.Value -> Either t1 String)
expectStringE cx value = ((\x -> case x of
  Model.ValueString v0 -> (Right v0)) value)

-- | Look up a required string attribute in a JSON object map
requireStringE :: (Context.Context -> String -> M.Map String Model.Value -> Either (Context.InContext Error.Error) String)
requireStringE cx fname m = (Maybes.maybe (err cx (Strings.cat [
  "required attribute ",
  (Literals.showString fname),
  " not found"])) (\v -> expectStringE cx v) (Maps.lookup fname m))

-- | Look up an optional string attribute in a JSON object map
optStringE :: Ord t1 => (t0 -> t1 -> M.Map t1 Model.Value -> Either t2 (Maybe String))
optStringE cx fname m = (Maybes.maybe (Right Nothing) (\v -> Eithers.map (\s -> Maybes.pure s) (expectStringE cx v)) (Maps.lookup fname m))
