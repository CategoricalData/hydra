-- Note: this is an automatically generated file. Do not edit.

-- | Avro-to-Hydra adapter for converting Avro schemas and data to Hydra types and terms

module Hydra.Ext.Avro.Coder where

import qualified Hydra.Context as Context
import qualified Hydra.Core as Core
import qualified Hydra.Error as Error
import qualified Hydra.Ext.Avro.Environment as Environment
import qualified Hydra.Ext.Org.Apache.Avro.Schema as Schema
import qualified Hydra.Json.Model as Model
import qualified Hydra.Lib.Eithers as Eithers
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Literals as Literals
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Pairs as Pairs
import qualified Hydra.Lib.Strings as Strings
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
avroHydraAdapter :: (t0 -> t1 -> t2 -> t3)
avroHydraAdapter cx schema env0 = (avroHydraAdapter cx schema env0)

-- | Thread AvroEnvironment through preparing multiple fields
prepareFields :: (t0 -> t1 -> t2 -> t3)
prepareFields cx env fields = (prepareFields cx env fields)

-- | Prepare a single field, producing an adapter and updated environment
prepareField :: (t0 -> t1 -> t2 -> t3)
prepareField cx env f = (prepareField cx env f)

-- | Annotate an adapter's target type with optional annotations
annotateAdapter :: (t0 -> t1 -> t2)
annotateAdapter ann ad = (annotateAdapter ann ad)

-- | Find the primary key field among a list of Avro fields
findAvroPrimaryKeyField :: (t0 -> t1 -> t2 -> t3)
findAvroPrimaryKeyField cx qname avroFields = (findAvroPrimaryKeyField cx qname avroFields)

-- | Convert an Avro qualified name to a Hydra name
avroNameToHydraName :: (t0 -> t1)
avroNameToHydraName qname = (avroNameToHydraName qname)

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
getAvroHydraAdapter :: (t0 -> t1 -> t2)
getAvroHydraAdapter qname env = (getAvroHydraAdapter qname env)

-- | Extract a foreign key annotation from a field, if present
foreignKeyE :: (t0 -> t1 -> t2)
foreignKeyE cx f = (foreignKeyE cx f)

-- | Create a name constructor from a pattern string
patternToNameConstructor :: (String -> String -> Core.Name)
patternToNameConstructor pat s = (Core.Name (Strings.intercalate s (Strings.splitOn "${}" pat)))

-- | Extract a primary key annotation from a field, if present
primaryKeyE :: (t0 -> t1 -> t2)
primaryKeyE cx f = (primaryKeyE cx f)

-- | Parse a dotted Avro name into a qualified name
parseAvroName :: (t0 -> t1 -> t2)
parseAvroName mns name = (parseAvroName mns name)

-- | Store an adapter in the environment by qualified name
putAvroHydraAdapter :: (t0 -> t1 -> t2 -> t3)
putAvroHydraAdapter qname ad env = (putAvroHydraAdapter qname ad env)

-- | Recursively rewrite an Avro schema using a monadic transformation function
rewriteAvroSchemaM :: (t0 -> t1 -> t2)
rewriteAvroSchemaM f schema = (rewriteAvroSchemaM f schema)

-- | Convert a JSON value to a string, supporting booleans, strings, and numbers
jsonToStringE :: (Context.Context -> Model.Value -> Either (Context.InContext Error.OtherError) String)
jsonToStringE cx v = ((\x -> case x of
  Model.ValueBoolean v0 -> (Right (Logic.ifElse v0 "true" "false"))
  Model.ValueString v0 -> (Right v0)
  Model.ValueNumber v0 -> (Right (Literals.showBigfloat v0))
  _ -> (unexpectedE cx "string, number, or boolean" "other")) v)

-- | Convert an Avro qualified name to a display string
showQname :: (t0 -> t1)
showQname qname = (showQname qname)

-- | Parse a string into a term of the expected type
stringToTermE :: (t0 -> t1 -> t2 -> t3)
stringToTermE cx typ s = (stringToTermE cx typ s)

-- | Convert a literal term to its string representation
termToStringE :: (t0 -> t1 -> t2)
termToStringE cx term = (termToStringE cx term)

-- | Construct an error result with a message in context
err :: (Context.Context -> String -> Either (Context.InContext Error.OtherError) t0)
err cx msg = (Left (Context.InContext {
  Context.inContextObject = (Error.OtherError msg),
  Context.inContextContext = cx}))

-- | Construct an error for unexpected values
unexpectedE :: (Context.Context -> String -> String -> Either (Context.InContext Error.OtherError) t0)
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
requireStringE :: (Context.Context -> String -> M.Map String Model.Value -> Either (Context.InContext Error.OtherError) String)
requireStringE cx fname m = (Maybes.maybe (err cx (Strings.cat [
  "required attribute ",
  (Literals.showString fname),
  " not found"])) (\v -> expectStringE cx v) (Maps.lookup fname m))

-- | Look up an optional string attribute in a JSON object map
optStringE :: Ord t1 => (t0 -> t1 -> M.Map t1 Model.Value -> Either t2 (Maybe String))
optStringE cx fname m = (Maybes.maybe (Right Nothing) (\v -> Eithers.map (\s -> Maybes.pure s) (expectStringE cx v)) (Maps.lookup fname m))
