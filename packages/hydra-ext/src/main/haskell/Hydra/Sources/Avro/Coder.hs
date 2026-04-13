module Hydra.Sources.Avro.Coder where

-- Standard imports for term-level sources outside of the kernel
import Hydra.Kernel
import Hydra.Sources.Libraries
import           Hydra.Dsl.Meta.Lib.Strings                as Strings
import           Hydra.Dsl.Meta.Phantoms                   as Phantoms
import qualified Hydra.Dsl.Annotations                     as Annotations
import qualified Hydra.Dsl.Bootstrap                       as Bootstrap
import qualified Hydra.Dsl.LiteralTypes                    as LiteralTypes
import qualified Hydra.Dsl.Literals                        as Literals
import qualified Hydra.Dsl.Paths                      as Paths
import qualified Hydra.Dsl.Ast                        as Ast
import qualified Hydra.Dsl.Meta.Base                       as MetaBase
import qualified Hydra.Dsl.Coders                     as Coders
import qualified Hydra.Dsl.Util                    as Util
import qualified Hydra.Dsl.Meta.Context                    as Ctx
import qualified Hydra.Dsl.Meta.Core                       as Core
import qualified Hydra.Dsl.Errors                      as Error
import qualified Hydra.Dsl.Meta.Graph                      as Graph
import qualified Hydra.Dsl.Json.Model                       as Json
import qualified Hydra.Dsl.Meta.Lib.Chars                  as Chars
import qualified Hydra.Dsl.Meta.Lib.Eithers                as Eithers
import qualified Hydra.Dsl.Meta.Lib.Equality               as Equality
import qualified Hydra.Dsl.Meta.Lib.Lists                  as Lists
import qualified Hydra.Dsl.Meta.Lib.Literals               as Literals
import qualified Hydra.Dsl.Meta.Lib.Logic                  as Logic
import qualified Hydra.Dsl.Meta.Lib.Maps                   as Maps
import qualified Hydra.Dsl.Meta.Lib.Math                   as Math
import qualified Hydra.Dsl.Meta.Lib.Maybes                 as Maybes
import qualified Hydra.Dsl.Meta.Lib.Pairs                  as Pairs
import qualified Hydra.Dsl.Meta.Lib.Sets                   as Sets
import qualified Hydra.Dsl.Packaging                     as Packaging
import qualified Hydra.Dsl.Meta.Terms                      as MetaTerms
import qualified Hydra.Dsl.Meta.Testing                    as Testing
import qualified Hydra.Dsl.Topology                   as Topology
import qualified Hydra.Dsl.Meta.Types                      as MetaTypes
import qualified Hydra.Dsl.Typing                     as Typing
import qualified Hydra.Dsl.Util                       as Util
import qualified Hydra.Dsl.Meta.Variants                   as Variants
import qualified Hydra.Dsl.Prims                           as Prims
import qualified Hydra.Dsl.Meta.Tabular                         as Tabular
import qualified Hydra.Dsl.Terms                           as Terms
import qualified Hydra.Dsl.Tests                           as Tests
import qualified Hydra.Dsl.Types                           as Types
import qualified Hydra.Sources.Decode.Core                 as DecodeCore
import qualified Hydra.Sources.Encode.Core                 as EncodeCore
import qualified Hydra.Sources.Kernel.Terms.Adapt           as Adapt
import qualified Hydra.Sources.Kernel.Terms.All            as KernelTerms
import qualified Hydra.Sources.Kernel.Terms.Annotations    as Annotations
import qualified Hydra.Sources.Kernel.Terms.Arity          as Arity
import qualified Hydra.Sources.Kernel.Terms.Checking       as Checking
import qualified Hydra.Sources.Kernel.Terms.Constants      as Constants
import qualified Hydra.Sources.Kernel.Terms.Extract.Core   as ExtractCore
import qualified Hydra.Sources.Kernel.Terms.Extract.Util   as ExtractUtil
import qualified Hydra.Sources.Kernel.Terms.Formatting     as Formatting
import qualified Hydra.Sources.Kernel.Terms.Inference      as Inference
import qualified Hydra.Sources.Kernel.Terms.Languages      as Languages
import qualified Hydra.Sources.Kernel.Terms.Lexical        as Lexical
import qualified Hydra.Sources.Kernel.Terms.Literals       as Literals
import qualified Hydra.Sources.Kernel.Terms.Names          as Names
import qualified Hydra.Sources.Kernel.Terms.Reduction      as Reduction
import qualified Hydra.Sources.Kernel.Terms.Reflect        as Reflect
import qualified Hydra.Sources.Kernel.Terms.Strip          as Strip
import qualified Hydra.Sources.Kernel.Terms.Serialization  as Serialization
import qualified Hydra.Sources.Kernel.Terms.Show.Paths as ShowPaths
import qualified Hydra.Sources.Kernel.Terms.Show.Core      as ShowCore
import qualified Hydra.Sources.Kernel.Terms.Show.Graph     as ShowGraph
import qualified Hydra.Sources.Kernel.Terms.Show.Variants  as ShowVariants
import qualified Hydra.Sources.Kernel.Terms.Show.Typing    as ShowTyping
import qualified Hydra.Sources.Kernel.Terms.Sorting        as Sorting
import qualified Hydra.Sources.Kernel.Terms.Substitution   as Substitution
import qualified Hydra.Sources.Kernel.Terms.Templates      as Templates
import qualified Hydra.Sources.Kernel.Terms.Unification    as Unification
import qualified Hydra.Sources.Kernel.Types.All            as KernelTypes
import           Prelude hiding ((++))
import qualified Data.Int                                  as I
import qualified Data.List                                 as L
import qualified Data.Map                                  as M
import qualified Data.Set                                  as S
import qualified Data.Maybe                                as Y

-- Additional imports
import qualified Hydra.Avro.Schema as Avro
import qualified Hydra.Json.Model as JM
import qualified Hydra.Sources.Avro.Schema as AvroSchema
import qualified Hydra.Avro.Environment as AvroEnv
-- Local type aliases
type Result a = Either Error a
type AvroHydraAdapter = Adapter Avro.Schema Type JM.Value Term


define :: String -> TTerm a -> TTermDefinition a
define = definitionInModule module_

avroSchemaPhantomNs :: Namespace
avroSchemaPhantomNs = Namespace "hydra.avro.schema"

jsonModelNs :: Namespace
jsonModelNs = Namespace "hydra.json.model"

ns :: Namespace
ns = Namespace "hydra.avro.coder"

avroEnvironmentNs :: Namespace
avroEnvironmentNs = Namespace "hydra.avro.environment"

module_ :: Module
module_ = Module ns definitions
    [ExtractCore.ns, Strip.ns]
    (avroEnvironmentNs:AvroSchema.ns:jsonModelNs:KernelTypes.kernelTypesNamespaces) $
    Just "Avro-to-Hydra adapter for converting Avro schemas and data to Hydra types and terms"
  where
    definitions = [
      toDefinition avro_foreignKey,
      toDefinition avro_primaryKey,
      toDefinition emptyAvroEnvironment,
      toDefinition avroHydraAdapter,
      toDefinition prepareFields,
      toDefinition prepareField,
      toDefinition annotateAdapter,
      toDefinition findAvroPrimaryKeyField,
      toDefinition avroNameToHydraName,
      toDefinition encodeAnnotationValue,
      toDefinition fieldAnnotationsToCore,
      toDefinition namedAnnotationsToCore,
      toDefinition getAvroHydraAdapter,
      toDefinition foreignKeyE,
      toDefinition patternToNameConstructor,
      toDefinition primaryKeyE,
      toDefinition parseAvroName,
      toDefinition putAvroHydraAdapter,
      toDefinition rewriteAvroSchemaM,
      toDefinition jsonToStringE,
      toDefinition showQname,
      toDefinition stringToTermE,
      toDefinition termToStringE,
      toDefinition err,
      toDefinition unexpectedE,
      toDefinition expectArrayE,
      toDefinition expectObjectE,
      toDefinition expectStringE,
      toDefinition requireStringE,
      toDefinition optStringE]


-- | Error helpers

err :: TTermDefinition (Context -> String -> Result a)
err = define "err" $
  doc "Construct an error result with a message in context" $
  lambda "cx" $ lambda "msg" $
    Ctx.failInContext (Error.errorOther $ Error.otherError (var "msg")) (var "cx")

unexpectedE :: TTermDefinition (Context -> String -> String -> Result a)
unexpectedE = define "unexpectedE" $
  doc "Construct an error for unexpected values" $
  lambda "cx" $ lambda "expected" $ lambda "found" $
    err @@ var "cx" @@ (Strings.cat $ list [string "Expected ", var "expected", string ", found: ", var "found"])

-- | JSON extraction helpers

-- Note: the error branches in these three functions are dropped during code generation because the
-- success branches don't constrain the `cx` parameter, so inference generalizes it to `t0` and the
-- code generator eliminates type-incompatible default branches. The functions remain safe because
-- callers (foreignKeyE, requireStringE, optStringE) always pass the correct JSON value types.
-- This mirrors the staging behavior (partial patterns with explicit error branches) but with
-- implicit rather than explicit partiality.

expectArrayE :: TTermDefinition (Context -> JM.Value -> Result [JM.Value])
expectArrayE = define "expectArrayE" $
  doc "Extract a JSON array or return an error" $
  lambda "cx" $ lambda "value" $
    cases JM._Value (var "value") Nothing [
      JM._Value_array>>: lambda "v" $ right (var "v")]

expectObjectE :: TTermDefinition (Context -> JM.Value -> Result (M.Map String JM.Value))
expectObjectE = define "expectObjectE" $
  doc "Extract a JSON object or return an error" $
  lambda "cx" $ lambda "value" $
    cases JM._Value (var "value") Nothing [
      JM._Value_object>>: lambda "v" $ right (var "v")]

expectStringE :: TTermDefinition (Context -> JM.Value -> Result String)
expectStringE = define "expectStringE" $
  doc "Extract a JSON string or return an error" $
  lambda "cx" $ lambda "value" $
    cases JM._Value (var "value") Nothing [
      JM._Value_string>>: lambda "v" $ right (var "v")]

requireStringE :: TTermDefinition (Context -> String -> M.Map String JM.Value -> Result String)
requireStringE = define "requireStringE" $
  doc "Look up a required string attribute in a JSON object map" $
  lambda "cx" $ lambda "fname" $ lambda "m" $
    Maybes.maybe
      (err @@ var "cx" @@ (Strings.cat $ list [string "required attribute ", Literals.showString (var "fname"), string " not found"]))
      (lambda "v" $ expectStringE @@ var "cx" @@ var "v")
      (Maps.lookup (var "fname") (var "m"))

optStringE :: TTermDefinition (Context -> String -> M.Map String JM.Value -> Result (Maybe String))
optStringE = define "optStringE" $
  doc "Look up an optional string attribute in a JSON object map" $
  lambda "cx" $ lambda "fname" $ lambda "m" $
    Maybes.maybe
      (right nothing)
      (lambda "v" $ Eithers.map (lambda "s" $ Maybes.pure (var "s")) (expectStringE @@ var "cx" @@ var "v"))
      (Maps.lookup (var "fname") (var "m"))


-- | Constants

avro_foreignKey :: TTermDefinition String
avro_foreignKey = define "avro_foreignKey" $
  string "@foreignKey"

avro_primaryKey :: TTermDefinition String
avro_primaryKey = define "avro_primaryKey" $
  string "@primaryKey"

emptyAvroEnvironment :: TTermDefinition AvroEnv.AvroEnvironment
emptyAvroEnvironment = define "emptyAvroEnvironment" $
  doc "An empty Avro environment with no named adapters, no namespace, and no elements" $
  record (Name "hydra.avro.environment.AvroEnvironment") [
    (Name "namedAdapters")>>: Maps.empty,
    (Name "namespace")>>: nothing,
    (Name "elements")>>: Maps.empty]


-- | Core functions


avroHydraAdapter :: TTermDefinition (Context -> Avro.Schema -> AvroEnv.AvroEnvironment -> Result (AvroHydraAdapter, AvroEnv.AvroEnvironment))
avroHydraAdapter = define "avroHydraAdapter" $
  doc "Create an adapter between Avro schemas and Hydra types/terms" $
  lambda "cx" $ lambda "schema" $ lambda "env0" $ lets [
    -- simpleAdapter: create a non-lossy adapter with given type, encode, and decode functions
    "simpleAdapter">: lambda "env" $ lambda "typ" $ lambda "encode" $ lambda "decode" $
      right (pair
        (Coders.adapter (boolean False) (var "schema") (var "typ") (Coders.coder (var "encode") (var "decode")))
        (var "env")),
    -- doubleToInt: truncate a JSON number (bigfloat) to int32 (toward zero)
    -- Math.truncate returns Float64 (divergence from Haskell); convert back to bigfloat before bigint.
    "doubleToInt">: lambda "d" $ Literals.bigintToInt32 (Literals.bigfloatToBigint (Literals.float64ToBigfloat (Math.truncate (Literals.bigfloatToFloat64 (var "d"))))),
    -- doubleToLong: truncate a JSON number (bigfloat) to int64 (toward zero)
    "doubleToLong">: lambda "d" $ Literals.bigintToInt64 (Literals.bigfloatToBigint (Literals.float64ToBigfloat (Math.truncate (Literals.bigfloatToFloat64 (var "d")))))] $
    cases Avro._Schema (var "schema") Nothing [
      -- SchemaArray
      Avro._Schema_array>>: lambda "arr" $
        Eithers.bind (avroHydraAdapter @@ var "cx" @@ (project Avro._Array Avro._Array_items @@ var "arr") @@ var "env0") (lambda "adEnv" $ lets [
          "ad">: Pairs.first (var "adEnv"),
          "env1">: Pairs.second (var "adEnv")] $
          right (pair
            (Coders.adapter (Coders.adapterIsLossy (var "ad")) (var "schema")
              (MetaTypes.list (Coders.adapterTarget (var "ad")))
              (Coders.coder
                (lambda "cx1" $ lambda "v" $
                  cases JM._Value (var "v") Nothing [
                    JM._Value_array>>: lambda "vals" $
                      Eithers.map (lambda "ts" $ Core.termList (var "ts"))
                        (Eithers.mapList (lambda "jv" $ Coders.coderEncode (Coders.adapterCoder (var "ad")) @@ var "cx1" @@ var "jv") (var "vals"))])
                (lambda "cx1" $ lambda "t" $
                  cases _Term (var "t") Nothing [
                    _Term_list>>: lambda "vals" $
                      Eithers.map (lambda "jvs" $ inject JM._Value JM._Value_array (var "jvs"))
                        (Eithers.mapList (lambda "tv" $ Coders.coderDecode (Coders.adapterCoder (var "ad")) @@ var "cx1" @@ var "tv") (var "vals"))])))
            (var "env1"))),

      -- SchemaMap
      Avro._Schema_map>>: lambda "mp" $
        Eithers.bind (avroHydraAdapter @@ var "cx" @@ (project Avro._Map Avro._Map_values @@ var "mp") @@ var "env0") (lambda "adEnv" $ lets [
          "ad">: Pairs.first (var "adEnv"),
          "env1">: Pairs.second (var "adEnv"),
          "pairToHydra">: lambda "cx1" $ lambda "entry" $ lets [
            "k">: Pairs.first (var "entry"),
            "v">: Pairs.second (var "entry")] $
            Eithers.map
              (lambda "v'" $ pair (MetaTerms.stringLift (var "k")) (var "v'"))
              (Coders.coderEncode (Coders.adapterCoder (var "ad")) @@ var "cx1" @@ var "v")] $
          right (pair
            (Coders.adapter (Coders.adapterIsLossy (var "ad")) (var "schema")
              (MetaTypes.map MetaTypes.string (Coders.adapterTarget (var "ad")))
              (Coders.coder
                (lambda "cx1" $ lambda "v" $
                  cases JM._Value (var "v") Nothing [
                    JM._Value_object>>: lambda "m" $
                      Eithers.map (lambda "pairs" $ Core.termMap (Maps.fromList (var "pairs")))
                        (Eithers.mapList (lambda "e" $ var "pairToHydra" @@ var "cx1" @@ var "e") (Maps.toList (var "m")))])
                (lambda "cx1" $ lambda "m" $
                  Eithers.map (lambda "mp'" $ inject JM._Value JM._Value_object (var "mp'"))
                    (ExtractCore.map @@ (lambda "t" $ ExtractCore.string @@ Graph.emptyGraph @@ var "t")
                      @@ (lambda "t" $ Coders.coderDecode (Coders.adapterCoder (var "ad")) @@ var "cx1" @@ var "t")
                      @@ Graph.emptyGraph @@ var "m"))))
            (var "env1"))),

      -- SchemaNamed
      Avro._Schema_named>>: lambda "n" $ lets [
        "ns">: project Avro._Named Avro._Named_namespace @@ var "n",
        "manns">: namedAnnotationsToCore @@ var "n",
        "ann">: Logic.ifElse (Maps.null (var "manns")) nothing (just (var "manns")),
        "lastNs">: project AvroEnv._AvroEnvironment AvroEnv._AvroEnvironment_namespace @@ var "env0",
        "nextNs">: Maybes.maybe (var "lastNs") (lambda "s" $ just (var "s")) (var "ns"),
        "env1">: record AvroEnv._AvroEnvironment [
          AvroEnv._AvroEnvironment_namedAdapters>>:
            project AvroEnv._AvroEnvironment AvroEnv._AvroEnvironment_namedAdapters @@ var "env0",
          AvroEnv._AvroEnvironment_namespace>>: var "nextNs",
          AvroEnv._AvroEnvironment_elements>>:
            project AvroEnv._AvroEnvironment AvroEnv._AvroEnvironment_elements @@ var "env0"],
        "qname">: record AvroEnv._AvroQualifiedName [
          AvroEnv._AvroQualifiedName_namespace>>: var "nextNs",
          AvroEnv._AvroQualifiedName_name>>: project Avro._Named Avro._Named_name @@ var "n"],
        "hydraName">: avroNameToHydraName @@ var "qname"] $
        -- Check if already defined
        Maybes.maybe
          -- Not previously defined: process based on named type
          (Eithers.bind
            (cases Avro._NamedType (project Avro._Named Avro._Named_type @@ var "n") Nothing [
              -- Enum
              Avro._NamedType_enum>>: lambda "e" $ lets [
                "syms">: project Avro._Enum Avro._Enum_symbols @@ var "e",
                "typ">: Core.typeUnion
                  (Lists.map (lambda "s" $ Core.fieldType (Core.name (var "s")) MetaTypes.unit) (var "syms"))] $
                var "simpleAdapter" @@ var "env1" @@ var "typ"
                  @@ (lambda "_cx" $ lambda "jv" $
                    cases JM._Value (var "jv") Nothing [
                      JM._Value_string>>: lambda "s" $
                        right (Core.termUnion (Core.injection (var "hydraName") (Core.field (Core.name (var "s")) Core.termUnit)))])
                  @@ (lambda "_cx" $ lambda "t" $
                    cases _Term (var "t") Nothing [
                      _Term_union>>: lambda "inj" $ lets [
                        "fld">: project _Injection _Injection_field @@ var "inj",
                        "fn">: project _Field _Field_name @@ var "fld"] $
                        right (inject JM._Value JM._Value_string (unwrap _Name @@ var "fn"))]),

              -- Fixed
              Avro._NamedType_fixed>>: lambda "_f" $
                var "simpleAdapter" @@ var "env1" @@ MetaTypes.binary
                  @@ (lambda "_cx" $ lambda "jv" $
                    cases JM._Value (var "jv") Nothing [
                      JM._Value_string>>: lambda "s" $
                        right (Core.termLiteral (Core.literalBinary (Literals.stringToBinary (var "s"))))])
                  @@ (lambda "cx1" $ lambda "t" $
                    Eithers.map (lambda "b" $ inject JM._Value JM._Value_string (Literals.binaryToString (var "b")))
                      (ExtractCore.binary @@ Graph.emptyGraph @@ var "t")),

              -- Record
              Avro._NamedType_record>>: lambda "r" $ lets [
                "avroFields">: project Avro._Record Avro._Record_fields @@ var "r"] $
                Eithers.bind (prepareFields @@ var "cx" @@ var "env1" @@ var "avroFields") (lambda "prepResult" $ lets [
                  "adaptersByFieldName">: Pairs.first (var "prepResult"),
                  "env2">: Pairs.second (var "prepResult")] $
                  Eithers.bind (findAvroPrimaryKeyField @@ var "cx" @@ var "qname" @@ var "avroFields") (lambda "pk" $ lets [
                    -- encodePair: encode a key-value pair from JSON object
                    "encodePair">: lambda "cx1" $ lambda "entry" $ lets [
                      "k">: Pairs.first (var "entry"),
                      "v">: Pairs.second (var "entry")] $
                      Maybes.maybe
                        (err @@ var "cx1" @@ Strings.cat (list [string "unrecognized field for ", showQname @@ var "qname", string ": ", var "k"]))
                        (lambda "fad" $ Eithers.map
                          (lambda "v'" $ Core.field (Core.name (var "k")) (var "v'"))
                          (Coders.coderEncode (Coders.adapterCoder (Pairs.second (var "fad"))) @@ var "cx1" @@ var "v"))
                        (Maps.lookup (var "k") (var "adaptersByFieldName")),
                    -- decodeField: decode a Hydra field back to a key-value pair
                    "decodeField">: lambda "cx1" $ lambda "fld" $ lets [
                      "k">: unwrap _Name @@ (project _Field _Field_name @@ var "fld"),
                      "v">: project _Field _Field_term @@ var "fld"] $
                      Maybes.maybe
                        (err @@ var "cx1" @@ Strings.cat (list [string "unrecognized field for ", showQname @@ var "qname", string ": ", var "k"]))
                        (lambda "fad" $ Eithers.map
                          (lambda "v'" $ pair (var "k") (var "v'"))
                          (Coders.coderDecode (Coders.adapterCoder (Pairs.second (var "fad"))) @@ var "cx1" @@ var "v"))
                        (Maps.lookup (var "k") (var "adaptersByFieldName")),
                    -- lossy: any adapter is lossy?
                    "lossy">: Lists.foldl (lambda "b" $ lambda "fad" $ Logic.or (var "b") (Coders.adapterIsLossy (Pairs.second (var "fad"))))
                      (boolean False) (Maps.elems (var "adaptersByFieldName")),
                    -- hfields: Hydra field types
                    "hfields">: Lists.map (lambda "fad" $
                      Core.fieldType (Core.name (project Avro._Field Avro._Field_name @@ Pairs.first (var "fad")))
                        (Coders.adapterTarget (Pairs.second (var "fad"))))
                      (Maps.elems (var "adaptersByFieldName")),
                    "target">: Core.typeRecord (var "hfields")] $
                    right (pair
                      (Coders.adapter (var "lossy") (var "schema") (var "target")
                        (Coders.coder
                          (lambda "cx1" $ lambda "jv" $
                            cases JM._Value (var "jv") Nothing [
                              JM._Value_object>>: lambda "m" $
                                Eithers.map (lambda "fields" $ Core.termRecord (Core.record (var "hydraName") (var "fields")))
                                  (Eithers.mapList (lambda "e" $ var "encodePair" @@ var "cx1" @@ var "e") (Maps.toList (var "m")))])
                          (lambda "cx1" $ lambda "t" $
                            cases _Term (var "t") Nothing [
                              _Term_record>>: lambda "rec" $
                                Eithers.map (lambda "kvs" $ inject JM._Value JM._Value_object (Maps.fromList (var "kvs")))
                                  (Eithers.mapList (lambda "fld" $ var "decodeField" @@ var "cx1" @@ var "fld")
                                    (project _Record _Record_fields @@ var "rec"))])))
                      (var "env2"))))])
            (lambda "adEnv2" $ lets [
              "ad">: Pairs.first (var "adEnv2"),
              "env2">: Pairs.second (var "adEnv2"),
              -- Note: staging stores unannotated adapter in env but returns annotated
              "env3">: putAvroHydraAdapter @@ var "qname" @@ var "ad" @@ var "env2",
              -- Restore the previous namespace
              "env4">: record AvroEnv._AvroEnvironment [
                AvroEnv._AvroEnvironment_namedAdapters>>:
                  project AvroEnv._AvroEnvironment AvroEnv._AvroEnvironment_namedAdapters @@ var "env3",
                AvroEnv._AvroEnvironment_namespace>>: var "lastNs",
                AvroEnv._AvroEnvironment_elements>>:
                  project AvroEnv._AvroEnvironment AvroEnv._AvroEnvironment_elements @@ var "env3"]] $
              right (pair (annotateAdapter @@ var "ann" @@ var "ad") (var "env4"))))
          -- Already defined: error
          (lambda "_ad" $ err @@ var "cx" @@ Strings.cat2 (string "Avro named type defined more than once: ") (showQname @@ var "qname"))
          (getAvroHydraAdapter @@ var "qname" @@ var "env1"),

      -- SchemaPrimitive
      Avro._Schema_primitive>>: lambda "p" $
        cases Avro._Primitive (var "p") Nothing [
          -- Null
          Avro._Primitive_null>>: constant $
            var "simpleAdapter" @@ var "env0" @@ MetaTypes.unit
              @@ (lambda "_cx" $ lambda "jv" $
                cases JM._Value (var "jv") Nothing [
                  JM._Value_string>>: lambda "s" $ right (MetaTerms.stringLift (var "s"))])
              @@ (lambda "cx1" $ lambda "t" $
                Eithers.map (lambda "s" $ inject JM._Value JM._Value_string (var "s"))
                  (ExtractCore.string @@ Graph.emptyGraph @@ var "t")),
          -- Boolean
          Avro._Primitive_boolean>>: constant $
            var "simpleAdapter" @@ var "env0" @@ MetaTypes.boolean
              @@ (lambda "_cx" $ lambda "jv" $
                cases JM._Value (var "jv") Nothing [
                  JM._Value_boolean>>: lambda "b" $ right (MetaTerms.booleanLift (var "b"))])
              @@ (lambda "cx1" $ lambda "t" $
                Eithers.map (lambda "b" $ inject JM._Value JM._Value_boolean (var "b"))
                  (ExtractCore.boolean @@ Graph.emptyGraph @@ var "t")),
          -- Int
          Avro._Primitive_int>>: constant $
            var "simpleAdapter" @@ var "env0" @@ MetaTypes.int32
              @@ (lambda "_cx" $ lambda "jv" $
                cases JM._Value (var "jv") Nothing [
                  JM._Value_number>>: lambda "d" $ right (Core.termLiteral (Core.literalInteger (Core.integerValueInt32 (var "doubleToInt" @@ var "d"))))])
              @@ (lambda "cx1" $ lambda "t" $
                Eithers.map (lambda "i" $ inject JM._Value JM._Value_number (Literals.bigintToBigfloat (Literals.int32ToBigint (var "i"))))
                  (ExtractCore.int32 @@ Graph.emptyGraph @@ var "t")),
          -- Long
          Avro._Primitive_long>>: constant $
            var "simpleAdapter" @@ var "env0" @@ MetaTypes.int64
              @@ (lambda "_cx" $ lambda "jv" $
                cases JM._Value (var "jv") Nothing [
                  JM._Value_number>>: lambda "d" $ right (Core.termLiteral (Core.literalInteger (Core.integerValueInt64 (var "doubleToLong" @@ var "d"))))])
              @@ (lambda "cx1" $ lambda "t" $
                Eithers.map (lambda "i" $ inject JM._Value JM._Value_number (Literals.bigintToBigfloat (Literals.int64ToBigint (var "i"))))
                  (ExtractCore.int64 @@ Graph.emptyGraph @@ var "t")),
          -- Float
          Avro._Primitive_float>>: constant $
            var "simpleAdapter" @@ var "env0" @@ MetaTypes.float32
              @@ (lambda "_cx" $ lambda "jv" $
                cases JM._Value (var "jv") Nothing [
                  JM._Value_number>>: lambda "d" $ right (Core.termLiteral (Core.literalFloat (Core.floatValueFloat32 (Literals.bigfloatToFloat32 (var "d")))))])
              @@ (lambda "cx1" $ lambda "t" $
                Eithers.map (lambda "f" $ inject JM._Value JM._Value_number (Literals.float32ToBigfloat (var "f")))
                  (ExtractCore.float32 @@ Graph.emptyGraph @@ var "t")),
          -- Double
          Avro._Primitive_double>>: constant $
            var "simpleAdapter" @@ var "env0" @@ MetaTypes.float64
              @@ (lambda "_cx" $ lambda "jv" $
                cases JM._Value (var "jv") Nothing [
                  JM._Value_number>>: lambda "d" $ right (Core.termLiteral (Core.literalFloat (Core.floatValueFloat64 (Literals.bigfloatToFloat64 (var "d")))))])
              @@ (lambda "cx1" $ lambda "t" $
                Eithers.map (lambda "d" $ inject JM._Value JM._Value_number (Literals.float64ToBigfloat (var "d")))
                  (ExtractCore.float64 @@ Graph.emptyGraph @@ var "t")),
          -- Bytes
          Avro._Primitive_bytes>>: constant $
            var "simpleAdapter" @@ var "env0" @@ MetaTypes.binary
              @@ (lambda "_cx" $ lambda "jv" $
                cases JM._Value (var "jv") Nothing [
                  JM._Value_string>>: lambda "s" $ right (Core.termLiteral (Core.literalBinary (Literals.stringToBinary (var "s"))))])
              @@ (lambda "cx1" $ lambda "t" $
                Eithers.map (lambda "b" $ inject JM._Value JM._Value_string (Literals.binaryToString (var "b")))
                  (ExtractCore.binary @@ Graph.emptyGraph @@ var "t")),
          -- String
          Avro._Primitive_string>>: constant $
            var "simpleAdapter" @@ var "env0" @@ MetaTypes.string
              @@ (lambda "_cx" $ lambda "jv" $
                cases JM._Value (var "jv") Nothing [
                  JM._Value_string>>: lambda "s" $ right (MetaTerms.stringLift (var "s"))])
              @@ (lambda "cx1" $ lambda "t" $
                Eithers.map (lambda "s" $ inject JM._Value JM._Value_string (var "s"))
                  (ExtractCore.string @@ Graph.emptyGraph @@ var "t"))],

      -- SchemaReference
      Avro._Schema_reference>>: lambda "name_" $ lets [
        "qname">: parseAvroName @@ (project AvroEnv._AvroEnvironment AvroEnv._AvroEnvironment_namespace @@ var "env0") @@ var "name_"] $
        Maybes.maybe
          (err @@ var "cx" @@ Strings.cat2 (string "Referenced Avro type has not been defined: ") (showQname @@ var "qname"))
          (lambda "ad" $ right (pair (var "ad") (var "env0")))
          (getAvroHydraAdapter @@ var "qname" @@ var "env0"),

      -- SchemaUnion
      Avro._Schema_union>>: lambda "u" $ lets [
        "schemas">: unwrap Avro._Union @@ var "u",
        "isNull">: lambda "s" $
          cases Avro._Schema (var "s") (Just (boolean False)) [
            Avro._Schema_primitive>>: lambda "prim" $
              cases Avro._Primitive (var "prim") (Just (boolean False)) [
                Avro._Primitive_null>>: constant (boolean True)]],
        "hasNull">: Logic.not (Lists.null (Lists.filter (var "isNull") (var "schemas"))),
        "nonNulls">: Lists.filter (lambda "s" $ Logic.not (var "isNull" @@ var "s")) (var "schemas"),
        "forOptional">: lambda "s" $
          Eithers.bind (avroHydraAdapter @@ var "cx" @@ var "s" @@ var "env0") (lambda "adEnv" $ lets [
            "ad">: Pairs.first (var "adEnv"),
            "env1">: Pairs.second (var "adEnv")] $
            right (pair
              (Coders.adapter (Coders.adapterIsLossy (var "ad")) (var "schema")
                (MetaTypes.optional (Coders.adapterTarget (var "ad")))
                (Coders.coder
                  (lambda "cx1" $ lambda "v" $
                    cases JM._Value (var "v") (Just (
                      Eithers.map (lambda "t" $ Core.termMaybe (just (var "t")))
                        (Coders.coderEncode (Coders.adapterCoder (var "ad")) @@ var "cx1" @@ var "v"))) [
                      JM._Value_null>>: constant (right (Core.termMaybe nothing))])
                  (lambda "cx1" $ lambda "t" $
                    cases _Term (var "t") Nothing [
                      _Term_maybe>>: lambda "ot" $
                        Maybes.maybe
                          (right (injectUnit JM._Value JM._Value_null))
                          (lambda "term'" $ Coders.coderDecode (Coders.adapterCoder (var "ad")) @@ var "cx1" @@ var "term'")
                          (var "ot")])))
              (var "env1")))] $
        Logic.ifElse (Equality.gt (Lists.length (var "nonNulls")) (int32 1))
          (err @@ var "cx" @@ string "general-purpose unions are not yet supported")
          (Logic.ifElse (Lists.null (var "nonNulls"))
            (err @@ var "cx" @@ string "cannot generate the empty type")
            (Logic.ifElse (var "hasNull")
              (var "forOptional" @@ Lists.head (var "nonNulls"))
              (Eithers.bind (avroHydraAdapter @@ var "cx" @@ Lists.head (var "nonNulls") @@ var "env0") (lambda "adEnv" $ lets [
                "ad">: Pairs.first (var "adEnv"),
                "env1">: Pairs.second (var "adEnv")] $
                right (pair
                  (Coders.adapter (Coders.adapterIsLossy (var "ad")) (var "schema")
                    (Coders.adapterTarget (var "ad")) (Coders.adapterCoder (var "ad")))
                  (var "env1"))))))
      ]

prepareFields :: TTermDefinition (Context -> AvroEnv.AvroEnvironment -> [Avro.Field] -> Result (M.Map String (Avro.Field, AvroHydraAdapter), AvroEnv.AvroEnvironment))
prepareFields = define "prepareFields" $
  doc "Thread AvroEnvironment through preparing multiple fields" $
  lambda "cx" $ lambda "env" $ lambda "fields" $
    Lists.foldl
      (lambda "acc" $ lambda "f" $
        Eithers.bind (var "acc") (lambda "accPair" $ lets [
          "m">: Pairs.first (var "accPair"),
          "env1">: Pairs.second (var "accPair")] $
          Eithers.bind (prepareField @@ var "cx" @@ var "env1" @@ var "f") (lambda "result" $ lets [
            "kv">: Pairs.first (var "result"),
            "env2">: Pairs.second (var "result"),
            "k">: Pairs.first (var "kv"),
            "v">: Pairs.second (var "kv")] $
            right (pair (Maps.insert (var "k") (var "v") (var "m")) (var "env2")))))
      (right (pair Maps.empty (var "env")))
      (var "fields")

prepareField :: TTermDefinition (Context -> AvroEnv.AvroEnvironment -> Avro.Field -> Result ((String, (Avro.Field, AvroHydraAdapter)), AvroEnv.AvroEnvironment))
prepareField = define "prepareField" $
  doc "Prepare a single field, producing an adapter and updated environment" $
  lambda "cx" $ lambda "env" $ lambda "f" $ lets [
    "manns">: fieldAnnotationsToCore @@ var "f",
    "ann">: Logic.ifElse (Maps.null (var "manns")) nothing (just (var "manns"))] $
    Eithers.bind (foreignKeyE @@ var "cx" @@ var "f") (lambda "fk" $
    Eithers.bind
      (Maybes.maybe
        -- No foreign key: just use avroHydraAdapter directly
        (avroHydraAdapter @@ var "cx" @@ (project Avro._Field Avro._Field_type @@ var "f") @@ var "env")
        -- Foreign key present: build a specialized adapter
        (lambda "fkVal" $ lets [
          "fkName">: project AvroEnv._AvroForeignKey AvroEnv._AvroForeignKey_typeName @@ var "fkVal",
          "fkConstr">: project AvroEnv._AvroForeignKey AvroEnv._AvroForeignKey_constructor @@ var "fkVal"] $
          Eithers.bind (avroHydraAdapter @@ var "cx" @@ (project Avro._Field Avro._Field_type @@ var "f") @@ var "env") (lambda "adEnvPair" $ lets [
            "ad0">: Pairs.first (var "adEnvPair"),
            "env1">: Pairs.second (var "adEnvPair"),
            "elTyp">: Core.typeVariable (var "fkName"),
            -- encodeValue: encode a JSON value to a TermVariable via the adapter
            "encodeValue">: lambda "cx1" $ lambda "v" $
              Eithers.bind (Coders.coderEncode (Coders.adapterCoder (var "ad0")) @@ var "cx1" @@ var "v") (lambda "encoded" $
              Eithers.bind (termToStringE @@ var "cx1" @@ var "encoded") (lambda "s" $
                right (Core.termVariable (var "fkConstr" @@ var "s")))),
            -- decodeTerm: decode a TermVariable back via the adapter
            "decodeTerm">: lambda "cx1" $ lambda "t" $
              cases _Term (var "t") (Just (err @@ var "cx1" @@ string "expected variable")) [
                _Term_variable>>: lambda "name_" $
                  Eithers.bind (stringToTermE @@ var "cx1" @@ Coders.adapterTarget (var "ad0") @@ (unwrap _Name @@ var "name_")) (lambda "term_" $
                  Coders.coderDecode (Coders.adapterCoder (var "ad0")) @@ var "cx1" @@ var "term_")],
            -- forTypeAndCoder: build an adapter with a given type and coder
            "forTypeAndCoder">: lambda "env2" $ lambda "ad1" $ lambda "typ" $ lambda "cdr" $
              right (pair
                (Coders.adapter (Coders.adapterIsLossy (var "ad1")) (project Avro._Field Avro._Field_type @@ var "f") (var "typ") (var "cdr"))
                (var "env2"))] $
            -- Match on the deannotated target type for foreign key handling
            cases _Type (Strip.deannotateType @@ Coders.adapterTarget (var "ad0"))
              (Just (err @@ var "cx" @@ Strings.cat2 (string "unsupported type annotated as foreign key: ") (string "unknown"))) [
              _Type_maybe>>: lambda "innerTyp" $
                cases _Type (var "innerTyp") (Just (err @@ var "cx" @@ string "expected literal type inside optional foreign key")) [
                  _Type_literal>>: lambda "_" $
                    var "forTypeAndCoder" @@ var "env1" @@ var "ad0"
                      @@ (MetaTypes.optional (var "elTyp"))
                      @@ (Coders.coder
                        (lambda "cx2" $ lambda "json" $
                          Eithers.map (lambda "v'" $ Core.termMaybe (just (var "v'"))) (var "encodeValue" @@ var "cx2" @@ var "json"))
                        (var "decodeTerm"))],
              _Type_list>>: lambda "innerTyp2" $
                cases _Type (var "innerTyp2") (Just (err @@ var "cx" @@ string "expected literal type inside list foreign key")) [
                  _Type_literal>>: lambda "_" $
                    var "forTypeAndCoder" @@ var "env1" @@ var "ad0"
                      @@ (MetaTypes.list (var "elTyp"))
                      @@ (Coders.coder
                        (lambda "cx2" $ lambda "json" $
                          cases JM._Value (var "json") (Just (err @@ var "cx2" @@ string "Expected JSON array")) [
                            JM._Value_array>>: lambda "vals" $
                              Eithers.map (lambda "terms" $ Core.termList (var "terms"))
                                (Eithers.mapList (lambda "jv" $ var "encodeValue" @@ var "cx2" @@ var "jv") (var "vals"))])
                        (var "decodeTerm"))],
              _Type_literal>>: lambda "_" $
                var "forTypeAndCoder" @@ var "env1" @@ var "ad0"
                  @@ var "elTyp"
                  @@ (Coders.coder (var "encodeValue") (var "decodeTerm"))]))
        (var "fk"))
      (lambda "adEnv" $ lets [
        "ad">: Pairs.first (var "adEnv"),
        "env1">: Pairs.second (var "adEnv")] $
        right (pair
          (pair (project Avro._Field Avro._Field_name @@ var "f") (pair (var "f") (annotateAdapter @@ var "ann" @@ var "ad")))
          (var "env1"))))

annotateAdapter :: TTermDefinition (Maybe (M.Map Name Term) -> AvroHydraAdapter -> AvroHydraAdapter)
annotateAdapter = define "annotateAdapter" $
  doc "Annotate an adapter's target type with optional annotations" $
  lambda "ann" $ lambda "ad" $
    Maybes.maybe
      (var "ad")
      (lambda "n" $ Coders.adapterWithTarget (var "ad") (MetaTypes.annot (var "n") (Coders.adapterTarget (var "ad"))))
      (var "ann")

findAvroPrimaryKeyField :: TTermDefinition (Context -> AvroEnv.AvroQualifiedName -> [Avro.Field] -> Result (Maybe AvroEnv.AvroPrimaryKey))
findAvroPrimaryKeyField = define "findAvroPrimaryKeyField" $
  doc "Find the primary key field among a list of Avro fields" $
  lambda "cx" $ lambda "qname" $ lambda "avroFields" $ lets [
    "keys">: Maybes.cat (Lists.map (lambda "f" $ primaryKeyE @@ var "cx" @@ var "f") (var "avroFields"))] $
    Logic.ifElse (Lists.null (var "keys"))
      (right nothing)
      (Logic.ifElse (Equality.equal (Lists.length (var "keys")) (int32 1))
        (right (just (Lists.head (var "keys"))))
        (err @@ var "cx" @@ Strings.cat2 (string "multiple primary key fields for ") (showQname @@ var "qname")))

avroNameToHydraName :: TTermDefinition (AvroEnv.AvroQualifiedName -> Name)
avroNameToHydraName = define "avroNameToHydraName" $
  doc "Convert an Avro qualified name to a Hydra name" $
  lambda "qname" $ lets [
    "mns">: project AvroEnv._AvroQualifiedName AvroEnv._AvroQualifiedName_namespace @@ var "qname",
    "local">: project AvroEnv._AvroQualifiedName AvroEnv._AvroQualifiedName_name @@ var "qname"] $
    Names.unqualifyName @@ Packaging.qualifiedName
      (Maybes.map (lambda "s" $ wrap _Namespace (var "s")) (var "mns"))
      (var "local")

encodeAnnotationValue :: TTermDefinition (JM.Value -> Term)
encodeAnnotationValue = define "encodeAnnotationValue" $
  doc "Encode a JSON value as a Hydra term for annotation purposes" $
  lambda "v" $
    cases JM._Value (var "v") Nothing [
      JM._Value_array>>: lambda "vals" $
        Core.termList (Lists.map encodeAnnotationValue (var "vals")),
      JM._Value_boolean>>: lambda "b" $
        MetaTerms.booleanLift (var "b"),
      JM._Value_null>>: constant $
        MetaTerms.tuple ([] :: [TTerm Term]),
      JM._Value_number>>: lambda "d" $
        MetaTerms.bigfloatLift (var "d"),
      JM._Value_object>>: lambda "m" $
        MetaTerms.map (Maps.fromList (Lists.map
          (lambda "entry" $ lets [
            "k">: Pairs.first (var "entry"),
            "v'">: Pairs.second (var "entry")] $
            pair (MetaTerms.stringLift (var "k")) (encodeAnnotationValue @@ var "v'"))
          (Maps.toList (var "m")))),
      JM._Value_string>>: lambda "s" $
        MetaTerms.stringLift (var "s")]

fieldAnnotationsToCore :: TTermDefinition (Avro.Field -> M.Map Name Term)
fieldAnnotationsToCore = define "fieldAnnotationsToCore" $
  doc "Extract field annotations and convert them to core Name/Term pairs" $
  lambda "f" $
    Maps.fromList (Lists.map
      (lambda "entry" $ lets [
        "k">: Pairs.first (var "entry"),
        "v">: Pairs.second (var "entry")] $
        pair (Core.name (var "k")) (encodeAnnotationValue @@ var "v"))
      (Maps.toList (project Avro._Field Avro._Field_annotations @@ var "f")))

namedAnnotationsToCore :: TTermDefinition (Avro.Named -> M.Map Name Term)
namedAnnotationsToCore = define "namedAnnotationsToCore" $
  doc "Extract named type annotations and convert them to core Name/Term pairs" $
  lambda "n" $
    Maps.fromList (Lists.map
      (lambda "entry" $ lets [
        "k">: Pairs.first (var "entry"),
        "v">: Pairs.second (var "entry")] $
        pair (Core.name (var "k")) (encodeAnnotationValue @@ var "v"))
      (Maps.toList (project Avro._Named Avro._Named_annotations @@ var "n")))

getAvroHydraAdapter :: TTermDefinition (AvroEnv.AvroQualifiedName -> AvroEnv.AvroEnvironment -> Maybe AvroHydraAdapter)
getAvroHydraAdapter = define "getAvroHydraAdapter" $
  doc "Look up an adapter by qualified name in the environment" $
  lambda "qname" $ lambda "env" $
    Maps.lookup (var "qname") (project AvroEnv._AvroEnvironment AvroEnv._AvroEnvironment_namedAdapters @@ var "env")

foreignKeyE :: TTermDefinition (Context -> Avro.Field -> Result (Maybe AvroEnv.AvroForeignKey))
foreignKeyE = define "foreignKeyE" $
  doc "Extract a foreign key annotation from a field, if present" $
  lambda "cx" $ lambda "f" $
    Maybes.maybe
      (right nothing)
      (lambda "v" $
        Eithers.bind (expectObjectE @@ var "cx" @@ var "v") (lambda "m" $
        Eithers.bind (Eithers.map (lambda "s" $ Core.name (var "s")) (requireStringE @@ var "cx" @@ string "type" @@ var "m")) (lambda "tname" $
        Eithers.bind (optStringE @@ var "cx" @@ string "pattern" @@ var "m") (lambda "pattern_" $
          lets [
            "constr">: Maybes.maybe
              (lambda "s" $ Core.name (var "s"))
              (lambda "pat" $ patternToNameConstructor @@ var "pat")
              (var "pattern_")] $
            right (just $ record AvroEnv._AvroForeignKey [
              AvroEnv._AvroForeignKey_typeName>>: var "tname",
              AvroEnv._AvroForeignKey_constructor>>: var "constr"])))))
      (Maps.lookup (avro_foreignKey) (project Avro._Field Avro._Field_annotations @@ var "f"))

patternToNameConstructor :: TTermDefinition (String -> String -> Name)
patternToNameConstructor = define "patternToNameConstructor" $
  doc "Create a name constructor from a pattern string" $
  lambda "pat" $ lambda "s" $
    Core.name (Strings.intercalate (var "s") (Strings.splitOn (string "${}") (var "pat")))

primaryKeyE :: TTermDefinition (Context -> Avro.Field -> Maybe AvroEnv.AvroPrimaryKey)
primaryKeyE = define "primaryKeyE" $
  doc "Extract a primary key annotation from a field, if present" $
  lambda "cx" $ lambda "f" $
    Maybes.maybe
      nothing
      (lambda "v" $
        Eithers.either_
          (lambda "_" $ nothing)
          (lambda "s" $ just $ record AvroEnv._AvroPrimaryKey [
            AvroEnv._AvroPrimaryKey_fieldName>>: Core.name (project Avro._Field Avro._Field_name @@ var "f"),
            AvroEnv._AvroPrimaryKey_constructor>>: patternToNameConstructor @@ var "s"])
          (expectStringE @@ var "cx" @@ var "v"))
      (Maps.lookup (avro_primaryKey) (project Avro._Field Avro._Field_annotations @@ var "f"))

parseAvroName :: TTermDefinition (Maybe String -> String -> AvroEnv.AvroQualifiedName)
parseAvroName = define "parseAvroName" $
  doc "Parse a dotted Avro name into a qualified name" $
  lambda "mns" $ lambda "name_" $ lets [
    "parts">: Strings.splitOn (string ".") (var "name_"),
    "local">: Lists.last (var "parts")] $
    Logic.ifElse (Equality.equal (Lists.length (var "parts")) (int32 1))
      (record AvroEnv._AvroQualifiedName [
        AvroEnv._AvroQualifiedName_namespace>>: var "mns",
        AvroEnv._AvroQualifiedName_name>>: var "local"])
      (record AvroEnv._AvroQualifiedName [
        AvroEnv._AvroQualifiedName_namespace>>: just (Strings.intercalate (string ".") (Lists.init (var "parts"))),
        AvroEnv._AvroQualifiedName_name>>: var "local"])

putAvroHydraAdapter :: TTermDefinition (AvroEnv.AvroQualifiedName -> AvroHydraAdapter -> AvroEnv.AvroEnvironment -> AvroEnv.AvroEnvironment)
putAvroHydraAdapter = define "putAvroHydraAdapter" $
  doc "Store an adapter in the environment by qualified name" $
  lambda "qname" $ lambda "ad" $ lambda "env" $
    record AvroEnv._AvroEnvironment [
      AvroEnv._AvroEnvironment_namedAdapters>>:
        Maps.insert (var "qname") (var "ad")
          (project AvroEnv._AvroEnvironment AvroEnv._AvroEnvironment_namedAdapters @@ var "env"),
      AvroEnv._AvroEnvironment_namespace>>:
        project AvroEnv._AvroEnvironment AvroEnv._AvroEnvironment_namespace @@ var "env",
      AvroEnv._AvroEnvironment_elements>>:
        project AvroEnv._AvroEnvironment AvroEnv._AvroEnvironment_elements @@ var "env"]

rewriteAvroSchemaM :: TTermDefinition (((Avro.Schema -> Result Avro.Schema) -> Avro.Schema -> Result Avro.Schema) -> Avro.Schema -> Result Avro.Schema)
rewriteAvroSchemaM = define "rewriteAvroSchemaM" $
  doc "Recursively rewrite an Avro schema using a monadic transformation function" $
  lambda "f" $ lambda "schema" $ lets [
    "recurse">: rewriteAvroSchemaM @@ var "f",
    -- fsub: structural descent into schema subtypes
    "fsub">: lambda "s" $
      cases Avro._Schema (var "s") (Just (right (var "s"))) [
        Avro._Schema_array>>: lambda "arr" $
          Eithers.map
            (lambda "els'" $ inject Avro._Schema Avro._Schema_array (record Avro._Array [
              Avro._Array_items>>: var "els'"]))
            (var "recurse" @@ (project Avro._Array Avro._Array_items @@ var "arr")),
        Avro._Schema_map>>: lambda "mp" $
          Eithers.map
            (lambda "vs'" $ inject Avro._Schema Avro._Schema_map (record Avro._Map [
              Avro._Map_values>>: var "vs'"]))
            (var "recurse" @@ (project Avro._Map Avro._Map_values @@ var "mp")),
        Avro._Schema_named>>: lambda "n" $
          Eithers.map
            (lambda "nt'" $ inject Avro._Schema Avro._Schema_named (record Avro._Named [
              Avro._Named_name>>: project Avro._Named Avro._Named_name @@ var "n",
              Avro._Named_namespace>>: project Avro._Named Avro._Named_namespace @@ var "n",
              Avro._Named_aliases>>: project Avro._Named Avro._Named_aliases @@ var "n",
              Avro._Named_doc>>: project Avro._Named Avro._Named_doc @@ var "n",
              Avro._Named_type>>: var "nt'",
              Avro._Named_annotations>>: project Avro._Named Avro._Named_annotations @@ var "n"]))
            (cases Avro._NamedType (project Avro._Named Avro._Named_type @@ var "n")
              (Just (right (project Avro._Named Avro._Named_type @@ var "n"))) [
              Avro._NamedType_record>>: lambda "r" $
                Eithers.map
                  (lambda "fields'" $ inject Avro._NamedType Avro._NamedType_record (record Avro._Record [
                    Avro._Record_fields>>: var "fields'"]))
                  (Eithers.mapList
                    (lambda "fld" $
                      Eithers.map
                        (lambda "t'" $ record Avro._Field [
                          Avro._Field_name>>: project Avro._Field Avro._Field_name @@ var "fld",
                          Avro._Field_doc>>: project Avro._Field Avro._Field_doc @@ var "fld",
                          Avro._Field_type>>: var "t'",
                          Avro._Field_default>>: project Avro._Field Avro._Field_default @@ var "fld",
                          Avro._Field_order>>: project Avro._Field Avro._Field_order @@ var "fld",
                          Avro._Field_aliases>>: project Avro._Field Avro._Field_aliases @@ var "fld",
                          Avro._Field_annotations>>: project Avro._Field Avro._Field_annotations @@ var "fld"])
                        (var "recurse" @@ (project Avro._Field Avro._Field_type @@ var "fld")))
                    (project Avro._Record Avro._Record_fields @@ var "r"))]),
        Avro._Schema_union>>: lambda "u" $
          Eithers.map
            (lambda "schemas'" $ inject Avro._Schema Avro._Schema_union (wrap Avro._Union (var "schemas'")))
            (Eithers.mapList (lambda "us" $ var "recurse" @@ var "us")
              (unwrap Avro._Union @@ var "u"))]] $
    var "f" @@ var "fsub" @@ var "schema"

jsonToStringE :: TTermDefinition (Context -> JM.Value -> Result String)
jsonToStringE = define "jsonToStringE" $
  doc "Convert a JSON value to a string, supporting booleans, strings, and numbers" $
  lambda "cx" $ lambda "v" $
    cases JM._Value (var "v") (Just (unexpectedE @@ var "cx" @@ string "string, number, or boolean" @@ string "other")) [
      JM._Value_boolean>>: lambda "b" $
        right (Logic.ifElse (var "b") (string "true") (string "false")),
      JM._Value_string>>: lambda "s" $
        right (var "s"),
      JM._Value_number>>: lambda "d" $
        right (Literals.showBigfloat (var "d"))]

showQname :: TTermDefinition (AvroEnv.AvroQualifiedName -> String)
showQname = define "showQname" $
  doc "Convert an Avro qualified name to a display string" $
  lambda "qname" $ lets [
    "mns">: project AvroEnv._AvroQualifiedName AvroEnv._AvroQualifiedName_namespace @@ var "qname",
    "local">: project AvroEnv._AvroQualifiedName AvroEnv._AvroQualifiedName_name @@ var "qname"] $
    Strings.cat2
      (Maybes.maybe (string "") (lambda "ns" $ Strings.cat2 (var "ns") (string ".")) (var "mns"))
      (var "local")

stringToTermE :: TTermDefinition (Context -> Type -> String -> Result Term)
stringToTermE = define "stringToTermE" $
  doc "Parse a string into a term of the expected type" $
  lambda "cx" $ lambda "typ" $ lambda "s" $ lets [
    "readErr">: err @@ var "cx" @@ string "failed to read value",
    "readAndWrap">: lambda "reader" $ lambda "wrapper" $
      Maybes.maybe (var "readErr") (lambda "v" $ right (Core.termLiteral (var "wrapper" @@ var "v"))) (var "reader" @@ var "s")] $
    cases _Type (Strip.deannotateType @@ var "typ")
      (Just (unexpectedE @@ var "cx" @@ string "literal type" @@ string "other")) [
      _Type_literal>>: lambda "lt" $
        cases _LiteralType (var "lt")
          (Just (unexpectedE @@ var "cx" @@ string "literal type" @@ string "other literal type")) [
          _LiteralType_boolean>>: constant $
            var "readAndWrap" @@ (lambda "x" $ Literals.readBoolean (var "x")) @@ (lambda "b" $ Core.literalBoolean (var "b")),
          _LiteralType_integer>>: lambda "it" $
            cases _IntegerType (var "it") Nothing [
              _IntegerType_bigint>>: constant $
                var "readAndWrap" @@ (lambda "x" $ Literals.readBigint (var "x")) @@ (lambda "i" $ Core.literalInteger $ Core.integerValueBigint (var "i")),
              _IntegerType_int8>>: constant $
                var "readAndWrap" @@ (lambda "x" $ Literals.readInt8 (var "x")) @@ (lambda "i" $ Core.literalInteger $ Core.integerValueInt8 (var "i")),
              _IntegerType_int16>>: constant $
                var "readAndWrap" @@ (lambda "x" $ Literals.readInt16 (var "x")) @@ (lambda "i" $ Core.literalInteger $ Core.integerValueInt16 (var "i")),
              _IntegerType_int32>>: constant $
                var "readAndWrap" @@ (lambda "x" $ Literals.readInt32 (var "x")) @@ (lambda "i" $ Core.literalInteger $ Core.integerValueInt32 (var "i")),
              _IntegerType_int64>>: constant $
                var "readAndWrap" @@ (lambda "x" $ Literals.readInt64 (var "x")) @@ (lambda "i" $ Core.literalInteger $ Core.integerValueInt64 (var "i")),
              _IntegerType_uint8>>: constant $
                var "readAndWrap" @@ (lambda "x" $ Literals.readUint8 (var "x")) @@ (lambda "i" $ Core.literalInteger $ Core.integerValueUint8 (var "i")),
              _IntegerType_uint16>>: constant $
                var "readAndWrap" @@ (lambda "x" $ Literals.readUint16 (var "x")) @@ (lambda "i" $ Core.literalInteger $ Core.integerValueUint16 (var "i")),
              _IntegerType_uint32>>: constant $
                var "readAndWrap" @@ (lambda "x" $ Literals.readUint32 (var "x")) @@ (lambda "i" $ Core.literalInteger $ Core.integerValueUint32 (var "i")),
              _IntegerType_uint64>>: constant $
                var "readAndWrap" @@ (lambda "x" $ Literals.readUint64 (var "x")) @@ (lambda "i" $ Core.literalInteger $ Core.integerValueUint64 (var "i"))],
          _LiteralType_string>>: constant $
            right (Core.termLiteral $ Core.literalString (var "s"))]]

termToStringE :: TTermDefinition (Context -> Term -> Result String)
termToStringE = define "termToStringE" $
  doc "Convert a literal term to its string representation" $
  lambda "cx" $ lambda "term" $
    cases _Term (Strip.deannotateTerm @@ var "term")
      (Just (unexpectedE @@ var "cx" @@ string "literal value" @@ string "other")) [
      _Term_literal>>: lambda "l" $
        cases _Literal (var "l")
          (Just (unexpectedE @@ var "cx" @@ string "boolean, integer, or string" @@ string "other literal")) [
          _Literal_boolean>>: lambda "b" $
            right (Literals.showBoolean (var "b")),
          _Literal_integer>>: lambda "iv" $
            right (cases _IntegerValue (var "iv") Nothing [
              _IntegerValue_bigint>>: lambda "i" $ Literals.showBigint (var "i"),
              _IntegerValue_int8>>: lambda "i" $ Literals.showInt8 (var "i"),
              _IntegerValue_int16>>: lambda "i" $ Literals.showInt16 (var "i"),
              _IntegerValue_int32>>: lambda "i" $ Literals.showInt32 (var "i"),
              _IntegerValue_int64>>: lambda "i" $ Literals.showInt64 (var "i"),
              _IntegerValue_uint8>>: lambda "i" $ Literals.showUint8 (var "i"),
              _IntegerValue_uint16>>: lambda "i" $ Literals.showUint16 (var "i"),
              _IntegerValue_uint32>>: lambda "i" $ Literals.showUint32 (var "i"),
              _IntegerValue_uint64>>: lambda "i" $ Literals.showUint64 (var "i")]),
          _Literal_string>>: lambda "s" $
            right (var "s")],
      _Term_maybe>>: lambda "ot" $
        Maybes.maybe
          (unexpectedE @@ var "cx" @@ string "literal value" @@ string "Nothing")
          (lambda "term'" $ termToStringE @@ var "cx" @@ var "term'")
          (var "ot")]
