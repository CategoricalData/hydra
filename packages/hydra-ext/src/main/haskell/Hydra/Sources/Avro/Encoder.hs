module Hydra.Sources.Avro.Encoder where

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
type HydraAvroAdapter = Adapter Type Avro.Schema Term JM.Value


define :: String -> TTerm a -> TTermDefinition a
define = definitionInModule module_

avroSchemaPhantomNs :: Namespace
avroSchemaPhantomNs = Namespace "hydra.avro.schema"

jsonModelNs :: Namespace
jsonModelNs = Namespace "hydra.json.model"

avroEnvironmentNs :: Namespace
avroEnvironmentNs = Namespace "hydra.avro.environment"

ns :: Namespace
ns = Namespace "hydra.avro.encoder"

module_ :: Module
module_ = Module ns definitions
    [ExtractCore.ns, Strip.ns]
    (avroEnvironmentNs:AvroSchema.ns:jsonModelNs:KernelTypes.kernelTypesNamespaces) $
    Just "Hydra-to-Avro encoder: converts Hydra types and terms to Avro schemas and JSON values"
  where
    definitions = [
      toDefinition buildAvroField,
      toDefinition emptyEncodeEnvironment,
      toDefinition encodeType,
      toDefinition encodeTypeInner,
      toDefinition encodeTypeWithEnv,
      toDefinition enumAdapter,
      toDefinition err,
      toDefinition extractAnnotations,
      toDefinition floatAdapter,
      toDefinition floatValueToDouble,
      toDefinition foldFieldAdapters,
      toDefinition hydraAnnotationsToAvro,
      toDefinition hydraAvroAdapter,
      toDefinition hydraNameToAvroName,
      toDefinition integerAdapter,
      toDefinition integerValueToDouble,
      toDefinition literalAdapter,
      toDefinition localName,
      toDefinition nameNamespace,
      toDefinition namedTypeAdapter,
      toDefinition recordTermCoder,
      toDefinition termToJsonValue,
      toDefinition typeToName,
      toDefinition unionAsRecordAdapter]


buildAvroField :: TTermDefinition ((Name, HydraAvroAdapter) -> Avro.Field)
buildAvroField = define "buildAvroField" $
  doc "Build an Avro field from a name-adapter pair" $
  lambda "nameAd" $ lets [
    "name_">: Pairs.first (var "nameAd"),
    "ad">: Pairs.second (var "nameAd")] $
    record Avro._Field [
      Avro._Field_name>>: localName @@ var "name_",
      Avro._Field_doc>>: nothing,
      Avro._Field_type>>: Coders.adapterTarget (var "ad"),
      Avro._Field_default>>: nothing,
      Avro._Field_order>>: nothing,
      Avro._Field_aliases>>: nothing,
      Avro._Field_annotations>>: Maps.empty]

emptyEncodeEnvironment :: TTermDefinition (M.Map Name Type -> AvroEnv.EncodeEnvironment)
emptyEncodeEnvironment = define "emptyEncodeEnvironment" $
  doc "Create an empty encode environment with the given type map" $
  lambda "typeMap" $
    record AvroEnv._EncodeEnvironment [
      AvroEnv._EncodeEnvironment_typeMap>>: var "typeMap",
      AvroEnv._EncodeEnvironment_emitted>>: Maps.empty]

encodeType :: TTermDefinition (Context -> M.Map Name Type -> Name -> Result HydraAvroAdapter)
encodeType = define "encodeType" $
  doc "Encode a Hydra type to an Avro schema adapter, given the type map and a root name" $
  lambda "cx" $ lambda "typeMap" $ lambda "name_" $
    Eithers.map (lambda "adEnv" $ Pairs.first (var "adEnv"))
      (encodeTypeWithEnv @@ var "cx" @@ var "name_" @@ (emptyEncodeEnvironment @@ var "typeMap"))

encodeTypeInner :: TTermDefinition (Context -> Maybe Name -> Type -> AvroEnv.EncodeEnvironment -> Result (HydraAvroAdapter, AvroEnv.EncodeEnvironment))
encodeTypeInner = define "encodeTypeInner" $
  doc "Core encoding logic: recursively encode a Hydra type to an Avro schema" $
  lambda "cx" $ lambda "mName" $ lambda "typ" $ lambda "env" $ lets [
    "annResult">: extractAnnotations @@ var "typ",
    "annotations">: Pairs.first (var "annResult"),
    "bareType">: Pairs.second (var "annResult"),
    "simpleAdapter">: lambda "target" $ lambda "lossy" $ lambda "encode" $ lambda "decode" $
      right (pair
        (Coders.adapter (var "lossy") (var "typ") (var "target") (Coders.coder (var "encode") (var "decode")))
        (var "env"))] $
    cases _Type (var "bareType") (Just (err @@ var "cx" @@ string "unsupported Hydra type for Avro encoding")) [
      _Type_unit>>: constant $
        var "simpleAdapter"
          @@ inject Avro._Schema Avro._Schema_primitive (injectUnit Avro._Primitive Avro._Primitive_null)
          @@ boolean False
          @@ (lambda "_cx" $ lambda "_t" $ right (injectUnit JM._Value JM._Value_null))
          @@ (lambda "_cx" $ lambda "_j" $ right Core.termUnit),
      _Type_literal>>: lambda "lt" $
        Eithers.map (lambda "ad" $ pair (var "ad") (var "env"))
          (literalAdapter @@ var "cx" @@ var "typ" @@ var "lt"),
      _Type_list>>: lambda "innerType" $
        Eithers.bind (encodeTypeInner @@ var "cx" @@ (nothing :: TTerm (Maybe Name)) @@ var "innerType" @@ var "env") (lambda "adEnv" $ lets [
          "innerAd">: Pairs.first (var "adEnv"),
          "env1">: Pairs.second (var "adEnv")] $
          right (pair
            (Coders.adapter (Coders.adapterIsLossy (var "innerAd")) (var "typ")
              (inject Avro._Schema Avro._Schema_array (record Avro._Array [
                Avro._Array_items>>: Coders.adapterTarget (var "innerAd")]))
              (Coders.coder
                (lambda "cx1" $ lambda "t" $
                  cases _Term (var "t") Nothing [
                    _Term_list>>: lambda "elements" $
                      Eithers.map (lambda "jvs" $ inject JM._Value JM._Value_array (var "jvs"))
                        (Eithers.mapList (lambda "el" $ Coders.coderEncode (Coders.adapterCoder (var "innerAd")) @@ var "cx1" @@ var "el") (var "elements"))])
                (lambda "cx1" $ lambda "j" $
                  cases JM._Value (var "j") Nothing [
                    JM._Value_array>>: lambda "elements" $
                      Eithers.map (lambda "ts" $ Core.termList (var "ts"))
                        (Eithers.mapList (lambda "el" $ Coders.coderDecode (Coders.adapterCoder (var "innerAd")) @@ var "cx1" @@ var "el") (var "elements"))])))
            (var "env1"))),
      _Type_map>>: lambda "mt" $ lets [
        "keyType">: project _MapType _MapType_keys @@ var "mt",
        "valType">: project _MapType _MapType_values @@ var "mt"] $
        cases _Type (Strip.deannotateType @@ var "keyType")
          (Just (err @@ var "cx" @@ string "Avro maps require string keys")) [
          _Type_literal>>: lambda "lt" $
            cases _LiteralType (var "lt") (Just (err @@ var "cx" @@ string "Avro maps require string keys")) [
              _LiteralType_string>>: constant $
                Eithers.bind (encodeTypeInner @@ var "cx" @@ (nothing :: TTerm (Maybe Name)) @@ var "valType" @@ var "env") (lambda "adEnv" $ lets [
                  "valAd">: Pairs.first (var "adEnv"),
                  "env1">: Pairs.second (var "adEnv")] $
                  right (pair
                    (Coders.adapter (Coders.adapterIsLossy (var "valAd")) (var "typ")
                      (inject Avro._Schema Avro._Schema_map (record Avro._Map [
                        Avro._Map_values>>: Coders.adapterTarget (var "valAd")]))
                      (Coders.coder
                        (lambda "cx1" $ lambda "t" $
                          cases _Term (var "t") Nothing [
                            _Term_map>>: lambda "entries" $ lets [
                              "encodeEntry">: lambda "entry" $ lets [
                                "k">: Pairs.first (var "entry"),
                                "v">: Pairs.second (var "entry")] $
                                Eithers.bind (ExtractCore.string @@ Graph.emptyGraph @@ var "k") (lambda "kStr" $
                                Eithers.map (lambda "vJson" $ pair (var "kStr") (var "vJson"))
                                  (Coders.coderEncode (Coders.adapterCoder (var "valAd")) @@ var "cx1" @@ var "v"))] $
                              Eithers.map (lambda "pairs" $ inject JM._Value JM._Value_object (Maps.fromList (var "pairs")))
                                (Eithers.mapList (var "encodeEntry") (Maps.toList (var "entries")))])
                        (lambda "cx1" $ lambda "j" $
                          cases JM._Value (var "j") Nothing [
                            JM._Value_object>>: lambda "m" $ lets [
                              "decodeEntry">: lambda "entry" $ lets [
                                "k">: Pairs.first (var "entry"),
                                "v">: Pairs.second (var "entry")] $
                                Eithers.map (lambda "vTerm" $ pair (MetaTerms.stringLift (var "k")) (var "vTerm"))
                                  (Coders.coderDecode (Coders.adapterCoder (var "valAd")) @@ var "cx1" @@ var "v")] $
                              Eithers.map (lambda "pairs" $ Core.termMap (Maps.fromList (var "pairs")))
                                (Eithers.mapList (var "decodeEntry") (Maps.toList (var "m")))])))
                    (var "env1")))]],
      _Type_record>>: lambda "fieldTypes" $
        namedTypeAdapter @@ var "cx" @@ var "typ" @@ var "mName" @@ var "annotations" @@ var "fieldTypes" @@ var "env"
          @@ (lambda "avroFields" $ inject Avro._NamedType Avro._NamedType_record (record Avro._Record [
            Avro._Record_fields>>: var "avroFields"]))
          @@ recordTermCoder,
      _Type_union>>: lambda "fieldTypes" $ lets [
        "allUnit">: Lists.foldl
          (lambda "b" $ lambda "ft" $ Logic.and (var "b")
            (cases _Type (project _FieldType _FieldType_type @@ var "ft") (Just (boolean False)) [
              _Type_unit>>: constant (boolean True)]))
          (boolean True) (var "fieldTypes")] $
        Logic.ifElse (var "allUnit")
          (enumAdapter @@ var "cx" @@ var "typ" @@ var "mName" @@ var "annotations" @@ var "fieldTypes" @@ var "env")
          (unionAsRecordAdapter @@ var "cx" @@ var "typ" @@ var "mName" @@ var "annotations" @@ var "fieldTypes" @@ var "env"),
      _Type_maybe>>: lambda "innerType" $
        Eithers.bind (encodeTypeInner @@ var "cx" @@ (nothing :: TTerm (Maybe Name)) @@ var "innerType" @@ var "env") (lambda "adEnv" $ lets [
          "innerAd">: Pairs.first (var "adEnv"),
          "env1">: Pairs.second (var "adEnv")] $
          right (pair
            (Coders.adapter (Coders.adapterIsLossy (var "innerAd")) (var "typ")
              (inject Avro._Schema Avro._Schema_union (wrap Avro._Union (list [
                inject Avro._Schema Avro._Schema_primitive (injectUnit Avro._Primitive Avro._Primitive_null),
                Coders.adapterTarget (var "innerAd")])))
              (Coders.coder
                (lambda "cx1" $ lambda "t" $
                  cases _Term (var "t") Nothing [
                    _Term_maybe>>: lambda "ot" $
                      Maybes.maybe
                        (right (injectUnit JM._Value JM._Value_null))
                        (lambda "inner" $ Coders.coderEncode (Coders.adapterCoder (var "innerAd")) @@ var "cx1" @@ var "inner")
                        (var "ot")])
                (lambda "cx1" $ lambda "j" $
                  cases JM._Value (var "j") (Just (
                    Eithers.map (lambda "t" $ Core.termMaybe (just (var "t")))
                      (Coders.coderDecode (Coders.adapterCoder (var "innerAd")) @@ var "cx1" @@ var "j"))) [
                    JM._Value_null>>: constant (right (Core.termMaybe nothing))])))
            (var "env1"))),
      _Type_wrap>>: lambda "inner" $
        encodeTypeInner @@ var "cx" @@ var "mName" @@ var "inner" @@ var "env",
      _Type_variable>>: lambda "name_" $
        Maybes.maybe
          (Maybes.maybe
            (err @@ var "cx" @@ Strings.cat2 (string "referenced type not found: ") (unwrap _Name @@ var "name_"))
            (lambda "refType" $ encodeTypeInner @@ var "cx" @@ just (var "name_") @@ var "refType" @@ var "env")
            (Maps.lookup (var "name_") (project AvroEnv._EncodeEnvironment AvroEnv._EncodeEnvironment_typeMap @@ var "env")))
          (lambda "existingAd" $
            right (pair
              (Coders.adapterWithTarget (var "existingAd") (inject Avro._Schema Avro._Schema_reference (localName @@ var "name_")))
              (var "env")))
          (Maps.lookup (var "name_") (project AvroEnv._EncodeEnvironment AvroEnv._EncodeEnvironment_emitted @@ var "env"))
    ]

encodeTypeWithEnv :: TTermDefinition (Context -> Name -> AvroEnv.EncodeEnvironment -> Result (HydraAvroAdapter, AvroEnv.EncodeEnvironment))
encodeTypeWithEnv = define "encodeTypeWithEnv" $
  doc "Encode with full environment threading. Returns the adapter and updated environment" $
  lambda "cx" $ lambda "name_" $ lambda "env" $
    Maybes.maybe
      (err @@ var "cx" @@ Strings.cat2 (string "type not found in type map: ") (Literals.showString (unwrap _Name @@ var "name_")))
      (lambda "typ" $ encodeTypeInner @@ var "cx" @@ just (var "name_") @@ var "typ" @@ var "env")
      (Maps.lookup (var "name_") (project AvroEnv._EncodeEnvironment AvroEnv._EncodeEnvironment_typeMap @@ var "env"))

enumAdapter :: TTermDefinition (Context -> Type -> Maybe Name -> M.Map Name Term -> [FieldType] -> AvroEnv.EncodeEnvironment -> Result (HydraAvroAdapter, AvroEnv.EncodeEnvironment))
enumAdapter = define "enumAdapter" $
  doc "Adapter for all-unit union types (enums)" $
  lambda "cx" $ lambda "typ" $ lambda "mName" $ lambda "annotations" $ lambda "fieldTypes" $ lambda "env0" $ lets [
    "symbols">: Lists.map (lambda "ft" $ localName @@ (project _FieldType _FieldType_name @@ var "ft")) (var "fieldTypes"),
    "typeName">: Maybes.fromMaybe (typeToName @@ var "typ") (var "mName"),
    "avroAnnotations">: hydraAnnotationsToAvro @@ var "annotations",
    "avroSchema">: inject Avro._Schema Avro._Schema_named (record Avro._Named [
      Avro._Named_name>>: localName @@ var "typeName",
      Avro._Named_namespace>>: nameNamespace @@ var "typeName",
      Avro._Named_aliases>>: nothing,
      Avro._Named_doc>>: nothing,
      Avro._Named_type>>: inject Avro._NamedType Avro._NamedType_enum (record Avro._Enum [
        Avro._Enum_symbols>>: var "symbols",
        Avro._Enum_default>>: nothing]),
      Avro._Named_annotations>>: var "avroAnnotations"]),
    "adapter_">: Coders.adapter (boolean False) (var "typ") (var "avroSchema")
      (Coders.coder
        (lambda "cx1" $ lambda "t" $
          cases _Term (var "t") (Just (err @@ var "cx1" @@ string "expected union term for enum")) [
            _Term_inject>>: lambda "inj" $ lets [
              "fname">: project _Injection _Injection_field @@ var "inj"] $
              right (inject JM._Value JM._Value_string (localName @@ (project _Field _Field_name @@ var "fname")))])
        (lambda "_cx" $ lambda "j" $
          cases JM._Value (var "j") Nothing [
            JM._Value_string>>: lambda "s" $
              right (Core.termInject (Core.injection (var "typeName") (Core.field (Core.name (var "s")) Core.termUnit)))])),
    "env1">: record AvroEnv._EncodeEnvironment [
      AvroEnv._EncodeEnvironment_typeMap>>: project AvroEnv._EncodeEnvironment AvroEnv._EncodeEnvironment_typeMap @@ var "env0",
      AvroEnv._EncodeEnvironment_emitted>>: Maps.insert (var "typeName") (var "adapter_")
        (project AvroEnv._EncodeEnvironment AvroEnv._EncodeEnvironment_emitted @@ var "env0")]] $
    right (pair (var "adapter_") (var "env1"))

err :: TTermDefinition (Context -> String -> Result a)
err = define "err" $
  doc "Construct an error result with a message in context" $
  lambda "cx" $ lambda "msg" $
    Ctx.failInContext (Error.errorOther $ Error.otherError (var "msg")) (var "cx")

extractAnnotations :: TTermDefinition (Type -> (M.Map Name Term, Type))
extractAnnotations = define "extractAnnotations" $
  doc "Extract annotations from a potentially annotated type" $
  lambda "typ" $
    cases _Type (var "typ") (Just (pair Maps.empty (var "typ"))) [
      _Type_annotated>>: lambda "at" $ lets [
        "inner">: project _AnnotatedType _AnnotatedType_body @@ var "at",
        "anns">: project _AnnotatedType _AnnotatedType_annotation @@ var "at",
        "innerResult">: extractAnnotations @@ var "inner",
        "innerAnns">: Pairs.first (var "innerResult"),
        "bareType">: Pairs.second (var "innerResult")] $
        pair (Maps.union (var "anns") (var "innerAnns")) (var "bareType")]

floatAdapter :: TTermDefinition (Context -> Type -> FloatType -> Result HydraAvroAdapter)
floatAdapter = define "floatAdapter" $
  doc "Create an adapter for float types" $
  lambda "cx" $ lambda "typ" $ lambda "ft" $ lets [
    "simple">: lambda "target" $ lambda "lossy" $ lambda "encode" $ lambda "decode" $
      right (Coders.adapter (var "lossy") (var "typ") (var "target") (Coders.coder (var "encode") (var "decode")))] $
    cases _FloatType (var "ft") (Just $
      var "simple"
        @@ inject Avro._Schema Avro._Schema_primitive (injectUnit Avro._Primitive Avro._Primitive_double)
        @@ boolean True
        @@ (lambda "_cx" $ lambda "t" $
          cases _Term (var "t") Nothing [
            _Term_literal>>: lambda "lit" $
              cases _Literal (var "lit") Nothing [
                _Literal_float>>: lambda "fv" $ right (inject JM._Value JM._Value_number (floatValueToDouble @@ var "fv"))]])
        @@ (lambda "_cx" $ lambda "j" $
          cases JM._Value (var "j") Nothing [
            JM._Value_number>>: lambda "d" $ right (Core.termLiteral (Core.literalFloat (Core.floatValueFloat64 (Literals.bigfloatToFloat64 (var "d")))))])) [
    _FloatType_float32>>: constant $
      var "simple"
        @@ inject Avro._Schema Avro._Schema_primitive (injectUnit Avro._Primitive Avro._Primitive_float)
        @@ boolean False
        @@ (lambda "_cx" $ lambda "t" $
          Eithers.map (lambda "f" $ inject JM._Value JM._Value_number (Literals.float32ToBigfloat (var "f")))
            (ExtractCore.float32 @@ Graph.emptyGraph @@ var "t"))
        @@ (lambda "_cx" $ lambda "j" $
          cases JM._Value (var "j") Nothing [
            JM._Value_number>>: lambda "d" $ right (Core.termLiteral (Core.literalFloat (Core.floatValueFloat32 (Literals.bigfloatToFloat32 (var "d")))))]),
    _FloatType_float64>>: constant $
      var "simple"
        @@ inject Avro._Schema Avro._Schema_primitive (injectUnit Avro._Primitive Avro._Primitive_double)
        @@ boolean False
        @@ (lambda "_cx" $ lambda "t" $
          Eithers.map (lambda "d" $ inject JM._Value JM._Value_number (Literals.float64ToBigfloat (var "d")))
            (ExtractCore.float64 @@ Graph.emptyGraph @@ var "t"))
        @@ (lambda "_cx" $ lambda "j" $
          cases JM._Value (var "j") Nothing [
            JM._Value_number>>: lambda "d" $ right (Core.termLiteral (Core.literalFloat (Core.floatValueFloat64 (Literals.bigfloatToFloat64 (var "d")))))])]

floatValueToDouble :: TTermDefinition (FloatValue -> Double)
floatValueToDouble = define "floatValueToDouble" $
  doc "Convert any float value to a double (bigfloat)" $
  lambda "fv" $
    cases _FloatValue (var "fv") Nothing [
      _FloatValue_bigfloat>>: lambda "d" $ var "d",
      _FloatValue_float32>>: lambda "f" $ Literals.float32ToBigfloat (var "f"),
      _FloatValue_float64>>: lambda "d" $ Literals.float64ToBigfloat (var "d")]

foldFieldAdapters :: TTermDefinition (Context -> [FieldType] -> AvroEnv.EncodeEnvironment -> Result ([(Name, HydraAvroAdapter)], AvroEnv.EncodeEnvironment))
foldFieldAdapters = define "foldFieldAdapters" $
  doc "Fold over field types, building adapters and threading the environment" $
  lambda "cx" $ lambda "fieldTypes" $ lambda "env0" $
    Lists.foldl
      (lambda "acc" $ lambda "ft" $
        Eithers.bind (var "acc") (lambda "accPair" $ lets [
          "soFar">: Pairs.first (var "accPair"),
          "env1">: Pairs.second (var "accPair"),
          "fname">: project _FieldType _FieldType_name @@ var "ft",
          "ftype">: project _FieldType _FieldType_type @@ var "ft"] $
          Eithers.bind (encodeTypeInner @@ var "cx" @@ (nothing :: TTerm (Maybe Name)) @@ var "ftype" @@ var "env1") (lambda "adEnv" $ lets [
            "ad">: Pairs.first (var "adEnv"),
            "env2">: Pairs.second (var "adEnv")] $
            right (pair (Lists.concat2 (var "soFar") (list [pair (var "fname") (var "ad")])) (var "env2")))))
      (right (pair (Phantoms.list emptyFieldAdapters) (var "env0")))
      (var "fieldTypes")
  where
    emptyFieldAdapters :: [TTerm (Name, HydraAvroAdapter)]
    emptyFieldAdapters = []

hydraAnnotationsToAvro :: TTermDefinition (M.Map Name Term -> M.Map String JM.Value)
hydraAnnotationsToAvro = define "hydraAnnotationsToAvro" $
  doc "Convert Hydra annotations to Avro annotation map" $
  lambda "anns" $
    Maps.fromList (Lists.map
      (lambda "entry" $ lets [
        "k">: Pairs.first (var "entry"),
        "v">: Pairs.second (var "entry")] $
        pair (unwrap _Name @@ var "k") (termToJsonValue @@ var "v"))
      (Maps.toList (var "anns")))

hydraAvroAdapter :: TTermDefinition (Context -> M.Map Name Type -> Type -> Result HydraAvroAdapter)
hydraAvroAdapter = define "hydraAvroAdapter" $
  doc "Encode a single type without a type map (for simple/anonymous types)" $
  lambda "cx" $ lambda "typeMap" $ lambda "typ" $
    Eithers.map (lambda "adEnv" $ Pairs.first (var "adEnv"))
      (encodeTypeInner @@ var "cx" @@ (nothing :: TTerm (Maybe Name)) @@ var "typ" @@ (emptyEncodeEnvironment @@ var "typeMap"))

hydraNameToAvroName :: TTermDefinition (Name -> (String, Maybe String))
hydraNameToAvroName = define "hydraNameToAvroName" $
  doc "Convert a Hydra Name to an Avro qualified name (local name, optional namespace)" $
  lambda "name_" $
    pair (localName @@ var "name_") (nameNamespace @@ var "name_")

integerAdapter :: TTermDefinition (Context -> Type -> IntegerType -> Result HydraAvroAdapter)
integerAdapter = define "integerAdapter" $
  doc "Create an adapter for integer types" $
  lambda "cx" $ lambda "typ" $ lambda "it" $ lets [
    "simple">: lambda "target" $ lambda "lossy" $ lambda "encode" $ lambda "decode" $
      right (Coders.adapter (var "lossy") (var "typ") (var "target") (Coders.coder (var "encode") (var "decode")))] $
    cases _IntegerType (var "it") (Just $
      var "simple"
        @@ inject Avro._Schema Avro._Schema_primitive (injectUnit Avro._Primitive Avro._Primitive_long)
        @@ boolean True
        @@ (lambda "_cx" $ lambda "t" $
          cases _Term (var "t") Nothing [
            _Term_literal>>: lambda "lit" $
              cases _Literal (var "lit") Nothing [
                _Literal_integer>>: lambda "iv" $ right (inject JM._Value JM._Value_number (integerValueToDouble @@ var "iv"))]])
        @@ (lambda "_cx" $ lambda "j" $
          cases JM._Value (var "j") Nothing [
            JM._Value_number>>: lambda "d" $ right (Core.termLiteral (Core.literalInteger (Core.integerValueInt64 (Literals.bigintToInt64 (Literals.bigfloatToBigint (Literals.float64ToBigfloat (Math.truncate (Literals.bigfloatToFloat64 (var "d")))))))))])) [
    _IntegerType_int32>>: constant $
      var "simple"
        @@ inject Avro._Schema Avro._Schema_primitive (injectUnit Avro._Primitive Avro._Primitive_int)
        @@ boolean False
        @@ (lambda "_cx" $ lambda "t" $
          Eithers.map (lambda "i" $ inject JM._Value JM._Value_number (Literals.bigintToBigfloat (Literals.int32ToBigint (var "i"))))
            (ExtractCore.int32 @@ Graph.emptyGraph @@ var "t"))
        @@ (lambda "_cx" $ lambda "j" $
          cases JM._Value (var "j") Nothing [
            JM._Value_number>>: lambda "d" $ right (Core.termLiteral (Core.literalInteger (Core.integerValueInt32 (Literals.bigintToInt32 (Literals.bigfloatToBigint (Literals.float64ToBigfloat (Math.truncate (Literals.bigfloatToFloat64 (var "d")))))))))]),
    _IntegerType_int64>>: constant $
      var "simple"
        @@ inject Avro._Schema Avro._Schema_primitive (injectUnit Avro._Primitive Avro._Primitive_long)
        @@ boolean False
        @@ (lambda "_cx" $ lambda "t" $
          Eithers.map (lambda "i" $ inject JM._Value JM._Value_number (Literals.bigintToBigfloat (Literals.int64ToBigint (var "i"))))
            (ExtractCore.int64 @@ Graph.emptyGraph @@ var "t"))
        @@ (lambda "_cx" $ lambda "j" $
          cases JM._Value (var "j") Nothing [
            JM._Value_number>>: lambda "d" $ right (Core.termLiteral (Core.literalInteger (Core.integerValueInt64 (Literals.bigintToInt64 (Literals.bigfloatToBigint (Literals.float64ToBigfloat (Math.truncate (Literals.bigfloatToFloat64 (var "d")))))))))])]

integerValueToDouble :: TTermDefinition (IntegerValue -> Double)
integerValueToDouble = define "integerValueToDouble" $
  doc "Convert any integer value to a double (bigfloat)" $
  lambda "iv" $
    cases _IntegerValue (var "iv") Nothing [
      _IntegerValue_bigint>>: lambda "i" $ Literals.bigintToBigfloat (var "i"),
      _IntegerValue_int8>>: lambda "i" $ Literals.bigintToBigfloat (Literals.int8ToBigint (var "i")),
      _IntegerValue_int16>>: lambda "i" $ Literals.bigintToBigfloat (Literals.int16ToBigint (var "i")),
      _IntegerValue_int32>>: lambda "i" $ Literals.bigintToBigfloat (Literals.int32ToBigint (var "i")),
      _IntegerValue_int64>>: lambda "i" $ Literals.bigintToBigfloat (Literals.int64ToBigint (var "i")),
      _IntegerValue_uint8>>: lambda "i" $ Literals.bigintToBigfloat (Literals.uint8ToBigint (var "i")),
      _IntegerValue_uint16>>: lambda "i" $ Literals.bigintToBigfloat (Literals.uint16ToBigint (var "i")),
      _IntegerValue_uint32>>: lambda "i" $ Literals.bigintToBigfloat (Literals.uint32ToBigint (var "i")),
      _IntegerValue_uint64>>: lambda "i" $ Literals.bigintToBigfloat (Literals.uint64ToBigint (var "i"))]

literalAdapter :: TTermDefinition (Context -> Type -> LiteralType -> Result HydraAvroAdapter)
literalAdapter = define "literalAdapter" $
  doc "Create an adapter for literal types" $
  lambda "cx" $ lambda "typ" $ lambda "lt" $ lets [
    "simple">: lambda "target" $ lambda "lossy" $ lambda "encode" $ lambda "decode" $
      right (Coders.adapter (var "lossy") (var "typ") (var "target") (Coders.coder (var "encode") (var "decode")))] $
    cases _LiteralType (var "lt") Nothing [
      _LiteralType_boolean>>: constant $
        var "simple"
          @@ inject Avro._Schema Avro._Schema_primitive (injectUnit Avro._Primitive Avro._Primitive_boolean)
          @@ boolean False
          @@ (lambda "_cx" $ lambda "t" $
            cases _Term (var "t") Nothing [
              _Term_literal>>: lambda "lit" $
                cases _Literal (var "lit") Nothing [
                  _Literal_boolean>>: lambda "b" $ right (inject JM._Value JM._Value_boolean (var "b"))]])
          @@ (lambda "_cx" $ lambda "j" $
            cases JM._Value (var "j") Nothing [
              JM._Value_boolean>>: lambda "b" $ right (Core.termLiteral (Core.literalBoolean (var "b")))]),
      _LiteralType_string>>: constant $
        var "simple"
          @@ inject Avro._Schema Avro._Schema_primitive (injectUnit Avro._Primitive Avro._Primitive_string)
          @@ boolean False
          @@ (lambda "_cx" $ lambda "t" $
            cases _Term (var "t") Nothing [
              _Term_literal>>: lambda "lit" $
                cases _Literal (var "lit") Nothing [
                  _Literal_string>>: lambda "s" $ right (inject JM._Value JM._Value_string (var "s"))]])
          @@ (lambda "_cx" $ lambda "j" $
            cases JM._Value (var "j") Nothing [
              JM._Value_string>>: lambda "s" $ right (Core.termLiteral (Core.literalString (var "s")))]),
      _LiteralType_binary>>: constant $
        var "simple"
          @@ inject Avro._Schema Avro._Schema_primitive (injectUnit Avro._Primitive Avro._Primitive_bytes)
          @@ boolean False
          @@ (lambda "_cx" $ lambda "t" $
            cases _Term (var "t") Nothing [
              _Term_literal>>: lambda "lit" $
                cases _Literal (var "lit") Nothing [
                  _Literal_binary>>: lambda "b" $ right (inject JM._Value JM._Value_string (Literals.binaryToString (var "b")))]])
          @@ (lambda "_cx" $ lambda "j" $
            cases JM._Value (var "j") Nothing [
              JM._Value_string>>: lambda "s" $ right (Core.termLiteral (Core.literalBinary (Literals.stringToBinary (var "s"))))]),
      _LiteralType_integer>>: lambda "it" $ integerAdapter @@ var "cx" @@ var "typ" @@ var "it",
      _LiteralType_float>>: lambda "ft" $ floatAdapter @@ var "cx" @@ var "typ" @@ var "ft"]

localName :: TTermDefinition (Name -> String)
localName = define "localName" $
  doc "Extract the local part of a qualified name" $
  lambda "name_" $ lets [
    "s">: unwrap _Name @@ var "name_",
    "parts">: Strings.splitOn (string ".") (var "s")] $
    Maybes.fromMaybe (var "s") (Lists.maybeLast (var "parts"))

nameNamespace :: TTermDefinition (Name -> Maybe String)
nameNamespace = define "nameNamespace" $
  doc "Extract the namespace from a qualified name, if any" $
  lambda "name_" $ lets [
    "s">: unwrap _Name @@ var "name_",
    "parts">: Strings.splitOn (string ".") (var "s")] $
    Logic.ifElse (Equality.equal (Lists.length (var "parts")) (int32 1))
      nothing
      (Maybes.map (lambda "ps" $ Strings.intercalate (string ".") (var "ps")) (Lists.maybeInit (var "parts")))

namedTypeAdapter :: TTermDefinition (Context -> Type -> Maybe Name -> M.Map Name Term -> [FieldType] -> AvroEnv.EncodeEnvironment
  -> ([Avro.Field] -> Avro.NamedType)
  -> (Context -> Name -> [(Name, HydraAvroAdapter)]
      -> (Context -> Term -> Result JM.Value, Context -> JM.Value -> Result Term))
  -> Result (HydraAvroAdapter, AvroEnv.EncodeEnvironment))
namedTypeAdapter = define "namedTypeAdapter" $
  doc "Build a named type adapter (shared between record and union-as-record)" $
  lambda "cx" $ lambda "typ" $ lambda "mName" $ lambda "annotations" $ lambda "fieldTypes" $ lambda "env0"
    $ lambda "mkNamedType" $ lambda "mkCoder" $ lets [
    "typeName">: Maybes.fromMaybe (typeToName @@ var "typ") (var "mName")] $
    Maybes.maybe
      (Eithers.bind (foldFieldAdapters @@ var "cx" @@ var "fieldTypes" @@ var "env0") (lambda "faResult" $ lets [
        "fieldAdapters">: Pairs.first (var "faResult"),
        "env1">: Pairs.second (var "faResult"),
        "avroFields">: Lists.map buildAvroField (var "fieldAdapters"),
        "avroAnnotations">: hydraAnnotationsToAvro @@ var "annotations",
        "avroSchema">: inject Avro._Schema Avro._Schema_named (record Avro._Named [
          Avro._Named_name>>: localName @@ var "typeName",
          Avro._Named_namespace>>: nameNamespace @@ var "typeName",
          Avro._Named_aliases>>: nothing,
          Avro._Named_doc>>: nothing,
          Avro._Named_type>>: var "mkNamedType" @@ var "avroFields",
          Avro._Named_annotations>>: var "avroAnnotations"]),
        "lossy">: Lists.foldl (lambda "b" $ lambda "fa" $ Logic.or (var "b") (Coders.adapterIsLossy (Pairs.second (var "fa"))))
          (boolean False) (var "fieldAdapters"),
        "coderPair">: var "mkCoder" @@ var "cx" @@ var "typeName" @@ var "fieldAdapters",
        "encodeFn">: Pairs.first (var "coderPair"),
        "decodeFn">: Pairs.second (var "coderPair"),
        "adapter_">: Coders.adapter (var "lossy") (var "typ") (var "avroSchema") (Coders.coder (var "encodeFn") (var "decodeFn")),
        "env2">: record AvroEnv._EncodeEnvironment [
          AvroEnv._EncodeEnvironment_typeMap>>: project AvroEnv._EncodeEnvironment AvroEnv._EncodeEnvironment_typeMap @@ var "env1",
          AvroEnv._EncodeEnvironment_emitted>>: Maps.insert (var "typeName") (var "adapter_")
            (project AvroEnv._EncodeEnvironment AvroEnv._EncodeEnvironment_emitted @@ var "env1")]] $
        right (pair (var "adapter_") (var "env2"))))
      (lambda "existingAd" $ right (pair (var "existingAd") (var "env0")))
      (Maps.lookup (var "typeName") (project AvroEnv._EncodeEnvironment AvroEnv._EncodeEnvironment_emitted @@ var "env0"))

recordTermCoder :: TTermDefinition (Context -> Name -> [(Name, HydraAvroAdapter)]
  -> (Context -> Term -> Result JM.Value, Context -> JM.Value -> Result Term))
recordTermCoder = define "recordTermCoder" $
  doc "Build a record term coder from field adapters" $
  lambda "cx" $ lambda "typeName" $ lambda "fieldAdapters" $ lets [
    "encode">: lambda "cx1" $ lambda "term" $
      cases _Term (var "term") (Just (err @@ var "cx" @@ string "expected record term")) [
        _Term_record>>: lambda "rec" $ lets [
          "fields">: project _Record _Record_fields @@ var "rec",
          "fieldMap">: Maps.fromList (Lists.map (lambda "f" $ pair (project _Field _Field_name @@ var "f") (project _Field _Field_term @@ var "f")) (var "fields")),
          "encodeField">: lambda "nameAd" $ lets [
            "fname">: Pairs.first (var "nameAd"),
            "ad">: Pairs.second (var "nameAd"),
            "fTerm">: Maybes.fromMaybe Core.termUnit (Maps.lookup (var "fname") (var "fieldMap"))] $
            Eithers.map (lambda "jv" $ pair (localName @@ var "fname") (var "jv"))
              (Coders.coderEncode (Coders.adapterCoder (var "ad")) @@ var "cx1" @@ var "fTerm")] $
          Eithers.map (lambda "pairs" $ inject JM._Value JM._Value_object (Maps.fromList (var "pairs")))
            (Eithers.mapList (var "encodeField") (var "fieldAdapters"))],
    "decode">: lambda "cx1" $ lambda "json" $
      cases JM._Value (var "json") (Just (err @@ var "cx" @@ string "expected JSON object")) [
        JM._Value_object>>: lambda "m" $ lets [
          "decodeField">: lambda "nameAd" $ lets [
            "fname">: Pairs.first (var "nameAd"),
            "ad">: Pairs.second (var "nameAd"),
            "jv">: Maybes.fromMaybe (injectUnit JM._Value JM._Value_null) (Maps.lookup (localName @@ var "fname") (var "m"))] $
            Eithers.map (lambda "t" $ Core.field (var "fname") (var "t"))
              (Coders.coderDecode (Coders.adapterCoder (var "ad")) @@ var "cx1" @@ var "jv")] $
          Eithers.map (lambda "fields" $ Core.termRecord (Core.record (var "typeName") (var "fields")))
            (Eithers.mapList (var "decodeField") (var "fieldAdapters"))]] $
    pair (var "encode") (var "decode")

termToJsonValue :: TTermDefinition (Term -> JM.Value)
termToJsonValue = define "termToJsonValue" $
  doc "Convert a Hydra term to a JSON value (for annotation values)" $
  lambda "term" $
    cases _Term (var "term") (Just (inject JM._Value JM._Value_string (string "<term>"))) [
      _Term_literal>>: lambda "lit" $
        cases _Literal (var "lit") Nothing [
          _Literal_string>>: lambda "s" $ inject JM._Value JM._Value_string (var "s"),
          _Literal_boolean>>: lambda "b" $ inject JM._Value JM._Value_boolean (var "b"),
          _Literal_integer>>: lambda "iv" $ inject JM._Value JM._Value_number (integerValueToDouble @@ var "iv"),
          _Literal_float>>: lambda "fv" $ inject JM._Value JM._Value_number (floatValueToDouble @@ var "fv"),
          _Literal_binary>>: lambda "b" $ inject JM._Value JM._Value_string (Literals.binaryToString (var "b"))],
      _Term_list>>: lambda "ts" $
        inject JM._Value JM._Value_array (Lists.map termToJsonValue (var "ts")),
      _Term_map>>: lambda "m" $
        inject JM._Value JM._Value_object (Maps.fromList (Lists.map
          (lambda "entry" $ lets [
            "k">: Pairs.first (var "entry"),
            "v">: Pairs.second (var "entry")] $
            pair
              (cases _Term (var "k") (Just (string "<key>")) [
                _Term_literal>>: lambda "kl" $
                  cases _Literal (var "kl") (Just (string "<key>")) [
                    _Literal_string>>: lambda "s" $ var "s"]])
              (termToJsonValue @@ var "v"))
          (Maps.toList (var "m")))),
      _Term_record>>: lambda "rec" $
        Logic.ifElse (Lists.null (project _Record _Record_fields @@ var "rec"))
          (injectUnit JM._Value JM._Value_null)
          (inject JM._Value JM._Value_string (string "<record>"))]

typeToName :: TTermDefinition (Type -> Name)
typeToName = define "typeToName" $
  doc "Generate a default name for an anonymous type" $
  lambda "t" $
    cases _Type (Strip.deannotateType @@ var "t") (Just (Core.name (string "Unknown"))) [
      _Type_record>>: constant $ Core.name (string "Record"),
      _Type_union>>: constant $ Core.name (string "Union")]

unionAsRecordAdapter :: TTermDefinition (Context -> Type -> Maybe Name -> M.Map Name Term -> [FieldType] -> AvroEnv.EncodeEnvironment -> Result (HydraAvroAdapter, AvroEnv.EncodeEnvironment))
unionAsRecordAdapter = define "unionAsRecordAdapter" $
  doc "Adapter for general unions (encoded as records with optional fields)" $
  lambda "cx" $ lambda "typ" $ lambda "mName" $ lambda "annotations" $ lambda "fieldTypes" $ lambda "env0" $
    Eithers.bind (foldFieldAdapters @@ var "cx" @@ var "fieldTypes" @@ var "env0") (lambda "faResult" $ lets [
      "fieldAdapters">: Pairs.first (var "faResult"),
      "env1">: Pairs.second (var "faResult"),
      "avroFields">: Lists.map
        (lambda "nameAd" $ lets [
          "fname">: Pairs.first (var "nameAd"),
          "ad">: Pairs.second (var "nameAd")] $
          record Avro._Field [
            Avro._Field_name>>: localName @@ var "fname",
            Avro._Field_doc>>: nothing,
            Avro._Field_type>>: inject Avro._Schema Avro._Schema_union (wrap Avro._Union (list [
              inject Avro._Schema Avro._Schema_primitive (injectUnit Avro._Primitive Avro._Primitive_null),
              Coders.adapterTarget (var "ad")])),
            Avro._Field_default>>: just (injectUnit JM._Value JM._Value_null),
            Avro._Field_order>>: nothing,
            Avro._Field_aliases>>: nothing,
            Avro._Field_annotations>>: Maps.empty])
        (var "fieldAdapters"),
      "typeName">: Maybes.fromMaybe (typeToName @@ var "typ") (var "mName"),
      "avroAnnotations">: hydraAnnotationsToAvro @@ var "annotations",
      "avroSchema">: inject Avro._Schema Avro._Schema_named (record Avro._Named [
        Avro._Named_name>>: localName @@ var "typeName",
        Avro._Named_namespace>>: nameNamespace @@ var "typeName",
        Avro._Named_aliases>>: nothing,
        Avro._Named_doc>>: nothing,
        Avro._Named_type>>: inject Avro._NamedType Avro._NamedType_record (record Avro._Record [
          Avro._Record_fields>>: var "avroFields"]),
        Avro._Named_annotations>>: var "avroAnnotations"]),
      "adapter_">: Coders.adapter (boolean True) (var "typ") (var "avroSchema")
        (Coders.coder
          (lambda "cx1" $ lambda "t" $
            cases _Term (var "t") (Just (err @@ var "cx1" @@ string "expected union term")) [
              _Term_inject>>: lambda "inj" $ lets [
                "activeName">: project _Field _Field_name @@ (project _Injection _Injection_field @@ var "inj"),
                "activeValue">: project _Field _Field_term @@ (project _Injection _Injection_field @@ var "inj"),
                "encodePair">: lambda "nameAd" $ lets [
                  "fname">: Pairs.first (var "nameAd"),
                  "ad">: Pairs.second (var "nameAd")] $
                  Logic.ifElse (Core.equalName_ (var "fname") (var "activeName"))
                    (Eithers.map (lambda "jv" $ pair (localName @@ var "fname") (var "jv"))
                      (Coders.coderEncode (Coders.adapterCoder (var "ad")) @@ var "cx1" @@ var "activeValue"))
                    (right (pair (localName @@ var "fname") (injectUnit JM._Value JM._Value_null)))] $
                Eithers.map (lambda "pairs" $ inject JM._Value JM._Value_object (Maps.fromList (var "pairs")))
                  (Eithers.mapList (var "encodePair") (var "fieldAdapters"))])
          (lambda "cx1" $ lambda "j" $
            cases JM._Value (var "j") (Just (err @@ var "cx1" @@ string "expected JSON object for union-as-record")) [
              JM._Value_object>>: lambda "m" $ lets [
                "findActive">: lambda "remaining" $
                  Maybes.maybe
                    (err @@ var "cx1" @@ string "no non-null field in union record")
                    (lambda "p" $ lets [
                      "head_">: Pairs.first (var "p"),
                      "rest_">: Pairs.second (var "p"),
                      "fname">: Pairs.first (var "head_"),
                      "ad">: Pairs.second (var "head_"),
                      "mjv">: Maps.lookup (localName @@ var "fname") (var "m")] $
                      Maybes.maybe
                        (var "findActive" @@ var "rest_")
                        (lambda "jv" $
                          cases JM._Value (var "jv") (Just (
                            Eithers.map (lambda "t" $ Core.termInject (Core.injection (var "typeName") (Core.field (var "fname") (var "t"))))
                              (Coders.coderDecode (Coders.adapterCoder (var "ad")) @@ var "cx1" @@ var "jv"))) [
                            JM._Value_null>>: constant (var "findActive" @@ var "rest_")])
                        (var "mjv"))
                    (Lists.uncons (var "remaining"))] $
                var "findActive" @@ var "fieldAdapters"])),
      "env2">: record AvroEnv._EncodeEnvironment [
        AvroEnv._EncodeEnvironment_typeMap>>: project AvroEnv._EncodeEnvironment AvroEnv._EncodeEnvironment_typeMap @@ var "env1",
        AvroEnv._EncodeEnvironment_emitted>>: Maps.insert (var "typeName") (var "adapter_")
          (project AvroEnv._EncodeEnvironment AvroEnv._EncodeEnvironment_emitted @@ var "env1")]] $
      right (pair (var "adapter_") (var "env2")))
