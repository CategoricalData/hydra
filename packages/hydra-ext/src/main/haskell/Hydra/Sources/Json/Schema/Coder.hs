module Hydra.Sources.Json.Schema.Coder where

-- Standard imports for term-level sources outside of the kernel
import Hydra.Kernel
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Meta.Lib.Strings                as Strings
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
import qualified Hydra.Dsl.Meta.Core                       as Core
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
import qualified Hydra.Sources.Kernel.Terms.Dependencies   as Dependencies
import qualified Hydra.Sources.Kernel.Terms.Environment    as Environment
import qualified Hydra.Sources.Kernel.Terms.Extract.Core   as ExtractCore
import qualified Hydra.Sources.Kernel.Terms.Extract.Util   as ExtractUtil
import qualified Hydra.Sources.Kernel.Terms.Formatting     as Formatting
import qualified Hydra.Sources.Kernel.Terms.Inference      as Inference
import qualified Hydra.Sources.Kernel.Terms.Languages      as Languages
import qualified Hydra.Sources.Kernel.Terms.Lexical        as Lexical
import qualified Hydra.Sources.Kernel.Terms.Literals       as Literals
import qualified Hydra.Sources.Kernel.Terms.Names          as Names
import qualified Hydra.Sources.Kernel.Terms.Predicates     as Predicates
import qualified Hydra.Sources.Kernel.Terms.Variables      as Variables
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
import qualified Hydra.Dsl.Errors as Error
import qualified Hydra.Json.Schema as JS
import qualified Hydra.Json.Model as JM
import qualified Hydra.Sources.Json.Schema as JsonSchema
import qualified Hydra.Sources.Json.Schema.Serde as JsonSchemaSerde


define :: String -> TypedTerm a -> TypedTermDefinition a
define = definitionInModule module_

ns :: ModuleName
ns = ModuleName "hydra.json.schema.coder"

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = definitions,
            moduleDependencies = Bootstrap.unqualifiedDep <$> ([Annotations.ns, Constants.ns, Dependencies.ns, Environment.ns,
              Formatting.ns, Names.ns, Predicates.ns, Reflect.ns,
              ShowVariants.ns, Strip.ns, Variables.ns, JsonSchemaSerde.ns] L.++ (moduleName JsonSchema.module_:jsonModelNs:KernelTypes.kernelTypesModuleNames)),
            moduleMetadata = Bootstrap.descriptionMetadata (Just "JSON Schema code generator: converts Hydra modules to JSON Schema documents")}
  where
    definitions = [
      toDefinition constructModule,
      toDefinition eitherBranch,
      toDefinition encodeField,
      toDefinition encodeName,
      toDefinition encodeNamedType,
      toDefinition encodeRecordOrUnion,
      toDefinition encodeUnion,
      toDefinition isRequiredField,
      toDefinition jsType,
      toDefinition literalTypeName,
      toDefinition moduleToJsonSchema,
      toDefinition nameToPath,
      toDefinition pairRestrictions,
      toDefinition referenceRestriction,
      toDefinition transitiveTypeDeps,
      toDefinition typeDefToDocument,
      toDefinition typeToExpr,
      toDefinition typeToKeywordSchemaPair]


constructModule :: TypedTermDefinition (InferenceContext -> Graph -> Module -> [TypeDefinition] -> Either Error (M.Map FilePath JS.Document))
constructModule = define "constructModule" $
  doc "Construct JSON Schema documents from type definitions" $
  lambda "cx" $ lambda "g" $ lambda "mod" $ lambda "typeDefs" $ lets [
    "typeBody">: lambda "td" $
      Core.typeSchemeBody (project _TypeDefinition _TypeDefinition_body @@ var "td"),
    "typeMap">: Maps.fromList (Lists.map
      ("td" ~> pair (project _TypeDefinition _TypeDefinition_name @@ var "td") (var "typeBody" @@ var "td"))
      (var "typeDefs"))] $
    Eithers.map (lambda "ps" $ Maps.fromList (var "ps")) (Eithers.mapList
      ("td" ~> typeDefToDocument @@ var "cx" @@ var "g" @@ var "typeMap"
        @@ (project _TypeDefinition _TypeDefinition_name @@ var "td")
        @@ (var "typeBody" @@ var "td"))
      (var "typeDefs"))

eitherBranch :: TypedTermDefinition (String -> [JS.Restriction] -> JS.Schema)
eitherBranch = define "eitherBranch" $
  doc "Build a single-property record Schema for one branch of an Either oneOf" $
  lambda "label" $ lambda "res" $
    wrap JS._Schema (list [
      inject JS._Restriction JS._Restriction_type
        (inject JS._Type JS._Type_single (inject JS._TypeName JS._TypeName_object unit)),
      inject JS._Restriction JS._Restriction_object
        (inject JS._ObjectRestriction JS._ObjectRestriction_properties
          (Maps.singleton (wrap JS._Keyword (var "label")) (wrap JS._Schema (var "res")))),
      inject JS._Restriction JS._Restriction_object
        (inject JS._ObjectRestriction JS._ObjectRestriction_required
          (list [wrap JS._Keyword (var "label")])),
      inject JS._Restriction JS._Restriction_object
        (inject JS._ObjectRestriction JS._ObjectRestriction_additionalProperties
          (inject JS._AdditionalItems JS._AdditionalItems_any false))])

encodeField :: TypedTermDefinition (InferenceContext -> Graph -> FieldType -> Either Error (JS.Keyword, JS.Schema))
encodeField = define "encodeField" $
  doc "Encode a field type as a JSON Schema keyword-schema pair" $
  lambda "cx" $ lambda "g" $ lambda "ft" $ lets [
    "name">: project _FieldType _FieldType_name @@ var "ft",
    "typ">: project _FieldType _FieldType_type @@ var "ft"] $
    Eithers.map
      (lambda "res" $ pair (wrap JS._Keyword (Core.unName $ var "name")) (wrap JS._Schema (var "res")))
      (typeToExpr @@ var "cx" @@ var "g" @@ false @@ var "typ")

encodeName :: TypedTermDefinition (Name -> String)
encodeName = define "encodeName" $
  doc "Encode a Hydra name as a safe identifier string, replacing non-alphanumeric characters with underscores" $
  lambda "name" $
    Formatting.nonAlnumToUnderscores @@ Core.unName (var "name")

encodeNamedType :: TypedTermDefinition (InferenceContext -> Graph -> Name -> Type -> Either Error [JS.Restriction])
encodeNamedType = define "encodeNamedType" $
  doc "Encode a named type as a list of JSON Schema restrictions with a title" $
  lambda "cx" $ lambda "g" $ lambda "name" $ lambda "typ" $
    Eithers.map
      (lambda "res" $ Lists.concat $ list [
        list [inject JS._Restriction JS._Restriction_title (Core.unName $ var "name")],
        var "res"])
      (typeToExpr @@ var "cx" @@ var "g" @@ false @@ (Strip.deannotateType @@ var "typ"))

encodeRecordOrUnion :: TypedTermDefinition (InferenceContext -> Graph -> Bool -> Bool -> [FieldType] -> Either Error [JS.Restriction])
encodeRecordOrUnion = define "encodeRecordOrUnion" $
  doc "Encode a record or union as a list of JSON Schema object restrictions; isUnion adds min/maxProperties=1" $
  lambda "cx" $ lambda "g" $ lambda "optional" $ lambda "isUnion" $ lambda "fields" $
    Eithers.bind
      (Eithers.mapList ("ft" ~> encodeField @@ var "cx" @@ var "g" @@ var "ft") (var "fields"))
      ("props" ~> lets [
        "objRes">: list [inject JS._Restriction JS._Restriction_object
          (inject JS._ObjectRestriction JS._ObjectRestriction_properties (Maps.fromList (var "props")))],
        "reqs">: Lists.foldl
          (lambda "acc" $ lambda "f" $ Logic.ifElse (isRequiredField @@ var "f")
            (Lists.concat2 (var "acc") (list [wrap JS._Keyword (Core.unName (project _FieldType _FieldType_name @@ var "f"))]))
            (var "acc"))
          (list ([] :: [TypedTerm JS.Keyword]))
          (var "fields"),
        "reqRes">: Logic.ifElse (Lists.null (var "reqs"))
          (list ([] :: [TypedTerm JS.Restriction]))
          (list [inject JS._Restriction JS._Restriction_object
            (inject JS._ObjectRestriction JS._ObjectRestriction_required (var "reqs"))]),
        "addPropsRes">: list [inject JS._Restriction JS._Restriction_object
          (inject JS._ObjectRestriction JS._ObjectRestriction_additionalProperties
            (inject JS._AdditionalItems JS._AdditionalItems_any false))],
        "cardRes">: Logic.ifElse (var "isUnion")
          (list [
            inject JS._Restriction JS._Restriction_object
              (inject JS._ObjectRestriction JS._ObjectRestriction_minProperties (int32 1)),
            inject JS._Restriction JS._Restriction_object
              (inject JS._ObjectRestriction JS._ObjectRestriction_maxProperties (int32 1))])
          (list ([] :: [TypedTerm JS.Restriction]))] $
        right (Lists.concat (list [
          jsType @@ var "optional" @@ inject JS._TypeName JS._TypeName_object unit,
          var "objRes",
          var "reqRes",
          var "addPropsRes",
          var "cardRes"])))

encodeUnion :: TypedTermDefinition (InferenceContext -> Graph -> Bool -> [FieldType] -> Either Error [JS.Restriction])
encodeUnion = define "encodeUnion" $
  doc "Encode a union type, splitting unit-typed (simple) variants into a string-enum branch" $
  lambda "cx" $ lambda "g" $ lambda "optional" $ lambda "fields" $ lets [
    "isSimple">: lambda "f" $
      Predicates.isUnitType @@ (Strip.deannotateType @@ (project _FieldType _FieldType_type @@ var "f")),
    "simple">: Lists.filter (var "isSimple") (var "fields"),
    "nonsimple">: Lists.filter ("f" ~> Logic.not (var "isSimple" @@ var "f")) (var "fields")] $
    Logic.ifElse (Lists.null (var "simple"))
      (encodeRecordOrUnion @@ var "cx" @@ var "g" @@ var "optional" @@ true @@ var "fields")
      (Eithers.bind
        (encodeRecordOrUnion @@ var "cx" @@ var "g" @@ false @@ true @@ var "nonsimple")
        ("recRes" ~> lets [
          "names">: Lists.map ("f" ~> Core.unName (project _FieldType _FieldType_name @@ var "f")) (var "simple"),
          "simpleSchema">: wrap JS._Schema (list [
            inject JS._Restriction JS._Restriction_type
              (inject JS._Type JS._Type_single (inject JS._TypeName JS._TypeName_string unit)),
            inject JS._Restriction JS._Restriction_multiple
              (inject JS._MultipleRestriction JS._MultipleRestriction_enum
                (Lists.map ("n" ~> inject JM._Value JM._Value_string (var "n")) (var "names")))])] $
          right (list [
            inject JS._Restriction JS._Restriction_multiple
              (inject JS._MultipleRestriction JS._MultipleRestriction_oneOf
                (list [wrap JS._Schema (var "recRes"), var "simpleSchema"]))])))

isRequiredField :: TypedTermDefinition (FieldType -> Bool)
isRequiredField = define "isRequiredField" $
  doc "Determine whether a field is required (i.e., not optional/Maybe)" $
  lambda "ft" $ lets [
    "typ">: project _FieldType _FieldType_type @@ var "ft"] $
    cases _Type (Strip.deannotateType @@ var "typ") (Just true) [
      _Type_maybe>>: constant false]

jsType :: TypedTermDefinition (Bool -> JS.TypeName -> [JS.Restriction])
jsType = define "jsType" $
  doc "Build the JSON Schema type-restriction list for a type name, optionally widening to allow null" $
  lambda "optional" $ lambda "tname" $
    list [inject JS._Restriction JS._Restriction_type
      (Logic.ifElse (var "optional")
        (inject JS._Type JS._Type_multiple (list [var "tname", inject JS._TypeName JS._TypeName_null unit]))
        (inject JS._Type JS._Type_single (var "tname")))]

jsonModelNs :: ModuleName
jsonModelNs = ModuleName "hydra.json.model"

jsonSchemaPhantomNs :: ModuleName
jsonSchemaPhantomNs = ModuleName "hydra.json.schema"

literalTypeName :: TypedTermDefinition (LiteralType -> JS.TypeName)
literalTypeName = define "literalTypeName" $
  doc "Map a Hydra literal type to a JSON Schema type name" $
  lambda "lt" $
    cases _LiteralType (var "lt")
      (Just (inject JS._TypeName JS._TypeName_string unit)) [
      _LiteralType_binary>>: constant (inject JS._TypeName JS._TypeName_string unit),
      _LiteralType_boolean>>: constant (inject JS._TypeName JS._TypeName_boolean unit),
      _LiteralType_float>>: constant (inject JS._TypeName JS._TypeName_number unit),
      _LiteralType_integer>>: constant (inject JS._TypeName JS._TypeName_integer unit),
      _LiteralType_string>>: constant (inject JS._TypeName JS._TypeName_string unit)]

moduleToJsonSchema :: TypedTermDefinition (Module -> [Definition] -> InferenceContext -> Graph -> Either Error (M.Map FilePath String))
moduleToJsonSchema = define "moduleToJsonSchema" $
  doc "Convert a Hydra module to a map from file path to JSON Schema document string" $
  lambda "mod" $ lambda "defs" $ lambda "cx" $ lambda "g" $ lets [
    "partitioned">: Environment.partitionDefinitions @@ var "defs",
    "typeDefs">: Pairs.first (var "partitioned")] $
    Eithers.map
      (lambda "docs" $ Maps.map (lambda "doc" $ JsonSchemaSerde.jsonSchemaDocumentToString @@ var "doc") (var "docs"))
      (constructModule @@ var "cx" @@ var "g" @@ var "mod" @@ var "typeDefs")

nameToPath :: TypedTermDefinition (Name -> FilePath)
nameToPath = define "nameToPath" $
  doc "Compute the JSON Schema output file path for a named type" $
  lambda "name" $ lets [
    "qn">: Names.qualifyName @@ var "name",
    "mns">: Util.qualifiedNameModuleName (var "qn"),
    "local">: Util.qualifiedNameLocal (var "qn"),
    "nsPart">: Maybes.cases (var "mns") (string "") ("ns" ~> Strings.cat2 (Packaging.unModuleName (var "ns")) (string "."))] $
    Names.moduleNameToFilePath
      @@ Util.caseConventionCamel
      @@ wrap _FileExtension (string "json")
      @@ wrap _ModuleName (Strings.cat2 (var "nsPart") (var "local"))

pairRestrictions :: TypedTermDefinition (Bool -> [JS.Restriction] -> [JS.Restriction] -> [JS.Restriction])
pairRestrictions = define "pairRestrictions" $
  doc "Build the JSON Schema restriction list for a pair type" $
  lambda "optional" $ lambda "firstRes" $ lambda "secondRes" $
    Lists.concat (list [
      jsType @@ var "optional" @@ inject JS._TypeName JS._TypeName_object unit,
      list [inject JS._Restriction JS._Restriction_object
        (inject JS._ObjectRestriction JS._ObjectRestriction_properties
          (Maps.fromList (list [
            pair (wrap JS._Keyword (string "first")) (wrap JS._Schema (var "firstRes")),
            pair (wrap JS._Keyword (string "second")) (wrap JS._Schema (var "secondRes"))])))],
      list [inject JS._Restriction JS._Restriction_object
        (inject JS._ObjectRestriction JS._ObjectRestriction_required
          (list [wrap JS._Keyword (string "first"), wrap JS._Keyword (string "second")]))],
      list [inject JS._Restriction JS._Restriction_object
        (inject JS._ObjectRestriction JS._ObjectRestriction_additionalProperties
          (inject JS._AdditionalItems JS._AdditionalItems_any false))]])

referenceRestriction :: TypedTermDefinition (Name -> JS.Restriction)
referenceRestriction = define "referenceRestriction" $
  doc "Create a JSON Schema reference restriction for a named type" $
  lambda "name" $
    inject JS._Restriction JS._Restriction_reference
      (wrap JS._SchemaReference (Strings.cat $ list [string "#/$defs/", encodeName @@ var "name"]))

transitiveTypeDeps :: TypedTermDefinition (M.Map Name Type -> S.Set Name -> Type -> S.Set Name)
transitiveTypeDeps = define "transitiveTypeDeps" $
  doc "Walk the transitive named-type dependency closure of a root type through typeMap; the visited set guards against cycles in self-/mutually-recursive types" $
  lambda "typeMap" $ lambda "visited" $ lambda "rootType" $ lets [
    "directDeps">: Dependencies.typeDependencyNames @@ true @@ var "rootType",
    "step">: lambda "acc" $ lambda "n" $
      Logic.ifElse (Sets.member (var "n") (var "acc"))
        (var "acc")
        (lets ["acc1">: Sets.insert (var "n") (var "acc")] $
          Maybes.cases (Maps.lookup (var "n") (var "typeMap")) (var "acc1") ("t" ~> asTerm transitiveTypeDeps @@ var "typeMap" @@ var "acc1" @@ var "t"))] $
    Lists.foldl (var "step") (var "visited") (Sets.toList (var "directDeps"))

typeDefToDocument :: TypedTermDefinition (InferenceContext -> Graph -> M.Map Name Type -> Name -> Type -> Either Error (FilePath, JS.Document))
typeDefToDocument = define "typeDefToDocument" $
  doc "Build a JSON Schema document for a single named type, with $defs covering its transitive dependencies and short-name substitution applied" $
  lambda "cx" $ lambda "g" $ lambda "typeMap" $ lambda "rootName" $ lambda "rootType" $ lets [
    "depNames">: Sets.toList (transitiveTypeDeps @@ var "typeMap" @@ Sets.empty @@ var "rootType"),
    "allNames">: Lists.concat2
      (list [var "rootName"])
      (Lists.filter ("n" ~> Logic.not (Equality.equal (var "n") (var "rootName"))) (var "depNames")),
    "allTypes">: Lists.map
      ("n" ~> Maybes.fromMaybe (Core.typeVariable (var "n")) (Maps.lookup (var "n") (var "typeMap")))
      (var "allNames"),
    "nameSubst">: Dependencies.toShortNames @@ var "allNames",
    "types">: Lists.map ("t" ~> Variables.substituteTypeVariables @@ var "nameSubst" @@ var "t") (var "allTypes"),
    "names">: Lists.map ("n" ~> Maybes.fromMaybe (var "n") (Maps.lookup (var "n") (var "nameSubst"))) (var "allNames"),
    "subRoot">: Maybes.fromMaybe (var "rootName") (Maps.lookup (var "rootName") (var "nameSubst")),
    "pairs">: Lists.zip (var "names") (var "types")] $
    Eithers.bind
      (Eithers.mapList ("p" ~> typeToKeywordSchemaPair @@ var "cx" @@ var "g"
        @@ Pairs.first (var "p") @@ Pairs.second (var "p")) (var "pairs"))
      ("schemas" ~> right (pair
        (nameToPath @@ var "rootName")
        (record JS._Document [
          JS._Document_id>>: nothing,
          JS._Document_definitions>>: just (Maps.fromList (var "schemas")),
          JS._Document_root>>: wrap JS._Schema (list [referenceRestriction @@ var "subRoot"])])))

typeToExpr :: TypedTermDefinition (InferenceContext -> Graph -> Bool -> Type -> Either Error [JS.Restriction])
typeToExpr = define "typeToExpr" $
  doc "Encode a Hydra type as a list of JSON Schema restrictions" $
  lambda "cx" $ lambda "g" $ lambda "optional" $ lambda "typ" $
    cases _Type (var "typ")
      (Just (left (Error.errorOther (Error.otherError
        (Strings.cat2 (string "JSON Schema: unsupported type variant: ")
          (ShowVariants.typeVariant @@ (Reflect.typeVariant @@ var "typ"))))))) [

      _Type_annotated>>: ("at" ~>
        Eithers.bind
          (asTerm typeToExpr @@ var "cx" @@ var "g" @@ var "optional" @@ (Strip.deannotateType @@ var "typ"))
          ("res" ~> Eithers.bind
            (Annotations.getTypeDescription @@ var "cx" @@ var "g" @@ var "typ")
            ("mdesc" ~> right (Lists.concat2
              (Maybes.cases (var "mdesc") (list ([] :: [TypedTerm JS.Restriction])) ("d" ~> list [inject JS._Restriction JS._Restriction_description (var "d")]))
              (var "res"))))),

      _Type_application>>: ("at" ~>
        asTerm typeToExpr @@ var "cx" @@ var "g" @@ var "optional"
          @@ (project _ApplicationType _ApplicationType_function @@ var "at")),

      _Type_either>>: ("et" ~> lets [
        "lt">: project _EitherType _EitherType_left @@ var "et",
        "rt">: project _EitherType _EitherType_right @@ var "et"] $
        Eithers.bind
          (asTerm typeToExpr @@ var "cx" @@ var "g" @@ false @@ var "lt")
          ("leftRes" ~> Eithers.bind
            (asTerm typeToExpr @@ var "cx" @@ var "g" @@ false @@ var "rt")
            ("rightRes" ~> right (list [
              inject JS._Restriction JS._Restriction_multiple
                (inject JS._MultipleRestriction JS._MultipleRestriction_oneOf
                  (list [
                    eitherBranch @@ string "left" @@ var "leftRes",
                    eitherBranch @@ string "right" @@ var "rightRes"]))])))),

      _Type_forall>>: ("ft" ~>
        asTerm typeToExpr @@ var "cx" @@ var "g" @@ var "optional"
          @@ (project _ForallType _ForallType_body @@ var "ft")),

      _Type_list>>: ("lt" ~> Eithers.bind
        (asTerm typeToExpr @@ var "cx" @@ var "g" @@ false @@ var "lt")
        ("els" ~> right (Lists.concat2
          (jsType @@ var "optional" @@ inject JS._TypeName JS._TypeName_array unit)
          (list [inject JS._Restriction JS._Restriction_array
            (inject JS._ArrayRestriction JS._ArrayRestriction_items
              (inject JS._Items JS._Items_sameItems (wrap JS._Schema (var "els"))))])))),

      _Type_literal>>: ("lt" ~> right (jsType @@ var "optional" @@ (literalTypeName @@ var "lt"))),

      _Type_map>>: ("mt" ~> lets [
        "vt">: project _MapType _MapType_values @@ var "mt"] $
        Eithers.bind
          (asTerm typeToExpr @@ var "cx" @@ var "g" @@ false @@ var "vt")
          ("vRes" ~> right (Lists.concat2
            (jsType @@ var "optional" @@ inject JS._TypeName JS._TypeName_object unit)
            (list [inject JS._Restriction JS._Restriction_object
              (inject JS._ObjectRestriction JS._ObjectRestriction_additionalProperties
                (inject JS._AdditionalItems JS._AdditionalItems_schema (wrap JS._Schema (var "vRes"))))])))),

      _Type_maybe>>: ("mt" ~>
        asTerm typeToExpr @@ var "cx" @@ var "g" @@ true @@ var "mt"),

      _Type_pair>>: ("pt" ~> lets [
        "ft">: project _PairType _PairType_first @@ var "pt",
        "st">: project _PairType _PairType_second @@ var "pt"] $
        Eithers.bind
          (asTerm typeToExpr @@ var "cx" @@ var "g" @@ false @@ var "ft")
          ("firstRes" ~> Eithers.bind
            (asTerm typeToExpr @@ var "cx" @@ var "g" @@ false @@ var "st")
            ("secondRes" ~> right (pairRestrictions @@ var "optional" @@ var "firstRes" @@ var "secondRes")))),

      _Type_record>>: ("fields" ~>
        encodeRecordOrUnion @@ var "cx" @@ var "g" @@ var "optional" @@ false @@ var "fields"),

      _Type_set>>: ("st" ~> Eithers.bind
        (asTerm typeToExpr @@ var "cx" @@ var "g" @@ false @@ var "st")
        ("els" ~> right (Lists.concat2
          (jsType @@ var "optional" @@ inject JS._TypeName JS._TypeName_array unit)
          (list [inject JS._Restriction JS._Restriction_array
            (inject JS._ArrayRestriction JS._ArrayRestriction_items
              (inject JS._Items JS._Items_sameItems (wrap JS._Schema (var "els"))))])))),

      _Type_union>>: ("fields" ~> encodeUnion @@ var "cx" @@ var "g" @@ var "optional" @@ var "fields"),

      _Type_variable>>: ("name" ~>
        right (list [referenceRestriction @@ var "name"])),

      _Type_wrap>>: ("inner" ~>
        asTerm typeToExpr @@ var "cx" @@ var "g" @@ var "optional" @@ var "inner")]

typeToKeywordSchemaPair :: TypedTermDefinition (InferenceContext -> Graph -> Name -> Type -> Either Error (JS.Keyword, JS.Schema))
typeToKeywordSchemaPair = define "typeToKeywordSchemaPair" $
  doc "Build a (Keyword, Schema) pair for a named type, used as a $defs entry" $
  lambda "cx" $ lambda "g" $ lambda "name" $ lambda "typ" $
    Eithers.map
      (lambda "res" $ pair
        (wrap JS._Keyword (encodeName @@ var "name"))
        (wrap JS._Schema (var "res")))
      (encodeNamedType @@ var "cx" @@ var "g" @@ var "name" @@ var "typ")
