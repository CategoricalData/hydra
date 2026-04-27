-- | Protobuf code generator in Hydra DSL.
-- This module provides DSL versions of Protobuf code generation functions.
-- Hydra type definitions are mapped to Protocol Buffers v3 message and enum types.

module Hydra.Sources.Protobuf.Coder where

-- Standard imports for term-level sources outside of the kernel
import Hydra.Kernel
import Hydra.Sources.Libraries
import           Hydra.Dsl.Meta.Lib.Strings                as Strings
import           Hydra.Dsl.Meta.Phantoms                   as Phantoms
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
import qualified Hydra.Dsl.Coders                     as Coders
import qualified Hydra.Dsl.Meta.Context                    as Ctx
import qualified Hydra.Dsl.Meta.Core                       as Core
import qualified Hydra.Dsl.Errors                      as Error
import qualified Hydra.Dsl.Packaging                     as Packaging
import qualified Hydra.Dsl.Util                       as Util
import qualified Hydra.Sources.Kernel.Terms.Adapt           as Adapt
import qualified Hydra.Sources.Kernel.Terms.Annotations    as Annotations
import qualified Hydra.Sources.Kernel.Terms.Constants      as Constants
import qualified Hydra.Sources.Kernel.Terms.Extract.Core   as ExtractCore
import qualified Hydra.Sources.Kernel.Terms.Formatting     as Formatting
import qualified Hydra.Sources.Kernel.Terms.Lexical        as Lexical
import qualified Hydra.Sources.Kernel.Terms.Names          as Names
import qualified Hydra.Sources.Kernel.Terms.Rewriting      as Rewriting
import qualified Hydra.Sources.Kernel.Terms.Strip          as Strip
import qualified Hydra.Sources.Kernel.Terms.Variables      as Variables
import qualified Hydra.Sources.Kernel.Terms.Predicates    as Predicates
import qualified Hydra.Sources.Kernel.Terms.Analysis      as Analysis
import qualified Hydra.Sources.Kernel.Terms.Environment   as Environment
import qualified Hydra.Sources.Kernel.Terms.Serialization  as Serialization
import qualified Hydra.Sources.Kernel.Terms.Show.Core      as ShowCore
import qualified Hydra.Sources.Kernel.Terms.Show.Errors     as ShowError
import qualified Hydra.Sources.Kernel.Types.All            as KernelTypes
import qualified Hydra.Sources.Decode.Core                 as DecodeCore
import           Prelude hiding ((++))
import qualified Data.Int                                  as I
import qualified Data.List                                 as L
import qualified Data.Map                                  as M
import qualified Data.Set                                  as S
import qualified Data.Maybe                                as Y

-- Additional imports
import qualified Hydra.Protobuf.Proto3 as P3
import qualified Hydra.Sources.Protobuf.Proto3 as Proto3Syntax
import qualified Hydra.Sources.Protobuf.Language as ProtobufLanguageSource
import qualified Hydra.Sources.Protobuf.Environment as ProtobufEnvironment
import qualified Hydra.Sources.Protobuf.Serde as ProtobufSerdeSource


def :: String -> TTerm a -> TTermDefinition a
def = definitionInModule module_

-- | Name for the StructuralTypeRef union type (either | pair)
_StructuralTypeRef :: Name
_StructuralTypeRef = Name "hydra.protobuf.environment.StructuralTypeRef"

-- | An empty list term, avoiding ambiguous type variable issues with 'list []'
emptyList :: TTerm [a]
emptyList = TTerm $ TermList []

-- | Reference to the hydra.decode.core.type function (Graph -> Term -> Either DecodingError Type)
decodeType :: TTerm (Graph -> Term -> Either DecodingError Type)
decodeType = TTerm $ TermVariable $ Name "hydra.decode.core.type"

ns :: Namespace
ns = Namespace "hydra.protobuf.coder"

module_ :: Module
module_ = Module {
            moduleNamespace = ns,
            moduleDefinitions = definitions,
            moduleTermDependencies = [moduleNamespace ProtobufSerdeSource.module_, moduleNamespace ProtobufLanguageSource.module_,
      Formatting.ns, Names.ns, Rewriting.ns, Strip.ns, Variables.ns, Analysis.ns, Environment.ns, Predicates.ns, Lexical.ns, Serialization.ns,
      Annotations.ns, Constants.ns, ExtractCore.ns, Adapt.ns, ShowCore.ns, ShowError.ns,
      moduleNamespace DecodeCore.module_],
            moduleTypeDependencies = (ProtobufEnvironment.ns:Proto3Syntax.ns:KernelTypes.kernelTypesNamespaces),
            moduleDescription = Just "Protobuf code generator: converts Hydra modules to Protocol Buffers v3 definitions"}
  where
    definitions = [
      toDefinition collectStructuralTypes,
      toDefinition collectStructuralTypes_collectFromType,
      toDefinition constructModule,
      toDefinition encodeDefinition,
      toDefinition encodeEnumDefinition,
      toDefinition encodeEnumValueName,
      toDefinition encodeFieldName,
      toDefinition encodeFieldType,
      toDefinition encodeRecordType,
      toDefinition encodeScalarType,
      toDefinition encodeScalarTypeWrapped,
      toDefinition encodeSimpleTypeForHelper,
      toDefinition encodeTypeName,
      toDefinition encodeTypeReference,
      toDefinition err,
      toDefinition findOptions,
      toDefinition flattenType,
      toDefinition fromEitherString,
      toDefinition generateStructuralTypeMessage,
      toDefinition isEnumDefinition,
      toDefinition isEnumFields,
      toDefinition javaMultipleFilesOptionName,
      toDefinition javaPackageOptionName,
      toDefinition key_proto_field_index,
      toDefinition mapAccumResult,
      toDefinition moduleToProtobuf,
      toDefinition namespaceToFileReference,
      toDefinition namespaceToPackageName,
      toDefinition readBooleanAnnotation,
      toDefinition simplifyType,
      toDefinition structuralTypeName,
      toDefinition unexpectedE]


-- =============================================================================
-- Constants
-- =============================================================================

-- | Collect all structural type references (Either, Pair) from a list of types
collectStructuralTypes :: TTermDefinition ([Type] -> S.Set Term)
collectStructuralTypes = def "collectStructuralTypes" $
  doc "Collect all structural type references (Either, Pair) from a list of types" $
  "types" ~>
    Lists.foldl
      ("acc" ~> "t" ~> Sets.union (var "acc") (asTerm collectStructuralTypes_collectFromType @@ var "t"))
      Sets.empty
      (var "types")

collectStructuralTypes_collectFromType :: TTermDefinition (Type -> S.Set Term)
collectStructuralTypes_collectFromType = def "collectStructuralTypes_collectFromType" $
  doc "Collect structural type references from a single type" $
  "typ" ~>
    Rewriting.foldOverType @@ Coders.traversalOrderPre @@
      ("acc" ~> "t" ~> lets [
        "st">: asTerm simplifyType @@ var "t"] $
        cases _Type (var "st")
          (Just $ var "acc") [
          _Type_either>>: "et" ~>
            Sets.insert
              (inject _StructuralTypeRef (Name "either") (pair (Core.eitherTypeLeft (var "et")) (Core.eitherTypeRight (var "et"))))
              (var "acc"),
          _Type_pair>>: "pt" ~>
            Sets.insert
              (inject _StructuralTypeRef (Name "pair") (pair (Core.pairTypeFirst (var "pt")) (Core.pairTypeSecond (var "pt"))))
              (var "acc")]) @@
      Sets.empty @@
      (var "typ")

-- =============================================================================
-- Module-level entry point
-- =============================================================================

constructModule :: TTermDefinition (Context -> Graph -> Module -> [TypeDefinition] -> Either Error P3.ProtoFile)
constructModule = def "constructModule" $
  doc "Construct a Protobuf file from a Hydra module and its type definitions" $
  "cx" ~> "g" ~> "mod" ~> "typeDefs" ~> lets [
    "ns_">: Packaging.moduleNamespace (var "mod"),
    "desc">: Packaging.moduleDescription (var "mod"),
    "toDef">: "td" ~> lets [
      "name">: Packaging.typeDefinitionName (var "td"),
      "typ">: Core.typeSchemeBody $ Packaging.typeDefinitionType (var "td"),
      "encodeDefEither">: "n" ~> "t" ~> asTerm encodeDefinition @@ var "cx" @@ var "g" @@ var "ns_" @@ var "n" @@ var "t",
      "flatTyp">: asTerm flattenType @@ var "typ",
      "enc">: var "encodeDefEither" @@ var "name"] $
      (cases _Type (Strip.deannotateType @@ var "flatTyp")
          (Just ("adaptedType" <<~ Adapt.adaptTypeForLanguage @@ ProtobufLanguageSource.protobufLanguage @@ var "flatTyp" $
            var "enc" @@ var "adaptedType")) [
          _Type_variable>>: constant (var "enc" @@ var "flatTyp")]),
    "types">: Lists.map ("td" ~> Core.typeSchemeBody (Packaging.typeDefinitionType (var "td"))) (var "typeDefs"),
    "structRefs">: asTerm collectStructuralTypes @@ var "types",
    "javaOptions">: list [
      record P3._Option [
        P3._Option_name>>: asTerm javaMultipleFilesOptionName,
        P3._Option_value>>: inject P3._Value P3._Value_boolean true],
      record P3._Option [
        P3._Option_name>>: asTerm javaPackageOptionName,
        P3._Option_value>>: inject P3._Value P3._Value_string (unwrap P3._PackageName @@ (asTerm namespaceToPackageName @@ var "ns_"))]],
    "descOption">: record P3._Option [
      P3._Option_name>>: ProtobufSerdeSource.descriptionOptionName,
      P3._Option_value>>: inject P3._Value P3._Value_string $
        Strings.cat2
          (Maybes.maybe (string "") ("d" ~> Strings.cat2 (var "d") (string "\n\n")) (var "desc"))
          (asTerm Constants.warningAutoGeneratedFile)],
    "checkFieldType_wrapper">: "typ" ~> cases _Type (Strip.deannotateType @@ var "typ")
      (Just false) [
      _Type_maybe>>: "ot" ~> cases _Type (Strip.deannotateType @@ var "ot")
        (Just false) [
        _Type_literal>>: constant true]],
    "checkFieldType_empty">: "typ" ~> Predicates.isUnitType @@ var "typ",
    "checkFields">: "checkType" ~> "checkFieldType" ~> "ts" ~>
      Lists.foldl ("b" ~> "t" ~> Logic.or (var "b")
        (Rewriting.foldOverType @@ Coders.traversalOrderPre @@
          ("b2" ~> "t2" ~> Logic.or (var "b2") (lets [
            "checkResult">: var "checkType" @@ var "t2"] $
            Maybes.maybe
              (cases _Type (var "t2")
                (Just false) [
                _Type_record>>: "fts" ~> Lists.foldl ("b3" ~> "f" ~> Logic.or (var "b3") (var "checkFieldType" @@ (Strip.deannotateType @@ Core.fieldTypeType (var "f")))) false (var "fts"),
                _Type_union>>: "fts" ~> Lists.foldl ("b3" ~> "f" ~> Logic.or (var "b3") (var "checkFieldType" @@ (Strip.deannotateType @@ Core.fieldTypeType (var "f")))) false (var "fts")])
              ("b3" ~> var "b3")
              (var "checkResult"))) @@
          false @@ var "t")) false (var "ts"),
    "wrapperImport">: Logic.ifElse
      (var "checkFields" @@ (constant nothing) @@ var "checkFieldType_wrapper" @@ var "types")
      (list [wrap P3._FileReference (string "google/protobuf/wrappers.proto")])
      emptyList,
    "emptyCheckType">: "typ" ~> Logic.ifElse (asTerm isEnumDefinition @@ var "typ") (just false) nothing,
    "emptyImport">: Logic.ifElse
      (var "checkFields" @@ var "emptyCheckType" @@ var "checkFieldType_empty" @@ var "types")
      (list [wrap P3._FileReference (string "google/protobuf/empty.proto")])
      emptyList] $
    "schemaImports" <<~ (Analysis.moduleDependencyNamespaces @@ var "cx" @@ var "g" @@ true @@ false @@ false @@ false @@ var "mod") $
    "definitions" <<~ (Eithers.mapList (var "toDef") (var "typeDefs")) $ lets [
      "schemaImportList">: Lists.map (lambda "n" $ asTerm namespaceToFileReference @@ var "n") (Sets.toList (var "schemaImports"))] $
      "helperResult" <<~ (asTerm mapAccumResult @@
        ("cx0" ~> "ref" ~> asTerm generateStructuralTypeMessage @@ var "cx0" @@ var "g" @@ var "ns_" @@ var "ref") @@
        var "cx" @@
        (Sets.toList (var "structRefs"))) $ lets [
        "helperDefs">: Pairs.first (var "helperResult")] $
        right $ record P3._ProtoFile [
          P3._ProtoFile_package>>: asTerm namespaceToPackageName @@ var "ns_",
          P3._ProtoFile_imports>>: Lists.concat (list [var "schemaImportList", var "wrapperImport", var "emptyImport"]),
          P3._ProtoFile_types>>: Lists.concat (list [var "helperDefs", var "definitions"]),
          P3._ProtoFile_options>>: Lists.cons (var "descOption") (var "javaOptions")]

-- =============================================================================
-- Accumulator helper
-- =============================================================================

encodeDefinition :: TTermDefinition (Context -> Graph -> Namespace -> Name -> Type -> Either Error P3.Definition)
encodeDefinition = def "encodeDefinition" $
  doc "Encode a Hydra type as a Protobuf definition" $
  "cx" ~> "g" ~> "localNs" ~> "name" ~> "typ" ~> lets [
    "cx1">: Annotations.resetCount @@ asTerm key_proto_field_index @@ var "cx",
    "cx2">: Pairs.second (Annotations.nextCount @@ asTerm key_proto_field_index @@ var "cx1"),
    "wrapAsRecordType">: "t" ~>
      inject _Type _Type_record (list [Core.fieldType (Core.name (string "value")) (var "t")]),
    "toEitherString">: "result" ~> var "result",
    "encode">: "cx0" ~> "options" ~> "t" ~>
      cases _Type (asTerm simplifyType @@ var "t")
        (Just $ var "encode" @@ var "cx0" @@ var "options" @@ (var "wrapAsRecordType" @@ var "t")) [
        _Type_record>>: "fts" ~>
          Eithers.map
            ("md" ~> inject P3._Definition P3._Definition_message (var "md"))
            (var "toEitherString" @@ (asTerm encodeRecordType @@ var "cx0" @@ var "g" @@ var "localNs" @@ var "options" @@ var "name" @@ var "fts")),
        _Type_union>>: "fts" ~> Logic.ifElse (asTerm isEnumDefinition @@ var "t")
          (Eithers.map
            ("ed" ~> inject P3._Definition P3._Definition_enum (var "ed"))
            (var "toEitherString" @@ (asTerm encodeEnumDefinition @@ var "cx0" @@ var "g" @@ var "options" @@ var "name" @@ var "fts")))
          (var "encode" @@ var "cx0" @@ var "options" @@ (var "wrapAsRecordType" @@ (inject _Type _Type_union (var "fts"))))]] $
    "options" <<~ (var "toEitherString" @@ (asTerm findOptions @@ var "cx" @@ var "g" @@ var "typ")) $
    var "encode" @@ var "cx2" @@ var "options" @@ var "typ"

-- =============================================================================
-- Enum encoding
-- =============================================================================

encodeEnumDefinition :: TTermDefinition (Context -> Graph -> [P3.Option] -> Name -> [FieldType] -> Either Error P3.EnumDefinition)
encodeEnumDefinition = def "encodeEnumDefinition" $
  doc "Encode a Hydra union type as a Protobuf enum definition" $
  "cx" ~> "g" ~> "options" ~> "tname" ~> "fts" ~> lets [
    "unspecifiedField">: record P3._EnumValue [
      P3._EnumValue_name>>: asTerm encodeEnumValueName @@ var "tname" @@ (Core.name (string "unspecified")),
      P3._EnumValue_number>>: int32 0,
      P3._EnumValue_options>>: emptyList],
    "encodeEnumField">: "field" ~> "idx" ~> lets [
      "fname">: Core.fieldTypeName (var "field"),
      "ftype">: Core.fieldTypeType (var "field")] $
      "opts" <<~ (asTerm findOptions @@ var "cx" @@ var "g" @@ var "ftype") $
      right $ record P3._EnumValue [
        P3._EnumValue_name>>: asTerm encodeEnumValueName @@ var "tname" @@ var "fname",
        P3._EnumValue_number>>: var "idx",
        P3._EnumValue_options>>: var "opts"],
    "indices">: Math.range (int32 1) (Lists.length (var "fts"))] $
    "values" <<~ (Eithers.mapList
      ("p" ~> var "encodeEnumField" @@ (Pairs.first (var "p")) @@ (Pairs.second (var "p")))
      (Lists.zip (var "fts") (var "indices"))) $
    right $ record P3._EnumDefinition [
      P3._EnumDefinition_name>>: asTerm encodeTypeName @@ var "tname",
      P3._EnumDefinition_values>>: Lists.cons (var "unspecifiedField") (var "values"),
      P3._EnumDefinition_options>>: var "options"]

encodeEnumValueName :: TTermDefinition (Name -> Name -> P3.EnumValueName)
encodeEnumValueName = def "encodeEnumValueName" $
  doc "Encode an enum value name from type name and field name" $
  "tname" ~> "fname" ~> lets [
    "prefix">: Formatting.nonAlnumToUnderscores @@ (Formatting.convertCaseCamelToUpperSnake @@ (Names.localNameOf @@ var "tname")),
    "suffix">: Formatting.nonAlnumToUnderscores @@ (Formatting.convertCaseCamelToUpperSnake @@ (unwrap _Name @@ var "fname"))] $
    wrap P3._EnumValueName (Strings.cat (list [var "prefix", string "_", var "suffix"]))

-- =============================================================================
-- Field encoding
-- =============================================================================

encodeFieldName :: TTermDefinition (Bool -> Name -> P3.FieldName)
encodeFieldName = def "encodeFieldName" $
  doc "Encode a field name, optionally preserving the original case" $
  "preserve" ~> "name" ~>
    wrap P3._FieldName $
      Logic.ifElse (var "preserve")
        (unwrap _Name @@ var "name")
        (Formatting.convertCaseCamelToLowerSnake @@ (unwrap _Name @@ var "name"))

-- | Returns the field and updated context (for counter threading)
encodeFieldType :: TTermDefinition (Context -> Graph -> Namespace -> FieldType -> Either Error (P3.Field, Context))
encodeFieldType = def "encodeFieldType" $
  doc "Encode a Hydra field type as a Protobuf field" $
  "cx" ~> "g" ~> "localNs" ~> "ft" ~> lets [
    "fname">: Core.fieldTypeName (var "ft"),
    "ftype">: Core.fieldTypeType (var "ft"),
    "encodeType_">: "cx0" ~> "g0" ~> "ns0" ~> "typ" ~>
      cases _Type (asTerm simplifyType @@ var "typ")
        (Just $ Eithers.map
          ("st" ~> inject P3._FieldType P3._FieldType_simple (var "st"))
          (var "encodeSimpleType_" @@ var "cx0" @@ var "g0" @@ var "ns0" @@ true @@ var "typ")) [
        _Type_either>>: "et" ~> lets [
          "ref">: inject _StructuralTypeRef (Name "either") (pair (Core.eitherTypeLeft (var "et")) (Core.eitherTypeRight (var "et")))] $
          right $ inject P3._FieldType P3._FieldType_simple
            (inject P3._SimpleType P3._SimpleType_reference (asTerm structuralTypeName @@ var "ns0" @@ var "ref")),
        _Type_pair>>: "pt" ~> lets [
          "ref">: inject _StructuralTypeRef (Name "pair") (pair (Core.pairTypeFirst (var "pt")) (Core.pairTypeSecond (var "pt")))] $
          right $ inject P3._FieldType P3._FieldType_simple
            (inject P3._SimpleType P3._SimpleType_reference (asTerm structuralTypeName @@ var "ns0" @@ var "ref")),
        _Type_list>>: "lt" ~>
          Eithers.map
            ("st" ~> inject P3._FieldType P3._FieldType_repeated (var "st"))
            (var "encodeSimpleType_" @@ var "cx0" @@ var "g0" @@ var "ns0" @@ true @@ var "lt"),
        _Type_set>>: "setType" ~>
          Eithers.map
            ("st" ~> inject P3._FieldType P3._FieldType_repeated (var "st"))
            (var "encodeSimpleType_" @@ var "cx0" @@ var "g0" @@ var "ns0" @@ true @@ var "setType"),
        _Type_map>>: "mt" ~>
          "kt" <<~ (var "encodeSimpleType_" @@ var "cx0" @@ var "g0" @@ var "ns0" @@ false @@ (Core.mapTypeKeys (var "mt"))) $
          "vt" <<~ (var "encodeSimpleType_" @@ var "cx0" @@ var "g0" @@ var "ns0" @@ true @@ (Core.mapTypeValues (var "mt"))) $
          right $ inject P3._FieldType P3._FieldType_map
            (record P3._MapType [
              P3._MapType_keys>>: var "kt",
              P3._MapType_values>>: var "vt"]),
        _Type_maybe>>: "ot" ~> cases _Type (Strip.deannotateType @@ var "ot")
          (Just $ var "encodeType_" @@ var "cx0" @@ var "g0" @@ var "ns0" @@ var "ot") [
          _Type_literal>>: "lt" ~>
            Eithers.map
              ("st" ~> inject P3._FieldType P3._FieldType_simple (var "st"))
              (asTerm encodeScalarTypeWrapped @@ var "cx0" @@ var "lt")],
        _Type_union>>: "fts" ~>
          "pfields" <<~ (asTerm mapAccumResult @@
            ("cx_" ~> "f" ~> asTerm encodeFieldType @@ var "cx_" @@ var "g0" @@ var "ns0" @@ var "f") @@
            var "cx0" @@
            (var "fts")) $ lets [
            "fields_">: Pairs.first (var "pfields")] $
            right $ inject P3._FieldType P3._FieldType_oneof (var "fields_")],
    "encodeSimpleType_">: "cx0" ~> "g0" ~> "ns0" ~> "noms" ~> "typ" ~> lets [
      "forNominal">: "name" ~> right (inject P3._SimpleType P3._SimpleType_reference (asTerm encodeTypeReference @@ var "ns0" @@ var "name"))] $
      cases _Type (asTerm simplifyType @@ var "typ")
        (Just $ asTerm unexpectedE @@ var "cx0" @@ string "simple type" @@ (ShowCore.type_ @@ (Strip.removeTypeAnnotations @@ var "typ"))) [
        _Type_literal>>: "lt" ~>
          Eithers.map
            ("st" ~> inject P3._SimpleType P3._SimpleType_scalar (var "st"))
            (asTerm encodeScalarType @@ var "cx0" @@ var "lt"),
        _Type_record>>: constant $ asTerm unexpectedE @@ var "cx0" @@ string "named type reference" @@ string "anonymous record type",
        _Type_union>>: constant $ asTerm unexpectedE @@ var "cx0" @@ string "named type reference" @@ string "anonymous union type",
        _Type_unit>>: constant $ right (inject P3._SimpleType P3._SimpleType_reference (wrap P3._TypeName (string "google.protobuf.Empty"))),
        _Type_variable>>: "name" ~> Logic.ifElse (var "noms")
          (var "forNominal" @@ var "name")
          ("el" <<~ (Lexical.requireBinding @@ var "g0" @@ var "name") $ lets [
            "term">: Core.bindingTerm (var "el")] $
            Eithers.bind
              (Eithers.bimap
                ("de" ~> Error.errorOther (Error.otherError ((unwrap _DecodingError) @@ var "de")))
                ("t" ~> var "t")
                (decodeType @@ var "g0" @@ var "term"))
              ("resolvedTyp" ~> var "encodeSimpleType_" @@ var "cx0" @@ var "g0" @@ var "ns0" @@ var "noms" @@ var "resolvedTyp"))]] $
    "options" <<~ (asTerm findOptions @@ var "cx" @@ var "g" @@ var "ftype") $
    "ft_" <<~ (var "encodeType_" @@ var "cx" @@ var "g" @@ var "localNs" @@ var "ftype") $ lets [
      "idxPair">: Annotations.nextCount @@ asTerm key_proto_field_index @@ var "cx",
      "idx">: Pairs.first (var "idxPair"),
      "cx1">: Pairs.second (var "idxPair")] $
    "preserve" <<~ (asTerm readBooleanAnnotation @@ var "cx" @@ var "g" @@ Constants.key_preserveFieldName @@ var "ftype") $
    right $ pair
      (record P3._Field [
        P3._Field_name>>: asTerm encodeFieldName @@ var "preserve" @@ var "fname",
        P3._Field_jsonName>>: nothing,
        P3._Field_type>>: var "ft_",
        P3._Field_number>>: var "idx",
        P3._Field_options>>: var "options"])
      (var "cx1")

-- =============================================================================
-- Record encoding
-- =============================================================================

-- | Returns the message definition; counter is threaded via context
encodeRecordType :: TTermDefinition (Context -> Graph -> Namespace -> [P3.Option] -> Name -> [FieldType] -> Either Error P3.MessageDefinition)
encodeRecordType = def "encodeRecordType" $
  doc "Encode a Hydra record type as a Protobuf message definition" $
  "cx" ~> "g" ~> "localNs" ~> "options" ~> "tname" ~> "fts" ~>
    "result" <<~ (asTerm mapAccumResult @@
      ("cx_" ~> "f" ~> asTerm encodeFieldType @@ var "cx_" @@ var "g" @@ var "localNs" @@ var "f") @@
      var "cx" @@
      var "fts") $ lets [
      "pfields">: Pairs.first (var "result")] $
      right $ record P3._MessageDefinition [
        P3._MessageDefinition_name>>: asTerm encodeTypeName @@ var "tname",
        P3._MessageDefinition_fields>>: var "pfields",
        P3._MessageDefinition_options>>: var "options"]

-- =============================================================================
-- Scalar type encoding
-- =============================================================================

encodeScalarType :: TTermDefinition (Context -> LiteralType -> Either Error P3.ScalarType)
encodeScalarType = def "encodeScalarType" $
  doc "Encode a Hydra literal type as a Protobuf scalar type" $
  "cx" ~> "lt" ~>
    cases _LiteralType (var "lt")
      (Just $ asTerm unexpectedE @@ var "cx" @@ string "supported literal type" @@ (ShowCore.literalType @@ var "lt")) [
      _LiteralType_binary>>: constant $ right (inject P3._ScalarType P3._ScalarType_bytes unit),
      _LiteralType_boolean>>: constant $ right (inject P3._ScalarType P3._ScalarType_bool unit),
      _LiteralType_float>>: "ft" ~>
        cases _FloatType (var "ft")
          (Just $ asTerm unexpectedE @@ var "cx" @@ string "32-bit or 64-bit floating-point type" @@ (ShowCore.floatType @@ var "ft")) [
          _FloatType_float32>>: constant $ right (inject P3._ScalarType P3._ScalarType_float unit),
          _FloatType_float64>>: constant $ right (inject P3._ScalarType P3._ScalarType_double unit)],
      _LiteralType_integer>>: "it" ~>
        cases _IntegerType (var "it")
          (Just $ asTerm unexpectedE @@ var "cx" @@ string "32-bit or 64-bit integer type" @@ (ShowCore.integerType @@ var "it")) [
          _IntegerType_int32>>: constant $ right (inject P3._ScalarType P3._ScalarType_int32 unit),
          _IntegerType_int64>>: constant $ right (inject P3._ScalarType P3._ScalarType_int64 unit),
          _IntegerType_uint32>>: constant $ right (inject P3._ScalarType P3._ScalarType_uint32 unit),
          _IntegerType_uint64>>: constant $ right (inject P3._ScalarType P3._ScalarType_uint64 unit)],
      _LiteralType_string>>: constant $ right (inject P3._ScalarType P3._ScalarType_string unit)]

encodeScalarTypeWrapped :: TTermDefinition (Context -> LiteralType -> Either Error P3.SimpleType)
encodeScalarTypeWrapped = def "encodeScalarTypeWrapped" $
  doc "Encode a Hydra literal type as a wrapped Protobuf type (for optional scalars)" $
  "cx" ~> "lt" ~> lets [
    "toType">: "label" ~> right $
      inject P3._SimpleType P3._SimpleType_reference (wrap P3._TypeName (Strings.cat (list [string "google.protobuf.", var "label", string "Value"])))] $
    cases _LiteralType (var "lt")
      (Just $ asTerm unexpectedE @@ var "cx" @@ string "supported literal type" @@ (ShowCore.literalType @@ var "lt")) [
      _LiteralType_binary>>: constant $ var "toType" @@ string "Bytes",
      _LiteralType_boolean>>: constant $ var "toType" @@ string "Bool",
      _LiteralType_float>>: "ft" ~>
        cases _FloatType (var "ft")
          (Just $ asTerm unexpectedE @@ var "cx" @@ string "32-bit or 64-bit floating-point type" @@ (ShowCore.floatType @@ var "ft")) [
          _FloatType_float32>>: constant $ var "toType" @@ string "Float",
          _FloatType_float64>>: constant $ var "toType" @@ string "Double"],
      _LiteralType_integer>>: "it" ~>
        cases _IntegerType (var "it")
          (Just $ asTerm unexpectedE @@ var "cx" @@ string "32-bit or 64-bit integer type" @@ (ShowCore.integerType @@ var "it")) [
          _IntegerType_int32>>: constant $ var "toType" @@ string "Int32",
          _IntegerType_int64>>: constant $ var "toType" @@ string "Int64",
          _IntegerType_uint32>>: constant $ var "toType" @@ string "UInt32",
          _IntegerType_uint64>>: constant $ var "toType" @@ string "UInt64"],
      _LiteralType_string>>: constant $ var "toType" @@ string "String"]

-- =============================================================================
-- Type name encoding
-- =============================================================================

-- | Encode a simple type for helper message fields
encodeSimpleTypeForHelper :: TTermDefinition (Context -> Namespace -> Type -> Either Error P3.SimpleType)
encodeSimpleTypeForHelper = def "encodeSimpleTypeForHelper" $
  doc "Encode a simple type for helper message fields" $
  "cx" ~> "localNs" ~> "typ" ~> lets [
    "forNominal">: "name" ~> right (inject P3._SimpleType P3._SimpleType_reference (asTerm encodeTypeReference @@ var "localNs" @@ var "name"))] $
    cases _Type (asTerm simplifyType @@ var "typ")
      (Just $ asTerm unexpectedE @@ var "cx" @@ string "simple type in structural type helper" @@ (ShowCore.type_ @@ (Strip.removeTypeAnnotations @@ var "typ"))) [
      _Type_literal>>: "lt" ~>
        Eithers.map
          ("st" ~> inject P3._SimpleType P3._SimpleType_scalar (var "st"))
          (asTerm encodeScalarType @@ var "cx" @@ var "lt"),
      _Type_record>>: constant $ asTerm unexpectedE @@ var "cx" @@ string "named type reference" @@ string "anonymous record type",
      _Type_union>>: constant $ asTerm unexpectedE @@ var "cx" @@ string "named type reference" @@ string "anonymous union type",
      _Type_unit>>: constant $ right (inject P3._SimpleType P3._SimpleType_reference (wrap P3._TypeName (string "google.protobuf.Empty"))),
      _Type_variable>>: "name" ~> var "forNominal" @@ var "name"]

encodeTypeName :: TTermDefinition (Name -> P3.TypeName)
encodeTypeName = def "encodeTypeName" $
  doc "Encode a Hydra type name as a Protobuf type name" $
  "name" ~> wrap P3._TypeName (Names.localNameOf @@ var "name")

encodeTypeReference :: TTermDefinition (Namespace -> Name -> P3.TypeName)
encodeTypeReference = def "encodeTypeReference" $
  doc "Encode a Hydra name as a Protobuf type reference" $
  "localNs" ~> "name" ~> lets [
    "qn">: Names.qualifyName @@ var "name",
    "local">: Packaging.qualifiedNameLocal (var "qn"),
    "ns_">: Packaging.qualifiedNameNamespace (var "qn"),
    "localNsParts">: Maybes.fromMaybe (list ([] :: [TTerm String])) (Lists.maybeInit (Strings.splitOn (string ".") (unwrap _Namespace @@ var "localNs")))] $
    wrap P3._TypeName $
      Maybes.maybe
        (var "local")
        ("nsVal" ~> lets [
          "nsParts">: Maybes.fromMaybe (list ([] :: [TTerm String])) (Lists.maybeInit (Strings.splitOn (string ".") (unwrap _Namespace @@ var "nsVal")))] $
          Logic.ifElse (Equality.equal (var "nsParts") (var "localNsParts"))
            (var "local")
            (Strings.intercalate (string ".") (Lists.concat (list [var "nsParts", list [var "local"]]))))
        (var "ns_")

-- =============================================================================
-- Type flattening
-- =============================================================================

err :: TTermDefinition (Context -> String -> Either Error a)
err = def "err" $
  "cx" ~> "msg" ~>
  Ctx.failInContext (Error.errorOther $ Error.otherError (var "msg")) (var "cx")

findOptions :: TTermDefinition (Context -> Graph -> Type -> Either Error [P3.Option])
findOptions = def "findOptions" $
  doc "Find Protobuf options for a type (description and deprecated)" $
  "cx" ~> "g" ~> "typ" ~>
    "mdesc" <<~ (Annotations.getTypeDescription @@ var "cx" @@ var "g" @@ var "typ") $
    "bdep" <<~ (asTerm readBooleanAnnotation @@ var "cx" @@ var "g" @@ Constants.key_deprecated @@ var "typ") $ lets [
      "mdescAnn">: Maybes.map
        ("desc_" ~> record P3._Option [
          P3._Option_name>>: ProtobufSerdeSource.descriptionOptionName,
          P3._Option_value>>: inject P3._Value P3._Value_string (var "desc_")])
        (var "mdesc"),
      "mdepAnn">: Logic.ifElse (var "bdep")
        (just $ record P3._Option [
          P3._Option_name>>: string "deprecated",
          P3._Option_value>>: inject P3._Value P3._Value_boolean true])
        nothing] $
      right $ Maybes.cat (list [var "mdescAnn", var "mdepAnn"])

-- =============================================================================
-- Enum detection
-- =============================================================================

-- | Eliminate type lambdas and type applications, simply replacing type variables with the string type
flattenType :: TTermDefinition (Type -> Type)
flattenType = def "flattenType" $
  doc "Eliminate type lambdas and type applications, replacing type variables with the string type" $
  "typ" ~>
    Rewriting.rewriteType @@
      ("recurse" ~> "t" ~>
        cases _Type (var "t")
          (Just $ var "recurse" @@ var "t") [
          _Type_forall>>: "ft" ~> var "recurse" @@ (Variables.replaceFreeTypeVariable @@ (Core.forallTypeParameter (var "ft")) @@ (inject _Type _Type_literal (inject _LiteralType _LiteralType_string unit)) @@ (Core.forallTypeBody (var "ft"))),
          _Type_application>>: "at" ~> var "recurse" @@ (Core.applicationTypeFunction (var "at"))]) @@
      var "typ"

-- =============================================================================
-- Options
-- =============================================================================

fromEitherString :: TTermDefinition (Context -> Either String a -> Either Error a)
fromEitherString = def "fromEitherString" $
  "cx" ~> "e" ~>
  Eithers.bimap
    ("msg" ~> Error.errorOther (Error.otherError (var "msg")))
    ("a" ~> var "a")
    (var "e")

-- =============================================================================
-- StructuralTypeRef helpers
-- =============================================================================

-- | Generate a helper message definition for a structural type.
-- Returns the definition and the updated context (counter state).
generateStructuralTypeMessage :: TTermDefinition (Context -> Graph -> Namespace -> Term -> Either Error (P3.Definition, Context))
generateStructuralTypeMessage = def "generateStructuralTypeMessage" $
  doc "Generate a helper message definition for a structural type" $
  "cx" ~> "g" ~> "localNs" ~> "ref" ~> lets [
    "cx1">: Annotations.resetCount @@ asTerm key_proto_field_index @@ var "cx",
    "cx2">: Pairs.second (Annotations.nextCount @@ asTerm key_proto_field_index @@ var "cx1"),
    "makeField">: "cx0" ~> "fname" ~> "ftyp" ~>
      "ft" <<~ (asTerm encodeSimpleTypeForHelper @@ var "cx0" @@ var "localNs" @@ var "ftyp") $ lets [
        "idxPair">: Annotations.nextCount @@ asTerm key_proto_field_index @@ var "cx0",
        "idx">: Pairs.first (var "idxPair"),
        "cx1_">: Pairs.second (var "idxPair")] $
        right $ pair
          (record P3._Field [
            P3._Field_name>>: wrap P3._FieldName (var "fname"),
            P3._Field_jsonName>>: nothing,
            P3._Field_type>>: inject P3._FieldType P3._FieldType_simple (var "ft"),
            P3._Field_number>>: var "idx",
            P3._Field_options>>: emptyList])
          (var "cx1_")] $
    match _StructuralTypeRef Nothing [
      (Name "either")>>: "p" ~> lets [
        "lt">: Pairs.first (var "p"),
        "rt">: Pairs.second (var "p")] $
        "leftResult" <<~ (var "makeField" @@ var "cx2" @@ string "left" @@ var "lt") $
        "leftField" <~ Pairs.first (var "leftResult") $
        "cx3" <~ Pairs.second (var "leftResult") $
        "rightResult" <<~ (var "makeField" @@ var "cx3" @@ string "right" @@ var "rt") $
        "rightField" <~ Pairs.first (var "rightResult") $
        "cx4" <~ Pairs.second (var "rightResult") $
        right $ pair
          (inject P3._Definition P3._Definition_message $
            record P3._MessageDefinition [
              P3._MessageDefinition_name>>: asTerm structuralTypeName @@ var "localNs" @@ var "ref",
              P3._MessageDefinition_fields>>: list [var "leftField", var "rightField"],
              P3._MessageDefinition_options>>: emptyList])
          (var "cx4"),
      (Name "pair")>>: "p" ~> lets [
        "ft">: Pairs.first (var "p"),
        "st">: Pairs.second (var "p")] $
        "firstResult" <<~ (var "makeField" @@ var "cx2" @@ string "first" @@ var "ft") $
        "firstField" <~ Pairs.first (var "firstResult") $
        "cx3" <~ Pairs.second (var "firstResult") $
        "secondResult" <<~ (var "makeField" @@ var "cx3" @@ string "second" @@ var "st") $
        "secondField" <~ Pairs.first (var "secondResult") $
        "cx4" <~ Pairs.second (var "secondResult") $
        right $ pair
          (inject P3._Definition P3._Definition_message $
            record P3._MessageDefinition [
              P3._MessageDefinition_name>>: asTerm structuralTypeName @@ var "localNs" @@ var "ref",
              P3._MessageDefinition_fields>>: list [var "firstField", var "secondField"],
              P3._MessageDefinition_options>>: emptyList])
          (var "cx4")] @@ var "ref"

isEnumDefinition :: TTermDefinition (Type -> Bool)
isEnumDefinition = def "isEnumDefinition" $
  doc "Check if a type is an enum definition" $
  "typ" ~>
    cases _Type (asTerm simplifyType @@ var "typ")
      (Just false) [
      _Type_union>>: "fts" ~> asTerm isEnumFields @@ (var "fts")]

-- =============================================================================
-- Namespace conversion
-- =============================================================================

isEnumFields :: TTermDefinition ([FieldType] -> Bool)
isEnumFields = def "isEnumFields" $
  doc "Check if all fields are unit types (i.e., this is an enum)" $
  "fts" ~>
    Lists.foldl
      ("b" ~> "f" ~> Logic.and (var "b") (Predicates.isUnitType @@ (asTerm simplifyType @@ Core.fieldTypeType (var "f"))))
      true
      (var "fts")

javaMultipleFilesOptionName :: TTermDefinition String
javaMultipleFilesOptionName = def "javaMultipleFilesOptionName" $
  string "java_multiple_files"

javaPackageOptionName :: TTermDefinition String
javaPackageOptionName = def "javaPackageOptionName" $
  string "java_package"

-- =============================================================================
-- Module construction
-- =============================================================================

key_proto_field_index :: TTermDefinition Name
key_proto_field_index = def "key_proto_field_index" $
  Core.name (string "proto_field_index")

-- =============================================================================
-- Error helpers
-- =============================================================================

-- | Helper to thread context through a list, accumulating results
mapAccumResult :: TTermDefinition ((Context -> a -> Either Error (b, Context)) -> Context -> [a] -> Either Error ([b], Context))
mapAccumResult = def "mapAccumResult" $
  doc "Thread context through a list, accumulating results" $
  "f" ~> "cx0" ~> "xs" ~>
    Lists.foldl
      ("accE" ~> "x" ~>
        Eithers.bind (var "accE") ("accPair" ~> lets [
          "bs">: Pairs.first (var "accPair"),
          "cxN">: Pairs.second (var "accPair")] $
          Eithers.map
            ("resultPair" ~> pair
              (Lists.concat (list [var "bs", list [Pairs.first (var "resultPair")]]))
              (Pairs.second (var "resultPair")))
            (var "f" @@ var "cxN" @@ var "x")))
      (right $ pair emptyList (var "cx0"))
      (var "xs")

-- =============================================================================
-- Definition encoding
-- =============================================================================

-- | Note: follows the Protobuf Style Guide (https://protobuf.dev/programming-guides/style)
moduleToProtobuf :: TTermDefinition (Module -> [Definition] -> Context -> Graph -> Either Error (M.Map FilePath String))
moduleToProtobuf = def "moduleToProtobuf" $
  doc "Convert a Hydra module to Protocol Buffers v3 source files" $
  "mod" ~> "defs" ~> "cx" ~> "g" ~> lets [
    "ns_">: Packaging.moduleNamespace (var "mod"),
    "partitioned">: Environment.partitionDefinitions @@ var "defs",
    "typeDefs">: Pairs.first (var "partitioned")] $
    "pfile" <<~ (asTerm constructModule @@ var "cx" @@ var "g" @@ var "mod" @@ var "typeDefs") $ lets [
      "content">: Serialization.printExpr @@ (Serialization.parenthesize @@ (ProtobufSerdeSource.writeProtoFile @@ var "pfile")),
      "path">: unwrap P3._FileReference @@ (asTerm namespaceToFileReference @@ var "ns_")] $
      right $ Maps.singleton (var "path") (var "content")

-- =============================================================================
-- Option name constants
-- =============================================================================

namespaceToFileReference :: TTermDefinition (Namespace -> P3.FileReference)
namespaceToFileReference = def "namespaceToFileReference" $
  doc "Convert a Hydra namespace to a Protobuf file reference" $
  "ns_" ~> lets [
    "pns">: Strings.intercalate (string "/")
      (Lists.map (lambda "s" $ Formatting.convertCaseCamelToLowerSnake @@ var "s") (Strings.splitOn (string ".") (unwrap _Namespace @@ var "ns_")))] $
    wrap P3._FileReference (Strings.cat2 (var "pns") (string ".proto"))

namespaceToPackageName :: TTermDefinition (Namespace -> P3.PackageName)
namespaceToPackageName = def "namespaceToPackageName" $
  doc "Convert a Hydra namespace to a Protobuf package name" $
  "ns_" ~>
    wrap P3._PackageName $
      Strings.intercalate (string ".")
        (Lists.map
          (lambda "s" $ Formatting.convertCaseCamelToLowerSnake @@ var "s")
          (Maybes.fromMaybe (list ([] :: [TTerm String])) (Lists.maybeInit (Strings.splitOn (string ".") (unwrap _Namespace @@ var "ns_")))))

-- =============================================================================
-- Boolean annotation reading
-- =============================================================================

readBooleanAnnotation :: TTermDefinition (Context -> Graph -> Name -> Type -> Either Error Bool)
readBooleanAnnotation = def "readBooleanAnnotation" $
  doc "Read a boolean annotation from a type" $
  "cx" ~> "g" ~> "key" ~> "typ" ~>
    Maybes.maybe
      (right false)
      ("term" ~> ExtractCore.boolean @@ var "g" @@ var "term")
      (Maps.lookup (var "key") (Annotations.typeAnnotationInternal @@ var "typ"))

-- =============================================================================
-- Type simplification
-- =============================================================================

-- | Note: this should probably be done in the term adapters
simplifyType :: TTermDefinition (Type -> Type)
simplifyType = def "simplifyType" $
  doc "Simplify a type by removing annotations and unwrapping newtypes" $
  "typ" ~>
    cases _Type (Strip.deannotateType @@ var "typ")
      (Just $ Strip.deannotateType @@ var "typ") [
      _Type_wrap>>: "wt" ~> asTerm simplifyType @@ (var "wt")]

-- | Generate a message name for a structural type reference.
-- The StructuralTypeRef is represented as a tagged union with "either" and "pair" variants,
-- where each variant holds a pair of types (left/right or first/second).
structuralTypeName :: TTermDefinition (Namespace -> Term -> P3.TypeName)
structuralTypeName = def "structuralTypeName" $
  doc "Generate a message name for a structural type reference" $
  "localNs" ~> "ref" ~> lets [
    "typeSuffix">: "typ" ~> lets [
      "st">: asTerm simplifyType @@ var "typ"] $
      cases _Type (var "st")
        (Just $ string "value") [
        _Type_literal>>: "lt" ~>
          cases _LiteralType (var "lt") (Just $ string "value") [
            _LiteralType_binary>>: constant $ string "bytes",
            _LiteralType_boolean>>: constant $ string "bool",
            _LiteralType_float>>: "ft" ~>
              cases _FloatType (var "ft") (Just $ string "float") [
                _FloatType_float32>>: constant $ string "float",
                _FloatType_float64>>: constant $ string "double"],
            _LiteralType_integer>>: "it" ~>
              cases _IntegerType (var "it") (Just $ string "int64") [
                _IntegerType_int32>>: constant $ string "int32",
                _IntegerType_int64>>: constant $ string "int64",
                _IntegerType_uint32>>: constant $ string "uint32",
                _IntegerType_uint64>>: constant $ string "uint64"],
            _LiteralType_string>>: constant $ string "string"],
        _Type_record>>: constant $ string "record",
        _Type_union>>: constant $ string "union",
        _Type_variable>>: "name" ~> Names.localNameOf @@ var "name",
        _Type_unit>>: constant $ string "unit",
        _Type_list>>: constant $ string "list",
        _Type_set>>: constant $ string "set",
        _Type_map>>: constant $ string "map",
        _Type_maybe>>: constant $ string "maybe"]] $
    wrap P3._TypeName $
      match _StructuralTypeRef Nothing [
        (Name "either")>>: "p" ~>
          Strings.cat (list [string "Either_",
            var "typeSuffix" @@ (Pairs.first (var "p")),
            string "_",
            var "typeSuffix" @@ (Pairs.second (var "p"))]),
        (Name "pair")>>: "p" ~>
          Strings.cat (list [string "Pair_",
            var "typeSuffix" @@ (Pairs.first (var "p")),
            string "_",
            var "typeSuffix" @@ (Pairs.second (var "p"))])] @@ var "ref"

unexpectedE :: TTermDefinition (Context -> String -> String -> Either Error a)
unexpectedE = def "unexpectedE" $
  "cx" ~> "expected" ~> "found" ~>
  asTerm err @@ var "cx" @@ (Strings.cat (list [string "Expected ", var "expected", string ", found: ", var "found"]))
