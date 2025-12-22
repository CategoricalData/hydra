{-# LANGUAGE FlexibleContexts #-}

module Hydra.Sources.Kernel.Terms.Encoding where

-- Standard imports for kernel terms modules
import Hydra.Kernel hiding (literalType)
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Meta.Accessors     as Accessors
import qualified Hydra.Dsl.Annotations   as Annotations
import qualified Hydra.Dsl.Meta.Ast           as Ast
import qualified Hydra.Dsl.Bootstrap     as Bootstrap
import qualified Hydra.Dsl.Meta.Coders        as Coders
import qualified Hydra.Dsl.Meta.Compute       as Compute
import qualified Hydra.Dsl.Meta.Core          as Core
import qualified Hydra.Dsl.Meta.Grammar       as Grammar
import qualified Hydra.Dsl.Grammars      as Grammars
import qualified Hydra.Dsl.Meta.Graph         as Graph
import qualified Hydra.Dsl.Meta.Json          as Json
import qualified Hydra.Dsl.Meta.Lib.Chars     as Chars
import qualified Hydra.Dsl.Meta.Lib.Eithers   as Eithers
import qualified Hydra.Dsl.Meta.Lib.Equality  as Equality
import qualified Hydra.Dsl.Meta.Lib.Flows     as Flows
import qualified Hydra.Dsl.Meta.Lib.Lists     as Lists
import qualified Hydra.Dsl.Meta.Lib.Literals  as Literals
import qualified Hydra.Dsl.Meta.Lib.Logic     as Logic
import qualified Hydra.Dsl.Meta.Lib.Maps      as Maps
import qualified Hydra.Dsl.Meta.Lib.Math      as Math
import qualified Hydra.Dsl.Meta.Lib.Maybes    as Maybes
import qualified Hydra.Dsl.Meta.Lib.Pairs     as Pairs
import qualified Hydra.Dsl.Meta.Lib.Sets      as Sets
import           Hydra.Dsl.Meta.Lib.Strings   as Strings
import qualified Hydra.Dsl.Literals      as Literals
import qualified Hydra.Dsl.LiteralTypes  as LiteralTypes
import qualified Hydra.Dsl.Meta.Base     as MetaBase
import qualified Hydra.Dsl.Meta.Terms    as MetaTerms
import qualified Hydra.Dsl.Meta.Types    as MetaTypes
import qualified Hydra.Dsl.Meta.Module        as Module
import qualified Hydra.Dsl.Meta.Phantoms as Phantoms
import           Hydra.Dsl.Meta.Phantoms as Phantoms hiding (
  elimination, field, fieldType, floatType, floatValue, function, injection, integerType, integerValue, lambda, literal,
  literalType, record, term, type_, typeScheme, wrap)
import qualified Hydra.Dsl.Prims         as Prims
import qualified Hydra.Dsl.Tabular       as Tabular
import qualified Hydra.Dsl.Meta.Testing       as Testing
import qualified Hydra.Dsl.Terms         as Terms
import qualified Hydra.Dsl.Tests         as Tests
import qualified Hydra.Dsl.Meta.Topology      as Topology
import qualified Hydra.Dsl.Types         as Types
import qualified Hydra.Dsl.Meta.Typing        as Typing
import qualified Hydra.Dsl.Meta.Util          as Util
import qualified Hydra.Dsl.Meta.Variants      as Variants
import           Hydra.Sources.Kernel.Types.All
import qualified Hydra.Sources.Kernel.Terms.Annotations as Annotations
import qualified Hydra.Sources.Kernel.Terms.Decode.Core as DecodeCore
import qualified Hydra.Sources.Kernel.Terms.Formatting as Formatting
import qualified Hydra.Sources.Kernel.Terms.Names as Names
import qualified Hydra.Sources.Kernel.Terms.Schemas as Schemas
import qualified Hydra.Dsl.Meta.DeepCore as DC
import           Hydra.Dsl.Meta.DeepCore ((@@@))
import           Prelude hiding ((++))
import qualified Data.Int                as I
import qualified Data.List               as L
import qualified Data.Map                as M
import qualified Data.Set                as S
import qualified Data.Maybe              as Y


ns :: Namespace
ns = Namespace "hydra.encoding"

module_ :: Module
module_ = Module ns elements
    [Annotations.ns, DecodeCore.ns, Formatting.ns, Names.ns, Schemas.ns]
    kernelTypesNamespaces $
    Just "Functions for generating term encoders from type modules"
  where
    elements = [
      toBinding encodeBinding,
      toBinding encodeBindingName,
      toBinding encodeFieldValue,
      toBinding encodeFloatValue,
      toBinding encodeInjection,
      toBinding encodeIntegerValue,
      toBinding encodeListType,
      toBinding encodeLiteralType,
      toBinding encodeEitherType,
      toBinding encodeForallType,
      toBinding encodeMapType,
      toBinding encodeOptionalType,
      toBinding encodePairType,
      toBinding encodeModule,
      toBinding encodeName,
      toBinding encodeNamespace,
      toBinding encodeRecordType,
      toBinding encodeSetType,
      toBinding encodeType,
      toBinding encodeUnionType,
      toBinding encodeWrappedType,
      toBinding filterTypeBindings,
      toBinding isEncodableBinding,
      toBinding isUnitType_]

define :: String -> TTerm x -> TBinding x
define = definitionInModule module_

-- | Encode a single type binding into an encoder binding
-- This decodes the term to a Type, then generates an encoder function
encodeBinding :: TBinding (Binding -> Flow Graph Binding)
encodeBinding = define "encodeBinding" $
  doc "Transform a type binding into an encoder binding" $
  "b" ~>
    Flows.bind (DecodeCore.type_ @@ (Core.bindingTerm (var "b"))) (
      "typ" ~>
      Flows.pure (Core.binding
        (encodeBindingName @@ (Core.bindingName (var "b")))
        (encodeType @@ (var "typ"))
        nothing))

-- | Generate a fully qualified binding name for an encoder function from a type name
-- For example, "hydra.core.Name" -> "hydra.encode.core.name"
-- For local types (no namespace), returns just the decapitalized local name
encodeBindingName :: TBinding (Name -> Name)
encodeBindingName = define "encodeBindingName" $
  doc "Generate a binding name for an encoder function from a type name" $
  "n" ~>
    -- Check if name has a namespace (contains ".")
    Logic.ifElse (Logic.not (Lists.null
      (Lists.tail (Strings.splitOn (string ".") (Core.unName (var "n"))))))
      -- Qualified type: e.g., "hydra.core.Name" -> "hydra.encode.core.name"
      (Core.name (
        Strings.intercalate (string ".") (
          Lists.concat2
            (list [string "hydra", string "encode"])
            (Lists.concat2
              (Lists.tail (Lists.init (Strings.splitOn (string ".") (Core.unName (var "n")))))
              (list [Formatting.decapitalize @@ (Names.localNameOf @@ (var "n"))])))))
      -- Local type: just decapitalize
      (Core.name (Formatting.decapitalize @@ (Names.localNameOf @@ (var "n"))))

-- | Generate the encoder term for a field value
-- Creates a lambda that encodes the field value and wraps in an encoded union/injection
encodeFieldValue :: TBinding (Name -> Name -> Type -> Term)
encodeFieldValue = define "encodeFieldValue" $
  doc "Generate the encoder for a field's value" $
  "typeName" ~> "fieldName" ~> "fieldType" ~>
    -- Create a lambda that encodes the value and wraps in Term.union with injection
    DC.lambda "v" $
      -- Build Term.union containing an encoded Injection with the encoded value
      DC.injection _Term (DC.field _Term_union
        (encodeInjection @@ var "typeName" @@ var "fieldName"
          @@ ((encodeType @@ var "fieldType") @@@ DC.var "v")))

-- | Encode an Injection as a Term (produces a Record of type hydra.core.Injection)
encodeInjection :: TBinding (Name -> Name -> Term -> Term)
encodeInjection = define "encodeInjection" $
  doc "Encode an Injection as a term" $
  "typeName" ~> "fieldName" ~> "fieldTerm" ~> DC.record _Injection [
    DC.field _Injection_typeName (encodeName @@ var "typeName"),
    DC.field _Injection_field (encodeField @@ var "fieldName" @@ var "fieldTerm")]
  where
    -- Encode a Field as a Term (produces a Record of type hydra.core.Field)
    encodeField :: TTerm (Name -> Term -> Term)
    encodeField = "fname" ~> "fterm" ~> DC.record _Field [
      DC.field _Field_name (encodeName @@ var "fname"),
      DC.field _Field_term (var "fterm")]

-- | Generate an encoder for a literal type
-- For literals, the input is a native Haskell value (e.g., String, Int32).
-- We need to wrap it in encoded TermLiteral with the appropriate Literal constructor.
encodeLiteralType :: TBinding (LiteralType -> Term)
encodeLiteralType = define "encodeLiteralType" $
  doc "Generate an encoder for a literal type" $
  match _LiteralType (Just identityEncoder) [
    _LiteralType_binary>>: constant $
      DC.lambda "x" $ termLiteral $ DC.injection _Literal (DC.field _Literal_binary (DC.var "x")),
    _LiteralType_boolean>>: constant $
      DC.lambda "x" $ termLiteral $ DC.injection _Literal (DC.field _Literal_boolean (DC.var "x")),
    _LiteralType_string>>: constant $
      DC.lambda "x" $ termLiteral $ DC.injection _Literal (DC.field _Literal_string (DC.var "x")),
    -- For integer types, wrap in Term.literal.integer with the specific integer variant
    _LiteralType_integer>>: "intType" ~>
      DC.lambda "x" $ termLiteral $ DC.injection _Literal
        (DC.field _Literal_integer (encodeIntegerValue @@ var "intType" @@ DC.var "x")),
    -- For float types, wrap in Term.literal.float with the specific float variant
    _LiteralType_float>>: "floatType" ~>
      DC.lambda "x" $ termLiteral $ DC.injection _Literal
        (DC.field _Literal_float (encodeFloatValue @@ var "floatType" @@ DC.var "x"))]
  where
    -- Helper to wrap a Literal value in Term.literal
    termLiteral lit = DC.injection _Term (DC.field _Term_literal lit)
    -- Default: identity (should not be reached for well-formed types)
    identityEncoder = DC.lambda "x" $ DC.var "x"

-- | Transform a type module into an encoder module
-- Returns Nothing if the module has no encodable type definitions
encodeModule :: TBinding (Module -> Flow Graph (Maybe Module))
encodeModule = define "encodeModule" $
  doc "Transform a type module into an encoder module" $
  "mod" ~>
    "typeBindings" <<~ (filterTypeBindings @@ (Module.moduleElements (var "mod"))) $
    Logic.ifElse (Lists.null (var "typeBindings"))
      (Flows.pure nothing)
      (Flows.bind (Flows.mapList encodeBinding (var "typeBindings")) (
        "encodedBindings" ~>
        -- The encoder module depends on encoder modules for the type dependencies
        -- E.g., hydra.encode.module depends on hydra.encode.core
        Flows.pure (just (Module.module_
          (encodeNamespace @@ (Module.moduleNamespace (var "mod")))
          (var "encodedBindings")
          -- Transform each type dependency namespace to its encoder namespace
          (primitive _lists_map @@ encodeNamespace @@ (Module.moduleTypeDependencies (var "mod")))
          -- The encoder module depends on the original type module
          (list [Module.moduleNamespace (var "mod")])
          (just (Strings.cat $ list [
            string "Term encoders for ",
            Module.unNamespace (Module.moduleNamespace (var "mod"))]))))))

-- | Encode a Name as a Term (produces a wrapped term of type hydra.core.Name)
encodeName :: TBinding (Name -> Term)
encodeName = define "encodeName" $
  doc "Encode a Name as a term" $
  "n" ~> DC.wrap _Name (DC.string (Core.unName (var "n")))

-- | Generate an encoder module namespace from a source module namespace
-- For example, "hydra.util" -> "hydra.encode.util"
encodeNamespace :: TBinding (Namespace -> Namespace)
encodeNamespace = define "encodeNamespace" $
  doc "Generate an encoder module namespace from a source module namespace" $
  "ns" ~> (
    Module.namespace (
      Strings.cat $ list [
        string "hydra.encode.",
        Strings.intercalate (string ".")
          (Lists.tail (Strings.splitOn (string ".") (Module.unNamespace (var "ns"))))]))

-- | Generate an encoder for a record type
-- For records, project each field, encode it, and build an encoded record
encodeRecordType :: TBinding (RowType -> Term)
encodeRecordType = define "encodeRecordType" $
  doc "Generate an encoder for a record type" $
  "rt" ~>
    DC.lambda "x" $
      DC.injection _Term (DC.field _Term_record
        (DC.record _Record [
          DC.field _Record_typeName (encodeName @@ Core.rowTypeTypeName (var "rt")),
          DC.field _Record_fields
            (DC.list (primitive _lists_map @@ (encodeRecordField @@ var "rt") @@ Core.rowTypeFields (var "rt")))]))
  where
    -- Helper to encode a single record field
    -- Takes the record type name and a field type, produces an encoded Field term
    encodeRecordField :: TTerm (RowType -> FieldType -> Term)
    encodeRecordField =
      "recType" ~> "ft" ~>
        DC.record _Field [
          DC.field _Field_name (encodeName @@ Core.fieldTypeName (var "ft")),
          DC.field _Field_term
            ((encodeType @@ Core.fieldTypeType (var "ft"))
              @@@ (projectField (Core.rowTypeTypeName (var "recType")) (Core.fieldTypeName (var "ft"))
                    @@@ DC.var "x"))]

    -- Helper to create a field projection term
    projectField typeName fieldName =
      Core.termFunction $ Core.functionElimination $ Core.eliminationRecord $
        Core.projection typeName fieldName

-- | Generate an encoder term for a given Type
-- This generates a function that encodes values of the type to Terms
encodeType :: TBinding (Type -> Term)
encodeType = define "encodeType" $
  doc "Generate an encoder term for a Type" $
  match _Type (Just identityEncoder) [
    _Type_annotated>>: "at" ~>
      -- Strip annotation and recurse
      encodeType @@ Core.annotatedTypeBody (var "at"),
    _Type_application>>: "appType" ~>
      -- For type applications like (DataRow v), apply the function encoder to the argument encoder
      (encodeType @@ Core.applicationTypeFunction (var "appType"))
        @@@ (encodeType @@ Core.applicationTypeArgument (var "appType")),
    _Type_either>>: "et" ~>
      encodeEitherType @@ var "et",
    _Type_forall>>: "ft" ~>
      encodeForallType @@ var "ft",
    _Type_function>>: constant $
      -- For function types, use identity encoder since functions can't be serialized as data
      identityEncoder,
    _Type_list>>: "elemType" ~>
      encodeListType @@ var "elemType",
    _Type_literal>>: "lt" ~>
      encodeLiteralType @@ var "lt",
    _Type_map>>: "mt" ~>
      encodeMapType @@ var "mt",
    _Type_maybe>>: "elemType" ~>
      encodeOptionalType @@ var "elemType",
    _Type_pair>>: "pt" ~>
      encodePairType @@ var "pt",
    _Type_record>>: "rt" ~>
      encodeRecordType @@ var "rt",
    _Type_set>>: "elemType" ~>
      encodeSetType @@ var "elemType",
    _Type_union>>: "rt" ~>
      encodeUnionType @@ var "rt",
    _Type_wrap>>: "wt" ~>
      encodeWrappedType @@ var "wt",
    _Type_unit>>: constant $
      -- For unit type, return a lambda that ignores input and produces encoded unit term
      DC.lambda "_" $ DC.injection _Term (DC.field _Term_unit DC.unit),
    _Type_variable>>: "typeName" ~>
      -- For type variables (references to other types), generate a reference to that
      -- type's encoder. Uses encodeBindingName which produces fully qualified names.
      Core.termVariable (encodeBindingName @@ var "typeName")]
  where
    identityEncoder = DC.lambda "x" $ DC.var "x"

-- | Generate an encoder for a union type (including enums)
-- Generates a case match over all variants
encodeUnionType :: TBinding (RowType -> Term)
encodeUnionType = define "encodeUnionType" $
  doc "Generate an encoder for a union type" $
  "rt" ~>
    Core.termFunction $ Core.functionElimination $ Core.eliminationUnion $
      Core.caseStatement
        (Core.rowTypeTypeName (var "rt"))
        nothing
        (primitive _lists_map @@
          ("ft" ~> Core.field
            (Core.fieldTypeName (var "ft"))
            (encodeFieldValue
              @@ Core.rowTypeTypeName (var "rt")
              @@ Core.fieldTypeName (var "ft")
              @@ Core.fieldTypeType (var "ft")))
          @@ Core.rowTypeFields (var "rt"))

-- | Generate an encoder for a wrapped type
-- Unwraps the value, encodes it, and wraps in encoded TermWrap
encodeWrappedType :: TBinding (WrappedType -> Term)
encodeWrappedType = define "encodeWrappedType" $
  doc "Generate an encoder for a wrapped type" $
  "wt" ~>
    DC.lambda "x" $
      DC.injection _Term (DC.field _Term_wrap
        (DC.record _WrappedTerm [
          DC.field _WrappedTerm_typeName (encodeName @@ Core.wrappedTypeTypeName (var "wt")),
          DC.field _WrappedTerm_body
            ((encodeType @@ Core.wrappedTypeBody (var "wt"))
              @@@ (DC.unwrapDynamic (Core.wrappedTypeTypeName (var "wt")) @@@ DC.var "x"))]))

-- | Filter bindings to only encodable type definitions
-- A binding is encodable if it is a native type AND is serializable (no function types in dependencies)
filterTypeBindings :: TBinding ([Binding] -> Flow Graph [Binding])
filterTypeBindings = define "filterTypeBindings" $
  doc "Filter bindings to only encodable type definitions" $
  "bindings" ~>
    -- First filter to native types, then check serializability for each
    Flows.map (primitive _maybes_cat) $
      Flows.mapList isEncodableBinding $
        primitive _lists_filter @@ Annotations.isNativeType @@ var "bindings"

-- | Check if a binding is encodable and return Just binding if so, Nothing otherwise
isEncodableBinding :: TBinding (Binding -> Flow Graph (Maybe Binding))
isEncodableBinding = define "isEncodableBinding" $
  doc "Check if a binding is encodable (serializable type)" $
  "b" ~>
    Flows.map
      ("serializable" ~> Logic.ifElse (var "serializable") (just (var "b")) nothing)
      (Schemas.isSerializableByName @@ (Core.bindingName (var "b")))

-- | Check whether a type is the unit type
isUnitType_ :: TBinding (Type -> Bool)
isUnitType_ = define "isUnitType" $
  doc "Check whether a type is the unit type" $
  match _Type (Just $ false) [
    _Type_unit>>: constant true]

-- | Encode an integer value based on its integer type
-- Wraps the value in the appropriate IntegerValue variant as an injection
encodeIntegerValue :: TBinding (IntegerType -> Term -> Term)
encodeIntegerValue = define "encodeIntegerValue" $
  doc "Encode an integer value based on its integer type" $
  "intType" ~> "valTerm" ~>
    Core.termUnion $ Core.injection
      (Core.nameLift _IntegerValue)
      (Core.field (intTypeToFieldName @@ var "intType") (var "valTerm"))
  where
    intTypeToFieldName :: TTerm (IntegerType -> Name)
    intTypeToFieldName = match _IntegerType Nothing [
      _IntegerType_bigint>>: constant $ Core.nameLift _IntegerValue_bigint,
      _IntegerType_int8>>:   constant $ Core.nameLift _IntegerValue_int8,
      _IntegerType_int16>>:  constant $ Core.nameLift _IntegerValue_int16,
      _IntegerType_int32>>:  constant $ Core.nameLift _IntegerValue_int32,
      _IntegerType_int64>>:  constant $ Core.nameLift _IntegerValue_int64,
      _IntegerType_uint8>>:  constant $ Core.nameLift _IntegerValue_uint8,
      _IntegerType_uint16>>: constant $ Core.nameLift _IntegerValue_uint16,
      _IntegerType_uint32>>: constant $ Core.nameLift _IntegerValue_uint32,
      _IntegerType_uint64>>: constant $ Core.nameLift _IntegerValue_uint64]

-- | Encode a float value based on its float type
-- Wraps the value in the appropriate FloatValue variant as an injection
encodeFloatValue :: TBinding (FloatType -> Term -> Term)
encodeFloatValue = define "encodeFloatValue" $
  doc "Encode a float value based on its float type" $
  "floatType" ~> "valTerm" ~>
    Core.termUnion $ Core.injection
      (Core.nameLift _FloatValue)
      (Core.field (floatTypeToFieldName @@ var "floatType") (var "valTerm"))
  where
    floatTypeToFieldName :: TTerm (FloatType -> Name)
    floatTypeToFieldName = match _FloatType Nothing [
      _FloatType_bigfloat>>: constant $ Core.nameLift _FloatValue_bigfloat,
      _FloatType_float32>>:  constant $ Core.nameLift _FloatValue_float32,
      _FloatType_float64>>:  constant $ Core.nameLift _FloatValue_float64]

-- | Generate an encoder for a list type
-- Maps the element encoder over the list and wraps in Term.list
encodeListType :: TBinding (Type -> Term)
encodeListType = define "encodeListType" $
  doc "Generate an encoder for a list type" $
  "elemType" ~>
    DC.lambda "xs" $
      DC.injection _Term (DC.field _Term_list
        (DC.primitiveEncoded _lists_map @@@ (encodeType @@ var "elemType") @@@ DC.var "xs"))

-- | Generate an encoder for a map type
-- Encodes each key/value pair and wraps in Term.map
encodeMapType :: TBinding (MapType -> Term)
encodeMapType = define "encodeMapType" $
  doc "Generate an encoder for a map type" $
  "mt" ~>
    DC.lambda "m" $
      DC.injection _Term (DC.field _Term_map
        (DC.primitiveEncoded _maps_bimap
          @@@ (encodeType @@ Core.mapTypeKeys (var "mt"))
          @@@ (encodeType @@ Core.mapTypeValues (var "mt"))
          @@@ DC.var "m"))

-- | Generate an encoder for an Either type
-- Generates a case match over Left/Right variants
encodeEitherType :: TBinding (EitherType -> Term)
encodeEitherType = define "encodeEitherType" $
  doc "Generate an encoder for an Either type" $
  "et" ~>
    DC.lambda "e" $
      DC.injection _Term (DC.field _Term_either
        (DC.primitiveEncoded _eithers_bimap
          @@@ (encodeType @@ Core.eitherTypeLeft (var "et"))
          @@@ (encodeType @@ Core.eitherTypeRight (var "et"))
          @@@ DC.var "e"))

-- | Generate an encoder for a polymorphic (forall) type
-- For a type like `forall a. T[a]`, generates a lambda that takes an encoder for `a`
-- and returns an encoder for the body type `T[a]`
encodeForallType :: TBinding (ForallType -> Term)
encodeForallType = define "encodeForallType" $
  doc "Generate an encoder for a polymorphic (forall) type" $
  "ft" ~>
    -- Generate a lambda that takes an encoder for the type parameter
    Core.termFunction $ Core.functionLambda $
      Core.lambda
        (encodeBindingName @@ Core.forallTypeParameter (var "ft"))
        nothing
        (encodeType @@ Core.forallTypeBody (var "ft"))

-- | Generate an encoder for an optional type
-- Encodes the inner value if present and wraps in Term.optional
encodeOptionalType :: TBinding (Type -> Term)
encodeOptionalType = define "encodeOptionalType" $
  doc "Generate an encoder for an optional type" $
  "elemType" ~> DC.lambda "opt" $
    DC.injection _Term (DC.field _Term_maybe
      (DC.primitiveEncoded _maybes_map @@@ (encodeType @@ var "elemType") @@@ DC.var "opt"))

-- | Generate an encoder for a pair type
-- Encodes both elements and wraps in Term.pair
encodePairType :: TBinding (PairType -> Term)
encodePairType = define "encodePairType" $
  doc "Generate an encoder for a pair type" $
  "pt" ~> DC.lambda "p" $ DC.injection _Term $ DC.field _Term_pair $ DC.primitiveEncoded _pairs_bimap
    @@@ (encodeType @@ Core.pairTypeFirst (var "pt"))
    @@@ (encodeType @@ Core.pairTypeSecond (var "pt"))
    @@@ DC.var "p"

-- | Generate an encoder for a set type
-- Encodes each element and wraps in Term.set
encodeSetType :: TBinding (Type -> Term)
encodeSetType = define "encodeSetType" $
  doc "Generate an encoder for a set type" $
  "elemType" ~> DC.lambda "s" $
    DC.injection _Term (DC.field _Term_set
      (DC.primitiveEncoded _sets_map @@@ (encodeType @@ var "elemType") @@@ DC.var "s"))
