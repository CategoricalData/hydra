{-# LANGUAGE FlexibleContexts #-}

module Hydra.Sources.Kernel.Terms.Decoding where

-- Standard imports for kernel terms modules
import Hydra.Kernel hiding (literalType, matchRecord, matchUnion)
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
  elimination, field, fieldType, floatType, floatValue, function, injection, integerType, integerValue, literal,
  literalType, record, term, type_, typeScheme)
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
import qualified Hydra.Sources.Kernel.Terms.Monads as Monads
import qualified Hydra.Sources.Kernel.Terms.Names as Names
import qualified Hydra.Sources.Kernel.Terms.Rewriting as Rewriting
import qualified Hydra.Sources.Kernel.Terms.Schemas as Schemas
import qualified Hydra.Sources.Kernel.Terms.Show.Core as ShowCore
import           Prelude hiding ((++))
import qualified Data.Int                as I
import qualified Data.List               as L
import qualified Data.Map                as M
import qualified Data.Set                as S
import qualified Data.Maybe              as Y

import qualified Hydra.Dsl.Meta.DeepCore as DC
import           Hydra.Dsl.Meta.DeepCore ((@@@))


ns :: Namespace
ns = Namespace "hydra.decoding"

module_ :: Module
module_ = Module ns elements
    [Annotations.ns, DecodeCore.ns, Formatting.ns, Monads.ns, Names.ns, Rewriting.ns, Schemas.ns, ShowCore.ns]
    kernelTypesNamespaces $
    Just "Functions for generating term decoders from type modules"
  where
    elements = [
      toBinding decodeBinding,
      toBinding decodeBindingName,
      toBinding decodeLiteralType,
      toBinding decodeModule,
      toBinding decodeNamespace,
      toBinding decodeRecordType,
      toBinding decodeType,
      toBinding decodeUnionType,
      toBinding decodeUnitType,
      toBinding decodeWrappedType,
      toBinding decoderResultType,
      toBinding decoderTypeScheme,
      toBinding filterTypeBindings,
      toBinding isDecodableBinding]

define :: String -> TTerm x -> TBinding x
define = definitionInModule module_

--------------------------------------------------------------------------------
-- Helper functions
--------------------------------------------------------------------------------

-- | Cast a term from one phantom type to another (the underlying Term is unchanged)
castTerm :: TTerm a -> TTerm b
castTerm (TTerm t) = TTerm t

-- | Make a decoding error from a string message
decodingError :: TTerm String -> TTerm DecodingError
decodingError msg = Phantoms.wrap _DecodingError msg

--------------------------------------------------------------------------------
-- Main decoder functions
--------------------------------------------------------------------------------

-- | Compute the result type for a decoder based on the input type
-- Returns the domain type name for the decoded value
decoderResultType :: TBinding (Type -> Name)
decoderResultType = define "decoderResultType" $
  doc "Compute the result type name for a decoder" $
  match _Type (Just (Core.nameLift _Term)) [
    _Type_annotated>>: "at" ~>
      decoderResultType @@ (Core.annotatedTypeBody (var "at")),
    _Type_literal>>: "_" ~>
      Core.nameLift _Literal,
    _Type_record>>: "rt" ~>
      Core.rowTypeTypeName (var "rt"),
    _Type_union>>: "rt" ~>
      Core.rowTypeTypeName (var "rt"),
    _Type_wrap>>: "wt" ~>
      Core.wrappedTypeTypeName (var "wt")]

-- | Build a decoder type scheme: Term -> Either DecodingError ResultType
decoderTypeScheme :: TBinding (Type -> TypeScheme)
decoderTypeScheme = define "decoderTypeScheme" $
  doc "Build type scheme for a decoder function" $
  "typ" ~>
    -- TypeScheme with no type variables (Either is not parameterized by state)
    Core.typeScheme
      (list ([] :: [TTerm Name]))
      (Core.typeFunction $ Core.functionType
        (Core.typeVariable (Core.nameLift _Term))
        (Core.typeEither $ Core.eitherType
          (Core.typeVariable (Core.nameLift _DecodingError))
          (Core.typeVariable (decoderResultType @@ var "typ"))))

-- | Decode a single type binding into a decoder binding
-- Uses Flow to decode the Type from the binding's term, then generates decoder
decodeBinding :: TBinding (Binding -> Flow Graph Binding)
decodeBinding = define "decodeBinding" $
  doc "Transform a type binding into a decoder binding" $
  "b" ~>
    Flows.bind (DecodeCore.type_ @@ (Core.bindingTerm (var "b"))) $
      "typ" ~>
      Flows.pure (Core.binding
        (decodeBindingName @@ (Core.bindingName (var "b")))
        (decodeType @@ (var "typ"))
        (just (decoderTypeScheme @@ var "typ")))

-- | Generate a fully qualified binding name for a decoder function from a type name
-- For example, "hydra.util.CaseConvention" -> "hydra.decodeNew.util.caseConvention"
decodeBindingName :: TBinding (Name -> Name)
decodeBindingName = define "decodeBindingName" $
  doc "Generate a binding name for a decoder function from a type name" $
  "n" ~>
    -- Check if name has a namespace (contains ".")
    Logic.ifElse (Logic.not (Lists.null
      (Lists.tail (Strings.splitOn (string ".") (Core.unName (var "n"))))))
      -- Qualified type: e.g., "hydra.util.CaseConvention" -> "hydra.decodeNew.util.caseConvention"
      (Core.name (
        Strings.intercalate (string ".") (
          Lists.concat2
            (list [string "hydra", string "decodeNew"])
            (Lists.concat2
              (Lists.tail (Lists.init (Strings.splitOn (string ".") (Core.unName (var "n")))))
              (list [Formatting.decapitalize @@ (Names.localNameOf @@ (var "n"))])))))
      -- Local type: just decapitalize
      (Core.name (Formatting.decapitalize @@ (Names.localNameOf @@ (var "n"))))

-- | Helper to create a decoding error term from a message (object-level)
-- Returns: Term.wrap (WrappedTerm "hydra.util.DecodingError" (Term.literal (Literal.string msg)))
decodingErrorTerm :: TTerm String -> TTerm Term
decodingErrorTerm msg = DC.wrap _DecodingError $ DC.string msg

-- | Helper to create a Left (error) result term
-- Returns: Term.either (Left (decodingErrorTerm msg))
leftError :: TTerm String -> TTerm Term
leftError msg = DC.left $ decodingErrorTerm msg

-- | Generate a decoder for a literal type
-- Match on the LiteralType to generate type-specific decoders
decodeLiteralType :: TBinding (LiteralType -> Term)
decodeLiteralType = define "decodeLiteralType" $
  doc "Generate a decoder for a literal type" $
  match _LiteralType Nothing [
    _LiteralType_binary>>: constant decodeBinary,
    _LiteralType_boolean>>: constant decodeBoolean,
    _LiteralType_float>>: "ft" ~> decodeFloat (var "ft"),
    _LiteralType_integer>>: "it" ~> decodeInteger (var "it"),
    _LiteralType_string>>: constant decodeString]
  where
    -- Helper to wrap a Literal handler with Term.literal matching
    decodeLiteral handleLiteral = DC.match _Term
      (just $ leftError (string "expected literal")) [
      DC.field _Term_literal $ DC.lambda "v" $ handleLiteral @@@ DC.var "v"]

    -- Decode binary: Term -> Either DecodingError Binary
    decodeBinary = decodeLiteral $ DC.match _Literal
      (just $ leftError (string "expected binary literal")) [
      DC.field _Literal_binary $ DC.lambda "b" $ DC.right $ DC.var "b"]

    -- Decode boolean: Term -> Either DecodingError Bool
    decodeBoolean = decodeLiteral $ DC.match _Literal
      (just $ leftError (string "expected boolean literal")) [
      DC.field _Literal_boolean $ DC.lambda "b" $ DC.right $ DC.var "b"]

    -- Decode float: Term -> Either DecodingError <specific float type>
    decodeFloat ft = match _FloatType Nothing [
        _FloatType_bigfloat>>: constant $ decodeFloatVariant _FloatValue_bigfloat (string "bigfloat"),
        _FloatType_float32>>: constant $ decodeFloatVariant _FloatValue_float32 (string "float32"),
        _FloatType_float64>>: constant $ decodeFloatVariant _FloatValue_float64 (string "float64")]
      @@ ft

    -- Helper to decode a specific float variant
    decodeFloatVariant floatVariant floatName = decodeLiteral $ DC.match _Literal
      (just $ leftError (Strings.cat $ list [string "expected ", floatName, string " literal"])) [
      DC.field _Literal_float $ DC.match _FloatValue
        (just $ leftError (Strings.cat $ list [string "expected ", floatName, string " value"])) [
        DC.field floatVariant $ DC.lambda "f" $ DC.right $ DC.var "f"]]

    -- Decode integer: Term -> Either DecodingError <specific integer type>
    decodeInteger it = match _IntegerType Nothing [
        _IntegerType_bigint>>: constant $ decodeIntegerVariant _IntegerValue_bigint (string "bigint"),
        _IntegerType_int8>>: constant $ decodeIntegerVariant _IntegerValue_int8 (string "int8"),
        _IntegerType_int16>>: constant $ decodeIntegerVariant _IntegerValue_int16 (string "int16"),
        _IntegerType_int32>>: constant $ decodeIntegerVariant _IntegerValue_int32 (string "int32"),
        _IntegerType_int64>>: constant $ decodeIntegerVariant _IntegerValue_int64 (string "int64"),
        _IntegerType_uint8>>: constant $ decodeIntegerVariant _IntegerValue_uint8 (string "uint8"),
        _IntegerType_uint16>>: constant $ decodeIntegerVariant _IntegerValue_uint16 (string "uint16"),
        _IntegerType_uint32>>: constant $ decodeIntegerVariant _IntegerValue_uint32 (string "uint32"),
        _IntegerType_uint64>>: constant $ decodeIntegerVariant _IntegerValue_uint64 (string "uint64")]
      @@ it

    -- Helper to decode a specific integer variant
    decodeIntegerVariant intVariant intName = decodeLiteral $ DC.match _Literal
      (just $ leftError (Strings.cat $ list [string "expected ", intName, string " literal"])) [
      DC.field _Literal_integer $ DC.match _IntegerValue
        (just $ leftError (Strings.cat $ list [string "expected ", intName, string " value"])) [
        DC.field intVariant $ DC.lambda "i" $ DC.right $ DC.var "i"]]

    -- Decode string: Term -> Either DecodingError String
    decodeString = decodeLiteral $ DC.match _Literal
      (just $ leftError (string "expected string literal")) [
      DC.field _Literal_string $ DC.lambda "s" $ DC.right $ DC.var "s"]

-- | Transform a type module into a decoder module
-- Returns Nothing if the module has no decodable type definitions
decodeModule :: TBinding (Module -> Flow Graph (Maybe Module))
decodeModule = define "decodeModule" $
  doc "Transform a type module into a decoder module" $
  "mod" ~>
    "typeBindings" <<~ (filterTypeBindings @@ (Module.moduleElements (var "mod"))) $
    Logic.ifElse (Lists.null (var "typeBindings"))
      (Flows.pure nothing)
      (Flows.bind (Flows.mapList decodeBinding (var "typeBindings")) $
        "decodedBindings" ~>
        Flows.pure (just (Module.module_
          (decodeNamespace @@ (Module.moduleNamespace (var "mod")))
          (var "decodedBindings")
          (primitive _lists_map @@ decodeNamespace @@ (Module.moduleTypeDependencies (var "mod")))
          (list [Module.moduleNamespace (var "mod")])
          (just (Strings.cat $ list [
            string "Term decoders for ",
            Module.unNamespace (Module.moduleNamespace (var "mod"))])))))

-- | Generate a decoder module namespace from a source module namespace
-- For example, "hydra.util" -> "hydra.decodeNew.util"
decodeNamespace :: TBinding (Namespace -> Namespace)
decodeNamespace = define "decodeNamespace" $
  doc "Generate a decoder module namespace from a source module namespace" $
  "ns" ~> (
    Module.namespace (
      Strings.cat $ list [
        string "hydra.decodeNew.",
        Strings.intercalate (string ".")
          (Lists.tail (Strings.splitOn (string ".") (Module.unNamespace (var "ns"))))]))

-- | Generate a decoder for a record type
-- Matches Term.record and extracts the Record with the expected type name
-- Returns Either DecodingError <RecordType>
decodeRecordType :: TBinding (RowType -> Term)
decodeRecordType = define "decodeRecordType" $
  doc "Generate a decoder for a record type" $
  "rt" ~> DC.match _Term
    (just $ leftError (
      Strings.cat $ list [string "expected record of type ", Core.unName (Core.rowTypeTypeName (var "rt"))])) [
    DC.field _Term_record $ DC.lambda "record" $
      DC.string $ string "foo"]



-- | Generate a decoder term for a given Type
decodeType :: TBinding (Type -> Term)
decodeType = define "decodeType" $
  doc "Generate a decoder term for a Type" $
  match _Type (Just $ DC.lambda "t" $ leftError $ string "unsupported type variant") [
    _Type_annotated>>: "at" ~> decodeType @@ (Core.annotatedTypeBody (var "at")),
    _Type_literal>>: "lt" ~> decodeLiteralType @@ var "lt",
    _Type_record>>: "rt" ~> decodeRecordType @@ var "rt",
    _Type_union>>: "rt" ~> decodeUnionType @@ var "rt",
    _Type_unit>>: constant decodeUnitType,
    _Type_wrap>>: "wt" ~> decodeWrappedType @@ var "wt",
    _Type_variable>>: "typeName" ~> Core.termVariable (decodeBindingName @@ var "typeName")]

-- | Generate a decoder for a union type (including enums)
-- Uses the RowType's type name in error messages and the case statement.
-- Returns Either DecodingError <UnionType>
decodeUnionType :: TBinding (RowType -> Term)
decodeUnionType = define "decodeUnionType" $
  doc "Generate a decoder for a union type" $
  "rt" ~>
  "typeName" <~ (Core.rowTypeTypeName $ var "rt") $
  "toVariantPair" <~ ("ft" ~>
    DC.pair
      (DC.wrap _Name $ DC.string $ Core.unName $ Core.fieldTypeName $ var "ft")
      (DC.lambda "term" $ DC.primitive _eithers_map
        @@@ (DC.lambda "t" $ Core.termUnion $ Core.injection (var "typeName") $ Core.field (Core.fieldTypeName $ var "ft") $ DC.var "t")
        @@@ ((decodeType @@ (Core.fieldTypeType $ var "ft")) @@@ DC.var "term"))) $
  DC.match _Term
    (just $ leftError $
      Strings.cat $ list [string "expected union of type ", Core.unName (var "typeName")]) [
    DC.field _Term_union $ DC.lambda "inj" $ DC.lets [
      ("tname", DC.project _Injection _Injection_typeName @@@ DC.var "inj"),
      ("field", DC.project _Injection _Injection_field @@@ DC.var "inj"),
      ("fname", DC.project _Field _Field_name @@@ DC.var "field"),
      ("fterm", DC.project _Field _Field_term @@@ DC.var "field"),
      ("variantMap", DC.primitive _maps_fromList
        @@@ (DC.list $ Lists.map (var "toVariantPair") $ Core.rowTypeFields $ var "rt"))] $

      DC.primitive _maybes_maybe
        @@@ (DC.left $ DC.wrap _DecodingError $ DC.primitive _strings_cat
          @@@ (DC.list $ list [
            DC.string $ string "no such field ",
            DC.unwrap _Name @@@ DC.var "fname",
            DC.string $ string " in union type ",
            DC.unwrap _Name @@@ DC.var "tname"]))
        @@@ (DC.lambda "f" $ DC.var "f" @@@ DC.var "fterm")
        @@@ (DC.primitive _maps_lookup
          @@@ DC.var "fname"
          @@@ DC.var "variantMap")]

decodeUnitType :: TBinding Term
decodeUnitType = define "decodeUnitType" $
  doc "Generate a decoder for the unit type" $
  DC.match _Term
    (just $ leftError $ string "expected a unit value") [
    DC.field _Term_unit $ DC.constant $ DC.right DC.unit]

-- | Generate a decoder for a wrapped type
-- Matches on Term.Wrap, decodes the body, and wraps the result
-- Returns Either DecodingError WrappedType
decodeWrappedType :: TBinding (WrappedType -> Term)
decodeWrappedType = define "decodeWrappedType" $
  doc "Generate a decoder for a wrapped type" $
  "wt" ~> DC.match _Term
    (just $ leftError (
      Strings.cat $ list [string "expected wrapped type ", Core.unName (Core.wrappedTypeTypeName (var "wt"))])) [
    DC.field _Term_wrap $ DC.lambda "wrappedTerm" $
      DC.primitive _eithers_map
        @@@ (DC.lambda "b" $ DC.wrapDynamic (Core.wrappedTypeTypeName $ var "wt") (DC.var "b"))
        @@@ ((decodeType @@ Core.wrappedTypeBody (var "wt"))
          @@@ (DC.project _WrappedTerm _WrappedTerm_body @@@ DC.var "wrappedTerm"))]

-- | Filter bindings to only decodable type definitions
filterTypeBindings :: TBinding ([Binding] -> Flow Graph [Binding])
filterTypeBindings = define "filterTypeBindings" $
  doc "Filter bindings to only decodable type definitions" $
  "bindings" ~>
    Flows.map (primitive _maybes_cat) $
      Flows.mapList isDecodableBinding $
        primitive _lists_filter @@ Annotations.isNativeType @@ var "bindings"

-- | Check if a binding is decodable and return Just binding if so, Nothing otherwise
isDecodableBinding :: TBinding (Binding -> Flow Graph (Maybe Binding))
isDecodableBinding = define "isDecodableBinding" $
  doc "Check if a binding is decodable (serializable type)" $
  "b" ~>
    Flows.map
      ("serializable" ~> Logic.ifElse (var "serializable") (just (var "b")) nothing)
      (Schemas.isSerializableByName @@ (Core.bindingName (var "b")))
