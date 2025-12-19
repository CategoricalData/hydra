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
import           Prelude hiding ((++))
import qualified Data.Int                as I
import qualified Data.List               as L
import qualified Data.Map                as M
import qualified Data.Set                as S
import qualified Data.Maybe              as Y


module_ :: Module
module_ = Module (Namespace "hydra.encoding") elements
    [Annotations.module_, DecodeCore.module_, Formatting.module_, Names.module_, Schemas.module_]
    kernelTypesModules $
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
      toBinding isUnitType]

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
-- For example, "hydra.core.Name" -> "hydra.encodeNew.core.name"
-- For local types (no namespace), returns just the decapitalized local name
encodeBindingName :: TBinding (Name -> Name)
encodeBindingName = define "encodeBindingName" $
  doc "Generate a binding name for an encoder function from a type name" $
  "n" ~>
    -- Check if name has a namespace (contains ".")
    Logic.ifElse (Logic.not (Lists.null
      (Lists.tail (Strings.splitOn (string ".") (Core.unName (var "n"))))))
      -- Qualified type: e.g., "hydra.core.Name" -> "hydra.encodeNew.core.name"
      (Core.name (
        Strings.intercalate (string ".") (
          Lists.concat2
            (list [string "hydra", string "encodeNew"])
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
    Core.termFunction $ Core.functionLambda $
      Core.lambda
        (Core.name (string "v"))
        nothing
        -- Build Term.union containing an encoded Injection with the encoded value
        (Core.termUnion $ Core.injection
          (Core.nameLift _Term)
          (Core.field (Core.nameLift _Term_union)
            (encodeInjection
              @@ (var "typeName")
              @@ (var "fieldName")
              @@ (Core.termApplication $ Core.application
                  (encodeType @@ var "fieldType")
                  (Core.termVariable (Core.name (string "v")))))))

-- | Encode an Injection as a Term (produces a Record of type hydra.core.Injection)
encodeInjection :: TBinding (Name -> Name -> Term -> Term)
encodeInjection = define "encodeInjection" $
  doc "Encode an Injection as a term" $
  "typeName" ~> "fieldName" ~> "fieldTerm" ~>
    Core.termRecord $ Core.record
      (Core.nameLift _Injection)
      (list [
        Core.field (Core.nameLift _Injection_typeName) (encodeName @@ var "typeName"),
        Core.field (Core.nameLift _Injection_field) (encodeField
          @@ (var "fieldName")
          @@ (var "fieldTerm"))])
  where
    -- Encode a Field as a Term (produces a Record of type hydra.core.Field)
    encodeField :: TTerm (Name -> Term -> Term)
    encodeField =
      "fname" ~> "fterm" ~>
        Core.termRecord $ Core.record
          (Core.nameLift _Field)
          (list [
            Core.field (Core.nameLift _Field_name) (encodeName @@ var "fname"),
            Core.field (Core.nameLift _Field_term) (var "fterm")])

-- | Generate an encoder for a literal type
-- For literals, the input is a native Haskell value (e.g., String, Int32).
-- We need to wrap it in encoded TermLiteral with the appropriate Literal constructor.
encodeLiteralType :: TBinding (LiteralType -> Term)
encodeLiteralType = define "encodeLiteralType" $
  doc "Generate an encoder for a literal type" $
  match _LiteralType (Just identityEncoder) [
    _LiteralType_binary>>: "_" ~>
      Core.termFunction $ Core.functionLambda $
        Core.lambda (Core.name (string "x")) nothing $
          Core.termUnion $ Core.injection
            (Core.nameLift _Term)
            (Core.field (Core.nameLift _Term_literal)
              (Core.termUnion $ Core.injection
                (Core.nameLift _Literal)
                (Core.field (Core.nameLift _Literal_binary)
                  (Core.termVariable (Core.name (string "x")))))),
    _LiteralType_boolean>>: "_" ~>
      Core.termFunction $ Core.functionLambda $
        Core.lambda (Core.name (string "x")) nothing $
          Core.termUnion $ Core.injection
            (Core.nameLift _Term)
            (Core.field (Core.nameLift _Term_literal)
              (Core.termUnion $ Core.injection
                (Core.nameLift _Literal)
                (Core.field (Core.nameLift _Literal_boolean)
                  (Core.termVariable (Core.name (string "x")))))),
    _LiteralType_string>>: "_" ~>
      Core.termFunction $ Core.functionLambda $
        Core.lambda (Core.name (string "x")) nothing $
          Core.termUnion $ Core.injection
            (Core.nameLift _Term)
            (Core.field (Core.nameLift _Term_literal)
              (Core.termUnion $ Core.injection
                (Core.nameLift _Literal)
                (Core.field (Core.nameLift _Literal_string)
                  (Core.termVariable (Core.name (string "x")))))),
    -- For integer types, wrap in Term.literal.integer with the specific integer variant
    _LiteralType_integer>>: "intType" ~>
      Core.termFunction $ Core.functionLambda $
        Core.lambda (Core.name (string "x")) nothing $
          Core.termUnion $ Core.injection
            (Core.nameLift _Term)
            (Core.field (Core.nameLift _Term_literal)
              (Core.termUnion $ Core.injection
                (Core.nameLift _Literal)
                (Core.field (Core.nameLift _Literal_integer)
                  (encodeIntegerValue @@ var "intType" @@ (Core.termVariable (Core.name (string "x"))))))),
    -- For float types, wrap in Term.literal.float with the specific float variant
    _LiteralType_float>>: "floatType" ~>
      Core.termFunction $ Core.functionLambda $
        Core.lambda (Core.name (string "x")) nothing $
          Core.termUnion $ Core.injection
            (Core.nameLift _Term)
            (Core.field (Core.nameLift _Term_literal)
              (Core.termUnion $ Core.injection
                (Core.nameLift _Literal)
                (Core.field (Core.nameLift _Literal_float)
                  (encodeFloatValue @@ var "floatType" @@ (Core.termVariable (Core.name (string "x")))))))]
  where
    -- Default: identity (should not be reached for well-formed types)
    identityEncoder =
      Core.termFunction $ Core.functionLambda $
        Core.lambda (Core.name (string "x")) nothing $
          Core.termVariable (Core.name (string "x"))

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
        -- E.g., hydra.encodeNew.module depends on hydra.encodeNew.core
        Flows.pure (just (Module.module_
          (encodeNamespace @@ (Module.moduleNamespace (var "mod")))
          (var "encodedBindings")
          (primitive _lists_map @@ encodeModuleDependency @@ (Module.moduleTypeDependencies (var "mod")))
          (list [var "mod"])
          (just (Strings.cat $ list [
            string "Term encoders for ",
            Module.unNamespace (Module.moduleNamespace (var "mod"))]))))))
  where
    -- Create a placeholder encoder module reference for a type dependency
    -- This just transforms the namespace, the actual module content isn't needed for imports
    encodeModuleDependency :: TTerm (Module -> Module)
    encodeModuleDependency =
      "depMod" ~>
        Module.module_
          (encodeNamespace @@ (Module.moduleNamespace (var "depMod")))
          (list ([] :: [TTerm Binding]))
          (list ([] :: [TTerm Module]))
          (list ([] :: [TTerm Module]))
          nothing

-- | Encode a Name as a Term (produces a wrapped term of type hydra.core.Name)
encodeName :: TBinding (Name -> Term)
encodeName = define "encodeName" $
  doc "Encode a Name as a term" $
  "n" ~>
    Core.termWrap $ Core.wrappedTerm
      (Core.nameLift _Name)
      (Core.termLiteral $ Core.literalString (Core.unName (var "n")))

-- | Generate an encoder module namespace from a source module namespace
-- For example, "hydra.util" -> "hydra.encodeNew.util"
encodeNamespace :: TBinding (Namespace -> Namespace)
encodeNamespace = define "encodeNamespace" $
  doc "Generate an encoder module namespace from a source module namespace" $
  "ns" ~> (
    Module.namespace (
      Strings.cat $ list [
        string "hydra.encodeNew.",
        Strings.intercalate (string ".")
          (Lists.tail (Strings.splitOn (string ".") (Module.unNamespace (var "ns"))))]))

-- | Generate an encoder for a record type
-- For records, project each field, encode it, and build an encoded record
encodeRecordType :: TBinding (RowType -> Term)
encodeRecordType = define "encodeRecordType" $
  doc "Generate an encoder for a record type" $
  "rt" ~>
    Core.termFunction $ Core.functionLambda $
      Core.lambda (Core.name (string "x")) nothing $
        Core.termUnion $ Core.injection
          (Core.nameLift _Term)
          (Core.field (Core.nameLift _Term_record)
            (Core.termRecord $ Core.record
              (Core.nameLift _Record)
              (list [
                Core.field (Core.nameLift _Record_typeName)
                  (encodeName @@ (Core.rowTypeTypeName (var "rt"))),
                Core.field (Core.nameLift _Record_fields)
                  (Core.termList (primitive _lists_map @@ (encodeRecordField @@ var "rt") @@ (Core.rowTypeFields (var "rt"))))])))
  where
    -- Helper to encode a single record field
    -- Takes the record type name and a field type, produces an encoded Field term
    encodeRecordField :: TTerm (RowType -> FieldType -> Term)
    encodeRecordField =
      "recType" ~> "ft" ~>
        Core.termRecord $ Core.record
          (Core.nameLift _Field)
          (list [
            Core.field (Core.nameLift _Field_name)
              (encodeName @@ (Core.fieldTypeName (var "ft"))),
            Core.field (Core.nameLift _Field_term)
              (Core.termApplication $ Core.application
                (encodeType @@ (Core.fieldTypeType (var "ft")))
                (Core.termApplication $ Core.application
                  (Core.termFunction $ Core.functionElimination $ Core.eliminationRecord $
                    Core.projection (Core.rowTypeTypeName (var "recType")) (Core.fieldTypeName (var "ft")))
                  (Core.termVariable (Core.name (string "x")))))])

-- | Generate an encoder term for a given Type
-- This generates a function that encodes values of the type to Terms
encodeType :: TBinding (Type -> Term)
encodeType = define "encodeType" $
  doc "Generate an encoder term for a Type" $
  match _Type (Just identityEncoder) [
    _Type_annotated>>: "at" ~>
      -- Strip annotation and recurse
      encodeType @@ (Core.annotatedTypeBody (var "at")),
    _Type_application>>: "appType" ~>
      -- For type applications like (DataRow v), apply the function encoder to the argument encoder
      -- encodeType(DataRow) @@ encodeType(v)
      Core.termApplication $ Core.application
        (encodeType @@ (Core.applicationTypeFunction (var "appType")))
        (encodeType @@ (Core.applicationTypeArgument (var "appType"))),
    _Type_either>>: "et" ~>
      encodeEitherType @@ var "et",
    _Type_forall>>: "ft" ~>
      encodeForallType @@ var "ft",
    _Type_function>>: "_" ~>
      -- For function types, use identity encoder since functions can't be serialized as data
      -- The function is passed through as-is (wrapped in TermFunction at the type level)
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
    _Type_unit>>: "_" ~>
      -- For unit type, return a lambda that ignores input and produces encoded unit term
      Core.termFunction $ Core.functionLambda $
        Core.lambda
          (Core.name (string "_"))
          nothing
          (Core.termUnion $ Core.injection
            (Core.nameLift _Term)
            (Core.field (Core.nameLift _Term_unit) Core.termUnit)),
    _Type_variable>>: "typeName" ~>
      -- For type variables (references to other types), generate a reference to that
      -- type's encoder. Uses encodeBindingName which produces fully qualified names
      -- for cross-module references.
      Core.termVariable (encodeBindingName @@ var "typeName")]
  where
    identityEncoder =
      Core.termFunction $ Core.functionLambda $
        Core.lambda
          (Core.name (string "x"))
          nothing
          (Core.termVariable (Core.name (string "x")))

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
              @@ (Core.rowTypeTypeName (var "rt"))
              @@ (Core.fieldTypeName (var "ft"))
              @@ (Core.fieldTypeType (var "ft"))))
          @@ (Core.rowTypeFields (var "rt")))

-- | Generate an encoder for a wrapped type
-- Unwraps the value, encodes it, and wraps in encoded TermWrap
encodeWrappedType :: TBinding (WrappedType -> Term)
encodeWrappedType = define "encodeWrappedType" $
  doc "Generate an encoder for a wrapped type" $
  "wt" ~>
    Core.termFunction $ Core.functionLambda $
      Core.lambda (Core.name (string "x")) nothing $
        Core.termUnion $ Core.injection
          (Core.nameLift _Term)
          (Core.field (Core.nameLift _Term_wrap)
            (Core.termRecord $ Core.record
              (Core.nameLift _WrappedTerm)
              (list [
                Core.field (Core.nameLift _WrappedTerm_typeName)
                  (encodeName @@ (Core.wrappedTypeTypeName (var "wt"))),
                Core.field (Core.nameLift _WrappedTerm_body)
                  (Core.termApplication $ Core.application
                    (encodeType @@ (Core.wrappedTypeBody (var "wt")))
                    (Core.termApplication $ Core.application
                      (Core.termFunction $ Core.functionElimination $
                        Core.eliminationWrap (Core.wrappedTypeTypeName (var "wt")))
                      (Core.termVariable (Core.name (string "x")))))])))

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
isUnitType :: TBinding (Type -> Bool)
isUnitType = define "isUnitType" $
  doc "Check whether a type is the unit type" $
  match _Type (Just $ false) [
    _Type_unit>>: "_" ~> true]

-- | Encode an integer value based on its integer type
-- Wraps the value in the appropriate IntegerValue variant as an injection
encodeIntegerValue :: TBinding (IntegerType -> Term -> Term)
encodeIntegerValue = define "encodeIntegerValue" $
  doc "Encode an integer value based on its integer type" $
  "intType" ~> "valTerm" ~>
    Core.termUnion $ Core.injection
      (Core.nameLift _IntegerValue)
      (Core.field
        (intTypeToFieldName @@ var "intType")
        (var "valTerm"))
  where
    -- Map IntegerType to the corresponding field name for IntegerValue
    intTypeToFieldName :: TTerm (IntegerType -> Name)
    intTypeToFieldName = match _IntegerType Nothing [
      _IntegerType_bigint>>: "_" ~> Core.nameLift _IntegerValue_bigint,
      _IntegerType_int8>>:   "_" ~> Core.nameLift _IntegerValue_int8,
      _IntegerType_int16>>:  "_" ~> Core.nameLift _IntegerValue_int16,
      _IntegerType_int32>>:  "_" ~> Core.nameLift _IntegerValue_int32,
      _IntegerType_int64>>:  "_" ~> Core.nameLift _IntegerValue_int64,
      _IntegerType_uint8>>:  "_" ~> Core.nameLift _IntegerValue_uint8,
      _IntegerType_uint16>>: "_" ~> Core.nameLift _IntegerValue_uint16,
      _IntegerType_uint32>>: "_" ~> Core.nameLift _IntegerValue_uint32,
      _IntegerType_uint64>>: "_" ~> Core.nameLift _IntegerValue_uint64]

-- | Encode a float value based on its float type
-- Wraps the value in the appropriate FloatValue variant as an injection
encodeFloatValue :: TBinding (FloatType -> Term -> Term)
encodeFloatValue = define "encodeFloatValue" $
  doc "Encode a float value based on its float type" $
  "floatType" ~> "valTerm" ~>
    Core.termUnion $ Core.injection
      (Core.nameLift _FloatValue)
      (Core.field
        (floatTypeToFieldName @@ var "floatType")
        (var "valTerm"))
  where
    -- Map FloatType to the corresponding field name for FloatValue
    floatTypeToFieldName :: TTerm (FloatType -> Name)
    floatTypeToFieldName = match _FloatType Nothing [
      _FloatType_bigfloat>>: "_" ~> Core.nameLift _FloatValue_bigfloat,
      _FloatType_float32>>:  "_" ~> Core.nameLift _FloatValue_float32,
      _FloatType_float64>>:  "_" ~> Core.nameLift _FloatValue_float64]

-- | Generate an encoder for a list type
-- Maps the element encoder over the list and wraps in Term.list
encodeListType :: TBinding (Type -> Term)
encodeListType = define "encodeListType" $
  doc "Generate an encoder for a list type" $
  "elemType" ~>
    -- Generate a lambda term that applies lists.map at runtime to encode each element
    -- The result is a Term representing: \xs -> Term.list (lists.map elemEncoder xs)
    Core.termFunction $ Core.functionLambda $
      Core.lambda (Core.name (string "xs")) nothing $
        Core.termUnion $ Core.injection
          (Core.nameLift _Term)
          (Core.field (Core.nameLift _Term_list)
            -- Apply lists.map primitive at runtime to map the element encoder over the input list
            (Core.termApplication $ Core.application
              (Core.termApplication $ Core.application
                (Core.termFunction $ Core.functionPrimitive $ encodedName _lists_map)
                (encodeType @@ var "elemType"))  -- The element encoder
              (Core.termVariable (Core.name (string "xs")))))

-- | Generate an encoder for a map type
-- Encodes each key/value pair and wraps in Term.map
encodeMapType :: TBinding (MapType -> Term)
encodeMapType = define "encodeMapType" $
  doc "Generate an encoder for a map type" $
  "mt" ~>
    Core.termFunction $ Core.functionLambda $
      Core.lambda (Core.name (string "m")) nothing $
        Core.termUnion $ Core.injection
          (Core.nameLift _Term)
          (Core.field (Core.nameLift _Term_map)
            -- Apply maps.bimap primitive at runtime to encode keys and values
            (Core.termApplication $ Core.application
              (Core.termApplication $ Core.application
                (Core.termApplication $ Core.application
                  (Core.termFunction $ Core.functionPrimitive $ encodedName _maps_bimap)
                  (encodeType @@ (Core.mapTypeKeys (var "mt"))))    -- Key encoder
                (encodeType @@ (Core.mapTypeValues (var "mt"))))  -- Value encoder
              (Core.termVariable (Core.name (string "m")))))

-- | Generate an encoder for an Either type
-- Generates a case match over Left/Right variants
encodeEitherType :: TBinding (EitherType -> Term)
encodeEitherType = define "encodeEitherType" $
  doc "Generate an encoder for an Either type" $
  "et" ~>
    Core.termFunction $ Core.functionLambda $
      Core.lambda (Core.name (string "e")) nothing $
        Core.termUnion $ Core.injection
          (Core.nameLift _Term)
          (Core.field (Core.nameLift _Term_either)
            -- Apply eithers.bimap primitive at runtime to encode Left/Right values
            (Core.termApplication $ Core.application
              (Core.termApplication $ Core.application
                (Core.termApplication $ Core.application
                  (Core.termFunction $ Core.functionPrimitive $ encodedName _eithers_bimap)
                  (encodeType @@ (Core.eitherTypeLeft (var "et"))))   -- Left encoder
                (encodeType @@ (Core.eitherTypeRight (var "et"))))  -- Right encoder
              (Core.termVariable (Core.name (string "e")))))

-- | Generate an encoder for a polymorphic (forall) type
-- For a type like `forall a. T[a]`, generates a lambda that takes an encoder for `a`
-- and returns an encoder for the body type `T[a]`
encodeForallType :: TBinding (ForallType -> Term)
encodeForallType = define "encodeForallType" $
  doc "Generate an encoder for a polymorphic (forall) type" $
  "ft" ~>
    -- Generate a lambda that takes an encoder for the type parameter
    -- The parameter name is the type variable name (e.g., "a" -> "encodeA" or just use the variable name)
    Core.termFunction $ Core.functionLambda $
      Core.lambda
        (encodeBindingName @@ (Core.forallTypeParameter (var "ft")))  -- parameter encoder name
        nothing
        -- The body is the encoder for the inner type, which can reference the parameter encoder
        (encodeType @@ (Core.forallTypeBody (var "ft")))

-- | Generate an encoder for an optional type
-- Encodes the inner value if present and wraps in Term.optional
encodeOptionalType :: TBinding (Type -> Term)
encodeOptionalType = define "encodeOptionalType" $
  doc "Generate an encoder for an optional type" $
  "elemType" ~>
    Core.termFunction $ Core.functionLambda $
      Core.lambda (Core.name (string "opt")) nothing $
        Core.termUnion $ Core.injection
          (Core.nameLift _Term)
          (Core.field (Core.nameLift _Term_maybe)
            -- Apply maybes.map primitive at runtime to encode the value if present
            (Core.termApplication $ Core.application
              (Core.termApplication $ Core.application
                (Core.termFunction $ Core.functionPrimitive $ encodedName _maybes_map)
                (encodeType @@ var "elemType"))  -- Element encoder
              (Core.termVariable (Core.name (string "opt")))))

-- | Generate an encoder for a pair type
-- Encodes both elements and wraps in Term.pair
encodePairType :: TBinding (PairType -> Term)
encodePairType = define "encodePairType" $
  doc "Generate an encoder for a pair type" $
  "pt" ~>
    Core.termFunction $ Core.functionLambda $
      Core.lambda (Core.name (string "p")) nothing $
        Core.termUnion $ Core.injection
          (Core.nameLift _Term)
          (Core.field (Core.nameLift _Term_pair)
            -- Apply pairs.bimap primitive at runtime to encode both elements
            (Core.termApplication $ Core.application
              (Core.termApplication $ Core.application
                (Core.termApplication $ Core.application
                  (Core.termFunction $ Core.functionPrimitive $ encodedName _pairs_bimap)
                  (encodeType @@ (Core.pairTypeFirst (var "pt"))))   -- First encoder
                (encodeType @@ (Core.pairTypeSecond (var "pt"))))  -- Second encoder
              (Core.termVariable (Core.name (string "p")))))

-- | Generate an encoder for a set type
-- Encodes each element and wraps in Term.set
encodeSetType :: TBinding (Type -> Term)
encodeSetType = define "encodeSetType" $
  doc "Generate an encoder for a set type" $
  "elemType" ~>
    Core.termFunction $ Core.functionLambda $
      Core.lambda (Core.name (string "s")) nothing $
        Core.termUnion $ Core.injection
          (Core.nameLift _Term)
          (Core.field (Core.nameLift _Term_set)
            -- Apply sets.map primitive at runtime to encode each element
            (Core.termApplication $ Core.application
              (Core.termApplication $ Core.application
                (Core.termFunction $ Core.functionPrimitive $ encodedName _sets_map)
                (encodeType @@ var "elemType"))  -- Element encoder
              (Core.termVariable (Core.name (string "s")))))
