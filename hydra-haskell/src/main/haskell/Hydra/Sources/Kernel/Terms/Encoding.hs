{-# LANGUAGE FlexibleContexts #-}

module Hydra.Sources.Kernel.Terms.Encoding where

-- Standard imports for kernel terms modules
import Hydra.Kernel hiding (literalType)
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Paths    as Paths
import qualified Hydra.Dsl.Annotations       as Annotations
import qualified Hydra.Dsl.Ast          as Ast
import qualified Hydra.Dsl.Bootstrap         as Bootstrap
import qualified Hydra.Dsl.Coders       as Coders
import qualified Hydra.Dsl.Util      as Util
import qualified Hydra.Dsl.Meta.Core         as Core
import qualified Hydra.Dsl.Meta.Graph        as Graph
import qualified Hydra.Dsl.Json.Model         as Json
import qualified Hydra.Dsl.Meta.Lib.Chars    as Chars
import qualified Hydra.Dsl.Meta.Lib.Eithers  as Eithers
import qualified Hydra.Dsl.Meta.Lib.Equality as Equality
import qualified Hydra.Dsl.Meta.Lib.Lists    as Lists
import qualified Hydra.Dsl.Meta.Lib.Literals as Literals
import qualified Hydra.Dsl.Meta.Lib.Logic    as Logic
import qualified Hydra.Dsl.Meta.Lib.Maps     as Maps
import qualified Hydra.Dsl.Meta.Lib.Math     as Math
import qualified Hydra.Dsl.Meta.Lib.Maybes   as Maybes
import qualified Hydra.Dsl.Meta.Lib.Pairs    as Pairs
import qualified Hydra.Dsl.Meta.Lib.Sets     as Sets
import           Hydra.Dsl.Meta.Lib.Strings  as Strings
import qualified Hydra.Dsl.Literals          as Literals
import qualified Hydra.Dsl.LiteralTypes      as LiteralTypes
import qualified Hydra.Dsl.Meta.Base         as MetaBase
import qualified Hydra.Dsl.Meta.Terms        as MetaTerms
import qualified Hydra.Dsl.Meta.Types        as MetaTypes
import qualified Hydra.Dsl.Module       as Module
import qualified Hydra.Dsl.Parsing      as Parsing
import qualified Hydra.Dsl.Meta.Phantoms     as Phantoms
import           Hydra.Dsl.Meta.Phantoms     as Phantoms hiding (
  elimination, field, fieldType, floatType, floatValue, function, injection, integerType, integerValue, lambda, literal,
  literalType, record, term, type_, typeScheme, wrap)
import qualified Hydra.Dsl.Prims             as Prims
import qualified Hydra.Dsl.Meta.Tabular           as Tabular
import qualified Hydra.Dsl.Meta.Testing      as Testing
import qualified Hydra.Dsl.Terms             as Terms
import qualified Hydra.Dsl.Tests             as Tests
import qualified Hydra.Dsl.Topology     as Topology
import qualified Hydra.Dsl.Types             as Types
import qualified Hydra.Dsl.Typing       as Typing
import qualified Hydra.Dsl.Meta.Context      as Ctx
import qualified Hydra.Dsl.Errors       as Error
import qualified Hydra.Dsl.Meta.Variants     as Variants
import           Hydra.Sources.Kernel.Types.All
import qualified Hydra.Sources.Kernel.Terms.Annotations as Annotations
import qualified Hydra.Sources.Decode.Core as DecodeCore

import qualified Hydra.Sources.Kernel.Terms.Formatting as Formatting
import qualified Hydra.Sources.Kernel.Terms.Names as Names
import qualified Hydra.Sources.Kernel.Terms.Rewriting as Rewriting
import qualified Hydra.Sources.Kernel.Terms.Schemas as Schemas
import qualified Hydra.Dsl.Meta.DeepCore as DC
import           Hydra.Dsl.Meta.DeepCore ((@@@))
import           Prelude hiding ((++))
import qualified Data.Int                    as I
import qualified Data.List                   as L
import qualified Data.Map                    as M
import qualified Data.Set                    as S
import qualified Data.Maybe                  as Y


ns :: Namespace
ns = Namespace "hydra.encoding"

module_ :: Module
module_ = Module ns elements
    [Annotations.ns, moduleNamespace DecodeCore.module_, Formatting.ns, Names.ns, Rewriting.ns, Schemas.ns]
    kernelTypesNamespaces $
    Just "Functions for generating term encoders from type modules"
  where
    elements = [
      toTermDefinition encodeBinding,
      toTermDefinition encodeBindingName,
      toTermDefinition encodeEitherType,
      toTermDefinition encodeFieldValue,
      toTermDefinition encodeFloatValue,
      toTermDefinition encodeForallType,
      toTermDefinition encodeInjection,
      toTermDefinition encodeIntegerValue,
      toTermDefinition encodeListType,
      toTermDefinition encodeLiteralType,
      toTermDefinition encodeMapType,
      toTermDefinition encodeModule,
      toTermDefinition encodeName,
      toTermDefinition encodeNamespace,
      toTermDefinition encodeOptionalType,
      toTermDefinition encodePairType,
      toTermDefinition encodeRecordType,
      toTermDefinition encodeRecordTypeNamed,
      toTermDefinition encodeSetType,
      toTermDefinition encodeType,
      toTermDefinition encodeTypeNamed,
      toTermDefinition encodeUnionType,
      toTermDefinition encodeUnionTypeNamed,
      toTermDefinition encodeWrappedType,
      toTermDefinition encodeWrappedTypeNamed,
      toTermDefinition encoderCollectForallVariables,
      toTermDefinition encoderCollectOrdVars,
      toTermDefinition encoderCollectTypeVarsFromType,
      toTermDefinition encoderFullResultType,
      toTermDefinition encoderFullResultTypeNamed,
      toTermDefinition encoderType,
      toTermDefinition encoderTypeNamed,
      toTermDefinition encoderTypeScheme,
      toTermDefinition encoderTypeSchemeNamed,
      toTermDefinition filterTypeBindings,
      toTermDefinition isEncodableBinding,
      toTermDefinition isUnitType_,
      toTermDefinition prependForallEncoders]

define :: String -> TTerm x -> TBinding x
define = definitionInModule module_

-- | Bridge helper: format InContext DecodingError as a string
formatDecodingError :: TTerm (InContext DecodingError -> String)
formatDecodingError = "ic" ~> unwrap _DecodingError @@ Ctx.inContextObject (var "ic")

-- | Encode a single type binding into an encoder binding
-- This decodes the term to a Type, then generates an encoder function.
-- Type variables that appear as Map keys or Set elements get Ord constraints
-- via the encoder type scheme.
encodeBinding :: TBinding (Context -> Graph -> Binding -> Either (InContext DecodingError) Binding)
encodeBinding = define "encodeBinding" $
  doc "Transform a type binding into an encoder binding" $
  "cx" ~> "graph" ~> "b" ~>
    Eithers.bind (Ctx.withContext (var "cx") (decoderFor _Type @@ var "graph" @@ (Core.bindingTerm (var "b")))) (
      "typ" ~>
      right (Core.binding
        (encodeBindingName @@ (Core.bindingName (var "b")))
        (encodeTypeNamed @@ (Core.bindingName (var "b")) @@ (var "typ"))
        (just (encoderTypeSchemeNamed @@ (Core.bindingName (var "b")) @@ var "typ"))))

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

-- | Generate the encoder term for a field value
-- Creates a lambda that encodes the field value and wraps in an encoded union/injection
encodeFieldValue :: TBinding (Name -> Name -> Type -> Term)
encodeFieldValue = define "encodeFieldValue" $
  doc "Generate the encoder for a field's value" $
  "typeName" ~> "fieldName" ~> "fieldType" ~>
    -- Create a lambda that encodes the value and wraps in Term.union with injection
    -- Note: use "y" instead of "v" to avoid shadowing type variable parameters named "v"
    DC.lambda "y" $
      -- Build Term.union containing an encoded Injection with the encoded value
      DC.injection _Term (DC.field _Term_union
        (encodeInjection @@ var "typeName" @@ var "fieldName"
          @@ ((encodeType @@ var "fieldType") @@@ DC.var "y")))

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

-- | Generate an encoder for a list type
-- Maps the element encoder over the list and wraps in Term.list
encodeListType :: TBinding (Type -> Term)
encodeListType = define "encodeListType" $
  doc "Generate an encoder for a list type" $
  "elemType" ~>
    DC.lambda "xs" $
      DC.injection _Term (DC.field _Term_list
        (DC.primitiveEncoded _lists_map @@@ (encodeType @@ var "elemType") @@@ DC.var "xs"))

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

-- | Transform a type module into an encoder module
-- Returns Nothing if the module has no encodable type definitions
encodeModule :: TBinding (Context -> Graph -> Module -> Prelude.Either (InContext Error) (Maybe Module))
encodeModule = define "encodeModule" $
  doc "Transform a type module into an encoder module" $
  "cx" ~> "graph" ~> "mod" ~>
    "typeBindings" <<~ (filterTypeBindings @@ var "cx" @@ var "graph" @@
      (Maybes.cat $ Lists.map
        ("d" ~> cases _Definition (var "d") (Just nothing) [
          _Definition_type>>: "td" ~>
            just (Annotations.typeElement @@ (Module.typeDefinitionName $ var "td") @@ (Module.typeDefinitionType $ var "td"))])
        (Module.moduleDefinitions (var "mod")))) $
    Logic.ifElse (Lists.null (var "typeBindings"))
      (right nothing)
      ("encodedBindings" <<~ Eithers.mapList ("b" ~>
        Eithers.bimap
          ("ic" ~> Ctx.inContext (Error.errorOther $ Error.otherError (unwrap _DecodingError @@ Ctx.inContextObject (var "ic"))) (Ctx.inContextContext (var "ic")))
          ("x" ~> var "x")
          (encodeBinding @@ var "cx" @@ var "graph" @@ var "b")) (var "typeBindings") $
        -- The encoder module depends on encoder modules for both the type and term dependencies
        -- E.g., hydra.encode.constraints depends on hydra.encode.core (type dep) and hydra.encode.query (term dep)
        right (just (Module.module_
          (encodeNamespace @@ (Module.moduleNamespace (var "mod")))
          (Lists.map ("b" ~> Module.definitionTerm (Module.termDefinition
            (Core.bindingName $ var "b") (Core.bindingTerm $ var "b")
            (Core.bindingType $ var "b")))
            (var "encodedBindings"))
          -- Transform both type and term dependency namespaces to their encoder namespaces
          (Lists.nub (Lists.concat2
            (primitive _lists_map @@ encodeNamespace @@ (Module.moduleTypeDependencies (var "mod")))
            (primitive _lists_map @@ encodeNamespace @@ (Module.moduleTermDependencies (var "mod")))))
          -- The encoder module depends on the original type module
          (list [Module.moduleNamespace (var "mod")])
          (just (Strings.cat $ list [
            string "Term encoders for ",
            Module.unNamespace (Module.moduleNamespace (var "mod"))])))))

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

encodeRecordType :: TBinding ([FieldType] -> Term)
encodeRecordType = define "encodeRecordType" $
  doc "Generate an encoder for a record type (unnamed — should not be called directly)" $
  "rt" ~> encodeRecordTypeNamed @@ Core.name (string "unknown") @@ var "rt"

-- | Generate an encoder for a record type
-- For records, project each field, encode it, and build an encoded record
encodeRecordTypeNamed :: TBinding (Name -> [FieldType] -> Term)
encodeRecordTypeNamed = define "encodeRecordTypeNamed" $
  doc "Generate an encoder for a record type with the given element name" $
  "ename" ~> "rt" ~>
    DC.lambda "x" $
      DC.injection _Term (DC.field _Term_record
        (DC.record _Record [
          DC.field _Record_typeName (encodeName @@ var "ename"),
          DC.field _Record_fields
            (DC.list (primitive _lists_map @@ (encodeRecordFieldNamed @@ var "ename" @@ var "rt") @@ var "rt"))]))
  where
    encodeRecordFieldNamed :: TTerm (Name -> [FieldType] -> FieldType -> Term)
    encodeRecordFieldNamed =
      "tname" ~> "recType" ~> "ft" ~>
        DC.record _Field [
          DC.field _Field_name (encodeName @@ Core.fieldTypeName (var "ft")),
          DC.field _Field_term
            ((encodeType @@ Core.fieldTypeType (var "ft"))
              @@@ (projectField (var "tname") (Core.fieldTypeName (var "ft"))
                    @@@ DC.var "x"))]

    projectField typeName fieldName =
      Core.termFunction $ Core.functionElimination $ Core.eliminationRecord $
        Core.projection typeName fieldName

-- | Generate an encoder for a set type
-- Encodes each element and wraps in Term.set
encodeSetType :: TBinding (Type -> Term)
encodeSetType = define "encodeSetType" $
  doc "Generate an encoder for a set type" $
  "elemType" ~> DC.lambda "s" $
    DC.injection _Term (DC.field _Term_set
      (DC.primitiveEncoded _sets_map @@@ (encodeType @@ var "elemType") @@@ DC.var "s"))


-- | Generate an encoder term for a given Type (without element name context)
encodeType :: TBinding (Type -> Term)
encodeType = define "encodeType" $
  doc "Generate an encoder term for a Type" $
  match _Type (Just identityEncoder) [
    _Type_annotated>>: "at" ~>
      encodeType @@ Core.annotatedTypeBody (var "at"),
    _Type_application>>: "appType" ~>
      (encodeType @@ Core.applicationTypeFunction (var "appType"))
        @@@ (encodeType @@ Core.applicationTypeArgument (var "appType")),
    _Type_either>>: "et" ~>
      encodeEitherType @@ var "et",
    _Type_forall>>: "ft" ~>
      encodeForallType @@ var "ft",
    _Type_function>>: constant identityEncoder,
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
      DC.lambda "_" $ DC.injection _Term (DC.field _Term_unit DC.unit),
    _Type_void>>: constant $
      DC.lambda "_" $ DC.injection _Term (DC.field _Term_unit DC.unit),
    _Type_variable>>: "typeName" ~>
      Core.termVariable (encodeBindingName @@ var "typeName")]
  where
    identityEncoder = DC.lambda "x" $ DC.var "x"

-- | Generate an encoder term for a given Type, using the element name for record/union/wrap
encodeTypeNamed :: TBinding (Name -> Type -> Term)
encodeTypeNamed = define "encodeTypeNamed" $
  doc "Generate an encoder term for a Type, with the element name for nominal types" $
  "ename" ~> "typ" ~>
  cases _Type (var "typ") (Just identityEncoder) [
    _Type_annotated>>: "at" ~>
      encodeTypeNamed @@ var "ename" @@ Core.annotatedTypeBody (var "at"),
    _Type_application>>: "appType" ~>
      (encodeType @@ Core.applicationTypeFunction (var "appType"))
        @@@ (encodeType @@ Core.applicationTypeArgument (var "appType")),
    _Type_either>>: "et" ~>
      encodeEitherType @@ var "et",
    _Type_forall>>: "ft" ~>
      Core.termFunction $ Core.functionLambda $
        Core.lambda (encodeBindingName @@ Core.forallTypeParameter (var "ft")) nothing
          (encodeTypeNamed @@ var "ename" @@ Core.forallTypeBody (var "ft")),
    _Type_function>>: constant identityEncoder,
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
      encodeRecordTypeNamed @@ var "ename" @@ var "rt",
    _Type_set>>: "elemType" ~>
      encodeSetType @@ var "elemType",
    _Type_union>>: "rt" ~>
      encodeUnionTypeNamed @@ var "ename" @@ var "rt",
    _Type_wrap>>: "wt" ~>
      encodeWrappedTypeNamed @@ var "ename" @@ var "wt",
    _Type_unit>>: constant $
      DC.lambda "_" $ DC.injection _Term (DC.field _Term_unit DC.unit),
    _Type_void>>: constant $
      DC.lambda "_" $ DC.injection _Term (DC.field _Term_unit DC.unit),
    _Type_variable>>: "typeName" ~>
      Core.termVariable (encodeBindingName @@ var "typeName")]
  where
    identityEncoder = DC.lambda "x" $ DC.var "x"

-- | Generate an encoder for a union type (placeholder name)
encodeUnionType :: TBinding ([FieldType] -> Term)
encodeUnionType = define "encodeUnionType" $
  doc "Generate an encoder for a union type (placeholder name)" $
  "rt" ~> encodeUnionTypeNamed @@ Core.name (string "unknown") @@ var "rt"

-- | Generate an encoder for a union type with a given element name
encodeUnionTypeNamed :: TBinding (Name -> [FieldType] -> Term)
encodeUnionTypeNamed = define "encodeUnionTypeNamed" $
  doc "Generate an encoder for a union type with the given element name" $
  "ename" ~> "rt" ~>
    Core.termFunction $ Core.functionElimination $ Core.eliminationUnion $
      Core.caseStatement
        (var "ename")
        nothing
        (primitive _lists_map @@
          ("ft" ~> Core.field
            (Core.fieldTypeName (var "ft"))
            (encodeFieldValue
              @@ var "ename"
              @@ Core.fieldTypeName (var "ft")
              @@ Core.fieldTypeType (var "ft")))
          @@ var "rt")

-- | Generate an encoder for a wrapped type (placeholder name)
encodeWrappedType :: TBinding (Type -> Term)
encodeWrappedType = define "encodeWrappedType" $
  doc "Generate an encoder for a wrapped type (placeholder name)" $
  "wt" ~> encodeWrappedTypeNamed @@ Core.name (string "unknown") @@ var "wt"

-- | Generate an encoder for a wrapped type with a given element name
encodeWrappedTypeNamed :: TBinding (Name -> Type -> Term)
encodeWrappedTypeNamed = define "encodeWrappedTypeNamed" $
  doc "Generate an encoder for a wrapped type with the given element name" $
  "ename" ~> "wt" ~>
    DC.lambda "x" $
      DC.injection _Term (DC.field _Term_wrap
        (DC.record _WrappedTerm [
          DC.field _WrappedTerm_typeName (encodeName @@ var "ename"),
          DC.field _WrappedTerm_body
            ((encodeType @@ var "wt")
              @@@ (DC.unwrapDynamic (var "ename") @@@ DC.var "x"))]))

-- | Collect forall type variables from a type
encoderCollectForallVariables :: TBinding (Type -> [Name])
encoderCollectForallVariables = define "encoderCollectForallVariables" $
  doc "Collect forall type variable names from a type" $
  "typ" ~>
  cases _Type (var "typ") (Just $ list ([] :: [TTerm Name])) [
    _Type_annotated>>: "at" ~>
      encoderCollectForallVariables @@ (Core.annotatedTypeBody (var "at")),
    _Type_forall>>: "ft" ~>
      Lists.cons (Core.forallTypeParameter (var "ft"))
        (encoderCollectForallVariables @@ Core.forallTypeBody (var "ft"))]

-- | Collect type variables that need Ord constraints (from Map key and Set element positions)
encoderCollectOrdVars :: TBinding (Type -> [Name])
encoderCollectOrdVars = define "encoderCollectOrdVars" $
  doc "Collect type variables needing Ord constraints" $
  "typ" ~>
  cases _Type (var "typ") (Just $ list ([] :: [TTerm Name])) [
    _Type_annotated>>: "at" ~>
      encoderCollectOrdVars @@ (Core.annotatedTypeBody (var "at")),
    _Type_application>>: "appType" ~>
      Lists.concat2
        (encoderCollectOrdVars @@ Core.applicationTypeFunction (var "appType"))
        (encoderCollectOrdVars @@ Core.applicationTypeArgument (var "appType")),
    _Type_either>>: "et" ~>
      Lists.concat2
        (encoderCollectOrdVars @@ Core.eitherTypeLeft (var "et"))
        (encoderCollectOrdVars @@ Core.eitherTypeRight (var "et")),
    _Type_forall>>: "ft" ~>
      encoderCollectOrdVars @@ Core.forallTypeBody (var "ft"),
    _Type_list>>: "elemType" ~>
      encoderCollectOrdVars @@ var "elemType",
    -- For Map<K, V>, collect all type variables from K (they need Ord)
    _Type_map>>: "mt" ~>
      Lists.concat (list [
        encoderCollectTypeVarsFromType @@ Core.mapTypeKeys (var "mt"),
        encoderCollectOrdVars @@ Core.mapTypeKeys (var "mt"),
        encoderCollectOrdVars @@ Core.mapTypeValues (var "mt")]),
    _Type_maybe>>: "elemType" ~>
      encoderCollectOrdVars @@ var "elemType",
    _Type_pair>>: "pt" ~>
      Lists.concat2
        (encoderCollectOrdVars @@ Core.pairTypeFirst (var "pt"))
        (encoderCollectOrdVars @@ Core.pairTypeSecond (var "pt")),
    _Type_record>>: "rt" ~>
      Lists.concat $ Lists.map
        ("ft" ~> encoderCollectOrdVars @@ Core.fieldTypeType (var "ft"))
        (var "rt"),
    -- For Set<T>, collect all type variables from T (they all need Ord)
    _Type_set>>: "elemType" ~>
      Lists.concat2
        (encoderCollectTypeVarsFromType @@ var "elemType")
        (encoderCollectOrdVars @@ var "elemType"),
    _Type_union>>: "rt" ~>
      Lists.concat $ Lists.map
        ("ft" ~> encoderCollectOrdVars @@ Core.fieldTypeType (var "ft"))
        (var "rt"),
    _Type_wrap>>: "wt" ~>
      encoderCollectOrdVars @@ var "wt"]

-- | Collect all type variables from a type expression
encoderCollectTypeVarsFromType :: TBinding (Type -> [Name])
encoderCollectTypeVarsFromType = define "encoderCollectTypeVarsFromType" $
  doc "Collect all type variable names from a type expression" $
  "typ" ~>
  cases _Type (var "typ") (Just $ list ([] :: [TTerm Name])) [
    _Type_annotated>>: "at" ~>
      encoderCollectTypeVarsFromType @@ Core.annotatedTypeBody (var "at"),
    _Type_application>>: "appType" ~>
      Lists.concat2
        (encoderCollectTypeVarsFromType @@ Core.applicationTypeFunction (var "appType"))
        (encoderCollectTypeVarsFromType @@ Core.applicationTypeArgument (var "appType")),
    _Type_forall>>: "ft" ~>
      encoderCollectTypeVarsFromType @@ Core.forallTypeBody (var "ft"),
    _Type_list>>: "elemType" ~>
      encoderCollectTypeVarsFromType @@ var "elemType",
    _Type_map>>: "mt" ~>
      Lists.concat2
        (encoderCollectTypeVarsFromType @@ Core.mapTypeKeys (var "mt"))
        (encoderCollectTypeVarsFromType @@ Core.mapTypeValues (var "mt")),
    _Type_maybe>>: "elemType" ~>
      encoderCollectTypeVarsFromType @@ var "elemType",
    _Type_pair>>: "pt" ~>
      Lists.concat2
        (encoderCollectTypeVarsFromType @@ Core.pairTypeFirst (var "pt"))
        (encoderCollectTypeVarsFromType @@ Core.pairTypeSecond (var "pt")),
    _Type_record>>: "rt" ~>
      Lists.concat $ Lists.map
        ("ft" ~> encoderCollectTypeVarsFromType @@ Core.fieldTypeType (var "ft"))
        (var "rt"),
    _Type_set>>: "elemType" ~>
      encoderCollectTypeVarsFromType @@ var "elemType",
    _Type_union>>: "rt" ~>
      Lists.concat $ Lists.map
        ("ft" ~> encoderCollectTypeVarsFromType @@ Core.fieldTypeType (var "ft"))
        (var "rt"),
    _Type_variable>>: "name" ~>
      list [var "name"],
    _Type_wrap>>: "wt" ~>
      encoderCollectTypeVarsFromType @@ var "wt"]

-- | Get the full result type for an encoder (the input type of the encoder function)
-- Maps structural types to their nominal names, preserving type applications.
encoderFullResultType :: TBinding (Type -> Type)
encoderFullResultType = define "encoderFullResultType" $
  doc "Get full result type for encoder input" $
  "typ" ~>
  cases _Type (var "typ") (Just $ Core.typeVariable (Core.nameLift _Term)) [
    _Type_annotated>>: "at" ~>
      encoderFullResultType @@ (Core.annotatedTypeBody (var "at")),
    _Type_application>>: "appType" ~>
      Core.typeApplication $ Core.applicationType
        (encoderFullResultType @@ Core.applicationTypeFunction (var "appType"))
        (Core.applicationTypeArgument (var "appType")),
    _Type_either>>: "et" ~>
      Core.typeEither $ Core.eitherType
        (encoderFullResultType @@ Core.eitherTypeLeft (var "et"))
        (encoderFullResultType @@ Core.eitherTypeRight (var "et")),
    _Type_forall>>: "ft" ~>
      Core.typeApplication $ Core.applicationType
        (encoderFullResultType @@ Core.forallTypeBody (var "ft"))
        (Core.typeVariable (Core.forallTypeParameter (var "ft"))),
    _Type_list>>: "elemType" ~>
      Core.typeList (encoderFullResultType @@ var "elemType"),
    _Type_literal>>: "_" ~>
      Core.typeVariable (Core.nameLift _Literal),
    _Type_map>>: "mt" ~>
      Core.typeMap $ Core.mapType
        (encoderFullResultType @@ Core.mapTypeKeys (var "mt"))
        (encoderFullResultType @@ Core.mapTypeValues (var "mt")),
    _Type_maybe>>: "elemType" ~>
      Core.typeMaybe (encoderFullResultType @@ var "elemType"),
    _Type_pair>>: "pt" ~>
      Core.typePair $ Core.pairType
        (encoderFullResultType @@ Core.pairTypeFirst (var "pt"))
        (encoderFullResultType @@ Core.pairTypeSecond (var "pt")),
    _Type_record>>: constant (Core.typeVariable (Core.nameLift _Term)),
    _Type_set>>: "elemType" ~>
      Core.typeSet (encoderFullResultType @@ var "elemType"),
    _Type_union>>: constant (Core.typeVariable (Core.nameLift _Term)),
    _Type_unit>>: constant Core.typeUnit,
    _Type_variable>>: "name" ~>
      Core.typeVariable (var "name"),
    _Type_void>>: constant Core.typeVoid,
    _Type_wrap>>: constant (Core.typeVariable (Core.nameLift _Term))]

-- | Get full result type for encoder input, with element name for nominal types
encoderFullResultTypeNamed :: TBinding (Name -> Type -> Type)
encoderFullResultTypeNamed = define "encoderFullResultTypeNamed" $
  doc "Get full result type for encoder input, using element name for nominal types" $
  "ename" ~> "typ" ~>
  cases _Type (var "typ") (Just $ Core.typeVariable (Core.nameLift _Term)) [
    _Type_annotated>>: "at" ~>
      encoderFullResultTypeNamed @@ var "ename" @@ (Core.annotatedTypeBody (var "at")),
    _Type_application>>: "appType" ~>
      Core.typeApplication $ Core.applicationType
        (encoderFullResultType @@ Core.applicationTypeFunction (var "appType"))
        (Core.applicationTypeArgument (var "appType")),
    _Type_either>>: "et" ~>
      Core.typeEither $ Core.eitherType
        (encoderFullResultType @@ Core.eitherTypeLeft (var "et"))
        (encoderFullResultType @@ Core.eitherTypeRight (var "et")),
    _Type_forall>>: "ft" ~>
      Core.typeApplication $ Core.applicationType
        (encoderFullResultTypeNamed @@ var "ename" @@ Core.forallTypeBody (var "ft"))
        (Core.typeVariable (Core.forallTypeParameter (var "ft"))),
    _Type_list>>: "elemType" ~>
      Core.typeList (encoderFullResultType @@ var "elemType"),
    _Type_literal>>: "_" ~>
      Core.typeVariable (Core.nameLift _Literal),
    _Type_map>>: "mt" ~>
      Core.typeMap $ Core.mapType
        (encoderFullResultType @@ Core.mapTypeKeys (var "mt"))
        (encoderFullResultType @@ Core.mapTypeValues (var "mt")),
    _Type_maybe>>: "elemType" ~>
      Core.typeMaybe (encoderFullResultType @@ var "elemType"),
    _Type_pair>>: "pt" ~>
      Core.typePair $ Core.pairType
        (encoderFullResultType @@ Core.pairTypeFirst (var "pt"))
        (encoderFullResultType @@ Core.pairTypeSecond (var "pt")),
    _Type_record>>: constant (Core.typeVariable (var "ename")),
    _Type_set>>: "elemType" ~>
      Core.typeSet (encoderFullResultType @@ var "elemType"),
    _Type_union>>: constant (Core.typeVariable (var "ename")),
    _Type_unit>>: constant Core.typeUnit,
    _Type_variable>>: "name" ~>
      Core.typeVariable (var "name"),
    _Type_void>>: constant Core.typeVoid,
    _Type_wrap>>: constant (Core.typeVariable (var "ename"))]

encoderType :: TBinding (Type -> Type)
encoderType = define "encoderType" $
  doc "Build encoder function type" $
  "typ" ~>
  "resultType" <~ (encoderFullResultType @@ var "typ") $
  "baseType" <~ (Core.typeFunction $ Core.functionType
    (var "resultType")
    (Core.typeVariable (Core.nameLift _Term))) $
  prependForallEncoders @@ var "baseType" @@ var "typ"

-- | Build the encoder function type for a given type
-- For monomorphic types: InputType -> Term
-- For polymorphic types: (a -> Term) -> ... -> InputType<a,...> -> Term
encoderTypeNamed :: TBinding (Name -> Type -> Type)
encoderTypeNamed = define "encoderTypeNamed" $
  doc "Build encoder function type with element name for nominal types" $
  "ename" ~> "typ" ~>
  "resultType" <~ (encoderFullResultTypeNamed @@ var "ename" @@ var "typ") $
  "baseType" <~ (Core.typeFunction $ Core.functionType
    (var "resultType")
    (Core.typeVariable (Core.nameLift _Term))) $
  prependForallEncoders @@ var "baseType" @@ var "typ"

encoderTypeScheme :: TBinding (Type -> TypeScheme)
encoderTypeScheme = define "encoderTypeScheme" $
  doc "Construct a TypeScheme for an encoder function from a source type" $
  "typ" ~> lets [
    -- Collect forall variables
    "typeVars">: encoderCollectForallVariables @@ var "typ",

    -- Build the encoder function type
    "encoderFunType">: encoderType @@ var "typ",

    -- Find Ord-constrained variables (those used as Map keys or Set elements)
    "allOrdVars">: encoderCollectOrdVars @@ var "typ",
    -- Filter to only actual forall-bound variables
    "ordVars">: Lists.filter
      ("v" ~> Lists.elem (var "v" :: TTerm Name) (var "typeVars" :: TTerm [Name]))
      (var "allOrdVars"),

    -- Build constraints map: {varName -> TypeVariableMetadata {classes = {ordering}}}
    "constraints">:
      Logic.ifElse (Lists.null (var "ordVars"))
        nothing
        (just $ Maps.fromList $ Lists.map
          ("v" ~> pair (var "v") (Core.typeVariableMetadata $ Sets.singleton $ Core.nameLift _TypeClass_ordering))
          (var "ordVars"))] $
  Core.typeScheme (var "typeVars") (var "encoderFunType") (var "constraints")

-- | Construct a TypeScheme for an encoder function from a source type definition.
-- For a type like @forall v. Graph v@ (where Graph has Map v (Vertex v)),
-- produces the scheme @forall v. (v -> Term) -> Graph v -> Term@ with Ord constraint on v.
--
-- The type scheme uses the same variable names as the original type definition.
-- Variable normalization (to t0, t1, etc.) happens later in the pipeline and
-- handles renaming both variables and constraint keys consistently.
encoderTypeSchemeNamed :: TBinding (Name -> Type -> TypeScheme)
encoderTypeSchemeNamed = define "encoderTypeSchemeNamed" $
  doc "Construct a TypeScheme for an encoder function, with element name for nominal types" $
  "ename" ~> "typ" ~> lets [
    "typeVars">: encoderCollectForallVariables @@ var "typ",
    "encoderFunType">: encoderTypeNamed @@ var "ename" @@ var "typ",
    "allOrdVars">: encoderCollectOrdVars @@ var "typ",
    "ordVars">: Lists.filter
      ("v" ~> Lists.elem (var "v" :: TTerm Name) (var "typeVars" :: TTerm [Name]))
      (var "allOrdVars"),
    "constraints">:
      Logic.ifElse (Lists.null (var "ordVars"))
        nothing
        (just $ Maps.fromList $ Lists.map
          ("v" ~> pair (var "v") (Core.typeVariableMetadata $ Sets.singleton $ Core.nameLift _TypeClass_ordering))
          (var "ordVars"))] $
  Core.typeScheme (var "typeVars") (var "encoderFunType") (var "constraints")

-- | Filter bindings to only encodable type definitions
-- A binding is encodable if it is a native type AND is serializable (no function types in dependencies)
filterTypeBindings :: TBinding (Context -> Graph -> [Binding] -> Prelude.Either (InContext Error) [Binding])
filterTypeBindings = define "filterTypeBindings" $
  doc "Filter bindings to only encodable type definitions" $
  "cx" ~> "graph" ~> "bindings" ~>
    -- First filter to native types, then check serializability for each
    Eithers.map (primitive _maybes_cat) $
      Eithers.mapList (isEncodableBinding @@ var "cx" @@ var "graph") $
        primitive _lists_filter @@ Annotations.isNativeType @@ var "bindings"

-- | Check if a binding is encodable and return Just binding if so, Nothing otherwise
isEncodableBinding :: TBinding (Context -> Graph -> Binding -> Prelude.Either (InContext Error) (Maybe Binding))
isEncodableBinding = define "isEncodableBinding" $
  doc "Check if a binding is encodable (serializable type)" $
  "cx" ~> "graph" ~> "b" ~>
    "serializable" <<~ Schemas.isSerializableByName @@ var "cx" @@ var "graph" @@ (Core.bindingName (var "b")) $
    right (Logic.ifElse (var "serializable") (just (var "b")) nothing)

-- | Check whether a type is the unit type
isUnitType_ :: TBinding (Type -> Bool)
isUnitType_ = define "isUnitType" $
  doc "Check whether a type is the unit type" $
  match _Type (Just $ false) [
    _Type_unit>>: constant true]

-- | Prepend encoder types for forall parameters
-- For forall a. T: prepends (a -> Term) -> to the base type
prependForallEncoders :: TBinding (Type -> Type -> Type)
prependForallEncoders = define "prependForallEncoders" $
  doc "Prepend encoder types for forall parameters to base type" $
  "baseType" ~> "typ" ~> cases _Type (var "typ") (Just $ var "baseType") [
    _Type_annotated>>: "at" ~>
      prependForallEncoders @@ var "baseType" @@ Core.annotatedTypeBody (var "at"),
    _Type_forall>>: "ft" ~>
      Core.typeFunction $ Core.functionType
        (Core.typeFunction $ Core.functionType
          (Core.typeVariable (Core.forallTypeParameter (var "ft")))
          (Core.typeVariable (Core.nameLift _Term)))
        (prependForallEncoders @@ var "baseType" @@ Core.forallTypeBody (var "ft"))]
