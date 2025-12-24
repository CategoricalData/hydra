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
import qualified Hydra.Sources.Decode.Core as DecodeCore
import qualified Hydra.Sources.Kernel.Terms.Formatting as Formatting
import qualified Hydra.Sources.Kernel.Terms.Lexical as Lexical
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
    [Annotations.ns, moduleNamespace DecodeCore.module_, Formatting.ns, Lexical.ns, Monads.ns, Names.ns, Rewriting.ns, Schemas.ns, ShowCore.ns]
    kernelTypesNamespaces $
    Just "Functions for generating term decoders from type modules"
  where
    elements = [
      toBinding collectForallVariables,
      toBinding collectTypeVariables,
      toBinding decodeBinding,
      toBinding decodeBindingName,
      toBinding decodeEitherType,
      toBinding decodeForallType,
      toBinding decodeListType,
      toBinding decodeLiteralType,
      toBinding decodeMapType,
      toBinding decodeModule,
      toBinding decodeMaybeType,
      toBinding decodeNamespace,
      toBinding decodePairType,
      toBinding decodeRecordType,
      toBinding decodeSetType,
      toBinding decodeType,
      toBinding decodeUnionType,
      toBinding decodeUnitType,
      toBinding decodeWrappedType,
      toBinding decoderFullResultType,
      toBinding decoderResultType,
      toBinding decoderType,
      toBinding decoderTypeScheme,
      toBinding filterTypeBindings,
      toBinding isDecodableBinding,
      toBinding prependForallDecoders]

define :: String -> TTerm x -> TBinding x
define = definitionInModule module_

--------------------------------------------------------------------------------
-- Helper functions
--------------------------------------------------------------------------------

-- | Build a decoder term that takes cx and raw, strips annotations/dereferences variables, and matches on it
-- The result is: \cx -> \raw -> either (\err -> Left (DecodingError err)) (\stripped -> case stripped of ...) (stripAndDereferenceTermEither cx raw)
deannotateAndMatch :: TTerm (Maybe Term) -> [TTerm Field] -> TTerm Term
deannotateAndMatch dflt cses = DC.lambda "cx" $ DC.lambda "raw" $
  DC.primitive _eithers_either
    -- If Left (error string), convert to DecodingError
    @@@ (DC.lambda "err" $ DC.left $ DC.wrap _DecodingError $ DC.var "err")
    -- If Right (stripped term), do the case match
    @@@ (DC.lambda "stripped" $ DC.cases _Term (DC.var "stripped") dflt cses)
    -- Call stripAndDereferenceTermEither cx raw
    @@@ (DC.ref Lexical.stripAndDereferenceTermEither @@@ DC.var "cx" @@@ DC.var "raw")

-- | Make a decoding error from a string message
decodingError :: TTerm String -> TTerm DecodingError
decodingError msg = Phantoms.wrap _DecodingError msg

-- | Helper to create a decoding error term from a message (object-level)
-- Returns: Term.wrap (WrappedTerm "hydra.util.DecodingError" (Term.literal (Literal.string msg)))
decodingErrorTerm :: TTerm String -> TTerm Term
decodingErrorTerm msg = DC.wrap _DecodingError $ DC.string msg

-- | Helper to create a Left (error) result term
-- Returns: Term.either (Left (decodingErrorTerm msg))
leftError :: TTerm String -> TTerm Term
leftError msg = DC.left $ decodingErrorTerm msg

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
    _Type_application>>: "appType" ~>
      -- For type applications like ColumnSchema<t>, get result type from function part
      decoderResultType @@ (Core.applicationTypeFunction (var "appType")),
    _Type_forall>>: "ft" ~>
      decoderResultType @@ (Core.forallTypeBody (var "ft")),
    _Type_literal>>: "_" ~>
      Core.nameLift _Literal,
    _Type_record>>: "rt" ~>
      Core.rowTypeTypeName (var "rt"),
    _Type_union>>: "rt" ~>
      Core.rowTypeTypeName (var "rt"),
    _Type_wrap>>: "wt" ~>
      Core.wrappedTypeTypeName (var "wt")]

-- | Build a decoder type scheme: Term -> Either DecodingError ResultType
-- For polymorphic types, adds extra arguments for the decoders of type parameters
decoderTypeScheme :: TBinding (Type -> TypeScheme)
decoderTypeScheme = define "decoderTypeScheme" $
  doc "Build type scheme for a decoder function" $
  "typ" ~>
    Core.typeScheme
      (collectTypeVariables @@ var "typ")
      (decoderType @@ var "typ")
      Phantoms.nothing

-- | Collect type variables from forall types
-- Note: Graph is NOT included as a type variable - it's a concrete type
collectTypeVariables :: TBinding (Type -> [Name])
collectTypeVariables = define "collectTypeVariables" $
  doc "Collect type variable names from a type (forall parameters only)" $
  "typ" ~> collectForallVariables @@ var "typ"

-- | Collect just the forall type variables from a type
collectForallVariables :: TBinding (Type -> [Name])
collectForallVariables = define "collectForallVariables" $
  doc "Collect forall type variable names from a type" $
  match _Type (Just $ list ([] :: [TTerm Name])) [
    _Type_annotated>>: "at" ~>
      collectForallVariables @@ (Core.annotatedTypeBody (var "at")),
    _Type_forall>>: "ft" ~>
      Lists.cons (Core.forallTypeParameter (var "ft"))
        (collectForallVariables @@ Core.forallTypeBody (var "ft"))]

-- | Build the decoder function type for a given type
-- For monomorphic types: Graph -> Term -> Either DecodingError ResultType
-- For polymorphic types: (Graph -> Term -> Either DecodingError a) -> ... -> Graph -> Term -> Either DecodingError ResultType<a>
-- The 'Graph' parameter is used for dereferencing term variables
decoderType :: TBinding (Type -> Type)
decoderType = define "decoderType" $
  doc "Build decoder function type" $
  "typ" ~>
  -- Get the result type (the full type, preserving type applications)
  "resultType" <~ (decoderFullResultType @@ var "typ") $
  -- Build the base decoder type: Graph -> Term -> Either DecodingError ResultType
  -- Graph is a concrete type (hydra.graph.Graph), not a type variable
  "baseType" <~ (Core.typeFunction $ Core.functionType
    (Core.typeVariable (Core.nameLift _Graph))
    (Core.typeFunction $ Core.functionType
      (Core.typeVariable (Core.nameLift _Term))
      (Core.typeEither $ Core.eitherType
        (Core.typeVariable (Core.nameLift _DecodingError))
        (var "resultType")))) $
  -- Prepend decoder types for each forall parameter
  prependForallDecoders @@ var "baseType" @@ var "typ"

-- | Helper to prepend decoder types for forall parameters
-- For forall a. forall b. T: prepends (Graph -> Term -> E a) -> (Graph -> Term -> E b) -> to the base type
prependForallDecoders :: TBinding (Type -> Type -> Type)
prependForallDecoders = define "prependForallDecoders" $
  doc "Prepend decoder types for forall parameters to base type" $
  "baseType" ~> match _Type (Just $ var "baseType") [
    _Type_annotated>>: "at" ~>
      prependForallDecoders @@ var "baseType" @@ Core.annotatedTypeBody (var "at"),
    _Type_forall>>: "ft" ~>
      -- For forall a. T: build (Graph -> Term -> Either E a) -> prependForallDecoders(baseType, T)
      Core.typeFunction $ Core.functionType
        (Core.typeFunction $ Core.functionType
          (Core.typeVariable (Core.nameLift _Graph))
          (Core.typeFunction $ Core.functionType
            (Core.typeVariable (Core.nameLift _Term))
            (Core.typeEither $ Core.eitherType
              (Core.typeVariable (Core.nameLift _DecodingError))
              (Core.typeVariable (Core.forallTypeParameter (var "ft"))))))
        (prependForallDecoders @@ var "baseType" @@ Core.forallTypeBody (var "ft"))]

-- | Get the full result type for a decoder, preserving type applications
-- For forall t. ColumnSchema<t>, returns ColumnSchema<t> (as a Type, not just a Name)
decoderFullResultType :: TBinding (Type -> Type)
decoderFullResultType = define "decoderFullResultType" $
  doc "Get full result type for decoder" $
  match _Type (Just $ Core.typeVariable (Core.nameLift _Term)) [
    _Type_annotated>>: "at" ~>
      decoderFullResultType @@ (Core.annotatedTypeBody (var "at")),
    _Type_application>>: "appType" ~>
      -- Preserve the full application: e.g., ColumnSchema<t> stays as Type.application
      Core.typeApplication $ Core.applicationType
        (decoderFullResultType @@ Core.applicationTypeFunction (var "appType"))
        (Core.applicationTypeArgument (var "appType")),
    _Type_either>>: "et" ~>
      -- Either L R -> Either (decoded L) (decoded R)
      Core.typeEither $ Core.eitherType
        (decoderFullResultType @@ Core.eitherTypeLeft (var "et"))
        (decoderFullResultType @@ Core.eitherTypeRight (var "et")),
    _Type_forall>>: "ft" ~>
      -- For forall t. Body, we need to apply the type parameter to the body's result type
      -- e.g., forall t. RecordType{name=ColumnSchema} -> ColumnSchema t
      Core.typeApplication $ Core.applicationType
        (decoderFullResultType @@ Core.forallTypeBody (var "ft"))
        (Core.typeVariable (Core.forallTypeParameter (var "ft"))),
    _Type_list>>: "elemType" ~>
      -- [a] -> [decoded a]
      Core.typeList (decoderFullResultType @@ var "elemType"),
    _Type_literal>>: "_" ~>
      Core.typeVariable (Core.nameLift _Literal),
    _Type_map>>: "mt" ~>
      -- Map k v -> Map (decoded k) (decoded v)
      Core.typeMap $ Core.mapType
        (decoderFullResultType @@ Core.mapTypeKeys (var "mt"))
        (decoderFullResultType @@ Core.mapTypeValues (var "mt")),
    _Type_maybe>>: "elemType" ~>
      -- Maybe a -> Maybe (decoded a)
      Core.typeMaybe (decoderFullResultType @@ var "elemType"),
    _Type_pair>>: "pt" ~>
      -- (a, b) -> (decoded a, decoded b)
      Core.typePair $ Core.pairType
        (decoderFullResultType @@ Core.pairTypeFirst (var "pt"))
        (decoderFullResultType @@ Core.pairTypeSecond (var "pt")),
    _Type_record>>: "rt" ~>
      Core.typeVariable (Core.rowTypeTypeName (var "rt")),
    _Type_set>>: "elemType" ~>
      -- Set a -> Set (decoded a)
      Core.typeSet (decoderFullResultType @@ var "elemType"),
    _Type_union>>: "rt" ~>
      Core.typeVariable (Core.rowTypeTypeName (var "rt")),
    _Type_unit>>: constant Core.typeUnit,
    _Type_variable>>: "name" ~>
      Core.typeVariable (var "name"),
    _Type_wrap>>: "wt" ~>
      Core.typeVariable (Core.wrappedTypeTypeName (var "wt"))]

-- | Decode a single type binding into a decoder binding
-- Uses Flow to decode the Type from the binding's term, then generates decoder
decodeBinding :: TBinding (Binding -> Flow Graph Binding)
decodeBinding = define "decodeBinding" $
  doc "Transform a type binding into a decoder binding" $
  "b" ~>
    "cx" <<~ Monads.getState $
    Flows.bind (Monads.eitherToFlow_ @@ Util.unDecodingError @@ (decoderFor _Type @@ var "cx" @@ (Core.bindingTerm (var "b")))) $
      "typ" ~>
      Flows.pure (Core.binding
        (decodeBindingName @@ (Core.bindingName (var "b")))
        (decodeType @@ (var "typ"))
        (just (decoderTypeScheme @@ var "typ")))

-- | Generate a fully qualified binding name for a decoder function from a type name
-- For example, "hydra.util.CaseConvention" -> "hydra.decode.util.caseConvention"
decodeBindingName :: TBinding (Name -> Name)
decodeBindingName = define "decodeBindingName" $
  doc "Generate a binding name for a decoder function from a type name" $
  "n" ~>
    -- Check if name has a namespace (contains ".")
    Logic.ifElse (Logic.not (Lists.null
      (Lists.tail (Strings.splitOn (string ".") (Core.unName (var "n"))))))
      -- Qualified type: e.g., "hydra.util.CaseConvention" -> "hydra.decode.util.caseConvention"
      (Core.name (
        Strings.intercalate (string ".") (
          Lists.concat2
            (list [string "hydra", string "decode"])
            (Lists.concat2
              (Lists.tail (Lists.init (Strings.splitOn (string ".") (Core.unName (var "n")))))
              (list [Formatting.decapitalize @@ (Names.localNameOf @@ (var "n"))])))))
      -- Local type: just decapitalize
      (Core.name (Formatting.decapitalize @@ (Names.localNameOf @@ (var "n"))))

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
    decodeLiteral handleLiteral = deannotateAndMatch
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
          (primitive _lists_cons
            @@ (Module.namespace $ string "hydra.lexical")
            @@ (primitive _lists_cons
              @@ (Module.namespace $ string "hydra.rewriting")
              @@ (primitive _lists_map @@ decodeNamespace @@ (Module.moduleTypeDependencies (var "mod")))))
          (list [
            Module.moduleNamespace (var "mod"),
            Module.namespace $ string "hydra.util"])
          (just (Strings.cat $ list [
            string "Term decoders for ",
            Module.unNamespace (Module.moduleNamespace (var "mod"))])))))

-- | Generate a decoder module namespace from a source module namespace
-- For example, "hydra.util" -> "hydra.decode.util"
decodeNamespace :: TBinding (Namespace -> Namespace)
decodeNamespace = define "decodeNamespace" $
  doc "Generate a decoder module namespace from a source module namespace" $
  "ns" ~> (
    Module.namespace (
      Strings.cat $ list [
        string "hydra.decode.",
        Strings.intercalate (string ".")
          (Lists.tail (Strings.splitOn (string ".") (Module.unNamespace (var "ns"))))]))

-- | Generate a decoder for a record type
-- Matches Term.record and extracts the Record with the expected type name
-- Returns Either DecodingError <RecordType>
decodeRecordType :: TBinding (RowType -> Term)
decodeRecordType = define "decodeRecordType" $
  doc "Generate a decoder for a record type" $
  "rt" ~>
  "typeName" <~ (Core.rowTypeTypeName $ var "rt") $
  "fieldTypes" <~ (Core.rowTypeFields $ var "rt") $
  -- For each field, build a term that decodes it from fieldMap
  -- Returns: Either DecodingError fieldValue
  "decodeFieldTerm" <~ ("ft" ~>
    DC.primitive _maybes_maybe
      -- If missing: Left error
      @@@ (DC.left $ DC.wrap _DecodingError $ DC.primitive _strings_cat
        @@@ (DC.list $ list [
          DC.string $ string "missing field ",
          DC.string $ Core.unName $ Core.fieldTypeName $ var "ft",
          DC.string $ string " in record"]))
      -- If found: decode the term (pass cx to the decoder)
      @@@ (DC.lambda "fieldTerm" $
        (decodeType @@ (Core.fieldTypeType $ var "ft")) @@@ DC.var "cx" @@@ DC.var "fieldTerm")
      @@@ (DC.primitive _maps_lookup
        @@@ (DC.wrap _Name $ DC.string $ Core.unName $ Core.fieldTypeName $ var "ft")
        @@@ DC.var "fieldMap")) $
  -- Build the body: a nested chain of eithers.either calls that decode each field and build the record
  -- We need: d1 >>= \v1 -> d2 >>= \v2 -> d3 >>= \v3 -> Right Record{...}
  -- Which is: either Left (\v1 -> either Left (\v2 -> either Left (\v3 -> Right Record{...}) d3) d2) d1
  -- Using foldl on reversed list to build from inside out
  -- The lambda for each field uses the field name as the parameter
  "toFieldLambda" <~ ("ft" ~> "body" ~>
    Core.termFunction $ Core.functionLambda $
      Core.lambda (Core.fieldTypeName $ var "ft") nothing $ var "body") $
  "decodeBody" <~ (
    Lists.foldl
      ("acc" ~> "ft" ~>
        DC.primitive _eithers_either
          @@@ (DC.lambda "err" $ DC.left $ DC.var "err")  -- propagate Left
          @@@ (var "toFieldLambda" @@ var "ft" @@ var "acc")  -- bind Right value and continue
          @@@ (var "decodeFieldTerm" @@ var "ft"))
      -- Base case: Right with the constructed record (wrapped as Term)
      (DC.right $ Core.termRecord $ Core.record (var "typeName") $
        Lists.map ("ft" ~> Core.field (Core.fieldTypeName $ var "ft") $ Core.termVariable $ Core.fieldTypeName $ var "ft")
          (var "fieldTypes"))
      (Lists.reverse $ var "fieldTypes")) $
  deannotateAndMatch
    (just $ leftError (
      Strings.cat $ list [string "expected record of type ", Core.unName (var "typeName")])) [
    DC.field _Term_record $ DC.lambda "record" $
      DC.lets [
        -- Build Map Name Term from the record's fields
        ("fieldMap", DC.primitive _maps_fromList
          @@@ (DC.primitive _lists_map
            @@@ (DC.lambda "f" $ DC.pair
              (DC.project _Field _Field_name @@@ DC.var "f")
              (DC.project _Field _Field_term @@@ DC.var "f"))
            @@@ (DC.project _Record _Record_fields @@@ DC.var "record")))] $
        var "decodeBody"]

-- | Generate a decoder for an Either type
-- Matches Term.either and decodes left/right based on the EitherType
decodeEitherType :: TBinding (EitherType -> Term)
decodeEitherType = define "decodeEitherType" $
  doc "Generate a decoder for an Either type" $
  "et" ~> deannotateAndMatch
    (just $ leftError $ string "expected either value") [
    DC.field _Term_either $ DC.lambda "e" $

      -- Either DecodingError (Either Term Term)
      DC.primitive _eithers_either
        @@@ (DC.primitive _eithers_map @@@ (DC.lambda "x" $ DC.left $ DC.var "x"))
        @@@ (DC.primitive _eithers_map @@@ (DC.lambda "x" $ DC.right $ DC.var "x"))
        @@@ (
          -- Either (Either DecodingError Term) (Either DecodingError Term)
          -- Pass cx to both left and right decoders
          DC.primitive _eithers_bimap
            @@@ ((decodeType @@ Core.eitherTypeLeft (var "et")) @@@ DC.var "cx")
            @@@ ((decodeType @@ Core.eitherTypeRight (var "et")) @@@ DC.var "cx")
            @@@ DC.var "e")]

-- | Generate a decoder for a polymorphic (forall) type
-- For a type like `forall a. T[a]`, generates a lambda that takes a decoder for `a`
-- and returns a decoder for the body type `T[a]`
decodeForallType :: TBinding (ForallType -> Term)
decodeForallType = define "decodeForallType" $
  doc "Generate a decoder for a polymorphic (forall) type" $
  "ft" ~>
    -- Generate a lambda that takes a decoder for the type parameter
    Core.termFunction $ Core.functionLambda $
      Core.lambda
        (decodeBindingName @@ Core.forallTypeParameter (var "ft"))
        nothing
        (decodeType @@ Core.forallTypeBody (var "ft"))

-- | Generate a decoder for a list type
-- Matches Term.list and decodes each element
decodeListType :: TBinding (Type -> Term)
decodeListType = define "decodeListType" $
  doc "Generate a decoder for a list type" $
  "elemType" ~>
    deannotateAndMatch
      (just $ leftError $ string "expected list") [
      DC.field _Term_list $ DC.lambda "xs" $
        DC.primitive _eithers_mapList
          @@@ ((decodeType @@ var "elemType") @@@ DC.var "cx")
          @@@ DC.var "xs"]

-- | Generate a decoder for a map type
-- Matches Term.map, converts to list of pairs, decodes each pair, rebuilds map
decodeMapType :: TBinding (MapType -> Term)
decodeMapType = define "decodeMapType" $
  doc "Generate a decoder for a map type" $
  "mt" ~>
    deannotateAndMatch
      (just $ leftError $ string "expected map") [
      DC.field _Term_map $ DC.lambda "m" $
        -- Convert Map Term Term to [(Term, Term)]
        DC.lets [
          ("pairs", DC.primitive _maps_toList @@@ DC.var "m"),
          -- Decoder for a single pair: (Term, Term) -> Either Error (k, v)
          -- Note: Using "rawKey" and "rawVal" to avoid name collision with type variable decoders
          ("decodePair", DC.lambda "kv" $
            DC.lets [
              ("rawKey", DC.primitive _pairs_first @@@ DC.var "kv"),
              ("rawVal", DC.primitive _pairs_second @@@ DC.var "kv")] $
              -- Decode key, then decode value, combine into pair (pass cx to decoders)
              DC.primitive _eithers_either
                @@@ (DC.lambda "err" $ DC.left $ DC.var "err")
                @@@ (DC.lambda "k2" $
                      DC.primitive _eithers_either
                        @@@ (DC.lambda "err2" $ DC.left $ DC.var "err2")
                        @@@ (DC.lambda "v2" $ DC.right $ DC.pair (DC.var "k2") (DC.var "v2"))
                        @@@ ((decodeType @@ Core.mapTypeValues (var "mt")) @@@ DC.var "cx" @@@ DC.var "rawVal"))
                @@@ ((decodeType @@ Core.mapTypeKeys (var "mt")) @@@ DC.var "cx" @@@ DC.var "rawKey"))] $
          -- Map decodePair over pairs list, then fromList
          DC.primitive _eithers_either
            @@@ (DC.lambda "err" $ DC.left $ DC.var "err")
            @@@ (DC.lambda "decodedPairs" $ DC.right $ DC.primitive _maps_fromList @@@ DC.var "decodedPairs")
            @@@ (DC.primitive _eithers_mapList @@@ DC.var "decodePair" @@@ DC.var "pairs")]

-- | Generate a decoder for an optional/maybe type
-- Matches Term.maybe and decodes the inner value if present
decodeMaybeType :: TBinding (Type -> Term)
decodeMaybeType = define "decodeMaybeType" $
  doc "Generate a decoder for an optional type" $
  "elemType" ~>
    deannotateAndMatch
      (just $ leftError $ string "expected optional value") [
      DC.field _Term_maybe $ DC.lambda "opt" $
        DC.primitive _eithers_mapMaybe
          @@@ ((decodeType @@ var "elemType") @@@ DC.var "cx")
          @@@ DC.var "opt"]

-- | Generate a decoder for a pair type
-- Matches Term.pair and decodes both elements
-- Note: Using "rawFirst" and "rawSecond" to avoid name collision with type variable decoders
decodePairType :: TBinding (PairType -> Term)
decodePairType = define "decodePairType" $
  doc "Generate a decoder for a pair type" $
  "pt" ~>
    deannotateAndMatch
      (just $ leftError $ string "expected pair") [
      DC.field _Term_pair $ DC.lambda "p" $
        DC.lets [
          ("rawFirst", DC.primitive _pairs_first @@@ DC.var "p"),
          ("rawSecond", DC.primitive _pairs_second @@@ DC.var "p")] $
          -- Decode first, then decode second, combine into pair (pass cx to decoders)
          DC.primitive _eithers_either
            @@@ (DC.lambda "err" $ DC.left $ DC.var "err")
            @@@ (DC.lambda "decodedFirst" $
                  DC.primitive _eithers_either
                    @@@ (DC.lambda "err2" $ DC.left $ DC.var "err2")
                    @@@ (DC.lambda "decodedSecond" $ DC.right $ DC.pair (DC.var "decodedFirst") (DC.var "decodedSecond"))
                    @@@ ((decodeType @@ Core.pairTypeSecond (var "pt")) @@@ DC.var "cx" @@@ DC.var "rawSecond"))
            @@@ ((decodeType @@ Core.pairTypeFirst (var "pt")) @@@ DC.var "cx" @@@ DC.var "rawFirst")]

-- | Generate a decoder for a set type
-- Matches Term.set, converts to list, decodes each element, rebuilds set
decodeSetType :: TBinding (Type -> Term)
decodeSetType = define "decodeSetType" $
  doc "Generate a decoder for a set type" $
  "elemType" ~>
    deannotateAndMatch
      (just $ leftError $ string "expected set") [
      DC.field _Term_set $ DC.lambda "s" $
        DC.lets [
          ("elements", DC.primitive _sets_toList @@@ DC.var "s")] $
          -- Map decoder over elements list, then fromList (pass cx to decoder)
          DC.primitive _eithers_either
            @@@ (DC.lambda "err" $ DC.left $ DC.var "err")
            @@@ (DC.lambda "decodedElems" $ DC.right $ DC.primitive _sets_fromList @@@ DC.var "decodedElems")
            @@@ (DC.primitive _eithers_mapList @@@ ((decodeType @@ var "elemType") @@@ DC.var "cx") @@@ DC.var "elements")]

-- | Generate a decoder term for a given Type
-- The generated decoder has the form: \cx -> \term -> ...
-- where cx is a graph context and term is the term to decode
decodeType :: TBinding (Type -> Term)
decodeType = define "decodeType" $
  doc "Generate a decoder term for a Type" $
  match _Type (Just $ DC.lambda "cx" $ DC.lambda "t" $ leftError $ string "unsupported type variant") [
    _Type_annotated>>: "at" ~> decodeType @@ (Core.annotatedTypeBody (var "at")),
    _Type_application>>: "appType" ~>
      -- For type applications like ColumnSchema<t>, apply the function decoder to the argument decoder
      (decodeType @@ Core.applicationTypeFunction (var "appType"))
        @@@ (decodeType @@ Core.applicationTypeArgument (var "appType")),
    _Type_either>>: "et" ~> decodeEitherType @@ var "et",
    _Type_forall>>: "ft" ~> decodeForallType @@ var "ft",
    _Type_list>>: "elemType" ~> decodeListType @@ var "elemType",
    _Type_literal>>: "lt" ~> decodeLiteralType @@ var "lt",
    _Type_map>>: "mt" ~> decodeMapType @@ var "mt",
    _Type_maybe>>: "elemType" ~> decodeMaybeType @@ var "elemType",
    _Type_pair>>: "pt" ~> decodePairType @@ var "pt",
    _Type_record>>: "rt" ~> decodeRecordType @@ var "rt",
    _Type_set>>: "elemType" ~> decodeSetType @@ var "elemType",
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
      -- Pass cx to the field decoder
      (DC.lambda "input" $ DC.primitive _eithers_map
        @@@ (DC.lambda "t" $ Core.termUnion $ Core.injection (var "typeName") $ Core.field (Core.fieldTypeName $ var "ft") $ DC.var "t")
        @@@ ((decodeType @@ (Core.fieldTypeType $ var "ft")) @@@ DC.var "cx" @@@ DC.var "input"))) $
  deannotateAndMatch
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
  deannotateAndMatch
    (just $ leftError $ string "expected a unit value") [
    DC.field _Term_unit $ DC.constant $ DC.right DC.unit]

-- | Generate a decoder for a wrapped type
-- Matches on Term.Wrap, decodes the body, and wraps the result
-- Returns Either DecodingError WrappedType
decodeWrappedType :: TBinding (WrappedType -> Term)
decodeWrappedType = define "decodeWrappedType" $
  doc "Generate a decoder for a wrapped type" $
  "wt" ~> deannotateAndMatch
    (just $ leftError (
      Strings.cat $ list [
        string "expected wrapped type ",
        Core.unName (Core.wrappedTypeTypeName (var "wt"))])) [
    DC.field _Term_wrap $ DC.lambda "wrappedTerm" $
      DC.primitive _eithers_map
        @@@ (DC.lambda "b" $ DC.wrapDynamic (Core.wrappedTypeTypeName $ var "wt") (DC.var "b"))
        -- Pass cx to the body decoder
        @@@ ((decodeType @@ Core.wrappedTypeBody (var "wt")) @@@ DC.var "cx"
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
