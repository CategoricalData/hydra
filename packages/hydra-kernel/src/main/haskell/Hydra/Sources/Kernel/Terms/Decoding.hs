{-# LANGUAGE FlexibleContexts #-}

module Hydra.Sources.Kernel.Terms.Decoding where

-- Standard imports for kernel terms modules
import Hydra.Kernel hiding (literalType, matchRecord, matchUnion)
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
import qualified Hydra.Dsl.Meta.Lib.Strings  as Strings
import qualified Hydra.Dsl.Literals          as Literals
import qualified Hydra.Dsl.LiteralTypes      as LiteralTypes
import qualified Hydra.Dsl.Meta.Base         as MetaBase
import qualified Hydra.Dsl.Meta.Terms        as MetaTerms
import qualified Hydra.Dsl.Meta.Types        as MetaTypes
import qualified Hydra.Dsl.Packaging       as Packaging
import qualified Hydra.Dsl.Parsing      as Parsing
import qualified Hydra.Dsl.Meta.Phantoms     as Phantoms
import           Hydra.Dsl.Meta.Phantoms     as Phantoms hiding (
  elimination, field, fieldType, floatType, floatValue, function, injection, integerType, integerValue, literal,
  literalType, record, term, type_, typeScheme)
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
import qualified Hydra.Sources.Kernel.Terms.Formatting as Formatting
import qualified Hydra.Sources.Kernel.Terms.Extract.Core as ExtractCore
import qualified Hydra.Sources.Kernel.Terms.Lexical as Lexical

import qualified Hydra.Sources.Kernel.Terms.Names as Names
import qualified Hydra.Sources.Kernel.Terms.Rewriting as Rewriting
import qualified Hydra.Sources.Kernel.Terms.Constants as Constants
import qualified Hydra.Sources.Kernel.Terms.Predicates as Predicates
import qualified Hydra.Sources.Kernel.Terms.Show.Core as ShowCore
import qualified Hydra.Sources.Kernel.Terms.Show.Errors as ShowError
import           Prelude hiding ((++))
import qualified Data.Int                    as I
import qualified Data.List                   as L
import qualified Data.Map                    as M
import qualified Data.Set                    as S
import qualified Data.Maybe                  as Y

import qualified Hydra.Dsl.Meta.DeepCore as DeepCore
import           Hydra.Dsl.Meta.DeepCore ((@@@))


ns :: ModuleName
ns = ModuleName "hydra.decoding"

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = definitions,
            moduleDependencies = Bootstrap.unqualifiedDep <$> ([Annotations.ns, ExtractCore.ns, Formatting.ns, Lexical.ns, Names.ns, Predicates.ns, Rewriting.ns, ShowCore.ns] L.++ kernelTypesModuleNames),
            moduleDescription = Just "Functions for generating term decoders from type modules"}
  where
    definitions = [
      toDefinition collectForallVariables,
      toDefinition collectOrdConstrainedVariables,
      toDefinition collectTypeVariables,
      toDefinition collectTypeVariablesFromType,
      toDefinition decodeBinding,
      toDefinition decodeBindingName,
      toDefinition decodeEitherType,
      toDefinition decodeForallType,
      toDefinition decodeListType,
      toDefinition decodeLiteralType,
      toDefinition decodeMapType,
      toDefinition decodeMaybeType,
      toDefinition decodeModule,
      toDefinition decodeNamespace,
      toDefinition decodePairType,
      toDefinition decodeRecordType,
      toDefinition decodeRecordTypeImpl,
      toDefinition decodeRecordTypeNamed,
      toDefinition decodeSetType,
      toDefinition decodeType,
      toDefinition decodeTypeNamed,
      toDefinition decodeUnionType,
      toDefinition decodeUnionTypeNamed,
      toDefinition decodeUnitType,
      toDefinition decodeWrappedType,
      toDefinition decodeWrappedTypeNamed,
      toDefinition decoderFullResultType,
      toDefinition decoderFullResultTypeNamed,
      toDefinition decoderResultType,
      toDefinition decoderType,
      toDefinition decoderTypeNamed,
      toDefinition decoderTypeScheme,
      toDefinition decoderTypeSchemeNamed,
      toDefinition filterTypeBindings,
      toDefinition isDecodableBinding,
      toDefinition prependForallDecoders]

define :: String -> TTerm x -> TTermDefinition x
define = definitionInModule module_

-- | Format a DecodingError as a string
formatDecodingError :: TTerm (DecodingError -> String)
formatDecodingError = "e" ~> unwrap _DecodingError @@ var "e"

--------------------------------------------------------------------------------
-- Helper functions
--------------------------------------------------------------------------------

-- | Build a decoder term that takes cx and raw, strips annotations/dereferences variables, and matches on it
-- The result is: \cx -> \raw -> either (\err -> Left (DecodingError err)) (\stripped -> case stripped of ...) (stripAndDereferenceTermEither cx raw)
-- Note: We use the original inline style - the Python coder should handle this pattern by recognizing
-- when a lambda body is a case application and generating a proper function with match statement.
deannotateAndMatch :: TTerm (Maybe Term) -> [TTerm Field] -> TTerm Term
deannotateAndMatch dflt cses = DeepCore.lambda "cx" $ DeepCore.lambda "raw" $
  DeepCore.primitive _eithers_either
    -- If Left (decoding error), propagate it
    @@@ (DeepCore.lambda "err" $ DeepCore.left $ DeepCore.var "err")
    -- If Right (stripped term), do the case match
    @@@ (DeepCore.lambda "stripped" $ DeepCore.cases _Term (DeepCore.var "stripped") dflt cses)
    -- Call stripWithDecodingError cx raw (returns Either DecodingError Term)
    @@@ (DeepCore.ref ExtractCore.stripWithDecodingError @@@ DeepCore.var "cx" @@@ DeepCore.var "raw")

-- | Helper to create a decoding error term from a message (object-level)
-- Returns: Term.wrap (WrappedTerm "hydra.util.DecodingError" (Term.literal (Literal.string msg)))
decodingErrorTerm :: TTerm String -> TTerm Term
decodingErrorTerm msg = DeepCore.wrap _DecodingError $ DeepCore.string msg

-- | Helper to create a Left (error) result term
-- Returns: Term.either (Left (decodingErrorTerm msg))
leftError :: TTerm String -> TTerm Term
leftError msg = DeepCore.left $ decodingErrorTerm msg

-- | Helper to strip and dereference with DecodingError — delegates to the module-level definition
stripWithDecodingError :: TTerm Graph -> TTerm Term -> TTerm (Either DecodingError Term)
stripWithDecodingError g term = ExtractCore.stripWithDecodingError @@ g @@ term

--------------------------------------------------------------------------------
-- Main decoder functions
--------------------------------------------------------------------------------

-- | Collect just the forall type variables from a type
-- Note: Uses 'cases' instead of 'match' to avoid variable shadowing from eta expansion
collectForallVariables :: TTermDefinition (Type -> [Name])
collectForallVariables = define "collectForallVariables" $
  doc "Collect forall type variable names from a type" $
  "typ" ~>
  cases _Type (var "typ") (Just $ list ([] :: [TTerm Name])) [
    _Type_annotated>>: "at" ~>
      collectForallVariables @@ (Core.annotatedTypeBody (var "at")),
    _Type_forall>>: "ft" ~>
      Lists.cons (Core.forallTypeParameter (var "ft"))
        (collectForallVariables @@ Core.forallTypeBody (var "ft"))]

-- | Collect type variables that need Ord constraints (from Map key or Set element positions).
-- This is a pure function that traverses the type structure without dereferencing type names.
-- The collected variables use their original names; normalization will rename them later.
-- Note: Uses 'cases' instead of 'match' to avoid variable shadowing from eta expansion
-- | Collect type variables that need Ord constraints (from Map key or Set element positions).
-- This is a pure function that traverses the type structure without dereferencing type names.
-- The collected variables use their original names; normalization will rename them later.
-- Note: Uses 'cases' instead of 'match' to avoid variable shadowing from eta expansion
collectOrdConstrainedVariables :: TTermDefinition (Type -> [Name])
collectOrdConstrainedVariables = define "collectOrdConstrainedVariables" $
  doc "Collect type variables needing Ord constraints (from Map key and Set element types)" $
  "typ" ~>
  cases _Type (var "typ") (Just $ list ([] :: [TTerm Name])) [
    _Type_annotated>>: "at" ~>
      collectOrdConstrainedVariables @@ (Core.annotatedTypeBody (var "at")),
    _Type_application>>: "appType" ~>
      Lists.concat2
        (collectOrdConstrainedVariables @@ Core.applicationTypeFunction (var "appType"))
        (collectOrdConstrainedVariables @@ Core.applicationTypeArgument (var "appType")),
    _Type_either>>: "et" ~>
      Lists.concat2
        (collectOrdConstrainedVariables @@ Core.eitherTypeLeft (var "et"))
        (collectOrdConstrainedVariables @@ Core.eitherTypeRight (var "et")),
    _Type_forall>>: "ft" ~>
      collectOrdConstrainedVariables @@ Core.forallTypeBody (var "ft"),
    _Type_list>>: "elemType" ~>
      collectOrdConstrainedVariables @@ var "elemType",
    -- For Map<K, V>, collect all type variables from K (they need Ord)
    -- plus recurse into both K and V for nested Maps/Sets
    _Type_map>>: "mt" ~>
      Lists.concat (list [
        collectTypeVariablesFromType @@ Core.mapTypeKeys (var "mt"),
        collectOrdConstrainedVariables @@ Core.mapTypeKeys (var "mt"),
        collectOrdConstrainedVariables @@ Core.mapTypeValues (var "mt")]),
    _Type_maybe>>: "elemType" ~>
      collectOrdConstrainedVariables @@ var "elemType",
    _Type_pair>>: "pt" ~>
      Lists.concat2
        (collectOrdConstrainedVariables @@ Core.pairTypeFirst (var "pt"))
        (collectOrdConstrainedVariables @@ Core.pairTypeSecond (var "pt")),
    _Type_record>>: "rt" ~>
      Lists.concat $ Lists.map
        ("ft" ~> collectOrdConstrainedVariables @@ Core.fieldTypeType (var "ft"))
        (var "rt"),
    -- For Set<T>, collect all type variables from T (they all need Ord)
    -- plus recurse into T for nested Sets
    _Type_set>>: "elemType" ~>
      Lists.concat2
        (collectTypeVariablesFromType @@ var "elemType")
        (collectOrdConstrainedVariables @@ var "elemType"),
    _Type_union>>: "rt" ~>
      Lists.concat $ Lists.map
        ("ft" ~> collectOrdConstrainedVariables @@ Core.fieldTypeType (var "ft"))
        (var "rt"),
    _Type_wrap>>: "wt" ~>
      collectOrdConstrainedVariables @@ var "wt"]

-- | Collect all type variables from a type expression (for use in Set element types)
-- Note: Uses 'cases' instead of 'match' to avoid variable shadowing from eta expansion
-- | Collect type variables from forall types
-- Note: Graph is NOT included as a type variable - it's a concrete type
collectTypeVariables :: TTermDefinition (Type -> [Name])
collectTypeVariables = define "collectTypeVariables" $
  doc "Collect type variable names from a type (forall parameters only)" $
  "typ" ~> collectForallVariables @@ var "typ"

-- | Collect just the forall type variables from a type
-- Note: Uses 'cases' instead of 'match' to avoid variable shadowing from eta expansion
-- | Collect all type variables from a type expression (for use in Set element types)
-- Note: Uses 'cases' instead of 'match' to avoid variable shadowing from eta expansion
collectTypeVariablesFromType :: TTermDefinition (Type -> [Name])
collectTypeVariablesFromType = define "collectTypeVariablesFromType" $
  doc "Collect all type variable names from a type expression" $
  "typ" ~>
  cases _Type (var "typ") (Just $ list ([] :: [TTerm Name])) [
    _Type_annotated>>: "at" ~>
      collectTypeVariablesFromType @@ (Core.annotatedTypeBody (var "at")),
    _Type_application>>: "appType" ~>
      Lists.concat2
        (collectTypeVariablesFromType @@ Core.applicationTypeFunction (var "appType"))
        (collectTypeVariablesFromType @@ Core.applicationTypeArgument (var "appType")),
    _Type_either>>: "et" ~>
      Lists.concat2
        (collectTypeVariablesFromType @@ Core.eitherTypeLeft (var "et"))
        (collectTypeVariablesFromType @@ Core.eitherTypeRight (var "et")),
    _Type_forall>>: "ft" ~>
      collectTypeVariablesFromType @@ Core.forallTypeBody (var "ft"),
    _Type_list>>: "elemType" ~>
      collectTypeVariablesFromType @@ var "elemType",
    _Type_map>>: "mt" ~>
      Lists.concat2
        (collectTypeVariablesFromType @@ Core.mapTypeKeys (var "mt"))
        (collectTypeVariablesFromType @@ Core.mapTypeValues (var "mt")),
    _Type_maybe>>: "elemType" ~>
      collectTypeVariablesFromType @@ var "elemType",
    _Type_pair>>: "pt" ~>
      Lists.concat2
        (collectTypeVariablesFromType @@ Core.pairTypeFirst (var "pt"))
        (collectTypeVariablesFromType @@ Core.pairTypeSecond (var "pt")),
    _Type_record>>: "rt" ~>
      Lists.concat $ Lists.map
        ("ft" ~> collectTypeVariablesFromType @@ Core.fieldTypeType (var "ft"))
        (var "rt"),
    _Type_set>>: "elemType" ~>
      collectTypeVariablesFromType @@ var "elemType",
    _Type_union>>: "rt" ~>
      Lists.concat $ Lists.map
        ("ft" ~> collectTypeVariablesFromType @@ Core.fieldTypeType (var "ft"))
        (var "rt"),
    _Type_variable>>: "name" ~>
      list [var "name"],
    _Type_wrap>>: "wt" ~>
      collectTypeVariablesFromType @@ var "wt"]

-- | Build the decoder function type for a given type
-- For monomorphic types: Graph -> Term -> Either DecodingError ResultType
-- For polymorphic types: (Graph -> Term -> Either DecodingError a) -> ... -> Graph -> Term -> Either DecodingError ResultType<a>
-- The 'Graph' parameter is used for dereferencing term variables
-- | Decode a single type binding into a decoder binding
-- Decodes the Type from the binding's term, then generates decoder
decodeBinding :: TTermDefinition (Context -> Graph -> Binding -> Either DecodingError Binding)
decodeBinding = define "decodeBinding" $
  doc "Transform a type binding into a decoder binding" $
  "cx" ~> "graph" ~> "b" ~>
    Eithers.bind (decoderFor _Type @@ var "graph" @@ (Core.bindingTerm (var "b"))) (
      "typ" ~>
      "rawBody" <~ (decodeTypeNamed @@ (Core.bindingName (var "b")) @@ (var "typ")) $
      "description" <~ (Strings.cat $ list [
        string "Decoder for ",
        Core.unName (Core.bindingName (var "b"))]) $
      right (Core.binding
        (decodeBindingName @@ (Core.bindingName (var "b")))
        (Annotations.setTermDescription @@ (just (var "description")) @@ var "rawBody")
        (just (decoderTypeSchemeNamed @@ (Core.bindingName (var "b")) @@ var "typ"))))

-- | Generate a fully qualified binding name for a decoder function from a type name
-- For example, "hydra.util.CaseConvention" -> "hydra.decode.util.caseConvention"
-- | Generate a fully qualified binding name for a decoder function from a type name
-- For example, "hydra.util.CaseConvention" -> "hydra.decode.util.caseConvention"
decodeBindingName :: TTermDefinition (Name -> Name)
decodeBindingName = define "decodeBindingName" $
  doc "Generate a binding name for a decoder function from a type name" $
  "n" ~>
  "parts" <~ (Strings.splitOn (string ".") (Core.unName (var "n"))) $
  "localPart" <~ (Formatting.decapitalize @@ (Names.localNameOf @@ (var "n"))) $
  "localResult" <~ (Core.name (var "localPart")) $
  Maybes.maybe
    (var "localResult")
    ("nsParts" ~>
      Maybes.maybe
        (var "localResult")
        ("nsUc" ~>
          "tail" <~ Pairs.second (var "nsUc") $
          Core.name (Strings.intercalate (string ".") (Lists.concat2
            (list [string "hydra", string "decode"])
            (Lists.concat2
              (var "tail")
              (list [var "localPart"])))))
        (Lists.uncons $ var "nsParts"))
    (Lists.maybeInit $ var "parts")

-- | Generate a decoder for a literal type
-- Match on the LiteralType to generate type-specific decoders
-- Note: Uses 'cases' instead of 'match' to avoid variable shadowing from eta expansion
-- | Generate a decoder for an Either type
decodeEitherType :: TTermDefinition (EitherType -> Term)
decodeEitherType = define "decodeEitherType" $
  doc "Generate a decoder for an Either type" $
  "et" ~>
  "leftDecoder" <~ decodeType @@ Core.eitherTypeLeft (var "et") $
  "rightDecoder" <~ decodeType @@ Core.eitherTypeRight (var "et") $
  DeepCore.ref ExtractCore.decodeEither @@@ var "leftDecoder" @@@ var "rightDecoder"

-- | Generate a decoder for a list type
-- | Generate a decoder for a polymorphic (forall) type
-- For a type like `forall a. T[a]`, generates a lambda that takes a decoder for `a`
-- and returns a decoder for the body type `T[a]`
decodeForallType :: TTermDefinition (ForallType -> Term)
decodeForallType = define "decodeForallType" $
  doc "Generate a decoder for a polymorphic (forall) type" $
  "ft" ~>
    -- Generate a lambda that takes a decoder for the type parameter
    Core.termLambda $ Core.lambda
        (decodeBindingName @@ Core.forallTypeParameter (var "ft"))
        nothing
        (decodeType @@ Core.forallTypeBody (var "ft"))

-- | Generate a decoder for an Either type
-- | Generate a decoder for a list type
decodeListType :: TTermDefinition (Type -> Term)
decodeListType = define "decodeListType" $
  doc "Generate a decoder for a list type" $
  "elemType" ~>
  "elemDecoder" <~ decodeType @@ var "elemType" $
  DeepCore.ref ExtractCore.decodeList @@@ var "elemDecoder"

-- | Generate a decoder for a map type
-- | Generate a decoder for a literal type
-- Match on the LiteralType to generate type-specific decoders
-- Note: Uses 'cases' instead of 'match' to avoid variable shadowing from eta expansion
decodeLiteralType :: TTermDefinition (LiteralType -> Term)
decodeLiteralType = define "decodeLiteralType" $
  doc "Generate a decoder for a literal type" $
  "lt" ~>
  cases _LiteralType (var "lt") Nothing [
    _LiteralType_binary>>: constant decodeBinary,
    _LiteralType_boolean>>: constant decodeBoolean,
    _LiteralType_decimal>>: constant decodeDecimal,
    _LiteralType_float>>: "ft" ~> decodeFloat (var "ft"),
    _LiteralType_integer>>: "it" ~> decodeInteger (var "it"),
    _LiteralType_string>>: constant decodeString]
  where
    -- Helper to wrap a Literal handler with Term.literal matching
    decodeLiteral handleLiteral = deannotateAndMatch
      (just $ leftError (string "expected literal")) [
      DeepCore.field _Term_literal $ DeepCore.lambda "v" $ handleLiteral @@@ DeepCore.var "v"]

    -- Decode binary: Term -> Either DecodingError Binary
    decodeBinary = decodeLiteral $ DeepCore.match _Literal
      (just $ leftError (string "expected binary literal")) [
      DeepCore.field _Literal_binary $ DeepCore.lambda "b" $ DeepCore.right $ DeepCore.var "b"]

    -- Decode boolean: Term -> Either DecodingError Bool
    decodeBoolean = decodeLiteral $ DeepCore.match _Literal
      (just $ leftError (string "expected boolean literal")) [
      DeepCore.field _Literal_boolean $ DeepCore.lambda "b" $ DeepCore.right $ DeepCore.var "b"]

    -- Decode decimal: Term -> Either DecodingError Scientific
    decodeDecimal = decodeLiteral $ DeepCore.match _Literal
      (just $ leftError (string "expected decimal literal")) [
      DeepCore.field _Literal_decimal $ DeepCore.lambda "d" $ DeepCore.right $ DeepCore.var "d"]

    -- Decode float: Term -> Either DecodingError <specific float type>
    decodeFloat ft = cases _FloatType ft Nothing [
        _FloatType_float32>>: constant $ decodeFloatVariant _FloatValue_float32 (string "float32"),
        _FloatType_float64>>: constant $ decodeFloatVariant _FloatValue_float64 (string "float64")]

    -- Helper to decode a specific float variant
    decodeFloatVariant floatVariant floatName = decodeLiteral $ DeepCore.match _Literal
      (just $ leftError (Strings.cat $ list [string "expected ", floatName, string " literal"])) [
      DeepCore.field _Literal_float $ DeepCore.match _FloatValue
        (just $ leftError (Strings.cat $ list [string "expected ", floatName, string " value"])) [
        DeepCore.field floatVariant $ DeepCore.lambda "f" $ DeepCore.right $ DeepCore.var "f"]]

    -- Decode integer: Term -> Either DecodingError <specific integer type>
    decodeInteger it = cases _IntegerType it Nothing [
        _IntegerType_bigint>>: constant $ decodeIntegerVariant _IntegerValue_bigint (string "bigint"),
        _IntegerType_int8>>: constant $ decodeIntegerVariant _IntegerValue_int8 (string "int8"),
        _IntegerType_int16>>: constant $ decodeIntegerVariant _IntegerValue_int16 (string "int16"),
        _IntegerType_int32>>: constant $ decodeIntegerVariant _IntegerValue_int32 (string "int32"),
        _IntegerType_int64>>: constant $ decodeIntegerVariant _IntegerValue_int64 (string "int64"),
        _IntegerType_uint8>>: constant $ decodeIntegerVariant _IntegerValue_uint8 (string "uint8"),
        _IntegerType_uint16>>: constant $ decodeIntegerVariant _IntegerValue_uint16 (string "uint16"),
        _IntegerType_uint32>>: constant $ decodeIntegerVariant _IntegerValue_uint32 (string "uint32"),
        _IntegerType_uint64>>: constant $ decodeIntegerVariant _IntegerValue_uint64 (string "uint64")]

    -- Helper to decode a specific integer variant
    decodeIntegerVariant intVariant intName = decodeLiteral $ DeepCore.match _Literal
      (just $ leftError (Strings.cat $ list [string "expected ", intName, string " literal"])) [
      DeepCore.field _Literal_integer $ DeepCore.match _IntegerValue
        (just $ leftError (Strings.cat $ list [string "expected ", intName, string " value"])) [
        DeepCore.field intVariant $ DeepCore.lambda "i" $ DeepCore.right $ DeepCore.var "i"]]

    -- Decode string: Term -> Either DecodingError String
    decodeString = decodeLiteral $ DeepCore.match _Literal
      (just $ leftError (string "expected string literal")) [
      DeepCore.field _Literal_string $ DeepCore.lambda "s" $ DeepCore.right $ DeepCore.var "s"]

-- | Transform a type module into a decoder module
-- Returns Nothing if the module has no decodable type definitions
-- | Generate a decoder for a map type
decodeMapType :: TTermDefinition (MapType -> Term)
decodeMapType = define "decodeMapType" $
  doc "Generate a decoder for a map type" $
  "mt" ~>
  "keyDecoder" <~ decodeType @@ Core.mapTypeKeys (var "mt") $
  "valDecoder" <~ decodeType @@ Core.mapTypeValues (var "mt") $
  DeepCore.ref ExtractCore.decodeMap @@@ var "keyDecoder" @@@ var "valDecoder"

-- | Generate a decoder for an optional/maybe type
-- | Generate a decoder for an optional/maybe type
decodeMaybeType :: TTermDefinition (Type -> Term)
decodeMaybeType = define "decodeMaybeType" $
  doc "Generate a decoder for an optional type" $
  "elemType" ~>
  "elemDecoder" <~ decodeType @@ var "elemType" $
  DeepCore.ref ExtractCore.decodeMaybe @@@ var "elemDecoder"

-- | Generate a decoder for a pair type
-- | Transform a type module into a decoder module
-- Returns Nothing if the module has no decodable type definitions
decodeModule :: TTermDefinition (Context -> Graph -> Module -> Prelude.Either Error (Maybe Module))
decodeModule = define "decodeModule" $
  doc "Transform a type module into a decoder module" $
  "cx" ~> "graph" ~> "mod" ~>
    "typeBindings" <<~ (filterTypeBindings @@ var "cx" @@ var "graph" @@
      (Maybes.cat $ Lists.map
        ("d" ~> cases _Definition (var "d") (Just nothing) [
          _Definition_type>>: "td" ~>
            just (Annotations.typeBinding @@ (Packaging.typeDefinitionName $ var "td") @@ (Core.typeSchemeBody $ Packaging.typeDefinitionTypeScheme $ var "td"))])
        (Packaging.moduleDefinitions (var "mod")))) $
    Logic.ifElse (Lists.null (var "typeBindings"))
      (right nothing)
      ("decodedBindings" <<~ Eithers.mapList ("b" ~>
        Eithers.bimap
          ("_e" ~> Error.errorDecoding $ var "_e")
          ("x" ~> var "x")
          (decodeBinding @@ var "cx" @@ var "graph" @@ var "b")) (var "typeBindings") $
        -- Decoder modules need:
        -- 1. hydra.extract.core, hydra.lexical, hydra.rewriting (for decoding utilities)
        -- 2. Decoded versions of source dependencies (e.g., hydra.core -> hydra.decode.core).
        --    If type A references type B, the decoder for A needs to call the decoder for B.
        -- 3. The original module's namespace (the schema being decoded) and hydra.util
        "allDecodedDeps" <~ (primitive _lists_nub @@ (Lists.map decodeNamespace (Lists.map ("dep" ~> Packaging.moduleDependencyModule (var "dep")) (Packaging.moduleDependencies (var "mod"))))) $
        right (just (Packaging.module_
          (just (Strings.cat $ list [
            string "Term decoders for ",
            Packaging.unModuleName (Packaging.moduleName (var "mod"))]))
          (decodeNamespace @@ (Packaging.moduleName (var "mod")))
          (Lists.map ("ns" ~> Packaging.moduleDependency (var "ns") nothing) (Lists.concat2
            (list [
              (Packaging.moduleName2 $ string "hydra.extract.core"),
              (Packaging.moduleName2 $ string "hydra.lexical"),
              (Packaging.moduleName2 $ string "hydra.rewriting"),
              Packaging.moduleName (var "mod"),
              Packaging.moduleName2 $ string "hydra.util"])
            (var "allDecodedDeps")))
          (Lists.map ("b" ~> Packaging.definitionTerm (Packaging.termDefinition
            (Core.bindingName $ var "b") (Core.bindingTerm $ var "b")
            (Core.bindingTypeScheme $ var "b")))
            (var "decodedBindings")))))

-- | Generate a decoder module namespace from a source module namespace
-- For example, "hydra.util" -> "hydra.decode.util"
-- | Generate a decoder module namespace from a source module namespace
-- For example, "hydra.util" -> "hydra.decode.util"
decodeNamespace :: TTermDefinition (ModuleName -> ModuleName)
decodeNamespace = define "decodeNamespace" $
  doc "Generate a decoder module namespace from a source module namespace" $
  "ns" ~>
  "parts" <~ Strings.splitOn (string ".") (Packaging.unModuleName (var "ns")) $
  "fallback" <~ Packaging.moduleName2 (Packaging.unModuleName (var "ns")) $
  Maybes.maybe
    (var "fallback")
    ("uc" ~>
      Packaging.moduleName2 (
        Strings.cat $ list [
          string "hydra.decode.",
          Strings.intercalate (string ".") (Pairs.second $ var "uc")]))
    (Lists.uncons $ var "parts")

-- | Generate a decoder for a record type with element name
-- | Generate a decoder for a pair type
decodePairType :: TTermDefinition (PairType -> Term)
decodePairType = define "decodePairType" $
  doc "Generate a decoder for a pair type" $
  "pt" ~>
  "firstDecoder" <~ decodeType @@ Core.pairTypeFirst (var "pt") $
  "secondDecoder" <~ decodeType @@ Core.pairTypeSecond (var "pt") $
  DeepCore.ref ExtractCore.decodePair @@@ var "firstDecoder" @@@ var "secondDecoder"

-- | Generate a decoder for a set type
-- | Generate a decoder for a record type (no element name)
decodeRecordType :: TTermDefinition ([FieldType] -> Term)
decodeRecordType = define "decodeRecordType" $
  doc "Generate a decoder for a record type" $
  "rt" ~> decodeRecordTypeImpl @@ Core.name (string "unknown") @@ var "rt"

-- | Generate a decoder for a record type (implementation with name parameter)
-- | Generate a decoder for a record type (implementation with name parameter)
decodeRecordTypeImpl :: TTermDefinition (Name -> [FieldType] -> Term)
decodeRecordTypeImpl = define "decodeRecordTypeImpl" $
  doc "Generate a decoder for a record type with a type name" $
  "tname" ~> "rt" ~>
  -- For each field, build a term that decodes it from fieldMap using requireField helper
  -- Returns: Either DecodingError fieldValue
  "decodeFieldTerm" <~ ("ft" ~>
    DeepCore.ref ExtractCore.requireField
      @@@ (DeepCore.string $ Core.unName $ Core.fieldTypeName $ var "ft")
      @@@ (decodeType @@ (Core.fieldTypeType $ var "ft"))
      @@@ DeepCore.var "fieldMap"
      @@@ DeepCore.var "cx") $
  -- Build the body: a nested chain of eithers.bind calls that decode each field and build the record
  -- We need: d1 >>= \v1 -> d2 >>= \v2 -> d3 >>= \v3 -> Right Record{...}
  -- Using foldl on reversed list to build from inside out
  -- The lambda for each field uses the field name with a prefix to avoid shadowing decoder functions
  "localVarName" <~ ("ft" ~> Core.name $ Strings.cat $ list [string "field_", Core.unName $ Core.fieldTypeName $ var "ft"]) $
  "toFieldLambda" <~ ("ft" ~> "body" ~>
    Core.termLambda $ Core.lambda (var "localVarName" @@ var "ft") nothing $ var "body") $
  "decodeBody" <~ (
    Lists.foldl
      ("acc" ~> "ft" ~>
        DeepCore.primitive _eithers_bind
          @@@ (var "decodeFieldTerm" @@ var "ft")
          @@@ (var "toFieldLambda" @@ var "ft" @@ var "acc"))
      -- Base case: Right with the decoded record value
      (DeepCore.right $ Core.termRecord $ Core.record (var "tname") $
        Lists.map ("ft" ~> Core.field (Core.fieldTypeName $ var "ft") $ Core.termVariable $ var "localVarName" @@ var "ft")
          (var "rt"))
      (Lists.reverse $ var "rt")) $
  deannotateAndMatch
    (just $ leftError (string "expected record")) [
    DeepCore.field _Term_record $ DeepCore.lambda "record" $
      DeepCore.lets [
        -- Build Map Name Term from the record's fields using toFieldMap helper
        ("fieldMap", DeepCore.ref ExtractCore.toFieldMap @@@ DeepCore.var "record")] $
        var "decodeBody"]

-- | Generate a decoder for a polymorphic (forall) type
-- For a type like `forall a. T[a]`, generates a lambda that takes a decoder for `a`
-- and returns a decoder for the body type `T[a]`
-- | Generate a decoder for a record type with element name
decodeRecordTypeNamed :: TTermDefinition (Name -> [FieldType] -> Term)
decodeRecordTypeNamed = define "decodeRecordTypeNamed" $
  doc "Generate a decoder for a record type with element name" $
  "ename" ~> "rt" ~> decodeRecordTypeImpl @@ var "ename" @@ var "rt"

-- | Generate a decoder for a record type (no element name)
-- | Generate a decoder for a set type
decodeSetType :: TTermDefinition (Type -> Term)
decodeSetType = define "decodeSetType" $
  doc "Generate a decoder for a set type" $
  "elemType" ~>
  "elemDecoder" <~ decodeType @@ var "elemType" $
  DeepCore.ref ExtractCore.decodeSet @@@ var "elemDecoder"

-- | Generate a decoder term for a given Type with element name context
-- | Generate a decoder term for a given Type (without element name context)
decodeType :: TTermDefinition (Type -> Term)
decodeType = define "decodeType" $
  doc "Generate a decoder term for a Type" $
  "typ" ~>
  cases _Type (var "typ")
    (Just $ DeepCore.lambda "cx" $ DeepCore.lambda "t" $ leftError $ string "unsupported type variant") [
    _Type_annotated>>: "at" ~> decodeType @@ (Core.annotatedTypeBody (var "at")),
    _Type_application>>: "appType" ~>
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
    _Type_void>>: constant decodeUnitType,
    _Type_wrap>>: "wt" ~> decodeWrappedType @@ var "wt",
    _Type_variable>>: "typeName" ~> Core.termVariable (decodeBindingName @@ var "typeName")]

-- | Generate a decoder for the unit type
-- | Generate a decoder term for a given Type with element name context
decodeTypeNamed :: TTermDefinition (Name -> Type -> Term)
decodeTypeNamed = define "decodeTypeNamed" $
  doc "Generate a decoder term for a Type, with element name for nominal types" $
  "ename" ~> "typ" ~>
  cases _Type (var "typ")
    (Just $ DeepCore.lambda "cx" $ DeepCore.lambda "t" $ leftError $ string "unsupported type variant") [
    _Type_annotated>>: "at" ~> decodeTypeNamed @@ var "ename" @@ (Core.annotatedTypeBody (var "at")),
    _Type_application>>: "appType" ~>
      (decodeType @@ Core.applicationTypeFunction (var "appType"))
        @@@ (decodeType @@ Core.applicationTypeArgument (var "appType")),
    _Type_either>>: "et" ~> decodeEitherType @@ var "et",
    _Type_forall>>: "ft" ~>
      Core.termLambda $ Core.lambda (decodeBindingName @@ Core.forallTypeParameter (var "ft")) nothing
          (decodeTypeNamed @@ var "ename" @@ Core.forallTypeBody (var "ft")),
    _Type_list>>: "elemType" ~> decodeListType @@ var "elemType",
    _Type_literal>>: "lt" ~> decodeLiteralType @@ var "lt",
    _Type_map>>: "mt" ~> decodeMapType @@ var "mt",
    _Type_maybe>>: "elemType" ~> decodeMaybeType @@ var "elemType",
    _Type_pair>>: "pt" ~> decodePairType @@ var "pt",
    _Type_record>>: "rt" ~> decodeRecordTypeNamed @@ var "ename" @@ var "rt",
    _Type_set>>: "elemType" ~> decodeSetType @@ var "elemType",
    _Type_union>>: "rt" ~> decodeUnionTypeNamed @@ var "ename" @@ var "rt",
    _Type_unit>>: constant decodeUnitType,
    _Type_void>>: constant decodeUnitType,
    _Type_wrap>>: "wt" ~> decodeWrappedTypeNamed @@ var "ename" @@ var "wt",
    _Type_variable>>: "typeName" ~> Core.termVariable (decodeBindingName @@ var "typeName")]

-- | Generate a decoder term for a given Type (without element name context)
-- | Generate a decoder for a union type (without element name)
decodeUnionType :: TTermDefinition ([FieldType] -> Term)
decodeUnionType = define "decodeUnionType" $
  doc "Generate a decoder for a union type" $
  "rt" ~> decodeUnionTypeNamed @@ Core.name (string "unknown") @@ var "rt"

-- | Generate a decoder for a wrapped type with element name
-- | Generate a decoder for a union type with element name
decodeUnionTypeNamed :: TTermDefinition (Name -> [FieldType] -> Term)
decodeUnionTypeNamed = define "decodeUnionTypeNamed" $
  doc "Generate a decoder for a union type with the given element name" $
  "ename" ~> "rt" ~>
  "toVariantPair" <~ ("ft" ~>
    DeepCore.pair
      (DeepCore.wrap _Name $ DeepCore.string $ Core.unName $ Core.fieldTypeName $ var "ft")
      (DeepCore.lambda "input" $ DeepCore.primitive _eithers_map
        @@@ (DeepCore.lambda "t" $ Core.termInject $ Core.injection (var "ename") $ Core.field (Core.fieldTypeName $ var "ft") $ DeepCore.var "t")
        @@@ ((decodeType @@ (Core.fieldTypeType $ var "ft")) @@@ DeepCore.var "cx" @@@ DeepCore.var "input"))) $
  deannotateAndMatch
    (just $ leftError $ string "expected union") [
    DeepCore.field _Term_inject $ DeepCore.lambda "inj" $ DeepCore.lets [
      ("field", DeepCore.project _Injection _Injection_field @@@ DeepCore.var "inj"),
      ("fname", DeepCore.project _Field _Field_name @@@ DeepCore.var "field"),
      ("fterm", DeepCore.project _Field _Field_term @@@ DeepCore.var "field"),
      ("variantMap", DeepCore.primitive _maps_fromList
        @@@ (DeepCore.list $ Lists.map (var "toVariantPair") $ var "rt"))] $
      DeepCore.primitive _maybes_maybe
        @@@ (DeepCore.left $ DeepCore.wrap _DecodingError $ DeepCore.primitive _strings_cat
          @@@ (DeepCore.list $ list [
            DeepCore.string $ string "no such field ",
            DeepCore.unwrap _Name @@@ DeepCore.var "fname",
            DeepCore.string $ string " in union"]))
        @@@ (DeepCore.lambda "f" $ DeepCore.var "f" @@@ DeepCore.var "fterm")
        @@@ (DeepCore.primitive _maps_lookup
          @@@ DeepCore.var "fname"
          @@@ DeepCore.var "variantMap")]

-- | Generate a decoder for a union type (without element name)
-- | Generate a decoder for the unit type
decodeUnitType :: TTermDefinition Term
decodeUnitType = define "decodeUnitType" $
  doc "Generate a decoder for the unit type" $
  DeepCore.lambda "cx" $ DeepCore.lambda "t" $ DeepCore.ref ExtractCore.decodeUnit @@@ DeepCore.var "cx" @@@ DeepCore.var "t"

-- | Generate a decoder for a union type with element name
-- | Generate a decoder for a wrapped type (without element name)
decodeWrappedType :: TTermDefinition (Type -> Term)
decodeWrappedType = define "decodeWrappedType" $
  doc "Generate a decoder for a wrapped type" $
  "wt" ~> decodeWrappedTypeNamed @@ Core.name (string "unknown") @@ var "wt"

-- | Filter bindings to only decodable type definitions
-- | Generate a decoder for a wrapped type with element name
decodeWrappedTypeNamed :: TTermDefinition (Name -> Type -> Term)
decodeWrappedTypeNamed = define "decodeWrappedTypeNamed" $
  doc "Generate a decoder for a wrapped type with the given element name" $
  "ename" ~> "wt" ~>
  "bodyDecoder" <~ decodeType @@ var "wt" $
  deannotateAndMatch
    (just $ leftError (string "expected wrapped type")) [
    DeepCore.field _Term_wrap $ DeepCore.lambda "wrappedTerm" $
      DeepCore.primitive _eithers_map
        @@@ (DeepCore.lambda "b" $ DeepCore.wrapDynamic (var "ename") (DeepCore.var "b"))
        @@@ (var "bodyDecoder" @@@ DeepCore.var "cx"
          @@@ (DeepCore.project _WrappedTerm _WrappedTerm_body @@@ DeepCore.var "wrappedTerm"))]

-- | Generate a decoder for a wrapped type (without element name)
-- | Get the full result type for a decoder, preserving type applications
-- For forall t. ColumnSchema<t>, returns ColumnSchema<t> (as a Type, not just a Name)
-- Note: Uses 'cases' instead of 'match' to avoid variable shadowing from eta expansion
decoderFullResultType :: TTermDefinition (Type -> Type)
decoderFullResultType = define "decoderFullResultType" $
  doc "Get full result type for decoder" $
  "typ" ~>
  cases _Type (var "typ") (Just $ Core.typeVariable (Core.nameLift _Term)) [
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
    _Type_record>>: constant (Core.typeVariable (Core.nameLift _Term)),
    _Type_set>>: "elemType" ~>
      -- Set a -> Set (decoded a)
      Core.typeSet (decoderFullResultType @@ var "elemType"),
    _Type_union>>: constant (Core.typeVariable (Core.nameLift _Term)),
    _Type_unit>>: constant Core.typeUnit,
    _Type_variable>>: "name" ~>
      Core.typeVariable (var "name"),
    _Type_void>>: constant Core.typeVoid,
    _Type_wrap>>: constant (Core.typeVariable (Core.nameLift _Term))]

-- | Decode a single type binding into a decoder binding
-- Decodes the Type from the binding's term, then generates decoder
-- | Get full result type for decoder with element name
decoderFullResultTypeNamed :: TTermDefinition (Name -> Type -> Type)
decoderFullResultTypeNamed = define "decoderFullResultTypeNamed" $
  doc "Get full result type for decoder with element name" $
  "ename" ~> "typ" ~>
  cases _Type (var "typ") (Just $ Core.typeVariable (Core.nameLift _Term)) [
    _Type_annotated>>: "at" ~>
      decoderFullResultTypeNamed @@ var "ename" @@ (Core.annotatedTypeBody (var "at")),
    _Type_forall>>: "ft" ~>
      Core.typeApplication $ Core.applicationType
        (decoderFullResultTypeNamed @@ var "ename" @@ Core.forallTypeBody (var "ft"))
        (Core.typeVariable (Core.forallTypeParameter (var "ft"))),
    _Type_record>>: constant (Core.typeVariable (var "ename")),
    _Type_union>>: constant (Core.typeVariable (var "ename")),
    _Type_wrap>>: constant (Core.typeVariable (var "ename")),
    _Type_application>>: "appType" ~>
      Core.typeApplication $ Core.applicationType
        (decoderFullResultType @@ Core.applicationTypeFunction (var "appType"))
        (Core.applicationTypeArgument (var "appType")),
    _Type_either>>: "et" ~>
      Core.typeEither $ Core.eitherType
        (decoderFullResultType @@ Core.eitherTypeLeft (var "et"))
        (decoderFullResultType @@ Core.eitherTypeRight (var "et")),
    _Type_list>>: "elemType" ~>
      Core.typeList (decoderFullResultType @@ var "elemType"),
    _Type_literal>>: "_" ~>
      Core.typeVariable (Core.nameLift _Literal),
    _Type_map>>: "mt" ~>
      Core.typeMap $ Core.mapType
        (decoderFullResultType @@ Core.mapTypeKeys (var "mt"))
        (decoderFullResultType @@ Core.mapTypeValues (var "mt")),
    _Type_maybe>>: "elemType" ~>
      Core.typeMaybe (decoderFullResultType @@ var "elemType"),
    _Type_pair>>: "pt" ~>
      Core.typePair $ Core.pairType
        (decoderFullResultType @@ Core.pairTypeFirst (var "pt"))
        (decoderFullResultType @@ Core.pairTypeSecond (var "pt")),
    _Type_set>>: "elemType" ~>
      Core.typeSet (decoderFullResultType @@ var "elemType"),
    _Type_unit>>: constant Core.typeUnit,
    _Type_variable>>: "name" ~>
      Core.typeVariable (var "name"),
    _Type_void>>: constant Core.typeVoid]

-- | Collect type variables from forall types
-- Note: Graph is NOT included as a type variable - it's a concrete type
-- | Compute the result type for a decoder based on the input type
-- Returns the domain type name for the decoded value
-- Note: Uses 'cases' instead of 'match' to avoid variable shadowing from eta expansion
decoderResultType :: TTermDefinition (Type -> Name)
decoderResultType = define "decoderResultType" $
  doc "Compute the result type name for a decoder" $
  "typ" ~>
  cases _Type (var "typ") (Just (Core.nameLift _Term)) [
    _Type_annotated>>: "at" ~>
      decoderResultType @@ (Core.annotatedTypeBody (var "at")),
    _Type_application>>: "appType" ~>
      -- For type applications like ColumnSchema<t>, get result type from function part
      decoderResultType @@ (Core.applicationTypeFunction (var "appType")),
    _Type_forall>>: "ft" ~>
      decoderResultType @@ (Core.forallTypeBody (var "ft")),
    _Type_literal>>: "_" ~>
      Core.nameLift _Literal,
    _Type_record>>: constant (Core.nameLift _Term),
    _Type_union>>: constant (Core.nameLift _Term),
    _Type_wrap>>: constant (Core.nameLift _Term)]

-- | Build a decoder type scheme: Term -> Either DecodingError ResultType
-- For polymorphic types, adds extra arguments for the decoders of type parameters
-- Includes Ord constraints for type variables that appear in Set element positions
-- | Build the decoder function type for a given type
-- For monomorphic types: Graph -> Term -> Either DecodingError ResultType
-- For polymorphic types: (Graph -> Term -> Either DecodingError a) -> ... -> Graph -> Term -> Either DecodingError ResultType<a>
-- The 'Graph' parameter is used for dereferencing term variables
decoderType :: TTermDefinition (Type -> Type)
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
-- Note: Uses 'cases' instead of 'match' to avoid variable shadowing from eta expansion
-- | Build decoder function type with element name
decoderTypeNamed :: TTermDefinition (Name -> Type -> Type)
decoderTypeNamed = define "decoderTypeNamed" $
  doc "Build decoder function type with element name" $
  "ename" ~> "typ" ~>
    "resultType" <~ (decoderFullResultTypeNamed @@ var "ename" @@ var "typ") $
    "baseType" <~ Core.typeFunction (Core.functionType
      (Core.typeVariable (Core.nameLift _Graph))
      (Core.typeFunction (Core.functionType
        (Core.typeVariable (Core.nameLift _Term))
        (Core.typeEither (Core.eitherType
          (Core.typeVariable (Core.nameLift _DecodingError))
          (var "resultType")))))) $
    prependForallDecoders @@ var "baseType" @@ var "typ"

-- | Get full result type for decoder with element name
-- | Build a decoder type scheme: Term -> Either DecodingError ResultType
-- For polymorphic types, adds extra arguments for the decoders of type parameters
-- Includes Ord constraints for type variables that appear in Set element positions
decoderTypeScheme :: TTermDefinition (Type -> TypeScheme)
decoderTypeScheme = define "decoderTypeScheme" $
  doc "Build type scheme for a decoder function" $
  "typ" ~>
    "typeVars" <~ collectTypeVariables @@ var "typ" $
    "allOrdVars" <~ collectOrdConstrainedVariables @@ var "typ" $
    -- Filter to only include actual forall-bound type variables
    -- (collectOrdConstrainedVariables may return nominal type references like "hydra.relational.ColumnName")
    "ordVars" <~ Lists.filter
      ("v" ~> Lists.elem (var "v" :: TTerm Name) (var "typeVars" :: TTerm [Name]))
      (var "allOrdVars") $
    -- Build constraints: for each ordVar, add Ord constraint (uses original var names, normalization renames them)
    "constraints" <~ (
      Logic.ifElse (Lists.null (var "ordVars"))
        Phantoms.nothing
        (just $ Maps.fromList $ Lists.map
          ("v" ~> pair (var "v") (Core.typeVariableMetadata $ Sets.singleton $ Core.nameLift _TypeClass_ordering))
          (var "ordVars"))) $
    Core.typeScheme
      (var "typeVars")
      (decoderType @@ var "typ")
      (var "constraints")

-- | Build a decoder type scheme with element name for nominal types
-- | Build a decoder type scheme with element name for nominal types
decoderTypeSchemeNamed :: TTermDefinition (Name -> Type -> TypeScheme)
decoderTypeSchemeNamed = define "decoderTypeSchemeNamed" $
  doc "Build type scheme for a decoder function with element name" $
  "ename" ~> "typ" ~>
    "typeVars" <~ collectTypeVariables @@ var "typ" $
    "allOrdVars" <~ collectOrdConstrainedVariables @@ var "typ" $
    "ordVars" <~ Lists.filter
      ("v" ~> Lists.elem (var "v" :: TTerm Name) (var "typeVars" :: TTerm [Name]))
      (var "allOrdVars") $
    "constraints" <~ (
      Logic.ifElse (Lists.null (var "ordVars"))
        Phantoms.nothing
        (just $ Maps.fromList $ Lists.map
          ("v" ~> pair (var "v") (Core.typeVariableMetadata $ Sets.singleton $ Core.nameLift _TypeClass_ordering))
          (var "ordVars"))) $
    Core.typeScheme
      (var "typeVars")
      (decoderTypeNamed @@ var "ename" @@ var "typ")
      (var "constraints")

-- | Build decoder function type with element name
-- | Filter bindings to only decodable type definitions
filterTypeBindings :: TTermDefinition (Context -> Graph -> [Binding] -> Prelude.Either Error [Binding])
filterTypeBindings = define "filterTypeBindings" $
  doc "Filter bindings to only decodable type definitions" $
  "cx" ~> "graph" ~> "bindings" ~>
  Eithers.map (primitive _maybes_cat) $
    Eithers.mapList (isDecodableBinding @@ var "cx" @@ var "graph") $
      primitive _lists_filter @@ Annotations.isNativeType @@ var "bindings"

-- | Check if a binding is decodable and return Just binding if so, Nothing otherwise
-- | Check if a binding is decodable and return Just binding if so, Nothing otherwise
isDecodableBinding :: TTermDefinition (Context -> Graph -> Binding -> Prelude.Either Error (Maybe Binding))
isDecodableBinding = define "isDecodableBinding" $
  doc "Check if a binding is decodable (serializable type)" $
  "cx" ~> "graph" ~> "b" ~>
    "serializable" <<~ Predicates.isSerializableByName @@ var "cx" @@ var "graph" @@ (Core.bindingName (var "b")) $
    right (Logic.ifElse (var "serializable") (just (var "b")) nothing)
-- | Helper to prepend decoder types for forall parameters
-- For forall a. forall b. T: prepends (Graph -> Term -> E a) -> (Graph -> Term -> E b) -> to the base type
-- Note: Uses 'cases' instead of 'match' to avoid variable shadowing from eta expansion
prependForallDecoders :: TTermDefinition (Type -> Type -> Type)
prependForallDecoders = define "prependForallDecoders" $
  doc "Prepend decoder types for forall parameters to base type" $
  "baseType" ~> "typ" ~> cases _Type (var "typ") (Just $ var "baseType") [
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
-- Note: Uses 'cases' instead of 'match' to avoid variable shadowing from eta expansion
