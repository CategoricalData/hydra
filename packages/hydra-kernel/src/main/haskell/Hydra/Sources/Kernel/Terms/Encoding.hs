{-# LANGUAGE FlexibleContexts #-}

module Hydra.Sources.Kernel.Terms.Encoding where

-- Standard imports for kernel terms modules
import Hydra.Kernel hiding (literalType)
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
import qualified Hydra.Dsl.Meta.Lib.Optionals   as Optionals
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
import qualified Hydra.Dsl.Errors       as Error
import qualified Hydra.Dsl.Meta.Variants     as Variants
import           Hydra.Sources.Kernel.Types.All
import qualified Hydra.Sources.Kernel.Terms.Annotations as Annotations
import qualified Hydra.Sources.Decode.Core as DecodeCore

import qualified Hydra.Sources.Kernel.Terms.Formatting as Formatting
import qualified Hydra.Sources.Kernel.Terms.Names as Names
import qualified Hydra.Sources.Kernel.Terms.Rewriting as Rewriting
import qualified Hydra.Sources.Kernel.Terms.Predicates as Predicates
import qualified Hydra.Sources.Kernel.Terms.Scoping as Scoping
import qualified Hydra.Dsl.Meta.DeepCore as DeepCore
import           Hydra.Dsl.Meta.DeepCore ((@@@))
import           Prelude hiding ((++))
import qualified Data.Int                    as I
import qualified Data.List                   as L
import qualified Data.Map                    as M
import qualified Data.Set                    as S
import qualified Data.Maybe                  as Y
import qualified Hydra.Lib.Eithers as DefEithers
import qualified Hydra.Lib.Lists as DefLists
import qualified Hydra.Lib.Maps as DefMaps
import qualified Hydra.Lib.Optionals as DefOptionals
import qualified Hydra.Lib.Pairs as DefPairs
import qualified Hydra.Lib.Sets as DefSets


ns :: ModuleName
ns = ModuleName "hydra.encoding"

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = definitions,
            moduleDependencies = Bootstrap.unqualifiedDep <$> ([Annotations.ns, moduleName DecodeCore.module_, Formatting.ns, Names.ns, Predicates.ns, Rewriting.ns] L.++ kernelTypesModuleNames),
            moduleMetadata = Bootstrap.descriptionMetadata (Just "Functions for generating term encoders from type modules")}
  where
    definitions = [
      toDefinition encodeBinding,
      toDefinition encodeBindingName,
      toDefinition encodeEitherType,
      toDefinition encodeFieldValue,
      toDefinition encodeFloatValue,
      toDefinition encodeForallType,
      toDefinition encodeInjection,
      toDefinition encodeIntegerValue,
      toDefinition encodeListType,
      toDefinition encodeLiteralType,
      toDefinition encodeMapType,
      toDefinition encodeModule,
      toDefinition encodeModuleName,
      toDefinition encodeName,
      toDefinition encodeOptionalType,
      toDefinition encodePairType,
      toDefinition encodeRecordType,
      toDefinition encodeRecordTypeNamed,
      toDefinition encodeSetType,
      toDefinition encodeType,
      toDefinition encodeTypeNamed,
      toDefinition encodeUnionType,
      toDefinition encodeUnionTypeNamed,
      toDefinition encodeWrappedType,
      toDefinition encodeWrappedTypeNamed,
      toDefinition encoderCollectForallVariables,
      toDefinition encoderCollectOrdVars,
      toDefinition encoderCollectTypeVarsFromType,
      toDefinition encoderFullResultType,
      toDefinition encoderFullResultTypeNamed,
      toDefinition encoderType,
      toDefinition encoderTypeNamed,
      toDefinition encoderTypeScheme,
      toDefinition encoderTypeSchemeNamed,
      toDefinition filterTypeBindings,
      toDefinition isEncodableBinding,
      toDefinition isUnitType_,
      toDefinition prependForallEncoders]

define :: String -> TypedTerm x -> TypedTermDefinition x
define = definitionInModule module_

-- | Encode a single type binding into an encoder binding
-- This decodes the term to a Type, then generates an encoder function.
-- Type variables that appear as Map keys or Set elements get Ord constraints
-- via the encoder type scheme.
encodeBinding :: TypedTermDefinition (InferenceContext -> Graph -> Binding -> Either DecodingError Binding)
encodeBinding = define "encodeBinding" $
  doc "Transform a type binding into an encoder binding" $
  "cx" ~> "graph" ~> "b" ~>
    Eithers.bind (decoderFor _Type @@ var "graph" @@ (Core.bindingTerm (var "b"))) (
      "typ" ~>
      "rawBody" <~ (encodeTypeNamed @@ (Core.bindingName (var "b")) @@ (var "typ")) $
      "description" <~ (Strings.cat $ list [
        string "Encoder for ",
        Core.unName (Core.bindingName (var "b"))]) $
      right (Core.binding
        (encodeBindingName @@ (Core.bindingName (var "b")))
        (Annotations.setTermDescription @@ (just (var "description")) @@ var "rawBody")
        (just (encoderTypeSchemeNamed @@ (Core.bindingName (var "b")) @@ var "typ"))))

-- | Construct a TypeScheme for an encoder function from a source type definition.
-- For a type like @forall v. Graph v@ (where Graph has Map v (Vertex v)),
-- produces the scheme @forall v. (v -> Term) -> Graph v -> Term@ with Ord constraint on v.
--
-- The type scheme uses the same variable names as the original type definition.
-- Variable normalization (to t0, t1, etc.) happens later in the pipeline and
-- handles renaming both variables and constraint keys consistently.
-- | Generate a fully qualified binding name for an encoder function from a type name
-- For example, "hydra.core.Name" -> "hydra.encode.core.name"
-- For local types (no namespace), returns just the decapitalized local name
encodeBindingName :: TypedTermDefinition (Name -> Name)
encodeBindingName = define "encodeBindingName" $
  doc "Generate a binding name for an encoder function from a type name" $
  "n" ~>
  "parts" <~ (Strings.splitOn (string ".") (Core.unName (var "n"))) $
  "localPart" <~ (Formatting.decapitalize @@ (Names.localNameOf @@ (var "n"))) $
  "localResult" <~ (Core.name (var "localPart")) $
  -- Extract namespace parts (all but the last); if the name has no namespace,
  -- fall back to the bare decapitalized local name.
  Optionals.cases (Lists.maybeInit $ var "parts") (var "localResult") ("nsParts" ~>
      Optionals.cases
        (Lists.uncons $ var "nsParts")
        (var "localResult")  -- unreachable: nsParts empty means no namespace
        ("nsUc" ~>
          "tail" <~ Pairs.second (var "nsUc") $
          Core.name (Strings.intercalate (string ".") (Lists.concat2
            (list [string "hydra", string "encode"])
            (Lists.concat2
              (var "tail")
              (list [var "localPart"]))))))

-- | Generate the encoder term for a field value
-- Creates a lambda that encodes the field value and wraps in an encoded union/injection

-- | Generate an encoder for an Either type
-- Generates a case match over Left/Right variants
encodeEitherType :: TypedTermDefinition (EitherType -> Term)
encodeEitherType = define "encodeEitherType" $
  doc "Generate an encoder for an Either type" $
  "et" ~>
    -- eithers.bimap : forall x,y,z,w. (x->z)->(y->w)->either<x,y>->either<z,w>;
    -- x=left, y=right, z=w=Term. Input domain: either<left,right>. (#476)
    MetaTerms.lambdaTyped "e" (Core.typeEither $ Core.eitherType
        (encoderFullResultType @@ Core.eitherTypeLeft (var "et"))
        (encoderFullResultType @@ Core.eitherTypeRight (var "et"))) $
      DeepCore.injection _Term (DeepCore.field _Term_either
        (MetaTerms.tyapps (DeepCore.primitiveEncoded DefEithers.bimap)
          [encoderFullResultType @@ Core.eitherTypeLeft (var "et"),
           encoderFullResultType @@ Core.eitherTypeRight (var "et"),
           Core.typeVariable (Core.nameLift _Term),
           Core.typeVariable (Core.nameLift _Term)]
          @@@ (encodeType @@ Core.eitherTypeLeft (var "et"))
          @@@ (encodeType @@ Core.eitherTypeRight (var "et"))
          @@@ DeepCore.var "e"))

-- | Generate an encoder for a polymorphic (forall) type
-- For a type like `forall a. T[a]`, generates a lambda that takes an encoder for `a`
-- and returns an encoder for the body type `T[a]`
-- | Generate the encoder term for a field value
-- Creates a lambda that encodes the field value and wraps in an encoded union/injection
encodeFieldValue :: TypedTermDefinition (Name -> Name -> Type -> Term)
encodeFieldValue = define "encodeFieldValue" $
  doc "Generate the encoder for a field's value" $
  "typeName" ~> "fieldName" ~> "fieldType" ~>
    -- Create a lambda that encodes the value and wraps in Term.union with injection.
    -- Domain = the field's (encoder-input) type. (#476)
    -- Note: use "y" instead of "v" to avoid shadowing type variable parameters named "v"
    MetaTerms.lambdaTyped "y" (encoderFullResultType @@ var "fieldType") $
      -- Build Term.union containing an encoded Injection with the encoded value
      DeepCore.injection _Term (DeepCore.field _Term_inject
        (encodeInjection @@ var "typeName" @@ var "fieldName"
          @@ ((encodeType @@ var "fieldType") @@@ DeepCore.var "y")))

-- | Encode an Injection as a Term (produces a Record of type hydra.core.Injection)
-- | Encode a float value based on its float type
-- Wraps the value in the appropriate FloatValue variant as an injection
encodeFloatValue :: TypedTermDefinition (FloatType -> Term -> Term)
encodeFloatValue = define "encodeFloatValue" $
  doc "Encode a float value based on its float type" $
  "floatType" ~> "valTerm" ~>
    Core.termInject $ Core.injection
      (Core.nameLift _FloatValue)
      (Core.field (floatTypeToFieldName @@ var "floatType") (var "valTerm"))
  where
    floatTypeToFieldName :: TypedTerm (FloatType -> Name)
    floatTypeToFieldName = match _FloatType Nothing [
      _FloatType_float32>>:  constant $ Core.nameLift _FloatValue_float32,
      _FloatType_float64>>:  constant $ Core.nameLift _FloatValue_float64]

-- | Generate an encoder for a list type
-- Maps the element encoder over the list and wraps in Term.list
-- | Generate an encoder for a polymorphic (forall) type
-- For a type like `forall a. T[a]`, generates a lambda that takes an encoder for `a`
-- and returns an encoder for the body type `T[a]`
encodeForallType :: TypedTermDefinition (ForallType -> Term)
encodeForallType = define "encodeForallType" $
  doc "Generate an encoder for a polymorphic (forall) type" $
  "ft" ~>
    -- Generate a lambda that takes an encoder for the type parameter
    Core.termLambda $ Core.lambda
        (encodeBindingName @@ Core.forallTypeParameter (var "ft"))
        nothing
        (encodeType @@ Core.forallTypeBody (var "ft"))

-- | Generate an encoder for a Maybe type
-- Encodes the inner value if present and wraps in Term.maybe
-- | Encode an Injection as a Term (produces a Record of type hydra.core.Injection)
encodeInjection :: TypedTermDefinition (Name -> Name -> Term -> Term)
encodeInjection = define "encodeInjection" $
  doc "Encode an Injection as a term" $
  "typeName" ~> "fieldName" ~> "fieldTerm" ~> DeepCore.record _Injection [
    DeepCore.field _Injection_typeName (encodeName @@ var "typeName"),
    DeepCore.field _Injection_field (encodeField @@ var "fieldName" @@ var "fieldTerm")]
  where
    -- Encode a Field as a Term (produces a Record of type hydra.core.Field)
    encodeField :: TypedTerm (Name -> Term -> Term)
    encodeField = "fname" ~> "fterm" ~> DeepCore.record _Field [
      DeepCore.field _Field_name (encodeName @@ var "fname"),
      DeepCore.field _Field_term (var "fterm")]

-- | Generate an encoder for a literal type
-- For literals, the input is a native Haskell value (e.g., String, Int32).
-- We need to wrap it in encoded TermLiteral with the appropriate Literal constructor.
-- | Encode an integer value based on its integer type
-- Wraps the value in the appropriate IntegerValue variant as an injection
encodeIntegerValue :: TypedTermDefinition (IntegerType -> Term -> Term)
encodeIntegerValue = define "encodeIntegerValue" $
  doc "Encode an integer value based on its integer type" $
  "intType" ~> "valTerm" ~>
    Core.termInject $ Core.injection
      (Core.nameLift _IntegerValue)
      (Core.field (intTypeToFieldName @@ var "intType") (var "valTerm"))
  where
    intTypeToFieldName :: TypedTerm (IntegerType -> Name)
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
-- | Generate an encoder for a list type
-- Maps the element encoder over the list and wraps in Term.list
encodeListType :: TypedTermDefinition (Type -> Term)
encodeListType = define "encodeListType" $
  doc "Generate an encoder for a list type" $
  "elemType" ~>
    -- The encoder takes a list of source elements (list<elemType>) and produces a Term.
    MetaTerms.lambdaTyped "xs" (Core.typeList (encoderFullResultType @@ var "elemType")) $
      DeepCore.injection _Term (DeepCore.field _Term_list
        -- lists.map : forall x,y. (x->y) -> list<x> -> list<y>; x = source elem type, y = Term. (#476)
        (MetaTerms.tyapps (DeepCore.primitiveEncoded DefLists.map)
          [encoderFullResultType @@ var "elemType", Core.typeVariable (Core.nameLift _Term)]
          @@@ (encodeType @@ var "elemType") @@@ DeepCore.var "xs"))

-- | Generate an encoder for a map type
-- Encodes each key/value pair and wraps in Term.map

-- | Generate an encoder for a literal type
-- For literals, the input is a native Haskell value (e.g., String, Int32).
-- We need to wrap it in encoded TermLiteral with the appropriate Literal constructor.
encodeLiteralType :: TypedTermDefinition (LiteralType -> Term)
encodeLiteralType = define "encodeLiteralType" $
  doc "Generate an encoder for a literal type" $
  -- The literal encoder's input is the native literal value; its domain is the
  -- corresponding literal type (e.g. binary -> literal<binary>, string -> literal<string>,
  -- integer<intType> -> literal<integer<intType>>). (#476)
  match _LiteralType (Just identityEncoder) [
    _LiteralType_binary>>: constant $
      litLam (Core.literalTypeBinary) $ \x -> termLiteral $ DeepCore.injection _Literal (DeepCore.field _Literal_binary x),
    _LiteralType_boolean>>: constant $
      litLam (Core.literalTypeBoolean) $ \x -> termLiteral $ DeepCore.injection _Literal (DeepCore.field _Literal_boolean x),
    _LiteralType_decimal>>: constant $
      litLam (Core.literalTypeDecimal) $ \x -> termLiteral $ DeepCore.injection _Literal (DeepCore.field _Literal_decimal x),
    _LiteralType_string>>: constant $
      litLam (Core.literalTypeString) $ \x -> termLiteral $ DeepCore.injection _Literal (DeepCore.field _Literal_string x),
    -- For integer types, wrap in Term.literal.integer with the specific integer variant
    _LiteralType_integer>>: "intType" ~>
      litLam (Core.literalTypeInteger (var "intType")) $ \x -> termLiteral $ DeepCore.injection _Literal
        (DeepCore.field _Literal_integer (encodeIntegerValue @@ var "intType" @@ x)),
    -- For float types, wrap in Term.literal.float with the specific float variant
    _LiteralType_float>>: "floatType" ~>
      litLam (Core.literalTypeFloat (var "floatType")) $ \x -> termLiteral $ DeepCore.injection _Literal
        (DeepCore.field _Literal_float (encodeFloatValue @@ var "floatType" @@ x))]
  where
    -- Build a typed lambda over the native literal value: domain = literal<lt>.
    litLam lt mk = MetaTerms.lambdaTyped "x" (Core.typeLiteral lt) (mk (DeepCore.var "x"))
    -- Helper to wrap a Literal value in Term.literal
    termLiteral lit = DeepCore.injection _Term (DeepCore.field _Term_literal lit)
    -- Default: identity (should not be reached for well-formed types)
    identityEncoder = DeepCore.lambda "x" $ DeepCore.var "x"

-- | Transform a type module into an encoder module
-- Returns Nothing if the module has no encodable type definitions
-- | Generate an encoder for a map type
-- Encodes each key/value pair and wraps in Term.map
encodeMapType :: TypedTermDefinition (MapType -> Term)
encodeMapType = define "encodeMapType" $
  doc "Generate an encoder for a map type" $
  "mt" ~>
    -- maps.bimap : forall k1,k2,v1,v2. (k1->k2)->(v1->v2)->map<k1,v1>->map<k2,v2>;
    -- k1=key, v1=val, k2=v2=Term (note forall order k1,k2,v1,v2). (#476)
    MetaTerms.lambdaTyped "m" (Core.typeMap $ Core.mapType
        (encoderFullResultType @@ Core.mapTypeKeys (var "mt"))
        (encoderFullResultType @@ Core.mapTypeValues (var "mt"))) $
      DeepCore.injection _Term (DeepCore.field _Term_map
        (MetaTerms.tyapps (DeepCore.primitiveEncoded DefMaps.bimap)
          [encoderFullResultType @@ Core.mapTypeKeys (var "mt"),
           Core.typeVariable (Core.nameLift _Term),
           encoderFullResultType @@ Core.mapTypeValues (var "mt"),
           Core.typeVariable (Core.nameLift _Term)]
          @@@ (encodeType @@ Core.mapTypeKeys (var "mt"))
          @@@ (encodeType @@ Core.mapTypeValues (var "mt"))
          @@@ DeepCore.var "m"))

-- | Generate an encoder for an Either type
-- Generates a case match over Left/Right variants
-- | Transform a type module into an encoder module
-- Returns Nothing if the module has no encodable type definitions
encodeModule :: TypedTermDefinition (InferenceContext -> Graph -> Module -> Prelude.Either Error (Maybe Module))
encodeModule = define "encodeModule" $
  doc "Transform a type module into an encoder module" $
  "cx" ~> "graph" ~> "mod" ~>
    "typeBindings" <<~ (filterTypeBindings @@ var "cx" @@ var "graph" @@
      (Optionals.cat $ Lists.map
        ("d" ~> cases _Definition (var "d") (Just nothing) [
          _Definition_type>>: "td" ~>
            just (Annotations.typeBinding @@ (Packaging.typeDefinitionName $ var "td") @@ (Core.typeSchemeBody $ Packaging.typeDefinitionBody $ var "td"))])
        (Packaging.moduleDefinitions (var "mod")))) $
    Logic.ifElse (Lists.null (var "typeBindings"))
      (right nothing)
      ("encodedBindings" <<~ Eithers.mapList ("b" ~>
        Eithers.bimap
          ("_e" ~> Error.errorDecoding $ var "_e")
          ("x" ~> var "x")
          (encodeBinding @@ var "cx" @@ var "graph" @@ var "b")) (var "typeBindings") $
        -- The encoder module depends on encoder modules of the source's dependencies, plus the original module
        right (just (Packaging.module_
          (encodeModuleName @@ (Packaging.moduleName (var "mod")))
          (just (Packaging.entityMetadata
            (just (Strings.cat $ list [
              string "Term encoders for ",
              Packaging.unModuleName (Packaging.moduleName (var "mod"))]))
            (list ([] :: [TypedTerm String])) (list ([] :: [TypedTerm EntityReference])) nothing))
          (Lists.map ("ns" ~> Packaging.moduleDependency (var "ns") nothing) (Lists.nub (Lists.concat2
            (primitive DefLists.map @@ encodeModuleName @@ (Lists.map ("dep" ~> Packaging.moduleDependencyModule (var "dep")) (Packaging.moduleDependencies (var "mod"))))
            (list [Packaging.moduleName (var "mod")]))))
          (Lists.map ("b" ~> Packaging.definitionTerm (Packaging.termDefinition
            (Core.bindingName $ var "b") nothing
            (Optionals.map Scoping.typeSchemeToTermSignature $ Core.bindingTypeScheme $ var "b")
            (Core.bindingTerm $ var "b")))
            (var "encodedBindings")))))

-- | Encode a Name as a Term (produces a wrapped term of type hydra.core.Name)
-- | Encode a Name as a Term (produces a wrapped term of type hydra.core.Name)
encodeName :: TypedTermDefinition (Name -> Term)
encodeName = define "encodeName" $
  doc "Encode a Name as a term" $
  "n" ~> DeepCore.wrap _Name (DeepCore.string (Core.unName (var "n")))

-- | Generate an encoder module name from a source module name
-- For example, "hydra.util" -> "hydra.encode.util"
encodeModuleName :: TypedTermDefinition (ModuleName -> ModuleName)
encodeModuleName = define "encodeModuleName" $
  doc "Generate an encoder module name from a source module name" $
  "ns" ~>
  "parts" <~ Strings.splitOn (string ".") (Packaging.unModuleName (var "ns")) $
  "fallback" <~ Packaging.moduleName2 (Packaging.unModuleName (var "ns")) $
  -- Drop the first segment (e.g. "hydra") and prepend "hydra.encode".
  Optionals.cases (Lists.uncons $ var "parts") (var "fallback") ("uc" ~>
      Packaging.moduleName2 (
        Strings.cat $ list [
          string "hydra.encode.",
          Strings.intercalate (string ".") (Pairs.second $ var "uc")]))

-- | Generate an encoder for a record type
-- For records, project each field, encode it, and build an encoded record
-- | Generate an encoder for a Maybe type
-- Encodes the inner value if present and wraps in Term.maybe
encodeOptionalType :: TypedTermDefinition (Type -> Term)
encodeOptionalType = define "encodeOptionalType" $
  doc "Generate an encoder for a Maybe type" $
  -- optionals.map : forall x,y. (x->y)->optional<x>->optional<y>; x=elem, y=Term. (#476)
  "elemType" ~> MetaTerms.lambdaTyped "opt" (Core.typeOptional (encoderFullResultType @@ var "elemType")) $
    DeepCore.injection _Term (DeepCore.field _Term_optional
      (MetaTerms.tyapps (DeepCore.primitiveEncoded DefOptionals.map)
        [encoderFullResultType @@ var "elemType", Core.typeVariable (Core.nameLift _Term)]
        @@@ (encodeType @@ var "elemType") @@@ DeepCore.var "opt"))

-- | Generate an encoder for a pair type
-- Encodes both elements and wraps in Term.pair
-- | Generate an encoder for a pair type
-- Encodes both elements and wraps in Term.pair
encodePairType :: TypedTermDefinition (PairType -> Term)
encodePairType = define "encodePairType" $
  doc "Generate an encoder for a pair type" $
  -- pairs.bimap : forall a,b,c,d. (a->c)->(b->d)->(a,b)->(c,d); a=first, b=second, c=d=Term. (#476)
  "pt" ~> MetaTerms.lambdaTyped "p" (Core.typePair $ Core.pairType
      (encoderFullResultType @@ Core.pairTypeFirst (var "pt"))
      (encoderFullResultType @@ Core.pairTypeSecond (var "pt"))) $
    DeepCore.injection _Term $ DeepCore.field _Term_pair $ MetaTerms.tyapps (DeepCore.primitiveEncoded DefPairs.bimap)
      [encoderFullResultType @@ Core.pairTypeFirst (var "pt"),
       encoderFullResultType @@ Core.pairTypeSecond (var "pt"),
       Core.typeVariable (Core.nameLift _Term),
       Core.typeVariable (Core.nameLift _Term)]
    @@@ (encodeType @@ Core.pairTypeFirst (var "pt"))
    @@@ (encodeType @@ Core.pairTypeSecond (var "pt"))
    @@@ DeepCore.var "p"

-- | Generate an encoder for a set type
-- Encodes each element and wraps in Term.set
encodeRecordType :: TypedTermDefinition ([FieldType] -> Term)
encodeRecordType = define "encodeRecordType" $
  doc "Generate an encoder for a record type (unnamed — should not be called directly)" $
  "rt" ~> encodeRecordTypeNamed @@ Core.name (string "unknown") @@ var "rt"

-- | Generate an encoder term for a given Type, using the element name for record/union/wrap
-- | Generate an encoder for a record type
-- For records, project each field, encode it, and build an encoded record
encodeRecordTypeNamed :: TypedTermDefinition (Name -> [FieldType] -> Term)
encodeRecordTypeNamed = define "encodeRecordTypeNamed" $
  doc "Generate an encoder for a record type with the given element name" $
  "ename" ~> "rt" ~>
    -- The record encoder takes a value of the nominal record type. (#476)
    MetaTerms.lambdaTyped "x" (Core.typeVariable (var "ename")) $
      DeepCore.injection _Term (DeepCore.field _Term_record
        (DeepCore.record _Record [
          DeepCore.field _Record_typeName (encodeName @@ var "ename"),
          DeepCore.field _Record_fields
            (DeepCore.list (primitive DefLists.map @@ (encodeRecordFieldNamed @@ var "ename" @@ var "rt") @@ var "rt"))]))
  where
    encodeRecordFieldNamed :: TypedTerm (Name -> [FieldType] -> FieldType -> Term)
    encodeRecordFieldNamed =
      "tname" ~> "recType" ~> "ft" ~>
        DeepCore.record _Field [
          DeepCore.field _Field_name (encodeName @@ Core.fieldTypeName (var "ft")),
          DeepCore.field _Field_term
            ((encodeType @@ Core.fieldTypeType (var "ft"))
              @@@ (projectField (var "tname") (Core.fieldTypeName (var "ft"))
                    @@@ DeepCore.var "x"))]

    projectField typeName fieldName =
      Core.termProject $ Core.projection typeName fieldName
-- | Generate an encoder for a set type
-- Encodes each element and wraps in Term.set
encodeSetType :: TypedTermDefinition (Type -> Term)
encodeSetType = define "encodeSetType" $
  doc "Generate an encoder for a set type" $
  -- sets.map : forall x,y. (x->y)->set<x>->set<y>; x=elem, y=Term. (#476)
  "elemType" ~> MetaTerms.lambdaTyped "s" (Core.typeSet (encoderFullResultType @@ var "elemType")) $
    DeepCore.injection _Term (DeepCore.field _Term_set
      (MetaTerms.tyapps (DeepCore.primitiveEncoded DefSets.map)
        [encoderFullResultType @@ var "elemType", Core.typeVariable (Core.nameLift _Term)]
        @@@ (encodeType @@ var "elemType") @@@ DeepCore.var "s"))

-- | Generate an encoder term for a given Type (without element name context)
encodeType :: TypedTermDefinition (Type -> Term)
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
    _Type_optional>>: "elemType" ~>
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
      MetaTerms.lambdaTyped "_" Core.typeUnit $ DeepCore.injection _Term (DeepCore.field _Term_unit DeepCore.unit),
    _Type_void>>: constant $
      MetaTerms.lambdaTyped "_" Core.typeUnit $ DeepCore.injection _Term (DeepCore.field _Term_unit DeepCore.unit),
    _Type_variable>>: "typeName" ~>
      Core.termVariable (encodeBindingName @@ var "typeName")]
  where
    identityEncoder = DeepCore.lambda "x" $ DeepCore.var "x"

-- | Generate an encoder for a union type with a given element name
-- | Generate an encoder term for a given Type, using the element name for record/union/wrap
encodeTypeNamed :: TypedTermDefinition (Name -> Type -> Term)
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
      Core.termLambda $ Core.lambda (encodeBindingName @@ Core.forallTypeParameter (var "ft")) nothing
          (encodeTypeNamed @@ var "ename" @@ Core.forallTypeBody (var "ft")),
    _Type_function>>: constant identityEncoder,
    _Type_list>>: "elemType" ~>
      encodeListType @@ var "elemType",
    _Type_literal>>: "lt" ~>
      encodeLiteralType @@ var "lt",
    _Type_map>>: "mt" ~>
      encodeMapType @@ var "mt",
    _Type_optional>>: "elemType" ~>
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
      MetaTerms.lambdaTyped "_" Core.typeUnit $ DeepCore.injection _Term (DeepCore.field _Term_unit DeepCore.unit),
    _Type_void>>: constant $
      MetaTerms.lambdaTyped "_" Core.typeUnit $ DeepCore.injection _Term (DeepCore.field _Term_unit DeepCore.unit),
    _Type_variable>>: "typeName" ~>
      Core.termVariable (encodeBindingName @@ var "typeName")]
  where
    identityEncoder = DeepCore.lambda "x" $ DeepCore.var "x"

-- | Generate an encoder term for a given Type (without element name context)
-- | Generate an encoder for a union type (placeholder name)
encodeUnionType :: TypedTermDefinition ([FieldType] -> Term)
encodeUnionType = define "encodeUnionType" $
  doc "Generate an encoder for a union type (placeholder name)" $
  "rt" ~> encodeUnionTypeNamed @@ Core.name (string "unknown") @@ var "rt"

-- | Generate an encoder for a wrapped type with a given element name
-- | Generate an encoder for a union type with a given element name
encodeUnionTypeNamed :: TypedTermDefinition (Name -> [FieldType] -> Term)
encodeUnionTypeNamed = define "encodeUnionTypeNamed" $
  doc "Generate an encoder for a union type with the given element name" $
  "ename" ~> "rt" ~>
    Core.termCases $ Core.caseStatement
        (var "ename")
        nothing
        (primitive DefLists.map @@
          ("ft" ~> Core.caseAlternative
            (Core.fieldTypeName (var "ft"))
            (encodeFieldValue
              @@ var "ename"
              @@ Core.fieldTypeName (var "ft")
              @@ Core.fieldTypeType (var "ft")))
          @@ var "rt")

-- | Generate an encoder for a union type (placeholder name)
-- | Generate an encoder for a wrapped type (placeholder name)
encodeWrappedType :: TypedTermDefinition (Type -> Term)
encodeWrappedType = define "encodeWrappedType" $
  doc "Generate an encoder for a wrapped type (placeholder name)" $
  "wt" ~> encodeWrappedTypeNamed @@ Core.name (string "unknown") @@ var "wt"

-- | Filter bindings to only encodable type definitions
-- A binding is encodable if it is a native type AND is serializable (no function types in dependencies)
-- | Generate an encoder for a wrapped type with a given element name
encodeWrappedTypeNamed :: TypedTermDefinition (Name -> Type -> Term)
encodeWrappedTypeNamed = define "encodeWrappedTypeNamed" $
  doc "Generate an encoder for a wrapped type with the given element name" $
  "ename" ~> "wt" ~>
    -- The wrapper encoder takes a value of the nominal wrapper type. (#476)
    MetaTerms.lambdaTyped "x" (Core.typeVariable (var "ename")) $
      DeepCore.injection _Term (DeepCore.field _Term_wrap
        (DeepCore.record _WrappedTerm [
          DeepCore.field _WrappedTerm_typeName (encodeName @@ var "ename"),
          DeepCore.field _WrappedTerm_body
            ((encodeType @@ var "wt")
              @@@ (DeepCore.unwrapDynamic (var "ename") @@@ DeepCore.var "x"))]))

-- | Generate an encoder for a wrapped type (placeholder name)
-- | Collect forall type variables from a type
encoderCollectForallVariables :: TypedTermDefinition (Type -> [Name])
encoderCollectForallVariables = define "encoderCollectForallVariables" $
  doc "Collect forall type variable names from a type" $
  "typ" ~>
  cases _Type (var "typ") (Just $ list ([] :: [TypedTerm Name])) [
    _Type_annotated>>: "at" ~>
      encoderCollectForallVariables @@ (Core.annotatedTypeBody (var "at")),
    _Type_forall>>: "ft" ~>
      Lists.cons (Core.forallTypeParameter (var "ft"))
        (encoderCollectForallVariables @@ Core.forallTypeBody (var "ft"))]

-- | Collect type variables that need Ord constraints (from Map key and Set element positions)
-- | Collect type variables that need Ord constraints (from Map key and Set element positions)
encoderCollectOrdVars :: TypedTermDefinition (Type -> [Name])
encoderCollectOrdVars = define "encoderCollectOrdVars" $
  doc "Collect type variables needing Ord constraints" $
  "typ" ~>
  cases _Type (var "typ") (Just $ list ([] :: [TypedTerm Name])) [
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
    _Type_optional>>: "elemType" ~>
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
-- | Collect all type variables from a type expression
encoderCollectTypeVarsFromType :: TypedTermDefinition (Type -> [Name])
encoderCollectTypeVarsFromType = define "encoderCollectTypeVarsFromType" $
  doc "Collect all type variable names from a type expression" $
  "typ" ~>
  cases _Type (var "typ") (Just $ list ([] :: [TypedTerm Name])) [
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
    _Type_optional>>: "elemType" ~>
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
-- | Get the full result type for an encoder (the input type of the encoder function)
-- Maps structural types to their nominal names, preserving type applications.
encoderFullResultType :: TypedTermDefinition (Type -> Type)
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
    -- A literal type's encoder input is that specific literal type (e.g. literal<string>),
    -- NOT the generic Literal — inference preserves the variant. (#476)
    _Type_literal>>: "lt" ~>
      Core.typeLiteral (var "lt"),
    _Type_map>>: "mt" ~>
      Core.typeMap $ Core.mapType
        (encoderFullResultType @@ Core.mapTypeKeys (var "mt"))
        (encoderFullResultType @@ Core.mapTypeValues (var "mt")),
    _Type_optional>>: "elemType" ~>
      Core.typeOptional (encoderFullResultType @@ var "elemType"),
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
    -- A wrapper (newtype) is value-transparent: a wrapper nested inside a
    -- container (e.g. list<Vertex>) is encoded from its unwrapped body, matching
    -- what inference resolves. TypeWrap carries a bare Type, so `wt` IS the
    -- body. Mirrors the _Type_list arm. (#476)
    _Type_wrap>>: "wt" ~>
      encoderFullResultType @@ var "wt"]

-- | Get full result type for encoder input, with element name for nominal types
-- | Get full result type for encoder input, with element name for nominal types
encoderFullResultTypeNamed :: TypedTermDefinition (Name -> Type -> Type)
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
    -- specific literal type (e.g. literal<string>), not generic Literal. (#476)
    _Type_literal>>: "lt" ~>
      Core.typeLiteral (var "lt"),
    _Type_map>>: "mt" ~>
      Core.typeMap $ Core.mapType
        (encoderFullResultType @@ Core.mapTypeKeys (var "mt"))
        (encoderFullResultType @@ Core.mapTypeValues (var "mt")),
    _Type_optional>>: "elemType" ~>
      Core.typeOptional (encoderFullResultType @@ var "elemType"),
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

-- | Build the encoder function type for a given type
-- For monomorphic types: InputType -> Term
-- For polymorphic types: (a -> Term) -> ... -> InputType<a,...> -> Term
encoderType :: TypedTermDefinition (Type -> Type)
encoderType = define "encoderType" $
  doc "Build encoder function type" $
  "typ" ~>
  "resultType" <~ (encoderFullResultType @@ var "typ") $
  "baseType" <~ (Core.typeFunction $ Core.functionType
    (var "resultType")
    (Core.typeVariable (Core.nameLift _Term))) $
  prependForallEncoders @@ var "baseType" @@ var "typ"

-- | Prepend encoder types for forall parameters
-- For forall a. T: prepends (a -> Term) -> to the base type
-- | Build the encoder function type for a given type
-- For monomorphic types: InputType -> Term
-- For polymorphic types: (a -> Term) -> ... -> InputType<a,...> -> Term
encoderTypeNamed :: TypedTermDefinition (Name -> Type -> Type)
encoderTypeNamed = define "encoderTypeNamed" $
  doc "Build encoder function type with element name for nominal types" $
  "ename" ~> "typ" ~>
  "resultType" <~ (encoderFullResultTypeNamed @@ var "ename" @@ var "typ") $
  "baseType" <~ (Core.typeFunction $ Core.functionType
    (var "resultType")
    (Core.typeVariable (Core.nameLift _Term))) $
  prependForallEncoders @@ var "baseType" @@ var "typ"
encoderTypeScheme :: TypedTermDefinition (Type -> TypeScheme)
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
      ("v" ~> Lists.elem (var "v" :: TypedTerm Name) (var "typeVars" :: TypedTerm [Name]))
      (var "allOrdVars"),

    -- Build constraints map: {varName -> TypeVariableConstraints {classes = {ordering}}}
    "constraints">:
      Logic.ifElse (Lists.null (var "ordVars"))
        nothing
        (just $ Maps.fromList $ Lists.map
          ("v" ~> pair (var "v") (Core.typeVariableConstraints $ list [Core.typeClassConstraintSimple $ Core.name (string "ordering")]))
          (var "ordVars"))] $
  Core.typeScheme (var "typeVars") (var "encoderFunType") (var "constraints")

-- | Collect forall type variables from a type
-- | Construct a TypeScheme for an encoder function from a source type definition.
-- For a type like @forall v. Graph v@ (where Graph has Map v (Vertex v)),
-- produces the scheme @forall v. (v -> Term) -> Graph v -> Term@ with Ord constraint on v.
--
-- The type scheme uses the same variable names as the original type definition.
-- Variable normalization (to t0, t1, etc.) happens later in the pipeline and
-- handles renaming both variables and constraint keys consistently.
encoderTypeSchemeNamed :: TypedTermDefinition (Name -> Type -> TypeScheme)
encoderTypeSchemeNamed = define "encoderTypeSchemeNamed" $
  doc "Construct a TypeScheme for an encoder function, with element name for nominal types" $
  "ename" ~> "typ" ~> lets [
    "typeVars">: encoderCollectForallVariables @@ var "typ",
    "encoderFunType">: encoderTypeNamed @@ var "ename" @@ var "typ",
    "allOrdVars">: encoderCollectOrdVars @@ var "typ",
    "ordVars">: Lists.filter
      ("v" ~> Lists.elem (var "v" :: TypedTerm Name) (var "typeVars" :: TypedTerm [Name]))
      (var "allOrdVars"),
    "constraints">:
      Logic.ifElse (Lists.null (var "ordVars"))
        nothing
        (just $ Maps.fromList $ Lists.map
          ("v" ~> pair (var "v") (Core.typeVariableConstraints $ list [Core.typeClassConstraintSimple $ Core.name (string "ordering")]))
          (var "ordVars"))] $
  Core.typeScheme (var "typeVars") (var "encoderFunType") (var "constraints")
-- | Filter bindings to only encodable type definitions
-- A binding is encodable if it is a native type AND is serializable (no function types in dependencies)
filterTypeBindings :: TypedTermDefinition (InferenceContext -> Graph -> [Binding] -> Prelude.Either Error [Binding])
filterTypeBindings = define "filterTypeBindings" $
  doc "Filter bindings to only encodable type definitions" $
  "cx" ~> "graph" ~> "bindings" ~>
    -- First filter to native types, then check serializability for each
    Eithers.map (primitive DefOptionals.cat) $
      Eithers.mapList (isEncodableBinding @@ var "cx" @@ var "graph") $
        primitive DefLists.filter @@ Annotations.isNativeType @@ var "bindings"

-- | Format a DecodingError as a string
formatDecodingError :: TypedTerm (DecodingError -> String)
formatDecodingError = "e" ~> unwrap _DecodingError @@ var "e"

-- | Check if a binding is encodable and return Just binding if so, Nothing otherwise
-- | Check if a binding is encodable and return Just binding if so, Nothing otherwise
isEncodableBinding :: TypedTermDefinition (InferenceContext -> Graph -> Binding -> Prelude.Either Error (Maybe Binding))
isEncodableBinding = define "isEncodableBinding" $
  doc "Check if a binding is encodable (serializable type)" $
  "cx" ~> "graph" ~> "b" ~>
    "serializable" <<~ Predicates.isSerializableByName @@ var "cx" @@ var "graph" @@ (Core.bindingName (var "b")) $
    right (Logic.ifElse (var "serializable") (just (var "b")) nothing)

-- | Check whether a type is the unit type
-- | Check whether a type is the unit type
isUnitType_ :: TypedTermDefinition (Type -> Bool)
isUnitType_ = define "isUnitType" $
  doc "Check whether a type is the unit type" $
  match _Type (Just $ false) [
    _Type_unit>>: constant true]

-- | Encode an integer value based on its integer type
-- Wraps the value in the appropriate IntegerValue variant as an injection
-- | Prepend encoder types for forall parameters
-- For forall a. T: prepends (a -> Term) -> to the base type
prependForallEncoders :: TypedTermDefinition (Type -> Type -> Type)
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

-- | Generate a fully qualified binding name for an encoder function from a type name
-- For example, "hydra.core.Name" -> "hydra.encode.core.name"
-- For local types (no namespace), returns just the decapitalized local name
