{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hydra.Sources.Kernel.Terms.Decoding where

-- Standard imports for kernel terms modules
import Hydra.Kernel hiding (literalType, matchRecord, matchUnion)
import qualified Hydra.Dsl.Paths    as Paths
import qualified Hydra.Overlay.Haskell.Dsl.Annotations       as Annotations
import qualified Hydra.Dsl.Ast          as Ast
import qualified Hydra.Overlay.Haskell.Bootstrap         as Bootstrap
import qualified Hydra.Dsl.Coders       as Coders
import qualified Hydra.Dsl.Util      as Util
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Core         as Core
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Graph        as Graph
import qualified Hydra.Dsl.Json.Model         as Json
import qualified Hydra.Dsl.Lib.Chars    as Chars
import qualified Hydra.Dsl.Lib.Eithers  as Eithers
import qualified Hydra.Dsl.Lib.Equality as Equality
import qualified Hydra.Dsl.Lib.Lists    as Lists
import qualified Hydra.Dsl.Lib.Literals as Literals
import qualified Hydra.Dsl.Lib.Logic    as Logic
import qualified Hydra.Dsl.Lib.Maps     as Maps
import qualified Hydra.Dsl.Lib.Math     as Math
import qualified Hydra.Dsl.Lib.Optionals   as Optionals
import qualified Hydra.Dsl.Lib.Pairs    as Pairs
import qualified Hydra.Dsl.Lib.Sets     as Sets
import qualified Hydra.Dsl.Lib.Strings  as Strings
import qualified Hydra.Overlay.Haskell.Dsl.Literals          as Literals
import qualified Hydra.Overlay.Haskell.Dsl.LiteralTypes      as LiteralTypes
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Base         as MetaBase
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Terms        as MetaTerms
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Types        as MetaTypes
import qualified Hydra.Dsl.Packaging       as Packaging
import qualified Hydra.Dsl.Parsing      as Parsing
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Phantoms     as Phantoms
import           Hydra.Overlay.Haskell.Dsl.Typed.Phantoms     as Phantoms hiding (
  elimination, field, fieldType, floatType, floatValue, function, injection, integerType, integerValue, literal,
  literalType, record, term, type_, typeScheme)
import qualified Hydra.Overlay.Haskell.Dsl.Prims             as Prims
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Tabular           as Tabular
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Testing      as Testing
import qualified Hydra.Overlay.Haskell.Dsl.Terms             as Terms
import qualified Hydra.Overlay.Haskell.Dsl.Tests             as Tests
import qualified Hydra.Dsl.Topology     as Topology
import qualified Hydra.Overlay.Haskell.Dsl.Types             as Types
import qualified Hydra.Dsl.Typing       as Typing
import qualified Hydra.Dsl.Errors       as Error
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Variants     as Variants
import           Hydra.Sources.Kernel.Types.All
import qualified Hydra.Sources.Kernel.Terms.Annotations as Annotations
import qualified Hydra.Sources.Kernel.Terms.Formatting as Formatting
import qualified Hydra.Sources.Kernel.Terms.Extract.Core as ExtractCore
import qualified Hydra.Sources.Kernel.Terms.Lexical as Lexical

import qualified Hydra.Sources.Kernel.Terms.Names as Names
import qualified Hydra.Sources.Kernel.Terms.Rewriting as Rewriting
import qualified Hydra.Sources.Kernel.Terms.Constants as Constants
import qualified Hydra.Sources.Kernel.Terms.Predicates as Predicates
import qualified Hydra.Sources.Kernel.Terms.Scoping as Scoping
import qualified Hydra.Sources.Kernel.Terms.Show.Core as ShowCore
import qualified Hydra.Sources.Kernel.Terms.Show.Errors as ShowError
import           Prelude hiding ((++))
import qualified Data.Int                    as I
import qualified Data.List                   as L
import qualified Data.Map                    as M
import qualified Data.Set                    as S
import qualified Data.Maybe                  as Y

import qualified Hydra.Overlay.Haskell.Dsl.Typed.DeepCore as DeepCore
import           Hydra.Overlay.Haskell.Dsl.Typed.DeepCore ((@@@))
import qualified Hydra.Lib.Eithers as DefEithers
import qualified Hydra.Lib.Lists as DefLists
import qualified Hydra.Lib.Maps as DefMaps
import qualified Hydra.Lib.Optionals as DefOptionals
import qualified Hydra.Lib.Strings as DefStrings


ns :: ModuleName
ns = ModuleName "hydra.decoding"

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = definitions,
            moduleDependencies = Bootstrap.unqualifiedDep <$> ([Annotations.ns, ExtractCore.ns, Formatting.ns, Lexical.ns, Names.ns, Predicates.ns, Rewriting.ns, ShowCore.ns] L.++ kernelTypesModuleNames),
            moduleMetadata = Bootstrap.descriptionMetadata (Just "Functions for generating term decoders from type modules")}
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
      toDefinition decodeModuleName,
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

define :: String -> TypedTerm x -> TypedTermDefinition x
define = definitionInModule module_

-- | Format a DecodingError as a string
formatDecodingError :: TypedTerm (DecodingError -> String)
formatDecodingError = "e" ~> unwrap _DecodingError @@ var "e"

--------------------------------------------------------------------------------
-- Helper functions
--------------------------------------------------------------------------------

-- | Build a decoder term that takes cx and raw, strips annotations/dereferences variables, and matches on it
-- The result is: \cx -> \raw -> either (\err -> Left (DecodingError err)) (\stripped -> case stripped of ...) (stripAndDereferenceTermEither cx raw)
-- Note: We use the original inline style - the Python coder should handle this pattern by recognizing
-- when a lambda body is a case application and generating a proper function with match statement.
-- | The first argument is the decoder's RESULT type (the X in
-- @Either DecodingError X@), used to instantiate the polymorphic eithers.either. (#476)
deannotateAndMatch :: TypedTerm Type -> TypedTerm (Maybe Term) -> [TypedTerm CaseAlternative] -> TypedTerm Term
deannotateAndMatch resultType dflt cses =
  -- Domains are known by construction: cx : hydra.graph.Graph, raw : Term,
  -- err : DecodingError (Left branch), stripped : Term (Right branch). (#476)
  dlam "cx" graphType $ dlam "raw" termType $
  -- eithers.either : forall x,y,z. (x->z)->(y->z)->either<x,y>->z;
  -- x=DecodingError, y=Term, z=either<DecodingError, resultType>. (#476)
  MetaTerms.tyapps (DeepCore.primitive DefEithers.either)
    [decodingErrorType, termType,
     Core.typeEither $ Core.eitherType decodingErrorType resultType]
    -- If Left (decoding error), propagate it. Left : either<DErr, resultType>. (#476)
    @@@ (dlam "err" decodingErrorType $ leftT resultType $ DeepCore.var "err")
    -- If Right (stripped term), do the case match
    @@@ (dlam "stripped" termType $ DeepCore.cases _Term (DeepCore.var "stripped") dflt cses)
    -- Call stripWithDecodingError cx raw (returns Either DecodingError Term)
    @@@ (DeepCore.ref ExtractCore.stripWithDecodingError @@@ DeepCore.var "cx" @@@ DeepCore.var "raw")
  where
    -- Build a real TermLambda with an explicit domain via DeepCore's family. (#476)
    dlam v ty body = DeepCore.lambdaTyped (Core.name (string v)) (just ty) body
    graphType         = Core.typeVariable (Core.name (string "hydra.graph.Graph"))
    termType          = Core.typeVariable (Core.nameLift _Term)
    decodingErrorType = Core.typeVariable (Core.nameLift _DecodingError)

-- | Helper to create a decoding error term from a message (object-level)
-- Returns: Term.wrap (WrappedTerm "hydra.util.DecodingError" (Term.literal (Literal.string msg)))
decodingErrorTerm :: TypedTerm String -> TypedTerm Term
decodingErrorTerm msg = DeepCore.wrap _DecodingError $ DeepCore.string msg

-- | Helper to create a Left (error) result term, type-applied to either<DErr, R>.
-- R is the decoder's result type (the Right side); every TermEither injection carries
-- [leftType=DecodingError, rightType=R] so the synthesized term needs no inference. (#476)
leftError :: TypedTerm Type -> TypedTerm String -> TypedTerm Term
leftError resultType msg = leftT resultType $ decodingErrorTerm msg

-- | Type-applied Left : either<DecodingError, R>. (#476)
leftT :: TypedTerm Type -> TypedTerm Term -> TypedTerm Term
leftT resultType t = MetaTerms.tyapps (DeepCore.left t)
  [Core.typeVariable (Core.nameLift _DecodingError), resultType]

-- | Type-applied Right : either<DecodingError, R>. (#476)
rightT :: TypedTerm Type -> TypedTerm Term -> TypedTerm Term
rightT resultType t = MetaTerms.tyapps (DeepCore.right t)
  [Core.typeVariable (Core.nameLift _DecodingError), resultType]

-- | A let expression whose bindings carry explicit (monomorphic) type schemes,
-- matching what inference stamps. Each binding is (name, type, value). (#476)
letsT :: [(String, TypedTerm Type, TypedTerm Term)] -> TypedTerm Term -> TypedTerm Term
letsT bindings body = Core.termLet $ Core.let_
  (Phantoms.list [ Core.binding (Core.name (string n)) v
                     (just (Core.typeScheme (Phantoms.list ([] :: [TypedTerm Name])) ty nothing))
                 | (n, ty, v) <- bindings ])
  body

-- | A type-applied pair : pair<A, B>. (#476)
pairT :: TypedTerm Type -> TypedTerm Type -> TypedTerm Term -> TypedTerm Term -> TypedTerm Term
pairT aType bType a b = MetaTerms.tyapps (DeepCore.pair a b) [aType, bType]

-- | Helper to strip and dereference with DecodingError — delegates to the module-level definition
stripWithDecodingError :: TypedTerm Graph -> TypedTerm Term -> TypedTerm (Either DecodingError Term)
stripWithDecodingError g term = ExtractCore.stripWithDecodingError @@ g @@ term

--------------------------------------------------------------------------------
-- Main decoder functions
--------------------------------------------------------------------------------

-- | Collect just the forall type variables from a type
-- Note: Uses 'cases' instead of 'match' to avoid variable shadowing from eta expansion
collectForallVariables :: TypedTermDefinition (Type -> [Name])
collectForallVariables = define "collectForallVariables" $
  doc "Collect forall type variable names from a type" $
  "typ" ~>
  cases _Type (var "typ") (Just $ list ([] :: [TypedTerm Name])) [
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
collectOrdConstrainedVariables :: TypedTermDefinition (Type -> [Name])
collectOrdConstrainedVariables = define "collectOrdConstrainedVariables" $
  doc "Collect type variables needing Ord constraints (from Map key and Set element types)" $
  "typ" ~>
  cases _Type (var "typ") (Just $ list ([] :: [TypedTerm Name])) [
    _Type_annotated>>: "at" ~>
      collectOrdConstrainedVariables @@ (Core.annotatedTypeBody (var "at")),
    _Type_application>>: "appType" ~>
      Lists.concat2
        (collectOrdConstrainedVariables @@ Core.applicationTypeFunction (var "appType"))
        (collectOrdConstrainedVariables @@ Core.applicationTypeArgument (var "appType")),
    _Type_effect>>: "elemType" ~>
      collectOrdConstrainedVariables @@ var "elemType",
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
    _Type_optional>>: "elemType" ~>
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
collectTypeVariables :: TypedTermDefinition (Type -> [Name])
collectTypeVariables = define "collectTypeVariables" $
  doc "Collect type variable names from a type (forall parameters only)" $
  "typ" ~> collectForallVariables @@ var "typ"

-- | Collect just the forall type variables from a type
-- Note: Uses 'cases' instead of 'match' to avoid variable shadowing from eta expansion
-- | Collect all type variables from a type expression (for use in Set element types)
-- Note: Uses 'cases' instead of 'match' to avoid variable shadowing from eta expansion
collectTypeVariablesFromType :: TypedTermDefinition (Type -> [Name])
collectTypeVariablesFromType = define "collectTypeVariablesFromType" $
  doc "Collect all type variable names from a type expression" $
  "typ" ~>
  cases _Type (var "typ") (Just $ list ([] :: [TypedTerm Name])) [
    _Type_annotated>>: "at" ~>
      collectTypeVariablesFromType @@ (Core.annotatedTypeBody (var "at")),
    _Type_application>>: "appType" ~>
      Lists.concat2
        (collectTypeVariablesFromType @@ Core.applicationTypeFunction (var "appType"))
        (collectTypeVariablesFromType @@ Core.applicationTypeArgument (var "appType")),
    _Type_effect>>: "elemType" ~>
      collectTypeVariablesFromType @@ var "elemType",
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
    _Type_optional>>: "elemType" ~>
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
decodeBinding :: TypedTermDefinition (InferenceContext -> Graph -> Binding -> Either DecodingError Binding)
decodeBinding = define "decodeBinding" $
  doc "Transform a type binding into a decoder binding" $
  "cx" ~> "graph" ~> "b" ~>
    Eithers.bind (decoderFor _Type @@ var "graph" @@ (Core.bindingTerm (var "b"))) (
      "typ" ~>
      -- The fully-applied result type (e.g. DataRow<v>) for the decoder body's
      -- intermediate annotations; matches the binding signature so Java/Scala
      -- coders emit parameterized (not raw) nominal types. (#476)
      "rtype" <~ (decoderFullResultTypeNamed @@ (Core.bindingName (var "b")) @@ (var "typ")) $
      "rawBody" <~ (decodeTypeNamed @@ (Core.bindingName (var "b")) @@ (var "typ") @@ var "rtype") $
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
decodeBindingName :: TypedTermDefinition (Name -> Name)
decodeBindingName = define "decodeBindingName" $
  doc "Generate a binding name for a decoder function from a type name" $
  Names.derivedBindingName @@ list [string "hydra", string "decode"] @@ boolean True

-- | Generate a decoder for a literal type
-- Match on the LiteralType to generate type-specific decoders
-- Note: Uses 'cases' instead of 'match' to avoid variable shadowing from eta expansion
-- | Generate a decoder for an Either type
decodeEitherType :: TypedTermDefinition (EitherType -> Term)
decodeEitherType = define "decodeEitherType" $
  doc "Generate a decoder for an Either type" $
  "et" ~>
  "leftDecoder" <~ decodeType @@ Core.eitherTypeLeft (var "et") $
  "rightDecoder" <~ decodeType @@ Core.eitherTypeRight (var "et") $
  -- decodeEither : forall t0,t1. decoder<t0> -> decoder<t1> -> decoder<either<t0,t1>>;
  -- instantiate t0,t1 to the decoded left/right types. (#476)
  MetaTerms.tyapps (DeepCore.ref ExtractCore.decodeEither)
    [decoderFullResultType @@ Core.eitherTypeLeft (var "et"),
     decoderFullResultType @@ Core.eitherTypeRight (var "et")]
    @@@ var "leftDecoder" @@@ var "rightDecoder"

-- | Generate a decoder for a list type
-- | Generate a decoder for a polymorphic (forall) type
-- For a type like `forall a. T[a]`, generates a lambda that takes a decoder for `a`
-- and returns a decoder for the body type `T[a]`
decodeForallType :: TypedTermDefinition (ForallType -> Term)
decodeForallType = define "decodeForallType" $
  doc "Generate a decoder for a polymorphic (forall) type" $
  "ft" ~>
    -- Generate a lambda that takes a decoder for the type parameter.
    -- The bound parameter IS that decoder; its type is
    -- (Graph -> Term -> Either DecodingError a), matching the argument
    -- prependForallDecoders adds to the decoder scheme. The synthesizer must
    -- populate this domain annotation: with inference disabled for derived
    -- modules (#476), an unannotated binder reaches codegen as an untyped term
    -- variable, which the Java/Scala coders reject.
    Core.termLambda $ Core.lambda
        (decodeBindingName @@ Core.forallTypeParameter (var "ft"))
        (just $ Core.typeFunction $ Core.functionType
          (Core.typeVariable (Core.nameLift _Graph))
          (Core.typeFunction $ Core.functionType
            (Core.typeVariable (Core.nameLift _Term))
            (Core.typeEither $ Core.eitherType
              (Core.typeVariable (Core.nameLift _DecodingError))
              (Core.typeVariable (Core.forallTypeParameter (var "ft"))))))
        (decodeType @@ Core.forallTypeBody (var "ft"))

-- | Generate a decoder for an Either type
-- | Generate a decoder for a list type
decodeListType :: TypedTermDefinition (Type -> Term)
decodeListType = define "decodeListType" $
  doc "Generate a decoder for a list type" $
  "elemType" ~>
  "elemDecoder" <~ decodeType @@ var "elemType" $
  -- decodeList : forall t0. decoder<t0> -> decoder<list<t0>>; instantiate t0
  -- to the decoded element type the synthesizer already knows. (#476)
  MetaTerms.tyapp (DeepCore.ref ExtractCore.decodeList) (decoderFullResultType @@ var "elemType")
    @@@ var "elemDecoder"

-- | Generate a decoder for a map type
-- | Generate a decoder for a literal type
-- Match on the LiteralType to generate type-specific decoders
-- Note: Uses 'cases' instead of 'match' to avoid variable shadowing from eta expansion
decodeLiteralType :: TypedTermDefinition (LiteralType -> Term)
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
    -- Helper to wrap a Literal handler with Term.literal matching.
    -- resultType = the native literal type this decoder yields (for eithers.either typeApp). (#476)
    -- The \v lambda binds the matched Literal value; its domain is the generic Literal
    -- (the inner handler narrows to the specific variant). (#476)
    decodeLiteral resultType handleLiteral = deannotateAndMatch resultType
      (just $ leftError resultType (string "expected literal")) [
      DeepCore.caseAlternative _Term_literal $ MetaTerms.lambdaTyped "v" (Core.typeVariable (Core.nameLift _Literal)) $ handleLiteral @@@ DeepCore.var "v"]

    -- For each literal variant, the value lambda's domain is the variant's literal type;
    -- its body is a type-applied Right : either<DErr, R>. Variable name = the original
    -- per-variant name (b/d/f/i/s) so output matches inference. (#476)
    rlam nm ty body = MetaTerms.lambdaTyped nm ty (body (DeepCore.var nm))

    -- Decode binary: Term -> Either DecodingError Binary
    decodeBinary = decodeLiteral binTy $ DeepCore.match _Literal
      (just $ leftError binTy (string "expected binary literal")) [
      DeepCore.caseAlternative _Literal_binary $ rlam "b" binTy $ \x -> rightT binTy x]
      where binTy = Core.typeLiteral Core.literalTypeBinary

    -- Decode boolean: Term -> Either DecodingError Bool
    decodeBoolean = decodeLiteral boolTy $ DeepCore.match _Literal
      (just $ leftError boolTy (string "expected boolean literal")) [
      DeepCore.caseAlternative _Literal_boolean $ rlam "b" boolTy $ \x -> rightT boolTy x]
      where boolTy = Core.typeLiteral Core.literalTypeBoolean

    -- Decode decimal: Term -> Either DecodingError Scientific
    decodeDecimal = decodeLiteral decTy $ DeepCore.match _Literal
      (just $ leftError decTy (string "expected decimal literal")) [
      DeepCore.caseAlternative _Literal_decimal $ rlam "d" decTy $ \x -> rightT decTy x]
      where decTy = Core.typeLiteral Core.literalTypeDecimal

    -- Decode float: Term -> Either DecodingError <specific float type>.
    -- ft (the FloatType term) is in scope, so the result literal type is literal<float<ft>>. (#476)
    decodeFloat ft = cases _FloatType ft Nothing [
        _FloatType_float32>>: constant $ decodeFloatVariant (Core.typeLiteral (Core.literalTypeFloat ft)) _FloatValue_float32 (string "float32"),
        _FloatType_float64>>: constant $ decodeFloatVariant (Core.typeLiteral (Core.literalTypeFloat ft)) _FloatValue_float64 (string "float64")]

    -- Helper to decode a specific float variant. The \f value lambda's domain is the float literal type. (#476)
    decodeFloatVariant resultType floatVariant floatName =
      decodeLiteral resultType $ DeepCore.match _Literal
      (just $ leftError resultType (Strings.cat $ list [string "expected ", floatName, string " literal"])) [
      DeepCore.caseAlternative _Literal_float $ DeepCore.match _FloatValue
        (just $ leftError resultType (Strings.cat $ list [string "expected ", floatName, string " value"])) [
        DeepCore.caseAlternative floatVariant $ MetaTerms.lambdaTyped "f" resultType $ rightT resultType $ DeepCore.var "f"]]

    -- Decode integer: Term -> Either DecodingError <specific integer type>.
    -- it (the IntegerType term) is in scope, so result literal type is literal<integer<it>>. (#476)
    decodeInteger it = cases _IntegerType it Nothing [
        _IntegerType_bigint>>: constant $ decodeIntegerVariant (intLit it) _IntegerValue_bigint (string "bigint"),
        _IntegerType_int8>>: constant $ decodeIntegerVariant (intLit it) _IntegerValue_int8 (string "int8"),
        _IntegerType_int16>>: constant $ decodeIntegerVariant (intLit it) _IntegerValue_int16 (string "int16"),
        _IntegerType_int32>>: constant $ decodeIntegerVariant (intLit it) _IntegerValue_int32 (string "int32"),
        _IntegerType_int64>>: constant $ decodeIntegerVariant (intLit it) _IntegerValue_int64 (string "int64"),
        _IntegerType_uint8>>: constant $ decodeIntegerVariant (intLit it) _IntegerValue_uint8 (string "uint8"),
        _IntegerType_uint16>>: constant $ decodeIntegerVariant (intLit it) _IntegerValue_uint16 (string "uint16"),
        _IntegerType_uint32>>: constant $ decodeIntegerVariant (intLit it) _IntegerValue_uint32 (string "uint32"),
        _IntegerType_uint64>>: constant $ decodeIntegerVariant (intLit it) _IntegerValue_uint64 (string "uint64")]
      where intLit i = Core.typeLiteral (Core.literalTypeInteger i)

    -- Helper to decode a specific integer variant
    decodeIntegerVariant resultType intVariant intName = decodeLiteral resultType $ DeepCore.match _Literal
      (just $ leftError resultType (Strings.cat $ list [string "expected ", intName, string " literal"])) [
      DeepCore.caseAlternative _Literal_integer $ DeepCore.match _IntegerValue
        (just $ leftError resultType (Strings.cat $ list [string "expected ", intName, string " value"])) [
        DeepCore.caseAlternative intVariant $ MetaTerms.lambdaTyped "i" resultType $ rightT resultType $ DeepCore.var "i"]]

    -- Decode string: Term -> Either DecodingError String
    decodeString = decodeLiteral (Core.typeLiteral Core.literalTypeString) $ DeepCore.match _Literal
      (just $ leftError strTy (string "expected string literal")) [
      DeepCore.caseAlternative _Literal_string $ rlam "s" strTy $ \x -> rightT strTy x]
      where strTy = Core.typeLiteral Core.literalTypeString

-- | Transform a type module into a decoder module
-- Returns Nothing if the module has no decodable type definitions
-- | Generate a decoder for a map type
decodeMapType :: TypedTermDefinition (MapType -> Term)
decodeMapType = define "decodeMapType" $
  doc "Generate a decoder for a map type" $
  "mt" ~>
  "keyDecoder" <~ decodeType @@ Core.mapTypeKeys (var "mt") $
  "valDecoder" <~ decodeType @@ Core.mapTypeValues (var "mt") $
  -- decodeMap : forall t0,t1. (ord t0) => decoder<t0> -> decoder<t1> -> decoder<map<t0,t1>>;
  -- instantiate t0,t1 to the decoded key/value types. (#476)
  MetaTerms.tyapps (DeepCore.ref (ExtractCore.decodeMap :: TypedTermDefinition ((Graph -> Term -> Either DecodingError Int) -> (Graph -> Term -> Either DecodingError Int) -> Graph -> Term -> Either DecodingError (M.Map Int Int))))
    [decoderFullResultType @@ Core.mapTypeKeys (var "mt"),
     decoderFullResultType @@ Core.mapTypeValues (var "mt")]
    @@@ var "keyDecoder" @@@ var "valDecoder"

-- | Generate a decoder for an optional/maybe type
-- | Generate a decoder for an optional/maybe type
decodeMaybeType :: TypedTermDefinition (Type -> Term)
decodeMaybeType = define "decodeMaybeType" $
  doc "Generate a decoder for an optional type" $
  "elemType" ~>
  "elemDecoder" <~ decodeType @@ var "elemType" $
  -- decodeMaybe : forall t0. decoder<t0> -> decoder<optional<t0>>; instantiate t0. (#476)
  MetaTerms.tyapp (DeepCore.ref ExtractCore.decodeMaybe) (decoderFullResultType @@ var "elemType")
    @@@ var "elemDecoder"

-- | Generate a decoder for a pair type
-- | Transform a type module into a decoder module
-- Returns Nothing if the module has no decodable type definitions
decodeModule :: TypedTermDefinition (InferenceContext -> Graph -> Module -> Prelude.Either Error (Maybe Module))
decodeModule = define "decodeModule" $
  doc "Transform a type module into a decoder module" $
  "cx" ~> "graph" ~> "mod" ~>
    "typeBindings" <<~ (filterTypeBindings @@ var "cx" @@ var "graph" @@
      (Optionals.cat $ Lists.map
        ("d" ~> cases _Definition (var "d") (Just nothing) [
          _Definition_type>>: "td" ~>
            just (Annotations.typeBinding @@ (Packaging.typeDefinitionName $ var "td") @@ (Core.typeSchemeBody $ Packaging.typeDefinitionBody $ var "td"))])
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
        "allDecodedDeps" <~ (primitive DefLists.nub @@ (Lists.map (asTerm decodeModuleName) (Lists.map ("dep" ~> Packaging.moduleDependencyModule (var "dep")) (Packaging.moduleDependencies (var "mod"))))) $
        right (just (Packaging.module_
          (decodeModuleName @@ (Packaging.moduleName (var "mod")))
          (just (Packaging.entityMetadata
            (just (Strings.cat $ list [
              string "Term decoders for ",
              Packaging.unModuleName (Packaging.moduleName (var "mod"))]))
            (list ([] :: [TypedTerm String])) (list ([] :: [TypedTerm EntityReference])) nothing))
          (Lists.map ("ns" ~> Packaging.moduleDependency (var "ns") nothing) (Lists.concat2
            (list [
              (Packaging.moduleName2 $ string "hydra.extract.core"),
              (Packaging.moduleName2 $ string "hydra.lexical"),
              (Packaging.moduleName2 $ string "hydra.rewriting"),
              Packaging.moduleName (var "mod"),
              Packaging.moduleName2 $ string "hydra.util"])
            (var "allDecodedDeps")))
          (Lists.map ("b" ~> Packaging.definitionTerm (Packaging.termDefinition
            (Core.bindingName $ var "b") nothing
            (Optionals.map (asTerm Scoping.typeSchemeToTermSignature) $ Core.bindingTypeScheme $ var "b")
            (Core.bindingTerm $ var "b")))
            (var "decodedBindings")))))

-- | Generate a decoder module name from a source module name
-- For example, "hydra.util" -> "hydra.decode.util"
decodeModuleName :: TypedTermDefinition (ModuleName -> ModuleName)
decodeModuleName = define "decodeModuleName" $
  doc "Generate a decoder module name from a source module name" $
  Names.derivedModuleName @@ list [string "hydra", string "decode"] @@ boolean True

-- | Generate a decoder for a record type with element name
-- | Generate a decoder for a pair type
decodePairType :: TypedTermDefinition (PairType -> Term)
decodePairType = define "decodePairType" $
  doc "Generate a decoder for a pair type" $
  "pt" ~>
  "firstDecoder" <~ decodeType @@ Core.pairTypeFirst (var "pt") $
  "secondDecoder" <~ decodeType @@ Core.pairTypeSecond (var "pt") $
  -- decodePair : forall t0,t1. decoder<t0> -> decoder<t1> -> decoder<(t0,t1)>;
  -- instantiate t0,t1 to the decoded first/second types. (#476)
  MetaTerms.tyapps (DeepCore.ref ExtractCore.decodePair)
    [decoderFullResultType @@ Core.pairTypeFirst (var "pt"),
     decoderFullResultType @@ Core.pairTypeSecond (var "pt")]
    @@@ var "firstDecoder" @@@ var "secondDecoder"

-- | Generate a decoder for a set type
-- | Generate a decoder for a record type (no element name)
decodeRecordType :: TypedTermDefinition ([FieldType] -> Term)
decodeRecordType = define "decodeRecordType" $
  doc "Generate a decoder for a record type" $
  "rt" ~> decodeRecordTypeImpl @@ Core.name (string "unknown") @@ var "rt"
            @@ Core.typeVariable (Core.name (string "unknown"))

-- | Generate a decoder for a record type (implementation with name parameter)
-- | Generate a decoder for a record type (implementation with name parameter)
decodeRecordTypeImpl :: TypedTermDefinition (Name -> [FieldType] -> Type -> Term)
decodeRecordTypeImpl = define "decodeRecordTypeImpl" $
  doc ("Generate a decoder for a record type with a type name. rtype is the fully-applied"
    <> " result type (e.g. Table<v>) used for the record's body annotations. (#476)") $
  "tname" ~> "rt" ~> "rtype" ~>
  "recType" <~ (var "rtype") $
  "graphType" <~ (Core.typeVariable (Core.name (string "hydra.graph.Graph"))) $
  "termType" <~ (Core.typeVariable (Core.nameLift _Term)) $
  -- For each field, build a term that decodes it from fieldMap using requireField helper.
  -- requireField : forall t0,t1,t2. ... ; t0=Graph, t1=Term, t2=field's decoded type. (#476)
  "decodeFieldTerm" <~ ("ft" ~>
    MetaTerms.tyapps (DeepCore.ref ExtractCore.requireField)
      [var "graphType", var "termType", decoderFullResultType @@ (Core.fieldTypeType $ var "ft")]
      @@@ (DeepCore.string $ Core.unName $ Core.fieldTypeName $ var "ft")
      @@@ (decodeType @@ (Core.fieldTypeType $ var "ft"))
      @@@ DeepCore.var "fieldMap"
      @@@ DeepCore.var "cx") $
  -- Build the body: a nested chain of eithers.bind calls that decode each field and build the record
  -- We need: d1 >>= \v1 -> d2 >>= \v2 -> d3 >>= \v3 -> Right Record{...}
  -- Using foldl on reversed list to build from inside out
  -- The lambda for each field uses the field name with a prefix to avoid shadowing decoder functions.
  -- Its domain is the field's decoded type. (#476)
  "localVarName" <~ ("ft" ~> Core.name $ Strings.cat $ list [string "field_", Core.unName $ Core.fieldTypeName $ var "ft"]) $
  "toFieldLambda" <~ ("ft" ~> "body" ~>
    Core.termLambda $ Core.lambda (var "localVarName" @@ var "ft")
      (just (decoderFullResultType @@ (Core.fieldTypeType $ var "ft"))) $ var "body") $
  "decodeBody" <~ (
    Lists.foldl
      ("acc" ~> "ft" ~>
        -- eithers.bind : forall x,y,z. either<x,y> -> (y->either<x,z>) -> either<x,z>;
        -- x=DecodingError, y=field's decoded type, z=record type. (#476)
        MetaTerms.tyapps (DeepCore.primitive DefEithers.bind)
          [Core.typeVariable (Core.nameLift _DecodingError),
           decoderFullResultType @@ (Core.fieldTypeType $ var "ft"),
           var "recType"]
          @@@ (var "decodeFieldTerm" @@ var "ft")
          @@@ (var "toFieldLambda" @@ var "ft" @@ var "acc"))
      -- Base case: Right with the decoded record value (type-applied either<DErr, recType>). (#476)
      (rightT (var "recType") $ Core.termRecord $ Core.record (var "tname") $
        Lists.map ("ft" ~> Core.field (Core.fieldTypeName $ var "ft") $ Core.termVariable $ var "localVarName" @@ var "ft")
          (var "rt"))
      (Lists.reverse $ var "rt")) $
  deannotateAndMatch
    (var "recType")
    -- Name the expected type in the fallback so a shape mismatch (e.g. a value
    -- decoded against the wrong type after a kernel record rename) fails loud at
    -- the decode site naming what was expected, rather than degrading silently
    -- and surfacing far downstream as untyped bindings (#414).
    (just $ leftError (var "recType") (Strings.cat $ list [
      string "expected a record of type ", Core.unName $ var "tname"])) [
    -- The \record lambda binds the matched Record value. (#476)
    DeepCore.caseAlternative _Term_record $ MetaTerms.lambdaTyped "record" (Core.typeVariable (Core.nameLift _Record)) $
      -- fieldMap : map<Name, Term> (from toFieldMap). (#476)
      letsT [
        ("fieldMap",
          Core.typeMap (Core.mapType (Core.typeVariable (Core.nameLift _Name)) (Core.typeVariable (Core.nameLift _Term))),
          DeepCore.ref ExtractCore.toFieldMap @@@ DeepCore.var "record")]
        (var "decodeBody")]

-- | Generate a decoder for a polymorphic (forall) type
-- For a type like `forall a. T[a]`, generates a lambda that takes a decoder for `a`
-- and returns a decoder for the body type `T[a]`
-- | Generate a decoder for a record type with element name
decodeRecordTypeNamed :: TypedTermDefinition (Name -> [FieldType] -> Type -> Term)
decodeRecordTypeNamed = define "decodeRecordTypeNamed" $
  doc "Generate a decoder for a record type with element name" $
  "ename" ~> "rt" ~> "rtype" ~> decodeRecordTypeImpl @@ var "ename" @@ var "rt" @@ var "rtype"

-- | Generate a decoder for a record type (no element name)
-- | Generate a decoder for a set type
decodeSetType :: TypedTermDefinition (Type -> Term)
decodeSetType = define "decodeSetType" $
  doc "Generate a decoder for a set type" $
  "elemType" ~>
  "elemDecoder" <~ decodeType @@ var "elemType" $
  -- decodeSet : forall t0. (ord t0) => decoder<t0> -> decoder<set<t0>>; instantiate t0. (#476)
  MetaTerms.tyapp (DeepCore.ref (ExtractCore.decodeSet :: TypedTermDefinition ((Graph -> Term -> Either DecodingError Int) -> Graph -> Term -> Either DecodingError (S.Set Int)))) (decoderFullResultType @@ var "elemType")
    @@@ var "elemDecoder"

-- | Generate a decoder term for a given Type with element name context
-- | Generate a decoder term for a given Type (without element name context)
decodeType :: TypedTermDefinition (Type -> Term)
decodeType = define "decodeType" $
  doc "Generate a decoder term for a Type" $
  "typ" ~>
  cases _Type (var "typ")
    (Just $ MetaTerms.lambdaTyped "cx" (Core.typeVariable (Core.name (string "hydra.graph.Graph"))) $ MetaTerms.lambdaTyped "t" (Core.typeVariable (Core.nameLift _Term)) $ leftError (Core.typeVariable (Core.nameLift _Term)) $ string "unsupported type variant") [
    _Type_annotated>>: "at" ~> decodeType @@ (Core.annotatedTypeBody (var "at")),
    _Type_application>>: "appType" ~>
      (decodeType @@ Core.applicationTypeFunction (var "appType"))
        @@@ (decodeType @@ Core.applicationTypeArgument (var "appType")),
    _Type_either>>: "et" ~> decodeEitherType @@ var "et",
    _Type_forall>>: "ft" ~> decodeForallType @@ var "ft",
    _Type_list>>: "elemType" ~> decodeListType @@ var "elemType",
    _Type_literal>>: "lt" ~> decodeLiteralType @@ var "lt",
    _Type_map>>: "mt" ~> decodeMapType @@ var "mt",
    _Type_optional>>: "elemType" ~> decodeMaybeType @@ var "elemType",
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
decodeTypeNamed :: TypedTermDefinition (Name -> Type -> Type -> Term)
decodeTypeNamed = define "decodeTypeNamed" $
  doc ("Generate a decoder term for a Type, with element name for nominal types."
    <> " rtype is the FULLY-APPLIED result type for nominal bodies (e.g. DataRow<v> for"
    <> " forall v. wrap...), so the body's intermediate type annotations carry the type"
    <> " parameters rather than a bare nominal name — otherwise Java/Scala coders emit raw"
    <> " types that fail to compile against the parameterized signature. (#476)") $
  "ename" ~> "typ" ~> "rtype" ~>
  cases _Type (var "typ")
    (Just $ MetaTerms.lambdaTyped "cx" (Core.typeVariable (Core.name (string "hydra.graph.Graph"))) $ MetaTerms.lambdaTyped "t" (Core.typeVariable (Core.nameLift _Term)) $ leftError (Core.typeVariable (Core.nameLift _Term)) $ string "unsupported type variant") [
    _Type_annotated>>: "at" ~> decodeTypeNamed @@ var "ename" @@ (Core.annotatedTypeBody (var "at")) @@ var "rtype",
    _Type_application>>: "appType" ~>
      (decodeType @@ Core.applicationTypeFunction (var "appType"))
        @@@ (decodeType @@ Core.applicationTypeArgument (var "appType")),
    _Type_either>>: "et" ~> decodeEitherType @@ var "et",
    _Type_forall>>: "ft" ~>
      -- The bound parameter IS the decoder for the type parameter; its type is
      -- (Graph -> Term -> Either DecodingError a), matching the argument
      -- prependForallDecoders adds to the decoder scheme. The synthesizer must
      -- populate this domain: with inference disabled for derived modules (#476),
      -- an unannotated binder reaches codegen as an untyped term variable, which
      -- the Java/Scala coders reject. This is the arm top-level bindings flow
      -- through (via decodeBinding -> decodeTypeNamed). (#476)
      Core.termLambda $ Core.lambda (decodeBindingName @@ Core.forallTypeParameter (var "ft"))
          (just $ Core.typeFunction $ Core.functionType
            (Core.typeVariable (Core.nameLift _Graph))
            (Core.typeFunction $ Core.functionType
              (Core.typeVariable (Core.nameLift _Term))
              (Core.typeEither $ Core.eitherType
                (Core.typeVariable (Core.nameLift _DecodingError))
                (Core.typeVariable (Core.forallTypeParameter (var "ft"))))))
          (decodeTypeNamed @@ var "ename" @@ Core.forallTypeBody (var "ft") @@ var "rtype"),
    _Type_list>>: "elemType" ~> decodeListType @@ var "elemType",
    _Type_literal>>: "lt" ~> decodeLiteralType @@ var "lt",
    _Type_map>>: "mt" ~> decodeMapType @@ var "mt",
    _Type_optional>>: "elemType" ~> decodeMaybeType @@ var "elemType",
    _Type_pair>>: "pt" ~> decodePairType @@ var "pt",
    _Type_record>>: "rt" ~> decodeRecordTypeNamed @@ var "ename" @@ var "rt" @@ var "rtype",
    _Type_set>>: "elemType" ~> decodeSetType @@ var "elemType",
    _Type_union>>: "rt" ~> decodeUnionTypeNamed @@ var "ename" @@ var "rt" @@ var "rtype",
    _Type_unit>>: constant decodeUnitType,
    _Type_void>>: constant decodeUnitType,
    _Type_wrap>>: "wt" ~> decodeWrappedTypeNamed @@ var "ename" @@ var "wt" @@ var "rtype",
    _Type_variable>>: "typeName" ~> Core.termVariable (decodeBindingName @@ var "typeName")]

-- | Generate a decoder term for a given Type (without element name context)
-- | Generate a decoder for a union type (without element name)
decodeUnionType :: TypedTermDefinition ([FieldType] -> Term)
decodeUnionType = define "decodeUnionType" $
  doc "Generate a decoder for a union type" $
  "rt" ~> decodeUnionTypeNamed @@ Core.name (string "unknown") @@ var "rt"
            @@ Core.typeVariable (Core.name (string "unknown"))

-- | Generate a decoder for a wrapped type with element name
-- | Generate a decoder for a union type with element name
decodeUnionTypeNamed :: TypedTermDefinition (Name -> [FieldType] -> Type -> Term)
decodeUnionTypeNamed = define "decodeUnionTypeNamed" $
  doc ("Generate a decoder for a union type with the given element name. rtype is the"
    <> " fully-applied result type (e.g. Foo<v>) used for body annotations. (#476)") $
  "ename" ~> "rt" ~> "rtype" ~>
  "unionType" <~ (var "rtype") $
  "decErrType" <~ (Core.typeVariable (Core.nameLift _DecodingError)) $
  "nameType" <~ (Core.typeVariable (Core.nameLift _Name)) $
  "variantFnType" <~ (Core.typeFunction $ Core.functionType
    (Core.typeVariable (Core.nameLift _Term))
    (Core.typeEither $ Core.eitherType (var "decErrType") (var "unionType"))) $
  "toVariantPair" <~ ("ft" ~>
    -- eithers.map : forall x,y,z. (x->y)->either<z,x>->either<z,y>;
    -- x=field's decoded type, y=union type, z=DecodingError. The variant fn is a decoder:
    -- it takes input : Term (NOT either) and runs (decodeType fieldType) over (cx, input);
    -- t : fieldType. (#476)
    "fldType" <~ (decoderFullResultType @@ (Core.fieldTypeType $ var "ft")) $
    -- pair<Name, variantFn>: first = the variant Name, second = its decoder lambda. (#476)
    pairT (Core.typeVariable (Core.nameLift _Name)) (var "variantFnType")
      (DeepCore.wrap _Name $ DeepCore.string $ Core.unName $ Core.fieldTypeName $ var "ft")
      (MetaTerms.lambdaTyped "input" (Core.typeVariable (Core.nameLift _Term)) $
        MetaTerms.tyapps (DeepCore.primitive DefEithers.map) [var "fldType", var "unionType", var "decErrType"]
        @@@ (MetaTerms.lambdaTyped "t" (var "fldType") $ Core.termInject $ Core.injection (var "ename") $ Core.field (Core.fieldTypeName $ var "ft") $ DeepCore.var "t")
        @@@ ((decodeType @@ (Core.fieldTypeType $ var "ft")) @@@ DeepCore.var "cx" @@@ DeepCore.var "input"))) $
  deannotateAndMatch
    (var "unionType")
    (just $ leftError (var "unionType") $ string "expected union") [
    DeepCore.caseAlternative _Term_inject $ MetaTerms.lambdaTyped "inj" (Core.typeVariable (Core.nameLift _Injection)) $ letsT [
      ("field", Core.typeVariable (Core.nameLift _Field), DeepCore.project _Injection _Injection_field @@@ DeepCore.var "inj"),
      ("fname", Core.typeVariable (Core.nameLift _Name), DeepCore.project _Field _Field_name @@@ DeepCore.var "field"),
      ("fterm", Core.typeVariable (Core.nameLift _Term), DeepCore.project _Field _Field_term @@@ DeepCore.var "field"),
      -- maps.fromList : forall k,v. list<(k,v)> -> map<k,v>; k=Name, v=variantFn. (#476)
      ("variantMap", Core.typeMap (Core.mapType (var "nameType") (var "variantFnType")),
        MetaTerms.tyapps (DeepCore.primitive DefMaps.fromList) [var "nameType", var "variantFnType"]
        @@@ (DeepCore.list $ Lists.map (var "toVariantPair") $ var "rt"))] $
      -- optionals.cases : forall x,y. optional<x> -> y -> (x->y) -> y;
      -- x=variantFn, y=either<DErr,unionType>. (#476)
      MetaTerms.tyapps (DeepCore.primitive DefOptionals.cases)
        [var "variantFnType", Core.typeEither $ Core.eitherType (var "decErrType") (var "unionType")]
        -- maps.lookup : forall k,v. k -> map<k,v> -> optional<v>; k=Name, v=variantFn. (#476)
        @@@ (MetaTerms.tyapps (DeepCore.primitive DefMaps.lookup) [var "nameType", var "variantFnType"]
          @@@ DeepCore.var "fname"
          @@@ DeepCore.var "variantMap")
        @@@ (leftT (var "unionType") $ DeepCore.wrap _DecodingError $ DeepCore.primitive DefStrings.cat
          @@@ (DeepCore.list $ list [
            DeepCore.string $ string "no such field ",
            DeepCore.unwrap _Name @@@ DeepCore.var "fname",
            DeepCore.string $ string " in union"]))
        @@@ (MetaTerms.lambdaTyped "f" (var "variantFnType") $ DeepCore.var "f" @@@ DeepCore.var "fterm")]

-- | Generate a decoder for a union type (without element name)
-- | Generate a decoder for the unit type
decodeUnitType :: TypedTermDefinition Term
decodeUnitType = define "decodeUnitType" $
  doc "Generate a decoder for the unit type" $
  MetaTerms.lambdaTyped "cx" (Core.typeVariable (Core.name (string "hydra.graph.Graph"))) $ MetaTerms.lambdaTyped "t" (Core.typeVariable (Core.nameLift _Term)) $ DeepCore.ref ExtractCore.decodeUnit @@@ DeepCore.var "cx" @@@ DeepCore.var "t"

-- | Generate a decoder for a union type with element name
-- | Generate a decoder for a wrapped type (without element name)
decodeWrappedType :: TypedTermDefinition (Type -> Term)
decodeWrappedType = define "decodeWrappedType" $
  doc "Generate a decoder for a wrapped type" $
  "wt" ~> decodeWrappedTypeNamed @@ Core.name (string "unknown") @@ var "wt"
            @@ Core.typeVariable (Core.name (string "unknown"))

-- | Filter bindings to only decodable type definitions
-- | Generate a decoder for a wrapped type with element name
decodeWrappedTypeNamed :: TypedTermDefinition (Name -> Type -> Type -> Term)
decodeWrappedTypeNamed = define "decodeWrappedTypeNamed" $
  doc ("Generate a decoder for a wrapped type with the given element name. rtype is the"
    <> " fully-applied result type (e.g. DataRow<v>) used for body annotations. (#476)") $
  "ename" ~> "wt" ~> "rtype" ~>
  "bodyDecoder" <~ decodeType @@ var "wt" $
  "bodyType" <~ (decoderFullResultType @@ var "wt") $
  deannotateAndMatch
    (var "rtype")
    (just $ leftError (var "rtype") (string "expected wrapped type")) [
    -- eithers.map : forall x,y,z. (x->y)->either<z,x>->either<z,y>;
    -- x=decoded body type, y=wrapper type, z=DecodingError. (#476)
    DeepCore.caseAlternative _Term_wrap $ MetaTerms.lambdaTyped "wrappedTerm" (Core.typeVariable (Core.nameLift _WrappedTerm)) $
      MetaTerms.tyapps (DeepCore.primitive DefEithers.map)
        [var "bodyType", var "rtype", Core.typeVariable (Core.nameLift _DecodingError)]
        @@@ (MetaTerms.lambdaTyped "b" (var "bodyType") $ DeepCore.wrapDynamic (var "ename") (DeepCore.var "b"))
        @@@ (var "bodyDecoder" @@@ DeepCore.var "cx"
          @@@ (DeepCore.project _WrappedTerm _WrappedTerm_body @@@ DeepCore.var "wrappedTerm"))]

-- | Generate a decoder for a wrapped type (without element name)
-- | Get the full result type for a decoder, preserving type applications
-- For forall t. ColumnSchema<t>, returns ColumnSchema<t> (as a Type, not just a Name)
-- Note: Uses 'cases' instead of 'match' to avoid variable shadowing from eta expansion
decoderFullResultType :: TypedTermDefinition (Type -> Type)
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
    _Type_effect>>: "elemType" ~>
      Core.typeEffect (decoderFullResultType @@ var "elemType"),
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
    -- A literal type decodes to that specific literal type (e.g. string -> literal<string>),
    -- NOT the generic Literal — inference preserves the variant. (#476)
    _Type_literal>>: "lt" ~>
      Core.typeLiteral (var "lt"),
    _Type_map>>: "mt" ~>
      -- Map k v -> Map (decoded k) (decoded v)
      Core.typeMap $ Core.mapType
        (decoderFullResultType @@ Core.mapTypeKeys (var "mt"))
        (decoderFullResultType @@ Core.mapTypeValues (var "mt")),
    _Type_optional>>: "elemType" ~>
      -- Maybe a -> Maybe (decoded a)
      Core.typeOptional (decoderFullResultType @@ var "elemType"),
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
    -- A wrapper (newtype) is value-transparent: a wrapper nested inside a
    -- container (e.g. list<Vertex>) decodes to its unwrapped body, matching
    -- what inference resolves. TypeWrap carries a bare Type, so `wt` IS the
    -- body. Mirrors the _Type_list arm. (#476)
    _Type_wrap>>: "wt" ~>
      decoderFullResultType @@ var "wt"]

-- | Decode a single type binding into a decoder binding
-- Decodes the Type from the binding's term, then generates decoder
-- | Get full result type for decoder with element name
decoderFullResultTypeNamed :: TypedTermDefinition (Name -> Type -> Type)
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
    _Type_effect>>: "elemType" ~>
      Core.typeEffect (decoderFullResultType @@ var "elemType"),
    _Type_either>>: "et" ~>
      Core.typeEither $ Core.eitherType
        (decoderFullResultType @@ Core.eitherTypeLeft (var "et"))
        (decoderFullResultType @@ Core.eitherTypeRight (var "et")),
    _Type_list>>: "elemType" ~>
      Core.typeList (decoderFullResultType @@ var "elemType"),
    -- specific literal type (e.g. literal<string>), not generic Literal. (#476)
    _Type_literal>>: "lt" ~>
      Core.typeLiteral (var "lt"),
    _Type_map>>: "mt" ~>
      Core.typeMap $ Core.mapType
        (decoderFullResultType @@ Core.mapTypeKeys (var "mt"))
        (decoderFullResultType @@ Core.mapTypeValues (var "mt")),
    _Type_optional>>: "elemType" ~>
      Core.typeOptional (decoderFullResultType @@ var "elemType"),
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
decoderResultType :: TypedTermDefinition (Type -> Name)
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
decoderType :: TypedTermDefinition (Type -> Type)
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
decoderTypeNamed :: TypedTermDefinition (Name -> Type -> Type)
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
decoderTypeScheme :: TypedTermDefinition (Type -> TypeScheme)
decoderTypeScheme = define "decoderTypeScheme" $
  doc "Build type scheme for a decoder function" $
  "typ" ~>
    "typeVars" <~ collectTypeVariables @@ var "typ" $
    "allOrdVars" <~ collectOrdConstrainedVariables @@ var "typ" $
    -- Filter to only include actual forall-bound type variables
    -- (collectOrdConstrainedVariables may return nominal type references like "hydra.relational.ColumnName")
    "ordVars" <~ Lists.filter
      ("v" ~> Lists.elem (var "v" :: TypedTerm Name) (var "typeVars" :: TypedTerm [Name]))
      (var "allOrdVars") $
    -- Build constraints: for each ordVar, add Ord constraint (uses original var names, normalization renames them)
    "constraints" <~ (
      Logic.ifElse (Lists.null (var "ordVars"))
        Phantoms.nothing
        (just ((Maps.fromList $ Lists.map
          ("v" ~> pair (var "v") (Core.typeVariableConstraints $ list [Core.typeClassConstraintSimple $ Core.name (string "ordering")]))
          (var "ordVars")) :: TypedTerm (M.Map Name TypeVariableConstraints)))) $
    Core.typeScheme
      (var "typeVars")
      (decoderType @@ var "typ")
      (var "constraints")

-- | Build a decoder type scheme with element name for nominal types
-- | Build a decoder type scheme with element name for nominal types
decoderTypeSchemeNamed :: TypedTermDefinition (Name -> Type -> TypeScheme)
decoderTypeSchemeNamed = define "decoderTypeSchemeNamed" $
  doc "Build type scheme for a decoder function with element name" $
  "ename" ~> "typ" ~>
    "typeVars" <~ collectTypeVariables @@ var "typ" $
    "allOrdVars" <~ collectOrdConstrainedVariables @@ var "typ" $
    "ordVars" <~ Lists.filter
      ("v" ~> Lists.elem (var "v" :: TypedTerm Name) (var "typeVars" :: TypedTerm [Name]))
      (var "allOrdVars") $
    "constraints" <~ (
      Logic.ifElse (Lists.null (var "ordVars"))
        Phantoms.nothing
        (just ((Maps.fromList $ Lists.map
          ("v" ~> pair (var "v") (Core.typeVariableConstraints $ list [Core.typeClassConstraintSimple $ Core.name (string "ordering")]))
          (var "ordVars")) :: TypedTerm (M.Map Name TypeVariableConstraints)))) $
    Core.typeScheme
      (var "typeVars")
      (decoderTypeNamed @@ var "ename" @@ var "typ")
      (var "constraints")

-- | Build decoder function type with element name
-- | Filter bindings to only decodable type definitions
filterTypeBindings :: TypedTermDefinition (InferenceContext -> Graph -> [Binding] -> Prelude.Either Error [Binding])
filterTypeBindings = define "filterTypeBindings" $
  doc "Filter bindings to only decodable type definitions" $
  "cx" ~> "graph" ~> "bindings" ~>
  Eithers.map (primitive DefOptionals.cat) $
    Eithers.mapList (isDecodableBinding @@ var "cx" @@ var "graph") $
      primitive DefLists.filter @@ Annotations.isNativeType @@ var "bindings"

-- | Check if a binding is decodable and return Just binding if so, Nothing otherwise
-- | Check if a binding is decodable and return Just binding if so, Nothing otherwise
isDecodableBinding :: TypedTermDefinition (InferenceContext -> Graph -> Binding -> Prelude.Either Error (Maybe Binding))
isDecodableBinding = define "isDecodableBinding" $
  doc "Check if a binding is decodable (serializable type)" $
  "cx" ~> "graph" ~> "b" ~>
    "serializable" <<~ Predicates.isSerializableByName @@ var "cx" @@ var "graph" @@ (Core.bindingName (var "b")) $
    right (Logic.ifElse (var "serializable") (just (var "b")) nothing)
-- | Helper to prepend decoder types for forall parameters
-- For forall a. forall b. T: prepends (Graph -> Term -> E a) -> (Graph -> Term -> E b) -> to the base type
-- Note: Uses 'cases' instead of 'match' to avoid variable shadowing from eta expansion
prependForallDecoders :: TypedTermDefinition (Type -> Type -> Type)
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
