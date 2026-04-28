
module Hydra.Sources.Kernel.Terms.Predicates where

-- Standard imports for kernel terms modules
import Hydra.Kernel hiding (
  isComplexBinding,
  isComplexTerm,
  isComplexVariable,
  isEncodedTerm,
  isEncodedType,
  isEnumRowType,
  isEnumType,
  isNominalType,
  isSerializable,
  isSerializableByName,
  isSerializableType,
  isTrivialTerm,
  isType,
  isUnitTerm,
  isUnitType,
  typeDependencies)
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
import qualified Hydra.Dsl.Packaging       as Packaging
import qualified Hydra.Dsl.Parsing      as Parsing
import           Hydra.Dsl.Meta.Phantoms     as Phantoms
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
import           Prelude hiding ((++))
import qualified Data.Int                    as I
import qualified Data.List                   as L
import qualified Data.Map                    as M
import qualified Data.Set                    as S
import qualified Data.Maybe                  as Y

import qualified Hydra.Sources.Kernel.Terms.Arity        as Arity
import qualified Hydra.Sources.Kernel.Terms.Reflect      as Reflect
import qualified Hydra.Sources.Kernel.Terms.Rewriting    as Rewriting
import qualified Hydra.Sources.Kernel.Terms.Strip        as Strip

import qualified Hydra.Sources.Kernel.Terms.Dependencies as Dependencies
import qualified Hydra.Sources.Kernel.Terms.Lexical     as Lexical
import qualified Hydra.Sources.Decode.Core              as DecodeCore


ns :: Namespace
ns = Namespace "hydra.predicates"

define :: String -> TTerm a -> TTermDefinition a
define = definitionInNamespace ns

module_ :: Module
module_ = Module {
            moduleNamespace = ns,
            moduleDefinitions = definitions,
            moduleTermDependencies = [Arity.ns, Dependencies.ns, moduleNamespace DecodeCore.module_, Lexical.ns, Reflect.ns, Rewriting.ns, Strip.ns],
            moduleTypeDependencies = kernelTypesNamespaces,
            moduleDescription = Just ("Type and term classification predicates")}
  where
    definitions = [
      toDefinition isComplexBinding,
      toDefinition isComplexTerm,
      toDefinition isComplexVariable,
      toDefinition isEncodedTerm,
      toDefinition isEncodedType,
      toDefinition isEnumRowType,
      toDefinition isEnumType,
      toDefinition isNominalType,
      toDefinition isSerializable,
      toDefinition isSerializableByName,
      toDefinition isSerializableType,
      toDefinition isTrivialTerm,
      toDefinition isType,
      toDefinition isUnitTerm,
      toDefinition isUnitType,
      toDefinition typeDependencies]

isComplexBinding :: TTermDefinition (Graph -> Binding -> Bool)
isComplexBinding = define "isComplexBinding" $
  doc "Check if a binding needs to be treated as a function" $
  "tc" ~> "b" ~>
  "term" <~ Core.bindingTerm (var "b") $
  "mts" <~ Core.bindingTypeScheme (var "b") $
  -- Bindings without type schemes are complex (e.g., lifted case expressions)
  Maybes.cases (var "mts")
    (isComplexTerm @@ var "tc" @@ var "term") $
    "ts" ~>
      -- Check if polymorphic
      "isPolymorphic" <~ Logic.not (Lists.null (Core.typeSchemeVariables $ var "ts")) $
      -- Check if non-nullary
      "isNonNullary" <~ Equality.gt (Arity.typeArity @@ (Core.typeSchemeBody $ var "ts")) (int32 0) $
      -- Check if complex term
      "isComplex" <~ isComplexTerm @@ var "tc" @@ var "term" $
      Logic.or (Logic.or (var "isPolymorphic") (var "isNonNullary")) (var "isComplex")

isComplexTerm :: TTermDefinition (Graph -> Term -> Bool)
isComplexTerm = define "isComplexTerm" $
  doc "Check if a term needs to be treated as a function rather than a simple value" $
  "tc" ~> "t" ~>
  cases _Term (var "t")
    (Just $
      -- Default: check if any subterm is complex
      Lists.foldl
        ("b" ~> "sub" ~> Logic.or (var "b") (isComplexTerm @@ var "tc" @@ var "sub"))
        (boolean False)
        (Rewriting.subterms @@ var "t")) [
    _Term_let>>: constant (boolean True),
    _Term_typeApplication>>: constant (boolean True),
    _Term_typeLambda>>: constant (boolean True),
    _Term_variable>>: "name" ~> isComplexVariable @@ var "tc" @@ var "name"]

isComplexVariable :: TTermDefinition (Graph -> Name -> Bool)
isComplexVariable = define "isComplexVariable" $
  doc "Check if a variable is bound to a complex term" $
  "tc" ~> "name" ~>
  -- Check if there's metadata for this variable (indicates complexity)
  "metaLookup" <~ Maps.lookup (var "name") (Graph.graphMetadata $ var "tc") $
  Logic.ifElse
    (Maybes.isJust (var "metaLookup"))
    (boolean True)
    -- Lambda-bound variables are complex because they might be thunked
    (Logic.ifElse
      (Sets.member (var "name") (Graph.graphLambdaVariables $ var "tc"))
      (boolean True)
      -- Check if the variable is in the graph's bound types
      ("typeLookup" <~ Maps.lookup (var "name") (Graph.graphBoundTypes $ var "tc") $
       Maybes.maybe
         -- Not in graphBoundTypes: fall through to graphPrimitives
         ("primLookup" <~ Maps.lookup (var "name") (Graph.graphPrimitives $ var "tc") $
          Maybes.maybe
            -- If not in graph at all, assume mutual recursion (complex)
            (boolean True)
            -- If a primitive, non-nullary iff type arity > 0
            ("prim" ~> Equality.gt (Arity.typeSchemeArity @@ Graph.primitiveTypeScheme (var "prim")) (int32 0))
            (var "primLookup"))
         -- If in graph, check if the binding itself is non-nullary (a function).
         -- Non-nullary bindings are always complex (they take parameters).
         -- Nullary bindings are assumed non-complex from this check;
         -- their actual complexity will be determined by isComplexBinding
         -- at the reference site.
         ("ts" ~> Equality.gt (Arity.typeSchemeArity @@ var "ts") (int32 0))
         (var "typeLookup")))

isEncodedTerm :: TTermDefinition (Term -> Bool)
isEncodedTerm = define "isEncodedTerm" $
  doc "Determines whether a given term is an encoded term (meta-level term)" $
  "t" ~> cases _Term (Strip.deannotateTerm @@ var "t") (Just false) [
    _Term_application>>: "a" ~>
      isEncodedTerm @@ (Core.applicationFunction (var "a")),
    _Term_inject>>: "i" ~>
      Equality.equal (string (unName _Term)) (Core.unName (Core.injectionTypeName (var "i")))]

isEncodedType :: TTermDefinition (Term -> Bool)
isEncodedType = define "isEncodedType" $
  doc "Determines whether a given term is an encoded type" $
  "t" ~> cases _Term (Strip.deannotateTerm @@ var "t") (Just false) [
    _Term_application>>: "a" ~>
      isEncodedType @@ (Core.applicationFunction (var "a")),
    _Term_inject>>: "i" ~>
      Equality.equal (string (unName _Type)) (Core.unName (Core.injectionTypeName (var "i")))]

isEnumRowType :: TTermDefinition ([FieldType] -> Bool)
isEnumRowType = define "isEnumRowType" $
  doc "Check if a row type represents an enum (all fields are unit-typed)" $
  "rt" ~> Lists.foldl (binaryFunction Logic.and) true $
    Lists.map ("f" ~> isUnitType @@ (Strip.deannotateType @@ (Core.fieldTypeType (var "f")))) $
      var "rt"

isEnumType :: TTermDefinition (Type -> Bool)
isEnumType = define "isEnumType" $
  doc "Check if a type is an enum type" $
  "typ" ~>
  match _Type (Just false) [
    _Type_union>>: "rt" ~> isEnumRowType @@ var "rt"]
  @@ (Strip.deannotateType @@ var "typ")

-- | Check whether a type is a nominal type definition (record, union, wrap, or forall wrapping one).
--   Type aliases (applications, functions, literal types, etc.) return false.
isNominalType :: TTermDefinition (Type -> Bool)
isNominalType = define "isNominalType" $
  lambda "typ" $
    cases _Type (Strip.deannotateType @@ var "typ")
      (Just false) [
      _Type_record>>: lambda "rt" $ true,
      _Type_union>>: lambda "rt" $ true,
      _Type_wrap>>: lambda "wt" $ true,
      _Type_forall>>: lambda "fa" $
        isNominalType @@ Core.forallTypeBody (var "fa")]

isSerializable :: TTermDefinition (Context -> Graph -> Binding -> Either Error Bool)
isSerializable = define "isSerializable" $
  doc "Check if an element is serializable (no function types in dependencies) (Either version)" $
  "cx" ~> "graph" ~> "el" ~>
  "variants" <~ ("typ" ~>
    Lists.map (Reflect.typeVariant) (Rewriting.foldOverType @@ Coders.traversalOrderPre @@
      ("m" ~> "t" ~> Lists.cons (var "t") (var "m")) @@ list ([] :: [TTerm Type]) @@ var "typ")) $
  Eithers.map
    ("deps" ~>
      "allVariants" <~ Sets.fromList (Lists.concat (Lists.map (var "variants") (Maps.elems (var "deps")))) $
      Logic.not (Sets.member Variants.typeVariantFunction (var "allVariants")))
    (typeDependencies @@ var "cx" @@ var "graph" @@ false @@ (unaryFunction Equality.identity) @@ Core.bindingName (var "el"))

isSerializableType :: TTermDefinition (Type -> Bool)
isSerializableType = define "isSerializableType" $
  doc "Check if a type is serializable (no function types in the type itself)" $
  "typ" ~>
  "allVariants" <~ Sets.fromList (Lists.map (Reflect.typeVariant)
    (Rewriting.foldOverType @@ Coders.traversalOrderPre @@
      ("m" ~> "t" ~> Lists.cons (var "t") (var "m")) @@ list ([] :: [TTerm Type]) @@ var "typ")) $
  Logic.not (Sets.member Variants.typeVariantFunction (var "allVariants"))

isSerializableByName :: TTermDefinition (Context -> Graph -> Name -> Either Error Bool)
isSerializableByName = define "isSerializableByName" $
  doc "Check if a type (by name) is serializable, resolving all type dependencies (Either version)" $
  "cx" ~> "graph" ~> "name" ~>
  "variants" <~ ("typ" ~>
    Lists.map (Reflect.typeVariant) (Rewriting.foldOverType @@ Coders.traversalOrderPre @@
      ("m" ~> "t" ~> Lists.cons (var "t") (var "m")) @@ list ([] :: [TTerm Type]) @@ var "typ")) $
  Eithers.map
    ("deps" ~>
      "allVariants" <~ Sets.fromList (Lists.concat (Lists.map (var "variants") (Maps.elems (var "deps")))) $
      Logic.not (Sets.member Variants.typeVariantFunction (var "allVariants")))
    (typeDependencies @@ var "cx" @@ var "graph" @@ false @@ (unaryFunction Equality.identity) @@ var "name")

isType :: TTermDefinition (Type -> Bool)
isType = define "isType" $
  doc "Check whether a type is a type (always true for non-encoded types)" $
  "t" ~> cases _Type (Strip.deannotateType @@ var "t") (Just false) [
    _Type_application>>: "a" ~>
      isType @@ (Core.applicationTypeFunction (var "a")),
    _Type_forall>>: "l" ~>
      isType @@ (Core.forallTypeBody (var "l")),
    _Type_union>>: "rt" ~> false,
    _Type_variable>>: "v" ~> Equality.equal (var "v") (Core.nameLift _Type)]

isTrivialTerm :: TTermDefinition (Term -> Bool)
isTrivialTerm = define "isTrivialTerm" $
  doc "Check if a term is trivially cheap (no thunking needed)" $
  "t" ~>
  cases _Term (Strip.deannotateTerm @@ var "t")
    (Just $ boolean False) [
    -- Literals are always trivial
    _Term_literal>>: constant (boolean True),
    -- Plain lambda-bound variables are trivial, but qualified names (element/primitive references
    -- like "hydra.lib.maps.empty") are calls, not just references, and are not trivial.
    _Term_variable>>: "nm" ~>
      Equality.equal (Lists.length (Strings.splitOn (string ".") (Core.unName (var "nm")))) (int32 1),
    -- Unit is trivial
    _Term_unit>>: constant (boolean True),
    -- Field projection on a trivial subterm is trivial (e.g. app.function)
    _Term_application>>: "app" ~>
      "fun" <~ Core.applicationFunction (var "app") $
      "arg" <~ Core.applicationArgument (var "app") $
      cases _Term (var "fun") (Just $ boolean False) [
        -- record projection: trivial if the subject is trivial
        _Term_project>>: constant (isTrivialTerm @@ var "arg"),
        -- newtype unwrap: trivial if the subject is trivial
        _Term_unwrap>>: constant (isTrivialTerm @@ var "arg")],
    -- Maybe term (just x) where x is trivial; nothing is also trivial
    _Term_maybe>>: "opt" ~>
      Maybes.maybe (boolean True) ("inner" ~> isTrivialTerm @@ var "inner") (var "opt"),
    -- Record construction is trivial if all field terms are trivial
    _Term_record>>: "rec" ~>
      Lists.foldl ("acc" ~> "fld" ~> Logic.and (var "acc") (isTrivialTerm @@ (Core.fieldTerm $ var "fld")))
        (boolean True) (Core.recordFields $ var "rec"),
    -- Wrap (newtype construction) is trivial if the inner term is trivial
    _Term_wrap>>: "wt" ~> isTrivialTerm @@ (Core.wrappedTermBody $ var "wt"),
    -- Type applications/lambdas: check the inner term
    _Term_typeApplication>>: "ta" ~> isTrivialTerm @@ (Core.typeApplicationTermBody $ var "ta"),
    _Term_typeLambda>>: "tl" ~> isTrivialTerm @@ (Core.typeLambdaBody $ var "tl")]

isUnitTerm :: TTermDefinition (Term -> Bool)
isUnitTerm = define "isUnitTerm" $
  doc "Check whether a term is the unit term" $
  match _Term (Just false) [_Term_unit>>: constant true]

isUnitType :: TTermDefinition (Type -> Bool)
isUnitType = define "isUnitType" $
  doc "Check whether a type is the unit type" $
  match _Type (Just false) [_Type_unit>>: constant true]

typeDependencies :: TTermDefinition (Context -> Graph -> Bool -> (Type -> Type) -> Name -> Either Error (M.Map Name Type))
typeDependencies = define "typeDependencies" $
  doc "Get all type dependencies for a given type name (Either version)" $
  "cx" ~> "graph" ~> "withSchema" ~> "transform" ~> "name" ~>
  "requireType" <~ ("name" ~>
    "cx1" <~ Ctx.pushTrace (Strings.cat2 (string "type dependencies of ") (Core.unName (var "name"))) (var "cx") $
    Eithers.bind (Lexical.requireBinding @@ var "graph" @@ var "name") (
      "el" ~> Eithers.bimap ("_e" ~> Error.errorDecoding $ var "_e") ("_a" ~> var "_a")
          (decoderFor _Type @@ var "graph" @@ Core.bindingTerm (var "el")))) $
  "toPair" <~ ("name" ~>
    Eithers.map ("typ" ~> pair (var "name") (var "transform" @@ var "typ"))
      (var "requireType" @@ var "name")) $
  "deps" <~ ("seeds" ~> "names" ~>
    Logic.ifElse (Sets.null (var "seeds"))
      (right (var "names"))
      (Eithers.bind (Eithers.mapList (var "toPair") (Sets.toList (var "seeds"))) (
        "pairs" ~>
        "newNames" <~ Maps.union (var "names") (Maps.fromList (var "pairs")) $
        "refs" <~ Lists.foldl (binaryFunction Sets.union) Sets.empty (Lists.map
          ("pair" ~> Dependencies.typeDependencyNames @@ var "withSchema" @@ Pairs.second (var "pair"))
          (var "pairs")) $
        "visited" <~ Sets.fromList (Maps.keys (var "names")) $
        "newSeeds" <~ Sets.difference (var "refs") (var "visited") $
        var "deps" @@ var "newSeeds" @@ var "newNames"))) $
  var "deps" @@ Sets.singleton (var "name") @@ Maps.empty
