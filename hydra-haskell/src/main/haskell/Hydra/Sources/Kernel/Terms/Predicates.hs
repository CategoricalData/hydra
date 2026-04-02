
module Hydra.Sources.Kernel.Terms.Predicates where

-- Standard imports for kernel terms modules
import Hydra.Kernel hiding (
  isEncodedTerm,
  isEncodedType,
  isEnumRowType,
  isEnumType,
  isNominalType,
  isSerializable,
  isSerializableByName,
  isSerializableType,
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
import qualified Hydra.Dsl.Module       as Module
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
module_ = Module ns elements
    [Dependencies.ns, moduleNamespace DecodeCore.module_, Lexical.ns, Reflect.ns, Rewriting.ns, Strip.ns]
    kernelTypesNamespaces $
    Just ("Type and term classification predicates")
  where
    elements = [
      toDefinition isEncodedTerm,
      toDefinition isEncodedType,
      toDefinition isEnumRowType,
      toDefinition isEnumType,
      toDefinition isNominalType,
      toDefinition isSerializable,
      toDefinition isSerializableByName,
      toDefinition isSerializableType,
      toDefinition isType,
      toDefinition isUnitTerm,
      toDefinition isUnitType,
      toDefinition typeDependencies]

isEncodedTerm :: TTermDefinition (Term -> Bool)
isEncodedTerm = define "isEncodedTerm" $
  doc "Determines whether a given term is an encoded term (meta-level term)" $
  "t" ~> cases _Term (Strip.deannotateTerm @@ var "t") (Just false) [
    _Term_application>>: "a" ~>
      isEncodedTerm @@ (Core.applicationFunction (var "a")),
    _Term_union>>: "i" ~>
      Equality.equal (string (unName _Term)) (Core.unName (Core.injectionTypeName (var "i")))]

isEncodedType :: TTermDefinition (Term -> Bool)
isEncodedType = define "isEncodedType" $
  doc "Determines whether a given term is an encoded type" $
  "t" ~> cases _Term (Strip.deannotateTerm @@ var "t") (Just false) [
    _Term_application>>: "a" ~>
      isEncodedType @@ (Core.applicationFunction (var "a")),
    _Term_union>>: "i" ~>
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

isSerializable :: TTermDefinition (Context -> Graph -> Binding -> Either (InContext Error) Bool)
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

isSerializableByName :: TTermDefinition (Context -> Graph -> Name -> Either (InContext Error) Bool)
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

isUnitTerm :: TTermDefinition (Term -> Bool)
isUnitTerm = define "isUnitTerm" $
  doc "Check whether a term is the unit term" $
  match _Term (Just false) [_Term_unit>>: constant true]

isUnitType :: TTermDefinition (Type -> Bool)
isUnitType = define "isUnitType" $
  doc "Check whether a type is the unit type" $
  match _Type (Just false) [_Type_unit>>: constant true]

typeDependencies :: TTermDefinition (Context -> Graph -> Bool -> (Type -> Type) -> Name -> Either (InContext Error) (M.Map Name Type))
typeDependencies = define "typeDependencies" $
  doc "Get all type dependencies for a given type name (Either version)" $
  "cx" ~> "graph" ~> "withSchema" ~> "transform" ~> "name" ~>
  "requireType" <~ ("name" ~>
    "cx1" <~ Ctx.pushTrace (Strings.cat2 (string "type dependencies of ") (Core.unName (var "name"))) (var "cx") $
    Eithers.bind (Lexical.requireElement @@ var "cx1" @@ var "graph" @@ var "name") (
      "el" ~> Ctx.withContext (var "cx1")
        (Eithers.bimap ("_e" ~> Error.errorOther $ Error.otherError (unwrap _DecodingError @@ var "_e")) ("_a" ~> var "_a")
          (decoderFor _Type @@ var "graph" @@ Core.bindingTerm (var "el"))))) $
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
