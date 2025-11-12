{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.Kernel.Terms.Annotations where

-- Standard imports for term-level kernel modules
import Hydra.Kernel
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Accessors     as Accessors
import qualified Hydra.Dsl.Ast           as Ast
import qualified Hydra.Dsl.Coders        as Coders
import qualified Hydra.Dsl.Compute       as Compute
import qualified Hydra.Dsl.Core          as Core
import qualified Hydra.Dsl.Grammar       as Grammar
import qualified Hydra.Dsl.Graph         as Graph
import qualified Hydra.Dsl.Json          as Json
import qualified Hydra.Dsl.Lib.Chars     as Chars
import qualified Hydra.Dsl.Lib.Equality  as Equality
import qualified Hydra.Dsl.Lib.Flows     as Flows
import qualified Hydra.Dsl.Lib.Lists     as Lists
import qualified Hydra.Dsl.Lib.Literals  as Literals
import qualified Hydra.Dsl.Lib.Logic     as Logic
import qualified Hydra.Dsl.Lib.Maps      as Maps
import qualified Hydra.Dsl.Lib.Math      as Math
import qualified Hydra.Dsl.Lib.Maybes    as Maybes
import           Hydra.Dsl.Phantoms      as Phantoms
import qualified Hydra.Dsl.Lib.Sets      as Sets
import           Hydra.Dsl.Lib.Strings   as Strings
import qualified Hydra.Dsl.Mantle        as Mantle
import qualified Hydra.Dsl.Module        as Module
import qualified Hydra.Dsl.TTerms        as TTerms
import qualified Hydra.Dsl.TTypes        as TTypes
import qualified Hydra.Dsl.Terms         as Terms
import qualified Hydra.Dsl.Topology      as Topology
import qualified Hydra.Dsl.Types         as Types
import qualified Hydra.Dsl.Typing        as Typing
import           Hydra.Sources.Kernel.Types.All
import           Prelude hiding ((++))
import qualified Data.Int                as I
import qualified Data.List               as L
import qualified Data.Map                as M
import qualified Data.Set                as S
import qualified Data.Maybe              as Y

import qualified Hydra.Sources.Kernel.Terms.Constants as Constants
import qualified Hydra.Sources.Kernel.Terms.Decode.Core as DecodeCore
import qualified Hydra.Sources.Kernel.Terms.Encode.Core as EncodeCore
import qualified Hydra.Sources.Kernel.Terms.Extract.Core as ExtractCore
import qualified Hydra.Sources.Kernel.Terms.Lexical as Lexical
import qualified Hydra.Sources.Kernel.Terms.Monads as Monads
import qualified Hydra.Sources.Kernel.Terms.Rewriting as Rewriting
import qualified Hydra.Sources.Kernel.Terms.Show.Core as ShowCore
import qualified Hydra.Sources.Kernel.Terms.Variants as Variants


module_ :: Module
module_ = Module (Namespace "hydra.annotations") elements
    [DecodeCore.module_, EncodeCore.module_, ExtractCore.module_, Lexical.module_, ShowCore.module_,
      Variants.module_, Monads.module_]
    kernelTypesModules $
    Just "Utilities for reading and writing type and term annotations"
  where
   elements = [
     el aggregateAnnotationsDef,
     el debugIfDef,
     el failOnFlagDef,
     el getDebugIdDef,
     el getAttrDef,
     el getAttrWithDefaultDef,
     el getCountDef,
     el getDescriptionDef,
     el getTermAnnotationDef,
     el getTermDescriptionDef,
     el getTypeDef,
     el getTypeAnnotationDef,
     el getTypeClassesDef,
     el getTypeDescriptionDef,
     el isNativeTypeDef,
     el hasDescriptionDef,
     el hasFlagDef,
     el hasTypeDescriptionDef,
     el nextCountDef,
     el normalizeTermAnnotationsDef,
     el normalizeTypeAnnotationsDef,
     el putAttrDef,
     el putCountDef,
     el resetCountDef,
     el setAnnotationDef,
     el setDescriptionDef,
     el setTermAnnotationDef,
     el setTermDescriptionDef,
     el setTypeDef,
     el setTypeAnnotationDef,
     el setTypeClassesDef,
     el setTypeDescriptionDef,
     el termAnnotationInternalDef,
     el typeAnnotationInternalDef,
     el typeElementDef,
     el whenFlagDef,
     el withDepthDef]

define :: String -> TTerm a -> TBinding a
define = definitionInModule module_

aggregateAnnotationsDef :: TBinding ((x -> Maybe y) -> (y -> x) -> (y -> M.Map Name Term) -> x -> M.Map Name Term)
aggregateAnnotationsDef = define "aggregateAnnotations" $
  doc "Aggregate annotations from nested structures" $
  "getValue" ~> "getX" ~> "getAnns" ~> "t" ~>
  "toPairs" <~ ("rest" ~> "t" ~> Maybes.maybe (var "rest")
    (lambda "yy" (var "toPairs"
      @@ Lists.cons (Maps.toList (var "getAnns" @@ var "yy")) (var "rest")
      @@ (var "getX" @@ var "yy")))
    (var "getValue" @@ var "t")) $
  Maps.fromList (Lists.concat (var "toPairs" @@ list [] @@ var "t"))

debugIfDef :: TBinding (String -> String -> Flow s ())
debugIfDef = define "debugIf" $
  doc "Debug if the debug ID matches" $
  "debugId" ~> "message" ~>
  "checkAndFail" <~ ("desc" ~> Logic.ifElse
    (Equality.equal (var "desc") (just $ string "debugId"))
    (Flows.fail (var "message"))
    (produce unit)) $
  Flows.bind (ref getDebugIdDef) (var "checkAndFail")

failOnFlagDef :: TBinding (Name -> String -> Flow s ())
failOnFlagDef = define "failOnFlag" $
  doc "Fail if the given flag is set" $
  "flag" ~> "msg" ~>
  "val" <<~ ref hasFlagDef @@ var "flag" $
  Logic.ifElse (var "val")
    (Flows.fail (var "msg"))
    (produce unit)

getAttrDef :: TBinding (Name -> Flow s (Maybe Term))
getAttrDef = define "getAttr" $
  doc "Get an attribute from the trace" $
  "key" ~> Compute.flow (
    "s0" ~> "t0" ~> Compute.flowState
      (just (Maps.lookup (var "key") (Compute.traceOther (var "t0"))))
      (var "s0")
      (var "t0"))

getAttrWithDefaultDef :: TBinding (Name -> Term -> Flow s Term)
getAttrWithDefaultDef = define "getAttrWithDefault" $
  doc "Get an attribute with a default value" $
  "key" ~> "def" ~> Flows.map
    ("mval" ~> Maybes.fromMaybe (var "def") (var "mval"))
    (ref getAttrDef @@ var "key")

getCountDef :: TBinding (Name -> Flow s Int)
getCountDef = define "getCount" $
  doc "Get a counter value" $
  "key" ~> ref Lexical.withEmptyGraphDef @@ (Flows.bind
    (ref getAttrWithDefaultDef @@ var "key" @@ (Core.int32 0))
    (ref ExtractCore.int32Def))

getDebugIdDef :: TBinding (Flow s (Maybe String))
getDebugIdDef = define "getDebugId" $
  doc "Get the debug ID from flow state" $
  ref Lexical.withEmptyGraphDef @@ (Flows.bind
    (ref getAttrDef @@ ref Constants.key_debugIdDef)
    ("desc" ~> Flows.mapMaybe (ref ExtractCore.stringDef) (var "desc")))

getDescriptionDef :: TBinding (M.Map Name Term -> Flow Graph (Maybe String))
getDescriptionDef = define "getDescription" $
  doc "Get description from annotations map" $
  "anns" ~> Maybes.maybe
    (produce nothing)
    ("term" ~> Flows.map (unaryFunction just) (ref ExtractCore.stringDef @@ var "term"))
    (Maps.lookup (Core.nameLift key_description) (var "anns"))

getTermAnnotationDef :: TBinding (Name -> Term -> Maybe Term)
getTermAnnotationDef = define "getTermAnnotation" $
  doc "Get a term annotation" $
  "key" ~> "term" ~> Maps.lookup (var "key") (ref termAnnotationInternalDef @@ var "term")

getTermDescriptionDef :: TBinding (Term -> Flow Graph (Maybe String))
getTermDescriptionDef = define "getTermDescription" $
  doc "Get term description" $
  "term" ~> ref getDescriptionDef @@ (ref termAnnotationInternalDef @@ var "term")

getTypeDef :: TBinding (M.Map Name Term -> Flow Graph (Maybe Type))
getTypeDef = define "getType" $
  doc "Get type from annotations" $
  "anns" ~> Maybes.maybe
    (produce nothing)
    ("dat" ~> Flows.map (unaryFunction just) (trace "get type" $ ref DecodeCore.typeDef @@ var "dat"))
    (Maps.lookup (ref Constants.key_typeDef) (var "anns"))

getTypeAnnotationDef :: TBinding (Name -> Type -> Maybe Term)
getTypeAnnotationDef = define "getTypeAnnotation" $
  doc "Get a type annotation" $
  "key" ~> "typ" ~> Maps.lookup (var "key") (ref typeAnnotationInternalDef @@ var "typ")

getTypeClassesDef :: TBinding (Term -> Flow Graph (M.Map Name (S.Set TypeClass)))
getTypeClassesDef = define "getTypeClasses" $
  doc "Get type classes from term" $
  "term" ~>
  "decodeClass" <~ ("term" ~>
    "byName" <~ Maps.fromList (list [
      tuple2 (Core.nameLift _TypeClass_equality) Graph.typeClassEquality,
      tuple2 (Core.nameLift _TypeClass_ordering) Graph.typeClassOrdering]) $
    "fn" <<~ ref ExtractCore.unitVariantDef @@ Core.nameLift _TypeClass @@ var "term" $
    Maybes.maybe
      (ref Monads.unexpectedDef @@ string "type class" @@ (ref ShowCore.termDef @@ var "term"))
      (unaryFunction produce)
      (Maps.lookup (var "fn") (var "byName"))) $
  Maybes.maybe
    (produce Maps.empty)
    ("term" ~> ref ExtractCore.mapDef
      @@ (ref DecodeCore.nameDef)
      @@ (ref ExtractCore.setOfDef @@ var "decodeClass")
      @@ (var "term"))
    (ref getTermAnnotationDef @@ ref Constants.key_classesDef @@ var "term")

getTypeDescriptionDef :: TBinding (Type -> Flow Graph (Maybe String))
getTypeDescriptionDef = define "getTypeDescription" $
  doc "Get type description" $
  "typ" ~> ref getDescriptionDef @@ (ref typeAnnotationInternalDef @@ var "typ")

isNativeTypeDef :: TBinding (Binding -> Bool)
isNativeTypeDef = define "isNativeType" $
  doc ("For a typed term, decide whether a coder should encode it as a native type expression,"
    <> " or as a Hydra type expression.") $
  "el" ~>
  "isFlaggedAsFirstClassType" <~ Maybes.fromMaybe false (
    Maybes.map
      (constant true)
      (ref getTermAnnotationDef @@ ref Constants.key_firstClassTypeDef @@ (Core.bindingTerm (var "el")))) $
  Maybes.maybe false
    ("ts" ~> Logic.and
      (Equality.equal (var "ts") (Core.typeScheme (list []) (Core.typeVariable (Core.nameLift _Type))))
      (Logic.not (var "isFlaggedAsFirstClassType")))
    (Core.bindingType (var "el"))

hasDescriptionDef :: TBinding (M.Map Name Term -> Bool)
hasDescriptionDef = define "hasDescription" $
  doc "Check if annotations contain description" $
  "anns" ~> Maybes.isJust (Maps.lookup (ref Constants.key_descriptionDef) (var "anns"))

hasFlagDef :: TBinding (Name -> Flow s Bool)
hasFlagDef = define "hasFlag" $
  doc "Check if flag is set" $
  "flag" ~> ref Lexical.withEmptyGraphDef
    @@ ("term" <<~ ref getAttrWithDefaultDef @@ var "flag" @@ Core.false $
        ref ExtractCore.booleanDef @@ var "term")

hasTypeDescriptionDef :: TBinding (Type -> Bool)
hasTypeDescriptionDef = define "hasTypeDescription" $
  doc "Check if type has description" $
  "typ" ~> ref hasDescriptionDef @@ (ref typeAnnotationInternalDef @@ var "typ")

nextCountDef :: TBinding (Name -> Flow s Int)
nextCountDef = define "nextCount" $
  doc "Return a zero-indexed counter for the given key: 0, 1, 2, ..." $
  "key" ~>
  "count" <<~ ref getCountDef @@ var "key" $
  Flows.map
    (constant (var "count"))
    (ref putCountDef @@ var "key" @@ Math.add (var "count") (int32 1))

normalizeTermAnnotationsDef :: TBinding (Term -> Term)
normalizeTermAnnotationsDef = define "normalizeTermAnnotations" $
  doc "Normalize term annotations" $
  "term" ~>
  "anns" <~ ref termAnnotationInternalDef @@ var "term" $
  "stripped" <~ ref Rewriting.deannotateTermDef @@ var "term" $
  Logic.ifElse (Maps.null (var "anns"))
    (var "stripped")
    (Core.termAnnotated (Core.annotatedTerm (var "stripped") (var "anns")))

normalizeTypeAnnotationsDef :: TBinding (Type -> Type)
normalizeTypeAnnotationsDef = define "normalizeTypeAnnotations" $
  doc "Normalize type annotations" $
  "typ" ~>
  "anns" <~ ref typeAnnotationInternalDef @@ var "typ" $
  "stripped" <~ ref Rewriting.deannotateTypeDef @@ var "typ" $
  Logic.ifElse (Maps.null (var "anns"))
    (var "stripped")
    (Core.typeAnnotated (Core.annotatedType (var "stripped") (var "anns")))

putAttrDef :: TBinding (Name -> Term -> Flow s ())
putAttrDef = define "putAttr" $
  doc "Set an attribute in the trace" $
  "key" ~> "val" ~> Compute.flow (
    "s0" ~> "t0" ~>
    Compute.flowState
      (just unit)
      (var "s0")
      (Compute.traceWithOther (var "t0") (Maps.insert (var "key") (var "val") (Compute.traceOther (var "t0")))))

putCountDef :: TBinding (Name -> Int -> Flow s ())
putCountDef = define "putCount" $
  doc "Set counter value" $
  "key" ~> "count" ~>
  ref putAttrDef @@ var "key" @@ (Core.termLiteral (Core.literalInteger (Core.integerValueInt32 (var "count"))))

resetCountDef :: TBinding (Name -> Flow s ())
resetCountDef = define "resetCount" $
  doc "Reset counter to zero" $
  "key" ~> ref putAttrDef @@ var "key" @@ TTerms.int32 0

setAnnotationDef :: TBinding (Name -> Maybe Term -> M.Map Name Term -> M.Map Name Term)
setAnnotationDef = define "setAnnotation" $
  doc "Set annotation in map" $
  "key" ~> "val" ~> "m" ~> Maps.alter (constant (var "val")) (var "key") (var "m")

setDescriptionDef :: TBinding (Maybe String -> M.Map Name Term -> M.Map Name Term)
setDescriptionDef = define "setDescription" $
  doc "Set description in annotations" $
  "d" ~> ref setAnnotationDef
    @@ ref Constants.key_descriptionDef
    @@ Maybes.map (unaryFunction Core.termLiteral <.> unaryFunction Core.literalString) (var "d")

setTermAnnotationDef :: TBinding (Name -> Maybe Term -> Term -> Term)
setTermAnnotationDef = define "setTermAnnotation" $
  doc "Set term annotation" $
  "key" ~> "val" ~> "term" ~>
  "term'" <~ ref Rewriting.deannotateTermDef @@ var "term" $
  "anns" <~ ref setAnnotationDef @@ var "key" @@ var "val" @@ (ref termAnnotationInternalDef @@ var "term") $
  Logic.ifElse (Maps.null (var "anns"))
    (var "term'")
    (Core.termAnnotated (Core.annotatedTerm (var "term'") (var "anns")))

setTermDescriptionDef :: TBinding (Maybe String -> Term -> Term)
setTermDescriptionDef = define "setTermDescription" $
  doc "Set term description" $
  "d" ~> ref setTermAnnotationDef
    @@ ref Constants.key_descriptionDef
    @@ Maybes.map ("s" ~> Core.termLiteral (Core.literalString (var "s"))) (var "d")

setTypeDef :: TBinding (Maybe Type -> M.Map Name Term -> M.Map Name Term)
setTypeDef = define "setType" $
  doc "Set type in annotations" $
  "mt" ~> ref setAnnotationDef @@ ref Constants.key_typeDef @@ Maybes.map (ref EncodeCore.typeDef) (var "mt")

setTypeAnnotationDef :: TBinding (Name -> Maybe Term -> Type -> Type)
setTypeAnnotationDef = define "setTypeAnnotation" $
  doc "Set type annotation" $
  "key" ~> "val" ~> "typ" ~>
  "typ'" <~ ref Rewriting.deannotateTypeDef @@ var "typ" $
  "anns" <~ ref setAnnotationDef @@ var "key" @@ var "val" @@ (ref typeAnnotationInternalDef @@ var "typ") $
  Logic.ifElse (Maps.null (var "anns"))
    (var "typ'")
    (Core.typeAnnotated (Core.annotatedType (var "typ'") (var "anns")))

setTypeClassesDef :: TBinding (M.Map Name (S.Set TypeClass) -> Term -> Term)
setTypeClassesDef = define "setTypeClasses" $
  doc "Set type classes on term" $
  "m" ~> "term" ~>
  "encodeClass" <~ ("tc" ~> cases _TypeClass (var "tc")
    Nothing [
    _TypeClass_equality>>: constant (TTerms.unitVariantPhantom _TypeClass _TypeClass_equality),
    _TypeClass_ordering>>: constant (TTerms.unitVariantPhantom _TypeClass _TypeClass_ordering)]) $
  "encodePair" <~ ("nameClasses" ~>
    "name" <~ first (var "nameClasses") $
    "classes" <~ second (var "nameClasses") $
    tuple2
      (ref EncodeCore.nameDef @@ var "name")
      (Core.termSet (Sets.fromList (Lists.map (var "encodeClass") (Sets.toList (var "classes")))))) $
  "encoded" <~ Logic.ifElse (Maps.null (var "m"))
    nothing
    (just (Core.termMap (Maps.fromList (Lists.map (var "encodePair") (Maps.toList (var "m")))))) $
  ref setTermAnnotationDef @@ ref Constants.key_classesDef @@ var "encoded" @@ var "term"

setTypeDescriptionDef :: TBinding (Maybe String -> Type -> Type)
setTypeDescriptionDef = define "setTypeDescription" $
  doc "Set type description" $
  "d" ~> ref setTypeAnnotationDef
    @@ ref Constants.key_descriptionDef
    @@ Maybes.map (unaryFunction Core.termLiteral <.> unaryFunction Core.literalString) (var "d")

termAnnotationInternalDef :: TBinding (Term -> M.Map Name Term)
termAnnotationInternalDef = define "termAnnotationInternal" $
  doc "Get internal term annotations" $
  "term" ~>
  "getAnn" <~ ("t" ~> cases _Term (var "t")
    (Just nothing) [
    _Term_annotated>>: "a" ~> just $ var "a"]) $
  ref aggregateAnnotationsDef
    @@ var "getAnn"
    @@ ("at" ~> Core.annotatedTermBody $ var "at")
    @@ ("at" ~> Core.annotatedTermAnnotation $ var "at")
    @@ var "term"

typeAnnotationInternalDef :: TBinding (Type -> M.Map Name Term)
typeAnnotationInternalDef = define "typeAnnotationInternal" $
  doc "Get internal type annotations" $
  "typ" ~>
  "getAnn" <~ lambda "t" (cases _Type (var "t")
    (Just nothing) [
    _Type_annotated>>: lambda "a" (just $ var "a")]) $
  ref aggregateAnnotationsDef
    @@ var "getAnn"
    @@ ("at" ~> Core.annotatedTypeBody $ var "at")
    @@ ("at" ~> Core.annotatedTypeAnnotation $ var "at")
    @@ var "typ"

-- TODO: deprecate
typeElementDef :: TBinding (Name -> Type -> Binding)
typeElementDef = define "typeElement" $
  doc "Create a type element with proper annotations" $
  "name" ~> "typ" ~>
  "schemaTerm" <~ Core.termVariable (Core.nameLift _Type) $
  "dataTerm" <~ ref normalizeTermAnnotationsDef @@ (Core.termAnnotated (Core.annotatedTerm
    (ref EncodeCore.typeDef @@ var "typ")
    (Maps.fromList (list [tuple2 (ref Constants.key_typeDef) (var "schemaTerm")])))) $
  Core.binding (var "name") (var "dataTerm") (just (Core.typeScheme (list []) (Core.typeVariable $ Core.nameLift _Type)))

whenFlagDef :: TBinding (Name -> Flow s a -> Flow s a -> Flow s a)
whenFlagDef = define "whenFlag" $
  doc "Execute different flows based on flag" $
  "flag" ~> "fthen" ~> "felse" ~>
  "b" <<~ ref hasFlagDef @@ var "flag" $
  Logic.ifElse (var "b") (var "fthen") (var "felse")

withDepthDef :: TBinding (Name -> (Int -> Flow s a) -> Flow s a)
withDepthDef = define "withDepth" $
  doc ("Provide an one-indexed, integer-valued 'depth' to a flow, where the depth is the number of nested calls."
    <> " This is useful for generating variable names while avoiding conflicts between the variables of parents and children."
    <> " E.g. a variable in an outer case/match statement might be \"v1\", whereas the variable of another case/match statement"
    <> " inside of the first one becomes \"v2\". See also nextCount.") $
  "key" ~> "f" ~>
  "count" <<~ ref getCountDef @@ var "key" $
  "inc" <~ Math.add (var "count") (int32 1) $
  exec (ref putCountDef @@ var "key" @@ var "inc") $
  "r" <<~ var "f" @@ var "inc" $
  exec (ref putCountDef @@ var "key" @@ var "count") $
  produce (var "r")
