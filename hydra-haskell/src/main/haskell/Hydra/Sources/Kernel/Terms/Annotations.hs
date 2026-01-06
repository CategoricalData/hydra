
module Hydra.Sources.Kernel.Terms.Annotations where

-- Standard imports for kernel terms modules
import Hydra.Kernel hiding (
  aggregateAnnotations, debugIf, failOnFlag, getDebugId, getAttr, getAttrWithDefault, getCount,
  getDescription, getTermAnnotation, getTermDescription, getType, getTypeAnnotation, getTypeClasses,
  getTypeDescription, isNativeType, hasDescription, hasFlag, hasTypeDescription, nextCount,
  normalizeTermAnnotations, normalizeTypeAnnotations, putAttr, putCount, resetCount, setAnnotation,
  setDescription, setTermAnnotation, setTermDescription, setType, setTypeAnnotation, setTypeClasses,
  setTypeDescription, termAnnotationInternal, typeAnnotationInternal, typeElement, whenFlag, withDepth)
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
import           Hydra.Dsl.Meta.Phantoms as Phantoms
import qualified Hydra.Dsl.Prims         as Prims
import qualified Hydra.Dsl.Tabular       as Tabular
import qualified Hydra.Dsl.Meta.Testing       as Testing
import qualified Hydra.Dsl.Terms         as Terms
import qualified Hydra.Dsl.Meta.Testing       as Testing
import qualified Hydra.Dsl.Tests         as Tests
import qualified Hydra.Dsl.Meta.Topology      as Topology
import qualified Hydra.Dsl.Types         as Types
import qualified Hydra.Dsl.Meta.Typing        as Typing
import qualified Hydra.Dsl.Meta.Util          as Util
import qualified Hydra.Dsl.Meta.Variants      as Variants
import           Hydra.Sources.Kernel.Types.All
import           Prelude hiding ((++))
import qualified Data.Int                as I
import qualified Data.List               as L
import qualified Data.Map                as M
import qualified Data.Set                as S
import qualified Data.Maybe              as Y

import qualified Hydra.Sources.Kernel.Terms.Constants    as Constants
import qualified Hydra.Sources.Kernel.Terms.Extract.Core as ExtractCore
import qualified Hydra.Sources.Kernel.Terms.Lexical      as Lexical
import qualified Hydra.Sources.Kernel.Terms.Monads       as Monads
import qualified Hydra.Sources.Kernel.Terms.Rewriting    as Rewriting
import qualified Hydra.Sources.Kernel.Terms.Show.Core    as ShowCore
import qualified Hydra.Sources.Encode.Core            as EncodeCore
import Hydra.Encoding (encodeBindingName)


ns :: Namespace
ns = Namespace "hydra.annotations"

module_ :: Module
module_ = Module ns elements
    [Constants.ns, moduleNamespace EncodeCore.module_, ExtractCore.ns, Lexical.ns, Monads.ns,
      Rewriting.ns, ShowCore.ns]
    kernelTypesNamespaces $
    Just "Utilities for reading and writing type and term annotations"
  where
   elements = [
     toBinding aggregateAnnotations,
     toBinding debugIf,
     toBinding failOnFlag,
     toBinding getDebugId,
     toBinding getAttr,
     toBinding getAttrWithDefault,
     toBinding getCount,
     toBinding getDescription,
     toBinding getTermAnnotation,
     toBinding getTermDescription,
     toBinding getType,
     toBinding getTypeAnnotation,
     toBinding getTypeClasses,
     toBinding getTypeDescription,
     toBinding isNativeType,
     toBinding hasDescription,
     toBinding hasFlag,
     toBinding hasTypeDescription,
     toBinding nextCount,
     toBinding normalizeTermAnnotations,
     toBinding normalizeTypeAnnotations,
     toBinding putAttr,
     toBinding putCount,
     toBinding resetCount,
     toBinding setAnnotation,
     toBinding setDescription,
     toBinding setTermAnnotation,
     toBinding setTermDescription,
     toBinding setType,
     toBinding setTypeAnnotation,
     toBinding setTypeClasses,
     toBinding setTypeDescription,
     toBinding termAnnotationInternal,
     toBinding typeAnnotationInternal,
     toBinding typeElement,
     toBinding whenFlag,
     toBinding withDepth]

define :: String -> TTerm a -> TBinding a
define = definitionInModule module_

aggregateAnnotations :: TBinding ((x -> Maybe y) -> (y -> x) -> (y -> M.Map Name Term) -> x -> M.Map Name Term)
aggregateAnnotations = define "aggregateAnnotations" $
  doc "Aggregate annotations from nested structures" $
  "getValue" ~> "getX" ~> "getAnns" ~> "t" ~>
  "toPairs" <~ ("rest" ~> "t" ~> Maybes.maybe (var "rest")
    (lambda "yy" (var "toPairs"
      @@ Lists.cons (Maps.toList (var "getAnns" @@ var "yy")) (var "rest")
      @@ (var "getX" @@ var "yy")))
    (var "getValue" @@ var "t")) $
  Maps.fromList (Lists.concat (var "toPairs" @@ list ([] :: [TTerm [(Name, Term)]]) @@ var "t"))

debugIf :: TBinding (String -> String -> Flow s ())
debugIf = define "debugIf" $
  doc "Debug if the debug ID matches" $
  "debugId" ~> "message" ~>
  "checkAndFail" <~ ("desc" ~> Logic.ifElse
    (Equality.equal (var "desc") (just $ string "debugId"))
    (Flows.fail (var "message"))
    (produce unit)) $
  Flows.bind getDebugId (var "checkAndFail")

failOnFlag :: TBinding (Name -> String -> Flow s ())
failOnFlag = define "failOnFlag" $
  doc "Fail if the given flag is set" $
  "flag" ~> "msg" ~>
  "val" <<~ hasFlag @@ var "flag" $
  Logic.ifElse (var "val")
    (Flows.fail (var "msg"))
    (produce unit)

getAttr :: TBinding (Name -> Flow s (Maybe Term))
getAttr = define "getAttr" $
  doc "Get an attribute from the trace" $
  "key" ~> Compute.flow (
    "s0" ~> "t0" ~> Compute.flowState
      (just (Maps.lookup (var "key") (Compute.traceOther (var "t0"))))
      (var "s0")
      (var "t0"))

getAttrWithDefault :: TBinding (Name -> Term -> Flow s Term)
getAttrWithDefault = define "getAttrWithDefault" $
  doc "Get an attribute with a default value" $
  "key" ~> "def" ~> Flows.map
    ("mval" ~> Maybes.fromMaybe (var "def") (var "mval"))
    (getAttr @@ var "key")

getCount :: TBinding (Name -> Flow s Int)
getCount = define "getCount" $
  doc "Get a counter value" $
  "key" ~> Lexical.withEmptyGraph @@ (Flows.bind
    (getAttrWithDefault @@ var "key" @@ (Core.int32 0))
    (ExtractCore.int32))

getDebugId :: TBinding (Flow s (Maybe String))
getDebugId = define "getDebugId" $
  doc "Get the debug ID from flow state" $
  Lexical.withEmptyGraph @@ (Flows.bind
    (getAttr @@ Constants.key_debugId)
    ("desc" ~> Flows.mapMaybe (ExtractCore.string) (var "desc")))

getDescription :: TBinding (M.Map Name Term -> Flow Graph (Maybe String))
getDescription = define "getDescription" $
  doc "Get description from annotations map" $
  "anns" ~> Maybes.maybe
    (produce nothing)
    ("term" ~> Flows.map (unaryFunction just) (ExtractCore.string @@ var "term"))
    (Maps.lookup (Core.nameLift key_description) (var "anns"))

getTermAnnotation :: TBinding (Name -> Term -> Maybe Term)
getTermAnnotation = define "getTermAnnotation" $
  doc "Get a term annotation" $
  "key" ~> "term" ~> Maps.lookup (var "key") (termAnnotationInternal @@ var "term")

getTermDescription :: TBinding (Term -> Flow Graph (Maybe String))
getTermDescription = define "getTermDescription" $
  doc "Get term description" $
  "term" ~> getDescription @@ (termAnnotationInternal @@ var "term")

getType :: TBinding (M.Map Name Term -> Flow Graph (Maybe Type))
getType = define "getType" $
  doc "Get type from annotations" $
  "anns" ~>
  "cx" <<~ Monads.getState $
  Maybes.maybe
    (produce nothing)
    ("dat" ~> Flows.map (unaryFunction just) (trace (string "get type") $ Monads.eitherToFlow_ @@ Util.unDecodingError @@ (decoderFor _Type @@ var "cx" @@ var "dat")))
    (Maps.lookup (Constants.key_type) (var "anns"))

getTypeAnnotation :: TBinding (Name -> Type -> Maybe Term)
getTypeAnnotation = define "getTypeAnnotation" $
  doc "Get a type annotation" $
  "key" ~> "typ" ~> Maps.lookup (var "key") (typeAnnotationInternal @@ var "typ")

getTypeClasses :: TBinding (Term -> Flow Graph (M.Map Name (S.Set TypeClass)))
getTypeClasses = define "getTypeClasses" $
  doc "Get type classes from term" $
  "term" ~>
  "cx" <<~ Monads.getState $
  "decodeClass" <~ ("term" ~>
    "byName" <~ Maps.fromList (list [
      pair (Core.nameLift _TypeClass_equality) Graph.typeClassEquality,
      pair (Core.nameLift _TypeClass_ordering) Graph.typeClassOrdering]) $
    "fn" <<~ ExtractCore.unitVariant @@ Core.nameLift _TypeClass @@ var "term" $
    Maybes.maybe
      (Monads.unexpected @@ string "type class" @@ (ShowCore.term @@ var "term"))
      (unaryFunction produce)
      (Maps.lookup (var "fn") (var "byName"))) $
  Maybes.maybe
    (produce Maps.empty)
    ("term" ~> ExtractCore.map
      @@ ("t" ~> Monads.eitherToFlow_ @@ Util.unDecodingError @@ (decoderFor _Name @@ var "cx" @@ var "t"))
      @@ (ExtractCore.setOf @@ var "decodeClass")
      @@ (var "term"))
    (getTermAnnotation @@ Constants.key_classes @@ var "term")

getTypeDescription :: TBinding (Type -> Flow Graph (Maybe String))
getTypeDescription = define "getTypeDescription" $
  doc "Get type description" $
  "typ" ~> getDescription @@ (typeAnnotationInternal @@ var "typ")

isNativeType :: TBinding (Binding -> Bool)
isNativeType = define "isNativeType" $
  doc ("For a typed term, decide whether a coder should encode it as a native type expression,"
    <> " or as a Hydra type expression.") $
  "el" ~>
  "isFlaggedAsFirstClassType" <~ Maybes.fromMaybe false (
    Maybes.map
      (constant true)
      (getTermAnnotation @@ Constants.key_firstClassType @@ (Core.bindingTerm (var "el")))) $
  Maybes.maybe false
    ("ts" ~> Logic.and
      (Equality.equal (var "ts") (Core.typeScheme (list ([] :: [TTerm Name])) (Core.typeVariable (Core.nameLift _Type)) Phantoms.nothing))
      (Logic.not (var "isFlaggedAsFirstClassType")))
    (Core.bindingType (var "el"))

hasDescription :: TBinding (M.Map Name Term -> Bool)
hasDescription = define "hasDescription" $
  doc "Check if annotations contain description" $
  "anns" ~> Maybes.isJust (Maps.lookup (Constants.key_description) (var "anns"))

hasFlag :: TBinding (Name -> Flow s Bool)
hasFlag = define "hasFlag" $
  doc "Check if flag is set" $
  "flag" ~> Lexical.withEmptyGraph
    @@ ("term" <<~ getAttrWithDefault @@ var "flag" @@ Core.false $
        ExtractCore.boolean @@ var "term")

hasTypeDescription :: TBinding (Type -> Bool)
hasTypeDescription = define "hasTypeDescription" $
  doc "Check if type has description" $
  "typ" ~> hasDescription @@ (typeAnnotationInternal @@ var "typ")

nextCount :: TBinding (Name -> Flow s Int)
nextCount = define "nextCount" $
  doc "Return a zero-indexed counter for the given key: 0, 1, 2, ..." $
  "key" ~>
  "count" <<~ getCount @@ var "key" $
  Flows.map
    (constant (var "count"))
    (putCount @@ var "key" @@ Math.add (var "count") (int32 1))

normalizeTermAnnotations :: TBinding (Term -> Term)
normalizeTermAnnotations = define "normalizeTermAnnotations" $
  doc "Normalize term annotations" $
  "term" ~>
  "anns" <~ termAnnotationInternal @@ var "term" $
  "stripped" <~ Rewriting.deannotateTerm @@ var "term" $
  Logic.ifElse (Maps.null (var "anns"))
    (var "stripped")
    (Core.termAnnotated (Core.annotatedTerm (var "stripped") (var "anns")))

normalizeTypeAnnotations :: TBinding (Type -> Type)
normalizeTypeAnnotations = define "normalizeTypeAnnotations" $
  doc "Normalize type annotations" $
  "typ" ~>
  "anns" <~ typeAnnotationInternal @@ var "typ" $
  "stripped" <~ Rewriting.deannotateType @@ var "typ" $
  Logic.ifElse (Maps.null (var "anns"))
    (var "stripped")
    (Core.typeAnnotated (Core.annotatedType (var "stripped") (var "anns")))

putAttr :: TBinding (Name -> Term -> Flow s ())
putAttr = define "putAttr" $
  doc "Set an attribute in the trace" $
  "key" ~> "val" ~> Compute.flow (
    "s0" ~> "t0" ~>
    Compute.flowState
      (just unit)
      (var "s0")
      (Compute.traceWithOther (var "t0") (Maps.insert (var "key") (var "val") (Compute.traceOther (var "t0")))))

putCount :: TBinding (Name -> Int -> Flow s ())
putCount = define "putCount" $
  doc "Set counter value" $
  "key" ~> "count" ~>
  putAttr @@ var "key" @@ (Core.termLiteral (Core.literalInteger (Core.integerValueInt32 (var "count"))))

resetCount :: TBinding (Name -> Flow s ())
resetCount = define "resetCount" $
  doc "Reset counter to zero" $
  "key" ~> putAttr @@ var "key" @@ MetaTerms.int32 0

setAnnotation :: TBinding (Name -> Maybe Term -> M.Map Name Term -> M.Map Name Term)
setAnnotation = define "setAnnotation" $
  doc "Set annotation in map" $
  "key" ~> "val" ~> "m" ~> Maps.alter (constant (var "val")) (var "key") (var "m")

setDescription :: TBinding (Maybe String -> M.Map Name Term -> M.Map Name Term)
setDescription = define "setDescription" $
  doc "Set description in annotations" $
  "d" ~> setAnnotation
    @@ Constants.key_description
    @@ Maybes.map (unaryFunction Core.termLiteral <.> unaryFunction Core.literalString) (var "d")

setTermAnnotation :: TBinding (Name -> Maybe Term -> Term -> Term)
setTermAnnotation = define "setTermAnnotation" $
  doc "Set term annotation" $
  "key" ~> "val" ~> "term" ~>
  "term'" <~ Rewriting.deannotateTerm @@ var "term" $
  "anns" <~ setAnnotation @@ var "key" @@ var "val" @@ (termAnnotationInternal @@ var "term") $
  Logic.ifElse (Maps.null (var "anns"))
    (var "term'")
    (Core.termAnnotated (Core.annotatedTerm (var "term'") (var "anns")))

setTermDescription :: TBinding (Maybe String -> Term -> Term)
setTermDescription = define "setTermDescription" $
  doc "Set term description" $
  "d" ~> setTermAnnotation
    @@ Constants.key_description
    @@ Maybes.map ("s" ~> Core.termLiteral (Core.literalString (var "s"))) (var "d")

setType :: TBinding (Maybe Type -> M.Map Name Term -> M.Map Name Term)
setType = define "setType" $
  doc "Set type in annotations" $
  "mt" ~> setAnnotation @@ Constants.key_type @@ Maybes.map (encoderFor _Type) (var "mt")

setTypeAnnotation :: TBinding (Name -> Maybe Term -> Type -> Type)
setTypeAnnotation = define "setTypeAnnotation" $
  doc "Set type annotation" $
  "key" ~> "val" ~> "typ" ~>
  "typ'" <~ Rewriting.deannotateType @@ var "typ" $
  "anns" <~ setAnnotation @@ var "key" @@ var "val" @@ (typeAnnotationInternal @@ var "typ") $
  Logic.ifElse (Maps.null (var "anns"))
    (var "typ'")
    (Core.typeAnnotated (Core.annotatedType (var "typ'") (var "anns")))

setTypeClasses :: TBinding (M.Map Name (S.Set TypeClass) -> Term -> Term)
setTypeClasses = define "setTypeClasses" $
  doc "Set type classes on term" $
  "m" ~> "term" ~>
  "encodeClass" <~ ("tc" ~> cases _TypeClass (var "tc")
    Nothing [
    _TypeClass_equality>>: constant (MetaTerms.injectUnitPhantom _TypeClass _TypeClass_equality),
    _TypeClass_ordering>>: constant (MetaTerms.injectUnitPhantom _TypeClass _TypeClass_ordering)]) $
  "encodePair" <~ ("nameClasses" ~>
    "name" <~ Pairs.first (var "nameClasses") $
    "classes" <~ Pairs.second (var "nameClasses") $
    pair
      (encoderFor _Name @@ var "name")
      (Core.termSet (Sets.fromList (Lists.map (var "encodeClass") (Sets.toList (var "classes")))))) $
  "encoded" <~ Logic.ifElse (Maps.null (var "m"))
    nothing
    (just (Core.termMap (Maps.fromList (Lists.map (var "encodePair") (Maps.toList (var "m")))))) $
  setTermAnnotation @@ Constants.key_classes @@ var "encoded" @@ var "term"

setTypeDescription :: TBinding (Maybe String -> Type -> Type)
setTypeDescription = define "setTypeDescription" $
  doc "Set type description" $
  "d" ~> setTypeAnnotation
    @@ Constants.key_description
    @@ Maybes.map (unaryFunction Core.termLiteral <.> unaryFunction Core.literalString) (var "d")

termAnnotationInternal :: TBinding (Term -> M.Map Name Term)
termAnnotationInternal = define "termAnnotationInternal" $
  doc "Get internal term annotations" $
  "term" ~>
  "getAnn" <~ ("t" ~> cases _Term (var "t")
    (Just nothing) [
    _Term_annotated>>: "a" ~> just $ var "a"]) $
  aggregateAnnotations
    @@ var "getAnn"
    @@ ("at" ~> Core.annotatedTermBody $ var "at")
    @@ ("at" ~> Core.annotatedTermAnnotation $ var "at")
    @@ var "term"

typeAnnotationInternal :: TBinding (Type -> M.Map Name Term)
typeAnnotationInternal = define "typeAnnotationInternal" $
  doc "Get internal type annotations" $
  "typ" ~>
  "getAnn" <~ lambda "t" (cases _Type (var "t")
    (Just nothing) [
    _Type_annotated>>: lambda "a" (just $ var "a")]) $
  aggregateAnnotations
    @@ var "getAnn"
    @@ ("at" ~> Core.annotatedTypeBody $ var "at")
    @@ ("at" ~> Core.annotatedTypeAnnotation $ var "at")
    @@ var "typ"

-- TODO: deprecate
typeElement :: TBinding (Name -> Type -> Binding)
typeElement = define "typeElement" $
  doc "Create a type element with proper annotations" $
  "name" ~> "typ" ~>
  "schemaTerm" <~ Core.termVariable (Core.nameLift _Type) $
  "dataTerm" <~ normalizeTermAnnotations @@ (Core.termAnnotated (Core.annotatedTerm
    (encoderFor _Type @@ var "typ")
    (Maps.fromList (list [pair (Constants.key_type) (var "schemaTerm")])))) $
  Core.binding (var "name") (var "dataTerm") (just (Core.typeScheme (list ([] :: [TTerm Name])) (Core.typeVariable $ Core.nameLift _Type) Phantoms.nothing))

whenFlag :: TBinding (Name -> Flow s a -> Flow s a -> Flow s a)
whenFlag = define "whenFlag" $
  doc "Execute different flows based on flag" $
  "flag" ~> "fthen" ~> "felse" ~>
  "b" <<~ hasFlag @@ var "flag" $
  Logic.ifElse (var "b") (var "fthen") (var "felse")

withDepth :: TBinding (Name -> (Int -> Flow s a) -> Flow s a)
withDepth = define "withDepth" $
  doc ("Provide an one-indexed, integer-valued 'depth' to a flow, where the depth is the number of nested calls."
    <> " This is useful for generating variable names while avoiding conflicts between the variables of parents and children."
    <> " E.g. a variable in an outer case/match statement might be \"v1\", whereas the variable of another case/match statement"
    <> " inside of the first one becomes \"v2\". See also nextCount.") $
  "key" ~> "f" ~>
  "count" <<~ getCount @@ var "key" $
  "inc" <~ Math.add (var "count") (int32 1) $
  exec (putCount @@ var "key" @@ var "inc") $
  "r" <<~ var "f" @@ var "inc" $
  exec (putCount @@ var "key" @@ var "count") $
  produce (var "r")
