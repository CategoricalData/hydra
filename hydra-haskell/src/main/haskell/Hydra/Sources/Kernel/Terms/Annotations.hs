
module Hydra.Sources.Kernel.Terms.Annotations where

-- Standard imports for kernel terms modules
import Hydra.Kernel hiding (
  aggregateAnnotations, debugIf, failOnFlag, getDebugId,
  getAttr, getAttrWithDefault, getCount,
  getDescription, getTermAnnotation, getTermDescription,
  getType, getTypeAnnotation, getTypeClasses,
  getTypeDescription, isNativeType, hasDescription, hasFlag,
  hasTypeDescription, nextCount,
  normalizeTermAnnotations, normalizeTypeAnnotations, putAttr, putCount,
  resetCount, setAnnotation,
  setDescription, setTermAnnotation, setTermDescription, setType, setTypeAnnotation, setTypeClasses,
  setTypeDescription, termAnnotationInternal, typeAnnotationInternal, typeElement, whenFlag)
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Paths    as Paths
import qualified Hydra.Dsl.Annotations       as Annotations
import qualified Hydra.Dsl.Ast          as Ast
import qualified Hydra.Dsl.Bootstrap         as Bootstrap
import qualified Hydra.Dsl.Coders       as Coders
import qualified Hydra.Dsl.Util      as Util
import qualified Hydra.Dsl.Meta.Context      as Ctx
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
import qualified Hydra.Dsl.Errors       as Error
import qualified Hydra.Dsl.Meta.Variants     as Variants
import           Hydra.Sources.Kernel.Types.All
import           Prelude hiding ((++))
import qualified Data.Int                    as I
import qualified Data.List                   as L
import qualified Data.Map                    as M
import qualified Data.Set                    as S
import qualified Data.Maybe                  as Y

import qualified Hydra.Sources.Kernel.Terms.Constants    as Constants
import qualified Hydra.Sources.Kernel.Terms.Extract.Core as ExtractCore
import qualified Hydra.Sources.Kernel.Terms.Lexical      as Lexical
import qualified Hydra.Sources.Kernel.Terms.Show.Core    as ShowCore
import qualified Hydra.Sources.Kernel.Terms.Strip        as Strip
import qualified Hydra.Sources.Kernel.Terms.Show.Errors  as ShowError
import qualified Hydra.Sources.Decode.Core            as DecodeCore
import qualified Hydra.Sources.Encode.Core            as EncodeCore
import Hydra.Encoding (encodeBindingName)


ns :: Namespace
ns = Namespace "hydra.annotations"

module_ :: Module
module_ = Module ns elements
    [Constants.ns, moduleNamespace DecodeCore.module_, moduleNamespace EncodeCore.module_, ExtractCore.ns, Lexical.ns,
      Strip.ns, ShowCore.ns, ShowError.ns]
    kernelTypesNamespaces $
    Just "Utilities for reading and writing type and term annotations"
  where
   elements = [
     toDefinition aggregateAnnotations,
     toDefinition debugIf,
     toDefinition failOnFlag,
     toDefinition getDebugId,
     toDefinition getAttr,
     toDefinition getAttrWithDefault,
     toDefinition getCount,
     toDefinition getDescription,
     toDefinition getTermAnnotation,
     toDefinition getTermDescription,
     toDefinition getType,
     toDefinition getTypeAnnotation,
     toDefinition getTypeClasses,
     toDefinition getTypeDescription,
     toDefinition isNativeType,
     toDefinition hasDescription,
     toDefinition hasFlag,
     toDefinition hasTypeDescription,
     toDefinition nextCount,
     toDefinition normalizeTermAnnotations,
     toDefinition normalizeTypeAnnotations,
     toDefinition putAttr,
     toDefinition putCount,
     toDefinition resetCount,
     toDefinition setAnnotation,
     toDefinition setDescription,
     toDefinition setTermAnnotation,
     toDefinition setTermDescription,
     toDefinition setType,
     toDefinition setTypeAnnotation,
     toDefinition setTypeClasses,
     toDefinition setTypeDescription,
     toDefinition termAnnotationInternal,
     toDefinition typeAnnotationInternal,
     toDefinition typeElement,
     toDefinition whenFlag]

define :: String -> TTerm a -> TTermDefinition a
define = definitionInModule module_

formatError :: TTerm (InContext Error -> String)
formatError = "ic" ~> ShowError.error_ @@ Ctx.inContextObject (var "ic")

aggregateAnnotations :: TTermDefinition ((x -> Maybe y) -> (y -> x) -> (y -> M.Map Name Term) -> x -> M.Map Name Term)
aggregateAnnotations = define "aggregateAnnotations" $
  doc "Aggregate annotations from nested structures" $
  "getValue" ~> "getX" ~> "getAnns" ~> "t" ~>
  "toPairs" <~ ("rest" ~> "t" ~> Maybes.maybe (var "rest")
    (lambda "yy" (var "toPairs"
      @@ Lists.cons (Maps.toList (var "getAnns" @@ var "yy")) (var "rest")
      @@ (var "getX" @@ var "yy")))
    (var "getValue" @@ var "t")) $
  Maps.fromList (Lists.concat (var "toPairs" @@ list ([] :: [TTerm [(Name, Term)]]) @@ var "t"))

debugIf :: TTermDefinition (Context -> String -> String -> Prelude.Either (InContext Error) ())
debugIf = define "debugIf" $
  doc "Debug if the debug ID matches (Either version)" $
  "cx" ~> "debugId" ~> "message" ~>
  "mid" <<~ getDebugId @@ var "cx" $
  Logic.ifElse (Equality.equal (var "mid") (just $ var "debugId"))
    (Ctx.failInContext (Error.errorOther $ Error.otherError (var "message")) (var "cx"))
    (right unit)

failOnFlag :: TTermDefinition (Context -> Name -> String -> Prelude.Either (InContext Error) ())
failOnFlag = define "failOnFlag" $
  doc "Fail if the given flag is set (Either version)" $
  "cx" ~> "flag" ~> "msg" ~>
  "val" <<~ hasFlag @@ var "cx" @@ var "flag" $
  Logic.ifElse (var "val")
    (Ctx.failInContext (Error.errorOther $ Error.otherError (var "msg")) (var "cx"))
    (right unit)

getAttr :: TTermDefinition (Name -> Context -> Maybe Term)
getAttr = define "getAttr" $
  doc "Get an attribute from a context (pure version)" $
  "key" ~> "cx" ~>
  Maps.lookup (var "key") (Ctx.contextOther (var "cx"))

getAttrWithDefault :: TTermDefinition (Name -> Term -> Context -> Term)
getAttrWithDefault = define "getAttrWithDefault" $
  doc "Get an attribute with a default value from context (pure version)" $
  "key" ~> "def" ~> "cx" ~>
  Maybes.fromMaybe (var "def") (getAttr @@ var "key" @@ var "cx")

getCount :: TTermDefinition (Name -> Context -> Int)
getCount = define "getCount" $
  doc "Get a counter value from context (pure version)" $
  "key" ~> "cx" ~>
  Maybes.maybe
    (int32 0)
    ("term" ~> cases _Term (var "term") (Just (int32 0)) [
      _Term_literal>>: "lit" ~> cases _Literal (var "lit") (Just (int32 0)) [
        _Literal_integer>>: "iv" ~> cases _IntegerValue (var "iv") (Just (int32 0)) [
          _IntegerValue_int32>>: "i" ~> var "i"]]])
    (Maps.lookup (var "key") (Ctx.contextOther (var "cx")))

getDebugId :: TTermDefinition (Context -> Prelude.Either (InContext Error) (Maybe String))
getDebugId = define "getDebugId" $
  doc "Get the debug ID from context (Either version)" $
  "cx" ~>
  Maybes.maybe
    (right nothing)
    ("term" ~> Eithers.map (unaryFunction just) (ExtractCore.string @@ var "cx" @@ Graph.emptyGraph @@ var "term"))
    (getAttr @@ Constants.key_debugId @@ var "cx")

getDescription :: TTermDefinition (Context -> Graph -> M.Map Name Term -> Prelude.Either (InContext Error) (Maybe String))
getDescription = define "getDescription" $
  doc "Get description from annotations map (Either version)" $
  "cx" ~> "graph" ~> "anns" ~>
  Maybes.maybe
    (right nothing)
    ("term" ~> Eithers.map (unaryFunction just) (ExtractCore.string @@ var "cx" @@ var "graph" @@ var "term"))
    (Maps.lookup (Core.nameLift key_description) (var "anns"))

getTermAnnotation :: TTermDefinition (Name -> Term -> Maybe Term)
getTermAnnotation = define "getTermAnnotation" $
  doc "Get a term annotation" $
  "key" ~> "term" ~> Maps.lookup (var "key") (termAnnotationInternal @@ var "term")

getTermDescription :: TTermDefinition (Context -> Graph -> Term -> Prelude.Either (InContext Error) (Maybe String))
getTermDescription = define "getTermDescription" $
  doc "Get term description (Either version)" $
  "cx" ~> "graph" ~> "term" ~>
  "peel" <~ ("t" ~> cases _Term (var "t")
    (Just $ var "t") [
    _Term_typeLambda>>: "tl" ~> var "peel" @@ Core.typeLambdaBody (var "tl"),
    _Term_typeApplication>>: "ta" ~> var "peel" @@ Core.typeApplicationTermBody (var "ta")]) $
  getDescription @@ var "cx" @@ var "graph" @@ (termAnnotationInternal @@ (var "peel" @@ var "term"))

getType :: TTermDefinition (Graph -> M.Map Name Term -> Prelude.Either DecodingError (Maybe Type))
getType = define "getType" $
  doc "Get type from annotations" $
  "graph" ~> "anns" ~>
  Maybes.maybe
    (right nothing)
    ("dat" ~> Eithers.map (unaryFunction just) (decoderFor _Type @@ var "graph" @@ var "dat"))
    (Maps.lookup (Constants.key_type) (var "anns"))

getTypeAnnotation :: TTermDefinition (Name -> Type -> Maybe Term)
getTypeAnnotation = define "getTypeAnnotation" $
  doc "Get a type annotation" $
  "key" ~> "typ" ~> Maps.lookup (var "key") (typeAnnotationInternal @@ var "typ")

getTypeClasses :: TTermDefinition (Context -> Graph -> Term -> Prelude.Either (InContext Error) (M.Map Name (S.Set TypeClass)))
getTypeClasses = define "getTypeClasses" $
  doc "Get type classes from term" $
  "cx" ~> "graph" ~> "term" ~>
  "decodeClass" <~ ("term" ~>
    "byName" <~ Maps.fromList (list [
      pair (Core.nameLift _TypeClass_equality) Graph.typeClassEquality,
      pair (Core.nameLift _TypeClass_ordering) Graph.typeClassOrdering]) $
    "fn" <<~ ExtractCore.unitVariant @@ var "cx" @@ Core.nameLift _TypeClass @@ var "graph" @@ var "term" $
    Maybes.maybe
      (Ctx.failInContext (Error.errorOther $ Error.otherError (string "unexpected: expected type class, got " ++ (ShowCore.term @@ var "term"))) (var "cx"))
      (unaryFunction right)
      (Maps.lookup (var "fn") (var "byName"))) $
  Maybes.maybe
    (right Maps.empty)
    ("term" ~>
      ExtractCore.map
        @@ var "cx"
        @@ ("t" ~> Eithers.bimap
          ("de" ~> Ctx.inContext (Error.errorOther $ Error.otherError (unwrap _DecodingError @@ var "de")) (var "cx"))
          ("x" ~> var "x")
          (decoderFor _Name @@ var "graph" @@ var "t"))
        @@ (ExtractCore.setOf @@ var "cx" @@ var "decodeClass" @@ var "graph")
        @@ var "graph"
        @@ (var "term"))
    (getTermAnnotation @@ Constants.key_classes @@ var "term")

getTypeDescription :: TTermDefinition (Context -> Graph -> Type -> Prelude.Either (InContext Error) (Maybe String))
getTypeDescription = define "getTypeDescription" $
  doc "Get type description (Either version)" $
  "cx" ~> "graph" ~> "typ" ~>
  getDescription @@ var "cx" @@ var "graph" @@ (typeAnnotationInternal @@ var "typ")

isNativeType :: TTermDefinition (Binding -> Bool)
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

hasDescription :: TTermDefinition (M.Map Name Term -> Bool)
hasDescription = define "hasDescription" $
  doc "Check if annotations contain description" $
  "anns" ~> Maybes.isJust (Maps.lookup (Constants.key_description) (var "anns"))

hasFlag :: TTermDefinition (Context -> Name -> Prelude.Either (InContext Error) Bool)
hasFlag = define "hasFlag" $
  doc "Check if flag is set (Either version)" $
  "cx" ~> "flag" ~>
  "term" <~ getAttrWithDefault @@ var "flag" @@ Core.false @@ var "cx" $
  ExtractCore.boolean @@ var "cx" @@ Graph.emptyGraph @@ var "term"

hasTypeDescription :: TTermDefinition (Type -> Bool)
hasTypeDescription = define "hasTypeDescription" $
  doc "Check if type has description" $
  "typ" ~> hasDescription @@ (typeAnnotationInternal @@ var "typ")

nextCount :: TTermDefinition (Name -> Context -> (Int, Context))
nextCount = define "nextCount" $
  doc "Return a zero-indexed counter for the given key and updated context (pure version)" $
  "key" ~> "cx" ~>
  "count" <~ getCount @@ var "key" @@ var "cx" $
  pair (var "count") (putCount @@ var "key" @@ Math.add (var "count") (int32 1) @@ var "cx")

normalizeTermAnnotations :: TTermDefinition (Term -> Term)
normalizeTermAnnotations = define "normalizeTermAnnotations" $
  doc "Normalize term annotations" $
  "term" ~>
  "anns" <~ termAnnotationInternal @@ var "term" $
  "stripped" <~ Strip.deannotateTerm @@ var "term" $
  Logic.ifElse (Maps.null (var "anns"))
    (var "stripped")
    (Core.termAnnotated (Core.annotatedTerm (var "stripped") (var "anns")))

normalizeTypeAnnotations :: TTermDefinition (Type -> Type)
normalizeTypeAnnotations = define "normalizeTypeAnnotations" $
  doc "Normalize type annotations" $
  "typ" ~>
  "anns" <~ typeAnnotationInternal @@ var "typ" $
  "stripped" <~ Strip.deannotateType @@ var "typ" $
  Logic.ifElse (Maps.null (var "anns"))
    (var "stripped")
    (Core.typeAnnotated (Core.annotatedType (var "stripped") (var "anns")))

putAttr :: TTermDefinition (Name -> Term -> Context -> Context)
putAttr = define "putAttr" $
  doc "Set an attribute in a context" $
  "key" ~> "val" ~> "cx" ~>
  Ctx.contextWithOther (var "cx") (Maps.insert (var "key") (var "val") (Ctx.contextOther (var "cx")))

putCount :: TTermDefinition (Name -> Int -> Context -> Context)
putCount = define "putCount" $
  doc "Set counter value in context" $
  "key" ~> "count" ~> "cx" ~>
  putAttr @@ var "key" @@ (Core.termLiteral (Core.literalInteger (Core.integerValueInt32 (var "count")))) @@ var "cx"

resetCount :: TTermDefinition (Name -> Context -> Context)
resetCount = define "resetCount" $
  doc "Reset counter to zero in context" $
  "key" ~> "cx" ~> putAttr @@ var "key" @@ MetaTerms.int32 0 @@ var "cx"

setAnnotation :: TTermDefinition (Name -> Maybe Term -> M.Map Name Term -> M.Map Name Term)
setAnnotation = define "setAnnotation" $
  doc "Set annotation in map" $
  "key" ~> "val" ~> "m" ~> Maps.alter (constant (var "val")) (var "key") (var "m")

setDescription :: TTermDefinition (Maybe String -> M.Map Name Term -> M.Map Name Term)
setDescription = define "setDescription" $
  doc "Set description in annotations" $
  "d" ~> setAnnotation
    @@ Constants.key_description
    @@ Maybes.map (unaryFunction Core.termLiteral <.> unaryFunction Core.literalString) (var "d")

setTermAnnotation :: TTermDefinition (Name -> Maybe Term -> Term -> Term)
setTermAnnotation = define "setTermAnnotation" $
  doc "Set term annotation" $
  "key" ~> "val" ~> "term" ~>
  "term'" <~ Strip.deannotateTerm @@ var "term" $
  "anns" <~ setAnnotation @@ var "key" @@ var "val" @@ (termAnnotationInternal @@ var "term") $
  Logic.ifElse (Maps.null (var "anns"))
    (var "term'")
    (Core.termAnnotated (Core.annotatedTerm (var "term'") (var "anns")))

setTermDescription :: TTermDefinition (Maybe String -> Term -> Term)
setTermDescription = define "setTermDescription" $
  doc "Set term description" $
  "d" ~> setTermAnnotation
    @@ Constants.key_description
    @@ Maybes.map ("s" ~> Core.termLiteral (Core.literalString (var "s"))) (var "d")

setType :: TTermDefinition (Maybe Type -> M.Map Name Term -> M.Map Name Term)
setType = define "setType" $
  doc "Set type in annotations" $
  "mt" ~> setAnnotation @@ Constants.key_type @@ Maybes.map (encoderFor _Type) (var "mt")

setTypeAnnotation :: TTermDefinition (Name -> Maybe Term -> Type -> Type)
setTypeAnnotation = define "setTypeAnnotation" $
  doc "Set type annotation" $
  "key" ~> "val" ~> "typ" ~>
  "typ'" <~ Strip.deannotateType @@ var "typ" $
  "anns" <~ setAnnotation @@ var "key" @@ var "val" @@ (typeAnnotationInternal @@ var "typ") $
  Logic.ifElse (Maps.null (var "anns"))
    (var "typ'")
    (Core.typeAnnotated (Core.annotatedType (var "typ'") (var "anns")))

setTypeClasses :: TTermDefinition (M.Map Name (S.Set TypeClass) -> Term -> Term)
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

setTypeDescription :: TTermDefinition (Maybe String -> Type -> Type)
setTypeDescription = define "setTypeDescription" $
  doc "Set type description" $
  "d" ~> setTypeAnnotation
    @@ Constants.key_description
    @@ Maybes.map (unaryFunction Core.termLiteral <.> unaryFunction Core.literalString) (var "d")

termAnnotationInternal :: TTermDefinition (Term -> M.Map Name Term)
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

typeAnnotationInternal :: TTermDefinition (Type -> M.Map Name Term)
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
typeElement :: TTermDefinition (Name -> Type -> Binding)
typeElement = define "typeElement" $
  doc "Create a type element with proper annotations" $
  "name" ~> "typ" ~>
  "schemaTerm" <~ Core.termVariable (Core.nameLift _Type) $
  "dataTerm" <~ normalizeTermAnnotations @@ (Core.termAnnotated (Core.annotatedTerm
    (encoderFor _Type @@ var "typ")
    (Maps.fromList (list [pair (Constants.key_type) (var "schemaTerm")])))) $
  Core.binding (var "name") (var "dataTerm") (just (Core.typeScheme (list ([] :: [TTerm Name])) (Core.typeVariable $ Core.nameLift _Type) Phantoms.nothing))

whenFlag :: TTermDefinition (Context -> Name -> Prelude.Either (InContext Error) a -> Prelude.Either (InContext Error) a -> Prelude.Either (InContext Error) a)
whenFlag = define "whenFlag" $
  doc "Execute different branches based on flag (Either version)" $
  "cx" ~> "flag" ~> "ethen" ~> "eelse" ~>
  "b" <<~ hasFlag @@ var "cx" @@ var "flag" $
  Logic.ifElse (var "b") (var "ethen") (var "eelse")
