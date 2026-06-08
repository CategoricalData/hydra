
module Hydra.Sources.Kernel.Terms.Annotations where

-- Standard imports for kernel terms modules
import Hydra.Kernel hiding (
  aggregateAnnotations, commentsFromBinding, commentsFromFieldType,
  getAnnotationMap,
  getDescription, getTermAnnotation, getTermDescription,
  getType, getTypeAnnotation, getTypeClasses,
  getTypeDescription, isNativeType, hasDescription,
  hasTypeDescription,
  normalizeTermAnnotations, normalizeTypeAnnotations, setAnnotation,
  setDescription, setTermAnnotation, setTermDescription, setType, setTypeAnnotation, setTypeClasses,
  setTypeDescription, termAnnotationInternal, typeAnnotationInternal,
  wrapAnnotationMap)
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


ns :: ModuleName
ns = ModuleName "hydra.annotations"

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = definitions,
            moduleDependencies = Bootstrap.unqualifiedDep <$> ([Constants.ns, moduleName DecodeCore.module_, moduleName EncodeCore.module_, ExtractCore.ns, Lexical.ns,
      Strip.ns, ShowCore.ns, ShowError.ns] L.++ kernelTypesModuleNames),
            moduleMetadata = Bootstrap.descriptionMetadata (Just "Utilities for reading and writing type and term annotations")}
  where
   definitions = [
     toDefinition aggregateAnnotations,
     toDefinition commentsFromBinding,
     toDefinition commentsFromFieldType,
     toDefinition getAnnotationMap,
     toDefinition getDescription,
     toDefinition getTermAnnotation,
     toDefinition getTermDescription,
     toDefinition getType,
     toDefinition getTypeAnnotation,
     toDefinition getTypeClasses,
     toDefinition getTypeDescription,
     toDefinition hasDescription,
     toDefinition hasTypeDescription,
     toDefinition isNativeType,
     toDefinition normalizeTermAnnotations,
     toDefinition normalizeTypeAnnotations,
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
     toDefinition wrapAnnotationMap]

define :: String -> TypedTerm a -> TypedTermDefinition a
define = definitionInModule module_

aggregateAnnotations :: TypedTermDefinition ((x -> Maybe y) -> (y -> x) -> (y -> M.Map Name Term) -> x -> M.Map Name Term)
aggregateAnnotations = define "aggregateAnnotations" $
  doc "Aggregate annotations from nested structures" $
  "getValue" ~> "getX" ~> "getAnns" ~> "t" ~>
  "toPairs" <~ ("rest" ~> "t" ~> Optionals.cases (var "getValue" @@ var "t") (var "rest") (lambda "yy" (var "toPairs"
      @@ Lists.cons (Maps.toList (var "getAnns" @@ var "yy")) (var "rest")
      @@ (var "getX" @@ var "yy")))) $
  Maps.fromList (Lists.concat (var "toPairs" @@ list ([] :: [TypedTerm [(Name, Term)]]) @@ var "t"))

commentsFromBinding :: TypedTermDefinition (InferenceContext -> Graph -> Binding -> Either Error (Maybe String))
commentsFromBinding = define "commentsFromBinding" $
  doc "Extract comments/description from a Binding" $
  "cx" ~> "g" ~> "b" ~>
  getTermDescription @@ var "cx" @@ var "g" @@ (Core.bindingTerm $ var "b")

commentsFromFieldType :: TypedTermDefinition (InferenceContext -> Graph -> FieldType -> Either Error (Maybe String))
commentsFromFieldType = define "commentsFromFieldType" $
  doc "Extract comments/description from a FieldType" $
  "cx" ~> "g" ~> "ft" ~>
  getTypeDescription @@ var "cx" @@ var "g" @@ (Core.fieldTypeType $ var "ft")

formatError :: TypedTerm (Error -> String)
formatError = "e" ~> ShowError.error_ @@ var "e"

getDescription :: TypedTermDefinition (InferenceContext -> Graph -> M.Map Name Term -> Prelude.Either Error (Maybe String))
getDescription = define "getDescription" $
  doc "Get description from annotations map (Either version)" $
  "cx" ~> "graph" ~> "anns" ~>
  Optionals.cases (Maps.lookup (Core.nameLift keyDescription) (var "anns")) (right nothing) ("term" ~> Eithers.map (reify just) (ExtractCore.string @@ var "graph" @@ var "term"))

getTermAnnotation :: TypedTermDefinition (Name -> Term -> Maybe Term)
getTermAnnotation = define "getTermAnnotation" $
  doc "Get a term annotation" $
  "key" ~> "term" ~> Maps.lookup (var "key") (termAnnotationInternal @@ var "term")

getTermDescription :: TypedTermDefinition (InferenceContext -> Graph -> Term -> Prelude.Either Error (Maybe String))
getTermDescription = define "getTermDescription" $
  doc "Get term description (Either version)" $
  "cx" ~> "graph" ~> "term" ~>
  "peel" <~ ("t" ~> cases _Term (var "t")
    (Just $ var "t") [
    _Term_typeLambda>>: "tl" ~> var "peel" @@ Core.typeLambdaBody (var "tl"),
    _Term_typeApplication>>: "ta" ~> var "peel" @@ Core.typeApplicationTermBody (var "ta")]) $
  getDescription @@ var "cx" @@ var "graph" @@ (termAnnotationInternal @@ (var "peel" @@ var "term"))

getType :: TypedTermDefinition (Graph -> M.Map Name Term -> Prelude.Either DecodingError (Maybe Type))
getType = define "getType" $
  doc "Get type from annotations" $
  "graph" ~> "anns" ~>
  Optionals.cases (Maps.lookup (Constants.keyType) (var "anns")) (right nothing) ("dat" ~> Eithers.map (reify just) (decoderFor _Type @@ var "graph" @@ var "dat"))

getTypeAnnotation :: TypedTermDefinition (Name -> Type -> Maybe Term)
getTypeAnnotation = define "getTypeAnnotation" $
  doc "Get a type annotation" $
  "key" ~> "typ" ~> Maps.lookup (var "key") (typeAnnotationInternal @@ var "typ")

getTypeClasses :: TypedTermDefinition (InferenceContext -> Graph -> Term -> Prelude.Either Error (M.Map Name (S.Set Name)))
getTypeClasses = define "getTypeClasses" $
  doc "Get type classes from term. Each Set Name contains bare class identifiers (#275)." $
  "cx" ~> "graph" ~> "term" ~>
  "decodeName" <~ ("term" ~> Eithers.bimap
    ("de" ~> Error.errorDecoding $ var "de")
    ("x" ~> var "x")
    (decoderFor _Name @@ var "graph" @@ var "term")) $
  Optionals.cases (getTermAnnotation @@ Constants.keyClasses @@ var "term") (right Maps.empty) ("term" ~>
      ExtractCore.map
        @@ var "decodeName"
        @@ (ExtractCore.setOf @@ var "decodeName" @@ var "graph")
        @@ var "graph"
        @@ (var "term"))

getTypeDescription :: TypedTermDefinition (InferenceContext -> Graph -> Type -> Prelude.Either Error (Maybe String))
getTypeDescription = define "getTypeDescription" $
  doc "Get type description (Either version)" $
  "cx" ~> "graph" ~> "typ" ~>
  getDescription @@ var "cx" @@ var "graph" @@ (typeAnnotationInternal @@ var "typ")

hasDescription :: TypedTermDefinition (M.Map Name Term -> Bool)
hasDescription = define "hasDescription" $
  doc "Check if annotations contain description" $
  "anns" ~> Optionals.isGiven (Maps.lookup (Constants.keyDescription) (var "anns"))

hasTypeDescription :: TypedTermDefinition (Type -> Bool)
hasTypeDescription = define "hasTypeDescription" $
  doc "Check if type has description" $
  "typ" ~> hasDescription @@ (typeAnnotationInternal @@ var "typ")

isNativeType :: TypedTermDefinition (Binding -> Bool)
isNativeType = define "isNativeType" $
  doc ("For a typed term, decide whether a coder should encode it as a native type expression,"
    <> " or as a Hydra type expression.") $
  "el" ~>
  "isFlaggedAsFirstClassType" <~ Optionals.fromOptional false (
    Optionals.map
      (constant true)
      (getTermAnnotation @@ Constants.keyFirstClassType @@ (Core.bindingTerm (var "el")))) $
  Optionals.cases (Core.bindingTypeScheme (var "el")) false ("ts" ~> Logic.and
      (Equality.equal (var "ts") (Core.typeScheme (list ([] :: [TypedTerm Name])) (Core.typeVariable (Core.nameLift _Type)) Phantoms.nothing))
      (Logic.not (var "isFlaggedAsFirstClassType")))

normalizeTermAnnotations :: TypedTermDefinition (Term -> Term)
normalizeTermAnnotations = define "normalizeTermAnnotations" $
  doc "Normalize term annotations" $
  "term" ~>
  "anns" <~ termAnnotationInternal @@ var "term" $
  "stripped" <~ Strip.deannotateTerm @@ var "term" $
  Logic.ifElse (Maps.null (var "anns"))
    (var "stripped")
    (Core.termAnnotated (Core.annotatedTerm (var "stripped") (wrapAnnotationMap @@ var "anns")))

normalizeTypeAnnotations :: TypedTermDefinition (Type -> Type)
normalizeTypeAnnotations = define "normalizeTypeAnnotations" $
  doc "Normalize type annotations" $
  "typ" ~>
  "anns" <~ typeAnnotationInternal @@ var "typ" $
  "stripped" <~ Strip.deannotateType @@ var "typ" $
  Logic.ifElse (Maps.null (var "anns"))
    (var "stripped")
    (Core.typeAnnotated (Core.annotatedType (var "stripped") (wrapAnnotationMap @@ var "anns")))

setAnnotation :: TypedTermDefinition (Name -> Maybe Term -> M.Map Name Term -> M.Map Name Term)
setAnnotation = define "setAnnotation" $
  doc "Set annotation in map" $
  "key" ~> "val" ~> "m" ~> Maps.alter (constant (var "val")) (var "key") (var "m")

setDescription :: TypedTermDefinition (Maybe String -> M.Map Name Term -> M.Map Name Term)
setDescription = define "setDescription" $
  doc "Set description in annotations" $
  "d" ~> setAnnotation
    @@ Constants.keyDescription
    @@ Optionals.map (reify Core.termLiteral <.> reify Core.literalString) (var "d")

setTermAnnotation :: TypedTermDefinition (Name -> Maybe Term -> Term -> Term)
setTermAnnotation = define "setTermAnnotation" $
  doc "Set term annotation" $
  "key" ~> "val" ~> "term" ~>
  "term'" <~ Strip.deannotateTerm @@ var "term" $
  "anns" <~ setAnnotation @@ var "key" @@ var "val" @@ (termAnnotationInternal @@ var "term") $
  Logic.ifElse (Maps.null (var "anns"))
    (var "term'")
    (Core.termAnnotated (Core.annotatedTerm (var "term'") (wrapAnnotationMap @@ var "anns")))

setTermDescription :: TypedTermDefinition (Maybe String -> Term -> Term)
setTermDescription = define "setTermDescription" $
  doc "Set term description" $
  "d" ~> setTermAnnotation
    @@ Constants.keyDescription
    @@ Optionals.map ("s" ~> Core.termLiteral (Core.literalString (var "s"))) (var "d")

setType :: TypedTermDefinition (Maybe Type -> M.Map Name Term -> M.Map Name Term)
setType = define "setType" $
  doc "Set type in annotations" $
  "mt" ~> setAnnotation @@ Constants.keyType @@ Optionals.map (encoderFor _Type) (var "mt")

setTypeAnnotation :: TypedTermDefinition (Name -> Maybe Term -> Type -> Type)
setTypeAnnotation = define "setTypeAnnotation" $
  doc "Set type annotation" $
  "key" ~> "val" ~> "typ" ~>
  "typ'" <~ Strip.deannotateType @@ var "typ" $
  "anns" <~ setAnnotation @@ var "key" @@ var "val" @@ (typeAnnotationInternal @@ var "typ") $
  Logic.ifElse (Maps.null (var "anns"))
    (var "typ'")
    (Core.typeAnnotated (Core.annotatedType (var "typ'") (wrapAnnotationMap @@ var "anns")))

setTypeClasses :: TypedTermDefinition (M.Map Name (S.Set Name) -> Term -> Term)
setTypeClasses = define "setTypeClasses" $
  doc "Set type classes on term. The Set Name carries bare class identifiers (#275)." $
  "m" ~> "term" ~>
  "encodePair" <~ ("nameClasses" ~>
    "name" <~ Pairs.first (var "nameClasses") $
    "classes" <~ Pairs.second (var "nameClasses") $
    pair
      (encoderFor _Name @@ var "name")
      (Core.termSet (Sets.fromList (Lists.map (encoderFor _Name) (Sets.toList (var "classes")))))) $
  "encoded" <~ Logic.ifElse (Maps.null (var "m"))
    nothing
    (just (Core.termMap (Maps.fromList (Lists.map (var "encodePair") (Maps.toList (var "m")))))) $
  setTermAnnotation @@ Constants.keyClasses @@ var "encoded" @@ var "term"

setTypeDescription :: TypedTermDefinition (Maybe String -> Type -> Type)
setTypeDescription = define "setTypeDescription" $
  doc "Set type description" $
  "d" ~> setTypeAnnotation
    @@ Constants.keyDescription
    @@ Optionals.map (reify Core.termLiteral <.> reify Core.literalString) (var "d")

termAnnotationInternal :: TypedTermDefinition (Term -> M.Map Name Term)
termAnnotationInternal = define "termAnnotationInternal" $
  doc "Get internal term annotations" $
  "term" ~>
  "getAnn" <~ ("t" ~> cases _Term (var "t")
    (Just nothing) [
    _Term_annotated>>: "a" ~> just $ var "a"]) $
  aggregateAnnotations
    @@ var "getAnn"
    @@ ("at" ~> Core.annotatedTermBody $ var "at")
    @@ ("at" ~> getAnnotationMap @@ (Core.annotatedTermAnnotation $ var "at"))
    @@ var "term"

typeAnnotationInternal :: TypedTermDefinition (Type -> M.Map Name Term)
typeAnnotationInternal = define "typeAnnotationInternal" $
  doc "Get internal type annotations" $
  "typ" ~>
  "getAnn" <~ lambda "t" (cases _Type (var "t")
    (Just nothing) [
    _Type_annotated>>: lambda "a" (just $ var "a")]) $
  aggregateAnnotations
    @@ var "getAnn"
    @@ ("at" ~> Core.annotatedTypeBody $ var "at")
    @@ ("at" ~> getAnnotationMap @@ (Core.annotatedTypeAnnotation $ var "at"))
    @@ var "typ"

-- | Project a Map<Name, Term> out of an annotation Term. If the Term is a
-- TermMap whose keys are TermVariable-encoded names, returns those
-- (Name, value) entries; for any other shape, returns the empty map.
--
-- This codifies Hydra's annotation map convention on top of the new
-- Term-typed annotation field (#386): annotations may be any Term, but
-- map-shaped annotations with TermVariable keys round-trip through
-- this and wrapAnnotationMap.
--
-- For backwards compatibility during the #386 migration, also recognizes
-- TermWrap-encoded Name keys (the previous EncodeCore.name shape). This
-- legacy branch can be removed once every package has been re-synced.
getAnnotationMap :: TypedTermDefinition (Term -> M.Map Name Term)
getAnnotationMap = define "getAnnotationMap" $
  doc ("Project a Map<Name, Term> out of an annotation Term."
    <> " For a TermMap with TermVariable-shaped keys (or, transitionally,"
    <> " TermWrap-encoded Name keys), returns those (Name, value) entries;"
    <> " for any other shape, returns the empty map.") $
  "t" ~> "extractName" <~ ("k" ~> cases _Term (var "k")
    (Just nothing) [
    _Term_variable>>: "n" ~> just (var "n"),
    _Term_wrap>>: "w" ~> cases _Term (Core.wrappedTermBody $ var "w")
      (Just nothing) [
      _Term_literal>>: "l" ~> cases _Literal (var "l")
        (Just nothing) [
        _Literal_string>>: "s" ~> just (Core.name (var "s"))]]]) $
    "fromEntry" <~ ("p" ~>
      "k" <~ Pairs.first (var "p") $
      "v" <~ Pairs.second (var "p") $
      Optionals.map ("n" ~> pair (var "n") (var "v")) (var "extractName" @@ var "k")) $
    cases _Term (var "t")
      (Just Maps.empty) [
      _Term_map>>: "m" ~> Maps.fromList
        (Optionals.cat (Lists.map (var "fromEntry") (Maps.toList (var "m"))))]

-- | Wrap a Map<Name, Term> as a TermMap annotation. Each Name key becomes a
-- TermVariable. Inverse of getAnnotationMap on map-shaped inputs.
wrapAnnotationMap :: TypedTermDefinition (M.Map Name Term -> Term)
wrapAnnotationMap = define "wrapAnnotationMap" $
  doc "Wrap a Map<Name, Term> as a TermMap annotation. Each Name key becomes a TermVariable." $
  "m" ~> Core.termMap (Maps.fromList
    (Lists.map ("p" ~> pair (Core.termVariable (Pairs.first (var "p"))) (Pairs.second (var "p")))
      (Maps.toList (var "m"))))

-- | Helper (not a registered definition) for creating a type binding from a name and type.
-- This was previously the deprecated "typeElement" definition.
typeBinding :: TypedTerm (Name -> Type -> Binding)
typeBinding =
  "name" ~> "typ" ~>
  "schemaTerm" <~ Core.termVariable (Core.nameLift _Type) $
  "dataTerm" <~ normalizeTermAnnotations @@ (Core.termAnnotated (Core.annotatedTerm
    (encoderFor _Type @@ var "typ")
    (wrapAnnotationMap @@ Maps.fromList (list [pair (Constants.keyType) (var "schemaTerm")])))) $
  Core.binding (var "name") (var "dataTerm") (just (Core.typeScheme (list ([] :: [TypedTerm Name])) (Core.typeVariable $ Core.nameLift _Type) Phantoms.nothing))
