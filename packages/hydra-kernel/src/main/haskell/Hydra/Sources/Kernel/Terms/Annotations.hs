
module Hydra.Sources.Kernel.Terms.Annotations where

-- Standard imports for kernel terms modules
import Hydra.Kernel hiding (
  aggregateAnnotations, commentsFromBinding, commentsFromFieldType,
  getDescription, getTermAnnotation, getTermDescription,
  getType, getTypeAnnotation, getTypeClasses,
  getTypeDescription, isNativeType, hasDescription,
  hasTypeDescription,
  normalizeTermAnnotations, normalizeTypeAnnotations, setAnnotation,
  setDescription, setTermAnnotation, setTermDescription, setType, setTypeAnnotation, setTypeClasses,
  setTypeDescription, termAnnotationInternal, typeAnnotationInternal)
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
            moduleDescription = Just "Utilities for reading and writing type and term annotations"}
  where
   definitions = [
     toDefinition aggregateAnnotations,
     toDefinition commentsFromBinding,
     toDefinition commentsFromFieldType,
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
     toDefinition typeAnnotationInternal]

define :: String -> TTerm a -> TTermDefinition a
define = definitionInModule module_

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

commentsFromBinding :: TTermDefinition (InferenceContext -> Graph -> Binding -> Either Error (Maybe String))
commentsFromBinding = define "commentsFromBinding" $
  doc "Extract comments/description from a Binding" $
  "cx" ~> "g" ~> "b" ~>
  getTermDescription @@ var "cx" @@ var "g" @@ (Core.bindingTerm $ var "b")

commentsFromFieldType :: TTermDefinition (InferenceContext -> Graph -> FieldType -> Either Error (Maybe String))
commentsFromFieldType = define "commentsFromFieldType" $
  doc "Extract comments/description from a FieldType" $
  "cx" ~> "g" ~> "ft" ~>
  getTypeDescription @@ var "cx" @@ var "g" @@ (Core.fieldTypeType $ var "ft")

formatError :: TTerm (Error -> String)
formatError = "e" ~> ShowError.error_ @@ var "e"

getDescription :: TTermDefinition (InferenceContext -> Graph -> M.Map Name Term -> Prelude.Either Error (Maybe String))
getDescription = define "getDescription" $
  doc "Get description from annotations map (Either version)" $
  "cx" ~> "graph" ~> "anns" ~>
  Maybes.maybe
    (right nothing)
    ("term" ~> Eithers.map (reify just) (ExtractCore.string @@ var "graph" @@ var "term"))
    (Maps.lookup (Core.nameLift keyDescription) (var "anns"))

getTermAnnotation :: TTermDefinition (Name -> Term -> Maybe Term)
getTermAnnotation = define "getTermAnnotation" $
  doc "Get a term annotation" $
  "key" ~> "term" ~> Maps.lookup (var "key") (termAnnotationInternal @@ var "term")

getTermDescription :: TTermDefinition (InferenceContext -> Graph -> Term -> Prelude.Either Error (Maybe String))
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
    ("dat" ~> Eithers.map (reify just) (decoderFor _Type @@ var "graph" @@ var "dat"))
    (Maps.lookup (Constants.keyType) (var "anns"))

getTypeAnnotation :: TTermDefinition (Name -> Type -> Maybe Term)
getTypeAnnotation = define "getTypeAnnotation" $
  doc "Get a type annotation" $
  "key" ~> "typ" ~> Maps.lookup (var "key") (typeAnnotationInternal @@ var "typ")

getTypeClasses :: TTermDefinition (InferenceContext -> Graph -> Term -> Prelude.Either Error (M.Map Name (S.Set Name)))
getTypeClasses = define "getTypeClasses" $
  doc "Get type classes from term. Each Set Name contains bare class identifiers (#275)." $
  "cx" ~> "graph" ~> "term" ~>
  "decodeName" <~ ("term" ~> Eithers.bimap
    ("de" ~> Error.errorDecoding $ var "de")
    ("x" ~> var "x")
    (decoderFor _Name @@ var "graph" @@ var "term")) $
  Maybes.maybe
    (right Maps.empty)
    ("term" ~>
      ExtractCore.map
        @@ var "decodeName"
        @@ (ExtractCore.setOf @@ var "decodeName" @@ var "graph")
        @@ var "graph"
        @@ (var "term"))
    (getTermAnnotation @@ Constants.keyClasses @@ var "term")

getTypeDescription :: TTermDefinition (InferenceContext -> Graph -> Type -> Prelude.Either Error (Maybe String))
getTypeDescription = define "getTypeDescription" $
  doc "Get type description (Either version)" $
  "cx" ~> "graph" ~> "typ" ~>
  getDescription @@ var "cx" @@ var "graph" @@ (typeAnnotationInternal @@ var "typ")

hasDescription :: TTermDefinition (M.Map Name Term -> Bool)
hasDescription = define "hasDescription" $
  doc "Check if annotations contain description" $
  "anns" ~> Maybes.isJust (Maps.lookup (Constants.keyDescription) (var "anns"))

hasTypeDescription :: TTermDefinition (Type -> Bool)
hasTypeDescription = define "hasTypeDescription" $
  doc "Check if type has description" $
  "typ" ~> hasDescription @@ (typeAnnotationInternal @@ var "typ")

isNativeType :: TTermDefinition (Binding -> Bool)
isNativeType = define "isNativeType" $
  doc ("For a typed term, decide whether a coder should encode it as a native type expression,"
    <> " or as a Hydra type expression.") $
  "el" ~>
  "isFlaggedAsFirstClassType" <~ Maybes.fromMaybe false (
    Maybes.map
      (constant true)
      (getTermAnnotation @@ Constants.keyFirstClassType @@ (Core.bindingTerm (var "el")))) $
  Maybes.maybe false
    ("ts" ~> Logic.and
      (Equality.equal (var "ts") (Core.typeScheme (list ([] :: [TTerm Name])) (Core.typeVariable (Core.nameLift _Type)) Phantoms.nothing))
      (Logic.not (var "isFlaggedAsFirstClassType")))
    (Core.bindingTypeScheme (var "el"))

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

setAnnotation :: TTermDefinition (Name -> Maybe Term -> M.Map Name Term -> M.Map Name Term)
setAnnotation = define "setAnnotation" $
  doc "Set annotation in map" $
  "key" ~> "val" ~> "m" ~> Maps.alter (constant (var "val")) (var "key") (var "m")

setDescription :: TTermDefinition (Maybe String -> M.Map Name Term -> M.Map Name Term)
setDescription = define "setDescription" $
  doc "Set description in annotations" $
  "d" ~> setAnnotation
    @@ Constants.keyDescription
    @@ Maybes.map (reify Core.termLiteral <.> reify Core.literalString) (var "d")

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
    @@ Constants.keyDescription
    @@ Maybes.map ("s" ~> Core.termLiteral (Core.literalString (var "s"))) (var "d")

setType :: TTermDefinition (Maybe Type -> M.Map Name Term -> M.Map Name Term)
setType = define "setType" $
  doc "Set type in annotations" $
  "mt" ~> setAnnotation @@ Constants.keyType @@ Maybes.map (encoderFor _Type) (var "mt")

setTypeAnnotation :: TTermDefinition (Name -> Maybe Term -> Type -> Type)
setTypeAnnotation = define "setTypeAnnotation" $
  doc "Set type annotation" $
  "key" ~> "val" ~> "typ" ~>
  "typ'" <~ Strip.deannotateType @@ var "typ" $
  "anns" <~ setAnnotation @@ var "key" @@ var "val" @@ (typeAnnotationInternal @@ var "typ") $
  Logic.ifElse (Maps.null (var "anns"))
    (var "typ'")
    (Core.typeAnnotated (Core.annotatedType (var "typ'") (var "anns")))

setTypeClasses :: TTermDefinition (M.Map Name (S.Set Name) -> Term -> Term)
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

setTypeDescription :: TTermDefinition (Maybe String -> Type -> Type)
setTypeDescription = define "setTypeDescription" $
  doc "Set type description" $
  "d" ~> setTypeAnnotation
    @@ Constants.keyDescription
    @@ Maybes.map (reify Core.termLiteral <.> reify Core.literalString) (var "d")

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

-- | Helper (not a registered definition) for creating a type binding from a name and type.
-- This was previously the deprecated "typeElement" definition.
typeBinding :: TTerm (Name -> Type -> Binding)
typeBinding =
  "name" ~> "typ" ~>
  "schemaTerm" <~ Core.termVariable (Core.nameLift _Type) $
  "dataTerm" <~ normalizeTermAnnotations @@ (Core.termAnnotated (Core.annotatedTerm
    (encoderFor _Type @@ var "typ")
    (Maps.fromList (list [pair (Constants.keyType) (var "schemaTerm")])))) $
  Core.binding (var "name") (var "dataTerm") (just (Core.typeScheme (list ([] :: [TTerm Name])) (Core.typeVariable $ Core.nameLift _Type) Phantoms.nothing))
