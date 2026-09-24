
module Hydra.Sources.Kernel.Terms.Strip where

-- Standard imports for kernel terms modules
import Hydra.Kernel hiding (
  deannotateAndDetypeTerm,
  deannotateTerm,
  deannotateType,
  deannotateTypeParameters,
  deannotateTypeRecursive,
  deannotateTypeSchemeRecursive,
  detypeTerm,
  removeTermAnnotations,
  removeTypeAnnotations,
  removeTypeAnnotationsFromTerm,
  removeTypesFromTerm,
  stripTypeLambdas)
import qualified Hydra.Core.Dsl.Paths        as Paths
import qualified Hydra.Core.Overlay.Haskell.Dsl.Annotations       as Annotations
import qualified Hydra.Core.Dsl.Ast          as Ast
import qualified Hydra.Core.Overlay.Haskell.Bootstrap         as Bootstrap
import qualified Hydra.Core.Dsl.Coders       as Coders
import qualified Hydra.Core.Dsl.Util      as Util
import qualified Hydra.Core.Overlay.Haskell.Dsl.Meta.Core         as Core
import qualified Hydra.Core.Overlay.Haskell.Dsl.Meta.Graph        as Graph
import qualified Hydra.Core.Dsl.Json.Model         as Json
import qualified Hydra.Core.Dsl.Lib.Chars    as Chars
import qualified Hydra.Core.Dsl.Lib.Eithers  as Eithers
import qualified Hydra.Core.Dsl.Lib.Equality as Equality
import qualified Hydra.Core.Dsl.Lib.Lists    as Lists
import qualified Hydra.Core.Dsl.Lib.Literals as Literals
import qualified Hydra.Core.Dsl.Lib.Logic    as Logic
import qualified Hydra.Core.Dsl.Lib.Maps     as Maps
import qualified Hydra.Core.Dsl.Lib.Math     as Math
import qualified Hydra.Core.Dsl.Lib.Optionals   as Optionals
import qualified Hydra.Core.Dsl.Lib.Pairs    as Pairs
import qualified Hydra.Core.Dsl.Lib.Sets     as Sets
import qualified Hydra.Core.Dsl.Lib.Strings  as Strings
import qualified Hydra.Core.Overlay.Haskell.Dsl.Literals          as Literals
import qualified Hydra.Core.Overlay.Haskell.Dsl.LiteralTypes      as LiteralTypes
import qualified Hydra.Core.Overlay.Haskell.Dsl.Base         as MetaBase
import qualified Hydra.Core.Overlay.Haskell.Dsl.Meta.Terms        as MetaTerms
import qualified Hydra.Core.Overlay.Haskell.Dsl.Meta.Types        as MetaTypes
import qualified Hydra.Core.Dsl.Packaging       as Packaging
import qualified Hydra.Core.Dsl.Parsing      as Parsing
import           Hydra.Core.Overlay.Haskell.Dsl.Phantoms     as Phantoms
import qualified Hydra.Core.Overlay.Haskell.Dsl.Prims             as Prims
import qualified Hydra.Core.Overlay.Haskell.Dsl.Meta.Tabular           as Tabular
import qualified Hydra.Core.Overlay.Haskell.Dsl.Meta.Testing      as Testing
import qualified Hydra.Core.Overlay.Haskell.Dsl.Terms             as Terms
import qualified Hydra.Core.Overlay.Haskell.Dsl.Tests             as Tests
import qualified Hydra.Core.Dsl.Topology     as Topology
import qualified Hydra.Core.Overlay.Haskell.Dsl.Types             as Types
import qualified Hydra.Core.Dsl.Typing       as Typing
import qualified Hydra.Core.Dsl.Util         as Util
import qualified Hydra.Core.Overlay.Haskell.Dsl.Meta.Variants     as Variants
import           Hydra.Sources.Kernel.Types.All
import           Prelude hiding ((++))
import qualified Data.Int                    as I
import qualified Data.List                   as L
import qualified Data.Map                    as M
import qualified Data.Set                    as S
import qualified Data.Maybe                  as Y

import qualified Hydra.Sources.Kernel.Terms.Rewriting as Rewriting
import qualified Hydra.Sources.Kernel.Terms.Sorting as Sorting


ns :: ModuleName
ns = ModuleName "hydra.core.strip"

define :: String -> TypedTerm a -> TypedTermDefinition a
define = definitionInModuleName ns

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = definitions,
            moduleDependencies = Bootstrap.unqualifiedDep <$> ([Rewriting.ns] L.++ kernelTypesModuleNames),
            moduleMetadata = Bootstrap.descriptionMetadata (Just ("Annotation and type stripping and normalization"))}
  where
   definitions = [
     toDefinition deannotateAndDetypeTerm,
     toDefinition deannotateTerm,
     toDefinition deannotateType,
     toDefinition deannotateTypeParameters,
     toDefinition deannotateTypeRecursive,
     toDefinition deannotateTypeSchemeRecursive,
     toDefinition detypeTerm,
     toDefinition removeTermAnnotations,
     toDefinition removeTypeAnnotations,
     toDefinition removeTypeAnnotationsFromTerm,
     toDefinition removeTypesFromTerm,
     toDefinition stripTypeLambdas]

deannotateAndDetypeTerm :: TypedTermDefinition (Term -> Term)
deannotateAndDetypeTerm = define "deannotateAndDetypeTerm" $
  doc "Strip type annotations from the top levels of a term" $
  "t" ~> match _Term (var "t")
    (Just $ var "t") [
    _Term_annotated>>: "at" ~> deannotateAndDetypeTerm @@ (Core.annotatedTermBody $ var "at"),
    _Term_typeApplication>>: "tt" ~> deannotateAndDetypeTerm @@ (Core.typeApplicationTermBody $ var "tt"),
    _Term_typeLambda>>: "ta" ~> deannotateAndDetypeTerm @@ (Core.typeLambdaBody $ var "ta")]

deannotateTerm :: TypedTermDefinition (Term -> Term)
deannotateTerm = define "deannotateTerm" $
  doc "Strip all annotations (including System F type annotations) from the top levels of a term" $
  "t" ~> match _Term (var "t")
    (Just $ var "t") [
    _Term_annotated>>: "at" ~> deannotateTerm @@ (Core.annotatedTermBody $ var "at")]

deannotateType :: TypedTermDefinition (Type -> Type)
deannotateType = define "deannotateType" $
  doc "Strip all annotations from a term" $
  "t" ~> match _Type (var "t")
    (Just $ var "t") [
    _Type_annotated>>: deannotateType <.> (project _AnnotatedType _AnnotatedType_body)]

deannotateTypeParameters :: TypedTermDefinition (Type -> Type)
deannotateTypeParameters = define "deannotateTypeParameters" $
  doc "Strip any top-level type lambdas from a type, extracting the (possibly nested) type body" $
  "t" ~> match _Type (deannotateType @@ var "t")
    (Just $ var "t") [
    _Type_forall>>: "lt" ~> deannotateTypeParameters @@ (project _ForallType _ForallType_body @@ var "lt")]

deannotateTypeRecursive :: TypedTermDefinition (Type -> Type)
deannotateTypeRecursive = define "deannotateTypeRecursive" $
  doc "Recursively strip all annotations from a type" $
  "typ" ~>
  "strip" <~ ("recurse" ~> "typ" ~>
    "rewritten" <~ var "recurse" @@ var "typ" $
    match _Type (var "rewritten")
      (Just $ var "rewritten") [
      _Type_annotated>>: "at" ~> Core.annotatedTypeBody $ var "at"]) $
  Rewriting.rewriteType @@ var "strip" @@ var "typ"

deannotateTypeSchemeRecursive :: TypedTermDefinition (TypeScheme -> TypeScheme)
deannotateTypeSchemeRecursive = define "deannotateTypeSchemeRecursive" $
  doc "Recursively strip all annotations from a type scheme" $
  "ts" ~>
  "vars" <~ Core.typeSchemeVariables (var "ts") $
  "typ" <~ Core.typeSchemeBody (var "ts") $
  "constraints" <~ Core.typeSchemeConstraints (var "ts") $
  Core.typeScheme (var "vars") (deannotateTypeRecursive @@ var "typ") (var "constraints")

detypeTerm :: TypedTermDefinition (Term -> Term)
detypeTerm = define "detypeTerm" $
  doc "Strip System F type annotations from the top levels of a term, but leave application-specific annotations intact" $
  "t" ~> match _Term (var "t")
    (Just $ var "t") [
    _Term_annotated>>: "at" ~>
       "subj" <~ Core.annotatedTermBody (var "at") $
       "ann" <~ Core.annotatedTermAnnotation (var "at") $
       Core.termAnnotated $ Core.annotatedTerm (detypeTerm @@ var "subj") (var "ann"),
    _Term_typeApplication>>: "tt" ~> deannotateAndDetypeTerm @@ (Core.typeApplicationTermBody $ var "tt"),
    _Term_typeLambda>>: "ta" ~> deannotateAndDetypeTerm @@ (Core.typeLambdaBody $ var "ta")]

removeTermAnnotations :: TypedTermDefinition (Term -> Term)
removeTermAnnotations = define "removeTermAnnotations" $
  doc "Recursively remove term annotations, including within subterms" $
  "term" ~>
  "remove" <~ ("recurse" ~> "term" ~>
    "rewritten" <~ var "recurse" @@ var "term" $
    match _Term (var "term")
      (Just $ var "rewritten") [
      _Term_annotated>>: "at" ~> Core.annotatedTermBody $ var "at"]) $
  Rewriting.rewriteTerm @@ var "remove" @@ var "term"

removeTypeAnnotations :: TypedTermDefinition (Type -> Type)
removeTypeAnnotations = define "removeTypeAnnotations" $
  doc "Recursively remove type annotations, including within subtypes" $
  "typ" ~>
  "remove" <~ ("recurse" ~> "typ" ~>
    "rewritten" <~ var "recurse" @@ var "typ" $
    match _Type (var "rewritten")
      (Just $ var "rewritten") [
      _Type_annotated>>: "at" ~> Core.annotatedTypeBody $ var "at"]) $
  Rewriting.rewriteType @@ var "remove" @@ var "typ"

removeTypeAnnotationsFromTerm :: TypedTermDefinition (Term -> Term)
removeTypeAnnotationsFromTerm = define "removeTypeAnnotationsFromTerm" $
  doc "Strip type annotations (TypeLambda, TypeApplication, binding type schemes) from terms while preserving lambda domain types and other annotations" $
  "term" ~>
  "strip" <~ ("recurse" ~> "term" ~>
    "rewritten" <~ var "recurse" @@ var "term" $
    "stripBinding" <~ ("b" ~> Core.binding
      (Core.bindingName $ var "b")
      (Core.bindingTerm $ var "b")
      nothing) $
    match _Term (var "rewritten")
      (Just $ var "rewritten") [
      _Term_let>>: "lt" ~> Core.termLet $ Core.let_
        (Lists.map (var "stripBinding") (Core.letBindings $ var "lt"))
        (Core.letBody $ var "lt"),
      _Term_typeApplication>>: "tt" ~> Core.typeApplicationTermBody $ var "tt",
      _Term_typeLambda>>: "ta" ~> Core.typeLambdaBody $ var "ta"]) $
  Rewriting.rewriteTerm @@ var "strip" @@ var "term"

removeTypesFromTerm :: TypedTermDefinition (Term -> Term)
removeTypesFromTerm = define "removeTypesFromTerm" $
  doc "Strip type annotations from terms while preserving other annotations" $
  "term" ~>
  "strip" <~ ("recurse" ~> "term" ~>
    "rewritten" <~ var "recurse" @@ var "term" $
    "stripBinding" <~ ("b" ~> Core.binding
      (Core.bindingName $ var "b")
      (Core.bindingTerm $ var "b")
      nothing) $
    match _Term (var "rewritten")
      (Just $ var "rewritten") [
      _Term_lambda>>: "l" ~> Core.termLambda $ Core.lambda
        (Core.lambdaParameter $ var "l")
        nothing
        (Core.lambdaBody $ var "l"),
      _Term_let>>: "lt" ~> Core.termLet $ Core.let_
        (Lists.map (var "stripBinding") (Core.letBindings $ var "lt"))
        (Core.letBody $ var "lt"),
      _Term_typeApplication>>: "tt" ~> Core.typeApplicationTermBody $ var "tt",
      _Term_typeLambda>>: "ta" ~> Core.typeLambdaBody $ var "ta"]) $
  Rewriting.rewriteTerm @@ var "strip" @@ var "term"

stripTypeLambdas :: TypedTermDefinition (Term -> Term)
stripTypeLambdas = define "stripTypeLambdas" $
  doc "Strip outer type lambda wrappers from a term, preserving type application wrappers and annotations" $
  "t" ~> match _Term (var "t")
    (Just $ var "t") [
    _Term_annotated>>: "at" ~>
       "subj" <~ Core.annotatedTermBody (var "at") $
       "ann" <~ Core.annotatedTermAnnotation (var "at") $
       Core.termAnnotated $ Core.annotatedTerm (stripTypeLambdas @@ var "subj") (var "ann"),
    _Term_typeLambda>>: "ta" ~> stripTypeLambdas @@ (Core.typeLambdaBody $ var "ta")]
