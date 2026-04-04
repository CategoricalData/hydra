
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
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Paths        as Paths
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
import qualified Hydra.Dsl.Util         as Util
import qualified Hydra.Dsl.Meta.Variants     as Variants
import           Hydra.Sources.Kernel.Types.All
import           Prelude hiding ((++))
import qualified Data.Int                    as I
import qualified Data.List                   as L
import qualified Data.Map                    as M
import qualified Data.Set                    as S
import qualified Data.Maybe                  as Y

import qualified Hydra.Sources.Kernel.Terms.Rewriting as Rewriting
import qualified Hydra.Sources.Kernel.Terms.Sorting as Sorting


ns :: Namespace
ns = Namespace "hydra.strip"

define :: String -> TTerm a -> TTermDefinition a
define = definitionInNamespace ns

module_ :: Module
module_ = Module ns elements
    [Rewriting.ns]
    kernelTypesNamespaces $
    Just ("Annotation and type stripping and normalization")
  where
   elements = [
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

deannotateAndDetypeTerm :: TTermDefinition (Term -> Term)
deannotateAndDetypeTerm = define "deannotateAndDetypeTerm" $
  doc "Strip type annotations from the top levels of a term" $
  "t" ~> cases _Term (var "t")
    (Just $ var "t") [
    _Term_annotated>>: "at" ~> deannotateAndDetypeTerm @@ (Core.annotatedTermBody $ var "at"),
    _Term_typeApplication>>: "tt" ~> deannotateAndDetypeTerm @@ (Core.typeApplicationTermBody $ var "tt"),
    _Term_typeLambda>>: "ta" ~> deannotateAndDetypeTerm @@ (Core.typeLambdaBody $ var "ta")]

deannotateTerm :: TTermDefinition (Term -> Term)
deannotateTerm = define "deannotateTerm" $
  doc "Strip all annotations (including System F type annotations) from the top levels of a term" $
  "t" ~> cases _Term (var "t")
    (Just $ var "t") [
    _Term_annotated>>: "at" ~> deannotateTerm @@ (Core.annotatedTermBody $ var "at")]

deannotateType :: TTermDefinition (Type -> Type)
deannotateType = define "deannotateType" $
  doc "Strip all annotations from a term" $
  "t" ~> cases _Type (var "t")
    (Just $ var "t") [
    _Type_annotated>>: deannotateType <.> (project _AnnotatedType _AnnotatedType_body)]

deannotateTypeParameters :: TTermDefinition (Type -> Type)
deannotateTypeParameters = define "deannotateTypeParameters" $
  doc "Strip any top-level type lambdas from a type, extracting the (possibly nested) type body" $
  "t" ~> cases _Type (deannotateType @@ var "t")
    (Just $ var "t") [
    _Type_forall>>: "lt" ~> deannotateTypeParameters @@ (project _ForallType _ForallType_body @@ var "lt")]

deannotateTypeRecursive :: TTermDefinition (Type -> Type)
deannotateTypeRecursive = define "deannotateTypeRecursive" $
  doc "Recursively strip all annotations from a type" $
  "typ" ~>
  "strip" <~ ("recurse" ~> "typ" ~>
    "rewritten" <~ var "recurse" @@ var "typ" $
    cases _Type (var "rewritten")
      (Just $ var "rewritten") [
      _Type_annotated>>: "at" ~> Core.annotatedTypeBody $ var "at"]) $
  Rewriting.rewriteType @@ var "strip" @@ var "typ"

deannotateTypeSchemeRecursive :: TTermDefinition (TypeScheme -> TypeScheme)
deannotateTypeSchemeRecursive = define "deannotateTypeSchemeRecursive" $
  doc "Recursively strip all annotations from a type scheme" $
  "ts" ~>
  "vars" <~ Core.typeSchemeVariables (var "ts") $
  "typ" <~ Core.typeSchemeType (var "ts") $
  "constraints" <~ Core.typeSchemeConstraints (var "ts") $
  Core.typeScheme (var "vars") (deannotateTypeRecursive @@ var "typ") (var "constraints")

detypeTerm :: TTermDefinition (Term -> Term)
detypeTerm = define "detypeTerm" $
  doc "Strip System F type annotations from the top levels of a term, but leave application-specific annotations intact" $
  "t" ~> cases _Term (var "t")
    (Just $ var "t") [
    _Term_annotated>>: "at" ~>
       "subj" <~ Core.annotatedTermBody (var "at") $
       "ann" <~ Core.annotatedTermAnnotation (var "at") $
       Core.termAnnotated $ Core.annotatedTerm (detypeTerm @@ var "subj") (var "ann"),
    _Term_typeApplication>>: "tt" ~> deannotateAndDetypeTerm @@ (Core.typeApplicationTermBody $ var "tt"),
    _Term_typeLambda>>: "ta" ~> deannotateAndDetypeTerm @@ (Core.typeLambdaBody $ var "ta")]

stripTypeLambdas :: TTermDefinition (Term -> Term)
stripTypeLambdas = define "stripTypeLambdas" $
  doc "Strip outer type lambda wrappers from a term, preserving type application wrappers and annotations" $
  "t" ~> cases _Term (var "t")
    (Just $ var "t") [
    _Term_annotated>>: "at" ~>
       "subj" <~ Core.annotatedTermBody (var "at") $
       "ann" <~ Core.annotatedTermAnnotation (var "at") $
       Core.termAnnotated $ Core.annotatedTerm (stripTypeLambdas @@ var "subj") (var "ann"),
    _Term_typeLambda>>: "ta" ~> stripTypeLambdas @@ (Core.typeLambdaBody $ var "ta")]

removeTermAnnotations :: TTermDefinition (Term -> Term)
removeTermAnnotations = define "removeTermAnnotations" $
  doc "Recursively remove term annotations, including within subterms" $
  "term" ~>
  "remove" <~ ("recurse" ~> "term" ~>
    "rewritten" <~ var "recurse" @@ var "term" $
    cases _Term (var "term")
      (Just $ var "rewritten") [
      _Term_annotated>>: "at" ~> Core.annotatedTermBody $ var "at"]) $
  Rewriting.rewriteTerm @@ var "remove" @@ var "term"

removeTypeAnnotations :: TTermDefinition (Type -> Type)
removeTypeAnnotations = define "removeTypeAnnotations" $
  doc "Recursively remove type annotations, including within subtypes" $
  "typ" ~>
  "remove" <~ ("recurse" ~> "typ" ~>
    "rewritten" <~ var "recurse" @@ var "typ" $
    cases _Type (var "rewritten")
      (Just $ var "rewritten") [
      _Type_annotated>>: "at" ~> Core.annotatedTypeBody $ var "at"]) $
  Rewriting.rewriteType @@ var "remove" @@ var "typ"

removeTypesFromTerm :: TTermDefinition (Term -> Term)
removeTypesFromTerm = define "removeTypesFromTerm" $
  doc "Strip type annotations from terms while preserving other annotations" $
  "term" ~>
  "strip" <~ ("recurse" ~> "term" ~>
    "rewritten" <~ var "recurse" @@ var "term" $
    "stripBinding" <~ ("b" ~> Core.binding
      (Core.bindingName $ var "b")
      (Core.bindingTerm $ var "b")
      nothing) $
    cases _Term (var "rewritten")
      (Just $ var "rewritten") [
      _Term_function>>: "f" ~> cases _Function (var "f")
        (Just $ Core.termFunction $ var "f") [
        _Function_elimination>>: "e" ~> Core.termFunction $ Core.functionElimination $ var "e",
        _Function_lambda>>: "l" ~> Core.termFunction $ Core.functionLambda $ Core.lambda
          (Core.lambdaParameter $ var "l")
          nothing
          (Core.lambdaBody $ var "l")],
      _Term_let>>: "lt" ~> Core.termLet $ Core.let_
        (Lists.map (var "stripBinding") (Core.letBindings $ var "lt"))
        (Core.letBody $ var "lt"),
      _Term_typeApplication>>: "tt" ~> Core.typeApplicationTermBody $ var "tt",
      _Term_typeLambda>>: "ta" ~> Core.typeLambdaBody $ var "ta"]) $
  Rewriting.rewriteTerm @@ var "strip" @@ var "term"

removeTypeAnnotationsFromTerm :: TTermDefinition (Term -> Term)
removeTypeAnnotationsFromTerm = define "removeTypeAnnotationsFromTerm" $
  doc "Strip type annotations (TypeLambda, TypeApplication, binding type schemes) from terms while preserving lambda domain types and other annotations" $
  "term" ~>
  "strip" <~ ("recurse" ~> "term" ~>
    "rewritten" <~ var "recurse" @@ var "term" $
    "stripBinding" <~ ("b" ~> Core.binding
      (Core.bindingName $ var "b")
      (Core.bindingTerm $ var "b")
      nothing) $
    cases _Term (var "rewritten")
      (Just $ var "rewritten") [
      _Term_let>>: "lt" ~> Core.termLet $ Core.let_
        (Lists.map (var "stripBinding") (Core.letBindings $ var "lt"))
        (Core.letBody $ var "lt"),
      _Term_typeApplication>>: "tt" ~> Core.typeApplicationTermBody $ var "tt",
      _Term_typeLambda>>: "ta" ~> Core.typeLambdaBody $ var "ta"]) $
  Rewriting.rewriteTerm @@ var "strip" @@ var "term"
