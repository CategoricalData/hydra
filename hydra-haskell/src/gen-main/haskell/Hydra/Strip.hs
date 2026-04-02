-- Note: this is an automatically generated file. Do not edit.

-- | Annotation and type stripping and normalization

module Hydra.Strip where

import qualified Hydra.Core as Core
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Rewriting as Rewriting
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | Strip type annotations from the top levels of a term
deannotateAndDetypeTerm :: Core.Term -> Core.Term
deannotateAndDetypeTerm t =
    case t of
      Core.TermAnnotated v0 -> deannotateAndDetypeTerm (Core.annotatedTermBody v0)
      Core.TermTypeApplication v0 -> deannotateAndDetypeTerm (Core.typeApplicationTermBody v0)
      Core.TermTypeLambda v0 -> deannotateAndDetypeTerm (Core.typeLambdaBody v0)
      _ -> t

-- | Strip all annotations (including System F type annotations) from the top levels of a term
deannotateTerm :: Core.Term -> Core.Term
deannotateTerm t =
    case t of
      Core.TermAnnotated v0 -> deannotateTerm (Core.annotatedTermBody v0)
      _ -> t

-- | Strip all annotations from a term
deannotateType :: Core.Type -> Core.Type
deannotateType t =
    case t of
      Core.TypeAnnotated v0 -> deannotateType (Core.annotatedTypeBody v0)
      _ -> t

-- | Strip any top-level type lambdas from a type, extracting the (possibly nested) type body
deannotateTypeParameters :: Core.Type -> Core.Type
deannotateTypeParameters t =
    case (deannotateType t) of
      Core.TypeForall v0 -> deannotateTypeParameters (Core.forallTypeBody v0)
      _ -> t

-- | Recursively strip all annotations from a type
deannotateTypeRecursive :: Core.Type -> Core.Type
deannotateTypeRecursive typ =

      let strip =
              \recurse -> \typ ->
                let rewritten = recurse typ
                in case rewritten of
                  Core.TypeAnnotated v0 -> Core.annotatedTypeBody v0
                  _ -> rewritten
      in (Rewriting.rewriteType strip typ)

-- | Recursively strip all annotations from a type scheme
deannotateTypeSchemeRecursive :: Core.TypeScheme -> Core.TypeScheme
deannotateTypeSchemeRecursive ts =

      let vars = Core.typeSchemeVariables ts
          typ = Core.typeSchemeType ts
          constraints = Core.typeSchemeConstraints ts
      in Core.TypeScheme {
        Core.typeSchemeVariables = vars,
        Core.typeSchemeType = (deannotateTypeRecursive typ),
        Core.typeSchemeConstraints = constraints}

-- | Strip System F type annotations from the top levels of a term, but leave application-specific annotations intact
detypeTerm :: Core.Term -> Core.Term
detypeTerm t =
    case t of
      Core.TermAnnotated v0 ->
        let subj = Core.annotatedTermBody v0
            ann = Core.annotatedTermAnnotation v0
        in (Core.TermAnnotated (Core.AnnotatedTerm {
          Core.annotatedTermBody = (detypeTerm subj),
          Core.annotatedTermAnnotation = ann}))
      Core.TermTypeApplication v0 -> deannotateAndDetypeTerm (Core.typeApplicationTermBody v0)
      Core.TermTypeLambda v0 -> deannotateAndDetypeTerm (Core.typeLambdaBody v0)
      _ -> t

-- | Recursively remove term annotations, including within subterms
removeTermAnnotations :: Core.Term -> Core.Term
removeTermAnnotations term =

      let remove =
              \recurse -> \term ->
                let rewritten = recurse term
                in case term of
                  Core.TermAnnotated v0 -> Core.annotatedTermBody v0
                  _ -> rewritten
      in (Rewriting.rewriteTerm remove term)

-- | Recursively remove type annotations, including within subtypes
removeTypeAnnotations :: Core.Type -> Core.Type
removeTypeAnnotations typ =

      let remove =
              \recurse -> \typ ->
                let rewritten = recurse typ
                in case rewritten of
                  Core.TypeAnnotated v0 -> Core.annotatedTypeBody v0
                  _ -> rewritten
      in (Rewriting.rewriteType remove typ)

-- | Strip type annotations (TypeLambda, TypeApplication, binding type schemes) from terms while preserving lambda domain types and other annotations
removeTypeAnnotationsFromTerm :: Core.Term -> Core.Term
removeTypeAnnotationsFromTerm term =

      let strip =
              \recurse -> \term ->
                let rewritten = recurse term
                    stripBinding =
                            \b -> Core.Binding {
                              Core.bindingName = (Core.bindingName b),
                              Core.bindingTerm = (Core.bindingTerm b),
                              Core.bindingType = Nothing}
                in case rewritten of
                  Core.TermLet v0 -> Core.TermLet (Core.Let {
                    Core.letBindings = (Lists.map stripBinding (Core.letBindings v0)),
                    Core.letBody = (Core.letBody v0)})
                  Core.TermTypeApplication v0 -> Core.typeApplicationTermBody v0
                  Core.TermTypeLambda v0 -> Core.typeLambdaBody v0
                  _ -> rewritten
      in (Rewriting.rewriteTerm strip term)

-- | Strip type annotations from terms while preserving other annotations
removeTypesFromTerm :: Core.Term -> Core.Term
removeTypesFromTerm term =

      let strip =
              \recurse -> \term ->
                let rewritten = recurse term
                    stripBinding =
                            \b -> Core.Binding {
                              Core.bindingName = (Core.bindingName b),
                              Core.bindingTerm = (Core.bindingTerm b),
                              Core.bindingType = Nothing}
                in case rewritten of
                  Core.TermFunction v0 -> case v0 of
                    Core.FunctionElimination v1 -> Core.TermFunction (Core.FunctionElimination v1)
                    Core.FunctionLambda v1 -> Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.lambdaParameter v1),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.lambdaBody v1)}))
                    _ -> Core.TermFunction v0
                  Core.TermLet v0 -> Core.TermLet (Core.Let {
                    Core.letBindings = (Lists.map stripBinding (Core.letBindings v0)),
                    Core.letBody = (Core.letBody v0)})
                  Core.TermTypeApplication v0 -> Core.typeApplicationTermBody v0
                  Core.TermTypeLambda v0 -> Core.typeLambdaBody v0
                  _ -> rewritten
      in (Rewriting.rewriteTerm strip term)

-- | Strip outer type lambda wrappers from a term, preserving type application wrappers and annotations
stripTypeLambdas :: Core.Term -> Core.Term
stripTypeLambdas t =
    case t of
      Core.TermAnnotated v0 ->
        let subj = Core.annotatedTermBody v0
            ann = Core.annotatedTermAnnotation v0
        in (Core.TermAnnotated (Core.AnnotatedTerm {
          Core.annotatedTermBody = (stripTypeLambdas subj),
          Core.annotatedTermAnnotation = ann}))
      Core.TermTypeLambda v0 -> stripTypeLambdas (Core.typeLambdaBody v0)
      _ -> t
