-- Note: this is an automatically generated file. Do not edit.

-- | DSL functions for hydra.ext.fr.inria.coq.syntax

module Hydra.Dsl.Ext.Fr.Inria.Coq.Syntax where

import qualified Hydra.Core as Core
import qualified Hydra.Ext.Fr.Inria.Coq.Syntax as Syntax
import qualified Hydra.Phantoms as Phantoms
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

annotatedApplication :: Phantoms.TTerm Syntax.QualidAnnotated -> Phantoms.TTerm [Syntax.Term1] -> Phantoms.TTerm Syntax.AnnotatedApplication
annotatedApplication annot terms =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.AnnotatedApplication"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annot"),
          Core.fieldTerm = (Phantoms.unTTerm annot)},
        Core.Field {
          Core.fieldName = (Core.Name "terms"),
          Core.fieldTerm = (Phantoms.unTTerm terms)}]}))

annotatedApplicationAnnot :: Phantoms.TTerm Syntax.AnnotatedApplication -> Phantoms.TTerm Syntax.QualidAnnotated
annotatedApplicationAnnot x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.AnnotatedApplication"),
        Core.projectionField = (Core.Name "annot")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

annotatedApplicationTerms :: Phantoms.TTerm Syntax.AnnotatedApplication -> Phantoms.TTerm [Syntax.Term1]
annotatedApplicationTerms x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.AnnotatedApplication"),
        Core.projectionField = (Core.Name "terms")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

annotatedApplicationWithAnnot :: Phantoms.TTerm Syntax.AnnotatedApplication -> Phantoms.TTerm Syntax.QualidAnnotated -> Phantoms.TTerm Syntax.AnnotatedApplication
annotatedApplicationWithAnnot original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.AnnotatedApplication"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annot"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "terms"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.AnnotatedApplication"),
              Core.projectionField = (Core.Name "terms")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

annotatedApplicationWithTerms :: Phantoms.TTerm Syntax.AnnotatedApplication -> Phantoms.TTerm [Syntax.Term1] -> Phantoms.TTerm Syntax.AnnotatedApplication
annotatedApplicationWithTerms original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.AnnotatedApplication"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annot"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.AnnotatedApplication"),
              Core.projectionField = (Core.Name "annot")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "terms"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

applicationNormal :: Phantoms.TTerm Syntax.NormalApplication -> Phantoms.TTerm Syntax.Application
applicationNormal x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Application"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "normal"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

applicationAnnotated :: Phantoms.TTerm Syntax.AnnotatedApplication -> Phantoms.TTerm Syntax.Application
applicationAnnotated x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Application"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "annotated"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

argIdent :: Phantoms.TTerm Syntax.IdentArg -> Phantoms.TTerm Syntax.Arg
argIdent x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Arg"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "ident"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

argNatural :: Phantoms.TTerm Syntax.NaturalArg -> Phantoms.TTerm Syntax.Arg
argNatural x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Arg"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "natural"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

argTerm :: Phantoms.TTerm Syntax.Term1 -> Phantoms.TTerm Syntax.Arg
argTerm x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Arg"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "term"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

binderName :: Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.Binder
binderName x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Binder"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "name"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

binderType :: Phantoms.TTerm Syntax.TypeBinders -> Phantoms.TTerm Syntax.Binder
binderType x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Binder"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "type"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

binderTerm :: Phantoms.TTerm Syntax.LetBinder -> Phantoms.TTerm Syntax.Binder
binderTerm x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Binder"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "term"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

binderImplicit :: Phantoms.TTerm Syntax.ImplicitBinders -> Phantoms.TTerm Syntax.Binder
binderImplicit x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Binder"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "implicit"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

binderGeneralizing :: Phantoms.TTerm Syntax.GeneralizingBinder -> Phantoms.TTerm Syntax.Binder
binderGeneralizing x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Binder"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "generalizing"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

binderPattern :: Phantoms.TTerm Syntax.Pattern0 -> Phantoms.TTerm Syntax.Binder
binderPattern x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Binder"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "pattern"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

caseItem :: Phantoms.TTerm Syntax.Term100 -> Phantoms.TTerm (Maybe Syntax.Name) -> Phantoms.TTerm (Maybe Syntax.Pattern) -> Phantoms.TTerm Syntax.CaseItem
caseItem term as in_ =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.CaseItem"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Phantoms.unTTerm term)},
        Core.Field {
          Core.fieldName = (Core.Name "as"),
          Core.fieldTerm = (Phantoms.unTTerm as)},
        Core.Field {
          Core.fieldName = (Core.Name "in"),
          Core.fieldTerm = (Phantoms.unTTerm in_)}]}))

caseItemTerm :: Phantoms.TTerm Syntax.CaseItem -> Phantoms.TTerm Syntax.Term100
caseItemTerm x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.CaseItem"),
        Core.projectionField = (Core.Name "term")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

caseItemAs :: Phantoms.TTerm Syntax.CaseItem -> Phantoms.TTerm (Maybe Syntax.Name)
caseItemAs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.CaseItem"),
        Core.projectionField = (Core.Name "as")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

caseItemIn :: Phantoms.TTerm Syntax.CaseItem -> Phantoms.TTerm (Maybe Syntax.Pattern)
caseItemIn x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.CaseItem"),
        Core.projectionField = (Core.Name "in")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

caseItemWithTerm :: Phantoms.TTerm Syntax.CaseItem -> Phantoms.TTerm Syntax.Term100 -> Phantoms.TTerm Syntax.CaseItem
caseItemWithTerm original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.CaseItem"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "as"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.CaseItem"),
              Core.projectionField = (Core.Name "as")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "in"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.CaseItem"),
              Core.projectionField = (Core.Name "in")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

caseItemWithAs :: Phantoms.TTerm Syntax.CaseItem -> Phantoms.TTerm (Maybe Syntax.Name) -> Phantoms.TTerm Syntax.CaseItem
caseItemWithAs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.CaseItem"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.CaseItem"),
              Core.projectionField = (Core.Name "term")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "as"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "in"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.CaseItem"),
              Core.projectionField = (Core.Name "in")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

caseItemWithIn :: Phantoms.TTerm Syntax.CaseItem -> Phantoms.TTerm (Maybe Syntax.Pattern) -> Phantoms.TTerm Syntax.CaseItem
caseItemWithIn original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.CaseItem"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.CaseItem"),
              Core.projectionField = (Core.Name "term")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "as"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.CaseItem"),
              Core.projectionField = (Core.Name "as")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "in"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

cofix :: Phantoms.TTerm Syntax.CofixBody -> Phantoms.TTerm (Maybe Syntax.CofixQual) -> Phantoms.TTerm Syntax.Cofix
cofix body qual =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Cofix"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm body)},
        Core.Field {
          Core.fieldName = (Core.Name "qual"),
          Core.fieldTerm = (Phantoms.unTTerm qual)}]}))

cofixBody :: Phantoms.TTerm Syntax.Cofix -> Phantoms.TTerm Syntax.CofixBody
cofixBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Cofix"),
        Core.projectionField = (Core.Name "body")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

cofixQual :: Phantoms.TTerm Syntax.Cofix -> Phantoms.TTerm (Maybe Syntax.CofixQual)
cofixQual x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Cofix"),
        Core.projectionField = (Core.Name "qual")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

cofixWithBody :: Phantoms.TTerm Syntax.Cofix -> Phantoms.TTerm Syntax.CofixBody -> Phantoms.TTerm Syntax.Cofix
cofixWithBody original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Cofix"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "qual"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Cofix"),
              Core.projectionField = (Core.Name "qual")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

cofixWithQual :: Phantoms.TTerm Syntax.Cofix -> Phantoms.TTerm (Maybe Syntax.CofixQual) -> Phantoms.TTerm Syntax.Cofix
cofixWithQual original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Cofix"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Cofix"),
              Core.projectionField = (Core.Name "body")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "qual"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

cofixBody_ :: Phantoms.TTerm Syntax.Ident -> Phantoms.TTerm [Syntax.Binder] -> Phantoms.TTerm (Maybe Syntax.Type) -> Phantoms.TTerm Syntax.Term -> Phantoms.TTerm Syntax.CofixBody
cofixBody_ ident binders type_ term =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.CofixBody"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "ident"),
          Core.fieldTerm = (Phantoms.unTTerm ident)},
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Phantoms.unTTerm binders)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm type_)},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Phantoms.unTTerm term)}]}))

cofixBodyIdent :: Phantoms.TTerm Syntax.CofixBody -> Phantoms.TTerm Syntax.Ident
cofixBodyIdent x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.CofixBody"),
        Core.projectionField = (Core.Name "ident")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

cofixBodyBinders :: Phantoms.TTerm Syntax.CofixBody -> Phantoms.TTerm [Syntax.Binder]
cofixBodyBinders x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.CofixBody"),
        Core.projectionField = (Core.Name "binders")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

cofixBodyType :: Phantoms.TTerm Syntax.CofixBody -> Phantoms.TTerm (Maybe Syntax.Type)
cofixBodyType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.CofixBody"),
        Core.projectionField = (Core.Name "type")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

cofixBodyTerm :: Phantoms.TTerm Syntax.CofixBody -> Phantoms.TTerm Syntax.Term
cofixBodyTerm x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.CofixBody"),
        Core.projectionField = (Core.Name "term")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

cofixBodyWithIdent :: Phantoms.TTerm Syntax.CofixBody -> Phantoms.TTerm Syntax.Ident -> Phantoms.TTerm Syntax.CofixBody
cofixBodyWithIdent original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.CofixBody"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "ident"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.CofixBody"),
              Core.projectionField = (Core.Name "binders")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.CofixBody"),
              Core.projectionField = (Core.Name "type")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.CofixBody"),
              Core.projectionField = (Core.Name "term")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

cofixBodyWithBinders :: Phantoms.TTerm Syntax.CofixBody -> Phantoms.TTerm [Syntax.Binder] -> Phantoms.TTerm Syntax.CofixBody
cofixBodyWithBinders original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.CofixBody"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "ident"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.CofixBody"),
              Core.projectionField = (Core.Name "ident")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.CofixBody"),
              Core.projectionField = (Core.Name "type")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.CofixBody"),
              Core.projectionField = (Core.Name "term")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

cofixBodyWithType :: Phantoms.TTerm Syntax.CofixBody -> Phantoms.TTerm (Maybe Syntax.Type) -> Phantoms.TTerm Syntax.CofixBody
cofixBodyWithType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.CofixBody"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "ident"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.CofixBody"),
              Core.projectionField = (Core.Name "ident")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.CofixBody"),
              Core.projectionField = (Core.Name "binders")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.CofixBody"),
              Core.projectionField = (Core.Name "term")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

cofixBodyWithTerm :: Phantoms.TTerm Syntax.CofixBody -> Phantoms.TTerm Syntax.Term -> Phantoms.TTerm Syntax.CofixBody
cofixBodyWithTerm original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.CofixBody"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "ident"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.CofixBody"),
              Core.projectionField = (Core.Name "ident")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.CofixBody"),
              Core.projectionField = (Core.Name "binders")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.CofixBody"),
              Core.projectionField = (Core.Name "type")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

cofixQualIn :: Phantoms.TTerm Syntax.Term -> Phantoms.TTerm Syntax.CofixQual
cofixQualIn x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.CofixQual"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "in"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

cofixQualWith :: Phantoms.TTerm Syntax.CofixWith -> Phantoms.TTerm Syntax.CofixQual
cofixQualWith x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.CofixQual"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "with"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

cofixWith :: Phantoms.TTerm [Syntax.CofixBody] -> Phantoms.TTerm (Maybe Syntax.Ident) -> Phantoms.TTerm Syntax.CofixWith
cofixWith with for =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.CofixWith"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "with"),
          Core.fieldTerm = (Phantoms.unTTerm with)},
        Core.Field {
          Core.fieldName = (Core.Name "for"),
          Core.fieldTerm = (Phantoms.unTTerm for)}]}))

cofixWithWith :: Phantoms.TTerm Syntax.CofixWith -> Phantoms.TTerm [Syntax.CofixBody]
cofixWithWith x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.CofixWith"),
        Core.projectionField = (Core.Name "with")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

cofixWithFor :: Phantoms.TTerm Syntax.CofixWith -> Phantoms.TTerm (Maybe Syntax.Ident)
cofixWithFor x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.CofixWith"),
        Core.projectionField = (Core.Name "for")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

cofixWithWithWith :: Phantoms.TTerm Syntax.CofixWith -> Phantoms.TTerm [Syntax.CofixBody] -> Phantoms.TTerm Syntax.CofixWith
cofixWithWithWith original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.CofixWith"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "with"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "for"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.CofixWith"),
              Core.projectionField = (Core.Name "for")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

cofixWithWithFor :: Phantoms.TTerm Syntax.CofixWith -> Phantoms.TTerm (Maybe Syntax.Ident) -> Phantoms.TTerm Syntax.CofixWith
cofixWithWithFor original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.CofixWith"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "with"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.CofixWith"),
              Core.projectionField = (Core.Name "with")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "for"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

equation :: Phantoms.TTerm [[Syntax.Pattern]] -> Phantoms.TTerm Syntax.Term -> Phantoms.TTerm Syntax.Equation
equation pattern term =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Equation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Phantoms.unTTerm pattern)},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Phantoms.unTTerm term)}]}))

equationPattern :: Phantoms.TTerm Syntax.Equation -> Phantoms.TTerm [[Syntax.Pattern]]
equationPattern x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Equation"),
        Core.projectionField = (Core.Name "pattern")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

equationTerm :: Phantoms.TTerm Syntax.Equation -> Phantoms.TTerm Syntax.Term
equationTerm x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Equation"),
        Core.projectionField = (Core.Name "term")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

equationWithPattern :: Phantoms.TTerm Syntax.Equation -> Phantoms.TTerm [[Syntax.Pattern]] -> Phantoms.TTerm Syntax.Equation
equationWithPattern original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Equation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Equation"),
              Core.projectionField = (Core.Name "term")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

equationWithTerm :: Phantoms.TTerm Syntax.Equation -> Phantoms.TTerm Syntax.Term -> Phantoms.TTerm Syntax.Equation
equationWithTerm original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Equation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Equation"),
              Core.projectionField = (Core.Name "pattern")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

existentialVariable :: Phantoms.TTerm Syntax.Ident -> Phantoms.TTerm Syntax.ExistentialVariableVariant -> Phantoms.TTerm Syntax.ExistentialVariable
existentialVariable ident variant =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.ExistentialVariable"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "ident"),
          Core.fieldTerm = (Phantoms.unTTerm ident)},
        Core.Field {
          Core.fieldName = (Core.Name "variant"),
          Core.fieldTerm = (Phantoms.unTTerm variant)}]}))

existentialVariableIdent :: Phantoms.TTerm Syntax.ExistentialVariable -> Phantoms.TTerm Syntax.Ident
existentialVariableIdent x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.ExistentialVariable"),
        Core.projectionField = (Core.Name "ident")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

existentialVariableVariant :: Phantoms.TTerm Syntax.ExistentialVariable -> Phantoms.TTerm Syntax.ExistentialVariableVariant
existentialVariableVariant x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.ExistentialVariable"),
        Core.projectionField = (Core.Name "variant")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

existentialVariableWithIdent :: Phantoms.TTerm Syntax.ExistentialVariable -> Phantoms.TTerm Syntax.Ident -> Phantoms.TTerm Syntax.ExistentialVariable
existentialVariableWithIdent original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.ExistentialVariable"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "ident"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "variant"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.ExistentialVariable"),
              Core.projectionField = (Core.Name "variant")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

existentialVariableWithVariant :: Phantoms.TTerm Syntax.ExistentialVariable -> Phantoms.TTerm Syntax.ExistentialVariableVariant -> Phantoms.TTerm Syntax.ExistentialVariable
existentialVariableWithVariant original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.ExistentialVariable"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "ident"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.ExistentialVariable"),
              Core.projectionField = (Core.Name "ident")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "variant"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

existentialVariableVariantPlaceholder :: Phantoms.TTerm Syntax.ExistentialVariableVariant
existentialVariableVariantPlaceholder =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.ExistentialVariableVariant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "placeholder"),
        Core.fieldTerm = Core.TermUnit}}))

existentialVariableVariantInside1 :: Phantoms.TTerm Syntax.ExistentialVariableVariant
existentialVariableVariantInside1 =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.ExistentialVariableVariant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "inside1"),
        Core.fieldTerm = Core.TermUnit}}))

existentialVariableVariantInside2 :: Phantoms.TTerm Syntax.ExistentialVariableVariant
existentialVariableVariantInside2 =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.ExistentialVariableVariant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "inside2"),
        Core.fieldTerm = Core.TermUnit}}))

existentialVariableVariantOutside :: Phantoms.TTerm (Maybe Syntax.IdentArg) -> Phantoms.TTerm Syntax.ExistentialVariableVariant
existentialVariableVariantOutside x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.ExistentialVariableVariant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "outside"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

fieldIdent :: Phantoms.TTerm Syntax.Ident -> Phantoms.TTerm Syntax.FieldIdent
fieldIdent x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.FieldIdent"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unFieldIdent :: Phantoms.TTerm Syntax.FieldIdent -> Phantoms.TTerm Syntax.Ident
unFieldIdent x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.fr.inria.coq.syntax.FieldIdent")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

fixDecl :: Phantoms.TTerm Syntax.Fix_Decl -> Phantoms.TTerm Syntax.Fix
fixDecl x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Fix"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "decl"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

fixQual :: Phantoms.TTerm (Maybe Syntax.Fix_Qual) -> Phantoms.TTerm Syntax.Fix
fixQual x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Fix"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "qual"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

fixAnnotStruct :: Phantoms.TTerm Syntax.Ident -> Phantoms.TTerm Syntax.FixAnnot
fixAnnotStruct x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.FixAnnot"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "struct"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

fixAnnotWf :: Phantoms.TTerm Syntax.FixAnnot_Wf -> Phantoms.TTerm Syntax.FixAnnot
fixAnnotWf x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.FixAnnot"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "wf"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

fixAnnotMeasure :: Phantoms.TTerm Syntax.FixAnnot_Measure -> Phantoms.TTerm Syntax.FixAnnot
fixAnnotMeasure x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.FixAnnot"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "measure"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

fixAnnot_Measure :: Phantoms.TTerm Syntax.OneTerm -> Phantoms.TTerm (Maybe Syntax.Ident) -> Phantoms.TTerm (Maybe Syntax.OneTerm) -> Phantoms.TTerm Syntax.FixAnnot_Measure
fixAnnot_Measure term ident term2 =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.FixAnnot_Measure"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Phantoms.unTTerm term)},
        Core.Field {
          Core.fieldName = (Core.Name "ident"),
          Core.fieldTerm = (Phantoms.unTTerm ident)},
        Core.Field {
          Core.fieldName = (Core.Name "term2"),
          Core.fieldTerm = (Phantoms.unTTerm term2)}]}))

fixAnnot_MeasureTerm :: Phantoms.TTerm Syntax.FixAnnot_Measure -> Phantoms.TTerm Syntax.OneTerm
fixAnnot_MeasureTerm x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.FixAnnot_Measure"),
        Core.projectionField = (Core.Name "term")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

fixAnnot_MeasureIdent :: Phantoms.TTerm Syntax.FixAnnot_Measure -> Phantoms.TTerm (Maybe Syntax.Ident)
fixAnnot_MeasureIdent x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.FixAnnot_Measure"),
        Core.projectionField = (Core.Name "ident")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

fixAnnot_MeasureTerm2 :: Phantoms.TTerm Syntax.FixAnnot_Measure -> Phantoms.TTerm (Maybe Syntax.OneTerm)
fixAnnot_MeasureTerm2 x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.FixAnnot_Measure"),
        Core.projectionField = (Core.Name "term2")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

fixAnnot_MeasureWithTerm :: Phantoms.TTerm Syntax.FixAnnot_Measure -> Phantoms.TTerm Syntax.OneTerm -> Phantoms.TTerm Syntax.FixAnnot_Measure
fixAnnot_MeasureWithTerm original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.FixAnnot_Measure"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "ident"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.FixAnnot_Measure"),
              Core.projectionField = (Core.Name "ident")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "term2"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.FixAnnot_Measure"),
              Core.projectionField = (Core.Name "term2")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

fixAnnot_MeasureWithIdent :: Phantoms.TTerm Syntax.FixAnnot_Measure -> Phantoms.TTerm (Maybe Syntax.Ident) -> Phantoms.TTerm Syntax.FixAnnot_Measure
fixAnnot_MeasureWithIdent original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.FixAnnot_Measure"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.FixAnnot_Measure"),
              Core.projectionField = (Core.Name "term")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ident"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "term2"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.FixAnnot_Measure"),
              Core.projectionField = (Core.Name "term2")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

fixAnnot_MeasureWithTerm2 :: Phantoms.TTerm Syntax.FixAnnot_Measure -> Phantoms.TTerm (Maybe Syntax.OneTerm) -> Phantoms.TTerm Syntax.FixAnnot_Measure
fixAnnot_MeasureWithTerm2 original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.FixAnnot_Measure"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.FixAnnot_Measure"),
              Core.projectionField = (Core.Name "term")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ident"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.FixAnnot_Measure"),
              Core.projectionField = (Core.Name "ident")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "term2"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

fixAnnot_Wf :: Phantoms.TTerm Syntax.OneTerm -> Phantoms.TTerm Syntax.Ident -> Phantoms.TTerm Syntax.FixAnnot_Wf
fixAnnot_Wf term ident =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.FixAnnot_Wf"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Phantoms.unTTerm term)},
        Core.Field {
          Core.fieldName = (Core.Name "ident"),
          Core.fieldTerm = (Phantoms.unTTerm ident)}]}))

fixAnnot_WfTerm :: Phantoms.TTerm Syntax.FixAnnot_Wf -> Phantoms.TTerm Syntax.OneTerm
fixAnnot_WfTerm x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.FixAnnot_Wf"),
        Core.projectionField = (Core.Name "term")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

fixAnnot_WfIdent :: Phantoms.TTerm Syntax.FixAnnot_Wf -> Phantoms.TTerm Syntax.Ident
fixAnnot_WfIdent x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.FixAnnot_Wf"),
        Core.projectionField = (Core.Name "ident")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

fixAnnot_WfWithTerm :: Phantoms.TTerm Syntax.FixAnnot_Wf -> Phantoms.TTerm Syntax.OneTerm -> Phantoms.TTerm Syntax.FixAnnot_Wf
fixAnnot_WfWithTerm original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.FixAnnot_Wf"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "ident"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.FixAnnot_Wf"),
              Core.projectionField = (Core.Name "ident")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

fixAnnot_WfWithIdent :: Phantoms.TTerm Syntax.FixAnnot_Wf -> Phantoms.TTerm Syntax.Ident -> Phantoms.TTerm Syntax.FixAnnot_Wf
fixAnnot_WfWithIdent original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.FixAnnot_Wf"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.FixAnnot_Wf"),
              Core.projectionField = (Core.Name "term")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ident"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

fix_Decl :: Phantoms.TTerm Syntax.Ident -> Phantoms.TTerm [Syntax.Binder] -> Phantoms.TTerm (Maybe Syntax.FixAnnot) -> Phantoms.TTerm (Maybe Syntax.Type) -> Phantoms.TTerm Syntax.Term -> Phantoms.TTerm Syntax.Fix_Decl
fix_Decl ident binders annot type_ term =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Fix_Decl"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "ident"),
          Core.fieldTerm = (Phantoms.unTTerm ident)},
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Phantoms.unTTerm binders)},
        Core.Field {
          Core.fieldName = (Core.Name "annot"),
          Core.fieldTerm = (Phantoms.unTTerm annot)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm type_)},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Phantoms.unTTerm term)}]}))

fix_DeclIdent :: Phantoms.TTerm Syntax.Fix_Decl -> Phantoms.TTerm Syntax.Ident
fix_DeclIdent x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Fix_Decl"),
        Core.projectionField = (Core.Name "ident")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

fix_DeclBinders :: Phantoms.TTerm Syntax.Fix_Decl -> Phantoms.TTerm [Syntax.Binder]
fix_DeclBinders x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Fix_Decl"),
        Core.projectionField = (Core.Name "binders")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

fix_DeclAnnot :: Phantoms.TTerm Syntax.Fix_Decl -> Phantoms.TTerm (Maybe Syntax.FixAnnot)
fix_DeclAnnot x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Fix_Decl"),
        Core.projectionField = (Core.Name "annot")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

fix_DeclType :: Phantoms.TTerm Syntax.Fix_Decl -> Phantoms.TTerm (Maybe Syntax.Type)
fix_DeclType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Fix_Decl"),
        Core.projectionField = (Core.Name "type")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

fix_DeclTerm :: Phantoms.TTerm Syntax.Fix_Decl -> Phantoms.TTerm Syntax.Term
fix_DeclTerm x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Fix_Decl"),
        Core.projectionField = (Core.Name "term")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

fix_DeclWithIdent :: Phantoms.TTerm Syntax.Fix_Decl -> Phantoms.TTerm Syntax.Ident -> Phantoms.TTerm Syntax.Fix_Decl
fix_DeclWithIdent original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Fix_Decl"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "ident"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Fix_Decl"),
              Core.projectionField = (Core.Name "binders")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "annot"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Fix_Decl"),
              Core.projectionField = (Core.Name "annot")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Fix_Decl"),
              Core.projectionField = (Core.Name "type")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Fix_Decl"),
              Core.projectionField = (Core.Name "term")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

fix_DeclWithBinders :: Phantoms.TTerm Syntax.Fix_Decl -> Phantoms.TTerm [Syntax.Binder] -> Phantoms.TTerm Syntax.Fix_Decl
fix_DeclWithBinders original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Fix_Decl"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "ident"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Fix_Decl"),
              Core.projectionField = (Core.Name "ident")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "annot"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Fix_Decl"),
              Core.projectionField = (Core.Name "annot")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Fix_Decl"),
              Core.projectionField = (Core.Name "type")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Fix_Decl"),
              Core.projectionField = (Core.Name "term")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

fix_DeclWithAnnot :: Phantoms.TTerm Syntax.Fix_Decl -> Phantoms.TTerm (Maybe Syntax.FixAnnot) -> Phantoms.TTerm Syntax.Fix_Decl
fix_DeclWithAnnot original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Fix_Decl"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "ident"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Fix_Decl"),
              Core.projectionField = (Core.Name "ident")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Fix_Decl"),
              Core.projectionField = (Core.Name "binders")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "annot"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Fix_Decl"),
              Core.projectionField = (Core.Name "type")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Fix_Decl"),
              Core.projectionField = (Core.Name "term")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

fix_DeclWithType :: Phantoms.TTerm Syntax.Fix_Decl -> Phantoms.TTerm (Maybe Syntax.Type) -> Phantoms.TTerm Syntax.Fix_Decl
fix_DeclWithType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Fix_Decl"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "ident"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Fix_Decl"),
              Core.projectionField = (Core.Name "ident")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Fix_Decl"),
              Core.projectionField = (Core.Name "binders")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "annot"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Fix_Decl"),
              Core.projectionField = (Core.Name "annot")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Fix_Decl"),
              Core.projectionField = (Core.Name "term")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

fix_DeclWithTerm :: Phantoms.TTerm Syntax.Fix_Decl -> Phantoms.TTerm Syntax.Term -> Phantoms.TTerm Syntax.Fix_Decl
fix_DeclWithTerm original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Fix_Decl"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "ident"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Fix_Decl"),
              Core.projectionField = (Core.Name "ident")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Fix_Decl"),
              Core.projectionField = (Core.Name "binders")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "annot"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Fix_Decl"),
              Core.projectionField = (Core.Name "annot")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Fix_Decl"),
              Core.projectionField = (Core.Name "type")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

fix_QualIn :: Phantoms.TTerm Syntax.Term -> Phantoms.TTerm Syntax.Fix_Qual
fix_QualIn x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Fix_Qual"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "in"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

fix_QualWith :: Phantoms.TTerm Syntax.FixWith -> Phantoms.TTerm Syntax.Fix_Qual
fix_QualWith x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Fix_Qual"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "with"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

fixWith :: Phantoms.TTerm [Syntax.Fix_Decl] -> Phantoms.TTerm (Maybe Syntax.Ident) -> Phantoms.TTerm Syntax.FixWith
fixWith decls for =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.FixWith"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "decls"),
          Core.fieldTerm = (Phantoms.unTTerm decls)},
        Core.Field {
          Core.fieldName = (Core.Name "for"),
          Core.fieldTerm = (Phantoms.unTTerm for)}]}))

fixWithDecls :: Phantoms.TTerm Syntax.FixWith -> Phantoms.TTerm [Syntax.Fix_Decl]
fixWithDecls x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.FixWith"),
        Core.projectionField = (Core.Name "decls")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

fixWithFor :: Phantoms.TTerm Syntax.FixWith -> Phantoms.TTerm (Maybe Syntax.Ident)
fixWithFor x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.FixWith"),
        Core.projectionField = (Core.Name "for")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

fixWithWithDecls :: Phantoms.TTerm Syntax.FixWith -> Phantoms.TTerm [Syntax.Fix_Decl] -> Phantoms.TTerm Syntax.FixWith
fixWithWithDecls original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.FixWith"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "decls"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "for"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.FixWith"),
              Core.projectionField = (Core.Name "for")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

fixWithWithFor :: Phantoms.TTerm Syntax.FixWith -> Phantoms.TTerm (Maybe Syntax.Ident) -> Phantoms.TTerm Syntax.FixWith
fixWithWithFor original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.FixWith"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "decls"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.FixWith"),
              Core.projectionField = (Core.Name "decls")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "for"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

forall_ :: Phantoms.TTerm Syntax.OpenBinders -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.Forall
forall_ binders type_ =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Forall"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Phantoms.unTTerm binders)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm type_)}]}))

forallBinders :: Phantoms.TTerm Syntax.Forall -> Phantoms.TTerm Syntax.OpenBinders
forallBinders x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Forall"),
        Core.projectionField = (Core.Name "binders")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

forallType :: Phantoms.TTerm Syntax.Forall -> Phantoms.TTerm Syntax.Type
forallType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Forall"),
        Core.projectionField = (Core.Name "type")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

forallWithBinders :: Phantoms.TTerm Syntax.Forall -> Phantoms.TTerm Syntax.OpenBinders -> Phantoms.TTerm Syntax.Forall
forallWithBinders original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Forall"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Forall"),
              Core.projectionField = (Core.Name "type")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

forallWithType :: Phantoms.TTerm Syntax.Forall -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.Forall
forallWithType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Forall"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Forall"),
              Core.projectionField = (Core.Name "binders")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

forallOrFunForall :: Phantoms.TTerm Syntax.Forall -> Phantoms.TTerm Syntax.ForallOrFun
forallOrFunForall x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.ForallOrFun"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "forall"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

forallOrFunFun :: Phantoms.TTerm Syntax.Fun -> Phantoms.TTerm Syntax.ForallOrFun
forallOrFunFun x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.ForallOrFun"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "fun"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

fun :: Phantoms.TTerm Syntax.OpenBinders -> Phantoms.TTerm Syntax.Term -> Phantoms.TTerm Syntax.Fun
fun binders body =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Fun"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Phantoms.unTTerm binders)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm body)}]}))

funBinders :: Phantoms.TTerm Syntax.Fun -> Phantoms.TTerm Syntax.OpenBinders
funBinders x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Fun"),
        Core.projectionField = (Core.Name "binders")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

funBody :: Phantoms.TTerm Syntax.Fun -> Phantoms.TTerm Syntax.Term
funBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Fun"),
        Core.projectionField = (Core.Name "body")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

funWithBinders :: Phantoms.TTerm Syntax.Fun -> Phantoms.TTerm Syntax.OpenBinders -> Phantoms.TTerm Syntax.Fun
funWithBinders original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Fun"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Fun"),
              Core.projectionField = (Core.Name "body")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

funWithBody :: Phantoms.TTerm Syntax.Fun -> Phantoms.TTerm Syntax.Term -> Phantoms.TTerm Syntax.Fun
funWithBody original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Fun"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Fun"),
              Core.projectionField = (Core.Name "binders")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

generalizingBinderExplicit :: Phantoms.TTerm Syntax.TypeclassConstraint -> Phantoms.TTerm Syntax.GeneralizingBinder
generalizingBinderExplicit x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.GeneralizingBinder"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "explicit"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

generalizingBinderImplicitMaximallyInserted :: Phantoms.TTerm Syntax.TypeclassConstraint -> Phantoms.TTerm Syntax.GeneralizingBinder
generalizingBinderImplicitMaximallyInserted x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.GeneralizingBinder"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "implicitMaximallyInserted"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

generalizingBinderImplicitNonMaximallyInserted :: Phantoms.TTerm Syntax.TypeclassConstraint -> Phantoms.TTerm Syntax.GeneralizingBinder
generalizingBinderImplicitNonMaximallyInserted x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.GeneralizingBinder"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "implicitNonMaximallyInserted"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

ident :: Phantoms.TTerm Syntax.String_ -> Phantoms.TTerm Syntax.Ident
ident x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Ident"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unIdent :: Phantoms.TTerm Syntax.Ident -> Phantoms.TTerm Syntax.String_
unIdent x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.fr.inria.coq.syntax.Ident")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

identArg :: Phantoms.TTerm Syntax.Ident -> Phantoms.TTerm Syntax.Term -> Phantoms.TTerm Syntax.IdentArg
identArg ident term =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.IdentArg"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "ident"),
          Core.fieldTerm = (Phantoms.unTTerm ident)},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Phantoms.unTTerm term)}]}))

identArgIdent :: Phantoms.TTerm Syntax.IdentArg -> Phantoms.TTerm Syntax.Ident
identArgIdent x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.IdentArg"),
        Core.projectionField = (Core.Name "ident")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

identArgTerm :: Phantoms.TTerm Syntax.IdentArg -> Phantoms.TTerm Syntax.Term
identArgTerm x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.IdentArg"),
        Core.projectionField = (Core.Name "term")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

identArgWithIdent :: Phantoms.TTerm Syntax.IdentArg -> Phantoms.TTerm Syntax.Ident -> Phantoms.TTerm Syntax.IdentArg
identArgWithIdent original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.IdentArg"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "ident"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.IdentArg"),
              Core.projectionField = (Core.Name "term")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

identArgWithTerm :: Phantoms.TTerm Syntax.IdentArg -> Phantoms.TTerm Syntax.Term -> Phantoms.TTerm Syntax.IdentArg
identArgWithTerm original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.IdentArg"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "ident"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.IdentArg"),
              Core.projectionField = (Core.Name "ident")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

if_ :: Phantoms.TTerm Syntax.Term -> Phantoms.TTerm (Maybe Syntax.ReturnAs) -> Phantoms.TTerm Syntax.Term -> Phantoms.TTerm Syntax.Term -> Phantoms.TTerm Syntax.If
if_ condition returnAs then_ else_ =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.If"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "condition"),
          Core.fieldTerm = (Phantoms.unTTerm condition)},
        Core.Field {
          Core.fieldName = (Core.Name "returnAs"),
          Core.fieldTerm = (Phantoms.unTTerm returnAs)},
        Core.Field {
          Core.fieldName = (Core.Name "then"),
          Core.fieldTerm = (Phantoms.unTTerm then_)},
        Core.Field {
          Core.fieldName = (Core.Name "else"),
          Core.fieldTerm = (Phantoms.unTTerm else_)}]}))

ifCondition :: Phantoms.TTerm Syntax.If -> Phantoms.TTerm Syntax.Term
ifCondition x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.If"),
        Core.projectionField = (Core.Name "condition")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

ifReturnAs :: Phantoms.TTerm Syntax.If -> Phantoms.TTerm (Maybe Syntax.ReturnAs)
ifReturnAs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.If"),
        Core.projectionField = (Core.Name "returnAs")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

ifThen :: Phantoms.TTerm Syntax.If -> Phantoms.TTerm Syntax.Term
ifThen x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.If"),
        Core.projectionField = (Core.Name "then")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

ifElse :: Phantoms.TTerm Syntax.If -> Phantoms.TTerm Syntax.Term
ifElse x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.If"),
        Core.projectionField = (Core.Name "else")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

ifWithCondition :: Phantoms.TTerm Syntax.If -> Phantoms.TTerm Syntax.Term -> Phantoms.TTerm Syntax.If
ifWithCondition original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.If"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "condition"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "returnAs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.If"),
              Core.projectionField = (Core.Name "returnAs")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "then"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.If"),
              Core.projectionField = (Core.Name "then")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "else"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.If"),
              Core.projectionField = (Core.Name "else")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

ifWithReturnAs :: Phantoms.TTerm Syntax.If -> Phantoms.TTerm (Maybe Syntax.ReturnAs) -> Phantoms.TTerm Syntax.If
ifWithReturnAs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.If"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "condition"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.If"),
              Core.projectionField = (Core.Name "condition")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "returnAs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "then"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.If"),
              Core.projectionField = (Core.Name "then")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "else"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.If"),
              Core.projectionField = (Core.Name "else")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

ifWithThen :: Phantoms.TTerm Syntax.If -> Phantoms.TTerm Syntax.Term -> Phantoms.TTerm Syntax.If
ifWithThen original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.If"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "condition"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.If"),
              Core.projectionField = (Core.Name "condition")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "returnAs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.If"),
              Core.projectionField = (Core.Name "returnAs")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "then"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "else"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.If"),
              Core.projectionField = (Core.Name "else")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

ifWithElse :: Phantoms.TTerm Syntax.If -> Phantoms.TTerm Syntax.Term -> Phantoms.TTerm Syntax.If
ifWithElse original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.If"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "condition"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.If"),
              Core.projectionField = (Core.Name "condition")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "returnAs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.If"),
              Core.projectionField = (Core.Name "returnAs")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "then"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.If"),
              Core.projectionField = (Core.Name "then")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "else"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

implicitBindersMaximallyInserted :: Phantoms.TTerm Syntax.TypeBinders -> Phantoms.TTerm Syntax.ImplicitBinders
implicitBindersMaximallyInserted x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.ImplicitBinders"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "maximallyInserted"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

implicitBindersNonMaximallyInserted :: Phantoms.TTerm Syntax.TypeBinders -> Phantoms.TTerm Syntax.ImplicitBinders
implicitBindersNonMaximallyInserted x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.ImplicitBinders"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "nonMaximallyInserted"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

let_ :: Phantoms.TTerm Syntax.LetBindings -> Phantoms.TTerm Syntax.Term -> Phantoms.TTerm Syntax.Let
let_ bindings in_ =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Let"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "bindings"),
          Core.fieldTerm = (Phantoms.unTTerm bindings)},
        Core.Field {
          Core.fieldName = (Core.Name "in"),
          Core.fieldTerm = (Phantoms.unTTerm in_)}]}))

letBindings :: Phantoms.TTerm Syntax.Let -> Phantoms.TTerm Syntax.LetBindings
letBindings x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Let"),
        Core.projectionField = (Core.Name "bindings")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

letIn :: Phantoms.TTerm Syntax.Let -> Phantoms.TTerm Syntax.Term
letIn x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Let"),
        Core.projectionField = (Core.Name "in")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

letWithBindings :: Phantoms.TTerm Syntax.Let -> Phantoms.TTerm Syntax.LetBindings -> Phantoms.TTerm Syntax.Let
letWithBindings original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Let"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "bindings"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "in"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Let"),
              Core.projectionField = (Core.Name "in")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

letWithIn :: Phantoms.TTerm Syntax.Let -> Phantoms.TTerm Syntax.Term -> Phantoms.TTerm Syntax.Let
letWithIn original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Let"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "bindings"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Let"),
              Core.projectionField = (Core.Name "bindings")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "in"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

letBinder :: Phantoms.TTerm Syntax.Name -> Phantoms.TTerm (Maybe Syntax.Type) -> Phantoms.TTerm Syntax.Term -> Phantoms.TTerm Syntax.LetBinder
letBinder name type_ term =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.LetBinder"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm type_)},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Phantoms.unTTerm term)}]}))

letBinderName :: Phantoms.TTerm Syntax.LetBinder -> Phantoms.TTerm Syntax.Name
letBinderName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.LetBinder"),
        Core.projectionField = (Core.Name "name")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

letBinderType :: Phantoms.TTerm Syntax.LetBinder -> Phantoms.TTerm (Maybe Syntax.Type)
letBinderType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.LetBinder"),
        Core.projectionField = (Core.Name "type")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

letBinderTerm :: Phantoms.TTerm Syntax.LetBinder -> Phantoms.TTerm Syntax.Term
letBinderTerm x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.LetBinder"),
        Core.projectionField = (Core.Name "term")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

letBinderWithName :: Phantoms.TTerm Syntax.LetBinder -> Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.LetBinder
letBinderWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.LetBinder"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.LetBinder"),
              Core.projectionField = (Core.Name "type")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.LetBinder"),
              Core.projectionField = (Core.Name "term")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

letBinderWithType :: Phantoms.TTerm Syntax.LetBinder -> Phantoms.TTerm (Maybe Syntax.Type) -> Phantoms.TTerm Syntax.LetBinder
letBinderWithType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.LetBinder"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.LetBinder"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.LetBinder"),
              Core.projectionField = (Core.Name "term")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

letBinderWithTerm :: Phantoms.TTerm Syntax.LetBinder -> Phantoms.TTerm Syntax.Term -> Phantoms.TTerm Syntax.LetBinder
letBinderWithTerm original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.LetBinder"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.LetBinder"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.LetBinder"),
              Core.projectionField = (Core.Name "type")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

letBindingsNamed :: Phantoms.TTerm Syntax.LetNamed -> Phantoms.TTerm Syntax.LetBindings
letBindingsNamed x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.LetBindings"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "named"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

letBindingsDestructuring :: Phantoms.TTerm Syntax.LetDestructuring -> Phantoms.TTerm Syntax.LetBindings
letBindingsDestructuring x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.LetBindings"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "destructuring"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

letNamed :: Phantoms.TTerm Syntax.LetBinder -> Phantoms.TTerm [Syntax.Binder] -> Phantoms.TTerm Syntax.LetNamed
letNamed binder binders =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.LetNamed"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "binder"),
          Core.fieldTerm = (Phantoms.unTTerm binder)},
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Phantoms.unTTerm binders)}]}))

letNamedBinder :: Phantoms.TTerm Syntax.LetNamed -> Phantoms.TTerm Syntax.LetBinder
letNamedBinder x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.LetNamed"),
        Core.projectionField = (Core.Name "binder")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

letNamedBinders :: Phantoms.TTerm Syntax.LetNamed -> Phantoms.TTerm [Syntax.Binder]
letNamedBinders x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.LetNamed"),
        Core.projectionField = (Core.Name "binders")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

letNamedWithBinder :: Phantoms.TTerm Syntax.LetNamed -> Phantoms.TTerm Syntax.LetBinder -> Phantoms.TTerm Syntax.LetNamed
letNamedWithBinder original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.LetNamed"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "binder"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.LetNamed"),
              Core.projectionField = (Core.Name "binders")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

letNamedWithBinders :: Phantoms.TTerm Syntax.LetNamed -> Phantoms.TTerm [Syntax.Binder] -> Phantoms.TTerm Syntax.LetNamed
letNamedWithBinders original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.LetNamed"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "binder"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.LetNamed"),
              Core.projectionField = (Core.Name "binder")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

letDestructuringVariant1 :: Phantoms.TTerm Syntax.LetDestructuring_Variant1 -> Phantoms.TTerm Syntax.LetDestructuring
letDestructuringVariant1 x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.LetDestructuring"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "variant1"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

letDestructuringVariant2 :: Phantoms.TTerm Syntax.LetDestructuring_Variant2 -> Phantoms.TTerm Syntax.LetDestructuring
letDestructuringVariant2 x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.LetDestructuring"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "variant2"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

letDestructuringVariant3 :: Phantoms.TTerm Syntax.LetDestructuring_Variant3 -> Phantoms.TTerm Syntax.LetDestructuring
letDestructuringVariant3 x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.LetDestructuring"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "variant3"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

letDestructuring_Variant1 :: Phantoms.TTerm [Syntax.Name] -> Phantoms.TTerm (Maybe Syntax.ReturnAs) -> Phantoms.TTerm Syntax.Term -> Phantoms.TTerm Syntax.LetDestructuring_Variant1
letDestructuring_Variant1 names returnAs term =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.LetDestructuring_Variant1"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "names"),
          Core.fieldTerm = (Phantoms.unTTerm names)},
        Core.Field {
          Core.fieldName = (Core.Name "returnAs"),
          Core.fieldTerm = (Phantoms.unTTerm returnAs)},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Phantoms.unTTerm term)}]}))

letDestructuring_Variant1Names :: Phantoms.TTerm Syntax.LetDestructuring_Variant1 -> Phantoms.TTerm [Syntax.Name]
letDestructuring_Variant1Names x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.LetDestructuring_Variant1"),
        Core.projectionField = (Core.Name "names")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

letDestructuring_Variant1ReturnAs :: Phantoms.TTerm Syntax.LetDestructuring_Variant1 -> Phantoms.TTerm (Maybe Syntax.ReturnAs)
letDestructuring_Variant1ReturnAs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.LetDestructuring_Variant1"),
        Core.projectionField = (Core.Name "returnAs")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

letDestructuring_Variant1Term :: Phantoms.TTerm Syntax.LetDestructuring_Variant1 -> Phantoms.TTerm Syntax.Term
letDestructuring_Variant1Term x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.LetDestructuring_Variant1"),
        Core.projectionField = (Core.Name "term")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

letDestructuring_Variant1WithNames :: Phantoms.TTerm Syntax.LetDestructuring_Variant1 -> Phantoms.TTerm [Syntax.Name] -> Phantoms.TTerm Syntax.LetDestructuring_Variant1
letDestructuring_Variant1WithNames original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.LetDestructuring_Variant1"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "names"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "returnAs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.LetDestructuring_Variant1"),
              Core.projectionField = (Core.Name "returnAs")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.LetDestructuring_Variant1"),
              Core.projectionField = (Core.Name "term")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

letDestructuring_Variant1WithReturnAs :: Phantoms.TTerm Syntax.LetDestructuring_Variant1 -> Phantoms.TTerm (Maybe Syntax.ReturnAs) -> Phantoms.TTerm Syntax.LetDestructuring_Variant1
letDestructuring_Variant1WithReturnAs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.LetDestructuring_Variant1"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "names"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.LetDestructuring_Variant1"),
              Core.projectionField = (Core.Name "names")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "returnAs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.LetDestructuring_Variant1"),
              Core.projectionField = (Core.Name "term")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

letDestructuring_Variant1WithTerm :: Phantoms.TTerm Syntax.LetDestructuring_Variant1 -> Phantoms.TTerm Syntax.Term -> Phantoms.TTerm Syntax.LetDestructuring_Variant1
letDestructuring_Variant1WithTerm original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.LetDestructuring_Variant1"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "names"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.LetDestructuring_Variant1"),
              Core.projectionField = (Core.Name "names")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "returnAs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.LetDestructuring_Variant1"),
              Core.projectionField = (Core.Name "returnAs")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

letDestructuring_Variant2 :: Phantoms.TTerm Syntax.Pattern -> Phantoms.TTerm Syntax.Term -> Phantoms.TTerm (Maybe Syntax.Term100) -> Phantoms.TTerm Syntax.LetDestructuring_Variant2
letDestructuring_Variant2 pattern term return =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.LetDestructuring_Variant2"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Phantoms.unTTerm pattern)},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Phantoms.unTTerm term)},
        Core.Field {
          Core.fieldName = (Core.Name "return"),
          Core.fieldTerm = (Phantoms.unTTerm return)}]}))

letDestructuring_Variant2Pattern :: Phantoms.TTerm Syntax.LetDestructuring_Variant2 -> Phantoms.TTerm Syntax.Pattern
letDestructuring_Variant2Pattern x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.LetDestructuring_Variant2"),
        Core.projectionField = (Core.Name "pattern")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

letDestructuring_Variant2Term :: Phantoms.TTerm Syntax.LetDestructuring_Variant2 -> Phantoms.TTerm Syntax.Term
letDestructuring_Variant2Term x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.LetDestructuring_Variant2"),
        Core.projectionField = (Core.Name "term")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

letDestructuring_Variant2Return :: Phantoms.TTerm Syntax.LetDestructuring_Variant2 -> Phantoms.TTerm (Maybe Syntax.Term100)
letDestructuring_Variant2Return x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.LetDestructuring_Variant2"),
        Core.projectionField = (Core.Name "return")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

letDestructuring_Variant2WithPattern :: Phantoms.TTerm Syntax.LetDestructuring_Variant2 -> Phantoms.TTerm Syntax.Pattern -> Phantoms.TTerm Syntax.LetDestructuring_Variant2
letDestructuring_Variant2WithPattern original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.LetDestructuring_Variant2"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.LetDestructuring_Variant2"),
              Core.projectionField = (Core.Name "term")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "return"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.LetDestructuring_Variant2"),
              Core.projectionField = (Core.Name "return")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

letDestructuring_Variant2WithTerm :: Phantoms.TTerm Syntax.LetDestructuring_Variant2 -> Phantoms.TTerm Syntax.Term -> Phantoms.TTerm Syntax.LetDestructuring_Variant2
letDestructuring_Variant2WithTerm original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.LetDestructuring_Variant2"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.LetDestructuring_Variant2"),
              Core.projectionField = (Core.Name "pattern")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "return"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.LetDestructuring_Variant2"),
              Core.projectionField = (Core.Name "return")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

letDestructuring_Variant2WithReturn :: Phantoms.TTerm Syntax.LetDestructuring_Variant2 -> Phantoms.TTerm (Maybe Syntax.Term100) -> Phantoms.TTerm Syntax.LetDestructuring_Variant2
letDestructuring_Variant2WithReturn original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.LetDestructuring_Variant2"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.LetDestructuring_Variant2"),
              Core.projectionField = (Core.Name "pattern")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.LetDestructuring_Variant2"),
              Core.projectionField = (Core.Name "term")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "return"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

letDestructuring_Variant3 :: Phantoms.TTerm Syntax.Pattern -> Phantoms.TTerm Syntax.Pattern -> Phantoms.TTerm Syntax.Term -> Phantoms.TTerm Syntax.Term100 -> Phantoms.TTerm Syntax.LetDestructuring_Variant3
letDestructuring_Variant3 pattern1 pattern2 term return =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.LetDestructuring_Variant3"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pattern1"),
          Core.fieldTerm = (Phantoms.unTTerm pattern1)},
        Core.Field {
          Core.fieldName = (Core.Name "pattern2"),
          Core.fieldTerm = (Phantoms.unTTerm pattern2)},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Phantoms.unTTerm term)},
        Core.Field {
          Core.fieldName = (Core.Name "return"),
          Core.fieldTerm = (Phantoms.unTTerm return)}]}))

letDestructuring_Variant3Pattern1 :: Phantoms.TTerm Syntax.LetDestructuring_Variant3 -> Phantoms.TTerm Syntax.Pattern
letDestructuring_Variant3Pattern1 x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.LetDestructuring_Variant3"),
        Core.projectionField = (Core.Name "pattern1")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

letDestructuring_Variant3Pattern2 :: Phantoms.TTerm Syntax.LetDestructuring_Variant3 -> Phantoms.TTerm Syntax.Pattern
letDestructuring_Variant3Pattern2 x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.LetDestructuring_Variant3"),
        Core.projectionField = (Core.Name "pattern2")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

letDestructuring_Variant3Term :: Phantoms.TTerm Syntax.LetDestructuring_Variant3 -> Phantoms.TTerm Syntax.Term
letDestructuring_Variant3Term x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.LetDestructuring_Variant3"),
        Core.projectionField = (Core.Name "term")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

letDestructuring_Variant3Return :: Phantoms.TTerm Syntax.LetDestructuring_Variant3 -> Phantoms.TTerm Syntax.Term100
letDestructuring_Variant3Return x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.LetDestructuring_Variant3"),
        Core.projectionField = (Core.Name "return")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

letDestructuring_Variant3WithPattern1 :: Phantoms.TTerm Syntax.LetDestructuring_Variant3 -> Phantoms.TTerm Syntax.Pattern -> Phantoms.TTerm Syntax.LetDestructuring_Variant3
letDestructuring_Variant3WithPattern1 original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.LetDestructuring_Variant3"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pattern1"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "pattern2"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.LetDestructuring_Variant3"),
              Core.projectionField = (Core.Name "pattern2")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.LetDestructuring_Variant3"),
              Core.projectionField = (Core.Name "term")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "return"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.LetDestructuring_Variant3"),
              Core.projectionField = (Core.Name "return")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

letDestructuring_Variant3WithPattern2 :: Phantoms.TTerm Syntax.LetDestructuring_Variant3 -> Phantoms.TTerm Syntax.Pattern -> Phantoms.TTerm Syntax.LetDestructuring_Variant3
letDestructuring_Variant3WithPattern2 original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.LetDestructuring_Variant3"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pattern1"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.LetDestructuring_Variant3"),
              Core.projectionField = (Core.Name "pattern1")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "pattern2"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.LetDestructuring_Variant3"),
              Core.projectionField = (Core.Name "term")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "return"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.LetDestructuring_Variant3"),
              Core.projectionField = (Core.Name "return")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

letDestructuring_Variant3WithTerm :: Phantoms.TTerm Syntax.LetDestructuring_Variant3 -> Phantoms.TTerm Syntax.Term -> Phantoms.TTerm Syntax.LetDestructuring_Variant3
letDestructuring_Variant3WithTerm original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.LetDestructuring_Variant3"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pattern1"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.LetDestructuring_Variant3"),
              Core.projectionField = (Core.Name "pattern1")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "pattern2"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.LetDestructuring_Variant3"),
              Core.projectionField = (Core.Name "pattern2")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "return"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.LetDestructuring_Variant3"),
              Core.projectionField = (Core.Name "return")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

letDestructuring_Variant3WithReturn :: Phantoms.TTerm Syntax.LetDestructuring_Variant3 -> Phantoms.TTerm Syntax.Term100 -> Phantoms.TTerm Syntax.LetDestructuring_Variant3
letDestructuring_Variant3WithReturn original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.LetDestructuring_Variant3"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pattern1"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.LetDestructuring_Variant3"),
              Core.projectionField = (Core.Name "pattern1")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "pattern2"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.LetDestructuring_Variant3"),
              Core.projectionField = (Core.Name "pattern2")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.LetDestructuring_Variant3"),
              Core.projectionField = (Core.Name "term")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "return"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

match :: Phantoms.TTerm [Syntax.CaseItem] -> Phantoms.TTerm (Maybe Syntax.Term100) -> Phantoms.TTerm Bool -> Phantoms.TTerm [Syntax.Equation] -> Phantoms.TTerm Syntax.Match
match caseItems return pipe equations =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Match"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "caseItems"),
          Core.fieldTerm = (Phantoms.unTTerm caseItems)},
        Core.Field {
          Core.fieldName = (Core.Name "return"),
          Core.fieldTerm = (Phantoms.unTTerm return)},
        Core.Field {
          Core.fieldName = (Core.Name "pipe"),
          Core.fieldTerm = (Phantoms.unTTerm pipe)},
        Core.Field {
          Core.fieldName = (Core.Name "equations"),
          Core.fieldTerm = (Phantoms.unTTerm equations)}]}))

matchCaseItems :: Phantoms.TTerm Syntax.Match -> Phantoms.TTerm [Syntax.CaseItem]
matchCaseItems x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Match"),
        Core.projectionField = (Core.Name "caseItems")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

matchReturn :: Phantoms.TTerm Syntax.Match -> Phantoms.TTerm (Maybe Syntax.Term100)
matchReturn x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Match"),
        Core.projectionField = (Core.Name "return")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

matchPipe :: Phantoms.TTerm Syntax.Match -> Phantoms.TTerm Bool
matchPipe x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Match"),
        Core.projectionField = (Core.Name "pipe")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

matchEquations :: Phantoms.TTerm Syntax.Match -> Phantoms.TTerm [Syntax.Equation]
matchEquations x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Match"),
        Core.projectionField = (Core.Name "equations")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

matchWithCaseItems :: Phantoms.TTerm Syntax.Match -> Phantoms.TTerm [Syntax.CaseItem] -> Phantoms.TTerm Syntax.Match
matchWithCaseItems original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Match"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "caseItems"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "return"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Match"),
              Core.projectionField = (Core.Name "return")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "pipe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Match"),
              Core.projectionField = (Core.Name "pipe")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "equations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Match"),
              Core.projectionField = (Core.Name "equations")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

matchWithReturn :: Phantoms.TTerm Syntax.Match -> Phantoms.TTerm (Maybe Syntax.Term100) -> Phantoms.TTerm Syntax.Match
matchWithReturn original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Match"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "caseItems"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Match"),
              Core.projectionField = (Core.Name "caseItems")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "return"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "pipe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Match"),
              Core.projectionField = (Core.Name "pipe")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "equations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Match"),
              Core.projectionField = (Core.Name "equations")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

matchWithPipe :: Phantoms.TTerm Syntax.Match -> Phantoms.TTerm Bool -> Phantoms.TTerm Syntax.Match
matchWithPipe original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Match"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "caseItems"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Match"),
              Core.projectionField = (Core.Name "caseItems")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "return"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Match"),
              Core.projectionField = (Core.Name "return")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "pipe"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "equations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Match"),
              Core.projectionField = (Core.Name "equations")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

matchWithEquations :: Phantoms.TTerm Syntax.Match -> Phantoms.TTerm [Syntax.Equation] -> Phantoms.TTerm Syntax.Match
matchWithEquations original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Match"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "caseItems"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Match"),
              Core.projectionField = (Core.Name "caseItems")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "return"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Match"),
              Core.projectionField = (Core.Name "return")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "pipe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Match"),
              Core.projectionField = (Core.Name "pipe")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "equations"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

name :: Phantoms.TTerm (Maybe Syntax.Ident) -> Phantoms.TTerm Syntax.Name
name x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Name"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unName :: Phantoms.TTerm Syntax.Name -> Phantoms.TTerm (Maybe Syntax.Ident)
unName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.fr.inria.coq.syntax.Name")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

natural :: Phantoms.TTerm Integer -> Phantoms.TTerm Syntax.Natural
natural x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Natural"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unNatural :: Phantoms.TTerm Syntax.Natural -> Phantoms.TTerm Integer
unNatural x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.fr.inria.coq.syntax.Natural")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

naturalArg :: Phantoms.TTerm Syntax.Natural -> Phantoms.TTerm Syntax.Term -> Phantoms.TTerm Syntax.NaturalArg
naturalArg natural term =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.NaturalArg"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "natural"),
          Core.fieldTerm = (Phantoms.unTTerm natural)},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Phantoms.unTTerm term)}]}))

naturalArgNatural :: Phantoms.TTerm Syntax.NaturalArg -> Phantoms.TTerm Syntax.Natural
naturalArgNatural x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.NaturalArg"),
        Core.projectionField = (Core.Name "natural")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

naturalArgTerm :: Phantoms.TTerm Syntax.NaturalArg -> Phantoms.TTerm Syntax.Term
naturalArgTerm x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.NaturalArg"),
        Core.projectionField = (Core.Name "term")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

naturalArgWithNatural :: Phantoms.TTerm Syntax.NaturalArg -> Phantoms.TTerm Syntax.Natural -> Phantoms.TTerm Syntax.NaturalArg
naturalArgWithNatural original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.NaturalArg"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "natural"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.NaturalArg"),
              Core.projectionField = (Core.Name "term")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

naturalArgWithTerm :: Phantoms.TTerm Syntax.NaturalArg -> Phantoms.TTerm Syntax.Term -> Phantoms.TTerm Syntax.NaturalArg
naturalArgWithTerm original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.NaturalArg"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "natural"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.NaturalArg"),
              Core.projectionField = (Core.Name "natural")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

normalApplication :: Phantoms.TTerm Syntax.Term1 -> Phantoms.TTerm [Syntax.Arg] -> Phantoms.TTerm Syntax.NormalApplication
normalApplication lhs rhs =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.NormalApplication"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Phantoms.unTTerm lhs)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTTerm rhs)}]}))

normalApplicationLhs :: Phantoms.TTerm Syntax.NormalApplication -> Phantoms.TTerm Syntax.Term1
normalApplicationLhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.NormalApplication"),
        Core.projectionField = (Core.Name "lhs")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

normalApplicationRhs :: Phantoms.TTerm Syntax.NormalApplication -> Phantoms.TTerm [Syntax.Arg]
normalApplicationRhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.NormalApplication"),
        Core.projectionField = (Core.Name "rhs")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

normalApplicationWithLhs :: Phantoms.TTerm Syntax.NormalApplication -> Phantoms.TTerm Syntax.Term1 -> Phantoms.TTerm Syntax.NormalApplication
normalApplicationWithLhs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.NormalApplication"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.NormalApplication"),
              Core.projectionField = (Core.Name "rhs")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

normalApplicationWithRhs :: Phantoms.TTerm Syntax.NormalApplication -> Phantoms.TTerm [Syntax.Arg] -> Phantoms.TTerm Syntax.NormalApplication
normalApplicationWithRhs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.NormalApplication"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.NormalApplication"),
              Core.projectionField = (Core.Name "lhs")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

number :: Phantoms.TTerm Double -> Phantoms.TTerm Syntax.Number
number x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Number"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unNumber :: Phantoms.TTerm Syntax.Number -> Phantoms.TTerm Double
unNumber x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.fr.inria.coq.syntax.Number")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

oneTermExplicit :: Phantoms.TTerm Syntax.QualidAnnotated -> Phantoms.TTerm Syntax.OneTerm
oneTermExplicit x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.OneTerm"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "explicit"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

oneTermTerm1 :: Phantoms.TTerm Syntax.Term1 -> Phantoms.TTerm Syntax.OneTerm
oneTermTerm1 x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.OneTerm"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "term1"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

openBindersType :: Phantoms.TTerm Syntax.TypeBinders -> Phantoms.TTerm Syntax.OpenBinders
openBindersType x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.OpenBinders"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "type"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

openBindersBinders :: Phantoms.TTerm [Syntax.Binder] -> Phantoms.TTerm Syntax.OpenBinders
openBindersBinders x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.OpenBinders"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "binders"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

patternPattern :: Phantoms.TTerm Syntax.Pattern10 -> Phantoms.TTerm Syntax.Pattern
patternPattern x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Pattern"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "pattern"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

patternTerm :: Phantoms.TTerm (Maybe Syntax.Term) -> Phantoms.TTerm Syntax.Pattern
patternTerm x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Pattern"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "term"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

pattern0Qualid :: Phantoms.TTerm Syntax.Qualid -> Phantoms.TTerm Syntax.Pattern0
pattern0Qualid x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Pattern0"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "qualid"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

pattern0QualIdAndPattern :: Phantoms.TTerm Syntax.QualidAndPattern -> Phantoms.TTerm Syntax.Pattern0
pattern0QualIdAndPattern x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Pattern0"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "qualIdAndPattern"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

pattern0Placeholder :: Phantoms.TTerm Syntax.Pattern0
pattern0Placeholder =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Pattern0"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "placeholder"),
        Core.fieldTerm = Core.TermUnit}}))

pattern0Parens :: Phantoms.TTerm [Syntax.Pattern] -> Phantoms.TTerm Syntax.Pattern0
pattern0Parens x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Pattern0"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "parens"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

pattern0Number :: Phantoms.TTerm Syntax.Number -> Phantoms.TTerm Syntax.Pattern0
pattern0Number x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Pattern0"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "number"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

pattern0String :: Phantoms.TTerm Syntax.String_ -> Phantoms.TTerm Syntax.Pattern0
pattern0String x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Pattern0"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "string"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

pattern1 :: Phantoms.TTerm Syntax.Pattern0 -> Phantoms.TTerm (Maybe Syntax.ScopeKey) -> Phantoms.TTerm Syntax.Pattern1
pattern1 pattern scope =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Pattern1"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Phantoms.unTTerm pattern)},
        Core.Field {
          Core.fieldName = (Core.Name "scope"),
          Core.fieldTerm = (Phantoms.unTTerm scope)}]}))

pattern1Pattern :: Phantoms.TTerm Syntax.Pattern1 -> Phantoms.TTerm Syntax.Pattern0
pattern1Pattern x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Pattern1"),
        Core.projectionField = (Core.Name "pattern")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

pattern1Scope :: Phantoms.TTerm Syntax.Pattern1 -> Phantoms.TTerm (Maybe Syntax.ScopeKey)
pattern1Scope x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Pattern1"),
        Core.projectionField = (Core.Name "scope")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

pattern1WithPattern :: Phantoms.TTerm Syntax.Pattern1 -> Phantoms.TTerm Syntax.Pattern0 -> Phantoms.TTerm Syntax.Pattern1
pattern1WithPattern original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Pattern1"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "scope"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Pattern1"),
              Core.projectionField = (Core.Name "scope")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

pattern1WithScope :: Phantoms.TTerm Syntax.Pattern1 -> Phantoms.TTerm (Maybe Syntax.ScopeKey) -> Phantoms.TTerm Syntax.Pattern1
pattern1WithScope original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Pattern1"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Pattern1"),
              Core.projectionField = (Core.Name "pattern")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "scope"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

pattern10As :: Phantoms.TTerm Syntax.Pattern10_As -> Phantoms.TTerm Syntax.Pattern10
pattern10As x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Pattern10"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "as"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

pattern10Patterns :: Phantoms.TTerm Syntax.Pattern10_Patterns -> Phantoms.TTerm Syntax.Pattern10
pattern10Patterns x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Pattern10"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "patterns"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

pattern10Qualiid :: Phantoms.TTerm Syntax.Pattern10_Qualid -> Phantoms.TTerm Syntax.Pattern10
pattern10Qualiid x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Pattern10"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "qualiid"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

pattern10_As :: Phantoms.TTerm Syntax.Pattern1 -> Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.Pattern10_As
pattern10_As pattern as =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Pattern10_As"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Phantoms.unTTerm pattern)},
        Core.Field {
          Core.fieldName = (Core.Name "as"),
          Core.fieldTerm = (Phantoms.unTTerm as)}]}))

pattern10_AsPattern :: Phantoms.TTerm Syntax.Pattern10_As -> Phantoms.TTerm Syntax.Pattern1
pattern10_AsPattern x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Pattern10_As"),
        Core.projectionField = (Core.Name "pattern")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

pattern10_AsAs :: Phantoms.TTerm Syntax.Pattern10_As -> Phantoms.TTerm Syntax.Name
pattern10_AsAs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Pattern10_As"),
        Core.projectionField = (Core.Name "as")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

pattern10_AsWithPattern :: Phantoms.TTerm Syntax.Pattern10_As -> Phantoms.TTerm Syntax.Pattern1 -> Phantoms.TTerm Syntax.Pattern10_As
pattern10_AsWithPattern original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Pattern10_As"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "as"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Pattern10_As"),
              Core.projectionField = (Core.Name "as")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

pattern10_AsWithAs :: Phantoms.TTerm Syntax.Pattern10_As -> Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.Pattern10_As
pattern10_AsWithAs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Pattern10_As"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Pattern10_As"),
              Core.projectionField = (Core.Name "pattern")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "as"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

pattern10_Patterns :: Phantoms.TTerm Syntax.Pattern1 -> Phantoms.TTerm [Syntax.Pattern1] -> Phantoms.TTerm Syntax.Pattern10_Patterns
pattern10_Patterns pattern patterns =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Pattern10_Patterns"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Phantoms.unTTerm pattern)},
        Core.Field {
          Core.fieldName = (Core.Name "patterns"),
          Core.fieldTerm = (Phantoms.unTTerm patterns)}]}))

pattern10_PatternsPattern :: Phantoms.TTerm Syntax.Pattern10_Patterns -> Phantoms.TTerm Syntax.Pattern1
pattern10_PatternsPattern x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Pattern10_Patterns"),
        Core.projectionField = (Core.Name "pattern")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

pattern10_PatternsPatterns :: Phantoms.TTerm Syntax.Pattern10_Patterns -> Phantoms.TTerm [Syntax.Pattern1]
pattern10_PatternsPatterns x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Pattern10_Patterns"),
        Core.projectionField = (Core.Name "patterns")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

pattern10_PatternsWithPattern :: Phantoms.TTerm Syntax.Pattern10_Patterns -> Phantoms.TTerm Syntax.Pattern1 -> Phantoms.TTerm Syntax.Pattern10_Patterns
pattern10_PatternsWithPattern original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Pattern10_Patterns"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "patterns"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Pattern10_Patterns"),
              Core.projectionField = (Core.Name "patterns")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

pattern10_PatternsWithPatterns :: Phantoms.TTerm Syntax.Pattern10_Patterns -> Phantoms.TTerm [Syntax.Pattern1] -> Phantoms.TTerm Syntax.Pattern10_Patterns
pattern10_PatternsWithPatterns original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Pattern10_Patterns"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Pattern10_Patterns"),
              Core.projectionField = (Core.Name "pattern")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "patterns"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

pattern10_Qualid :: Phantoms.TTerm Syntax.Qualid -> Phantoms.TTerm [Syntax.Pattern1] -> Phantoms.TTerm Syntax.Pattern10_Qualid
pattern10_Qualid qualid patterns =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Pattern10_Qualid"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "qualid"),
          Core.fieldTerm = (Phantoms.unTTerm qualid)},
        Core.Field {
          Core.fieldName = (Core.Name "patterns"),
          Core.fieldTerm = (Phantoms.unTTerm patterns)}]}))

pattern10_QualidQualid :: Phantoms.TTerm Syntax.Pattern10_Qualid -> Phantoms.TTerm Syntax.Qualid
pattern10_QualidQualid x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Pattern10_Qualid"),
        Core.projectionField = (Core.Name "qualid")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

pattern10_QualidPatterns :: Phantoms.TTerm Syntax.Pattern10_Qualid -> Phantoms.TTerm [Syntax.Pattern1]
pattern10_QualidPatterns x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Pattern10_Qualid"),
        Core.projectionField = (Core.Name "patterns")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

pattern10_QualidWithQualid :: Phantoms.TTerm Syntax.Pattern10_Qualid -> Phantoms.TTerm Syntax.Qualid -> Phantoms.TTerm Syntax.Pattern10_Qualid
pattern10_QualidWithQualid original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Pattern10_Qualid"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "qualid"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "patterns"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Pattern10_Qualid"),
              Core.projectionField = (Core.Name "patterns")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

pattern10_QualidWithPatterns :: Phantoms.TTerm Syntax.Pattern10_Qualid -> Phantoms.TTerm [Syntax.Pattern1] -> Phantoms.TTerm Syntax.Pattern10_Qualid
pattern10_QualidWithPatterns original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Pattern10_Qualid"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "qualid"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Pattern10_Qualid"),
              Core.projectionField = (Core.Name "qualid")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "patterns"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

primitiveNotationsNumber :: Phantoms.TTerm Syntax.Number -> Phantoms.TTerm Syntax.PrimitiveNotations
primitiveNotationsNumber x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.PrimitiveNotations"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "number"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

primitiveNotationsString :: Phantoms.TTerm Syntax.String_ -> Phantoms.TTerm Syntax.PrimitiveNotations
primitiveNotationsString x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.PrimitiveNotations"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "string"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

qualid :: Phantoms.TTerm Syntax.Ident -> Phantoms.TTerm [Syntax.FieldIdent] -> Phantoms.TTerm Syntax.Qualid
qualid id fieldIds =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Qualid"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Phantoms.unTTerm id)},
        Core.Field {
          Core.fieldName = (Core.Name "fieldIds"),
          Core.fieldTerm = (Phantoms.unTTerm fieldIds)}]}))

qualidId :: Phantoms.TTerm Syntax.Qualid -> Phantoms.TTerm Syntax.Ident
qualidId x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Qualid"),
        Core.projectionField = (Core.Name "id")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

qualidFieldIds :: Phantoms.TTerm Syntax.Qualid -> Phantoms.TTerm [Syntax.FieldIdent]
qualidFieldIds x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Qualid"),
        Core.projectionField = (Core.Name "fieldIds")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

qualidWithId :: Phantoms.TTerm Syntax.Qualid -> Phantoms.TTerm Syntax.Ident -> Phantoms.TTerm Syntax.Qualid
qualidWithId original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Qualid"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "fieldIds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Qualid"),
              Core.projectionField = (Core.Name "fieldIds")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

qualidWithFieldIds :: Phantoms.TTerm Syntax.Qualid -> Phantoms.TTerm [Syntax.FieldIdent] -> Phantoms.TTerm Syntax.Qualid
qualidWithFieldIds original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Qualid"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Qualid"),
              Core.projectionField = (Core.Name "id")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "fieldIds"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

qualidAndPattern :: Phantoms.TTerm Syntax.Qualid -> Phantoms.TTerm Syntax.Pattern -> Phantoms.TTerm Syntax.QualidAndPattern
qualidAndPattern qualid pattern =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.QualidAndPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "qualid"),
          Core.fieldTerm = (Phantoms.unTTerm qualid)},
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Phantoms.unTTerm pattern)}]}))

qualidAndPatternQualid :: Phantoms.TTerm Syntax.QualidAndPattern -> Phantoms.TTerm Syntax.Qualid
qualidAndPatternQualid x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.QualidAndPattern"),
        Core.projectionField = (Core.Name "qualid")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

qualidAndPatternPattern :: Phantoms.TTerm Syntax.QualidAndPattern -> Phantoms.TTerm Syntax.Pattern
qualidAndPatternPattern x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.QualidAndPattern"),
        Core.projectionField = (Core.Name "pattern")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

qualidAndPatternWithQualid :: Phantoms.TTerm Syntax.QualidAndPattern -> Phantoms.TTerm Syntax.Qualid -> Phantoms.TTerm Syntax.QualidAndPattern
qualidAndPatternWithQualid original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.QualidAndPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "qualid"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.QualidAndPattern"),
              Core.projectionField = (Core.Name "pattern")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

qualidAndPatternWithPattern :: Phantoms.TTerm Syntax.QualidAndPattern -> Phantoms.TTerm Syntax.Pattern -> Phantoms.TTerm Syntax.QualidAndPattern
qualidAndPatternWithPattern original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.QualidAndPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "qualid"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.QualidAndPattern"),
              Core.projectionField = (Core.Name "qualid")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

qualidAnnotated :: Phantoms.TTerm Syntax.Qualid -> Phantoms.TTerm (Maybe Syntax.UnivAnnot) -> Phantoms.TTerm Syntax.QualidAnnotated
qualidAnnotated qualid univAnnot =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.QualidAnnotated"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "qualid"),
          Core.fieldTerm = (Phantoms.unTTerm qualid)},
        Core.Field {
          Core.fieldName = (Core.Name "univAnnot"),
          Core.fieldTerm = (Phantoms.unTTerm univAnnot)}]}))

qualidAnnotatedQualid :: Phantoms.TTerm Syntax.QualidAnnotated -> Phantoms.TTerm Syntax.Qualid
qualidAnnotatedQualid x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.QualidAnnotated"),
        Core.projectionField = (Core.Name "qualid")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

qualidAnnotatedUnivAnnot :: Phantoms.TTerm Syntax.QualidAnnotated -> Phantoms.TTerm (Maybe Syntax.UnivAnnot)
qualidAnnotatedUnivAnnot x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.QualidAnnotated"),
        Core.projectionField = (Core.Name "univAnnot")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

qualidAnnotatedWithQualid :: Phantoms.TTerm Syntax.QualidAnnotated -> Phantoms.TTerm Syntax.Qualid -> Phantoms.TTerm Syntax.QualidAnnotated
qualidAnnotatedWithQualid original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.QualidAnnotated"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "qualid"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "univAnnot"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.QualidAnnotated"),
              Core.projectionField = (Core.Name "univAnnot")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

qualidAnnotatedWithUnivAnnot :: Phantoms.TTerm Syntax.QualidAnnotated -> Phantoms.TTerm (Maybe Syntax.UnivAnnot) -> Phantoms.TTerm Syntax.QualidAnnotated
qualidAnnotatedWithUnivAnnot original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.QualidAnnotated"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "qualid"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.QualidAnnotated"),
              Core.projectionField = (Core.Name "qualid")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "univAnnot"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

returnAs :: Phantoms.TTerm (Maybe Syntax.Name) -> Phantoms.TTerm Syntax.Term100 -> Phantoms.TTerm Syntax.ReturnAs
returnAs as return =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.ReturnAs"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "as"),
          Core.fieldTerm = (Phantoms.unTTerm as)},
        Core.Field {
          Core.fieldName = (Core.Name "return"),
          Core.fieldTerm = (Phantoms.unTTerm return)}]}))

returnAsAs :: Phantoms.TTerm Syntax.ReturnAs -> Phantoms.TTerm (Maybe Syntax.Name)
returnAsAs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.ReturnAs"),
        Core.projectionField = (Core.Name "as")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

returnAsReturn :: Phantoms.TTerm Syntax.ReturnAs -> Phantoms.TTerm Syntax.Term100
returnAsReturn x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.ReturnAs"),
        Core.projectionField = (Core.Name "return")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

returnAsWithAs :: Phantoms.TTerm Syntax.ReturnAs -> Phantoms.TTerm (Maybe Syntax.Name) -> Phantoms.TTerm Syntax.ReturnAs
returnAsWithAs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.ReturnAs"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "as"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "return"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.ReturnAs"),
              Core.projectionField = (Core.Name "return")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

returnAsWithReturn :: Phantoms.TTerm Syntax.ReturnAs -> Phantoms.TTerm Syntax.Term100 -> Phantoms.TTerm Syntax.ReturnAs
returnAsWithReturn original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.ReturnAs"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "as"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.ReturnAs"),
              Core.projectionField = (Core.Name "as")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "return"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

scopeKey :: Phantoms.TTerm Syntax.Ident -> Phantoms.TTerm Syntax.ScopeKey
scopeKey x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.ScopeKey"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unScopeKey :: Phantoms.TTerm Syntax.ScopeKey -> Phantoms.TTerm Syntax.Ident
unScopeKey x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.fr.inria.coq.syntax.ScopeKey")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

sortSet :: Phantoms.TTerm Syntax.Sort
sortSet =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Sort"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "set"),
        Core.fieldTerm = Core.TermUnit}}))

sortProp :: Phantoms.TTerm Syntax.Sort
sortProp =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Sort"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "prop"),
        Core.fieldTerm = Core.TermUnit}}))

sortSProp :: Phantoms.TTerm Syntax.Sort
sortSProp =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Sort"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "sProp"),
        Core.fieldTerm = Core.TermUnit}}))

sortType :: Phantoms.TTerm Syntax.Sort
sortType =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Sort"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "type"),
        Core.fieldTerm = Core.TermUnit}}))

sortTypeWithAnyUniverse :: Phantoms.TTerm Syntax.Sort
sortTypeWithAnyUniverse =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Sort"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "typeWithAnyUniverse"),
        Core.fieldTerm = Core.TermUnit}}))

sortTypeWithUniverse :: Phantoms.TTerm Syntax.Universe -> Phantoms.TTerm Syntax.Sort
sortTypeWithUniverse x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Sort"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "typeWithUniverse"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

string :: Phantoms.TTerm String -> Phantoms.TTerm Syntax.String_
string x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.String"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unString :: Phantoms.TTerm Syntax.String_ -> Phantoms.TTerm String
unString x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.fr.inria.coq.syntax.String")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

termForallOrFun :: Phantoms.TTerm Syntax.ForallOrFun -> Phantoms.TTerm Syntax.Term
termForallOrFun x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Term"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "forallOrFun"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

termLet :: Phantoms.TTerm Syntax.Let -> Phantoms.TTerm Syntax.Term
termLet x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Term"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "let"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

termIf :: Phantoms.TTerm Syntax.If -> Phantoms.TTerm Syntax.Term
termIf x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Term"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "if"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

termFix :: Phantoms.TTerm Syntax.Fix -> Phantoms.TTerm Syntax.Term
termFix x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Term"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "fix"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

termCofix :: Phantoms.TTerm Syntax.Cofix -> Phantoms.TTerm Syntax.Term
termCofix x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Term"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "cofix"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

termTerm100 :: Phantoms.TTerm Syntax.Term100 -> Phantoms.TTerm Syntax.Term
termTerm100 x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Term"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "term100"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

term0QualidAnnotated :: Phantoms.TTerm Syntax.QualidAnnotated -> Phantoms.TTerm Syntax.Term0
term0QualidAnnotated x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Term0"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "qualidAnnotated"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

term0Sort :: Phantoms.TTerm Syntax.Sort -> Phantoms.TTerm Syntax.Term0
term0Sort x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Term0"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "sort"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

term0PrimitiveNotations :: Phantoms.TTerm Syntax.PrimitiveNotations -> Phantoms.TTerm Syntax.Term0
term0PrimitiveNotations x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Term0"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "primitiveNotations"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

term0Evar :: Phantoms.TTerm Syntax.ExistentialVariable -> Phantoms.TTerm Syntax.Term0
term0Evar x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Term0"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "evar"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

term0Match :: Phantoms.TTerm Syntax.Match -> Phantoms.TTerm Syntax.Term0
term0Match x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Term0"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "match"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

term0Record :: Phantoms.TTerm Syntax.Term0
term0Record =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Term0"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "record"),
        Core.fieldTerm = Core.TermUnit}}))

term0Generalizing :: Phantoms.TTerm Syntax.Term0
term0Generalizing =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Term0"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "generalizing"),
        Core.fieldTerm = Core.TermUnit}}))

term0Ltac :: Phantoms.TTerm Syntax.Term0
term0Ltac =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Term0"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "ltac"),
        Core.fieldTerm = Core.TermUnit}}))

term0Parens :: Phantoms.TTerm Syntax.Term -> Phantoms.TTerm Syntax.Term0
term0Parens x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Term0"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "parens"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

term1Projection :: Phantoms.TTerm Syntax.Term1
term1Projection =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Term1"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "projection"),
        Core.fieldTerm = Core.TermUnit}}))

term1Scope :: Phantoms.TTerm Syntax.Term1
term1Scope =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Term1"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "scope"),
        Core.fieldTerm = Core.TermUnit}}))

term1Term0 :: Phantoms.TTerm Syntax.Term0 -> Phantoms.TTerm Syntax.Term1
term1Term0 x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Term1"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "term0"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

term10Application :: Phantoms.TTerm Syntax.Application -> Phantoms.TTerm Syntax.Term10
term10Application x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Term10"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "application"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

term10OneTerm :: Phantoms.TTerm Syntax.OneTerm -> Phantoms.TTerm Syntax.Term10
term10OneTerm x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Term10"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "oneTerm"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

term100Cast :: Phantoms.TTerm Syntax.TypeCast -> Phantoms.TTerm Syntax.Term100
term100Cast x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Term100"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "cast"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

term100Term10 :: Phantoms.TTerm Syntax.Term10 -> Phantoms.TTerm Syntax.Term100
term100Term10 x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Term100"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "term10"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

type_ :: Phantoms.TTerm Syntax.Term -> Phantoms.TTerm Syntax.Type
type_ x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Type"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unType :: Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.Term
unType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.fr.inria.coq.syntax.Type")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

typeCast :: Phantoms.TTerm Syntax.Term10 -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.TypeCastOperator -> Phantoms.TTerm Syntax.TypeCast
typeCast term type_ operator =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.TypeCast"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Phantoms.unTTerm term)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm type_)},
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Phantoms.unTTerm operator)}]}))

typeCastTerm :: Phantoms.TTerm Syntax.TypeCast -> Phantoms.TTerm Syntax.Term10
typeCastTerm x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.TypeCast"),
        Core.projectionField = (Core.Name "term")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

typeCastType :: Phantoms.TTerm Syntax.TypeCast -> Phantoms.TTerm Syntax.Type
typeCastType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.TypeCast"),
        Core.projectionField = (Core.Name "type")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

typeCastOperator :: Phantoms.TTerm Syntax.TypeCast -> Phantoms.TTerm Syntax.TypeCastOperator
typeCastOperator x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.TypeCast"),
        Core.projectionField = (Core.Name "operator")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

typeCastWithTerm :: Phantoms.TTerm Syntax.TypeCast -> Phantoms.TTerm Syntax.Term10 -> Phantoms.TTerm Syntax.TypeCast
typeCastWithTerm original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.TypeCast"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.TypeCast"),
              Core.projectionField = (Core.Name "type")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.TypeCast"),
              Core.projectionField = (Core.Name "operator")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

typeCastWithType :: Phantoms.TTerm Syntax.TypeCast -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.TypeCast
typeCastWithType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.TypeCast"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.TypeCast"),
              Core.projectionField = (Core.Name "term")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.TypeCast"),
              Core.projectionField = (Core.Name "operator")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

typeCastWithOperator :: Phantoms.TTerm Syntax.TypeCast -> Phantoms.TTerm Syntax.TypeCastOperator -> Phantoms.TTerm Syntax.TypeCast
typeCastWithOperator original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.TypeCast"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.TypeCast"),
              Core.projectionField = (Core.Name "term")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.TypeCast"),
              Core.projectionField = (Core.Name "type")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

typeCastOperatorNormal :: Phantoms.TTerm Syntax.TypeCastOperator
typeCastOperatorNormal =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.TypeCastOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "normal"),
        Core.fieldTerm = Core.TermUnit}}))

typeCastOperatorVmCompute :: Phantoms.TTerm Syntax.TypeCastOperator
typeCastOperatorVmCompute =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.TypeCastOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "vmCompute"),
        Core.fieldTerm = Core.TermUnit}}))

typeCastOperatorNativeCompute :: Phantoms.TTerm Syntax.TypeCastOperator
typeCastOperatorNativeCompute =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.TypeCastOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "nativeCompute"),
        Core.fieldTerm = Core.TermUnit}}))

typeBinders :: Phantoms.TTerm [Syntax.Name] -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.TypeBinders
typeBinders names type_ =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.TypeBinders"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "names"),
          Core.fieldTerm = (Phantoms.unTTerm names)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm type_)}]}))

typeBindersNames :: Phantoms.TTerm Syntax.TypeBinders -> Phantoms.TTerm [Syntax.Name]
typeBindersNames x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.TypeBinders"),
        Core.projectionField = (Core.Name "names")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

typeBindersType :: Phantoms.TTerm Syntax.TypeBinders -> Phantoms.TTerm Syntax.Type
typeBindersType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.TypeBinders"),
        Core.projectionField = (Core.Name "type")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

typeBindersWithNames :: Phantoms.TTerm Syntax.TypeBinders -> Phantoms.TTerm [Syntax.Name] -> Phantoms.TTerm Syntax.TypeBinders
typeBindersWithNames original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.TypeBinders"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "names"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.TypeBinders"),
              Core.projectionField = (Core.Name "type")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

typeBindersWithType :: Phantoms.TTerm Syntax.TypeBinders -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.TypeBinders
typeBindersWithType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.TypeBinders"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "names"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.TypeBinders"),
              Core.projectionField = (Core.Name "names")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

typeclassConstraint :: Phantoms.TTerm (Maybe Syntax.Name) -> Phantoms.TTerm Bool -> Phantoms.TTerm Syntax.Term -> Phantoms.TTerm Syntax.TypeclassConstraint
typeclassConstraint name generalizing term =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.TypeclassConstraint"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "generalizing"),
          Core.fieldTerm = (Phantoms.unTTerm generalizing)},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Phantoms.unTTerm term)}]}))

typeclassConstraintName :: Phantoms.TTerm Syntax.TypeclassConstraint -> Phantoms.TTerm (Maybe Syntax.Name)
typeclassConstraintName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.TypeclassConstraint"),
        Core.projectionField = (Core.Name "name")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

typeclassConstraintGeneralizing :: Phantoms.TTerm Syntax.TypeclassConstraint -> Phantoms.TTerm Bool
typeclassConstraintGeneralizing x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.TypeclassConstraint"),
        Core.projectionField = (Core.Name "generalizing")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

typeclassConstraintTerm :: Phantoms.TTerm Syntax.TypeclassConstraint -> Phantoms.TTerm Syntax.Term
typeclassConstraintTerm x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.TypeclassConstraint"),
        Core.projectionField = (Core.Name "term")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

typeclassConstraintWithName :: Phantoms.TTerm Syntax.TypeclassConstraint -> Phantoms.TTerm (Maybe Syntax.Name) -> Phantoms.TTerm Syntax.TypeclassConstraint
typeclassConstraintWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.TypeclassConstraint"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "generalizing"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.TypeclassConstraint"),
              Core.projectionField = (Core.Name "generalizing")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.TypeclassConstraint"),
              Core.projectionField = (Core.Name "term")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

typeclassConstraintWithGeneralizing :: Phantoms.TTerm Syntax.TypeclassConstraint -> Phantoms.TTerm Bool -> Phantoms.TTerm Syntax.TypeclassConstraint
typeclassConstraintWithGeneralizing original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.TypeclassConstraint"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.TypeclassConstraint"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "generalizing"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.TypeclassConstraint"),
              Core.projectionField = (Core.Name "term")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

typeclassConstraintWithTerm :: Phantoms.TTerm Syntax.TypeclassConstraint -> Phantoms.TTerm Syntax.Term -> Phantoms.TTerm Syntax.TypeclassConstraint
typeclassConstraintWithTerm original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.TypeclassConstraint"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.TypeclassConstraint"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "generalizing"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.TypeclassConstraint"),
              Core.projectionField = (Core.Name "generalizing")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

univAnnot :: Phantoms.TTerm [Syntax.UniverseLevel] -> Phantoms.TTerm Syntax.UnivAnnot
univAnnot x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.UnivAnnot"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unUnivAnnot :: Phantoms.TTerm Syntax.UnivAnnot -> Phantoms.TTerm [Syntax.UniverseLevel]
unUnivAnnot x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.fr.inria.coq.syntax.UnivAnnot")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

universeMax :: Phantoms.TTerm [Syntax.Universe_Expr] -> Phantoms.TTerm Syntax.Universe
universeMax x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Universe"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "max"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

universeExpr :: Phantoms.TTerm Syntax.Universe_Expr -> Phantoms.TTerm Syntax.Universe
universeExpr x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Universe"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "expr"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

universe_Expr :: Phantoms.TTerm Syntax.UniverseName -> Phantoms.TTerm (Maybe Syntax.Natural) -> Phantoms.TTerm Syntax.Universe_Expr
universe_Expr name number =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Universe_Expr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "number"),
          Core.fieldTerm = (Phantoms.unTTerm number)}]}))

universe_ExprName :: Phantoms.TTerm Syntax.Universe_Expr -> Phantoms.TTerm Syntax.UniverseName
universe_ExprName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Universe_Expr"),
        Core.projectionField = (Core.Name "name")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

universe_ExprNumber :: Phantoms.TTerm Syntax.Universe_Expr -> Phantoms.TTerm (Maybe Syntax.Natural)
universe_ExprNumber x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Universe_Expr"),
        Core.projectionField = (Core.Name "number")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

universe_ExprWithName :: Phantoms.TTerm Syntax.Universe_Expr -> Phantoms.TTerm Syntax.UniverseName -> Phantoms.TTerm Syntax.Universe_Expr
universe_ExprWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Universe_Expr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "number"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Universe_Expr"),
              Core.projectionField = (Core.Name "number")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

universe_ExprWithNumber :: Phantoms.TTerm Syntax.Universe_Expr -> Phantoms.TTerm (Maybe Syntax.Natural) -> Phantoms.TTerm Syntax.Universe_Expr
universe_ExprWithNumber original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Universe_Expr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.Universe_Expr"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "number"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

universeLevelSet :: Phantoms.TTerm Syntax.UniverseLevel
universeLevelSet =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.UniverseLevel"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "set"),
        Core.fieldTerm = Core.TermUnit}}))

universeLevelProp :: Phantoms.TTerm Syntax.UniverseLevel
universeLevelProp =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.UniverseLevel"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "prop"),
        Core.fieldTerm = Core.TermUnit}}))

universeLevelType :: Phantoms.TTerm Syntax.UniverseLevel
universeLevelType =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.UniverseLevel"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "type"),
        Core.fieldTerm = Core.TermUnit}}))

universeLevelIgnored :: Phantoms.TTerm Syntax.UniverseLevel
universeLevelIgnored =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.UniverseLevel"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "ignored"),
        Core.fieldTerm = Core.TermUnit}}))

universeLevelQualid :: Phantoms.TTerm Syntax.Qualid -> Phantoms.TTerm Syntax.UniverseLevel
universeLevelQualid x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.UniverseLevel"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "qualid"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

universeNameQualid :: Phantoms.TTerm Syntax.Qualid -> Phantoms.TTerm Syntax.UniverseName
universeNameQualid x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.UniverseName"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "qualid"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

universeNameSet :: Phantoms.TTerm Syntax.UniverseName
universeNameSet =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.UniverseName"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "set"),
        Core.fieldTerm = Core.TermUnit}}))

universeNameProp :: Phantoms.TTerm Syntax.UniverseName
universeNameProp =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.fr.inria.coq.syntax.UniverseName"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "prop"),
        Core.fieldTerm = Core.TermUnit}}))
