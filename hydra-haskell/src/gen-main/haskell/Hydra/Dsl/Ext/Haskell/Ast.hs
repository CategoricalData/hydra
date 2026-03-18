-- Note: this is an automatically generated file. Do not edit.

-- | DSL functions for hydra.ext.haskell.ast

module Hydra.Dsl.Ext.Haskell.Ast where

import qualified Hydra.Core as Core
import qualified Hydra.Ext.Haskell.Ast as Ast
import qualified Hydra.Phantoms as Phantoms
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

alternative :: Phantoms.TTerm Ast.Pattern -> Phantoms.TTerm Ast.CaseRhs -> Phantoms.TTerm (Maybe Ast.LocalBindings) -> Phantoms.TTerm Ast.Alternative
alternative pattern rhs binds =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.ast.Alternative"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Phantoms.unTTerm pattern)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTTerm rhs)},
        Core.Field {
          Core.fieldName = (Core.Name "binds"),
          Core.fieldTerm = (Phantoms.unTTerm binds)}]}))

alternativePattern :: Phantoms.TTerm Ast.Alternative -> Phantoms.TTerm Ast.Pattern
alternativePattern x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.Alternative"),
        Core.projectionField = (Core.Name "pattern")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

alternativeRhs :: Phantoms.TTerm Ast.Alternative -> Phantoms.TTerm Ast.CaseRhs
alternativeRhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.Alternative"),
        Core.projectionField = (Core.Name "rhs")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

alternativeBinds :: Phantoms.TTerm Ast.Alternative -> Phantoms.TTerm (Maybe Ast.LocalBindings)
alternativeBinds x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.Alternative"),
        Core.projectionField = (Core.Name "binds")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

alternativeWithPattern :: Phantoms.TTerm Ast.Alternative -> Phantoms.TTerm Ast.Pattern -> Phantoms.TTerm Ast.Alternative
alternativeWithPattern original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.ast.Alternative"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.Alternative"),
              Core.projectionField = (Core.Name "rhs")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "binds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.Alternative"),
              Core.projectionField = (Core.Name "binds")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

alternativeWithRhs :: Phantoms.TTerm Ast.Alternative -> Phantoms.TTerm Ast.CaseRhs -> Phantoms.TTerm Ast.Alternative
alternativeWithRhs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.ast.Alternative"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.Alternative"),
              Core.projectionField = (Core.Name "pattern")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "binds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.Alternative"),
              Core.projectionField = (Core.Name "binds")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

alternativeWithBinds :: Phantoms.TTerm Ast.Alternative -> Phantoms.TTerm (Maybe Ast.LocalBindings) -> Phantoms.TTerm Ast.Alternative
alternativeWithBinds original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.ast.Alternative"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.Alternative"),
              Core.projectionField = (Core.Name "pattern")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.Alternative"),
              Core.projectionField = (Core.Name "rhs")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "binds"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

assertionClass :: Phantoms.TTerm Ast.ClassAssertion -> Phantoms.TTerm Ast.Assertion
assertionClass x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.haskell.ast.Assertion"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "class"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

assertionTuple :: Phantoms.TTerm [Ast.Assertion] -> Phantoms.TTerm Ast.Assertion
assertionTuple x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.haskell.ast.Assertion"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "tuple"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

classAssertion :: Phantoms.TTerm Ast.Name -> Phantoms.TTerm [Ast.Type] -> Phantoms.TTerm Ast.ClassAssertion
classAssertion name types =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.ast.ClassAssertion"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "types"),
          Core.fieldTerm = (Phantoms.unTTerm types)}]}))

classAssertionName :: Phantoms.TTerm Ast.ClassAssertion -> Phantoms.TTerm Ast.Name
classAssertionName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.ClassAssertion"),
        Core.projectionField = (Core.Name "name")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

classAssertionTypes :: Phantoms.TTerm Ast.ClassAssertion -> Phantoms.TTerm [Ast.Type]
classAssertionTypes x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.ClassAssertion"),
        Core.projectionField = (Core.Name "types")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

classAssertionWithName :: Phantoms.TTerm Ast.ClassAssertion -> Phantoms.TTerm Ast.Name -> Phantoms.TTerm Ast.ClassAssertion
classAssertionWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.ast.ClassAssertion"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "types"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.ClassAssertion"),
              Core.projectionField = (Core.Name "types")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

classAssertionWithTypes :: Phantoms.TTerm Ast.ClassAssertion -> Phantoms.TTerm [Ast.Type] -> Phantoms.TTerm Ast.ClassAssertion
classAssertionWithTypes original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.ast.ClassAssertion"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.ClassAssertion"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "types"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

caseRhs :: Phantoms.TTerm Ast.Expression -> Phantoms.TTerm Ast.CaseRhs
caseRhs x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.haskell.ast.CaseRhs"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unCaseRhs :: Phantoms.TTerm Ast.CaseRhs -> Phantoms.TTerm Ast.Expression
unCaseRhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.haskell.ast.CaseRhs")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

constructorOrdinary :: Phantoms.TTerm Ast.OrdinaryConstructor -> Phantoms.TTerm Ast.Constructor
constructorOrdinary x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.haskell.ast.Constructor"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "ordinary"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

constructorRecord :: Phantoms.TTerm Ast.RecordConstructor -> Phantoms.TTerm Ast.Constructor
constructorRecord x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.haskell.ast.Constructor"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "record"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

ordinaryConstructor :: Phantoms.TTerm Ast.Name -> Phantoms.TTerm [Ast.Type] -> Phantoms.TTerm Ast.OrdinaryConstructor
ordinaryConstructor name fields =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.ast.OrdinaryConstructor"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "fields"),
          Core.fieldTerm = (Phantoms.unTTerm fields)}]}))

ordinaryConstructorName :: Phantoms.TTerm Ast.OrdinaryConstructor -> Phantoms.TTerm Ast.Name
ordinaryConstructorName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.OrdinaryConstructor"),
        Core.projectionField = (Core.Name "name")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

ordinaryConstructorFields :: Phantoms.TTerm Ast.OrdinaryConstructor -> Phantoms.TTerm [Ast.Type]
ordinaryConstructorFields x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.OrdinaryConstructor"),
        Core.projectionField = (Core.Name "fields")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

ordinaryConstructorWithName :: Phantoms.TTerm Ast.OrdinaryConstructor -> Phantoms.TTerm Ast.Name -> Phantoms.TTerm Ast.OrdinaryConstructor
ordinaryConstructorWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.ast.OrdinaryConstructor"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "fields"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.OrdinaryConstructor"),
              Core.projectionField = (Core.Name "fields")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

ordinaryConstructorWithFields :: Phantoms.TTerm Ast.OrdinaryConstructor -> Phantoms.TTerm [Ast.Type] -> Phantoms.TTerm Ast.OrdinaryConstructor
ordinaryConstructorWithFields original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.ast.OrdinaryConstructor"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.OrdinaryConstructor"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "fields"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

recordConstructor :: Phantoms.TTerm Ast.Name -> Phantoms.TTerm [Ast.FieldWithComments] -> Phantoms.TTerm Ast.RecordConstructor
recordConstructor name fields =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.ast.RecordConstructor"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "fields"),
          Core.fieldTerm = (Phantoms.unTTerm fields)}]}))

recordConstructorName :: Phantoms.TTerm Ast.RecordConstructor -> Phantoms.TTerm Ast.Name
recordConstructorName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.RecordConstructor"),
        Core.projectionField = (Core.Name "name")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

recordConstructorFields :: Phantoms.TTerm Ast.RecordConstructor -> Phantoms.TTerm [Ast.FieldWithComments]
recordConstructorFields x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.RecordConstructor"),
        Core.projectionField = (Core.Name "fields")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

recordConstructorWithName :: Phantoms.TTerm Ast.RecordConstructor -> Phantoms.TTerm Ast.Name -> Phantoms.TTerm Ast.RecordConstructor
recordConstructorWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.ast.RecordConstructor"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "fields"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.RecordConstructor"),
              Core.projectionField = (Core.Name "fields")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

recordConstructorWithFields :: Phantoms.TTerm Ast.RecordConstructor -> Phantoms.TTerm [Ast.FieldWithComments] -> Phantoms.TTerm Ast.RecordConstructor
recordConstructorWithFields original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.ast.RecordConstructor"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.RecordConstructor"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "fields"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

constructorWithComments :: Phantoms.TTerm Ast.Constructor -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Ast.ConstructorWithComments
constructorWithComments body comments =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.ast.ConstructorWithComments"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm body)},
        Core.Field {
          Core.fieldName = (Core.Name "comments"),
          Core.fieldTerm = (Phantoms.unTTerm comments)}]}))

constructorWithCommentsBody :: Phantoms.TTerm Ast.ConstructorWithComments -> Phantoms.TTerm Ast.Constructor
constructorWithCommentsBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.ConstructorWithComments"),
        Core.projectionField = (Core.Name "body")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

constructorWithCommentsComments :: Phantoms.TTerm Ast.ConstructorWithComments -> Phantoms.TTerm (Maybe String)
constructorWithCommentsComments x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.ConstructorWithComments"),
        Core.projectionField = (Core.Name "comments")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

constructorWithCommentsWithBody :: Phantoms.TTerm Ast.ConstructorWithComments -> Phantoms.TTerm Ast.Constructor -> Phantoms.TTerm Ast.ConstructorWithComments
constructorWithCommentsWithBody original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.ast.ConstructorWithComments"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "comments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.ConstructorWithComments"),
              Core.projectionField = (Core.Name "comments")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

constructorWithCommentsWithComments :: Phantoms.TTerm Ast.ConstructorWithComments -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Ast.ConstructorWithComments
constructorWithCommentsWithComments original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.ast.ConstructorWithComments"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.ConstructorWithComments"),
              Core.projectionField = (Core.Name "body")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "comments"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

dataDeclaration :: Phantoms.TTerm Ast.DataOrNewtype -> Phantoms.TTerm [Ast.Assertion] -> Phantoms.TTerm Ast.DeclarationHead -> Phantoms.TTerm [Ast.ConstructorWithComments] -> Phantoms.TTerm [Ast.Deriving] -> Phantoms.TTerm Ast.DataDeclaration
dataDeclaration keyword context head constructors deriving_ =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.ast.DataDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "keyword"),
          Core.fieldTerm = (Phantoms.unTTerm keyword)},
        Core.Field {
          Core.fieldName = (Core.Name "context"),
          Core.fieldTerm = (Phantoms.unTTerm context)},
        Core.Field {
          Core.fieldName = (Core.Name "head"),
          Core.fieldTerm = (Phantoms.unTTerm head)},
        Core.Field {
          Core.fieldName = (Core.Name "constructors"),
          Core.fieldTerm = (Phantoms.unTTerm constructors)},
        Core.Field {
          Core.fieldName = (Core.Name "deriving"),
          Core.fieldTerm = (Phantoms.unTTerm deriving_)}]}))

dataDeclarationKeyword :: Phantoms.TTerm Ast.DataDeclaration -> Phantoms.TTerm Ast.DataOrNewtype
dataDeclarationKeyword x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.DataDeclaration"),
        Core.projectionField = (Core.Name "keyword")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

dataDeclarationContext :: Phantoms.TTerm Ast.DataDeclaration -> Phantoms.TTerm [Ast.Assertion]
dataDeclarationContext x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.DataDeclaration"),
        Core.projectionField = (Core.Name "context")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

dataDeclarationHead :: Phantoms.TTerm Ast.DataDeclaration -> Phantoms.TTerm Ast.DeclarationHead
dataDeclarationHead x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.DataDeclaration"),
        Core.projectionField = (Core.Name "head")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

dataDeclarationConstructors :: Phantoms.TTerm Ast.DataDeclaration -> Phantoms.TTerm [Ast.ConstructorWithComments]
dataDeclarationConstructors x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.DataDeclaration"),
        Core.projectionField = (Core.Name "constructors")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

dataDeclarationDeriving :: Phantoms.TTerm Ast.DataDeclaration -> Phantoms.TTerm [Ast.Deriving]
dataDeclarationDeriving x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.DataDeclaration"),
        Core.projectionField = (Core.Name "deriving")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

dataDeclarationWithKeyword :: Phantoms.TTerm Ast.DataDeclaration -> Phantoms.TTerm Ast.DataOrNewtype -> Phantoms.TTerm Ast.DataDeclaration
dataDeclarationWithKeyword original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.ast.DataDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "keyword"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "context"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.DataDeclaration"),
              Core.projectionField = (Core.Name "context")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "head"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.DataDeclaration"),
              Core.projectionField = (Core.Name "head")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "constructors"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.DataDeclaration"),
              Core.projectionField = (Core.Name "constructors")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "deriving"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.DataDeclaration"),
              Core.projectionField = (Core.Name "deriving")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

dataDeclarationWithContext :: Phantoms.TTerm Ast.DataDeclaration -> Phantoms.TTerm [Ast.Assertion] -> Phantoms.TTerm Ast.DataDeclaration
dataDeclarationWithContext original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.ast.DataDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "keyword"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.DataDeclaration"),
              Core.projectionField = (Core.Name "keyword")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "context"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "head"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.DataDeclaration"),
              Core.projectionField = (Core.Name "head")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "constructors"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.DataDeclaration"),
              Core.projectionField = (Core.Name "constructors")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "deriving"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.DataDeclaration"),
              Core.projectionField = (Core.Name "deriving")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

dataDeclarationWithHead :: Phantoms.TTerm Ast.DataDeclaration -> Phantoms.TTerm Ast.DeclarationHead -> Phantoms.TTerm Ast.DataDeclaration
dataDeclarationWithHead original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.ast.DataDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "keyword"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.DataDeclaration"),
              Core.projectionField = (Core.Name "keyword")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "context"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.DataDeclaration"),
              Core.projectionField = (Core.Name "context")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "head"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "constructors"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.DataDeclaration"),
              Core.projectionField = (Core.Name "constructors")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "deriving"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.DataDeclaration"),
              Core.projectionField = (Core.Name "deriving")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

dataDeclarationWithConstructors :: Phantoms.TTerm Ast.DataDeclaration -> Phantoms.TTerm [Ast.ConstructorWithComments] -> Phantoms.TTerm Ast.DataDeclaration
dataDeclarationWithConstructors original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.ast.DataDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "keyword"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.DataDeclaration"),
              Core.projectionField = (Core.Name "keyword")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "context"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.DataDeclaration"),
              Core.projectionField = (Core.Name "context")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "head"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.DataDeclaration"),
              Core.projectionField = (Core.Name "head")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "constructors"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "deriving"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.DataDeclaration"),
              Core.projectionField = (Core.Name "deriving")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

dataDeclarationWithDeriving :: Phantoms.TTerm Ast.DataDeclaration -> Phantoms.TTerm [Ast.Deriving] -> Phantoms.TTerm Ast.DataDeclaration
dataDeclarationWithDeriving original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.ast.DataDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "keyword"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.DataDeclaration"),
              Core.projectionField = (Core.Name "keyword")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "context"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.DataDeclaration"),
              Core.projectionField = (Core.Name "context")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "head"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.DataDeclaration"),
              Core.projectionField = (Core.Name "head")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "constructors"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.DataDeclaration"),
              Core.projectionField = (Core.Name "constructors")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "deriving"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

dataOrNewtypeData :: Phantoms.TTerm Ast.DataOrNewtype
dataOrNewtypeData =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.haskell.ast.DataOrNewtype"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "data"),
        Core.fieldTerm = Core.TermUnit}}))

dataOrNewtypeNewtype :: Phantoms.TTerm Ast.DataOrNewtype
dataOrNewtypeNewtype =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.haskell.ast.DataOrNewtype"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "newtype"),
        Core.fieldTerm = Core.TermUnit}}))

declarationWithComments :: Phantoms.TTerm Ast.Declaration -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Ast.DeclarationWithComments
declarationWithComments body comments =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.ast.DeclarationWithComments"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm body)},
        Core.Field {
          Core.fieldName = (Core.Name "comments"),
          Core.fieldTerm = (Phantoms.unTTerm comments)}]}))

declarationWithCommentsBody :: Phantoms.TTerm Ast.DeclarationWithComments -> Phantoms.TTerm Ast.Declaration
declarationWithCommentsBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.DeclarationWithComments"),
        Core.projectionField = (Core.Name "body")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

declarationWithCommentsComments :: Phantoms.TTerm Ast.DeclarationWithComments -> Phantoms.TTerm (Maybe String)
declarationWithCommentsComments x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.DeclarationWithComments"),
        Core.projectionField = (Core.Name "comments")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

declarationWithCommentsWithBody :: Phantoms.TTerm Ast.DeclarationWithComments -> Phantoms.TTerm Ast.Declaration -> Phantoms.TTerm Ast.DeclarationWithComments
declarationWithCommentsWithBody original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.ast.DeclarationWithComments"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "comments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.DeclarationWithComments"),
              Core.projectionField = (Core.Name "comments")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

declarationWithCommentsWithComments :: Phantoms.TTerm Ast.DeclarationWithComments -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Ast.DeclarationWithComments
declarationWithCommentsWithComments original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.ast.DeclarationWithComments"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.DeclarationWithComments"),
              Core.projectionField = (Core.Name "body")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "comments"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

declarationData :: Phantoms.TTerm Ast.DataDeclaration -> Phantoms.TTerm Ast.Declaration
declarationData x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.haskell.ast.Declaration"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "data"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

declarationType :: Phantoms.TTerm Ast.TypeDeclaration -> Phantoms.TTerm Ast.Declaration
declarationType x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.haskell.ast.Declaration"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "type"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

declarationValueBinding :: Phantoms.TTerm Ast.ValueBinding -> Phantoms.TTerm Ast.Declaration
declarationValueBinding x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.haskell.ast.Declaration"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "valueBinding"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

declarationTypedBinding :: Phantoms.TTerm Ast.TypedBinding -> Phantoms.TTerm Ast.Declaration
declarationTypedBinding x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.haskell.ast.Declaration"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "typedBinding"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

declarationHeadApplication :: Phantoms.TTerm Ast.ApplicationDeclarationHead -> Phantoms.TTerm Ast.DeclarationHead
declarationHeadApplication x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.haskell.ast.DeclarationHead"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "application"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

declarationHeadParens :: Phantoms.TTerm Ast.DeclarationHead -> Phantoms.TTerm Ast.DeclarationHead
declarationHeadParens x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.haskell.ast.DeclarationHead"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "parens"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

declarationHeadSimple :: Phantoms.TTerm Ast.Name -> Phantoms.TTerm Ast.DeclarationHead
declarationHeadSimple x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.haskell.ast.DeclarationHead"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "simple"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

applicationDeclarationHead :: Phantoms.TTerm Ast.DeclarationHead -> Phantoms.TTerm Ast.Variable -> Phantoms.TTerm Ast.ApplicationDeclarationHead
applicationDeclarationHead function operand =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.ast.ApplicationDeclarationHead"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "function"),
          Core.fieldTerm = (Phantoms.unTTerm function)},
        Core.Field {
          Core.fieldName = (Core.Name "operand"),
          Core.fieldTerm = (Phantoms.unTTerm operand)}]}))

applicationDeclarationHeadFunction :: Phantoms.TTerm Ast.ApplicationDeclarationHead -> Phantoms.TTerm Ast.DeclarationHead
applicationDeclarationHeadFunction x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.ApplicationDeclarationHead"),
        Core.projectionField = (Core.Name "function")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

applicationDeclarationHeadOperand :: Phantoms.TTerm Ast.ApplicationDeclarationHead -> Phantoms.TTerm Ast.Variable
applicationDeclarationHeadOperand x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.ApplicationDeclarationHead"),
        Core.projectionField = (Core.Name "operand")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

applicationDeclarationHeadWithFunction :: Phantoms.TTerm Ast.ApplicationDeclarationHead -> Phantoms.TTerm Ast.DeclarationHead -> Phantoms.TTerm Ast.ApplicationDeclarationHead
applicationDeclarationHeadWithFunction original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.ast.ApplicationDeclarationHead"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "function"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "operand"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.ApplicationDeclarationHead"),
              Core.projectionField = (Core.Name "operand")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

applicationDeclarationHeadWithOperand :: Phantoms.TTerm Ast.ApplicationDeclarationHead -> Phantoms.TTerm Ast.Variable -> Phantoms.TTerm Ast.ApplicationDeclarationHead
applicationDeclarationHeadWithOperand original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.ast.ApplicationDeclarationHead"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "function"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.ApplicationDeclarationHead"),
              Core.projectionField = (Core.Name "function")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "operand"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

deriving_ :: Phantoms.TTerm [Ast.Name] -> Phantoms.TTerm Ast.Deriving
deriving_ x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.haskell.ast.Deriving"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unDeriving :: Phantoms.TTerm Ast.Deriving -> Phantoms.TTerm [Ast.Name]
unDeriving x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.haskell.ast.Deriving")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

exportDeclaration :: Phantoms.TTerm Ast.ImportExportSpec -> Phantoms.TTerm Ast.Export
exportDeclaration x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.haskell.ast.Export"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "declaration"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

exportModule :: Phantoms.TTerm Ast.ModuleName -> Phantoms.TTerm Ast.Export
exportModule x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.haskell.ast.Export"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "module"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

expressionApplication :: Phantoms.TTerm Ast.ApplicationExpression -> Phantoms.TTerm Ast.Expression
expressionApplication x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.haskell.ast.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "application"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

expressionCase :: Phantoms.TTerm Ast.CaseExpression -> Phantoms.TTerm Ast.Expression
expressionCase x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.haskell.ast.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "case"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

expressionConstructRecord :: Phantoms.TTerm Ast.ConstructRecordExpression -> Phantoms.TTerm Ast.Expression
expressionConstructRecord x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.haskell.ast.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "constructRecord"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

expressionDo :: Phantoms.TTerm [Ast.Statement] -> Phantoms.TTerm Ast.Expression
expressionDo x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.haskell.ast.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "do"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

expressionIf :: Phantoms.TTerm Ast.IfExpression -> Phantoms.TTerm Ast.Expression
expressionIf x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.haskell.ast.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "if"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

expressionInfixApplication :: Phantoms.TTerm Ast.InfixApplicationExpression -> Phantoms.TTerm Ast.Expression
expressionInfixApplication x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.haskell.ast.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "infixApplication"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

expressionLiteral :: Phantoms.TTerm Ast.Literal -> Phantoms.TTerm Ast.Expression
expressionLiteral x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.haskell.ast.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "literal"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

expressionLambda :: Phantoms.TTerm Ast.LambdaExpression -> Phantoms.TTerm Ast.Expression
expressionLambda x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.haskell.ast.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "lambda"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

expressionLeftSection :: Phantoms.TTerm Ast.SectionExpression -> Phantoms.TTerm Ast.Expression
expressionLeftSection x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.haskell.ast.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "leftSection"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

expressionLet :: Phantoms.TTerm Ast.LetExpression -> Phantoms.TTerm Ast.Expression
expressionLet x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.haskell.ast.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "let"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

expressionList :: Phantoms.TTerm [Ast.Expression] -> Phantoms.TTerm Ast.Expression
expressionList x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.haskell.ast.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "list"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

expressionParens :: Phantoms.TTerm Ast.Expression -> Phantoms.TTerm Ast.Expression
expressionParens x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.haskell.ast.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "parens"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

expressionPrefixApplication :: Phantoms.TTerm Ast.PrefixApplicationExpression -> Phantoms.TTerm Ast.Expression
expressionPrefixApplication x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.haskell.ast.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "prefixApplication"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

expressionRightSection :: Phantoms.TTerm Ast.SectionExpression -> Phantoms.TTerm Ast.Expression
expressionRightSection x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.haskell.ast.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "rightSection"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

expressionTuple :: Phantoms.TTerm [Ast.Expression] -> Phantoms.TTerm Ast.Expression
expressionTuple x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.haskell.ast.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "tuple"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

expressionTypeSignature :: Phantoms.TTerm Ast.TypeSignatureExpression -> Phantoms.TTerm Ast.Expression
expressionTypeSignature x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.haskell.ast.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "typeSignature"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

expressionUpdateRecord :: Phantoms.TTerm Ast.UpdateRecordExpression -> Phantoms.TTerm Ast.Expression
expressionUpdateRecord x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.haskell.ast.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "updateRecord"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

expressionVariable :: Phantoms.TTerm Ast.Name -> Phantoms.TTerm Ast.Expression
expressionVariable x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.haskell.ast.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "variable"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

applicationExpression :: Phantoms.TTerm Ast.Expression -> Phantoms.TTerm Ast.Expression -> Phantoms.TTerm Ast.ApplicationExpression
applicationExpression function argument =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.ast.ApplicationExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "function"),
          Core.fieldTerm = (Phantoms.unTTerm function)},
        Core.Field {
          Core.fieldName = (Core.Name "argument"),
          Core.fieldTerm = (Phantoms.unTTerm argument)}]}))

applicationExpressionFunction :: Phantoms.TTerm Ast.ApplicationExpression -> Phantoms.TTerm Ast.Expression
applicationExpressionFunction x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.ApplicationExpression"),
        Core.projectionField = (Core.Name "function")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

applicationExpressionArgument :: Phantoms.TTerm Ast.ApplicationExpression -> Phantoms.TTerm Ast.Expression
applicationExpressionArgument x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.ApplicationExpression"),
        Core.projectionField = (Core.Name "argument")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

applicationExpressionWithFunction :: Phantoms.TTerm Ast.ApplicationExpression -> Phantoms.TTerm Ast.Expression -> Phantoms.TTerm Ast.ApplicationExpression
applicationExpressionWithFunction original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.ast.ApplicationExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "function"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "argument"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.ApplicationExpression"),
              Core.projectionField = (Core.Name "argument")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

applicationExpressionWithArgument :: Phantoms.TTerm Ast.ApplicationExpression -> Phantoms.TTerm Ast.Expression -> Phantoms.TTerm Ast.ApplicationExpression
applicationExpressionWithArgument original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.ast.ApplicationExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "function"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.ApplicationExpression"),
              Core.projectionField = (Core.Name "function")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "argument"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

caseExpression :: Phantoms.TTerm Ast.Expression -> Phantoms.TTerm [Ast.Alternative] -> Phantoms.TTerm Ast.CaseExpression
caseExpression case_ alternatives =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.ast.CaseExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "case"),
          Core.fieldTerm = (Phantoms.unTTerm case_)},
        Core.Field {
          Core.fieldName = (Core.Name "alternatives"),
          Core.fieldTerm = (Phantoms.unTTerm alternatives)}]}))

caseExpressionCase :: Phantoms.TTerm Ast.CaseExpression -> Phantoms.TTerm Ast.Expression
caseExpressionCase x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.CaseExpression"),
        Core.projectionField = (Core.Name "case")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

caseExpressionAlternatives :: Phantoms.TTerm Ast.CaseExpression -> Phantoms.TTerm [Ast.Alternative]
caseExpressionAlternatives x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.CaseExpression"),
        Core.projectionField = (Core.Name "alternatives")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

caseExpressionWithCase :: Phantoms.TTerm Ast.CaseExpression -> Phantoms.TTerm Ast.Expression -> Phantoms.TTerm Ast.CaseExpression
caseExpressionWithCase original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.ast.CaseExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "case"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "alternatives"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.CaseExpression"),
              Core.projectionField = (Core.Name "alternatives")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

caseExpressionWithAlternatives :: Phantoms.TTerm Ast.CaseExpression -> Phantoms.TTerm [Ast.Alternative] -> Phantoms.TTerm Ast.CaseExpression
caseExpressionWithAlternatives original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.ast.CaseExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "case"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.CaseExpression"),
              Core.projectionField = (Core.Name "case")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "alternatives"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

constructRecordExpression :: Phantoms.TTerm Ast.Name -> Phantoms.TTerm [Ast.FieldUpdate] -> Phantoms.TTerm Ast.ConstructRecordExpression
constructRecordExpression name fields =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.ast.ConstructRecordExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "fields"),
          Core.fieldTerm = (Phantoms.unTTerm fields)}]}))

constructRecordExpressionName :: Phantoms.TTerm Ast.ConstructRecordExpression -> Phantoms.TTerm Ast.Name
constructRecordExpressionName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.ConstructRecordExpression"),
        Core.projectionField = (Core.Name "name")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

constructRecordExpressionFields :: Phantoms.TTerm Ast.ConstructRecordExpression -> Phantoms.TTerm [Ast.FieldUpdate]
constructRecordExpressionFields x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.ConstructRecordExpression"),
        Core.projectionField = (Core.Name "fields")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

constructRecordExpressionWithName :: Phantoms.TTerm Ast.ConstructRecordExpression -> Phantoms.TTerm Ast.Name -> Phantoms.TTerm Ast.ConstructRecordExpression
constructRecordExpressionWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.ast.ConstructRecordExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "fields"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.ConstructRecordExpression"),
              Core.projectionField = (Core.Name "fields")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

constructRecordExpressionWithFields :: Phantoms.TTerm Ast.ConstructRecordExpression -> Phantoms.TTerm [Ast.FieldUpdate] -> Phantoms.TTerm Ast.ConstructRecordExpression
constructRecordExpressionWithFields original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.ast.ConstructRecordExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.ConstructRecordExpression"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "fields"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

ifExpression :: Phantoms.TTerm Ast.Expression -> Phantoms.TTerm Ast.Expression -> Phantoms.TTerm Ast.Expression -> Phantoms.TTerm Ast.IfExpression
ifExpression condition then_ else_ =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.ast.IfExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "condition"),
          Core.fieldTerm = (Phantoms.unTTerm condition)},
        Core.Field {
          Core.fieldName = (Core.Name "then"),
          Core.fieldTerm = (Phantoms.unTTerm then_)},
        Core.Field {
          Core.fieldName = (Core.Name "else"),
          Core.fieldTerm = (Phantoms.unTTerm else_)}]}))

ifExpressionCondition :: Phantoms.TTerm Ast.IfExpression -> Phantoms.TTerm Ast.Expression
ifExpressionCondition x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.IfExpression"),
        Core.projectionField = (Core.Name "condition")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

ifExpressionThen :: Phantoms.TTerm Ast.IfExpression -> Phantoms.TTerm Ast.Expression
ifExpressionThen x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.IfExpression"),
        Core.projectionField = (Core.Name "then")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

ifExpressionElse :: Phantoms.TTerm Ast.IfExpression -> Phantoms.TTerm Ast.Expression
ifExpressionElse x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.IfExpression"),
        Core.projectionField = (Core.Name "else")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

ifExpressionWithCondition :: Phantoms.TTerm Ast.IfExpression -> Phantoms.TTerm Ast.Expression -> Phantoms.TTerm Ast.IfExpression
ifExpressionWithCondition original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.ast.IfExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "condition"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "then"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.IfExpression"),
              Core.projectionField = (Core.Name "then")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "else"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.IfExpression"),
              Core.projectionField = (Core.Name "else")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

ifExpressionWithThen :: Phantoms.TTerm Ast.IfExpression -> Phantoms.TTerm Ast.Expression -> Phantoms.TTerm Ast.IfExpression
ifExpressionWithThen original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.ast.IfExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "condition"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.IfExpression"),
              Core.projectionField = (Core.Name "condition")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "then"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "else"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.IfExpression"),
              Core.projectionField = (Core.Name "else")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

ifExpressionWithElse :: Phantoms.TTerm Ast.IfExpression -> Phantoms.TTerm Ast.Expression -> Phantoms.TTerm Ast.IfExpression
ifExpressionWithElse original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.ast.IfExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "condition"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.IfExpression"),
              Core.projectionField = (Core.Name "condition")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "then"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.IfExpression"),
              Core.projectionField = (Core.Name "then")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "else"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

infixApplicationExpression :: Phantoms.TTerm Ast.Expression -> Phantoms.TTerm Ast.Operator -> Phantoms.TTerm Ast.Expression -> Phantoms.TTerm Ast.InfixApplicationExpression
infixApplicationExpression lhs operator rhs =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.ast.InfixApplicationExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Phantoms.unTTerm lhs)},
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Phantoms.unTTerm operator)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTTerm rhs)}]}))

infixApplicationExpressionLhs :: Phantoms.TTerm Ast.InfixApplicationExpression -> Phantoms.TTerm Ast.Expression
infixApplicationExpressionLhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.InfixApplicationExpression"),
        Core.projectionField = (Core.Name "lhs")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

infixApplicationExpressionOperator :: Phantoms.TTerm Ast.InfixApplicationExpression -> Phantoms.TTerm Ast.Operator
infixApplicationExpressionOperator x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.InfixApplicationExpression"),
        Core.projectionField = (Core.Name "operator")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

infixApplicationExpressionRhs :: Phantoms.TTerm Ast.InfixApplicationExpression -> Phantoms.TTerm Ast.Expression
infixApplicationExpressionRhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.InfixApplicationExpression"),
        Core.projectionField = (Core.Name "rhs")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

infixApplicationExpressionWithLhs :: Phantoms.TTerm Ast.InfixApplicationExpression -> Phantoms.TTerm Ast.Expression -> Phantoms.TTerm Ast.InfixApplicationExpression
infixApplicationExpressionWithLhs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.ast.InfixApplicationExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.InfixApplicationExpression"),
              Core.projectionField = (Core.Name "operator")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.InfixApplicationExpression"),
              Core.projectionField = (Core.Name "rhs")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

infixApplicationExpressionWithOperator :: Phantoms.TTerm Ast.InfixApplicationExpression -> Phantoms.TTerm Ast.Operator -> Phantoms.TTerm Ast.InfixApplicationExpression
infixApplicationExpressionWithOperator original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.ast.InfixApplicationExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.InfixApplicationExpression"),
              Core.projectionField = (Core.Name "lhs")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.InfixApplicationExpression"),
              Core.projectionField = (Core.Name "rhs")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

infixApplicationExpressionWithRhs :: Phantoms.TTerm Ast.InfixApplicationExpression -> Phantoms.TTerm Ast.Expression -> Phantoms.TTerm Ast.InfixApplicationExpression
infixApplicationExpressionWithRhs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.ast.InfixApplicationExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.InfixApplicationExpression"),
              Core.projectionField = (Core.Name "lhs")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.InfixApplicationExpression"),
              Core.projectionField = (Core.Name "operator")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

lambdaExpression :: Phantoms.TTerm [Ast.Pattern] -> Phantoms.TTerm Ast.Expression -> Phantoms.TTerm Ast.LambdaExpression
lambdaExpression bindings inner =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.ast.LambdaExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "bindings"),
          Core.fieldTerm = (Phantoms.unTTerm bindings)},
        Core.Field {
          Core.fieldName = (Core.Name "inner"),
          Core.fieldTerm = (Phantoms.unTTerm inner)}]}))

lambdaExpressionBindings :: Phantoms.TTerm Ast.LambdaExpression -> Phantoms.TTerm [Ast.Pattern]
lambdaExpressionBindings x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.LambdaExpression"),
        Core.projectionField = (Core.Name "bindings")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

lambdaExpressionInner :: Phantoms.TTerm Ast.LambdaExpression -> Phantoms.TTerm Ast.Expression
lambdaExpressionInner x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.LambdaExpression"),
        Core.projectionField = (Core.Name "inner")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

lambdaExpressionWithBindings :: Phantoms.TTerm Ast.LambdaExpression -> Phantoms.TTerm [Ast.Pattern] -> Phantoms.TTerm Ast.LambdaExpression
lambdaExpressionWithBindings original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.ast.LambdaExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "bindings"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "inner"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.LambdaExpression"),
              Core.projectionField = (Core.Name "inner")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

lambdaExpressionWithInner :: Phantoms.TTerm Ast.LambdaExpression -> Phantoms.TTerm Ast.Expression -> Phantoms.TTerm Ast.LambdaExpression
lambdaExpressionWithInner original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.ast.LambdaExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "bindings"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.LambdaExpression"),
              Core.projectionField = (Core.Name "bindings")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inner"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

letExpression :: Phantoms.TTerm [Ast.LocalBinding] -> Phantoms.TTerm Ast.Expression -> Phantoms.TTerm Ast.LetExpression
letExpression bindings inner =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.ast.LetExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "bindings"),
          Core.fieldTerm = (Phantoms.unTTerm bindings)},
        Core.Field {
          Core.fieldName = (Core.Name "inner"),
          Core.fieldTerm = (Phantoms.unTTerm inner)}]}))

letExpressionBindings :: Phantoms.TTerm Ast.LetExpression -> Phantoms.TTerm [Ast.LocalBinding]
letExpressionBindings x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.LetExpression"),
        Core.projectionField = (Core.Name "bindings")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

letExpressionInner :: Phantoms.TTerm Ast.LetExpression -> Phantoms.TTerm Ast.Expression
letExpressionInner x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.LetExpression"),
        Core.projectionField = (Core.Name "inner")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

letExpressionWithBindings :: Phantoms.TTerm Ast.LetExpression -> Phantoms.TTerm [Ast.LocalBinding] -> Phantoms.TTerm Ast.LetExpression
letExpressionWithBindings original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.ast.LetExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "bindings"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "inner"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.LetExpression"),
              Core.projectionField = (Core.Name "inner")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

letExpressionWithInner :: Phantoms.TTerm Ast.LetExpression -> Phantoms.TTerm Ast.Expression -> Phantoms.TTerm Ast.LetExpression
letExpressionWithInner original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.ast.LetExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "bindings"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.LetExpression"),
              Core.projectionField = (Core.Name "bindings")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inner"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

prefixApplicationExpression :: Phantoms.TTerm Ast.Operator -> Phantoms.TTerm Ast.Expression -> Phantoms.TTerm Ast.PrefixApplicationExpression
prefixApplicationExpression operator rhs =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.ast.PrefixApplicationExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Phantoms.unTTerm operator)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTTerm rhs)}]}))

prefixApplicationExpressionOperator :: Phantoms.TTerm Ast.PrefixApplicationExpression -> Phantoms.TTerm Ast.Operator
prefixApplicationExpressionOperator x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.PrefixApplicationExpression"),
        Core.projectionField = (Core.Name "operator")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

prefixApplicationExpressionRhs :: Phantoms.TTerm Ast.PrefixApplicationExpression -> Phantoms.TTerm Ast.Expression
prefixApplicationExpressionRhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.PrefixApplicationExpression"),
        Core.projectionField = (Core.Name "rhs")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

prefixApplicationExpressionWithOperator :: Phantoms.TTerm Ast.PrefixApplicationExpression -> Phantoms.TTerm Ast.Operator -> Phantoms.TTerm Ast.PrefixApplicationExpression
prefixApplicationExpressionWithOperator original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.ast.PrefixApplicationExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.PrefixApplicationExpression"),
              Core.projectionField = (Core.Name "rhs")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

prefixApplicationExpressionWithRhs :: Phantoms.TTerm Ast.PrefixApplicationExpression -> Phantoms.TTerm Ast.Expression -> Phantoms.TTerm Ast.PrefixApplicationExpression
prefixApplicationExpressionWithRhs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.ast.PrefixApplicationExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.PrefixApplicationExpression"),
              Core.projectionField = (Core.Name "operator")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

sectionExpression :: Phantoms.TTerm Ast.Operator -> Phantoms.TTerm Ast.Expression -> Phantoms.TTerm Ast.SectionExpression
sectionExpression operator expression =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.ast.SectionExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Phantoms.unTTerm operator)},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTTerm expression)}]}))

sectionExpressionOperator :: Phantoms.TTerm Ast.SectionExpression -> Phantoms.TTerm Ast.Operator
sectionExpressionOperator x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.SectionExpression"),
        Core.projectionField = (Core.Name "operator")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

sectionExpressionExpression :: Phantoms.TTerm Ast.SectionExpression -> Phantoms.TTerm Ast.Expression
sectionExpressionExpression x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.SectionExpression"),
        Core.projectionField = (Core.Name "expression")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

sectionExpressionWithOperator :: Phantoms.TTerm Ast.SectionExpression -> Phantoms.TTerm Ast.Operator -> Phantoms.TTerm Ast.SectionExpression
sectionExpressionWithOperator original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.ast.SectionExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.SectionExpression"),
              Core.projectionField = (Core.Name "expression")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

sectionExpressionWithExpression :: Phantoms.TTerm Ast.SectionExpression -> Phantoms.TTerm Ast.Expression -> Phantoms.TTerm Ast.SectionExpression
sectionExpressionWithExpression original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.ast.SectionExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.SectionExpression"),
              Core.projectionField = (Core.Name "operator")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

typeSignatureExpression :: Phantoms.TTerm Ast.Expression -> Phantoms.TTerm Ast.Type -> Phantoms.TTerm Ast.TypeSignatureExpression
typeSignatureExpression inner type_ =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.ast.TypeSignatureExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "inner"),
          Core.fieldTerm = (Phantoms.unTTerm inner)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm type_)}]}))

typeSignatureExpressionInner :: Phantoms.TTerm Ast.TypeSignatureExpression -> Phantoms.TTerm Ast.Expression
typeSignatureExpressionInner x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.TypeSignatureExpression"),
        Core.projectionField = (Core.Name "inner")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

typeSignatureExpressionType :: Phantoms.TTerm Ast.TypeSignatureExpression -> Phantoms.TTerm Ast.Type
typeSignatureExpressionType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.TypeSignatureExpression"),
        Core.projectionField = (Core.Name "type")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

typeSignatureExpressionWithInner :: Phantoms.TTerm Ast.TypeSignatureExpression -> Phantoms.TTerm Ast.Expression -> Phantoms.TTerm Ast.TypeSignatureExpression
typeSignatureExpressionWithInner original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.ast.TypeSignatureExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "inner"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.TypeSignatureExpression"),
              Core.projectionField = (Core.Name "type")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

typeSignatureExpressionWithType :: Phantoms.TTerm Ast.TypeSignatureExpression -> Phantoms.TTerm Ast.Type -> Phantoms.TTerm Ast.TypeSignatureExpression
typeSignatureExpressionWithType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.ast.TypeSignatureExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "inner"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.TypeSignatureExpression"),
              Core.projectionField = (Core.Name "inner")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

updateRecordExpression :: Phantoms.TTerm Ast.Expression -> Phantoms.TTerm [Ast.FieldUpdate] -> Phantoms.TTerm Ast.UpdateRecordExpression
updateRecordExpression inner fields =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.ast.UpdateRecordExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "inner"),
          Core.fieldTerm = (Phantoms.unTTerm inner)},
        Core.Field {
          Core.fieldName = (Core.Name "fields"),
          Core.fieldTerm = (Phantoms.unTTerm fields)}]}))

updateRecordExpressionInner :: Phantoms.TTerm Ast.UpdateRecordExpression -> Phantoms.TTerm Ast.Expression
updateRecordExpressionInner x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.UpdateRecordExpression"),
        Core.projectionField = (Core.Name "inner")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

updateRecordExpressionFields :: Phantoms.TTerm Ast.UpdateRecordExpression -> Phantoms.TTerm [Ast.FieldUpdate]
updateRecordExpressionFields x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.UpdateRecordExpression"),
        Core.projectionField = (Core.Name "fields")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

updateRecordExpressionWithInner :: Phantoms.TTerm Ast.UpdateRecordExpression -> Phantoms.TTerm Ast.Expression -> Phantoms.TTerm Ast.UpdateRecordExpression
updateRecordExpressionWithInner original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.ast.UpdateRecordExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "inner"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "fields"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.UpdateRecordExpression"),
              Core.projectionField = (Core.Name "fields")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

updateRecordExpressionWithFields :: Phantoms.TTerm Ast.UpdateRecordExpression -> Phantoms.TTerm [Ast.FieldUpdate] -> Phantoms.TTerm Ast.UpdateRecordExpression
updateRecordExpressionWithFields original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.ast.UpdateRecordExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "inner"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.UpdateRecordExpression"),
              Core.projectionField = (Core.Name "inner")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "fields"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

field :: Phantoms.TTerm Ast.Name -> Phantoms.TTerm Ast.Type -> Phantoms.TTerm Ast.Field
field name type_ =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.ast.Field"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm type_)}]}))

fieldName :: Phantoms.TTerm Ast.Field -> Phantoms.TTerm Ast.Name
fieldName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.Field"),
        Core.projectionField = (Core.Name "name")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

fieldType :: Phantoms.TTerm Ast.Field -> Phantoms.TTerm Ast.Type
fieldType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.Field"),
        Core.projectionField = (Core.Name "type")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

fieldWithName :: Phantoms.TTerm Ast.Field -> Phantoms.TTerm Ast.Name -> Phantoms.TTerm Ast.Field
fieldWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.ast.Field"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.Field"),
              Core.projectionField = (Core.Name "type")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

fieldWithType :: Phantoms.TTerm Ast.Field -> Phantoms.TTerm Ast.Type -> Phantoms.TTerm Ast.Field
fieldWithType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.ast.Field"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.Field"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

fieldWithComments :: Phantoms.TTerm Ast.Field -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Ast.FieldWithComments
fieldWithComments field comments =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.ast.FieldWithComments"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "field"),
          Core.fieldTerm = (Phantoms.unTTerm field)},
        Core.Field {
          Core.fieldName = (Core.Name "comments"),
          Core.fieldTerm = (Phantoms.unTTerm comments)}]}))

fieldWithCommentsField :: Phantoms.TTerm Ast.FieldWithComments -> Phantoms.TTerm Ast.Field
fieldWithCommentsField x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.FieldWithComments"),
        Core.projectionField = (Core.Name "field")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

fieldWithCommentsComments :: Phantoms.TTerm Ast.FieldWithComments -> Phantoms.TTerm (Maybe String)
fieldWithCommentsComments x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.FieldWithComments"),
        Core.projectionField = (Core.Name "comments")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

fieldWithCommentsWithField :: Phantoms.TTerm Ast.FieldWithComments -> Phantoms.TTerm Ast.Field -> Phantoms.TTerm Ast.FieldWithComments
fieldWithCommentsWithField original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.ast.FieldWithComments"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "field"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "comments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.FieldWithComments"),
              Core.projectionField = (Core.Name "comments")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

fieldWithCommentsWithComments :: Phantoms.TTerm Ast.FieldWithComments -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Ast.FieldWithComments
fieldWithCommentsWithComments original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.ast.FieldWithComments"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "field"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.FieldWithComments"),
              Core.projectionField = (Core.Name "field")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "comments"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

fieldUpdate :: Phantoms.TTerm Ast.Name -> Phantoms.TTerm Ast.Expression -> Phantoms.TTerm Ast.FieldUpdate
fieldUpdate name value =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.ast.FieldUpdate"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm value)}]}))

fieldUpdateName :: Phantoms.TTerm Ast.FieldUpdate -> Phantoms.TTerm Ast.Name
fieldUpdateName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.FieldUpdate"),
        Core.projectionField = (Core.Name "name")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

fieldUpdateValue :: Phantoms.TTerm Ast.FieldUpdate -> Phantoms.TTerm Ast.Expression
fieldUpdateValue x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.FieldUpdate"),
        Core.projectionField = (Core.Name "value")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

fieldUpdateWithName :: Phantoms.TTerm Ast.FieldUpdate -> Phantoms.TTerm Ast.Name -> Phantoms.TTerm Ast.FieldUpdate
fieldUpdateWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.ast.FieldUpdate"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.FieldUpdate"),
              Core.projectionField = (Core.Name "value")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

fieldUpdateWithValue :: Phantoms.TTerm Ast.FieldUpdate -> Phantoms.TTerm Ast.Expression -> Phantoms.TTerm Ast.FieldUpdate
fieldUpdateWithValue original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.ast.FieldUpdate"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.FieldUpdate"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

import_ :: Phantoms.TTerm Bool -> Phantoms.TTerm Ast.ModuleName -> Phantoms.TTerm (Maybe Ast.ModuleName) -> Phantoms.TTerm (Maybe Ast.SpecImport) -> Phantoms.TTerm Ast.Import
import_ qualified module_ as spec =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.ast.Import"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "qualified"),
          Core.fieldTerm = (Phantoms.unTTerm qualified)},
        Core.Field {
          Core.fieldName = (Core.Name "module"),
          Core.fieldTerm = (Phantoms.unTTerm module_)},
        Core.Field {
          Core.fieldName = (Core.Name "as"),
          Core.fieldTerm = (Phantoms.unTTerm as)},
        Core.Field {
          Core.fieldName = (Core.Name "spec"),
          Core.fieldTerm = (Phantoms.unTTerm spec)}]}))

importQualified :: Phantoms.TTerm Ast.Import -> Phantoms.TTerm Bool
importQualified x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.Import"),
        Core.projectionField = (Core.Name "qualified")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

importModule :: Phantoms.TTerm Ast.Import -> Phantoms.TTerm Ast.ModuleName
importModule x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.Import"),
        Core.projectionField = (Core.Name "module")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

importAs :: Phantoms.TTerm Ast.Import -> Phantoms.TTerm (Maybe Ast.ModuleName)
importAs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.Import"),
        Core.projectionField = (Core.Name "as")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

importSpec :: Phantoms.TTerm Ast.Import -> Phantoms.TTerm (Maybe Ast.SpecImport)
importSpec x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.Import"),
        Core.projectionField = (Core.Name "spec")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

importWithQualified :: Phantoms.TTerm Ast.Import -> Phantoms.TTerm Bool -> Phantoms.TTerm Ast.Import
importWithQualified original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.ast.Import"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "qualified"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "module"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.Import"),
              Core.projectionField = (Core.Name "module")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "as"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.Import"),
              Core.projectionField = (Core.Name "as")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "spec"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.Import"),
              Core.projectionField = (Core.Name "spec")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

importWithModule :: Phantoms.TTerm Ast.Import -> Phantoms.TTerm Ast.ModuleName -> Phantoms.TTerm Ast.Import
importWithModule original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.ast.Import"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "qualified"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.Import"),
              Core.projectionField = (Core.Name "qualified")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "module"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "as"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.Import"),
              Core.projectionField = (Core.Name "as")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "spec"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.Import"),
              Core.projectionField = (Core.Name "spec")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

importWithAs :: Phantoms.TTerm Ast.Import -> Phantoms.TTerm (Maybe Ast.ModuleName) -> Phantoms.TTerm Ast.Import
importWithAs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.ast.Import"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "qualified"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.Import"),
              Core.projectionField = (Core.Name "qualified")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "module"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.Import"),
              Core.projectionField = (Core.Name "module")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "as"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "spec"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.Import"),
              Core.projectionField = (Core.Name "spec")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

importWithSpec :: Phantoms.TTerm Ast.Import -> Phantoms.TTerm (Maybe Ast.SpecImport) -> Phantoms.TTerm Ast.Import
importWithSpec original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.ast.Import"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "qualified"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.Import"),
              Core.projectionField = (Core.Name "qualified")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "module"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.Import"),
              Core.projectionField = (Core.Name "module")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "as"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.Import"),
              Core.projectionField = (Core.Name "as")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "spec"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

specImportList :: Phantoms.TTerm [Ast.ImportExportSpec] -> Phantoms.TTerm Ast.SpecImport
specImportList x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.haskell.ast.SpecImport"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "list"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

specImportHiding :: Phantoms.TTerm [Ast.ImportExportSpec] -> Phantoms.TTerm Ast.SpecImport
specImportHiding x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.haskell.ast.SpecImport"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "hiding"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

importModifierPattern :: Phantoms.TTerm Ast.ImportModifier
importModifierPattern =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.haskell.ast.ImportModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "pattern"),
        Core.fieldTerm = Core.TermUnit}}))

importModifierType :: Phantoms.TTerm Ast.ImportModifier
importModifierType =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.haskell.ast.ImportModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "type"),
        Core.fieldTerm = Core.TermUnit}}))

importExportSpec :: Phantoms.TTerm (Maybe Ast.ImportModifier) -> Phantoms.TTerm Ast.Name -> Phantoms.TTerm (Maybe Ast.SubspecImportExportSpec) -> Phantoms.TTerm Ast.ImportExportSpec
importExportSpec modifier name subspec =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.ast.ImportExportSpec"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifier"),
          Core.fieldTerm = (Phantoms.unTTerm modifier)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "subspec"),
          Core.fieldTerm = (Phantoms.unTTerm subspec)}]}))

importExportSpecModifier :: Phantoms.TTerm Ast.ImportExportSpec -> Phantoms.TTerm (Maybe Ast.ImportModifier)
importExportSpecModifier x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.ImportExportSpec"),
        Core.projectionField = (Core.Name "modifier")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

importExportSpecName :: Phantoms.TTerm Ast.ImportExportSpec -> Phantoms.TTerm Ast.Name
importExportSpecName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.ImportExportSpec"),
        Core.projectionField = (Core.Name "name")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

importExportSpecSubspec :: Phantoms.TTerm Ast.ImportExportSpec -> Phantoms.TTerm (Maybe Ast.SubspecImportExportSpec)
importExportSpecSubspec x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.ImportExportSpec"),
        Core.projectionField = (Core.Name "subspec")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

importExportSpecWithModifier :: Phantoms.TTerm Ast.ImportExportSpec -> Phantoms.TTerm (Maybe Ast.ImportModifier) -> Phantoms.TTerm Ast.ImportExportSpec
importExportSpecWithModifier original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.ast.ImportExportSpec"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifier"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.ImportExportSpec"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "subspec"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.ImportExportSpec"),
              Core.projectionField = (Core.Name "subspec")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

importExportSpecWithName :: Phantoms.TTerm Ast.ImportExportSpec -> Phantoms.TTerm Ast.Name -> Phantoms.TTerm Ast.ImportExportSpec
importExportSpecWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.ast.ImportExportSpec"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.ImportExportSpec"),
              Core.projectionField = (Core.Name "modifier")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "subspec"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.ImportExportSpec"),
              Core.projectionField = (Core.Name "subspec")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

importExportSpecWithSubspec :: Phantoms.TTerm Ast.ImportExportSpec -> Phantoms.TTerm (Maybe Ast.SubspecImportExportSpec) -> Phantoms.TTerm Ast.ImportExportSpec
importExportSpecWithSubspec original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.ast.ImportExportSpec"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.ImportExportSpec"),
              Core.projectionField = (Core.Name "modifier")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.ImportExportSpec"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "subspec"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

subspecImportExportSpecAll :: Phantoms.TTerm Ast.SubspecImportExportSpec
subspecImportExportSpecAll =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.haskell.ast.SubspecImportExportSpec"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "all"),
        Core.fieldTerm = Core.TermUnit}}))

subspecImportExportSpecList :: Phantoms.TTerm [Ast.Name] -> Phantoms.TTerm Ast.SubspecImportExportSpec
subspecImportExportSpecList x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.haskell.ast.SubspecImportExportSpec"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "list"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

literalChar :: Phantoms.TTerm Int -> Phantoms.TTerm Ast.Literal
literalChar x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.haskell.ast.Literal"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "char"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

literalDouble :: Phantoms.TTerm Double -> Phantoms.TTerm Ast.Literal
literalDouble x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.haskell.ast.Literal"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "double"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

literalFloat :: Phantoms.TTerm Float -> Phantoms.TTerm Ast.Literal
literalFloat x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.haskell.ast.Literal"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "float"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

literalInt :: Phantoms.TTerm Int -> Phantoms.TTerm Ast.Literal
literalInt x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.haskell.ast.Literal"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "int"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

literalInteger :: Phantoms.TTerm Integer -> Phantoms.TTerm Ast.Literal
literalInteger x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.haskell.ast.Literal"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "integer"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

literalString :: Phantoms.TTerm String -> Phantoms.TTerm Ast.Literal
literalString x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.haskell.ast.Literal"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "string"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

localBindingSignature :: Phantoms.TTerm Ast.TypeSignature -> Phantoms.TTerm Ast.LocalBinding
localBindingSignature x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.haskell.ast.LocalBinding"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "signature"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

localBindingValue :: Phantoms.TTerm Ast.ValueBinding -> Phantoms.TTerm Ast.LocalBinding
localBindingValue x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.haskell.ast.LocalBinding"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "value"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

localBindings :: Phantoms.TTerm [Ast.LocalBinding] -> Phantoms.TTerm Ast.LocalBindings
localBindings x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.haskell.ast.LocalBindings"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unLocalBindings :: Phantoms.TTerm Ast.LocalBindings -> Phantoms.TTerm [Ast.LocalBinding]
unLocalBindings x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.haskell.ast.LocalBindings")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

module_ :: Phantoms.TTerm (Maybe Ast.ModuleHead) -> Phantoms.TTerm [Ast.Import] -> Phantoms.TTerm [Ast.DeclarationWithComments] -> Phantoms.TTerm Ast.Module
module_ head imports declarations =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.ast.Module"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "head"),
          Core.fieldTerm = (Phantoms.unTTerm head)},
        Core.Field {
          Core.fieldName = (Core.Name "imports"),
          Core.fieldTerm = (Phantoms.unTTerm imports)},
        Core.Field {
          Core.fieldName = (Core.Name "declarations"),
          Core.fieldTerm = (Phantoms.unTTerm declarations)}]}))

moduleHead :: Phantoms.TTerm Ast.Module -> Phantoms.TTerm (Maybe Ast.ModuleHead)
moduleHead x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.Module"),
        Core.projectionField = (Core.Name "head")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

moduleImports :: Phantoms.TTerm Ast.Module -> Phantoms.TTerm [Ast.Import]
moduleImports x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.Module"),
        Core.projectionField = (Core.Name "imports")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

moduleDeclarations :: Phantoms.TTerm Ast.Module -> Phantoms.TTerm [Ast.DeclarationWithComments]
moduleDeclarations x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.Module"),
        Core.projectionField = (Core.Name "declarations")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

moduleWithHead :: Phantoms.TTerm Ast.Module -> Phantoms.TTerm (Maybe Ast.ModuleHead) -> Phantoms.TTerm Ast.Module
moduleWithHead original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.ast.Module"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "head"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "imports"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.Module"),
              Core.projectionField = (Core.Name "imports")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "declarations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.Module"),
              Core.projectionField = (Core.Name "declarations")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

moduleWithImports :: Phantoms.TTerm Ast.Module -> Phantoms.TTerm [Ast.Import] -> Phantoms.TTerm Ast.Module
moduleWithImports original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.ast.Module"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "head"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.Module"),
              Core.projectionField = (Core.Name "head")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "imports"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "declarations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.Module"),
              Core.projectionField = (Core.Name "declarations")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

moduleWithDeclarations :: Phantoms.TTerm Ast.Module -> Phantoms.TTerm [Ast.DeclarationWithComments] -> Phantoms.TTerm Ast.Module
moduleWithDeclarations original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.ast.Module"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "head"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.Module"),
              Core.projectionField = (Core.Name "head")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "imports"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.Module"),
              Core.projectionField = (Core.Name "imports")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "declarations"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

moduleHead_ :: Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Ast.ModuleName -> Phantoms.TTerm [Ast.Export] -> Phantoms.TTerm Ast.ModuleHead
moduleHead_ comments name exports =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.ast.ModuleHead"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "comments"),
          Core.fieldTerm = (Phantoms.unTTerm comments)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "exports"),
          Core.fieldTerm = (Phantoms.unTTerm exports)}]}))

moduleHeadComments :: Phantoms.TTerm Ast.ModuleHead -> Phantoms.TTerm (Maybe String)
moduleHeadComments x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.ModuleHead"),
        Core.projectionField = (Core.Name "comments")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

moduleHeadName :: Phantoms.TTerm Ast.ModuleHead -> Phantoms.TTerm Ast.ModuleName
moduleHeadName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.ModuleHead"),
        Core.projectionField = (Core.Name "name")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

moduleHeadExports :: Phantoms.TTerm Ast.ModuleHead -> Phantoms.TTerm [Ast.Export]
moduleHeadExports x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.ModuleHead"),
        Core.projectionField = (Core.Name "exports")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

moduleHeadWithComments :: Phantoms.TTerm Ast.ModuleHead -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Ast.ModuleHead
moduleHeadWithComments original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.ast.ModuleHead"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "comments"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.ModuleHead"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "exports"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.ModuleHead"),
              Core.projectionField = (Core.Name "exports")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

moduleHeadWithName :: Phantoms.TTerm Ast.ModuleHead -> Phantoms.TTerm Ast.ModuleName -> Phantoms.TTerm Ast.ModuleHead
moduleHeadWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.ast.ModuleHead"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "comments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.ModuleHead"),
              Core.projectionField = (Core.Name "comments")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "exports"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.ModuleHead"),
              Core.projectionField = (Core.Name "exports")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

moduleHeadWithExports :: Phantoms.TTerm Ast.ModuleHead -> Phantoms.TTerm [Ast.Export] -> Phantoms.TTerm Ast.ModuleHead
moduleHeadWithExports original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.ast.ModuleHead"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "comments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.ModuleHead"),
              Core.projectionField = (Core.Name "comments")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.ModuleHead"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "exports"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

moduleName :: Phantoms.TTerm String -> Phantoms.TTerm Ast.ModuleName
moduleName x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.haskell.ast.ModuleName"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unModuleName :: Phantoms.TTerm Ast.ModuleName -> Phantoms.TTerm String
unModuleName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.haskell.ast.ModuleName")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

nameImplicit :: Phantoms.TTerm Ast.QualifiedName -> Phantoms.TTerm Ast.Name
nameImplicit x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.haskell.ast.Name"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "implicit"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

nameNormal :: Phantoms.TTerm Ast.QualifiedName -> Phantoms.TTerm Ast.Name
nameNormal x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.haskell.ast.Name"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "normal"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

nameParens :: Phantoms.TTerm Ast.QualifiedName -> Phantoms.TTerm Ast.Name
nameParens x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.haskell.ast.Name"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "parens"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

namePart :: Phantoms.TTerm String -> Phantoms.TTerm Ast.NamePart
namePart x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.haskell.ast.NamePart"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unNamePart :: Phantoms.TTerm Ast.NamePart -> Phantoms.TTerm String
unNamePart x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.haskell.ast.NamePart")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

operatorBacktick :: Phantoms.TTerm Ast.QualifiedName -> Phantoms.TTerm Ast.Operator
operatorBacktick x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.haskell.ast.Operator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "backtick"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

operatorNormal :: Phantoms.TTerm Ast.QualifiedName -> Phantoms.TTerm Ast.Operator
operatorNormal x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.haskell.ast.Operator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "normal"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

patternApplication :: Phantoms.TTerm Ast.ApplicationPattern -> Phantoms.TTerm Ast.Pattern
patternApplication x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.haskell.ast.Pattern"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "application"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

patternAs :: Phantoms.TTerm Ast.AsPattern -> Phantoms.TTerm Ast.Pattern
patternAs x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.haskell.ast.Pattern"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "as"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

patternList :: Phantoms.TTerm [Ast.Pattern] -> Phantoms.TTerm Ast.Pattern
patternList x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.haskell.ast.Pattern"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "list"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

patternLiteral :: Phantoms.TTerm Ast.Literal -> Phantoms.TTerm Ast.Pattern
patternLiteral x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.haskell.ast.Pattern"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "literal"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

patternName :: Phantoms.TTerm Ast.Name -> Phantoms.TTerm Ast.Pattern
patternName x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.haskell.ast.Pattern"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "name"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

patternParens :: Phantoms.TTerm Ast.Pattern -> Phantoms.TTerm Ast.Pattern
patternParens x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.haskell.ast.Pattern"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "parens"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

patternRecord :: Phantoms.TTerm Ast.RecordPattern -> Phantoms.TTerm Ast.Pattern
patternRecord x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.haskell.ast.Pattern"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "record"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

patternTuple :: Phantoms.TTerm [Ast.Pattern] -> Phantoms.TTerm Ast.Pattern
patternTuple x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.haskell.ast.Pattern"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "tuple"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

patternTyped :: Phantoms.TTerm Ast.TypedPattern -> Phantoms.TTerm Ast.Pattern
patternTyped x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.haskell.ast.Pattern"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "typed"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

patternWildcard :: Phantoms.TTerm Ast.Pattern
patternWildcard =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.haskell.ast.Pattern"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "wildcard"),
        Core.fieldTerm = Core.TermUnit}}))

applicationPattern :: Phantoms.TTerm Ast.Name -> Phantoms.TTerm [Ast.Pattern] -> Phantoms.TTerm Ast.ApplicationPattern
applicationPattern name args =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.ast.ApplicationPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "args"),
          Core.fieldTerm = (Phantoms.unTTerm args)}]}))

applicationPatternName :: Phantoms.TTerm Ast.ApplicationPattern -> Phantoms.TTerm Ast.Name
applicationPatternName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.ApplicationPattern"),
        Core.projectionField = (Core.Name "name")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

applicationPatternArgs :: Phantoms.TTerm Ast.ApplicationPattern -> Phantoms.TTerm [Ast.Pattern]
applicationPatternArgs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.ApplicationPattern"),
        Core.projectionField = (Core.Name "args")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

applicationPatternWithName :: Phantoms.TTerm Ast.ApplicationPattern -> Phantoms.TTerm Ast.Name -> Phantoms.TTerm Ast.ApplicationPattern
applicationPatternWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.ast.ApplicationPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "args"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.ApplicationPattern"),
              Core.projectionField = (Core.Name "args")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

applicationPatternWithArgs :: Phantoms.TTerm Ast.ApplicationPattern -> Phantoms.TTerm [Ast.Pattern] -> Phantoms.TTerm Ast.ApplicationPattern
applicationPatternWithArgs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.ast.ApplicationPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.ApplicationPattern"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "args"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

asPattern :: Phantoms.TTerm Ast.Name -> Phantoms.TTerm Ast.Pattern -> Phantoms.TTerm Ast.AsPattern
asPattern name inner =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.ast.AsPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "inner"),
          Core.fieldTerm = (Phantoms.unTTerm inner)}]}))

asPatternName :: Phantoms.TTerm Ast.AsPattern -> Phantoms.TTerm Ast.Name
asPatternName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.AsPattern"),
        Core.projectionField = (Core.Name "name")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

asPatternInner :: Phantoms.TTerm Ast.AsPattern -> Phantoms.TTerm Ast.Pattern
asPatternInner x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.AsPattern"),
        Core.projectionField = (Core.Name "inner")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

asPatternWithName :: Phantoms.TTerm Ast.AsPattern -> Phantoms.TTerm Ast.Name -> Phantoms.TTerm Ast.AsPattern
asPatternWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.ast.AsPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "inner"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.AsPattern"),
              Core.projectionField = (Core.Name "inner")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

asPatternWithInner :: Phantoms.TTerm Ast.AsPattern -> Phantoms.TTerm Ast.Pattern -> Phantoms.TTerm Ast.AsPattern
asPatternWithInner original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.ast.AsPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.AsPattern"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inner"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

recordPattern :: Phantoms.TTerm Ast.Name -> Phantoms.TTerm [Ast.PatternField] -> Phantoms.TTerm Ast.RecordPattern
recordPattern name fields =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.ast.RecordPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "fields"),
          Core.fieldTerm = (Phantoms.unTTerm fields)}]}))

recordPatternName :: Phantoms.TTerm Ast.RecordPattern -> Phantoms.TTerm Ast.Name
recordPatternName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.RecordPattern"),
        Core.projectionField = (Core.Name "name")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

recordPatternFields :: Phantoms.TTerm Ast.RecordPattern -> Phantoms.TTerm [Ast.PatternField]
recordPatternFields x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.RecordPattern"),
        Core.projectionField = (Core.Name "fields")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

recordPatternWithName :: Phantoms.TTerm Ast.RecordPattern -> Phantoms.TTerm Ast.Name -> Phantoms.TTerm Ast.RecordPattern
recordPatternWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.ast.RecordPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "fields"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.RecordPattern"),
              Core.projectionField = (Core.Name "fields")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

recordPatternWithFields :: Phantoms.TTerm Ast.RecordPattern -> Phantoms.TTerm [Ast.PatternField] -> Phantoms.TTerm Ast.RecordPattern
recordPatternWithFields original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.ast.RecordPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.RecordPattern"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "fields"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

typedPattern :: Phantoms.TTerm Ast.Pattern -> Phantoms.TTerm Ast.Type -> Phantoms.TTerm Ast.TypedPattern
typedPattern inner type_ =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.ast.TypedPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "inner"),
          Core.fieldTerm = (Phantoms.unTTerm inner)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm type_)}]}))

typedPatternInner :: Phantoms.TTerm Ast.TypedPattern -> Phantoms.TTerm Ast.Pattern
typedPatternInner x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.TypedPattern"),
        Core.projectionField = (Core.Name "inner")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

typedPatternType :: Phantoms.TTerm Ast.TypedPattern -> Phantoms.TTerm Ast.Type
typedPatternType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.TypedPattern"),
        Core.projectionField = (Core.Name "type")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

typedPatternWithInner :: Phantoms.TTerm Ast.TypedPattern -> Phantoms.TTerm Ast.Pattern -> Phantoms.TTerm Ast.TypedPattern
typedPatternWithInner original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.ast.TypedPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "inner"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.TypedPattern"),
              Core.projectionField = (Core.Name "type")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

typedPatternWithType :: Phantoms.TTerm Ast.TypedPattern -> Phantoms.TTerm Ast.Type -> Phantoms.TTerm Ast.TypedPattern
typedPatternWithType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.ast.TypedPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "inner"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.TypedPattern"),
              Core.projectionField = (Core.Name "inner")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

patternField :: Phantoms.TTerm Ast.Name -> Phantoms.TTerm Ast.Pattern -> Phantoms.TTerm Ast.PatternField
patternField name pattern =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.ast.PatternField"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Phantoms.unTTerm pattern)}]}))

patternFieldName :: Phantoms.TTerm Ast.PatternField -> Phantoms.TTerm Ast.Name
patternFieldName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.PatternField"),
        Core.projectionField = (Core.Name "name")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

patternFieldPattern :: Phantoms.TTerm Ast.PatternField -> Phantoms.TTerm Ast.Pattern
patternFieldPattern x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.PatternField"),
        Core.projectionField = (Core.Name "pattern")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

patternFieldWithName :: Phantoms.TTerm Ast.PatternField -> Phantoms.TTerm Ast.Name -> Phantoms.TTerm Ast.PatternField
patternFieldWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.ast.PatternField"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.PatternField"),
              Core.projectionField = (Core.Name "pattern")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

patternFieldWithPattern :: Phantoms.TTerm Ast.PatternField -> Phantoms.TTerm Ast.Pattern -> Phantoms.TTerm Ast.PatternField
patternFieldWithPattern original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.ast.PatternField"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.PatternField"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

qualifiedName :: Phantoms.TTerm [Ast.NamePart] -> Phantoms.TTerm Ast.NamePart -> Phantoms.TTerm Ast.QualifiedName
qualifiedName qualifiers unqualified =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.ast.QualifiedName"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "qualifiers"),
          Core.fieldTerm = (Phantoms.unTTerm qualifiers)},
        Core.Field {
          Core.fieldName = (Core.Name "unqualified"),
          Core.fieldTerm = (Phantoms.unTTerm unqualified)}]}))

qualifiedNameQualifiers :: Phantoms.TTerm Ast.QualifiedName -> Phantoms.TTerm [Ast.NamePart]
qualifiedNameQualifiers x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.QualifiedName"),
        Core.projectionField = (Core.Name "qualifiers")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

qualifiedNameUnqualified :: Phantoms.TTerm Ast.QualifiedName -> Phantoms.TTerm Ast.NamePart
qualifiedNameUnqualified x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.QualifiedName"),
        Core.projectionField = (Core.Name "unqualified")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

qualifiedNameWithQualifiers :: Phantoms.TTerm Ast.QualifiedName -> Phantoms.TTerm [Ast.NamePart] -> Phantoms.TTerm Ast.QualifiedName
qualifiedNameWithQualifiers original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.ast.QualifiedName"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "qualifiers"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "unqualified"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.QualifiedName"),
              Core.projectionField = (Core.Name "unqualified")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

qualifiedNameWithUnqualified :: Phantoms.TTerm Ast.QualifiedName -> Phantoms.TTerm Ast.NamePart -> Phantoms.TTerm Ast.QualifiedName
qualifiedNameWithUnqualified original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.ast.QualifiedName"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "qualifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.QualifiedName"),
              Core.projectionField = (Core.Name "qualifiers")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "unqualified"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

rightHandSide :: Phantoms.TTerm Ast.Expression -> Phantoms.TTerm Ast.RightHandSide
rightHandSide x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.haskell.ast.RightHandSide"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unRightHandSide :: Phantoms.TTerm Ast.RightHandSide -> Phantoms.TTerm Ast.Expression
unRightHandSide x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.haskell.ast.RightHandSide")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

statement :: Phantoms.TTerm Ast.Expression -> Phantoms.TTerm Ast.Statement
statement x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.haskell.ast.Statement"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unStatement :: Phantoms.TTerm Ast.Statement -> Phantoms.TTerm Ast.Expression
unStatement x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.haskell.ast.Statement")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

typeApplication :: Phantoms.TTerm Ast.ApplicationType -> Phantoms.TTerm Ast.Type
typeApplication x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.haskell.ast.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "application"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

typeCtx :: Phantoms.TTerm Ast.ContextType -> Phantoms.TTerm Ast.Type
typeCtx x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.haskell.ast.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "ctx"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

typeFunction :: Phantoms.TTerm Ast.FunctionType -> Phantoms.TTerm Ast.Type
typeFunction x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.haskell.ast.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "function"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

typeInfix :: Phantoms.TTerm Ast.InfixType -> Phantoms.TTerm Ast.Type
typeInfix x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.haskell.ast.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "infix"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

typeList :: Phantoms.TTerm Ast.Type -> Phantoms.TTerm Ast.Type
typeList x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.haskell.ast.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "list"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

typeParens :: Phantoms.TTerm Ast.Type -> Phantoms.TTerm Ast.Type
typeParens x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.haskell.ast.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "parens"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

typeTuple :: Phantoms.TTerm [Ast.Type] -> Phantoms.TTerm Ast.Type
typeTuple x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.haskell.ast.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "tuple"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

typeVariable :: Phantoms.TTerm Ast.Name -> Phantoms.TTerm Ast.Type
typeVariable x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.haskell.ast.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "variable"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

applicationType :: Phantoms.TTerm Ast.Type -> Phantoms.TTerm Ast.Type -> Phantoms.TTerm Ast.ApplicationType
applicationType context argument =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.ast.ApplicationType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "context"),
          Core.fieldTerm = (Phantoms.unTTerm context)},
        Core.Field {
          Core.fieldName = (Core.Name "argument"),
          Core.fieldTerm = (Phantoms.unTTerm argument)}]}))

applicationTypeContext :: Phantoms.TTerm Ast.ApplicationType -> Phantoms.TTerm Ast.Type
applicationTypeContext x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.ApplicationType"),
        Core.projectionField = (Core.Name "context")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

applicationTypeArgument :: Phantoms.TTerm Ast.ApplicationType -> Phantoms.TTerm Ast.Type
applicationTypeArgument x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.ApplicationType"),
        Core.projectionField = (Core.Name "argument")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

applicationTypeWithContext :: Phantoms.TTerm Ast.ApplicationType -> Phantoms.TTerm Ast.Type -> Phantoms.TTerm Ast.ApplicationType
applicationTypeWithContext original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.ast.ApplicationType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "context"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "argument"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.ApplicationType"),
              Core.projectionField = (Core.Name "argument")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

applicationTypeWithArgument :: Phantoms.TTerm Ast.ApplicationType -> Phantoms.TTerm Ast.Type -> Phantoms.TTerm Ast.ApplicationType
applicationTypeWithArgument original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.ast.ApplicationType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "context"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.ApplicationType"),
              Core.projectionField = (Core.Name "context")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "argument"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

contextType :: Phantoms.TTerm Ast.Assertion -> Phantoms.TTerm Ast.Type -> Phantoms.TTerm Ast.ContextType
contextType ctx type_ =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.ast.ContextType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "ctx"),
          Core.fieldTerm = (Phantoms.unTTerm ctx)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm type_)}]}))

contextTypeCtx :: Phantoms.TTerm Ast.ContextType -> Phantoms.TTerm Ast.Assertion
contextTypeCtx x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.ContextType"),
        Core.projectionField = (Core.Name "ctx")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

contextTypeType :: Phantoms.TTerm Ast.ContextType -> Phantoms.TTerm Ast.Type
contextTypeType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.ContextType"),
        Core.projectionField = (Core.Name "type")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

contextTypeWithCtx :: Phantoms.TTerm Ast.ContextType -> Phantoms.TTerm Ast.Assertion -> Phantoms.TTerm Ast.ContextType
contextTypeWithCtx original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.ast.ContextType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "ctx"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.ContextType"),
              Core.projectionField = (Core.Name "type")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

contextTypeWithType :: Phantoms.TTerm Ast.ContextType -> Phantoms.TTerm Ast.Type -> Phantoms.TTerm Ast.ContextType
contextTypeWithType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.ast.ContextType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "ctx"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.ContextType"),
              Core.projectionField = (Core.Name "ctx")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

functionType :: Phantoms.TTerm Ast.Type -> Phantoms.TTerm Ast.Type -> Phantoms.TTerm Ast.FunctionType
functionType domain codomain =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.ast.FunctionType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "domain"),
          Core.fieldTerm = (Phantoms.unTTerm domain)},
        Core.Field {
          Core.fieldName = (Core.Name "codomain"),
          Core.fieldTerm = (Phantoms.unTTerm codomain)}]}))

functionTypeDomain :: Phantoms.TTerm Ast.FunctionType -> Phantoms.TTerm Ast.Type
functionTypeDomain x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.FunctionType"),
        Core.projectionField = (Core.Name "domain")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

functionTypeCodomain :: Phantoms.TTerm Ast.FunctionType -> Phantoms.TTerm Ast.Type
functionTypeCodomain x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.FunctionType"),
        Core.projectionField = (Core.Name "codomain")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

functionTypeWithDomain :: Phantoms.TTerm Ast.FunctionType -> Phantoms.TTerm Ast.Type -> Phantoms.TTerm Ast.FunctionType
functionTypeWithDomain original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.ast.FunctionType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "domain"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "codomain"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.FunctionType"),
              Core.projectionField = (Core.Name "codomain")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

functionTypeWithCodomain :: Phantoms.TTerm Ast.FunctionType -> Phantoms.TTerm Ast.Type -> Phantoms.TTerm Ast.FunctionType
functionTypeWithCodomain original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.ast.FunctionType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "domain"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.FunctionType"),
              Core.projectionField = (Core.Name "domain")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "codomain"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

infixType :: Phantoms.TTerm Ast.Type -> Phantoms.TTerm Ast.Operator -> Phantoms.TTerm Ast.Operator -> Phantoms.TTerm Ast.InfixType
infixType lhs operator rhs =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.ast.InfixType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Phantoms.unTTerm lhs)},
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Phantoms.unTTerm operator)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTTerm rhs)}]}))

infixTypeLhs :: Phantoms.TTerm Ast.InfixType -> Phantoms.TTerm Ast.Type
infixTypeLhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.InfixType"),
        Core.projectionField = (Core.Name "lhs")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

infixTypeOperator :: Phantoms.TTerm Ast.InfixType -> Phantoms.TTerm Ast.Operator
infixTypeOperator x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.InfixType"),
        Core.projectionField = (Core.Name "operator")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

infixTypeRhs :: Phantoms.TTerm Ast.InfixType -> Phantoms.TTerm Ast.Operator
infixTypeRhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.InfixType"),
        Core.projectionField = (Core.Name "rhs")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

infixTypeWithLhs :: Phantoms.TTerm Ast.InfixType -> Phantoms.TTerm Ast.Type -> Phantoms.TTerm Ast.InfixType
infixTypeWithLhs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.ast.InfixType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.InfixType"),
              Core.projectionField = (Core.Name "operator")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.InfixType"),
              Core.projectionField = (Core.Name "rhs")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

infixTypeWithOperator :: Phantoms.TTerm Ast.InfixType -> Phantoms.TTerm Ast.Operator -> Phantoms.TTerm Ast.InfixType
infixTypeWithOperator original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.ast.InfixType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.InfixType"),
              Core.projectionField = (Core.Name "lhs")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.InfixType"),
              Core.projectionField = (Core.Name "rhs")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

infixTypeWithRhs :: Phantoms.TTerm Ast.InfixType -> Phantoms.TTerm Ast.Operator -> Phantoms.TTerm Ast.InfixType
infixTypeWithRhs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.ast.InfixType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.InfixType"),
              Core.projectionField = (Core.Name "lhs")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.InfixType"),
              Core.projectionField = (Core.Name "operator")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

typeDeclaration :: Phantoms.TTerm Ast.DeclarationHead -> Phantoms.TTerm Ast.Type -> Phantoms.TTerm Ast.TypeDeclaration
typeDeclaration name type_ =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.ast.TypeDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm type_)}]}))

typeDeclarationName :: Phantoms.TTerm Ast.TypeDeclaration -> Phantoms.TTerm Ast.DeclarationHead
typeDeclarationName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.TypeDeclaration"),
        Core.projectionField = (Core.Name "name")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

typeDeclarationType :: Phantoms.TTerm Ast.TypeDeclaration -> Phantoms.TTerm Ast.Type
typeDeclarationType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.TypeDeclaration"),
        Core.projectionField = (Core.Name "type")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

typeDeclarationWithName :: Phantoms.TTerm Ast.TypeDeclaration -> Phantoms.TTerm Ast.DeclarationHead -> Phantoms.TTerm Ast.TypeDeclaration
typeDeclarationWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.ast.TypeDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.TypeDeclaration"),
              Core.projectionField = (Core.Name "type")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

typeDeclarationWithType :: Phantoms.TTerm Ast.TypeDeclaration -> Phantoms.TTerm Ast.Type -> Phantoms.TTerm Ast.TypeDeclaration
typeDeclarationWithType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.ast.TypeDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.TypeDeclaration"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

typeSignature :: Phantoms.TTerm Ast.Name -> Phantoms.TTerm Ast.Type -> Phantoms.TTerm Ast.TypeSignature
typeSignature name type_ =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.ast.TypeSignature"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm type_)}]}))

typeSignatureName :: Phantoms.TTerm Ast.TypeSignature -> Phantoms.TTerm Ast.Name
typeSignatureName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.TypeSignature"),
        Core.projectionField = (Core.Name "name")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

typeSignatureType :: Phantoms.TTerm Ast.TypeSignature -> Phantoms.TTerm Ast.Type
typeSignatureType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.TypeSignature"),
        Core.projectionField = (Core.Name "type")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

typeSignatureWithName :: Phantoms.TTerm Ast.TypeSignature -> Phantoms.TTerm Ast.Name -> Phantoms.TTerm Ast.TypeSignature
typeSignatureWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.ast.TypeSignature"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.TypeSignature"),
              Core.projectionField = (Core.Name "type")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

typeSignatureWithType :: Phantoms.TTerm Ast.TypeSignature -> Phantoms.TTerm Ast.Type -> Phantoms.TTerm Ast.TypeSignature
typeSignatureWithType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.ast.TypeSignature"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.TypeSignature"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

typedBinding :: Phantoms.TTerm Ast.TypeSignature -> Phantoms.TTerm Ast.ValueBinding -> Phantoms.TTerm Ast.TypedBinding
typedBinding typeSignature valueBinding =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.ast.TypedBinding"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "typeSignature"),
          Core.fieldTerm = (Phantoms.unTTerm typeSignature)},
        Core.Field {
          Core.fieldName = (Core.Name "valueBinding"),
          Core.fieldTerm = (Phantoms.unTTerm valueBinding)}]}))

typedBindingTypeSignature :: Phantoms.TTerm Ast.TypedBinding -> Phantoms.TTerm Ast.TypeSignature
typedBindingTypeSignature x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.TypedBinding"),
        Core.projectionField = (Core.Name "typeSignature")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

typedBindingValueBinding :: Phantoms.TTerm Ast.TypedBinding -> Phantoms.TTerm Ast.ValueBinding
typedBindingValueBinding x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.TypedBinding"),
        Core.projectionField = (Core.Name "valueBinding")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

typedBindingWithTypeSignature :: Phantoms.TTerm Ast.TypedBinding -> Phantoms.TTerm Ast.TypeSignature -> Phantoms.TTerm Ast.TypedBinding
typedBindingWithTypeSignature original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.ast.TypedBinding"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "typeSignature"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "valueBinding"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.TypedBinding"),
              Core.projectionField = (Core.Name "valueBinding")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

typedBindingWithValueBinding :: Phantoms.TTerm Ast.TypedBinding -> Phantoms.TTerm Ast.ValueBinding -> Phantoms.TTerm Ast.TypedBinding
typedBindingWithValueBinding original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.ast.TypedBinding"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "typeSignature"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.TypedBinding"),
              Core.projectionField = (Core.Name "typeSignature")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "valueBinding"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

valueBindingSimple :: Phantoms.TTerm Ast.SimpleValueBinding -> Phantoms.TTerm Ast.ValueBinding
valueBindingSimple x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.haskell.ast.ValueBinding"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "simple"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

simpleValueBinding :: Phantoms.TTerm Ast.Pattern -> Phantoms.TTerm Ast.RightHandSide -> Phantoms.TTerm (Maybe Ast.LocalBindings) -> Phantoms.TTerm Ast.SimpleValueBinding
simpleValueBinding pattern rhs localBindings =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.ast.SimpleValueBinding"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Phantoms.unTTerm pattern)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTTerm rhs)},
        Core.Field {
          Core.fieldName = (Core.Name "localBindings"),
          Core.fieldTerm = (Phantoms.unTTerm localBindings)}]}))

simpleValueBindingPattern :: Phantoms.TTerm Ast.SimpleValueBinding -> Phantoms.TTerm Ast.Pattern
simpleValueBindingPattern x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.SimpleValueBinding"),
        Core.projectionField = (Core.Name "pattern")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

simpleValueBindingRhs :: Phantoms.TTerm Ast.SimpleValueBinding -> Phantoms.TTerm Ast.RightHandSide
simpleValueBindingRhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.SimpleValueBinding"),
        Core.projectionField = (Core.Name "rhs")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

simpleValueBindingLocalBindings :: Phantoms.TTerm Ast.SimpleValueBinding -> Phantoms.TTerm (Maybe Ast.LocalBindings)
simpleValueBindingLocalBindings x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.SimpleValueBinding"),
        Core.projectionField = (Core.Name "localBindings")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

simpleValueBindingWithPattern :: Phantoms.TTerm Ast.SimpleValueBinding -> Phantoms.TTerm Ast.Pattern -> Phantoms.TTerm Ast.SimpleValueBinding
simpleValueBindingWithPattern original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.ast.SimpleValueBinding"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.SimpleValueBinding"),
              Core.projectionField = (Core.Name "rhs")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "localBindings"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.SimpleValueBinding"),
              Core.projectionField = (Core.Name "localBindings")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

simpleValueBindingWithRhs :: Phantoms.TTerm Ast.SimpleValueBinding -> Phantoms.TTerm Ast.RightHandSide -> Phantoms.TTerm Ast.SimpleValueBinding
simpleValueBindingWithRhs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.ast.SimpleValueBinding"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.SimpleValueBinding"),
              Core.projectionField = (Core.Name "pattern")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "localBindings"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.SimpleValueBinding"),
              Core.projectionField = (Core.Name "localBindings")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

simpleValueBindingWithLocalBindings :: Phantoms.TTerm Ast.SimpleValueBinding -> Phantoms.TTerm (Maybe Ast.LocalBindings) -> Phantoms.TTerm Ast.SimpleValueBinding
simpleValueBindingWithLocalBindings original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.ast.SimpleValueBinding"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.SimpleValueBinding"),
              Core.projectionField = (Core.Name "pattern")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.ast.SimpleValueBinding"),
              Core.projectionField = (Core.Name "rhs")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "localBindings"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

variable :: Phantoms.TTerm Ast.Name -> Phantoms.TTerm Ast.Variable
variable x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.haskell.ast.Variable"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unVariable :: Phantoms.TTerm Ast.Variable -> Phantoms.TTerm Ast.Name
unVariable x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.haskell.ast.Variable")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
