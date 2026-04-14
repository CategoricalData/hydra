-- Note: this is an automatically generated file. Do not edit.

-- | DSL functions for hydra.datalog.syntax

module Hydra.Dsl.Datalog.Syntax where

import qualified Hydra.Core as Core
import qualified Hydra.Datalog.Syntax as Syntax
import qualified Hydra.Phantoms as Phantoms
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)

atom :: Phantoms.TTerm Syntax.Relation -> Phantoms.TTerm Syntax.TermList -> Phantoms.TTerm Syntax.Atom
atom relation termList =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.datalog.syntax.Atom"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Relation"),
          Core.fieldTerm = (Phantoms.unTTerm relation)},
        Core.Field {
          Core.fieldName = (Core.Name "TermList"),
          Core.fieldTerm = (Phantoms.unTTerm termList)}]}))

atomListMultiple :: Phantoms.TTerm Syntax.AtomList_Multiple -> Phantoms.TTerm Syntax.AtomList
atomListMultiple x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.datalog.syntax.AtomList"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "multiple"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

atomListSingle :: Phantoms.TTerm Syntax.Atom -> Phantoms.TTerm Syntax.AtomList
atomListSingle x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.datalog.syntax.AtomList"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "single"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

atomList_Multiple :: Phantoms.TTerm Syntax.Atom -> Phantoms.TTerm Syntax.AtomList -> Phantoms.TTerm Syntax.AtomList_Multiple
atomList_Multiple atom atomList =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.datalog.syntax.AtomList_Multiple"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Atom"),
          Core.fieldTerm = (Phantoms.unTTerm atom)},
        Core.Field {
          Core.fieldName = (Core.Name "AtomList"),
          Core.fieldTerm = (Phantoms.unTTerm atomList)}]}))

atomList_MultipleAtom :: Phantoms.TTerm Syntax.AtomList_Multiple -> Phantoms.TTerm Syntax.Atom
atomList_MultipleAtom x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.datalog.syntax.AtomList_Multiple"),
        Core.projectionField = (Core.Name "Atom")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

atomList_MultipleAtomList :: Phantoms.TTerm Syntax.AtomList_Multiple -> Phantoms.TTerm Syntax.AtomList
atomList_MultipleAtomList x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.datalog.syntax.AtomList_Multiple"),
        Core.projectionField = (Core.Name "AtomList")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

atomList_MultipleWithAtom :: Phantoms.TTerm Syntax.AtomList_Multiple -> Phantoms.TTerm Syntax.Atom -> Phantoms.TTerm Syntax.AtomList_Multiple
atomList_MultipleWithAtom original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.datalog.syntax.AtomList_Multiple"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Atom"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "AtomList"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.datalog.syntax.AtomList_Multiple"),
              Core.projectionField = (Core.Name "AtomList")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

atomList_MultipleWithAtomList :: Phantoms.TTerm Syntax.AtomList_Multiple -> Phantoms.TTerm Syntax.AtomList -> Phantoms.TTerm Syntax.AtomList_Multiple
atomList_MultipleWithAtomList original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.datalog.syntax.AtomList_Multiple"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Atom"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.datalog.syntax.AtomList_Multiple"),
              Core.projectionField = (Core.Name "Atom")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "AtomList"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

atomRelation :: Phantoms.TTerm Syntax.Atom -> Phantoms.TTerm Syntax.Relation
atomRelation x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.datalog.syntax.Atom"),
        Core.projectionField = (Core.Name "Relation")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

atomTermList :: Phantoms.TTerm Syntax.Atom -> Phantoms.TTerm Syntax.TermList
atomTermList x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.datalog.syntax.Atom"),
        Core.projectionField = (Core.Name "TermList")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

atomWithRelation :: Phantoms.TTerm Syntax.Atom -> Phantoms.TTerm Syntax.Relation -> Phantoms.TTerm Syntax.Atom
atomWithRelation original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.datalog.syntax.Atom"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Relation"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "TermList"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.datalog.syntax.Atom"),
              Core.projectionField = (Core.Name "TermList")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

atomWithTermList :: Phantoms.TTerm Syntax.Atom -> Phantoms.TTerm Syntax.TermList -> Phantoms.TTerm Syntax.Atom
atomWithTermList original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.datalog.syntax.Atom"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Relation"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.datalog.syntax.Atom"),
              Core.projectionField = (Core.Name "Relation")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "TermList"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

constant :: Phantoms.TTerm String -> Phantoms.TTerm Syntax.Constant
constant x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.datalog.syntax.Constant"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

constantListMultiple :: Phantoms.TTerm Syntax.ConstantList_Multiple -> Phantoms.TTerm Syntax.ConstantList
constantListMultiple x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.datalog.syntax.ConstantList"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "multiple"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

constantListSingle :: Phantoms.TTerm Syntax.Constant -> Phantoms.TTerm Syntax.ConstantList
constantListSingle x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.datalog.syntax.ConstantList"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "single"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

constantList_Multiple :: Phantoms.TTerm Syntax.Constant -> Phantoms.TTerm Syntax.ConstantList -> Phantoms.TTerm Syntax.ConstantList_Multiple
constantList_Multiple constant constantList =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.datalog.syntax.ConstantList_Multiple"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Constant"),
          Core.fieldTerm = (Phantoms.unTTerm constant)},
        Core.Field {
          Core.fieldName = (Core.Name "ConstantList"),
          Core.fieldTerm = (Phantoms.unTTerm constantList)}]}))

constantList_MultipleConstant :: Phantoms.TTerm Syntax.ConstantList_Multiple -> Phantoms.TTerm Syntax.Constant
constantList_MultipleConstant x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.datalog.syntax.ConstantList_Multiple"),
        Core.projectionField = (Core.Name "Constant")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

constantList_MultipleConstantList :: Phantoms.TTerm Syntax.ConstantList_Multiple -> Phantoms.TTerm Syntax.ConstantList
constantList_MultipleConstantList x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.datalog.syntax.ConstantList_Multiple"),
        Core.projectionField = (Core.Name "ConstantList")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

constantList_MultipleWithConstant :: Phantoms.TTerm Syntax.ConstantList_Multiple -> Phantoms.TTerm Syntax.Constant -> Phantoms.TTerm Syntax.ConstantList_Multiple
constantList_MultipleWithConstant original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.datalog.syntax.ConstantList_Multiple"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Constant"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "ConstantList"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.datalog.syntax.ConstantList_Multiple"),
              Core.projectionField = (Core.Name "ConstantList")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

constantList_MultipleWithConstantList :: Phantoms.TTerm Syntax.ConstantList_Multiple -> Phantoms.TTerm Syntax.ConstantList -> Phantoms.TTerm Syntax.ConstantList_Multiple
constantList_MultipleWithConstantList original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.datalog.syntax.ConstantList_Multiple"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Constant"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.datalog.syntax.ConstantList_Multiple"),
              Core.projectionField = (Core.Name "Constant")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ConstantList"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

fact :: Phantoms.TTerm Syntax.Relation -> Phantoms.TTerm Syntax.ConstantList -> Phantoms.TTerm Syntax.Fact
fact relation constantList =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.datalog.syntax.Fact"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Relation"),
          Core.fieldTerm = (Phantoms.unTTerm relation)},
        Core.Field {
          Core.fieldName = (Core.Name "ConstantList"),
          Core.fieldTerm = (Phantoms.unTTerm constantList)}]}))

factConstantList :: Phantoms.TTerm Syntax.Fact -> Phantoms.TTerm Syntax.ConstantList
factConstantList x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.datalog.syntax.Fact"),
        Core.projectionField = (Core.Name "ConstantList")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

factRelation :: Phantoms.TTerm Syntax.Fact -> Phantoms.TTerm Syntax.Relation
factRelation x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.datalog.syntax.Fact"),
        Core.projectionField = (Core.Name "Relation")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

factWithConstantList :: Phantoms.TTerm Syntax.Fact -> Phantoms.TTerm Syntax.ConstantList -> Phantoms.TTerm Syntax.Fact
factWithConstantList original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.datalog.syntax.Fact"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Relation"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.datalog.syntax.Fact"),
              Core.projectionField = (Core.Name "Relation")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ConstantList"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

factWithRelation :: Phantoms.TTerm Syntax.Fact -> Phantoms.TTerm Syntax.Relation -> Phantoms.TTerm Syntax.Fact
factWithRelation original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.datalog.syntax.Fact"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Relation"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "ConstantList"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.datalog.syntax.Fact"),
              Core.projectionField = (Core.Name "ConstantList")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

program :: Phantoms.TTerm [Syntax.Program_Elmt] -> Phantoms.TTerm Syntax.Program
program x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.datalog.syntax.Program"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

program_ElmtFact :: Phantoms.TTerm Syntax.Fact -> Phantoms.TTerm Syntax.Program_Elmt
program_ElmtFact x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.datalog.syntax.Program_Elmt"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "Fact"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

program_ElmtRule :: Phantoms.TTerm Syntax.Rule -> Phantoms.TTerm Syntax.Program_Elmt
program_ElmtRule x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.datalog.syntax.Program_Elmt"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "Rule"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

relation :: Phantoms.TTerm String -> Phantoms.TTerm Syntax.Relation
relation x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.datalog.syntax.Relation"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

rule :: Phantoms.TTerm Syntax.Atom -> Phantoms.TTerm Syntax.AtomList -> Phantoms.TTerm Syntax.Rule
rule atom atomList =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.datalog.syntax.Rule"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Atom"),
          Core.fieldTerm = (Phantoms.unTTerm atom)},
        Core.Field {
          Core.fieldName = (Core.Name "AtomList"),
          Core.fieldTerm = (Phantoms.unTTerm atomList)}]}))

ruleAtom :: Phantoms.TTerm Syntax.Rule -> Phantoms.TTerm Syntax.Atom
ruleAtom x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.datalog.syntax.Rule"),
        Core.projectionField = (Core.Name "Atom")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

ruleAtomList :: Phantoms.TTerm Syntax.Rule -> Phantoms.TTerm Syntax.AtomList
ruleAtomList x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.datalog.syntax.Rule"),
        Core.projectionField = (Core.Name "AtomList")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

ruleWithAtom :: Phantoms.TTerm Syntax.Rule -> Phantoms.TTerm Syntax.Atom -> Phantoms.TTerm Syntax.Rule
ruleWithAtom original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.datalog.syntax.Rule"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Atom"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "AtomList"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.datalog.syntax.Rule"),
              Core.projectionField = (Core.Name "AtomList")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

ruleWithAtomList :: Phantoms.TTerm Syntax.Rule -> Phantoms.TTerm Syntax.AtomList -> Phantoms.TTerm Syntax.Rule
ruleWithAtomList original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.datalog.syntax.Rule"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Atom"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.datalog.syntax.Rule"),
              Core.projectionField = (Core.Name "Atom")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "AtomList"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

termConstant :: Phantoms.TTerm Syntax.Constant -> Phantoms.TTerm Syntax.Term
termConstant x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.datalog.syntax.Term"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "Constant"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

termListMultiple :: Phantoms.TTerm Syntax.TermList_Multiple -> Phantoms.TTerm Syntax.TermList
termListMultiple x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.datalog.syntax.TermList"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "multiple"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

termListSingle :: Phantoms.TTerm Syntax.Term -> Phantoms.TTerm Syntax.TermList
termListSingle x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.datalog.syntax.TermList"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "single"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

termList_Multiple :: Phantoms.TTerm Syntax.Term -> Phantoms.TTerm Syntax.TermList -> Phantoms.TTerm Syntax.TermList_Multiple
termList_Multiple term termList =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.datalog.syntax.TermList_Multiple"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Term"),
          Core.fieldTerm = (Phantoms.unTTerm term)},
        Core.Field {
          Core.fieldName = (Core.Name "TermList"),
          Core.fieldTerm = (Phantoms.unTTerm termList)}]}))

termList_MultipleTerm :: Phantoms.TTerm Syntax.TermList_Multiple -> Phantoms.TTerm Syntax.Term
termList_MultipleTerm x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.datalog.syntax.TermList_Multiple"),
        Core.projectionField = (Core.Name "Term")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

termList_MultipleTermList :: Phantoms.TTerm Syntax.TermList_Multiple -> Phantoms.TTerm Syntax.TermList
termList_MultipleTermList x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.datalog.syntax.TermList_Multiple"),
        Core.projectionField = (Core.Name "TermList")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

termList_MultipleWithTerm :: Phantoms.TTerm Syntax.TermList_Multiple -> Phantoms.TTerm Syntax.Term -> Phantoms.TTerm Syntax.TermList_Multiple
termList_MultipleWithTerm original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.datalog.syntax.TermList_Multiple"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Term"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "TermList"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.datalog.syntax.TermList_Multiple"),
              Core.projectionField = (Core.Name "TermList")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

termList_MultipleWithTermList :: Phantoms.TTerm Syntax.TermList_Multiple -> Phantoms.TTerm Syntax.TermList -> Phantoms.TTerm Syntax.TermList_Multiple
termList_MultipleWithTermList original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.datalog.syntax.TermList_Multiple"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Term"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.datalog.syntax.TermList_Multiple"),
              Core.projectionField = (Core.Name "Term")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "TermList"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

termVariable :: Phantoms.TTerm Syntax.Variable -> Phantoms.TTerm Syntax.Term
termVariable x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.datalog.syntax.Term"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "Variable"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

unConstant :: Phantoms.TTerm Syntax.Constant -> Phantoms.TTerm String
unConstant x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.datalog.syntax.Constant")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unProgram :: Phantoms.TTerm Syntax.Program -> Phantoms.TTerm [Syntax.Program_Elmt]
unProgram x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.datalog.syntax.Program")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unRelation :: Phantoms.TTerm Syntax.Relation -> Phantoms.TTerm String
unRelation x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.datalog.syntax.Relation")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unVariable :: Phantoms.TTerm Syntax.Variable -> Phantoms.TTerm String
unVariable x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.datalog.syntax.Variable")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

variable :: Phantoms.TTerm String -> Phantoms.TTerm Syntax.Variable
variable x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.datalog.syntax.Variable"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
