-- Note: this is an automatically generated file. Do not edit.

-- | DSL functions for hydra.ext.scala.syntax

module Hydra.Dsl.Ext.Scala.Syntax where

import qualified Hydra.Core as Core
import qualified Hydra.Ext.Scala.Syntax as Syntax
import qualified Hydra.Phantoms as Phantoms
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

case_ :: Phantoms.TTerm Syntax.Pat -> Phantoms.TTerm (Maybe Syntax.Data) -> Phantoms.TTerm Syntax.Data -> Phantoms.TTerm Syntax.Case
case_ pat cond body =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Case"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pat"),
          Core.fieldTerm = (Phantoms.unTTerm pat)},
        Core.Field {
          Core.fieldName = (Core.Name "cond"),
          Core.fieldTerm = (Phantoms.unTTerm cond)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm body)}]}))

caseBody :: Phantoms.TTerm Syntax.Case -> Phantoms.TTerm Syntax.Data
caseBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Case"),
        Core.projectionField = (Core.Name "body")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

caseCond :: Phantoms.TTerm Syntax.Case -> Phantoms.TTerm (Maybe Syntax.Data)
caseCond x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Case"),
        Core.projectionField = (Core.Name "cond")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

casePat :: Phantoms.TTerm Syntax.Case -> Phantoms.TTerm Syntax.Pat
casePat x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Case"),
        Core.projectionField = (Core.Name "pat")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

caseTreeCase :: Phantoms.TTerm Syntax.Case -> Phantoms.TTerm Syntax.CaseTree
caseTreeCase x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.CaseTree"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "case"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

caseTreeTypeCase :: Phantoms.TTerm Syntax.TypeCase -> Phantoms.TTerm Syntax.CaseTree
caseTreeTypeCase x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.CaseTree"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "typeCase"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

caseWithBody :: Phantoms.TTerm Syntax.Case -> Phantoms.TTerm Syntax.Data -> Phantoms.TTerm Syntax.Case
caseWithBody original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Case"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pat"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Case"),
              Core.projectionField = (Core.Name "pat")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "cond"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Case"),
              Core.projectionField = (Core.Name "cond")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

caseWithCond :: Phantoms.TTerm Syntax.Case -> Phantoms.TTerm (Maybe Syntax.Data) -> Phantoms.TTerm Syntax.Case
caseWithCond original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Case"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pat"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Case"),
              Core.projectionField = (Core.Name "pat")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "cond"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Case"),
              Core.projectionField = (Core.Name "body")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

caseWithPat :: Phantoms.TTerm Syntax.Case -> Phantoms.TTerm Syntax.Pat -> Phantoms.TTerm Syntax.Case
caseWithPat original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Case"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pat"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "cond"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Case"),
              Core.projectionField = (Core.Name "cond")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Case"),
              Core.projectionField = (Core.Name "body")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

ctorPrimary :: Phantoms.TTerm Syntax.Ctor_Primary -> Phantoms.TTerm Syntax.Ctor
ctorPrimary x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Ctor"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "primary"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

ctorSecondary :: Phantoms.TTerm Syntax.Ctor_Secondary -> Phantoms.TTerm Syntax.Ctor
ctorSecondary x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Ctor"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "secondary"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

ctor_Primary :: Phantoms.TTerm [Syntax.Mod] -> Phantoms.TTerm Syntax.Name -> Phantoms.TTerm [[Syntax.Data_Param]] -> Phantoms.TTerm Syntax.Ctor_Primary
ctor_Primary mods name paramss =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Ctor_Primary"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Phantoms.unTTerm mods)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "paramss"),
          Core.fieldTerm = (Phantoms.unTTerm paramss)}]}))

ctor_PrimaryMods :: Phantoms.TTerm Syntax.Ctor_Primary -> Phantoms.TTerm [Syntax.Mod]
ctor_PrimaryMods x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Ctor_Primary"),
        Core.projectionField = (Core.Name "mods")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

ctor_PrimaryName :: Phantoms.TTerm Syntax.Ctor_Primary -> Phantoms.TTerm Syntax.Name
ctor_PrimaryName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Ctor_Primary"),
        Core.projectionField = (Core.Name "name")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

ctor_PrimaryParamss :: Phantoms.TTerm Syntax.Ctor_Primary -> Phantoms.TTerm [[Syntax.Data_Param]]
ctor_PrimaryParamss x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Ctor_Primary"),
        Core.projectionField = (Core.Name "paramss")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

ctor_PrimaryWithMods :: Phantoms.TTerm Syntax.Ctor_Primary -> Phantoms.TTerm [Syntax.Mod] -> Phantoms.TTerm Syntax.Ctor_Primary
ctor_PrimaryWithMods original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Ctor_Primary"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Ctor_Primary"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "paramss"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Ctor_Primary"),
              Core.projectionField = (Core.Name "paramss")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

ctor_PrimaryWithName :: Phantoms.TTerm Syntax.Ctor_Primary -> Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.Ctor_Primary
ctor_PrimaryWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Ctor_Primary"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Ctor_Primary"),
              Core.projectionField = (Core.Name "mods")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "paramss"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Ctor_Primary"),
              Core.projectionField = (Core.Name "paramss")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

ctor_PrimaryWithParamss :: Phantoms.TTerm Syntax.Ctor_Primary -> Phantoms.TTerm [[Syntax.Data_Param]] -> Phantoms.TTerm Syntax.Ctor_Primary
ctor_PrimaryWithParamss original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Ctor_Primary"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Ctor_Primary"),
              Core.projectionField = (Core.Name "mods")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Ctor_Primary"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "paramss"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

ctor_Secondary :: Phantoms.TTerm [Syntax.Mod] -> Phantoms.TTerm Syntax.Name -> Phantoms.TTerm [[Syntax.Data_Param]] -> Phantoms.TTerm Syntax.Init -> Phantoms.TTerm [Syntax.Stat] -> Phantoms.TTerm Syntax.Ctor_Secondary
ctor_Secondary mods name paramss init stats =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Ctor_Secondary"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Phantoms.unTTerm mods)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "paramss"),
          Core.fieldTerm = (Phantoms.unTTerm paramss)},
        Core.Field {
          Core.fieldName = (Core.Name "init"),
          Core.fieldTerm = (Phantoms.unTTerm init)},
        Core.Field {
          Core.fieldName = (Core.Name "stats"),
          Core.fieldTerm = (Phantoms.unTTerm stats)}]}))

ctor_SecondaryInit :: Phantoms.TTerm Syntax.Ctor_Secondary -> Phantoms.TTerm Syntax.Init
ctor_SecondaryInit x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Ctor_Secondary"),
        Core.projectionField = (Core.Name "init")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

ctor_SecondaryMods :: Phantoms.TTerm Syntax.Ctor_Secondary -> Phantoms.TTerm [Syntax.Mod]
ctor_SecondaryMods x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Ctor_Secondary"),
        Core.projectionField = (Core.Name "mods")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

ctor_SecondaryName :: Phantoms.TTerm Syntax.Ctor_Secondary -> Phantoms.TTerm Syntax.Name
ctor_SecondaryName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Ctor_Secondary"),
        Core.projectionField = (Core.Name "name")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

ctor_SecondaryParamss :: Phantoms.TTerm Syntax.Ctor_Secondary -> Phantoms.TTerm [[Syntax.Data_Param]]
ctor_SecondaryParamss x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Ctor_Secondary"),
        Core.projectionField = (Core.Name "paramss")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

ctor_SecondaryStats :: Phantoms.TTerm Syntax.Ctor_Secondary -> Phantoms.TTerm [Syntax.Stat]
ctor_SecondaryStats x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Ctor_Secondary"),
        Core.projectionField = (Core.Name "stats")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

ctor_SecondaryWithInit :: Phantoms.TTerm Syntax.Ctor_Secondary -> Phantoms.TTerm Syntax.Init -> Phantoms.TTerm Syntax.Ctor_Secondary
ctor_SecondaryWithInit original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Ctor_Secondary"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Ctor_Secondary"),
              Core.projectionField = (Core.Name "mods")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Ctor_Secondary"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "paramss"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Ctor_Secondary"),
              Core.projectionField = (Core.Name "paramss")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "init"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "stats"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Ctor_Secondary"),
              Core.projectionField = (Core.Name "stats")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

ctor_SecondaryWithMods :: Phantoms.TTerm Syntax.Ctor_Secondary -> Phantoms.TTerm [Syntax.Mod] -> Phantoms.TTerm Syntax.Ctor_Secondary
ctor_SecondaryWithMods original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Ctor_Secondary"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Ctor_Secondary"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "paramss"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Ctor_Secondary"),
              Core.projectionField = (Core.Name "paramss")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "init"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Ctor_Secondary"),
              Core.projectionField = (Core.Name "init")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "stats"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Ctor_Secondary"),
              Core.projectionField = (Core.Name "stats")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

ctor_SecondaryWithName :: Phantoms.TTerm Syntax.Ctor_Secondary -> Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.Ctor_Secondary
ctor_SecondaryWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Ctor_Secondary"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Ctor_Secondary"),
              Core.projectionField = (Core.Name "mods")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "paramss"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Ctor_Secondary"),
              Core.projectionField = (Core.Name "paramss")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "init"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Ctor_Secondary"),
              Core.projectionField = (Core.Name "init")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "stats"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Ctor_Secondary"),
              Core.projectionField = (Core.Name "stats")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

ctor_SecondaryWithParamss :: Phantoms.TTerm Syntax.Ctor_Secondary -> Phantoms.TTerm [[Syntax.Data_Param]] -> Phantoms.TTerm Syntax.Ctor_Secondary
ctor_SecondaryWithParamss original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Ctor_Secondary"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Ctor_Secondary"),
              Core.projectionField = (Core.Name "mods")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Ctor_Secondary"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "paramss"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "init"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Ctor_Secondary"),
              Core.projectionField = (Core.Name "init")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "stats"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Ctor_Secondary"),
              Core.projectionField = (Core.Name "stats")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

ctor_SecondaryWithStats :: Phantoms.TTerm Syntax.Ctor_Secondary -> Phantoms.TTerm [Syntax.Stat] -> Phantoms.TTerm Syntax.Ctor_Secondary
ctor_SecondaryWithStats original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Ctor_Secondary"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Ctor_Secondary"),
              Core.projectionField = (Core.Name "mods")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Ctor_Secondary"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "paramss"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Ctor_Secondary"),
              Core.projectionField = (Core.Name "paramss")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "init"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Ctor_Secondary"),
              Core.projectionField = (Core.Name "init")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "stats"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

dataAnnotate :: Phantoms.TTerm Syntax.Data_Annotate -> Phantoms.TTerm Syntax.Data
dataAnnotate x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "annotate"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

dataApply :: Phantoms.TTerm Syntax.Data_Apply -> Phantoms.TTerm Syntax.Data
dataApply x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "apply"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

dataApplyType :: Phantoms.TTerm Syntax.Data_ApplyType -> Phantoms.TTerm Syntax.Data
dataApplyType x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "applyType"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

dataApplyUsing :: Phantoms.TTerm Syntax.Data_ApplyUsing -> Phantoms.TTerm Syntax.Data
dataApplyUsing x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "applyUsing"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

dataAscribe :: Phantoms.TTerm Syntax.Data_Ascribe -> Phantoms.TTerm Syntax.Data
dataAscribe x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "ascribe"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

dataAssign :: Phantoms.TTerm Syntax.Data_Assign -> Phantoms.TTerm Syntax.Data
dataAssign x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "assign"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

dataBlock :: Phantoms.TTerm Syntax.Data_Block -> Phantoms.TTerm Syntax.Data
dataBlock x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "block"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

dataDo :: Phantoms.TTerm Syntax.Data_Do -> Phantoms.TTerm Syntax.Data
dataDo x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "do"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

dataEndMarker :: Phantoms.TTerm Syntax.Data_EndMarker -> Phantoms.TTerm Syntax.Data
dataEndMarker x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "endMarker"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

dataEta :: Phantoms.TTerm Syntax.Data_Eta -> Phantoms.TTerm Syntax.Data
dataEta x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "eta"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

dataFor :: Phantoms.TTerm Syntax.Data_For -> Phantoms.TTerm Syntax.Data
dataFor x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "for"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

dataForYield :: Phantoms.TTerm Syntax.Data_ForYield -> Phantoms.TTerm Syntax.Data
dataForYield x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "forYield"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

dataFunctionData :: Phantoms.TTerm Syntax.Data_FunctionData -> Phantoms.TTerm Syntax.Data
dataFunctionData x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "functionData"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

dataIf :: Phantoms.TTerm Syntax.Data_If -> Phantoms.TTerm Syntax.Data
dataIf x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "if"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

dataInterpolate :: Phantoms.TTerm Syntax.Data_Interpolate -> Phantoms.TTerm Syntax.Data
dataInterpolate x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "interpolate"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

dataLit :: Phantoms.TTerm Syntax.Lit -> Phantoms.TTerm Syntax.Data
dataLit x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "lit"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

dataMatch :: Phantoms.TTerm Syntax.Data_Match -> Phantoms.TTerm Syntax.Data
dataMatch x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "match"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

dataNew :: Phantoms.TTerm Syntax.Data_New -> Phantoms.TTerm Syntax.Data
dataNew x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "new"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

dataNewAnonymous :: Phantoms.TTerm Syntax.Data_NewAnonymous -> Phantoms.TTerm Syntax.Data
dataNewAnonymous x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "newAnonymous"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

dataParam :: Phantoms.TTerm Syntax.Data_Param -> Phantoms.TTerm Syntax.Data
dataParam x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "param"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

dataPartialFunction :: Phantoms.TTerm Syntax.Data_PartialFunction -> Phantoms.TTerm Syntax.Data
dataPartialFunction x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "partialFunction"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

dataPlaceholder :: Phantoms.TTerm Syntax.Data
dataPlaceholder =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "placeholder"),
        Core.fieldTerm = Core.TermUnit}}))

dataPolyFunction :: Phantoms.TTerm Syntax.Data_PolyFunction -> Phantoms.TTerm Syntax.Data
dataPolyFunction x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "polyFunction"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

dataQuotedMacroExpr :: Phantoms.TTerm Syntax.Data_QuotedMacroExpr -> Phantoms.TTerm Syntax.Data
dataQuotedMacroExpr x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "quotedMacroExpr"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

dataQuotedMacroType :: Phantoms.TTerm Syntax.Data_QuotedMacroType -> Phantoms.TTerm Syntax.Data
dataQuotedMacroType x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "quotedMacroType"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

dataRef :: Phantoms.TTerm Syntax.Data_Ref -> Phantoms.TTerm Syntax.Data
dataRef x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "ref"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

dataRepeated :: Phantoms.TTerm Syntax.Data_Repeated -> Phantoms.TTerm Syntax.Data
dataRepeated x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "repeated"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

dataReturn :: Phantoms.TTerm Syntax.Data_Return -> Phantoms.TTerm Syntax.Data
dataReturn x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "return"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

dataSplicedMacroExpr :: Phantoms.TTerm Syntax.Data_SplicedMacroExpr -> Phantoms.TTerm Syntax.Data
dataSplicedMacroExpr x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "splicedMacroExpr"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

dataThrow :: Phantoms.TTerm Syntax.Data_Throw -> Phantoms.TTerm Syntax.Data
dataThrow x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "throw"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

dataTry :: Phantoms.TTerm Syntax.Data_Try -> Phantoms.TTerm Syntax.Data
dataTry x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "try"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

dataTryWithHandler :: Phantoms.TTerm Syntax.Data_TryWithHandler -> Phantoms.TTerm Syntax.Data
dataTryWithHandler x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "tryWithHandler"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

dataTuple :: Phantoms.TTerm Syntax.Data_Tuple -> Phantoms.TTerm Syntax.Data
dataTuple x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "tuple"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

dataWhile :: Phantoms.TTerm Syntax.Data_While -> Phantoms.TTerm Syntax.Data
dataWhile x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "while"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

dataXml :: Phantoms.TTerm Syntax.Data_Xml -> Phantoms.TTerm Syntax.Data
dataXml x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "xml"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

data_Annotate :: Phantoms.TTerm Syntax.Data -> Phantoms.TTerm [Syntax.Mod_Annot] -> Phantoms.TTerm Syntax.Data_Annotate
data_Annotate expr annots =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Data_Annotate"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expr"),
          Core.fieldTerm = (Phantoms.unTTerm expr)},
        Core.Field {
          Core.fieldName = (Core.Name "annots"),
          Core.fieldTerm = (Phantoms.unTTerm annots)}]}))

data_AnnotateAnnots :: Phantoms.TTerm Syntax.Data_Annotate -> Phantoms.TTerm [Syntax.Mod_Annot]
data_AnnotateAnnots x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data_Annotate"),
        Core.projectionField = (Core.Name "annots")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

data_AnnotateExpr :: Phantoms.TTerm Syntax.Data_Annotate -> Phantoms.TTerm Syntax.Data
data_AnnotateExpr x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data_Annotate"),
        Core.projectionField = (Core.Name "expr")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

data_AnnotateWithAnnots :: Phantoms.TTerm Syntax.Data_Annotate -> Phantoms.TTerm [Syntax.Mod_Annot] -> Phantoms.TTerm Syntax.Data_Annotate
data_AnnotateWithAnnots original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Data_Annotate"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expr"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data_Annotate"),
              Core.projectionField = (Core.Name "expr")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "annots"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

data_AnnotateWithExpr :: Phantoms.TTerm Syntax.Data_Annotate -> Phantoms.TTerm Syntax.Data -> Phantoms.TTerm Syntax.Data_Annotate
data_AnnotateWithExpr original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Data_Annotate"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expr"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "annots"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data_Annotate"),
              Core.projectionField = (Core.Name "annots")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

data_Anonymous :: Phantoms.TTerm () -> Phantoms.TTerm Syntax.Data_Anonymous
data_Anonymous x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.scala.syntax.Data_Anonymous"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

data_Apply :: Phantoms.TTerm Syntax.Data -> Phantoms.TTerm [Syntax.Data] -> Phantoms.TTerm Syntax.Data_Apply
data_Apply fun args =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Data_Apply"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "fun"),
          Core.fieldTerm = (Phantoms.unTTerm fun)},
        Core.Field {
          Core.fieldName = (Core.Name "args"),
          Core.fieldTerm = (Phantoms.unTTerm args)}]}))

data_ApplyArgs :: Phantoms.TTerm Syntax.Data_Apply -> Phantoms.TTerm [Syntax.Data]
data_ApplyArgs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data_Apply"),
        Core.projectionField = (Core.Name "args")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

data_ApplyFun :: Phantoms.TTerm Syntax.Data_Apply -> Phantoms.TTerm Syntax.Data
data_ApplyFun x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data_Apply"),
        Core.projectionField = (Core.Name "fun")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

data_ApplyInfix :: Phantoms.TTerm Syntax.Data -> Phantoms.TTerm Syntax.Data_Name -> Phantoms.TTerm [Syntax.Type] -> Phantoms.TTerm [Syntax.Data] -> Phantoms.TTerm Syntax.Data_ApplyInfix
data_ApplyInfix lhs op targs args =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Data_ApplyInfix"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Phantoms.unTTerm lhs)},
        Core.Field {
          Core.fieldName = (Core.Name "op"),
          Core.fieldTerm = (Phantoms.unTTerm op)},
        Core.Field {
          Core.fieldName = (Core.Name "targs"),
          Core.fieldTerm = (Phantoms.unTTerm targs)},
        Core.Field {
          Core.fieldName = (Core.Name "args"),
          Core.fieldTerm = (Phantoms.unTTerm args)}]}))

data_ApplyInfixArgs :: Phantoms.TTerm Syntax.Data_ApplyInfix -> Phantoms.TTerm [Syntax.Data]
data_ApplyInfixArgs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data_ApplyInfix"),
        Core.projectionField = (Core.Name "args")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

data_ApplyInfixLhs :: Phantoms.TTerm Syntax.Data_ApplyInfix -> Phantoms.TTerm Syntax.Data
data_ApplyInfixLhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data_ApplyInfix"),
        Core.projectionField = (Core.Name "lhs")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

data_ApplyInfixOp :: Phantoms.TTerm Syntax.Data_ApplyInfix -> Phantoms.TTerm Syntax.Data_Name
data_ApplyInfixOp x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data_ApplyInfix"),
        Core.projectionField = (Core.Name "op")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

data_ApplyInfixTargs :: Phantoms.TTerm Syntax.Data_ApplyInfix -> Phantoms.TTerm [Syntax.Type]
data_ApplyInfixTargs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data_ApplyInfix"),
        Core.projectionField = (Core.Name "targs")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

data_ApplyInfixWithArgs :: Phantoms.TTerm Syntax.Data_ApplyInfix -> Phantoms.TTerm [Syntax.Data] -> Phantoms.TTerm Syntax.Data_ApplyInfix
data_ApplyInfixWithArgs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Data_ApplyInfix"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data_ApplyInfix"),
              Core.projectionField = (Core.Name "lhs")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "op"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data_ApplyInfix"),
              Core.projectionField = (Core.Name "op")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "targs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data_ApplyInfix"),
              Core.projectionField = (Core.Name "targs")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "args"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

data_ApplyInfixWithLhs :: Phantoms.TTerm Syntax.Data_ApplyInfix -> Phantoms.TTerm Syntax.Data -> Phantoms.TTerm Syntax.Data_ApplyInfix
data_ApplyInfixWithLhs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Data_ApplyInfix"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "op"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data_ApplyInfix"),
              Core.projectionField = (Core.Name "op")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "targs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data_ApplyInfix"),
              Core.projectionField = (Core.Name "targs")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "args"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data_ApplyInfix"),
              Core.projectionField = (Core.Name "args")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

data_ApplyInfixWithOp :: Phantoms.TTerm Syntax.Data_ApplyInfix -> Phantoms.TTerm Syntax.Data_Name -> Phantoms.TTerm Syntax.Data_ApplyInfix
data_ApplyInfixWithOp original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Data_ApplyInfix"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data_ApplyInfix"),
              Core.projectionField = (Core.Name "lhs")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "op"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "targs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data_ApplyInfix"),
              Core.projectionField = (Core.Name "targs")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "args"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data_ApplyInfix"),
              Core.projectionField = (Core.Name "args")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

data_ApplyInfixWithTargs :: Phantoms.TTerm Syntax.Data_ApplyInfix -> Phantoms.TTerm [Syntax.Type] -> Phantoms.TTerm Syntax.Data_ApplyInfix
data_ApplyInfixWithTargs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Data_ApplyInfix"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data_ApplyInfix"),
              Core.projectionField = (Core.Name "lhs")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "op"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data_ApplyInfix"),
              Core.projectionField = (Core.Name "op")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "targs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "args"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data_ApplyInfix"),
              Core.projectionField = (Core.Name "args")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

data_ApplyType :: Phantoms.TTerm Syntax.Data -> Phantoms.TTerm Syntax.Data_Name -> Phantoms.TTerm [Syntax.Type] -> Phantoms.TTerm [Syntax.Data] -> Phantoms.TTerm Syntax.Data_ApplyType
data_ApplyType lhs op targs args =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Data_ApplyType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Phantoms.unTTerm lhs)},
        Core.Field {
          Core.fieldName = (Core.Name "op"),
          Core.fieldTerm = (Phantoms.unTTerm op)},
        Core.Field {
          Core.fieldName = (Core.Name "targs"),
          Core.fieldTerm = (Phantoms.unTTerm targs)},
        Core.Field {
          Core.fieldName = (Core.Name "args"),
          Core.fieldTerm = (Phantoms.unTTerm args)}]}))

data_ApplyTypeArgs :: Phantoms.TTerm Syntax.Data_ApplyType -> Phantoms.TTerm [Syntax.Data]
data_ApplyTypeArgs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data_ApplyType"),
        Core.projectionField = (Core.Name "args")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

data_ApplyTypeLhs :: Phantoms.TTerm Syntax.Data_ApplyType -> Phantoms.TTerm Syntax.Data
data_ApplyTypeLhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data_ApplyType"),
        Core.projectionField = (Core.Name "lhs")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

data_ApplyTypeOp :: Phantoms.TTerm Syntax.Data_ApplyType -> Phantoms.TTerm Syntax.Data_Name
data_ApplyTypeOp x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data_ApplyType"),
        Core.projectionField = (Core.Name "op")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

data_ApplyTypeTargs :: Phantoms.TTerm Syntax.Data_ApplyType -> Phantoms.TTerm [Syntax.Type]
data_ApplyTypeTargs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data_ApplyType"),
        Core.projectionField = (Core.Name "targs")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

data_ApplyTypeWithArgs :: Phantoms.TTerm Syntax.Data_ApplyType -> Phantoms.TTerm [Syntax.Data] -> Phantoms.TTerm Syntax.Data_ApplyType
data_ApplyTypeWithArgs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Data_ApplyType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data_ApplyType"),
              Core.projectionField = (Core.Name "lhs")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "op"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data_ApplyType"),
              Core.projectionField = (Core.Name "op")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "targs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data_ApplyType"),
              Core.projectionField = (Core.Name "targs")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "args"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

data_ApplyTypeWithLhs :: Phantoms.TTerm Syntax.Data_ApplyType -> Phantoms.TTerm Syntax.Data -> Phantoms.TTerm Syntax.Data_ApplyType
data_ApplyTypeWithLhs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Data_ApplyType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "op"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data_ApplyType"),
              Core.projectionField = (Core.Name "op")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "targs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data_ApplyType"),
              Core.projectionField = (Core.Name "targs")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "args"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data_ApplyType"),
              Core.projectionField = (Core.Name "args")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

data_ApplyTypeWithOp :: Phantoms.TTerm Syntax.Data_ApplyType -> Phantoms.TTerm Syntax.Data_Name -> Phantoms.TTerm Syntax.Data_ApplyType
data_ApplyTypeWithOp original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Data_ApplyType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data_ApplyType"),
              Core.projectionField = (Core.Name "lhs")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "op"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "targs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data_ApplyType"),
              Core.projectionField = (Core.Name "targs")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "args"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data_ApplyType"),
              Core.projectionField = (Core.Name "args")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

data_ApplyTypeWithTargs :: Phantoms.TTerm Syntax.Data_ApplyType -> Phantoms.TTerm [Syntax.Type] -> Phantoms.TTerm Syntax.Data_ApplyType
data_ApplyTypeWithTargs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Data_ApplyType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data_ApplyType"),
              Core.projectionField = (Core.Name "lhs")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "op"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data_ApplyType"),
              Core.projectionField = (Core.Name "op")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "targs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "args"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data_ApplyType"),
              Core.projectionField = (Core.Name "args")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

data_ApplyUnary :: Phantoms.TTerm Syntax.Data_Name -> Phantoms.TTerm Syntax.Data -> Phantoms.TTerm Syntax.Data_ApplyUnary
data_ApplyUnary op arg =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Data_ApplyUnary"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "op"),
          Core.fieldTerm = (Phantoms.unTTerm op)},
        Core.Field {
          Core.fieldName = (Core.Name "arg"),
          Core.fieldTerm = (Phantoms.unTTerm arg)}]}))

data_ApplyUnaryArg :: Phantoms.TTerm Syntax.Data_ApplyUnary -> Phantoms.TTerm Syntax.Data
data_ApplyUnaryArg x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data_ApplyUnary"),
        Core.projectionField = (Core.Name "arg")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

data_ApplyUnaryOp :: Phantoms.TTerm Syntax.Data_ApplyUnary -> Phantoms.TTerm Syntax.Data_Name
data_ApplyUnaryOp x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data_ApplyUnary"),
        Core.projectionField = (Core.Name "op")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

data_ApplyUnaryWithArg :: Phantoms.TTerm Syntax.Data_ApplyUnary -> Phantoms.TTerm Syntax.Data -> Phantoms.TTerm Syntax.Data_ApplyUnary
data_ApplyUnaryWithArg original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Data_ApplyUnary"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "op"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data_ApplyUnary"),
              Core.projectionField = (Core.Name "op")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "arg"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

data_ApplyUnaryWithOp :: Phantoms.TTerm Syntax.Data_ApplyUnary -> Phantoms.TTerm Syntax.Data_Name -> Phantoms.TTerm Syntax.Data_ApplyUnary
data_ApplyUnaryWithOp original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Data_ApplyUnary"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "op"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "arg"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data_ApplyUnary"),
              Core.projectionField = (Core.Name "arg")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

data_ApplyUsing :: Phantoms.TTerm Syntax.Data -> Phantoms.TTerm [Syntax.Data] -> Phantoms.TTerm Syntax.Data_ApplyUsing
data_ApplyUsing fun targs =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Data_ApplyUsing"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "fun"),
          Core.fieldTerm = (Phantoms.unTTerm fun)},
        Core.Field {
          Core.fieldName = (Core.Name "targs"),
          Core.fieldTerm = (Phantoms.unTTerm targs)}]}))

data_ApplyUsingFun :: Phantoms.TTerm Syntax.Data_ApplyUsing -> Phantoms.TTerm Syntax.Data
data_ApplyUsingFun x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data_ApplyUsing"),
        Core.projectionField = (Core.Name "fun")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

data_ApplyUsingTargs :: Phantoms.TTerm Syntax.Data_ApplyUsing -> Phantoms.TTerm [Syntax.Data]
data_ApplyUsingTargs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data_ApplyUsing"),
        Core.projectionField = (Core.Name "targs")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

data_ApplyUsingWithFun :: Phantoms.TTerm Syntax.Data_ApplyUsing -> Phantoms.TTerm Syntax.Data -> Phantoms.TTerm Syntax.Data_ApplyUsing
data_ApplyUsingWithFun original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Data_ApplyUsing"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "fun"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "targs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data_ApplyUsing"),
              Core.projectionField = (Core.Name "targs")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

data_ApplyUsingWithTargs :: Phantoms.TTerm Syntax.Data_ApplyUsing -> Phantoms.TTerm [Syntax.Data] -> Phantoms.TTerm Syntax.Data_ApplyUsing
data_ApplyUsingWithTargs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Data_ApplyUsing"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "fun"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data_ApplyUsing"),
              Core.projectionField = (Core.Name "fun")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "targs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

data_ApplyWithArgs :: Phantoms.TTerm Syntax.Data_Apply -> Phantoms.TTerm [Syntax.Data] -> Phantoms.TTerm Syntax.Data_Apply
data_ApplyWithArgs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Data_Apply"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "fun"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data_Apply"),
              Core.projectionField = (Core.Name "fun")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "args"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

data_ApplyWithFun :: Phantoms.TTerm Syntax.Data_Apply -> Phantoms.TTerm Syntax.Data -> Phantoms.TTerm Syntax.Data_Apply
data_ApplyWithFun original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Data_Apply"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "fun"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "args"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data_Apply"),
              Core.projectionField = (Core.Name "args")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

data_Ascribe :: Phantoms.TTerm Syntax.Data -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.Data_Ascribe
data_Ascribe expr tpe =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Data_Ascribe"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expr"),
          Core.fieldTerm = (Phantoms.unTTerm expr)},
        Core.Field {
          Core.fieldName = (Core.Name "tpe"),
          Core.fieldTerm = (Phantoms.unTTerm tpe)}]}))

data_AscribeExpr :: Phantoms.TTerm Syntax.Data_Ascribe -> Phantoms.TTerm Syntax.Data
data_AscribeExpr x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data_Ascribe"),
        Core.projectionField = (Core.Name "expr")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

data_AscribeTpe :: Phantoms.TTerm Syntax.Data_Ascribe -> Phantoms.TTerm Syntax.Type
data_AscribeTpe x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data_Ascribe"),
        Core.projectionField = (Core.Name "tpe")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

data_AscribeWithExpr :: Phantoms.TTerm Syntax.Data_Ascribe -> Phantoms.TTerm Syntax.Data -> Phantoms.TTerm Syntax.Data_Ascribe
data_AscribeWithExpr original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Data_Ascribe"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expr"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "tpe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data_Ascribe"),
              Core.projectionField = (Core.Name "tpe")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

data_AscribeWithTpe :: Phantoms.TTerm Syntax.Data_Ascribe -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.Data_Ascribe
data_AscribeWithTpe original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Data_Ascribe"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expr"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data_Ascribe"),
              Core.projectionField = (Core.Name "expr")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tpe"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

data_Assign :: Phantoms.TTerm Syntax.Data -> Phantoms.TTerm Syntax.Data -> Phantoms.TTerm Syntax.Data_Assign
data_Assign lhs rhs =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Data_Assign"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Phantoms.unTTerm lhs)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTTerm rhs)}]}))

data_AssignLhs :: Phantoms.TTerm Syntax.Data_Assign -> Phantoms.TTerm Syntax.Data
data_AssignLhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data_Assign"),
        Core.projectionField = (Core.Name "lhs")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

data_AssignRhs :: Phantoms.TTerm Syntax.Data_Assign -> Phantoms.TTerm Syntax.Data
data_AssignRhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data_Assign"),
        Core.projectionField = (Core.Name "rhs")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

data_AssignWithLhs :: Phantoms.TTerm Syntax.Data_Assign -> Phantoms.TTerm Syntax.Data -> Phantoms.TTerm Syntax.Data_Assign
data_AssignWithLhs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Data_Assign"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data_Assign"),
              Core.projectionField = (Core.Name "rhs")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

data_AssignWithRhs :: Phantoms.TTerm Syntax.Data_Assign -> Phantoms.TTerm Syntax.Data -> Phantoms.TTerm Syntax.Data_Assign
data_AssignWithRhs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Data_Assign"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data_Assign"),
              Core.projectionField = (Core.Name "lhs")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

data_Block :: Phantoms.TTerm [Syntax.Stat] -> Phantoms.TTerm Syntax.Data_Block
data_Block stats =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Data_Block"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "stats"),
          Core.fieldTerm = (Phantoms.unTTerm stats)}]}))

data_BlockStats :: Phantoms.TTerm Syntax.Data_Block -> Phantoms.TTerm [Syntax.Stat]
data_BlockStats x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data_Block"),
        Core.projectionField = (Core.Name "stats")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

data_BlockWithStats :: Phantoms.TTerm Syntax.Data_Block -> Phantoms.TTerm [Syntax.Stat] -> Phantoms.TTerm Syntax.Data_Block
data_BlockWithStats original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Data_Block"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "stats"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

data_ContextFunction :: Phantoms.TTerm [Syntax.Data_Param] -> Phantoms.TTerm Syntax.Data -> Phantoms.TTerm Syntax.Data_ContextFunction
data_ContextFunction params body =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Data_ContextFunction"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Phantoms.unTTerm params)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm body)}]}))

data_ContextFunctionBody :: Phantoms.TTerm Syntax.Data_ContextFunction -> Phantoms.TTerm Syntax.Data
data_ContextFunctionBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data_ContextFunction"),
        Core.projectionField = (Core.Name "body")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

data_ContextFunctionParams :: Phantoms.TTerm Syntax.Data_ContextFunction -> Phantoms.TTerm [Syntax.Data_Param]
data_ContextFunctionParams x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data_ContextFunction"),
        Core.projectionField = (Core.Name "params")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

data_ContextFunctionWithBody :: Phantoms.TTerm Syntax.Data_ContextFunction -> Phantoms.TTerm Syntax.Data -> Phantoms.TTerm Syntax.Data_ContextFunction
data_ContextFunctionWithBody original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Data_ContextFunction"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data_ContextFunction"),
              Core.projectionField = (Core.Name "params")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

data_ContextFunctionWithParams :: Phantoms.TTerm Syntax.Data_ContextFunction -> Phantoms.TTerm [Syntax.Data_Param] -> Phantoms.TTerm Syntax.Data_ContextFunction
data_ContextFunctionWithParams original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Data_ContextFunction"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data_ContextFunction"),
              Core.projectionField = (Core.Name "body")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

data_Do :: Phantoms.TTerm Syntax.Data -> Phantoms.TTerm Syntax.Data -> Phantoms.TTerm Syntax.Data_Do
data_Do body expr =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Data_Do"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm body)},
        Core.Field {
          Core.fieldName = (Core.Name "expr"),
          Core.fieldTerm = (Phantoms.unTTerm expr)}]}))

data_DoBody :: Phantoms.TTerm Syntax.Data_Do -> Phantoms.TTerm Syntax.Data
data_DoBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data_Do"),
        Core.projectionField = (Core.Name "body")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

data_DoExpr :: Phantoms.TTerm Syntax.Data_Do -> Phantoms.TTerm Syntax.Data
data_DoExpr x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data_Do"),
        Core.projectionField = (Core.Name "expr")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

data_DoWithBody :: Phantoms.TTerm Syntax.Data_Do -> Phantoms.TTerm Syntax.Data -> Phantoms.TTerm Syntax.Data_Do
data_DoWithBody original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Data_Do"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "expr"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data_Do"),
              Core.projectionField = (Core.Name "expr")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

data_DoWithExpr :: Phantoms.TTerm Syntax.Data_Do -> Phantoms.TTerm Syntax.Data -> Phantoms.TTerm Syntax.Data_Do
data_DoWithExpr original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Data_Do"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data_Do"),
              Core.projectionField = (Core.Name "body")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "expr"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

data_EndMarker :: Phantoms.TTerm Syntax.Data_Name -> Phantoms.TTerm Syntax.Data_EndMarker
data_EndMarker name =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Data_EndMarker"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)}]}))

data_EndMarkerName :: Phantoms.TTerm Syntax.Data_EndMarker -> Phantoms.TTerm Syntax.Data_Name
data_EndMarkerName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data_EndMarker"),
        Core.projectionField = (Core.Name "name")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

data_EndMarkerWithName :: Phantoms.TTerm Syntax.Data_EndMarker -> Phantoms.TTerm Syntax.Data_Name -> Phantoms.TTerm Syntax.Data_EndMarker
data_EndMarkerWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Data_EndMarker"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

data_Eta :: Phantoms.TTerm Syntax.Data -> Phantoms.TTerm Syntax.Data_Eta
data_Eta expr =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Data_Eta"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expr"),
          Core.fieldTerm = (Phantoms.unTTerm expr)}]}))

data_EtaExpr :: Phantoms.TTerm Syntax.Data_Eta -> Phantoms.TTerm Syntax.Data
data_EtaExpr x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data_Eta"),
        Core.projectionField = (Core.Name "expr")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

data_EtaWithExpr :: Phantoms.TTerm Syntax.Data_Eta -> Phantoms.TTerm Syntax.Data -> Phantoms.TTerm Syntax.Data_Eta
data_EtaWithExpr original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Data_Eta"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expr"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

data_For :: Phantoms.TTerm [Syntax.Enumerator] -> Phantoms.TTerm Syntax.Data_For
data_For enums =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Data_For"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "enums"),
          Core.fieldTerm = (Phantoms.unTTerm enums)}]}))

data_ForEnums :: Phantoms.TTerm Syntax.Data_For -> Phantoms.TTerm [Syntax.Enumerator]
data_ForEnums x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data_For"),
        Core.projectionField = (Core.Name "enums")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

data_ForWithEnums :: Phantoms.TTerm Syntax.Data_For -> Phantoms.TTerm [Syntax.Enumerator] -> Phantoms.TTerm Syntax.Data_For
data_ForWithEnums original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Data_For"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "enums"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

data_ForYield :: Phantoms.TTerm [Syntax.Enumerator] -> Phantoms.TTerm Syntax.Data_ForYield
data_ForYield enums =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Data_ForYield"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "enums"),
          Core.fieldTerm = (Phantoms.unTTerm enums)}]}))

data_ForYieldEnums :: Phantoms.TTerm Syntax.Data_ForYield -> Phantoms.TTerm [Syntax.Enumerator]
data_ForYieldEnums x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data_ForYield"),
        Core.projectionField = (Core.Name "enums")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

data_ForYieldWithEnums :: Phantoms.TTerm Syntax.Data_ForYield -> Phantoms.TTerm [Syntax.Enumerator] -> Phantoms.TTerm Syntax.Data_ForYield
data_ForYieldWithEnums original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Data_ForYield"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "enums"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

data_Function :: Phantoms.TTerm [Syntax.Data_Param] -> Phantoms.TTerm Syntax.Data -> Phantoms.TTerm Syntax.Data_Function
data_Function params body =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Data_Function"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Phantoms.unTTerm params)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm body)}]}))

data_FunctionBody :: Phantoms.TTerm Syntax.Data_Function -> Phantoms.TTerm Syntax.Data
data_FunctionBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data_Function"),
        Core.projectionField = (Core.Name "body")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

data_FunctionDataContextFunction :: Phantoms.TTerm Syntax.Data_ContextFunction -> Phantoms.TTerm Syntax.Data_FunctionData
data_FunctionDataContextFunction x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data_FunctionData"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "contextFunction"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

data_FunctionDataFunction :: Phantoms.TTerm Syntax.Data_Function -> Phantoms.TTerm Syntax.Data_FunctionData
data_FunctionDataFunction x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data_FunctionData"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "function"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

data_FunctionParams :: Phantoms.TTerm Syntax.Data_Function -> Phantoms.TTerm [Syntax.Data_Param]
data_FunctionParams x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data_Function"),
        Core.projectionField = (Core.Name "params")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

data_FunctionWithBody :: Phantoms.TTerm Syntax.Data_Function -> Phantoms.TTerm Syntax.Data -> Phantoms.TTerm Syntax.Data_Function
data_FunctionWithBody original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Data_Function"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data_Function"),
              Core.projectionField = (Core.Name "params")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

data_FunctionWithParams :: Phantoms.TTerm Syntax.Data_Function -> Phantoms.TTerm [Syntax.Data_Param] -> Phantoms.TTerm Syntax.Data_Function
data_FunctionWithParams original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Data_Function"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data_Function"),
              Core.projectionField = (Core.Name "body")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

data_If :: Phantoms.TTerm Syntax.Data -> Phantoms.TTerm Syntax.Data -> Phantoms.TTerm Syntax.Data -> Phantoms.TTerm Syntax.Data_If
data_If cond thenp elsep =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Data_If"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "cond"),
          Core.fieldTerm = (Phantoms.unTTerm cond)},
        Core.Field {
          Core.fieldName = (Core.Name "thenp"),
          Core.fieldTerm = (Phantoms.unTTerm thenp)},
        Core.Field {
          Core.fieldName = (Core.Name "elsep"),
          Core.fieldTerm = (Phantoms.unTTerm elsep)}]}))

data_IfCond :: Phantoms.TTerm Syntax.Data_If -> Phantoms.TTerm Syntax.Data
data_IfCond x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data_If"),
        Core.projectionField = (Core.Name "cond")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

data_IfElsep :: Phantoms.TTerm Syntax.Data_If -> Phantoms.TTerm Syntax.Data
data_IfElsep x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data_If"),
        Core.projectionField = (Core.Name "elsep")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

data_IfThenp :: Phantoms.TTerm Syntax.Data_If -> Phantoms.TTerm Syntax.Data
data_IfThenp x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data_If"),
        Core.projectionField = (Core.Name "thenp")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

data_IfWithCond :: Phantoms.TTerm Syntax.Data_If -> Phantoms.TTerm Syntax.Data -> Phantoms.TTerm Syntax.Data_If
data_IfWithCond original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Data_If"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "cond"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "thenp"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data_If"),
              Core.projectionField = (Core.Name "thenp")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "elsep"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data_If"),
              Core.projectionField = (Core.Name "elsep")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

data_IfWithElsep :: Phantoms.TTerm Syntax.Data_If -> Phantoms.TTerm Syntax.Data -> Phantoms.TTerm Syntax.Data_If
data_IfWithElsep original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Data_If"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "cond"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data_If"),
              Core.projectionField = (Core.Name "cond")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "thenp"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data_If"),
              Core.projectionField = (Core.Name "thenp")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "elsep"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

data_IfWithThenp :: Phantoms.TTerm Syntax.Data_If -> Phantoms.TTerm Syntax.Data -> Phantoms.TTerm Syntax.Data_If
data_IfWithThenp original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Data_If"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "cond"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data_If"),
              Core.projectionField = (Core.Name "cond")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "thenp"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "elsep"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data_If"),
              Core.projectionField = (Core.Name "elsep")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

data_Interpolate :: Phantoms.TTerm Syntax.Data_Name -> Phantoms.TTerm [Syntax.Lit] -> Phantoms.TTerm [Syntax.Data] -> Phantoms.TTerm Syntax.Data_Interpolate
data_Interpolate prefix parts args =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Data_Interpolate"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "prefix"),
          Core.fieldTerm = (Phantoms.unTTerm prefix)},
        Core.Field {
          Core.fieldName = (Core.Name "parts"),
          Core.fieldTerm = (Phantoms.unTTerm parts)},
        Core.Field {
          Core.fieldName = (Core.Name "args"),
          Core.fieldTerm = (Phantoms.unTTerm args)}]}))

data_InterpolateArgs :: Phantoms.TTerm Syntax.Data_Interpolate -> Phantoms.TTerm [Syntax.Data]
data_InterpolateArgs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data_Interpolate"),
        Core.projectionField = (Core.Name "args")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

data_InterpolateParts :: Phantoms.TTerm Syntax.Data_Interpolate -> Phantoms.TTerm [Syntax.Lit]
data_InterpolateParts x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data_Interpolate"),
        Core.projectionField = (Core.Name "parts")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

data_InterpolatePrefix :: Phantoms.TTerm Syntax.Data_Interpolate -> Phantoms.TTerm Syntax.Data_Name
data_InterpolatePrefix x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data_Interpolate"),
        Core.projectionField = (Core.Name "prefix")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

data_InterpolateWithArgs :: Phantoms.TTerm Syntax.Data_Interpolate -> Phantoms.TTerm [Syntax.Data] -> Phantoms.TTerm Syntax.Data_Interpolate
data_InterpolateWithArgs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Data_Interpolate"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "prefix"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data_Interpolate"),
              Core.projectionField = (Core.Name "prefix")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "parts"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data_Interpolate"),
              Core.projectionField = (Core.Name "parts")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "args"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

data_InterpolateWithParts :: Phantoms.TTerm Syntax.Data_Interpolate -> Phantoms.TTerm [Syntax.Lit] -> Phantoms.TTerm Syntax.Data_Interpolate
data_InterpolateWithParts original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Data_Interpolate"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "prefix"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data_Interpolate"),
              Core.projectionField = (Core.Name "prefix")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "parts"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "args"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data_Interpolate"),
              Core.projectionField = (Core.Name "args")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

data_InterpolateWithPrefix :: Phantoms.TTerm Syntax.Data_Interpolate -> Phantoms.TTerm Syntax.Data_Name -> Phantoms.TTerm Syntax.Data_Interpolate
data_InterpolateWithPrefix original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Data_Interpolate"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "prefix"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "parts"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data_Interpolate"),
              Core.projectionField = (Core.Name "parts")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "args"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data_Interpolate"),
              Core.projectionField = (Core.Name "args")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

data_Match :: Phantoms.TTerm Syntax.Data -> Phantoms.TTerm [Syntax.Case] -> Phantoms.TTerm Syntax.Data_Match
data_Match expr cases =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Data_Match"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expr"),
          Core.fieldTerm = (Phantoms.unTTerm expr)},
        Core.Field {
          Core.fieldName = (Core.Name "cases"),
          Core.fieldTerm = (Phantoms.unTTerm cases)}]}))

data_MatchCases :: Phantoms.TTerm Syntax.Data_Match -> Phantoms.TTerm [Syntax.Case]
data_MatchCases x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data_Match"),
        Core.projectionField = (Core.Name "cases")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

data_MatchExpr :: Phantoms.TTerm Syntax.Data_Match -> Phantoms.TTerm Syntax.Data
data_MatchExpr x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data_Match"),
        Core.projectionField = (Core.Name "expr")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

data_MatchWithCases :: Phantoms.TTerm Syntax.Data_Match -> Phantoms.TTerm [Syntax.Case] -> Phantoms.TTerm Syntax.Data_Match
data_MatchWithCases original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Data_Match"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expr"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data_Match"),
              Core.projectionField = (Core.Name "expr")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "cases"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

data_MatchWithExpr :: Phantoms.TTerm Syntax.Data_Match -> Phantoms.TTerm Syntax.Data -> Phantoms.TTerm Syntax.Data_Match
data_MatchWithExpr original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Data_Match"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expr"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "cases"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data_Match"),
              Core.projectionField = (Core.Name "cases")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

data_Name :: Phantoms.TTerm Syntax.PredefString -> Phantoms.TTerm Syntax.Data_Name
data_Name value =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Data_Name"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm value)}]}))

data_NameValue :: Phantoms.TTerm Syntax.Data_Name -> Phantoms.TTerm Syntax.PredefString
data_NameValue x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data_Name"),
        Core.projectionField = (Core.Name "value")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

data_NameWithValue :: Phantoms.TTerm Syntax.Data_Name -> Phantoms.TTerm Syntax.PredefString -> Phantoms.TTerm Syntax.Data_Name
data_NameWithValue original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Data_Name"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

data_New :: Phantoms.TTerm Syntax.Init -> Phantoms.TTerm Syntax.Data_New
data_New init =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Data_New"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "init"),
          Core.fieldTerm = (Phantoms.unTTerm init)}]}))

data_NewAnonymous :: Phantoms.TTerm Syntax.Template -> Phantoms.TTerm Syntax.Data_NewAnonymous
data_NewAnonymous templ =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Data_NewAnonymous"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "templ"),
          Core.fieldTerm = (Phantoms.unTTerm templ)}]}))

data_NewAnonymousTempl :: Phantoms.TTerm Syntax.Data_NewAnonymous -> Phantoms.TTerm Syntax.Template
data_NewAnonymousTempl x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data_NewAnonymous"),
        Core.projectionField = (Core.Name "templ")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

data_NewAnonymousWithTempl :: Phantoms.TTerm Syntax.Data_NewAnonymous -> Phantoms.TTerm Syntax.Template -> Phantoms.TTerm Syntax.Data_NewAnonymous
data_NewAnonymousWithTempl original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Data_NewAnonymous"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "templ"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

data_NewInit :: Phantoms.TTerm Syntax.Data_New -> Phantoms.TTerm Syntax.Init
data_NewInit x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data_New"),
        Core.projectionField = (Core.Name "init")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

data_NewWithInit :: Phantoms.TTerm Syntax.Data_New -> Phantoms.TTerm Syntax.Init -> Phantoms.TTerm Syntax.Data_New
data_NewWithInit original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Data_New"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "init"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

data_Param :: Phantoms.TTerm [Syntax.Mod] -> Phantoms.TTerm Syntax.Name -> Phantoms.TTerm (Maybe Syntax.Type) -> Phantoms.TTerm (Maybe Syntax.Data) -> Phantoms.TTerm Syntax.Data_Param
data_Param mods name decltpe default_ =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Data_Param"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Phantoms.unTTerm mods)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Phantoms.unTTerm decltpe)},
        Core.Field {
          Core.fieldName = (Core.Name "default"),
          Core.fieldTerm = (Phantoms.unTTerm default_)}]}))

data_ParamDecltpe :: Phantoms.TTerm Syntax.Data_Param -> Phantoms.TTerm (Maybe Syntax.Type)
data_ParamDecltpe x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data_Param"),
        Core.projectionField = (Core.Name "decltpe")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

data_ParamDefault :: Phantoms.TTerm Syntax.Data_Param -> Phantoms.TTerm (Maybe Syntax.Data)
data_ParamDefault x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data_Param"),
        Core.projectionField = (Core.Name "default")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

data_ParamMods :: Phantoms.TTerm Syntax.Data_Param -> Phantoms.TTerm [Syntax.Mod]
data_ParamMods x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data_Param"),
        Core.projectionField = (Core.Name "mods")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

data_ParamName :: Phantoms.TTerm Syntax.Data_Param -> Phantoms.TTerm Syntax.Name
data_ParamName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data_Param"),
        Core.projectionField = (Core.Name "name")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

data_ParamWithDecltpe :: Phantoms.TTerm Syntax.Data_Param -> Phantoms.TTerm (Maybe Syntax.Type) -> Phantoms.TTerm Syntax.Data_Param
data_ParamWithDecltpe original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Data_Param"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data_Param"),
              Core.projectionField = (Core.Name "mods")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data_Param"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "default"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data_Param"),
              Core.projectionField = (Core.Name "default")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

data_ParamWithDefault :: Phantoms.TTerm Syntax.Data_Param -> Phantoms.TTerm (Maybe Syntax.Data) -> Phantoms.TTerm Syntax.Data_Param
data_ParamWithDefault original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Data_Param"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data_Param"),
              Core.projectionField = (Core.Name "mods")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data_Param"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data_Param"),
              Core.projectionField = (Core.Name "decltpe")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "default"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

data_ParamWithMods :: Phantoms.TTerm Syntax.Data_Param -> Phantoms.TTerm [Syntax.Mod] -> Phantoms.TTerm Syntax.Data_Param
data_ParamWithMods original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Data_Param"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data_Param"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data_Param"),
              Core.projectionField = (Core.Name "decltpe")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "default"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data_Param"),
              Core.projectionField = (Core.Name "default")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

data_ParamWithName :: Phantoms.TTerm Syntax.Data_Param -> Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.Data_Param
data_ParamWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Data_Param"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data_Param"),
              Core.projectionField = (Core.Name "mods")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data_Param"),
              Core.projectionField = (Core.Name "decltpe")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "default"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data_Param"),
              Core.projectionField = (Core.Name "default")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

data_PartialFunction :: Phantoms.TTerm [Syntax.Case] -> Phantoms.TTerm Syntax.Data_PartialFunction
data_PartialFunction cases =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Data_PartialFunction"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "cases"),
          Core.fieldTerm = (Phantoms.unTTerm cases)}]}))

data_PartialFunctionCases :: Phantoms.TTerm Syntax.Data_PartialFunction -> Phantoms.TTerm [Syntax.Case]
data_PartialFunctionCases x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data_PartialFunction"),
        Core.projectionField = (Core.Name "cases")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

data_PartialFunctionWithCases :: Phantoms.TTerm Syntax.Data_PartialFunction -> Phantoms.TTerm [Syntax.Case] -> Phantoms.TTerm Syntax.Data_PartialFunction
data_PartialFunctionWithCases original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Data_PartialFunction"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "cases"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

data_PolyFunction :: Phantoms.TTerm [Syntax.Type_Param] -> Phantoms.TTerm Syntax.Data -> Phantoms.TTerm Syntax.Data_PolyFunction
data_PolyFunction tparams body =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Data_PolyFunction"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Phantoms.unTTerm tparams)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm body)}]}))

data_PolyFunctionBody :: Phantoms.TTerm Syntax.Data_PolyFunction -> Phantoms.TTerm Syntax.Data
data_PolyFunctionBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data_PolyFunction"),
        Core.projectionField = (Core.Name "body")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

data_PolyFunctionTparams :: Phantoms.TTerm Syntax.Data_PolyFunction -> Phantoms.TTerm [Syntax.Type_Param]
data_PolyFunctionTparams x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data_PolyFunction"),
        Core.projectionField = (Core.Name "tparams")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

data_PolyFunctionWithBody :: Phantoms.TTerm Syntax.Data_PolyFunction -> Phantoms.TTerm Syntax.Data -> Phantoms.TTerm Syntax.Data_PolyFunction
data_PolyFunctionWithBody original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Data_PolyFunction"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data_PolyFunction"),
              Core.projectionField = (Core.Name "tparams")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

data_PolyFunctionWithTparams :: Phantoms.TTerm Syntax.Data_PolyFunction -> Phantoms.TTerm [Syntax.Type_Param] -> Phantoms.TTerm Syntax.Data_PolyFunction
data_PolyFunctionWithTparams original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Data_PolyFunction"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data_PolyFunction"),
              Core.projectionField = (Core.Name "body")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

data_QuotedMacroExpr :: Phantoms.TTerm Syntax.Data -> Phantoms.TTerm Syntax.Data_QuotedMacroExpr
data_QuotedMacroExpr body =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Data_QuotedMacroExpr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm body)}]}))

data_QuotedMacroExprBody :: Phantoms.TTerm Syntax.Data_QuotedMacroExpr -> Phantoms.TTerm Syntax.Data
data_QuotedMacroExprBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data_QuotedMacroExpr"),
        Core.projectionField = (Core.Name "body")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

data_QuotedMacroExprWithBody :: Phantoms.TTerm Syntax.Data_QuotedMacroExpr -> Phantoms.TTerm Syntax.Data -> Phantoms.TTerm Syntax.Data_QuotedMacroExpr
data_QuotedMacroExprWithBody original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Data_QuotedMacroExpr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

data_QuotedMacroType :: Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.Data_QuotedMacroType
data_QuotedMacroType tpe =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Data_QuotedMacroType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "tpe"),
          Core.fieldTerm = (Phantoms.unTTerm tpe)}]}))

data_QuotedMacroTypeTpe :: Phantoms.TTerm Syntax.Data_QuotedMacroType -> Phantoms.TTerm Syntax.Type
data_QuotedMacroTypeTpe x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data_QuotedMacroType"),
        Core.projectionField = (Core.Name "tpe")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

data_QuotedMacroTypeWithTpe :: Phantoms.TTerm Syntax.Data_QuotedMacroType -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.Data_QuotedMacroType
data_QuotedMacroTypeWithTpe original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Data_QuotedMacroType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "tpe"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

data_RefAnonymous :: Phantoms.TTerm Syntax.Data_Anonymous -> Phantoms.TTerm Syntax.Data_Ref
data_RefAnonymous x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data_Ref"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "anonymous"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

data_RefApplyUnary :: Phantoms.TTerm Syntax.Data_ApplyUnary -> Phantoms.TTerm Syntax.Data_Ref
data_RefApplyUnary x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data_Ref"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "applyUnary"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

data_RefName :: Phantoms.TTerm Syntax.Data_Name -> Phantoms.TTerm Syntax.Data_Ref
data_RefName x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data_Ref"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "name"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

data_RefSelect :: Phantoms.TTerm Syntax.Data_Select -> Phantoms.TTerm Syntax.Data_Ref
data_RefSelect x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data_Ref"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "select"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

data_RefSuper :: Phantoms.TTerm Syntax.Data_Super -> Phantoms.TTerm Syntax.Data_Ref
data_RefSuper x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data_Ref"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "super"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

data_RefThis :: Phantoms.TTerm Syntax.Data_This -> Phantoms.TTerm Syntax.Data_Ref
data_RefThis x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data_Ref"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "this"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

data_Repeated :: Phantoms.TTerm Syntax.Data -> Phantoms.TTerm Syntax.Data_Repeated
data_Repeated expr =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Data_Repeated"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expr"),
          Core.fieldTerm = (Phantoms.unTTerm expr)}]}))

data_RepeatedExpr :: Phantoms.TTerm Syntax.Data_Repeated -> Phantoms.TTerm Syntax.Data
data_RepeatedExpr x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data_Repeated"),
        Core.projectionField = (Core.Name "expr")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

data_RepeatedWithExpr :: Phantoms.TTerm Syntax.Data_Repeated -> Phantoms.TTerm Syntax.Data -> Phantoms.TTerm Syntax.Data_Repeated
data_RepeatedWithExpr original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Data_Repeated"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expr"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

data_Return :: Phantoms.TTerm Syntax.Data -> Phantoms.TTerm Syntax.Data_Return
data_Return expr =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Data_Return"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expr"),
          Core.fieldTerm = (Phantoms.unTTerm expr)}]}))

data_ReturnExpr :: Phantoms.TTerm Syntax.Data_Return -> Phantoms.TTerm Syntax.Data
data_ReturnExpr x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data_Return"),
        Core.projectionField = (Core.Name "expr")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

data_ReturnWithExpr :: Phantoms.TTerm Syntax.Data_Return -> Phantoms.TTerm Syntax.Data -> Phantoms.TTerm Syntax.Data_Return
data_ReturnWithExpr original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Data_Return"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expr"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

data_Select :: Phantoms.TTerm Syntax.Data -> Phantoms.TTerm Syntax.Data_Name -> Phantoms.TTerm Syntax.Data_Select
data_Select qual name =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Data_Select"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "qual"),
          Core.fieldTerm = (Phantoms.unTTerm qual)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)}]}))

data_SelectName :: Phantoms.TTerm Syntax.Data_Select -> Phantoms.TTerm Syntax.Data_Name
data_SelectName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data_Select"),
        Core.projectionField = (Core.Name "name")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

data_SelectQual :: Phantoms.TTerm Syntax.Data_Select -> Phantoms.TTerm Syntax.Data
data_SelectQual x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data_Select"),
        Core.projectionField = (Core.Name "qual")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

data_SelectWithName :: Phantoms.TTerm Syntax.Data_Select -> Phantoms.TTerm Syntax.Data_Name -> Phantoms.TTerm Syntax.Data_Select
data_SelectWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Data_Select"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "qual"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data_Select"),
              Core.projectionField = (Core.Name "qual")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

data_SelectWithQual :: Phantoms.TTerm Syntax.Data_Select -> Phantoms.TTerm Syntax.Data -> Phantoms.TTerm Syntax.Data_Select
data_SelectWithQual original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Data_Select"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "qual"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data_Select"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

data_SplicedMacroExpr :: Phantoms.TTerm Syntax.Data -> Phantoms.TTerm Syntax.Data_SplicedMacroExpr
data_SplicedMacroExpr body =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Data_SplicedMacroExpr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm body)}]}))

data_SplicedMacroExprBody :: Phantoms.TTerm Syntax.Data_SplicedMacroExpr -> Phantoms.TTerm Syntax.Data
data_SplicedMacroExprBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data_SplicedMacroExpr"),
        Core.projectionField = (Core.Name "body")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

data_SplicedMacroExprWithBody :: Phantoms.TTerm Syntax.Data_SplicedMacroExpr -> Phantoms.TTerm Syntax.Data -> Phantoms.TTerm Syntax.Data_SplicedMacroExpr
data_SplicedMacroExprWithBody original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Data_SplicedMacroExpr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

data_Super :: Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.Data_Super
data_Super thisp superp =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Data_Super"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "thisp"),
          Core.fieldTerm = (Phantoms.unTTerm thisp)},
        Core.Field {
          Core.fieldName = (Core.Name "superp"),
          Core.fieldTerm = (Phantoms.unTTerm superp)}]}))

data_SuperSuperp :: Phantoms.TTerm Syntax.Data_Super -> Phantoms.TTerm Syntax.Name
data_SuperSuperp x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data_Super"),
        Core.projectionField = (Core.Name "superp")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

data_SuperThisp :: Phantoms.TTerm Syntax.Data_Super -> Phantoms.TTerm Syntax.Name
data_SuperThisp x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data_Super"),
        Core.projectionField = (Core.Name "thisp")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

data_SuperWithSuperp :: Phantoms.TTerm Syntax.Data_Super -> Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.Data_Super
data_SuperWithSuperp original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Data_Super"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "thisp"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data_Super"),
              Core.projectionField = (Core.Name "thisp")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "superp"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

data_SuperWithThisp :: Phantoms.TTerm Syntax.Data_Super -> Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.Data_Super
data_SuperWithThisp original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Data_Super"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "thisp"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "superp"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data_Super"),
              Core.projectionField = (Core.Name "superp")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

data_This :: Phantoms.TTerm () -> Phantoms.TTerm Syntax.Data_This
data_This x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.scala.syntax.Data_This"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

data_Throw :: Phantoms.TTerm Syntax.Data -> Phantoms.TTerm Syntax.Data_Throw
data_Throw expr =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Data_Throw"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expr"),
          Core.fieldTerm = (Phantoms.unTTerm expr)}]}))

data_ThrowExpr :: Phantoms.TTerm Syntax.Data_Throw -> Phantoms.TTerm Syntax.Data
data_ThrowExpr x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data_Throw"),
        Core.projectionField = (Core.Name "expr")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

data_ThrowWithExpr :: Phantoms.TTerm Syntax.Data_Throw -> Phantoms.TTerm Syntax.Data -> Phantoms.TTerm Syntax.Data_Throw
data_ThrowWithExpr original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Data_Throw"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expr"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

data_Try :: Phantoms.TTerm Syntax.Data -> Phantoms.TTerm [Syntax.Case] -> Phantoms.TTerm (Maybe Syntax.Data) -> Phantoms.TTerm Syntax.Data_Try
data_Try expr catchp finallyp =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Data_Try"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expr"),
          Core.fieldTerm = (Phantoms.unTTerm expr)},
        Core.Field {
          Core.fieldName = (Core.Name "catchp"),
          Core.fieldTerm = (Phantoms.unTTerm catchp)},
        Core.Field {
          Core.fieldName = (Core.Name "finallyp"),
          Core.fieldTerm = (Phantoms.unTTerm finallyp)}]}))

data_TryCatchp :: Phantoms.TTerm Syntax.Data_Try -> Phantoms.TTerm [Syntax.Case]
data_TryCatchp x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data_Try"),
        Core.projectionField = (Core.Name "catchp")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

data_TryExpr :: Phantoms.TTerm Syntax.Data_Try -> Phantoms.TTerm Syntax.Data
data_TryExpr x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data_Try"),
        Core.projectionField = (Core.Name "expr")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

data_TryFinallyp :: Phantoms.TTerm Syntax.Data_Try -> Phantoms.TTerm (Maybe Syntax.Data)
data_TryFinallyp x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data_Try"),
        Core.projectionField = (Core.Name "finallyp")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

data_TryWithCatchp :: Phantoms.TTerm Syntax.Data_Try -> Phantoms.TTerm [Syntax.Case] -> Phantoms.TTerm Syntax.Data_Try
data_TryWithCatchp original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Data_Try"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expr"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data_Try"),
              Core.projectionField = (Core.Name "expr")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "catchp"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "finallyp"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data_Try"),
              Core.projectionField = (Core.Name "finallyp")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

data_TryWithExpr :: Phantoms.TTerm Syntax.Data_Try -> Phantoms.TTerm Syntax.Data -> Phantoms.TTerm Syntax.Data_Try
data_TryWithExpr original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Data_Try"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expr"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "catchp"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data_Try"),
              Core.projectionField = (Core.Name "catchp")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "finallyp"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data_Try"),
              Core.projectionField = (Core.Name "finallyp")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

data_TryWithFinallyp :: Phantoms.TTerm Syntax.Data_Try -> Phantoms.TTerm (Maybe Syntax.Data) -> Phantoms.TTerm Syntax.Data_Try
data_TryWithFinallyp original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Data_Try"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expr"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data_Try"),
              Core.projectionField = (Core.Name "expr")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "catchp"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data_Try"),
              Core.projectionField = (Core.Name "catchp")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "finallyp"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

data_TryWithHandler :: Phantoms.TTerm Syntax.Data -> Phantoms.TTerm Syntax.Data -> Phantoms.TTerm (Maybe Syntax.Data) -> Phantoms.TTerm Syntax.Data_TryWithHandler
data_TryWithHandler expr catchp finallyp =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Data_TryWithHandler"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expr"),
          Core.fieldTerm = (Phantoms.unTTerm expr)},
        Core.Field {
          Core.fieldName = (Core.Name "catchp"),
          Core.fieldTerm = (Phantoms.unTTerm catchp)},
        Core.Field {
          Core.fieldName = (Core.Name "finallyp"),
          Core.fieldTerm = (Phantoms.unTTerm finallyp)}]}))

data_TryWithHandlerCatchp :: Phantoms.TTerm Syntax.Data_TryWithHandler -> Phantoms.TTerm Syntax.Data
data_TryWithHandlerCatchp x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data_TryWithHandler"),
        Core.projectionField = (Core.Name "catchp")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

data_TryWithHandlerExpr :: Phantoms.TTerm Syntax.Data_TryWithHandler -> Phantoms.TTerm Syntax.Data
data_TryWithHandlerExpr x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data_TryWithHandler"),
        Core.projectionField = (Core.Name "expr")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

data_TryWithHandlerFinallyp :: Phantoms.TTerm Syntax.Data_TryWithHandler -> Phantoms.TTerm (Maybe Syntax.Data)
data_TryWithHandlerFinallyp x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data_TryWithHandler"),
        Core.projectionField = (Core.Name "finallyp")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

data_TryWithHandlerWithCatchp :: Phantoms.TTerm Syntax.Data_TryWithHandler -> Phantoms.TTerm Syntax.Data -> Phantoms.TTerm Syntax.Data_TryWithHandler
data_TryWithHandlerWithCatchp original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Data_TryWithHandler"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expr"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data_TryWithHandler"),
              Core.projectionField = (Core.Name "expr")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "catchp"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "finallyp"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data_TryWithHandler"),
              Core.projectionField = (Core.Name "finallyp")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

data_TryWithHandlerWithExpr :: Phantoms.TTerm Syntax.Data_TryWithHandler -> Phantoms.TTerm Syntax.Data -> Phantoms.TTerm Syntax.Data_TryWithHandler
data_TryWithHandlerWithExpr original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Data_TryWithHandler"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expr"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "catchp"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data_TryWithHandler"),
              Core.projectionField = (Core.Name "catchp")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "finallyp"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data_TryWithHandler"),
              Core.projectionField = (Core.Name "finallyp")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

data_TryWithHandlerWithFinallyp :: Phantoms.TTerm Syntax.Data_TryWithHandler -> Phantoms.TTerm (Maybe Syntax.Data) -> Phantoms.TTerm Syntax.Data_TryWithHandler
data_TryWithHandlerWithFinallyp original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Data_TryWithHandler"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expr"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data_TryWithHandler"),
              Core.projectionField = (Core.Name "expr")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "catchp"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data_TryWithHandler"),
              Core.projectionField = (Core.Name "catchp")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "finallyp"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

data_Tuple :: Phantoms.TTerm [Syntax.Data] -> Phantoms.TTerm Syntax.Data_Tuple
data_Tuple args =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Data_Tuple"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "args"),
          Core.fieldTerm = (Phantoms.unTTerm args)}]}))

data_TupleArgs :: Phantoms.TTerm Syntax.Data_Tuple -> Phantoms.TTerm [Syntax.Data]
data_TupleArgs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data_Tuple"),
        Core.projectionField = (Core.Name "args")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

data_TupleWithArgs :: Phantoms.TTerm Syntax.Data_Tuple -> Phantoms.TTerm [Syntax.Data] -> Phantoms.TTerm Syntax.Data_Tuple
data_TupleWithArgs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Data_Tuple"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "args"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

data_While :: Phantoms.TTerm Syntax.Data -> Phantoms.TTerm Syntax.Data -> Phantoms.TTerm Syntax.Data_While
data_While expr body =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Data_While"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expr"),
          Core.fieldTerm = (Phantoms.unTTerm expr)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm body)}]}))

data_WhileBody :: Phantoms.TTerm Syntax.Data_While -> Phantoms.TTerm Syntax.Data
data_WhileBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data_While"),
        Core.projectionField = (Core.Name "body")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

data_WhileExpr :: Phantoms.TTerm Syntax.Data_While -> Phantoms.TTerm Syntax.Data
data_WhileExpr x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data_While"),
        Core.projectionField = (Core.Name "expr")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

data_WhileWithBody :: Phantoms.TTerm Syntax.Data_While -> Phantoms.TTerm Syntax.Data -> Phantoms.TTerm Syntax.Data_While
data_WhileWithBody original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Data_While"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expr"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data_While"),
              Core.projectionField = (Core.Name "expr")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

data_WhileWithExpr :: Phantoms.TTerm Syntax.Data_While -> Phantoms.TTerm Syntax.Data -> Phantoms.TTerm Syntax.Data_While
data_WhileWithExpr original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Data_While"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expr"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data_While"),
              Core.projectionField = (Core.Name "body")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

data_Xml :: Phantoms.TTerm [Syntax.Lit] -> Phantoms.TTerm [Syntax.Data] -> Phantoms.TTerm Syntax.Data_Xml
data_Xml parts args =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Data_Xml"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "parts"),
          Core.fieldTerm = (Phantoms.unTTerm parts)},
        Core.Field {
          Core.fieldName = (Core.Name "args"),
          Core.fieldTerm = (Phantoms.unTTerm args)}]}))

data_XmlArgs :: Phantoms.TTerm Syntax.Data_Xml -> Phantoms.TTerm [Syntax.Data]
data_XmlArgs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data_Xml"),
        Core.projectionField = (Core.Name "args")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

data_XmlParts :: Phantoms.TTerm Syntax.Data_Xml -> Phantoms.TTerm [Syntax.Lit]
data_XmlParts x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data_Xml"),
        Core.projectionField = (Core.Name "parts")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

data_XmlWithArgs :: Phantoms.TTerm Syntax.Data_Xml -> Phantoms.TTerm [Syntax.Data] -> Phantoms.TTerm Syntax.Data_Xml
data_XmlWithArgs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Data_Xml"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "parts"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data_Xml"),
              Core.projectionField = (Core.Name "parts")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "args"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

data_XmlWithParts :: Phantoms.TTerm Syntax.Data_Xml -> Phantoms.TTerm [Syntax.Lit] -> Phantoms.TTerm Syntax.Data_Xml
data_XmlWithParts original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Data_Xml"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "parts"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "args"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Data_Xml"),
              Core.projectionField = (Core.Name "args")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

declDef :: Phantoms.TTerm Syntax.Decl_Def -> Phantoms.TTerm Syntax.Decl
declDef x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Decl"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "def"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

declGiven :: Phantoms.TTerm Syntax.Decl_Given -> Phantoms.TTerm Syntax.Decl
declGiven x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Decl"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "given"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

declType :: Phantoms.TTerm Syntax.Decl_Type -> Phantoms.TTerm Syntax.Decl
declType x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Decl"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "type"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

declVal :: Phantoms.TTerm Syntax.Decl_Val -> Phantoms.TTerm Syntax.Decl
declVal x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Decl"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "val"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

declVar :: Phantoms.TTerm Syntax.Decl_Var -> Phantoms.TTerm Syntax.Decl
declVar x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Decl"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "var"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

decl_Def :: Phantoms.TTerm [Syntax.Mod] -> Phantoms.TTerm Syntax.Data_Name -> Phantoms.TTerm [Syntax.Type_Param] -> Phantoms.TTerm [[Syntax.Data_Param]] -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.Decl_Def
decl_Def mods name tparams paramss decltpe =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Decl_Def"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Phantoms.unTTerm mods)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Phantoms.unTTerm tparams)},
        Core.Field {
          Core.fieldName = (Core.Name "paramss"),
          Core.fieldTerm = (Phantoms.unTTerm paramss)},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Phantoms.unTTerm decltpe)}]}))

decl_DefDecltpe :: Phantoms.TTerm Syntax.Decl_Def -> Phantoms.TTerm Syntax.Type
decl_DefDecltpe x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Decl_Def"),
        Core.projectionField = (Core.Name "decltpe")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

decl_DefMods :: Phantoms.TTerm Syntax.Decl_Def -> Phantoms.TTerm [Syntax.Mod]
decl_DefMods x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Decl_Def"),
        Core.projectionField = (Core.Name "mods")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

decl_DefName :: Phantoms.TTerm Syntax.Decl_Def -> Phantoms.TTerm Syntax.Data_Name
decl_DefName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Decl_Def"),
        Core.projectionField = (Core.Name "name")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

decl_DefParamss :: Phantoms.TTerm Syntax.Decl_Def -> Phantoms.TTerm [[Syntax.Data_Param]]
decl_DefParamss x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Decl_Def"),
        Core.projectionField = (Core.Name "paramss")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

decl_DefTparams :: Phantoms.TTerm Syntax.Decl_Def -> Phantoms.TTerm [Syntax.Type_Param]
decl_DefTparams x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Decl_Def"),
        Core.projectionField = (Core.Name "tparams")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

decl_DefWithDecltpe :: Phantoms.TTerm Syntax.Decl_Def -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.Decl_Def
decl_DefWithDecltpe original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Decl_Def"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Decl_Def"),
              Core.projectionField = (Core.Name "mods")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Decl_Def"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Decl_Def"),
              Core.projectionField = (Core.Name "tparams")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "paramss"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Decl_Def"),
              Core.projectionField = (Core.Name "paramss")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

decl_DefWithMods :: Phantoms.TTerm Syntax.Decl_Def -> Phantoms.TTerm [Syntax.Mod] -> Phantoms.TTerm Syntax.Decl_Def
decl_DefWithMods original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Decl_Def"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Decl_Def"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Decl_Def"),
              Core.projectionField = (Core.Name "tparams")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "paramss"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Decl_Def"),
              Core.projectionField = (Core.Name "paramss")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Decl_Def"),
              Core.projectionField = (Core.Name "decltpe")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

decl_DefWithName :: Phantoms.TTerm Syntax.Decl_Def -> Phantoms.TTerm Syntax.Data_Name -> Phantoms.TTerm Syntax.Decl_Def
decl_DefWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Decl_Def"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Decl_Def"),
              Core.projectionField = (Core.Name "mods")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Decl_Def"),
              Core.projectionField = (Core.Name "tparams")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "paramss"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Decl_Def"),
              Core.projectionField = (Core.Name "paramss")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Decl_Def"),
              Core.projectionField = (Core.Name "decltpe")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

decl_DefWithParamss :: Phantoms.TTerm Syntax.Decl_Def -> Phantoms.TTerm [[Syntax.Data_Param]] -> Phantoms.TTerm Syntax.Decl_Def
decl_DefWithParamss original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Decl_Def"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Decl_Def"),
              Core.projectionField = (Core.Name "mods")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Decl_Def"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Decl_Def"),
              Core.projectionField = (Core.Name "tparams")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "paramss"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Decl_Def"),
              Core.projectionField = (Core.Name "decltpe")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

decl_DefWithTparams :: Phantoms.TTerm Syntax.Decl_Def -> Phantoms.TTerm [Syntax.Type_Param] -> Phantoms.TTerm Syntax.Decl_Def
decl_DefWithTparams original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Decl_Def"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Decl_Def"),
              Core.projectionField = (Core.Name "mods")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Decl_Def"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "paramss"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Decl_Def"),
              Core.projectionField = (Core.Name "paramss")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Decl_Def"),
              Core.projectionField = (Core.Name "decltpe")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

decl_Given :: Phantoms.TTerm [Syntax.Mod] -> Phantoms.TTerm Syntax.Data_Name -> Phantoms.TTerm [Syntax.Type_Param] -> Phantoms.TTerm [[Syntax.Data_Param]] -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.Decl_Given
decl_Given mods name tparams sparams decltpe =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Decl_Given"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Phantoms.unTTerm mods)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Phantoms.unTTerm tparams)},
        Core.Field {
          Core.fieldName = (Core.Name "sparams"),
          Core.fieldTerm = (Phantoms.unTTerm sparams)},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Phantoms.unTTerm decltpe)}]}))

decl_GivenDecltpe :: Phantoms.TTerm Syntax.Decl_Given -> Phantoms.TTerm Syntax.Type
decl_GivenDecltpe x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Decl_Given"),
        Core.projectionField = (Core.Name "decltpe")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

decl_GivenMods :: Phantoms.TTerm Syntax.Decl_Given -> Phantoms.TTerm [Syntax.Mod]
decl_GivenMods x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Decl_Given"),
        Core.projectionField = (Core.Name "mods")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

decl_GivenName :: Phantoms.TTerm Syntax.Decl_Given -> Phantoms.TTerm Syntax.Data_Name
decl_GivenName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Decl_Given"),
        Core.projectionField = (Core.Name "name")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

decl_GivenSparams :: Phantoms.TTerm Syntax.Decl_Given -> Phantoms.TTerm [[Syntax.Data_Param]]
decl_GivenSparams x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Decl_Given"),
        Core.projectionField = (Core.Name "sparams")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

decl_GivenTparams :: Phantoms.TTerm Syntax.Decl_Given -> Phantoms.TTerm [Syntax.Type_Param]
decl_GivenTparams x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Decl_Given"),
        Core.projectionField = (Core.Name "tparams")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

decl_GivenWithDecltpe :: Phantoms.TTerm Syntax.Decl_Given -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.Decl_Given
decl_GivenWithDecltpe original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Decl_Given"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Decl_Given"),
              Core.projectionField = (Core.Name "mods")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Decl_Given"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Decl_Given"),
              Core.projectionField = (Core.Name "tparams")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "sparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Decl_Given"),
              Core.projectionField = (Core.Name "sparams")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

decl_GivenWithMods :: Phantoms.TTerm Syntax.Decl_Given -> Phantoms.TTerm [Syntax.Mod] -> Phantoms.TTerm Syntax.Decl_Given
decl_GivenWithMods original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Decl_Given"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Decl_Given"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Decl_Given"),
              Core.projectionField = (Core.Name "tparams")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "sparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Decl_Given"),
              Core.projectionField = (Core.Name "sparams")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Decl_Given"),
              Core.projectionField = (Core.Name "decltpe")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

decl_GivenWithName :: Phantoms.TTerm Syntax.Decl_Given -> Phantoms.TTerm Syntax.Data_Name -> Phantoms.TTerm Syntax.Decl_Given
decl_GivenWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Decl_Given"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Decl_Given"),
              Core.projectionField = (Core.Name "mods")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Decl_Given"),
              Core.projectionField = (Core.Name "tparams")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "sparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Decl_Given"),
              Core.projectionField = (Core.Name "sparams")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Decl_Given"),
              Core.projectionField = (Core.Name "decltpe")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

decl_GivenWithSparams :: Phantoms.TTerm Syntax.Decl_Given -> Phantoms.TTerm [[Syntax.Data_Param]] -> Phantoms.TTerm Syntax.Decl_Given
decl_GivenWithSparams original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Decl_Given"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Decl_Given"),
              Core.projectionField = (Core.Name "mods")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Decl_Given"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Decl_Given"),
              Core.projectionField = (Core.Name "tparams")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "sparams"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Decl_Given"),
              Core.projectionField = (Core.Name "decltpe")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

decl_GivenWithTparams :: Phantoms.TTerm Syntax.Decl_Given -> Phantoms.TTerm [Syntax.Type_Param] -> Phantoms.TTerm Syntax.Decl_Given
decl_GivenWithTparams original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Decl_Given"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Decl_Given"),
              Core.projectionField = (Core.Name "mods")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Decl_Given"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "sparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Decl_Given"),
              Core.projectionField = (Core.Name "sparams")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Decl_Given"),
              Core.projectionField = (Core.Name "decltpe")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

decl_Type :: Phantoms.TTerm [Syntax.Mod] -> Phantoms.TTerm Syntax.Type_Name -> Phantoms.TTerm [Syntax.Type_Param] -> Phantoms.TTerm Syntax.TypeBounds -> Phantoms.TTerm Syntax.Decl_Type
decl_Type mods name tparams bounds =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Decl_Type"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Phantoms.unTTerm mods)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Phantoms.unTTerm tparams)},
        Core.Field {
          Core.fieldName = (Core.Name "bounds"),
          Core.fieldTerm = (Phantoms.unTTerm bounds)}]}))

decl_TypeBounds :: Phantoms.TTerm Syntax.Decl_Type -> Phantoms.TTerm Syntax.TypeBounds
decl_TypeBounds x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Decl_Type"),
        Core.projectionField = (Core.Name "bounds")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

decl_TypeMods :: Phantoms.TTerm Syntax.Decl_Type -> Phantoms.TTerm [Syntax.Mod]
decl_TypeMods x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Decl_Type"),
        Core.projectionField = (Core.Name "mods")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

decl_TypeName :: Phantoms.TTerm Syntax.Decl_Type -> Phantoms.TTerm Syntax.Type_Name
decl_TypeName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Decl_Type"),
        Core.projectionField = (Core.Name "name")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

decl_TypeTparams :: Phantoms.TTerm Syntax.Decl_Type -> Phantoms.TTerm [Syntax.Type_Param]
decl_TypeTparams x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Decl_Type"),
        Core.projectionField = (Core.Name "tparams")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

decl_TypeWithBounds :: Phantoms.TTerm Syntax.Decl_Type -> Phantoms.TTerm Syntax.TypeBounds -> Phantoms.TTerm Syntax.Decl_Type
decl_TypeWithBounds original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Decl_Type"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Decl_Type"),
              Core.projectionField = (Core.Name "mods")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Decl_Type"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Decl_Type"),
              Core.projectionField = (Core.Name "tparams")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "bounds"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

decl_TypeWithMods :: Phantoms.TTerm Syntax.Decl_Type -> Phantoms.TTerm [Syntax.Mod] -> Phantoms.TTerm Syntax.Decl_Type
decl_TypeWithMods original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Decl_Type"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Decl_Type"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Decl_Type"),
              Core.projectionField = (Core.Name "tparams")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "bounds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Decl_Type"),
              Core.projectionField = (Core.Name "bounds")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

decl_TypeWithName :: Phantoms.TTerm Syntax.Decl_Type -> Phantoms.TTerm Syntax.Type_Name -> Phantoms.TTerm Syntax.Decl_Type
decl_TypeWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Decl_Type"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Decl_Type"),
              Core.projectionField = (Core.Name "mods")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Decl_Type"),
              Core.projectionField = (Core.Name "tparams")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "bounds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Decl_Type"),
              Core.projectionField = (Core.Name "bounds")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

decl_TypeWithTparams :: Phantoms.TTerm Syntax.Decl_Type -> Phantoms.TTerm [Syntax.Type_Param] -> Phantoms.TTerm Syntax.Decl_Type
decl_TypeWithTparams original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Decl_Type"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Decl_Type"),
              Core.projectionField = (Core.Name "mods")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Decl_Type"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "bounds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Decl_Type"),
              Core.projectionField = (Core.Name "bounds")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

decl_Val :: Phantoms.TTerm [Syntax.Mod] -> Phantoms.TTerm [Syntax.Pat] -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.Decl_Val
decl_Val mods pats decltpe =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Decl_Val"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Phantoms.unTTerm mods)},
        Core.Field {
          Core.fieldName = (Core.Name "pats"),
          Core.fieldTerm = (Phantoms.unTTerm pats)},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Phantoms.unTTerm decltpe)}]}))

decl_ValDecltpe :: Phantoms.TTerm Syntax.Decl_Val -> Phantoms.TTerm Syntax.Type
decl_ValDecltpe x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Decl_Val"),
        Core.projectionField = (Core.Name "decltpe")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

decl_ValMods :: Phantoms.TTerm Syntax.Decl_Val -> Phantoms.TTerm [Syntax.Mod]
decl_ValMods x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Decl_Val"),
        Core.projectionField = (Core.Name "mods")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

decl_ValPats :: Phantoms.TTerm Syntax.Decl_Val -> Phantoms.TTerm [Syntax.Pat]
decl_ValPats x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Decl_Val"),
        Core.projectionField = (Core.Name "pats")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

decl_ValWithDecltpe :: Phantoms.TTerm Syntax.Decl_Val -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.Decl_Val
decl_ValWithDecltpe original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Decl_Val"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Decl_Val"),
              Core.projectionField = (Core.Name "mods")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "pats"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Decl_Val"),
              Core.projectionField = (Core.Name "pats")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

decl_ValWithMods :: Phantoms.TTerm Syntax.Decl_Val -> Phantoms.TTerm [Syntax.Mod] -> Phantoms.TTerm Syntax.Decl_Val
decl_ValWithMods original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Decl_Val"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "pats"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Decl_Val"),
              Core.projectionField = (Core.Name "pats")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Decl_Val"),
              Core.projectionField = (Core.Name "decltpe")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

decl_ValWithPats :: Phantoms.TTerm Syntax.Decl_Val -> Phantoms.TTerm [Syntax.Pat] -> Phantoms.TTerm Syntax.Decl_Val
decl_ValWithPats original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Decl_Val"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Decl_Val"),
              Core.projectionField = (Core.Name "mods")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "pats"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Decl_Val"),
              Core.projectionField = (Core.Name "decltpe")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

decl_Var :: Phantoms.TTerm [Syntax.Mod] -> Phantoms.TTerm [Syntax.Pat] -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.Decl_Var
decl_Var mods pats decltpe =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Decl_Var"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Phantoms.unTTerm mods)},
        Core.Field {
          Core.fieldName = (Core.Name "pats"),
          Core.fieldTerm = (Phantoms.unTTerm pats)},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Phantoms.unTTerm decltpe)}]}))

decl_VarDecltpe :: Phantoms.TTerm Syntax.Decl_Var -> Phantoms.TTerm Syntax.Type
decl_VarDecltpe x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Decl_Var"),
        Core.projectionField = (Core.Name "decltpe")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

decl_VarMods :: Phantoms.TTerm Syntax.Decl_Var -> Phantoms.TTerm [Syntax.Mod]
decl_VarMods x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Decl_Var"),
        Core.projectionField = (Core.Name "mods")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

decl_VarPats :: Phantoms.TTerm Syntax.Decl_Var -> Phantoms.TTerm [Syntax.Pat]
decl_VarPats x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Decl_Var"),
        Core.projectionField = (Core.Name "pats")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

decl_VarWithDecltpe :: Phantoms.TTerm Syntax.Decl_Var -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.Decl_Var
decl_VarWithDecltpe original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Decl_Var"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Decl_Var"),
              Core.projectionField = (Core.Name "mods")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "pats"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Decl_Var"),
              Core.projectionField = (Core.Name "pats")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

decl_VarWithMods :: Phantoms.TTerm Syntax.Decl_Var -> Phantoms.TTerm [Syntax.Mod] -> Phantoms.TTerm Syntax.Decl_Var
decl_VarWithMods original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Decl_Var"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "pats"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Decl_Var"),
              Core.projectionField = (Core.Name "pats")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Decl_Var"),
              Core.projectionField = (Core.Name "decltpe")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

decl_VarWithPats :: Phantoms.TTerm Syntax.Decl_Var -> Phantoms.TTerm [Syntax.Pat] -> Phantoms.TTerm Syntax.Decl_Var
decl_VarWithPats original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Decl_Var"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Decl_Var"),
              Core.projectionField = (Core.Name "mods")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "pats"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Decl_Var"),
              Core.projectionField = (Core.Name "decltpe")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

defnClass :: Phantoms.TTerm Syntax.Defn_Class -> Phantoms.TTerm Syntax.Defn
defnClass x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "class"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

defnDef :: Phantoms.TTerm Syntax.Defn_Def -> Phantoms.TTerm Syntax.Defn
defnDef x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "def"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

defnEnum :: Phantoms.TTerm Syntax.Defn_Enum -> Phantoms.TTerm Syntax.Defn
defnEnum x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "enum"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

defnEnumCase :: Phantoms.TTerm Syntax.Defn_EnumCase -> Phantoms.TTerm Syntax.Defn
defnEnumCase x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "enumCase"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

defnExtensionGroup :: Phantoms.TTerm Syntax.Defn_ExtensionGroup -> Phantoms.TTerm Syntax.Defn
defnExtensionGroup x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "extensionGroup"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

defnGiven :: Phantoms.TTerm Syntax.Defn_Given -> Phantoms.TTerm Syntax.Defn
defnGiven x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "given"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

defnGivenAlias :: Phantoms.TTerm Syntax.Defn_GivenAlias -> Phantoms.TTerm Syntax.Defn
defnGivenAlias x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "givenAlias"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

defnMacro :: Phantoms.TTerm Syntax.Defn_Macro -> Phantoms.TTerm Syntax.Defn
defnMacro x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "macro"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

defnObject :: Phantoms.TTerm Syntax.Defn_Object -> Phantoms.TTerm Syntax.Defn
defnObject x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "object"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

defnRepeatedEnumCase :: Phantoms.TTerm Syntax.Defn_RepeatedEnumCase -> Phantoms.TTerm Syntax.Defn
defnRepeatedEnumCase x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "repeatedEnumCase"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

defnTrait :: Phantoms.TTerm Syntax.Defn_Trait -> Phantoms.TTerm Syntax.Defn
defnTrait x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "trait"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

defnType :: Phantoms.TTerm Syntax.Defn_Type -> Phantoms.TTerm Syntax.Defn
defnType x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "type"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

defnVal :: Phantoms.TTerm Syntax.Defn_Val -> Phantoms.TTerm Syntax.Defn
defnVal x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "val"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

defnVar :: Phantoms.TTerm Syntax.Defn_Var -> Phantoms.TTerm Syntax.Defn
defnVar x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "var"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

defn_Class :: Phantoms.TTerm [Syntax.Mod] -> Phantoms.TTerm Syntax.Type_Name -> Phantoms.TTerm [Syntax.Type_Param] -> Phantoms.TTerm Syntax.Ctor_Primary -> Phantoms.TTerm Syntax.Template -> Phantoms.TTerm Syntax.Defn_Class
defn_Class mods name tparams ctor template =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Class"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Phantoms.unTTerm mods)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Phantoms.unTTerm tparams)},
        Core.Field {
          Core.fieldName = (Core.Name "ctor"),
          Core.fieldTerm = (Phantoms.unTTerm ctor)},
        Core.Field {
          Core.fieldName = (Core.Name "template"),
          Core.fieldTerm = (Phantoms.unTTerm template)}]}))

defn_ClassCtor :: Phantoms.TTerm Syntax.Defn_Class -> Phantoms.TTerm Syntax.Ctor_Primary
defn_ClassCtor x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Class"),
        Core.projectionField = (Core.Name "ctor")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

defn_ClassMods :: Phantoms.TTerm Syntax.Defn_Class -> Phantoms.TTerm [Syntax.Mod]
defn_ClassMods x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Class"),
        Core.projectionField = (Core.Name "mods")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

defn_ClassName :: Phantoms.TTerm Syntax.Defn_Class -> Phantoms.TTerm Syntax.Type_Name
defn_ClassName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Class"),
        Core.projectionField = (Core.Name "name")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

defn_ClassTemplate :: Phantoms.TTerm Syntax.Defn_Class -> Phantoms.TTerm Syntax.Template
defn_ClassTemplate x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Class"),
        Core.projectionField = (Core.Name "template")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

defn_ClassTparams :: Phantoms.TTerm Syntax.Defn_Class -> Phantoms.TTerm [Syntax.Type_Param]
defn_ClassTparams x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Class"),
        Core.projectionField = (Core.Name "tparams")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

defn_ClassWithCtor :: Phantoms.TTerm Syntax.Defn_Class -> Phantoms.TTerm Syntax.Ctor_Primary -> Phantoms.TTerm Syntax.Defn_Class
defn_ClassWithCtor original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Class"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Class"),
              Core.projectionField = (Core.Name "mods")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Class"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Class"),
              Core.projectionField = (Core.Name "tparams")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ctor"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "template"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Class"),
              Core.projectionField = (Core.Name "template")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

defn_ClassWithMods :: Phantoms.TTerm Syntax.Defn_Class -> Phantoms.TTerm [Syntax.Mod] -> Phantoms.TTerm Syntax.Defn_Class
defn_ClassWithMods original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Class"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Class"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Class"),
              Core.projectionField = (Core.Name "tparams")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ctor"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Class"),
              Core.projectionField = (Core.Name "ctor")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "template"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Class"),
              Core.projectionField = (Core.Name "template")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

defn_ClassWithName :: Phantoms.TTerm Syntax.Defn_Class -> Phantoms.TTerm Syntax.Type_Name -> Phantoms.TTerm Syntax.Defn_Class
defn_ClassWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Class"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Class"),
              Core.projectionField = (Core.Name "mods")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Class"),
              Core.projectionField = (Core.Name "tparams")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ctor"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Class"),
              Core.projectionField = (Core.Name "ctor")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "template"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Class"),
              Core.projectionField = (Core.Name "template")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

defn_ClassWithTemplate :: Phantoms.TTerm Syntax.Defn_Class -> Phantoms.TTerm Syntax.Template -> Phantoms.TTerm Syntax.Defn_Class
defn_ClassWithTemplate original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Class"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Class"),
              Core.projectionField = (Core.Name "mods")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Class"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Class"),
              Core.projectionField = (Core.Name "tparams")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ctor"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Class"),
              Core.projectionField = (Core.Name "ctor")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "template"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

defn_ClassWithTparams :: Phantoms.TTerm Syntax.Defn_Class -> Phantoms.TTerm [Syntax.Type_Param] -> Phantoms.TTerm Syntax.Defn_Class
defn_ClassWithTparams original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Class"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Class"),
              Core.projectionField = (Core.Name "mods")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Class"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "ctor"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Class"),
              Core.projectionField = (Core.Name "ctor")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "template"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Class"),
              Core.projectionField = (Core.Name "template")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

defn_Def :: Phantoms.TTerm [Syntax.Mod] -> Phantoms.TTerm Syntax.Data_Name -> Phantoms.TTerm [Syntax.Type_Param] -> Phantoms.TTerm [[Syntax.Data_Param]] -> Phantoms.TTerm (Maybe Syntax.Type) -> Phantoms.TTerm Syntax.Data -> Phantoms.TTerm Syntax.Defn_Def
defn_Def mods name tparams paramss decltpe body =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Def"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Phantoms.unTTerm mods)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Phantoms.unTTerm tparams)},
        Core.Field {
          Core.fieldName = (Core.Name "paramss"),
          Core.fieldTerm = (Phantoms.unTTerm paramss)},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Phantoms.unTTerm decltpe)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm body)}]}))

defn_DefBody :: Phantoms.TTerm Syntax.Defn_Def -> Phantoms.TTerm Syntax.Data
defn_DefBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Def"),
        Core.projectionField = (Core.Name "body")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

defn_DefDecltpe :: Phantoms.TTerm Syntax.Defn_Def -> Phantoms.TTerm (Maybe Syntax.Type)
defn_DefDecltpe x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Def"),
        Core.projectionField = (Core.Name "decltpe")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

defn_DefMods :: Phantoms.TTerm Syntax.Defn_Def -> Phantoms.TTerm [Syntax.Mod]
defn_DefMods x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Def"),
        Core.projectionField = (Core.Name "mods")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

defn_DefName :: Phantoms.TTerm Syntax.Defn_Def -> Phantoms.TTerm Syntax.Data_Name
defn_DefName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Def"),
        Core.projectionField = (Core.Name "name")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

defn_DefParamss :: Phantoms.TTerm Syntax.Defn_Def -> Phantoms.TTerm [[Syntax.Data_Param]]
defn_DefParamss x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Def"),
        Core.projectionField = (Core.Name "paramss")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

defn_DefTparams :: Phantoms.TTerm Syntax.Defn_Def -> Phantoms.TTerm [Syntax.Type_Param]
defn_DefTparams x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Def"),
        Core.projectionField = (Core.Name "tparams")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

defn_DefWithBody :: Phantoms.TTerm Syntax.Defn_Def -> Phantoms.TTerm Syntax.Data -> Phantoms.TTerm Syntax.Defn_Def
defn_DefWithBody original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Def"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Def"),
              Core.projectionField = (Core.Name "mods")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Def"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Def"),
              Core.projectionField = (Core.Name "tparams")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "paramss"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Def"),
              Core.projectionField = (Core.Name "paramss")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Def"),
              Core.projectionField = (Core.Name "decltpe")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

defn_DefWithDecltpe :: Phantoms.TTerm Syntax.Defn_Def -> Phantoms.TTerm (Maybe Syntax.Type) -> Phantoms.TTerm Syntax.Defn_Def
defn_DefWithDecltpe original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Def"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Def"),
              Core.projectionField = (Core.Name "mods")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Def"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Def"),
              Core.projectionField = (Core.Name "tparams")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "paramss"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Def"),
              Core.projectionField = (Core.Name "paramss")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Def"),
              Core.projectionField = (Core.Name "body")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

defn_DefWithMods :: Phantoms.TTerm Syntax.Defn_Def -> Phantoms.TTerm [Syntax.Mod] -> Phantoms.TTerm Syntax.Defn_Def
defn_DefWithMods original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Def"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Def"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Def"),
              Core.projectionField = (Core.Name "tparams")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "paramss"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Def"),
              Core.projectionField = (Core.Name "paramss")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Def"),
              Core.projectionField = (Core.Name "decltpe")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Def"),
              Core.projectionField = (Core.Name "body")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

defn_DefWithName :: Phantoms.TTerm Syntax.Defn_Def -> Phantoms.TTerm Syntax.Data_Name -> Phantoms.TTerm Syntax.Defn_Def
defn_DefWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Def"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Def"),
              Core.projectionField = (Core.Name "mods")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Def"),
              Core.projectionField = (Core.Name "tparams")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "paramss"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Def"),
              Core.projectionField = (Core.Name "paramss")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Def"),
              Core.projectionField = (Core.Name "decltpe")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Def"),
              Core.projectionField = (Core.Name "body")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

defn_DefWithParamss :: Phantoms.TTerm Syntax.Defn_Def -> Phantoms.TTerm [[Syntax.Data_Param]] -> Phantoms.TTerm Syntax.Defn_Def
defn_DefWithParamss original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Def"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Def"),
              Core.projectionField = (Core.Name "mods")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Def"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Def"),
              Core.projectionField = (Core.Name "tparams")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "paramss"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Def"),
              Core.projectionField = (Core.Name "decltpe")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Def"),
              Core.projectionField = (Core.Name "body")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

defn_DefWithTparams :: Phantoms.TTerm Syntax.Defn_Def -> Phantoms.TTerm [Syntax.Type_Param] -> Phantoms.TTerm Syntax.Defn_Def
defn_DefWithTparams original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Def"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Def"),
              Core.projectionField = (Core.Name "mods")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Def"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "paramss"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Def"),
              Core.projectionField = (Core.Name "paramss")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Def"),
              Core.projectionField = (Core.Name "decltpe")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Def"),
              Core.projectionField = (Core.Name "body")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

defn_Enum :: Phantoms.TTerm [Syntax.Mod] -> Phantoms.TTerm Syntax.Type_Name -> Phantoms.TTerm [Syntax.Type_Param] -> Phantoms.TTerm Syntax.Ctor_Primary -> Phantoms.TTerm Syntax.Template -> Phantoms.TTerm Syntax.Defn_Enum
defn_Enum mods name tparams ctor template =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Enum"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Phantoms.unTTerm mods)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Phantoms.unTTerm tparams)},
        Core.Field {
          Core.fieldName = (Core.Name "ctor"),
          Core.fieldTerm = (Phantoms.unTTerm ctor)},
        Core.Field {
          Core.fieldName = (Core.Name "template"),
          Core.fieldTerm = (Phantoms.unTTerm template)}]}))

defn_EnumCase :: Phantoms.TTerm [Syntax.Mod] -> Phantoms.TTerm Syntax.Data_Name -> Phantoms.TTerm [Syntax.Type_Param] -> Phantoms.TTerm Syntax.Ctor_Primary -> Phantoms.TTerm [Syntax.Init] -> Phantoms.TTerm Syntax.Defn_EnumCase
defn_EnumCase mods name tparams ctor inits =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_EnumCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Phantoms.unTTerm mods)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Phantoms.unTTerm tparams)},
        Core.Field {
          Core.fieldName = (Core.Name "ctor"),
          Core.fieldTerm = (Phantoms.unTTerm ctor)},
        Core.Field {
          Core.fieldName = (Core.Name "inits"),
          Core.fieldTerm = (Phantoms.unTTerm inits)}]}))

defn_EnumCaseCtor :: Phantoms.TTerm Syntax.Defn_EnumCase -> Phantoms.TTerm Syntax.Ctor_Primary
defn_EnumCaseCtor x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_EnumCase"),
        Core.projectionField = (Core.Name "ctor")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

defn_EnumCaseInits :: Phantoms.TTerm Syntax.Defn_EnumCase -> Phantoms.TTerm [Syntax.Init]
defn_EnumCaseInits x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_EnumCase"),
        Core.projectionField = (Core.Name "inits")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

defn_EnumCaseMods :: Phantoms.TTerm Syntax.Defn_EnumCase -> Phantoms.TTerm [Syntax.Mod]
defn_EnumCaseMods x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_EnumCase"),
        Core.projectionField = (Core.Name "mods")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

defn_EnumCaseName :: Phantoms.TTerm Syntax.Defn_EnumCase -> Phantoms.TTerm Syntax.Data_Name
defn_EnumCaseName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_EnumCase"),
        Core.projectionField = (Core.Name "name")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

defn_EnumCaseTparams :: Phantoms.TTerm Syntax.Defn_EnumCase -> Phantoms.TTerm [Syntax.Type_Param]
defn_EnumCaseTparams x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_EnumCase"),
        Core.projectionField = (Core.Name "tparams")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

defn_EnumCaseWithCtor :: Phantoms.TTerm Syntax.Defn_EnumCase -> Phantoms.TTerm Syntax.Ctor_Primary -> Phantoms.TTerm Syntax.Defn_EnumCase
defn_EnumCaseWithCtor original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_EnumCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_EnumCase"),
              Core.projectionField = (Core.Name "mods")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_EnumCase"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_EnumCase"),
              Core.projectionField = (Core.Name "tparams")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ctor"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "inits"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_EnumCase"),
              Core.projectionField = (Core.Name "inits")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

defn_EnumCaseWithInits :: Phantoms.TTerm Syntax.Defn_EnumCase -> Phantoms.TTerm [Syntax.Init] -> Phantoms.TTerm Syntax.Defn_EnumCase
defn_EnumCaseWithInits original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_EnumCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_EnumCase"),
              Core.projectionField = (Core.Name "mods")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_EnumCase"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_EnumCase"),
              Core.projectionField = (Core.Name "tparams")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ctor"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_EnumCase"),
              Core.projectionField = (Core.Name "ctor")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inits"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

defn_EnumCaseWithMods :: Phantoms.TTerm Syntax.Defn_EnumCase -> Phantoms.TTerm [Syntax.Mod] -> Phantoms.TTerm Syntax.Defn_EnumCase
defn_EnumCaseWithMods original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_EnumCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_EnumCase"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_EnumCase"),
              Core.projectionField = (Core.Name "tparams")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ctor"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_EnumCase"),
              Core.projectionField = (Core.Name "ctor")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inits"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_EnumCase"),
              Core.projectionField = (Core.Name "inits")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

defn_EnumCaseWithName :: Phantoms.TTerm Syntax.Defn_EnumCase -> Phantoms.TTerm Syntax.Data_Name -> Phantoms.TTerm Syntax.Defn_EnumCase
defn_EnumCaseWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_EnumCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_EnumCase"),
              Core.projectionField = (Core.Name "mods")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_EnumCase"),
              Core.projectionField = (Core.Name "tparams")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ctor"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_EnumCase"),
              Core.projectionField = (Core.Name "ctor")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inits"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_EnumCase"),
              Core.projectionField = (Core.Name "inits")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

defn_EnumCaseWithTparams :: Phantoms.TTerm Syntax.Defn_EnumCase -> Phantoms.TTerm [Syntax.Type_Param] -> Phantoms.TTerm Syntax.Defn_EnumCase
defn_EnumCaseWithTparams original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_EnumCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_EnumCase"),
              Core.projectionField = (Core.Name "mods")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_EnumCase"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "ctor"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_EnumCase"),
              Core.projectionField = (Core.Name "ctor")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inits"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_EnumCase"),
              Core.projectionField = (Core.Name "inits")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

defn_EnumCtor :: Phantoms.TTerm Syntax.Defn_Enum -> Phantoms.TTerm Syntax.Ctor_Primary
defn_EnumCtor x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Enum"),
        Core.projectionField = (Core.Name "ctor")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

defn_EnumMods :: Phantoms.TTerm Syntax.Defn_Enum -> Phantoms.TTerm [Syntax.Mod]
defn_EnumMods x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Enum"),
        Core.projectionField = (Core.Name "mods")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

defn_EnumName :: Phantoms.TTerm Syntax.Defn_Enum -> Phantoms.TTerm Syntax.Type_Name
defn_EnumName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Enum"),
        Core.projectionField = (Core.Name "name")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

defn_EnumTemplate :: Phantoms.TTerm Syntax.Defn_Enum -> Phantoms.TTerm Syntax.Template
defn_EnumTemplate x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Enum"),
        Core.projectionField = (Core.Name "template")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

defn_EnumTparams :: Phantoms.TTerm Syntax.Defn_Enum -> Phantoms.TTerm [Syntax.Type_Param]
defn_EnumTparams x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Enum"),
        Core.projectionField = (Core.Name "tparams")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

defn_EnumWithCtor :: Phantoms.TTerm Syntax.Defn_Enum -> Phantoms.TTerm Syntax.Ctor_Primary -> Phantoms.TTerm Syntax.Defn_Enum
defn_EnumWithCtor original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Enum"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Enum"),
              Core.projectionField = (Core.Name "mods")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Enum"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Enum"),
              Core.projectionField = (Core.Name "tparams")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ctor"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "template"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Enum"),
              Core.projectionField = (Core.Name "template")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

defn_EnumWithMods :: Phantoms.TTerm Syntax.Defn_Enum -> Phantoms.TTerm [Syntax.Mod] -> Phantoms.TTerm Syntax.Defn_Enum
defn_EnumWithMods original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Enum"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Enum"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Enum"),
              Core.projectionField = (Core.Name "tparams")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ctor"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Enum"),
              Core.projectionField = (Core.Name "ctor")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "template"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Enum"),
              Core.projectionField = (Core.Name "template")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

defn_EnumWithName :: Phantoms.TTerm Syntax.Defn_Enum -> Phantoms.TTerm Syntax.Type_Name -> Phantoms.TTerm Syntax.Defn_Enum
defn_EnumWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Enum"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Enum"),
              Core.projectionField = (Core.Name "mods")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Enum"),
              Core.projectionField = (Core.Name "tparams")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ctor"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Enum"),
              Core.projectionField = (Core.Name "ctor")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "template"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Enum"),
              Core.projectionField = (Core.Name "template")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

defn_EnumWithTemplate :: Phantoms.TTerm Syntax.Defn_Enum -> Phantoms.TTerm Syntax.Template -> Phantoms.TTerm Syntax.Defn_Enum
defn_EnumWithTemplate original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Enum"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Enum"),
              Core.projectionField = (Core.Name "mods")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Enum"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Enum"),
              Core.projectionField = (Core.Name "tparams")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ctor"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Enum"),
              Core.projectionField = (Core.Name "ctor")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "template"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

defn_EnumWithTparams :: Phantoms.TTerm Syntax.Defn_Enum -> Phantoms.TTerm [Syntax.Type_Param] -> Phantoms.TTerm Syntax.Defn_Enum
defn_EnumWithTparams original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Enum"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Enum"),
              Core.projectionField = (Core.Name "mods")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Enum"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "ctor"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Enum"),
              Core.projectionField = (Core.Name "ctor")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "template"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Enum"),
              Core.projectionField = (Core.Name "template")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

defn_ExtensionGroup :: Phantoms.TTerm [Syntax.Type_Param] -> Phantoms.TTerm [[Syntax.Data_Param]] -> Phantoms.TTerm Syntax.Stat -> Phantoms.TTerm Syntax.Defn_ExtensionGroup
defn_ExtensionGroup tparams parmss body =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_ExtensionGroup"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Phantoms.unTTerm tparams)},
        Core.Field {
          Core.fieldName = (Core.Name "parmss"),
          Core.fieldTerm = (Phantoms.unTTerm parmss)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm body)}]}))

defn_ExtensionGroupBody :: Phantoms.TTerm Syntax.Defn_ExtensionGroup -> Phantoms.TTerm Syntax.Stat
defn_ExtensionGroupBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_ExtensionGroup"),
        Core.projectionField = (Core.Name "body")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

defn_ExtensionGroupParmss :: Phantoms.TTerm Syntax.Defn_ExtensionGroup -> Phantoms.TTerm [[Syntax.Data_Param]]
defn_ExtensionGroupParmss x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_ExtensionGroup"),
        Core.projectionField = (Core.Name "parmss")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

defn_ExtensionGroupTparams :: Phantoms.TTerm Syntax.Defn_ExtensionGroup -> Phantoms.TTerm [Syntax.Type_Param]
defn_ExtensionGroupTparams x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_ExtensionGroup"),
        Core.projectionField = (Core.Name "tparams")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

defn_ExtensionGroupWithBody :: Phantoms.TTerm Syntax.Defn_ExtensionGroup -> Phantoms.TTerm Syntax.Stat -> Phantoms.TTerm Syntax.Defn_ExtensionGroup
defn_ExtensionGroupWithBody original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_ExtensionGroup"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_ExtensionGroup"),
              Core.projectionField = (Core.Name "tparams")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "parmss"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_ExtensionGroup"),
              Core.projectionField = (Core.Name "parmss")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

defn_ExtensionGroupWithParmss :: Phantoms.TTerm Syntax.Defn_ExtensionGroup -> Phantoms.TTerm [[Syntax.Data_Param]] -> Phantoms.TTerm Syntax.Defn_ExtensionGroup
defn_ExtensionGroupWithParmss original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_ExtensionGroup"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_ExtensionGroup"),
              Core.projectionField = (Core.Name "tparams")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "parmss"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_ExtensionGroup"),
              Core.projectionField = (Core.Name "body")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

defn_ExtensionGroupWithTparams :: Phantoms.TTerm Syntax.Defn_ExtensionGroup -> Phantoms.TTerm [Syntax.Type_Param] -> Phantoms.TTerm Syntax.Defn_ExtensionGroup
defn_ExtensionGroupWithTparams original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_ExtensionGroup"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "parmss"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_ExtensionGroup"),
              Core.projectionField = (Core.Name "parmss")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_ExtensionGroup"),
              Core.projectionField = (Core.Name "body")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

defn_Given :: Phantoms.TTerm [Syntax.Mod] -> Phantoms.TTerm Syntax.Name -> Phantoms.TTerm [[Syntax.Type_Param]] -> Phantoms.TTerm [[Syntax.Data_Param]] -> Phantoms.TTerm Syntax.Template -> Phantoms.TTerm Syntax.Defn_Given
defn_Given mods name tparams sparams templ =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Given"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Phantoms.unTTerm mods)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Phantoms.unTTerm tparams)},
        Core.Field {
          Core.fieldName = (Core.Name "sparams"),
          Core.fieldTerm = (Phantoms.unTTerm sparams)},
        Core.Field {
          Core.fieldName = (Core.Name "templ"),
          Core.fieldTerm = (Phantoms.unTTerm templ)}]}))

defn_GivenAlias :: Phantoms.TTerm [Syntax.Mod] -> Phantoms.TTerm Syntax.Name -> Phantoms.TTerm [[Syntax.Type_Param]] -> Phantoms.TTerm [[Syntax.Data_Param]] -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.Data -> Phantoms.TTerm Syntax.Defn_GivenAlias
defn_GivenAlias mods name tparams sparams decltpe body =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_GivenAlias"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Phantoms.unTTerm mods)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Phantoms.unTTerm tparams)},
        Core.Field {
          Core.fieldName = (Core.Name "sparams"),
          Core.fieldTerm = (Phantoms.unTTerm sparams)},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Phantoms.unTTerm decltpe)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm body)}]}))

defn_GivenAliasBody :: Phantoms.TTerm Syntax.Defn_GivenAlias -> Phantoms.TTerm Syntax.Data
defn_GivenAliasBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_GivenAlias"),
        Core.projectionField = (Core.Name "body")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

defn_GivenAliasDecltpe :: Phantoms.TTerm Syntax.Defn_GivenAlias -> Phantoms.TTerm Syntax.Type
defn_GivenAliasDecltpe x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_GivenAlias"),
        Core.projectionField = (Core.Name "decltpe")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

defn_GivenAliasMods :: Phantoms.TTerm Syntax.Defn_GivenAlias -> Phantoms.TTerm [Syntax.Mod]
defn_GivenAliasMods x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_GivenAlias"),
        Core.projectionField = (Core.Name "mods")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

defn_GivenAliasName :: Phantoms.TTerm Syntax.Defn_GivenAlias -> Phantoms.TTerm Syntax.Name
defn_GivenAliasName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_GivenAlias"),
        Core.projectionField = (Core.Name "name")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

defn_GivenAliasSparams :: Phantoms.TTerm Syntax.Defn_GivenAlias -> Phantoms.TTerm [[Syntax.Data_Param]]
defn_GivenAliasSparams x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_GivenAlias"),
        Core.projectionField = (Core.Name "sparams")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

defn_GivenAliasTparams :: Phantoms.TTerm Syntax.Defn_GivenAlias -> Phantoms.TTerm [[Syntax.Type_Param]]
defn_GivenAliasTparams x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_GivenAlias"),
        Core.projectionField = (Core.Name "tparams")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

defn_GivenAliasWithBody :: Phantoms.TTerm Syntax.Defn_GivenAlias -> Phantoms.TTerm Syntax.Data -> Phantoms.TTerm Syntax.Defn_GivenAlias
defn_GivenAliasWithBody original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_GivenAlias"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_GivenAlias"),
              Core.projectionField = (Core.Name "mods")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_GivenAlias"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_GivenAlias"),
              Core.projectionField = (Core.Name "tparams")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "sparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_GivenAlias"),
              Core.projectionField = (Core.Name "sparams")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_GivenAlias"),
              Core.projectionField = (Core.Name "decltpe")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

defn_GivenAliasWithDecltpe :: Phantoms.TTerm Syntax.Defn_GivenAlias -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.Defn_GivenAlias
defn_GivenAliasWithDecltpe original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_GivenAlias"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_GivenAlias"),
              Core.projectionField = (Core.Name "mods")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_GivenAlias"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_GivenAlias"),
              Core.projectionField = (Core.Name "tparams")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "sparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_GivenAlias"),
              Core.projectionField = (Core.Name "sparams")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_GivenAlias"),
              Core.projectionField = (Core.Name "body")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

defn_GivenAliasWithMods :: Phantoms.TTerm Syntax.Defn_GivenAlias -> Phantoms.TTerm [Syntax.Mod] -> Phantoms.TTerm Syntax.Defn_GivenAlias
defn_GivenAliasWithMods original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_GivenAlias"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_GivenAlias"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_GivenAlias"),
              Core.projectionField = (Core.Name "tparams")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "sparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_GivenAlias"),
              Core.projectionField = (Core.Name "sparams")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_GivenAlias"),
              Core.projectionField = (Core.Name "decltpe")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_GivenAlias"),
              Core.projectionField = (Core.Name "body")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

defn_GivenAliasWithName :: Phantoms.TTerm Syntax.Defn_GivenAlias -> Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.Defn_GivenAlias
defn_GivenAliasWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_GivenAlias"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_GivenAlias"),
              Core.projectionField = (Core.Name "mods")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_GivenAlias"),
              Core.projectionField = (Core.Name "tparams")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "sparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_GivenAlias"),
              Core.projectionField = (Core.Name "sparams")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_GivenAlias"),
              Core.projectionField = (Core.Name "decltpe")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_GivenAlias"),
              Core.projectionField = (Core.Name "body")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

defn_GivenAliasWithSparams :: Phantoms.TTerm Syntax.Defn_GivenAlias -> Phantoms.TTerm [[Syntax.Data_Param]] -> Phantoms.TTerm Syntax.Defn_GivenAlias
defn_GivenAliasWithSparams original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_GivenAlias"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_GivenAlias"),
              Core.projectionField = (Core.Name "mods")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_GivenAlias"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_GivenAlias"),
              Core.projectionField = (Core.Name "tparams")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "sparams"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_GivenAlias"),
              Core.projectionField = (Core.Name "decltpe")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_GivenAlias"),
              Core.projectionField = (Core.Name "body")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

defn_GivenAliasWithTparams :: Phantoms.TTerm Syntax.Defn_GivenAlias -> Phantoms.TTerm [[Syntax.Type_Param]] -> Phantoms.TTerm Syntax.Defn_GivenAlias
defn_GivenAliasWithTparams original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_GivenAlias"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_GivenAlias"),
              Core.projectionField = (Core.Name "mods")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_GivenAlias"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "sparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_GivenAlias"),
              Core.projectionField = (Core.Name "sparams")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_GivenAlias"),
              Core.projectionField = (Core.Name "decltpe")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_GivenAlias"),
              Core.projectionField = (Core.Name "body")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

defn_GivenMods :: Phantoms.TTerm Syntax.Defn_Given -> Phantoms.TTerm [Syntax.Mod]
defn_GivenMods x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Given"),
        Core.projectionField = (Core.Name "mods")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

defn_GivenName :: Phantoms.TTerm Syntax.Defn_Given -> Phantoms.TTerm Syntax.Name
defn_GivenName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Given"),
        Core.projectionField = (Core.Name "name")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

defn_GivenSparams :: Phantoms.TTerm Syntax.Defn_Given -> Phantoms.TTerm [[Syntax.Data_Param]]
defn_GivenSparams x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Given"),
        Core.projectionField = (Core.Name "sparams")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

defn_GivenTempl :: Phantoms.TTerm Syntax.Defn_Given -> Phantoms.TTerm Syntax.Template
defn_GivenTempl x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Given"),
        Core.projectionField = (Core.Name "templ")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

defn_GivenTparams :: Phantoms.TTerm Syntax.Defn_Given -> Phantoms.TTerm [[Syntax.Type_Param]]
defn_GivenTparams x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Given"),
        Core.projectionField = (Core.Name "tparams")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

defn_GivenWithMods :: Phantoms.TTerm Syntax.Defn_Given -> Phantoms.TTerm [Syntax.Mod] -> Phantoms.TTerm Syntax.Defn_Given
defn_GivenWithMods original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Given"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Given"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Given"),
              Core.projectionField = (Core.Name "tparams")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "sparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Given"),
              Core.projectionField = (Core.Name "sparams")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "templ"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Given"),
              Core.projectionField = (Core.Name "templ")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

defn_GivenWithName :: Phantoms.TTerm Syntax.Defn_Given -> Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.Defn_Given
defn_GivenWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Given"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Given"),
              Core.projectionField = (Core.Name "mods")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Given"),
              Core.projectionField = (Core.Name "tparams")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "sparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Given"),
              Core.projectionField = (Core.Name "sparams")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "templ"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Given"),
              Core.projectionField = (Core.Name "templ")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

defn_GivenWithSparams :: Phantoms.TTerm Syntax.Defn_Given -> Phantoms.TTerm [[Syntax.Data_Param]] -> Phantoms.TTerm Syntax.Defn_Given
defn_GivenWithSparams original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Given"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Given"),
              Core.projectionField = (Core.Name "mods")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Given"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Given"),
              Core.projectionField = (Core.Name "tparams")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "sparams"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "templ"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Given"),
              Core.projectionField = (Core.Name "templ")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

defn_GivenWithTempl :: Phantoms.TTerm Syntax.Defn_Given -> Phantoms.TTerm Syntax.Template -> Phantoms.TTerm Syntax.Defn_Given
defn_GivenWithTempl original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Given"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Given"),
              Core.projectionField = (Core.Name "mods")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Given"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Given"),
              Core.projectionField = (Core.Name "tparams")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "sparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Given"),
              Core.projectionField = (Core.Name "sparams")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "templ"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

defn_GivenWithTparams :: Phantoms.TTerm Syntax.Defn_Given -> Phantoms.TTerm [[Syntax.Type_Param]] -> Phantoms.TTerm Syntax.Defn_Given
defn_GivenWithTparams original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Given"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Given"),
              Core.projectionField = (Core.Name "mods")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Given"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "sparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Given"),
              Core.projectionField = (Core.Name "sparams")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "templ"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Given"),
              Core.projectionField = (Core.Name "templ")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

defn_Macro :: Phantoms.TTerm [Syntax.Mod] -> Phantoms.TTerm Syntax.Data_Name -> Phantoms.TTerm [Syntax.Type_Param] -> Phantoms.TTerm [[Syntax.Data_Param]] -> Phantoms.TTerm (Maybe Syntax.Type) -> Phantoms.TTerm Syntax.Data -> Phantoms.TTerm Syntax.Defn_Macro
defn_Macro mods name tparams paramss decltpe body =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Macro"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Phantoms.unTTerm mods)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Phantoms.unTTerm tparams)},
        Core.Field {
          Core.fieldName = (Core.Name "paramss"),
          Core.fieldTerm = (Phantoms.unTTerm paramss)},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Phantoms.unTTerm decltpe)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm body)}]}))

defn_MacroBody :: Phantoms.TTerm Syntax.Defn_Macro -> Phantoms.TTerm Syntax.Data
defn_MacroBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Macro"),
        Core.projectionField = (Core.Name "body")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

defn_MacroDecltpe :: Phantoms.TTerm Syntax.Defn_Macro -> Phantoms.TTerm (Maybe Syntax.Type)
defn_MacroDecltpe x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Macro"),
        Core.projectionField = (Core.Name "decltpe")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

defn_MacroMods :: Phantoms.TTerm Syntax.Defn_Macro -> Phantoms.TTerm [Syntax.Mod]
defn_MacroMods x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Macro"),
        Core.projectionField = (Core.Name "mods")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

defn_MacroName :: Phantoms.TTerm Syntax.Defn_Macro -> Phantoms.TTerm Syntax.Data_Name
defn_MacroName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Macro"),
        Core.projectionField = (Core.Name "name")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

defn_MacroParamss :: Phantoms.TTerm Syntax.Defn_Macro -> Phantoms.TTerm [[Syntax.Data_Param]]
defn_MacroParamss x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Macro"),
        Core.projectionField = (Core.Name "paramss")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

defn_MacroTparams :: Phantoms.TTerm Syntax.Defn_Macro -> Phantoms.TTerm [Syntax.Type_Param]
defn_MacroTparams x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Macro"),
        Core.projectionField = (Core.Name "tparams")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

defn_MacroWithBody :: Phantoms.TTerm Syntax.Defn_Macro -> Phantoms.TTerm Syntax.Data -> Phantoms.TTerm Syntax.Defn_Macro
defn_MacroWithBody original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Macro"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Macro"),
              Core.projectionField = (Core.Name "mods")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Macro"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Macro"),
              Core.projectionField = (Core.Name "tparams")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "paramss"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Macro"),
              Core.projectionField = (Core.Name "paramss")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Macro"),
              Core.projectionField = (Core.Name "decltpe")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

defn_MacroWithDecltpe :: Phantoms.TTerm Syntax.Defn_Macro -> Phantoms.TTerm (Maybe Syntax.Type) -> Phantoms.TTerm Syntax.Defn_Macro
defn_MacroWithDecltpe original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Macro"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Macro"),
              Core.projectionField = (Core.Name "mods")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Macro"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Macro"),
              Core.projectionField = (Core.Name "tparams")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "paramss"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Macro"),
              Core.projectionField = (Core.Name "paramss")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Macro"),
              Core.projectionField = (Core.Name "body")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

defn_MacroWithMods :: Phantoms.TTerm Syntax.Defn_Macro -> Phantoms.TTerm [Syntax.Mod] -> Phantoms.TTerm Syntax.Defn_Macro
defn_MacroWithMods original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Macro"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Macro"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Macro"),
              Core.projectionField = (Core.Name "tparams")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "paramss"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Macro"),
              Core.projectionField = (Core.Name "paramss")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Macro"),
              Core.projectionField = (Core.Name "decltpe")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Macro"),
              Core.projectionField = (Core.Name "body")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

defn_MacroWithName :: Phantoms.TTerm Syntax.Defn_Macro -> Phantoms.TTerm Syntax.Data_Name -> Phantoms.TTerm Syntax.Defn_Macro
defn_MacroWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Macro"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Macro"),
              Core.projectionField = (Core.Name "mods")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Macro"),
              Core.projectionField = (Core.Name "tparams")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "paramss"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Macro"),
              Core.projectionField = (Core.Name "paramss")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Macro"),
              Core.projectionField = (Core.Name "decltpe")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Macro"),
              Core.projectionField = (Core.Name "body")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

defn_MacroWithParamss :: Phantoms.TTerm Syntax.Defn_Macro -> Phantoms.TTerm [[Syntax.Data_Param]] -> Phantoms.TTerm Syntax.Defn_Macro
defn_MacroWithParamss original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Macro"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Macro"),
              Core.projectionField = (Core.Name "mods")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Macro"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Macro"),
              Core.projectionField = (Core.Name "tparams")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "paramss"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Macro"),
              Core.projectionField = (Core.Name "decltpe")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Macro"),
              Core.projectionField = (Core.Name "body")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

defn_MacroWithTparams :: Phantoms.TTerm Syntax.Defn_Macro -> Phantoms.TTerm [Syntax.Type_Param] -> Phantoms.TTerm Syntax.Defn_Macro
defn_MacroWithTparams original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Macro"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Macro"),
              Core.projectionField = (Core.Name "mods")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Macro"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "paramss"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Macro"),
              Core.projectionField = (Core.Name "paramss")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Macro"),
              Core.projectionField = (Core.Name "decltpe")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Macro"),
              Core.projectionField = (Core.Name "body")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

defn_Object :: Phantoms.TTerm Syntax.Data_Name -> Phantoms.TTerm Syntax.Defn_Object
defn_Object name =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Object"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)}]}))

defn_ObjectName :: Phantoms.TTerm Syntax.Defn_Object -> Phantoms.TTerm Syntax.Data_Name
defn_ObjectName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Object"),
        Core.projectionField = (Core.Name "name")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

defn_ObjectWithName :: Phantoms.TTerm Syntax.Defn_Object -> Phantoms.TTerm Syntax.Data_Name -> Phantoms.TTerm Syntax.Defn_Object
defn_ObjectWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Object"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

defn_RepeatedEnumCase :: Phantoms.TTerm [Syntax.Mod] -> Phantoms.TTerm [Syntax.Data_Name] -> Phantoms.TTerm Syntax.Defn_RepeatedEnumCase
defn_RepeatedEnumCase mods cases =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_RepeatedEnumCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Phantoms.unTTerm mods)},
        Core.Field {
          Core.fieldName = (Core.Name "cases"),
          Core.fieldTerm = (Phantoms.unTTerm cases)}]}))

defn_RepeatedEnumCaseCases :: Phantoms.TTerm Syntax.Defn_RepeatedEnumCase -> Phantoms.TTerm [Syntax.Data_Name]
defn_RepeatedEnumCaseCases x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_RepeatedEnumCase"),
        Core.projectionField = (Core.Name "cases")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

defn_RepeatedEnumCaseMods :: Phantoms.TTerm Syntax.Defn_RepeatedEnumCase -> Phantoms.TTerm [Syntax.Mod]
defn_RepeatedEnumCaseMods x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_RepeatedEnumCase"),
        Core.projectionField = (Core.Name "mods")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

defn_RepeatedEnumCaseWithCases :: Phantoms.TTerm Syntax.Defn_RepeatedEnumCase -> Phantoms.TTerm [Syntax.Data_Name] -> Phantoms.TTerm Syntax.Defn_RepeatedEnumCase
defn_RepeatedEnumCaseWithCases original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_RepeatedEnumCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_RepeatedEnumCase"),
              Core.projectionField = (Core.Name "mods")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "cases"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

defn_RepeatedEnumCaseWithMods :: Phantoms.TTerm Syntax.Defn_RepeatedEnumCase -> Phantoms.TTerm [Syntax.Mod] -> Phantoms.TTerm Syntax.Defn_RepeatedEnumCase
defn_RepeatedEnumCaseWithMods original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_RepeatedEnumCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "cases"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_RepeatedEnumCase"),
              Core.projectionField = (Core.Name "cases")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

defn_Trait :: Phantoms.TTerm [Syntax.Mod] -> Phantoms.TTerm Syntax.Type_Name -> Phantoms.TTerm [Syntax.Type_Param] -> Phantoms.TTerm Syntax.Ctor_Primary -> Phantoms.TTerm Syntax.Template -> Phantoms.TTerm Syntax.Defn_Trait
defn_Trait mods name tparams ctor template =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Trait"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Phantoms.unTTerm mods)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Phantoms.unTTerm tparams)},
        Core.Field {
          Core.fieldName = (Core.Name "ctor"),
          Core.fieldTerm = (Phantoms.unTTerm ctor)},
        Core.Field {
          Core.fieldName = (Core.Name "template"),
          Core.fieldTerm = (Phantoms.unTTerm template)}]}))

defn_TraitCtor :: Phantoms.TTerm Syntax.Defn_Trait -> Phantoms.TTerm Syntax.Ctor_Primary
defn_TraitCtor x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Trait"),
        Core.projectionField = (Core.Name "ctor")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

defn_TraitMods :: Phantoms.TTerm Syntax.Defn_Trait -> Phantoms.TTerm [Syntax.Mod]
defn_TraitMods x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Trait"),
        Core.projectionField = (Core.Name "mods")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

defn_TraitName :: Phantoms.TTerm Syntax.Defn_Trait -> Phantoms.TTerm Syntax.Type_Name
defn_TraitName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Trait"),
        Core.projectionField = (Core.Name "name")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

defn_TraitTemplate :: Phantoms.TTerm Syntax.Defn_Trait -> Phantoms.TTerm Syntax.Template
defn_TraitTemplate x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Trait"),
        Core.projectionField = (Core.Name "template")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

defn_TraitTparams :: Phantoms.TTerm Syntax.Defn_Trait -> Phantoms.TTerm [Syntax.Type_Param]
defn_TraitTparams x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Trait"),
        Core.projectionField = (Core.Name "tparams")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

defn_TraitWithCtor :: Phantoms.TTerm Syntax.Defn_Trait -> Phantoms.TTerm Syntax.Ctor_Primary -> Phantoms.TTerm Syntax.Defn_Trait
defn_TraitWithCtor original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Trait"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Trait"),
              Core.projectionField = (Core.Name "mods")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Trait"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Trait"),
              Core.projectionField = (Core.Name "tparams")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ctor"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "template"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Trait"),
              Core.projectionField = (Core.Name "template")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

defn_TraitWithMods :: Phantoms.TTerm Syntax.Defn_Trait -> Phantoms.TTerm [Syntax.Mod] -> Phantoms.TTerm Syntax.Defn_Trait
defn_TraitWithMods original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Trait"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Trait"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Trait"),
              Core.projectionField = (Core.Name "tparams")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ctor"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Trait"),
              Core.projectionField = (Core.Name "ctor")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "template"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Trait"),
              Core.projectionField = (Core.Name "template")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

defn_TraitWithName :: Phantoms.TTerm Syntax.Defn_Trait -> Phantoms.TTerm Syntax.Type_Name -> Phantoms.TTerm Syntax.Defn_Trait
defn_TraitWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Trait"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Trait"),
              Core.projectionField = (Core.Name "mods")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Trait"),
              Core.projectionField = (Core.Name "tparams")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ctor"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Trait"),
              Core.projectionField = (Core.Name "ctor")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "template"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Trait"),
              Core.projectionField = (Core.Name "template")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

defn_TraitWithTemplate :: Phantoms.TTerm Syntax.Defn_Trait -> Phantoms.TTerm Syntax.Template -> Phantoms.TTerm Syntax.Defn_Trait
defn_TraitWithTemplate original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Trait"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Trait"),
              Core.projectionField = (Core.Name "mods")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Trait"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Trait"),
              Core.projectionField = (Core.Name "tparams")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ctor"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Trait"),
              Core.projectionField = (Core.Name "ctor")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "template"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

defn_TraitWithTparams :: Phantoms.TTerm Syntax.Defn_Trait -> Phantoms.TTerm [Syntax.Type_Param] -> Phantoms.TTerm Syntax.Defn_Trait
defn_TraitWithTparams original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Trait"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Trait"),
              Core.projectionField = (Core.Name "mods")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Trait"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "ctor"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Trait"),
              Core.projectionField = (Core.Name "ctor")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "template"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Trait"),
              Core.projectionField = (Core.Name "template")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

defn_Type :: Phantoms.TTerm [Syntax.Mod] -> Phantoms.TTerm Syntax.Type_Name -> Phantoms.TTerm [Syntax.Type_Param] -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.Defn_Type
defn_Type mods name tparams body =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Type"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Phantoms.unTTerm mods)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Phantoms.unTTerm tparams)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm body)}]}))

defn_TypeBody :: Phantoms.TTerm Syntax.Defn_Type -> Phantoms.TTerm Syntax.Type
defn_TypeBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Type"),
        Core.projectionField = (Core.Name "body")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

defn_TypeMods :: Phantoms.TTerm Syntax.Defn_Type -> Phantoms.TTerm [Syntax.Mod]
defn_TypeMods x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Type"),
        Core.projectionField = (Core.Name "mods")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

defn_TypeName :: Phantoms.TTerm Syntax.Defn_Type -> Phantoms.TTerm Syntax.Type_Name
defn_TypeName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Type"),
        Core.projectionField = (Core.Name "name")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

defn_TypeTparams :: Phantoms.TTerm Syntax.Defn_Type -> Phantoms.TTerm [Syntax.Type_Param]
defn_TypeTparams x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Type"),
        Core.projectionField = (Core.Name "tparams")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

defn_TypeWithBody :: Phantoms.TTerm Syntax.Defn_Type -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.Defn_Type
defn_TypeWithBody original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Type"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Type"),
              Core.projectionField = (Core.Name "mods")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Type"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Type"),
              Core.projectionField = (Core.Name "tparams")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

defn_TypeWithMods :: Phantoms.TTerm Syntax.Defn_Type -> Phantoms.TTerm [Syntax.Mod] -> Phantoms.TTerm Syntax.Defn_Type
defn_TypeWithMods original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Type"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Type"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Type"),
              Core.projectionField = (Core.Name "tparams")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Type"),
              Core.projectionField = (Core.Name "body")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

defn_TypeWithName :: Phantoms.TTerm Syntax.Defn_Type -> Phantoms.TTerm Syntax.Type_Name -> Phantoms.TTerm Syntax.Defn_Type
defn_TypeWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Type"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Type"),
              Core.projectionField = (Core.Name "mods")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Type"),
              Core.projectionField = (Core.Name "tparams")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Type"),
              Core.projectionField = (Core.Name "body")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

defn_TypeWithTparams :: Phantoms.TTerm Syntax.Defn_Type -> Phantoms.TTerm [Syntax.Type_Param] -> Phantoms.TTerm Syntax.Defn_Type
defn_TypeWithTparams original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Type"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Type"),
              Core.projectionField = (Core.Name "mods")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Type"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Type"),
              Core.projectionField = (Core.Name "body")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

defn_Val :: Phantoms.TTerm [Syntax.Mod] -> Phantoms.TTerm [Syntax.Pat] -> Phantoms.TTerm (Maybe Syntax.Type) -> Phantoms.TTerm Syntax.Data -> Phantoms.TTerm Syntax.Defn_Val
defn_Val mods pats decltpe rhs =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Val"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Phantoms.unTTerm mods)},
        Core.Field {
          Core.fieldName = (Core.Name "pats"),
          Core.fieldTerm = (Phantoms.unTTerm pats)},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Phantoms.unTTerm decltpe)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTTerm rhs)}]}))

defn_ValDecltpe :: Phantoms.TTerm Syntax.Defn_Val -> Phantoms.TTerm (Maybe Syntax.Type)
defn_ValDecltpe x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Val"),
        Core.projectionField = (Core.Name "decltpe")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

defn_ValMods :: Phantoms.TTerm Syntax.Defn_Val -> Phantoms.TTerm [Syntax.Mod]
defn_ValMods x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Val"),
        Core.projectionField = (Core.Name "mods")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

defn_ValPats :: Phantoms.TTerm Syntax.Defn_Val -> Phantoms.TTerm [Syntax.Pat]
defn_ValPats x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Val"),
        Core.projectionField = (Core.Name "pats")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

defn_ValRhs :: Phantoms.TTerm Syntax.Defn_Val -> Phantoms.TTerm Syntax.Data
defn_ValRhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Val"),
        Core.projectionField = (Core.Name "rhs")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

defn_ValWithDecltpe :: Phantoms.TTerm Syntax.Defn_Val -> Phantoms.TTerm (Maybe Syntax.Type) -> Phantoms.TTerm Syntax.Defn_Val
defn_ValWithDecltpe original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Val"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Val"),
              Core.projectionField = (Core.Name "mods")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "pats"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Val"),
              Core.projectionField = (Core.Name "pats")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Val"),
              Core.projectionField = (Core.Name "rhs")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

defn_ValWithMods :: Phantoms.TTerm Syntax.Defn_Val -> Phantoms.TTerm [Syntax.Mod] -> Phantoms.TTerm Syntax.Defn_Val
defn_ValWithMods original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Val"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "pats"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Val"),
              Core.projectionField = (Core.Name "pats")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Val"),
              Core.projectionField = (Core.Name "decltpe")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Val"),
              Core.projectionField = (Core.Name "rhs")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

defn_ValWithPats :: Phantoms.TTerm Syntax.Defn_Val -> Phantoms.TTerm [Syntax.Pat] -> Phantoms.TTerm Syntax.Defn_Val
defn_ValWithPats original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Val"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Val"),
              Core.projectionField = (Core.Name "mods")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "pats"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Val"),
              Core.projectionField = (Core.Name "decltpe")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Val"),
              Core.projectionField = (Core.Name "rhs")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

defn_ValWithRhs :: Phantoms.TTerm Syntax.Defn_Val -> Phantoms.TTerm Syntax.Data -> Phantoms.TTerm Syntax.Defn_Val
defn_ValWithRhs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Val"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Val"),
              Core.projectionField = (Core.Name "mods")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "pats"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Val"),
              Core.projectionField = (Core.Name "pats")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Val"),
              Core.projectionField = (Core.Name "decltpe")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

defn_Var :: Phantoms.TTerm [Syntax.Mod] -> Phantoms.TTerm [Syntax.Pat] -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm (Maybe Syntax.Data) -> Phantoms.TTerm Syntax.Defn_Var
defn_Var mods pats decltpe rhs =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Var"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Phantoms.unTTerm mods)},
        Core.Field {
          Core.fieldName = (Core.Name "pats"),
          Core.fieldTerm = (Phantoms.unTTerm pats)},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Phantoms.unTTerm decltpe)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTTerm rhs)}]}))

defn_VarDecltpe :: Phantoms.TTerm Syntax.Defn_Var -> Phantoms.TTerm Syntax.Type
defn_VarDecltpe x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Var"),
        Core.projectionField = (Core.Name "decltpe")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

defn_VarMods :: Phantoms.TTerm Syntax.Defn_Var -> Phantoms.TTerm [Syntax.Mod]
defn_VarMods x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Var"),
        Core.projectionField = (Core.Name "mods")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

defn_VarPats :: Phantoms.TTerm Syntax.Defn_Var -> Phantoms.TTerm [Syntax.Pat]
defn_VarPats x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Var"),
        Core.projectionField = (Core.Name "pats")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

defn_VarRhs :: Phantoms.TTerm Syntax.Defn_Var -> Phantoms.TTerm (Maybe Syntax.Data)
defn_VarRhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Var"),
        Core.projectionField = (Core.Name "rhs")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

defn_VarWithDecltpe :: Phantoms.TTerm Syntax.Defn_Var -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.Defn_Var
defn_VarWithDecltpe original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Var"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Var"),
              Core.projectionField = (Core.Name "mods")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "pats"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Var"),
              Core.projectionField = (Core.Name "pats")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Var"),
              Core.projectionField = (Core.Name "rhs")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

defn_VarWithMods :: Phantoms.TTerm Syntax.Defn_Var -> Phantoms.TTerm [Syntax.Mod] -> Phantoms.TTerm Syntax.Defn_Var
defn_VarWithMods original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Var"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "pats"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Var"),
              Core.projectionField = (Core.Name "pats")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Var"),
              Core.projectionField = (Core.Name "decltpe")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Var"),
              Core.projectionField = (Core.Name "rhs")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

defn_VarWithPats :: Phantoms.TTerm Syntax.Defn_Var -> Phantoms.TTerm [Syntax.Pat] -> Phantoms.TTerm Syntax.Defn_Var
defn_VarWithPats original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Var"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Var"),
              Core.projectionField = (Core.Name "mods")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "pats"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Var"),
              Core.projectionField = (Core.Name "decltpe")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Var"),
              Core.projectionField = (Core.Name "rhs")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

defn_VarWithRhs :: Phantoms.TTerm Syntax.Defn_Var -> Phantoms.TTerm (Maybe Syntax.Data) -> Phantoms.TTerm Syntax.Defn_Var
defn_VarWithRhs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Var"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Var"),
              Core.projectionField = (Core.Name "mods")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "pats"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Var"),
              Core.projectionField = (Core.Name "pats")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Defn_Var"),
              Core.projectionField = (Core.Name "decltpe")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

enumeratorCaseGenerator :: Phantoms.TTerm Syntax.Enumerator_CaseGenerator -> Phantoms.TTerm Syntax.Enumerator
enumeratorCaseGenerator x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Enumerator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "caseGenerator"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

enumeratorGenerator :: Phantoms.TTerm Syntax.Enumerator_Generator -> Phantoms.TTerm Syntax.Enumerator
enumeratorGenerator x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Enumerator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "generator"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

enumeratorGuard :: Phantoms.TTerm Syntax.Enumerator_Guard -> Phantoms.TTerm Syntax.Enumerator
enumeratorGuard x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Enumerator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "guard"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

enumeratorVal :: Phantoms.TTerm Syntax.Enumerator_Val -> Phantoms.TTerm Syntax.Enumerator
enumeratorVal x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Enumerator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "val"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

enumerator_CaseGenerator :: Phantoms.TTerm Syntax.Pat -> Phantoms.TTerm Syntax.Data -> Phantoms.TTerm Syntax.Enumerator_CaseGenerator
enumerator_CaseGenerator pat rhs =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Enumerator_CaseGenerator"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pat"),
          Core.fieldTerm = (Phantoms.unTTerm pat)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTTerm rhs)}]}))

enumerator_CaseGeneratorPat :: Phantoms.TTerm Syntax.Enumerator_CaseGenerator -> Phantoms.TTerm Syntax.Pat
enumerator_CaseGeneratorPat x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Enumerator_CaseGenerator"),
        Core.projectionField = (Core.Name "pat")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

enumerator_CaseGeneratorRhs :: Phantoms.TTerm Syntax.Enumerator_CaseGenerator -> Phantoms.TTerm Syntax.Data
enumerator_CaseGeneratorRhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Enumerator_CaseGenerator"),
        Core.projectionField = (Core.Name "rhs")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

enumerator_CaseGeneratorWithPat :: Phantoms.TTerm Syntax.Enumerator_CaseGenerator -> Phantoms.TTerm Syntax.Pat -> Phantoms.TTerm Syntax.Enumerator_CaseGenerator
enumerator_CaseGeneratorWithPat original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Enumerator_CaseGenerator"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pat"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Enumerator_CaseGenerator"),
              Core.projectionField = (Core.Name "rhs")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

enumerator_CaseGeneratorWithRhs :: Phantoms.TTerm Syntax.Enumerator_CaseGenerator -> Phantoms.TTerm Syntax.Data -> Phantoms.TTerm Syntax.Enumerator_CaseGenerator
enumerator_CaseGeneratorWithRhs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Enumerator_CaseGenerator"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pat"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Enumerator_CaseGenerator"),
              Core.projectionField = (Core.Name "pat")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

enumerator_Generator :: Phantoms.TTerm Syntax.Pat -> Phantoms.TTerm Syntax.Data -> Phantoms.TTerm Syntax.Enumerator_Generator
enumerator_Generator pat rhs =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Enumerator_Generator"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pat"),
          Core.fieldTerm = (Phantoms.unTTerm pat)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTTerm rhs)}]}))

enumerator_GeneratorPat :: Phantoms.TTerm Syntax.Enumerator_Generator -> Phantoms.TTerm Syntax.Pat
enumerator_GeneratorPat x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Enumerator_Generator"),
        Core.projectionField = (Core.Name "pat")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

enumerator_GeneratorRhs :: Phantoms.TTerm Syntax.Enumerator_Generator -> Phantoms.TTerm Syntax.Data
enumerator_GeneratorRhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Enumerator_Generator"),
        Core.projectionField = (Core.Name "rhs")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

enumerator_GeneratorWithPat :: Phantoms.TTerm Syntax.Enumerator_Generator -> Phantoms.TTerm Syntax.Pat -> Phantoms.TTerm Syntax.Enumerator_Generator
enumerator_GeneratorWithPat original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Enumerator_Generator"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pat"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Enumerator_Generator"),
              Core.projectionField = (Core.Name "rhs")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

enumerator_GeneratorWithRhs :: Phantoms.TTerm Syntax.Enumerator_Generator -> Phantoms.TTerm Syntax.Data -> Phantoms.TTerm Syntax.Enumerator_Generator
enumerator_GeneratorWithRhs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Enumerator_Generator"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pat"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Enumerator_Generator"),
              Core.projectionField = (Core.Name "pat")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

enumerator_Guard :: Phantoms.TTerm Syntax.Data -> Phantoms.TTerm Syntax.Enumerator_Guard
enumerator_Guard cond =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Enumerator_Guard"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "cond"),
          Core.fieldTerm = (Phantoms.unTTerm cond)}]}))

enumerator_GuardCond :: Phantoms.TTerm Syntax.Enumerator_Guard -> Phantoms.TTerm Syntax.Data
enumerator_GuardCond x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Enumerator_Guard"),
        Core.projectionField = (Core.Name "cond")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

enumerator_GuardWithCond :: Phantoms.TTerm Syntax.Enumerator_Guard -> Phantoms.TTerm Syntax.Data -> Phantoms.TTerm Syntax.Enumerator_Guard
enumerator_GuardWithCond original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Enumerator_Guard"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "cond"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

enumerator_Val :: Phantoms.TTerm Syntax.Pat -> Phantoms.TTerm Syntax.Data -> Phantoms.TTerm Syntax.Enumerator_Val
enumerator_Val pat rhs =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Enumerator_Val"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pat"),
          Core.fieldTerm = (Phantoms.unTTerm pat)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTTerm rhs)}]}))

enumerator_ValPat :: Phantoms.TTerm Syntax.Enumerator_Val -> Phantoms.TTerm Syntax.Pat
enumerator_ValPat x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Enumerator_Val"),
        Core.projectionField = (Core.Name "pat")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

enumerator_ValRhs :: Phantoms.TTerm Syntax.Enumerator_Val -> Phantoms.TTerm Syntax.Data
enumerator_ValRhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Enumerator_Val"),
        Core.projectionField = (Core.Name "rhs")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

enumerator_ValWithPat :: Phantoms.TTerm Syntax.Enumerator_Val -> Phantoms.TTerm Syntax.Pat -> Phantoms.TTerm Syntax.Enumerator_Val
enumerator_ValWithPat original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Enumerator_Val"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pat"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Enumerator_Val"),
              Core.projectionField = (Core.Name "rhs")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

enumerator_ValWithRhs :: Phantoms.TTerm Syntax.Enumerator_Val -> Phantoms.TTerm Syntax.Data -> Phantoms.TTerm Syntax.Enumerator_Val
enumerator_ValWithRhs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Enumerator_Val"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pat"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Enumerator_Val"),
              Core.projectionField = (Core.Name "pat")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

export :: Phantoms.TTerm [Syntax.Importer] -> Phantoms.TTerm Syntax.Export
export importers =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Export"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "importers"),
          Core.fieldTerm = (Phantoms.unTTerm importers)}]}))

exportImporters :: Phantoms.TTerm Syntax.Export -> Phantoms.TTerm [Syntax.Importer]
exportImporters x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Export"),
        Core.projectionField = (Core.Name "importers")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

exportWithImporters :: Phantoms.TTerm Syntax.Export -> Phantoms.TTerm [Syntax.Importer] -> Phantoms.TTerm Syntax.Export
exportWithImporters original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Export"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "importers"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

import_ :: Phantoms.TTerm [Syntax.Importer] -> Phantoms.TTerm Syntax.Import
import_ importers =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Import"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "importers"),
          Core.fieldTerm = (Phantoms.unTTerm importers)}]}))

importExportStatExport :: Phantoms.TTerm Syntax.Export -> Phantoms.TTerm Syntax.ImportExportStat
importExportStatExport x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.ImportExportStat"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "export"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

importExportStatImport :: Phantoms.TTerm Syntax.Import -> Phantoms.TTerm Syntax.ImportExportStat
importExportStatImport x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.ImportExportStat"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "import"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

importImporters :: Phantoms.TTerm Syntax.Import -> Phantoms.TTerm [Syntax.Importer]
importImporters x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Import"),
        Core.projectionField = (Core.Name "importers")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

importWithImporters :: Phantoms.TTerm Syntax.Import -> Phantoms.TTerm [Syntax.Importer] -> Phantoms.TTerm Syntax.Import
importWithImporters original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Import"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "importers"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

importeeGiven :: Phantoms.TTerm Syntax.Importee_Given -> Phantoms.TTerm Syntax.Importee
importeeGiven x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Importee"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "given"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

importeeGivenAll :: Phantoms.TTerm Syntax.Importee
importeeGivenAll =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Importee"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "givenAll"),
        Core.fieldTerm = Core.TermUnit}}))

importeeName :: Phantoms.TTerm Syntax.Importee_Name -> Phantoms.TTerm Syntax.Importee
importeeName x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Importee"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "name"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

importeeRename :: Phantoms.TTerm Syntax.Importee_Rename -> Phantoms.TTerm Syntax.Importee
importeeRename x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Importee"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "rename"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

importeeUnimport :: Phantoms.TTerm Syntax.Importee_Unimport -> Phantoms.TTerm Syntax.Importee
importeeUnimport x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Importee"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "unimport"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

importeeWildcard :: Phantoms.TTerm Syntax.Importee
importeeWildcard =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Importee"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "wildcard"),
        Core.fieldTerm = Core.TermUnit}}))

importee_Given :: Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.Importee_Given
importee_Given tpe =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Importee_Given"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "tpe"),
          Core.fieldTerm = (Phantoms.unTTerm tpe)}]}))

importee_GivenTpe :: Phantoms.TTerm Syntax.Importee_Given -> Phantoms.TTerm Syntax.Type
importee_GivenTpe x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Importee_Given"),
        Core.projectionField = (Core.Name "tpe")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

importee_GivenWithTpe :: Phantoms.TTerm Syntax.Importee_Given -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.Importee_Given
importee_GivenWithTpe original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Importee_Given"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "tpe"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

importee_Name :: Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.Importee_Name
importee_Name name =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Importee_Name"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)}]}))

importee_NameName :: Phantoms.TTerm Syntax.Importee_Name -> Phantoms.TTerm Syntax.Name
importee_NameName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Importee_Name"),
        Core.projectionField = (Core.Name "name")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

importee_NameWithName :: Phantoms.TTerm Syntax.Importee_Name -> Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.Importee_Name
importee_NameWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Importee_Name"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

importee_Rename :: Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.Importee_Rename
importee_Rename name rename =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Importee_Rename"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "rename"),
          Core.fieldTerm = (Phantoms.unTTerm rename)}]}))

importee_RenameName :: Phantoms.TTerm Syntax.Importee_Rename -> Phantoms.TTerm Syntax.Name
importee_RenameName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Importee_Rename"),
        Core.projectionField = (Core.Name "name")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

importee_RenameRename :: Phantoms.TTerm Syntax.Importee_Rename -> Phantoms.TTerm Syntax.Name
importee_RenameRename x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Importee_Rename"),
        Core.projectionField = (Core.Name "rename")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

importee_RenameWithName :: Phantoms.TTerm Syntax.Importee_Rename -> Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.Importee_Rename
importee_RenameWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Importee_Rename"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "rename"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Importee_Rename"),
              Core.projectionField = (Core.Name "rename")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

importee_RenameWithRename :: Phantoms.TTerm Syntax.Importee_Rename -> Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.Importee_Rename
importee_RenameWithRename original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Importee_Rename"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Importee_Rename"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rename"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

importee_Unimport :: Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.Importee_Unimport
importee_Unimport name =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Importee_Unimport"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)}]}))

importee_UnimportName :: Phantoms.TTerm Syntax.Importee_Unimport -> Phantoms.TTerm Syntax.Name
importee_UnimportName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Importee_Unimport"),
        Core.projectionField = (Core.Name "name")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

importee_UnimportWithName :: Phantoms.TTerm Syntax.Importee_Unimport -> Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.Importee_Unimport
importee_UnimportWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Importee_Unimport"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

importer :: Phantoms.TTerm Syntax.Data_Ref -> Phantoms.TTerm [Syntax.Importee] -> Phantoms.TTerm Syntax.Importer
importer ref importees =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Importer"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "ref"),
          Core.fieldTerm = (Phantoms.unTTerm ref)},
        Core.Field {
          Core.fieldName = (Core.Name "importees"),
          Core.fieldTerm = (Phantoms.unTTerm importees)}]}))

importerImportees :: Phantoms.TTerm Syntax.Importer -> Phantoms.TTerm [Syntax.Importee]
importerImportees x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Importer"),
        Core.projectionField = (Core.Name "importees")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

importerRef :: Phantoms.TTerm Syntax.Importer -> Phantoms.TTerm Syntax.Data_Ref
importerRef x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Importer"),
        Core.projectionField = (Core.Name "ref")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

importerWithImportees :: Phantoms.TTerm Syntax.Importer -> Phantoms.TTerm [Syntax.Importee] -> Phantoms.TTerm Syntax.Importer
importerWithImportees original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Importer"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "ref"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Importer"),
              Core.projectionField = (Core.Name "ref")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "importees"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

importerWithRef :: Phantoms.TTerm Syntax.Importer -> Phantoms.TTerm Syntax.Data_Ref -> Phantoms.TTerm Syntax.Importer
importerWithRef original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Importer"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "ref"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "importees"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Importer"),
              Core.projectionField = (Core.Name "importees")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

init :: Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.Name -> Phantoms.TTerm [[Syntax.Data]] -> Phantoms.TTerm Syntax.Init
init tpe name argss =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Init"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "tpe"),
          Core.fieldTerm = (Phantoms.unTTerm tpe)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "argss"),
          Core.fieldTerm = (Phantoms.unTTerm argss)}]}))

initArgss :: Phantoms.TTerm Syntax.Init -> Phantoms.TTerm [[Syntax.Data]]
initArgss x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Init"),
        Core.projectionField = (Core.Name "argss")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

initName :: Phantoms.TTerm Syntax.Init -> Phantoms.TTerm Syntax.Name
initName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Init"),
        Core.projectionField = (Core.Name "name")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

initTpe :: Phantoms.TTerm Syntax.Init -> Phantoms.TTerm Syntax.Type
initTpe x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Init"),
        Core.projectionField = (Core.Name "tpe")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

initWithArgss :: Phantoms.TTerm Syntax.Init -> Phantoms.TTerm [[Syntax.Data]] -> Phantoms.TTerm Syntax.Init
initWithArgss original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Init"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "tpe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Init"),
              Core.projectionField = (Core.Name "tpe")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Init"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "argss"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

initWithName :: Phantoms.TTerm Syntax.Init -> Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.Init
initWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Init"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "tpe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Init"),
              Core.projectionField = (Core.Name "tpe")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "argss"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Init"),
              Core.projectionField = (Core.Name "argss")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

initWithTpe :: Phantoms.TTerm Syntax.Init -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.Init
initWithTpe original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Init"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "tpe"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Init"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "argss"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Init"),
              Core.projectionField = (Core.Name "argss")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

litBoolean :: Phantoms.TTerm Bool -> Phantoms.TTerm Syntax.Lit
litBoolean x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Lit"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "boolean"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

litByte :: Phantoms.TTerm I.Int8 -> Phantoms.TTerm Syntax.Lit
litByte x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Lit"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "byte"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

litBytes :: Phantoms.TTerm [Int] -> Phantoms.TTerm Syntax.Lit
litBytes x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Lit"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "bytes"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

litChar :: Phantoms.TTerm Int -> Phantoms.TTerm Syntax.Lit
litChar x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Lit"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "char"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

litDouble :: Phantoms.TTerm Double -> Phantoms.TTerm Syntax.Lit
litDouble x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Lit"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "double"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

litFloat :: Phantoms.TTerm Float -> Phantoms.TTerm Syntax.Lit
litFloat x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Lit"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "float"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

litInt :: Phantoms.TTerm Int -> Phantoms.TTerm Syntax.Lit
litInt x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Lit"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "int"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

litLong :: Phantoms.TTerm I.Int64 -> Phantoms.TTerm Syntax.Lit
litLong x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Lit"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "long"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

litNull :: Phantoms.TTerm Syntax.Lit
litNull =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Lit"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "null"),
        Core.fieldTerm = Core.TermUnit}}))

litShort :: Phantoms.TTerm I.Int16 -> Phantoms.TTerm Syntax.Lit
litShort x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Lit"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "short"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

litString :: Phantoms.TTerm String -> Phantoms.TTerm Syntax.Lit
litString x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Lit"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "string"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

litSymbol :: Phantoms.TTerm Syntax.ScalaSymbol -> Phantoms.TTerm Syntax.Lit
litSymbol x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Lit"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "symbol"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

litUnit :: Phantoms.TTerm Syntax.Lit
litUnit =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Lit"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "unit"),
        Core.fieldTerm = Core.TermUnit}}))

memberSelf :: Phantoms.TTerm Syntax.Self -> Phantoms.TTerm Syntax.Member
memberSelf x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Member"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "self"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

memberTerm :: Phantoms.TTerm Syntax.Member_Data -> Phantoms.TTerm Syntax.Member
memberTerm x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Member"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "term"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

memberTermParam :: Phantoms.TTerm Syntax.Data_Param -> Phantoms.TTerm Syntax.Member
memberTermParam x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Member"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "termParam"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

memberType :: Phantoms.TTerm Syntax.Member_Type -> Phantoms.TTerm Syntax.Member
memberType x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Member"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "type"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

memberTypeParam :: Phantoms.TTerm Syntax.Type_Param -> Phantoms.TTerm Syntax.Member
memberTypeParam x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Member"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "typeParam"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

member_DataObject :: Phantoms.TTerm Syntax.Pkg_Object -> Phantoms.TTerm Syntax.Member_Data
member_DataObject x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Member_Data"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "object"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

member_DataPkg :: Phantoms.TTerm Syntax.Pkg -> Phantoms.TTerm Syntax.Member_Data
member_DataPkg x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Member_Data"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "pkg"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

member_Type :: Phantoms.TTerm Syntax.Type_Name -> Phantoms.TTerm Syntax.Member_Type
member_Type name =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Member_Type"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)}]}))

member_TypeName :: Phantoms.TTerm Syntax.Member_Type -> Phantoms.TTerm Syntax.Type_Name
member_TypeName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Member_Type"),
        Core.projectionField = (Core.Name "name")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

member_TypeWithName :: Phantoms.TTerm Syntax.Member_Type -> Phantoms.TTerm Syntax.Type_Name -> Phantoms.TTerm Syntax.Member_Type
member_TypeWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Member_Type"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

modAbstract :: Phantoms.TTerm Syntax.Mod
modAbstract =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Mod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "abstract"),
        Core.fieldTerm = Core.TermUnit}}))

modAnnot :: Phantoms.TTerm Syntax.Mod_Annot -> Phantoms.TTerm Syntax.Mod
modAnnot x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Mod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "annot"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

modCase :: Phantoms.TTerm Syntax.Mod
modCase =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Mod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "case"),
        Core.fieldTerm = Core.TermUnit}}))

modContravariant :: Phantoms.TTerm Syntax.Mod
modContravariant =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Mod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "contravariant"),
        Core.fieldTerm = Core.TermUnit}}))

modCovariant :: Phantoms.TTerm Syntax.Mod
modCovariant =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Mod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "covariant"),
        Core.fieldTerm = Core.TermUnit}}))

modFinal :: Phantoms.TTerm Syntax.Mod
modFinal =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Mod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "final"),
        Core.fieldTerm = Core.TermUnit}}))

modImplicit :: Phantoms.TTerm Syntax.Mod
modImplicit =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Mod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "implicit"),
        Core.fieldTerm = Core.TermUnit}}))

modInfix :: Phantoms.TTerm Syntax.Mod
modInfix =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Mod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "infix"),
        Core.fieldTerm = Core.TermUnit}}))

modInline :: Phantoms.TTerm Syntax.Mod
modInline =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Mod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "inline"),
        Core.fieldTerm = Core.TermUnit}}))

modLazy :: Phantoms.TTerm Syntax.Mod
modLazy =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Mod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "lazy"),
        Core.fieldTerm = Core.TermUnit}}))

modOpaque :: Phantoms.TTerm Syntax.Mod
modOpaque =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Mod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "opaque"),
        Core.fieldTerm = Core.TermUnit}}))

modOpen :: Phantoms.TTerm Syntax.Mod
modOpen =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Mod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "open"),
        Core.fieldTerm = Core.TermUnit}}))

modOverride :: Phantoms.TTerm Syntax.Mod
modOverride =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Mod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "override"),
        Core.fieldTerm = Core.TermUnit}}))

modPrivate :: Phantoms.TTerm Syntax.Mod_Private -> Phantoms.TTerm Syntax.Mod
modPrivate x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Mod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "private"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

modProtected :: Phantoms.TTerm Syntax.Mod_Protected -> Phantoms.TTerm Syntax.Mod
modProtected x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Mod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "protected"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

modSealed :: Phantoms.TTerm Syntax.Mod
modSealed =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Mod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "sealed"),
        Core.fieldTerm = Core.TermUnit}}))

modSuper :: Phantoms.TTerm Syntax.Mod
modSuper =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Mod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "super"),
        Core.fieldTerm = Core.TermUnit}}))

modTransparent :: Phantoms.TTerm Syntax.Mod
modTransparent =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Mod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "transparent"),
        Core.fieldTerm = Core.TermUnit}}))

modUsing :: Phantoms.TTerm Syntax.Mod
modUsing =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Mod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "using"),
        Core.fieldTerm = Core.TermUnit}}))

modValParam :: Phantoms.TTerm Syntax.Mod
modValParam =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Mod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "valParam"),
        Core.fieldTerm = Core.TermUnit}}))

modVarParam :: Phantoms.TTerm Syntax.Mod
modVarParam =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Mod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "varParam"),
        Core.fieldTerm = Core.TermUnit}}))

mod_Annot :: Phantoms.TTerm Syntax.Init -> Phantoms.TTerm Syntax.Mod_Annot
mod_Annot init =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Mod_Annot"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "init"),
          Core.fieldTerm = (Phantoms.unTTerm init)}]}))

mod_AnnotInit :: Phantoms.TTerm Syntax.Mod_Annot -> Phantoms.TTerm Syntax.Init
mod_AnnotInit x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Mod_Annot"),
        Core.projectionField = (Core.Name "init")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

mod_AnnotWithInit :: Phantoms.TTerm Syntax.Mod_Annot -> Phantoms.TTerm Syntax.Init -> Phantoms.TTerm Syntax.Mod_Annot
mod_AnnotWithInit original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Mod_Annot"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "init"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

mod_Private :: Phantoms.TTerm Syntax.Ref -> Phantoms.TTerm Syntax.Mod_Private
mod_Private within =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Mod_Private"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "within"),
          Core.fieldTerm = (Phantoms.unTTerm within)}]}))

mod_PrivateWithWithin :: Phantoms.TTerm Syntax.Mod_Private -> Phantoms.TTerm Syntax.Ref -> Phantoms.TTerm Syntax.Mod_Private
mod_PrivateWithWithin original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Mod_Private"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "within"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

mod_PrivateWithin :: Phantoms.TTerm Syntax.Mod_Private -> Phantoms.TTerm Syntax.Ref
mod_PrivateWithin x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Mod_Private"),
        Core.projectionField = (Core.Name "within")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

mod_Protected :: Phantoms.TTerm Syntax.Ref -> Phantoms.TTerm Syntax.Mod_Protected
mod_Protected within =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Mod_Protected"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "within"),
          Core.fieldTerm = (Phantoms.unTTerm within)}]}))

mod_ProtectedWithWithin :: Phantoms.TTerm Syntax.Mod_Protected -> Phantoms.TTerm Syntax.Ref -> Phantoms.TTerm Syntax.Mod_Protected
mod_ProtectedWithWithin original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Mod_Protected"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "within"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

mod_ProtectedWithin :: Phantoms.TTerm Syntax.Mod_Protected -> Phantoms.TTerm Syntax.Ref
mod_ProtectedWithin x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Mod_Protected"),
        Core.projectionField = (Core.Name "within")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

nameAnonymous :: Phantoms.TTerm Syntax.Name
nameAnonymous =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Name"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "anonymous"),
        Core.fieldTerm = Core.TermUnit}}))

nameIndeterminate :: Phantoms.TTerm Syntax.PredefString -> Phantoms.TTerm Syntax.Name
nameIndeterminate x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Name"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "indeterminate"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

nameValue :: Phantoms.TTerm String -> Phantoms.TTerm Syntax.Name
nameValue x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Name"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "value"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

patAlternative :: Phantoms.TTerm Syntax.Pat_Alternative -> Phantoms.TTerm Syntax.Pat
patAlternative x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Pat"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "alternative"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

patBind :: Phantoms.TTerm Syntax.Pat_Bind -> Phantoms.TTerm Syntax.Pat
patBind x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Pat"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "bind"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

patExtract :: Phantoms.TTerm Syntax.Pat_Extract -> Phantoms.TTerm Syntax.Pat
patExtract x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Pat"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "extract"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

patExtractInfix :: Phantoms.TTerm Syntax.Pat_ExtractInfix -> Phantoms.TTerm Syntax.Pat
patExtractInfix x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Pat"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "extractInfix"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

patGiven :: Phantoms.TTerm Syntax.Pat_Given -> Phantoms.TTerm Syntax.Pat
patGiven x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Pat"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "given"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

patInterpolate :: Phantoms.TTerm Syntax.Pat_Interpolate -> Phantoms.TTerm Syntax.Pat
patInterpolate x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Pat"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "interpolate"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

patMacro :: Phantoms.TTerm Syntax.Pat_Macro -> Phantoms.TTerm Syntax.Pat
patMacro x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Pat"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "macro"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

patRepeated :: Phantoms.TTerm Syntax.Pat_Repeated -> Phantoms.TTerm Syntax.Pat
patRepeated x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Pat"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "repeated"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

patSeqWildcard :: Phantoms.TTerm Syntax.Pat
patSeqWildcard =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Pat"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "seqWildcard"),
        Core.fieldTerm = Core.TermUnit}}))

patTuple :: Phantoms.TTerm Syntax.Pat_Tuple -> Phantoms.TTerm Syntax.Pat
patTuple x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Pat"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "tuple"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

patTyped :: Phantoms.TTerm Syntax.Pat_Typed -> Phantoms.TTerm Syntax.Pat
patTyped x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Pat"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "typed"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

patVar :: Phantoms.TTerm Syntax.Pat_Var -> Phantoms.TTerm Syntax.Pat
patVar x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Pat"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "var"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

patWildcard :: Phantoms.TTerm Syntax.Pat
patWildcard =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Pat"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "wildcard"),
        Core.fieldTerm = Core.TermUnit}}))

patXml :: Phantoms.TTerm Syntax.Pat_Xml -> Phantoms.TTerm Syntax.Pat
patXml x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Pat"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "xml"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

pat_Alternative :: Phantoms.TTerm Syntax.Pat -> Phantoms.TTerm Syntax.Pat -> Phantoms.TTerm Syntax.Pat_Alternative
pat_Alternative lhs rhs =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Pat_Alternative"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Phantoms.unTTerm lhs)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTTerm rhs)}]}))

pat_AlternativeLhs :: Phantoms.TTerm Syntax.Pat_Alternative -> Phantoms.TTerm Syntax.Pat
pat_AlternativeLhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Pat_Alternative"),
        Core.projectionField = (Core.Name "lhs")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

pat_AlternativeRhs :: Phantoms.TTerm Syntax.Pat_Alternative -> Phantoms.TTerm Syntax.Pat
pat_AlternativeRhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Pat_Alternative"),
        Core.projectionField = (Core.Name "rhs")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

pat_AlternativeWithLhs :: Phantoms.TTerm Syntax.Pat_Alternative -> Phantoms.TTerm Syntax.Pat -> Phantoms.TTerm Syntax.Pat_Alternative
pat_AlternativeWithLhs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Pat_Alternative"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Pat_Alternative"),
              Core.projectionField = (Core.Name "rhs")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

pat_AlternativeWithRhs :: Phantoms.TTerm Syntax.Pat_Alternative -> Phantoms.TTerm Syntax.Pat -> Phantoms.TTerm Syntax.Pat_Alternative
pat_AlternativeWithRhs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Pat_Alternative"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Pat_Alternative"),
              Core.projectionField = (Core.Name "lhs")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

pat_Bind :: Phantoms.TTerm Syntax.Pat -> Phantoms.TTerm Syntax.Pat -> Phantoms.TTerm Syntax.Pat_Bind
pat_Bind lhs rhs =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Pat_Bind"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Phantoms.unTTerm lhs)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTTerm rhs)}]}))

pat_BindLhs :: Phantoms.TTerm Syntax.Pat_Bind -> Phantoms.TTerm Syntax.Pat
pat_BindLhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Pat_Bind"),
        Core.projectionField = (Core.Name "lhs")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

pat_BindRhs :: Phantoms.TTerm Syntax.Pat_Bind -> Phantoms.TTerm Syntax.Pat
pat_BindRhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Pat_Bind"),
        Core.projectionField = (Core.Name "rhs")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

pat_BindWithLhs :: Phantoms.TTerm Syntax.Pat_Bind -> Phantoms.TTerm Syntax.Pat -> Phantoms.TTerm Syntax.Pat_Bind
pat_BindWithLhs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Pat_Bind"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Pat_Bind"),
              Core.projectionField = (Core.Name "rhs")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

pat_BindWithRhs :: Phantoms.TTerm Syntax.Pat_Bind -> Phantoms.TTerm Syntax.Pat -> Phantoms.TTerm Syntax.Pat_Bind
pat_BindWithRhs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Pat_Bind"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Pat_Bind"),
              Core.projectionField = (Core.Name "lhs")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

pat_Extract :: Phantoms.TTerm Syntax.Data -> Phantoms.TTerm [Syntax.Pat] -> Phantoms.TTerm Syntax.Pat_Extract
pat_Extract fun args =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Pat_Extract"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "fun"),
          Core.fieldTerm = (Phantoms.unTTerm fun)},
        Core.Field {
          Core.fieldName = (Core.Name "args"),
          Core.fieldTerm = (Phantoms.unTTerm args)}]}))

pat_ExtractArgs :: Phantoms.TTerm Syntax.Pat_Extract -> Phantoms.TTerm [Syntax.Pat]
pat_ExtractArgs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Pat_Extract"),
        Core.projectionField = (Core.Name "args")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

pat_ExtractFun :: Phantoms.TTerm Syntax.Pat_Extract -> Phantoms.TTerm Syntax.Data
pat_ExtractFun x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Pat_Extract"),
        Core.projectionField = (Core.Name "fun")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

pat_ExtractInfix :: Phantoms.TTerm Syntax.Pat -> Phantoms.TTerm Syntax.Data_Name -> Phantoms.TTerm [Syntax.Pat] -> Phantoms.TTerm Syntax.Pat_ExtractInfix
pat_ExtractInfix lhs op rhs =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Pat_ExtractInfix"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Phantoms.unTTerm lhs)},
        Core.Field {
          Core.fieldName = (Core.Name "op"),
          Core.fieldTerm = (Phantoms.unTTerm op)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTTerm rhs)}]}))

pat_ExtractInfixLhs :: Phantoms.TTerm Syntax.Pat_ExtractInfix -> Phantoms.TTerm Syntax.Pat
pat_ExtractInfixLhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Pat_ExtractInfix"),
        Core.projectionField = (Core.Name "lhs")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

pat_ExtractInfixOp :: Phantoms.TTerm Syntax.Pat_ExtractInfix -> Phantoms.TTerm Syntax.Data_Name
pat_ExtractInfixOp x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Pat_ExtractInfix"),
        Core.projectionField = (Core.Name "op")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

pat_ExtractInfixRhs :: Phantoms.TTerm Syntax.Pat_ExtractInfix -> Phantoms.TTerm [Syntax.Pat]
pat_ExtractInfixRhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Pat_ExtractInfix"),
        Core.projectionField = (Core.Name "rhs")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

pat_ExtractInfixWithLhs :: Phantoms.TTerm Syntax.Pat_ExtractInfix -> Phantoms.TTerm Syntax.Pat -> Phantoms.TTerm Syntax.Pat_ExtractInfix
pat_ExtractInfixWithLhs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Pat_ExtractInfix"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "op"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Pat_ExtractInfix"),
              Core.projectionField = (Core.Name "op")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Pat_ExtractInfix"),
              Core.projectionField = (Core.Name "rhs")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

pat_ExtractInfixWithOp :: Phantoms.TTerm Syntax.Pat_ExtractInfix -> Phantoms.TTerm Syntax.Data_Name -> Phantoms.TTerm Syntax.Pat_ExtractInfix
pat_ExtractInfixWithOp original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Pat_ExtractInfix"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Pat_ExtractInfix"),
              Core.projectionField = (Core.Name "lhs")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "op"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Pat_ExtractInfix"),
              Core.projectionField = (Core.Name "rhs")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

pat_ExtractInfixWithRhs :: Phantoms.TTerm Syntax.Pat_ExtractInfix -> Phantoms.TTerm [Syntax.Pat] -> Phantoms.TTerm Syntax.Pat_ExtractInfix
pat_ExtractInfixWithRhs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Pat_ExtractInfix"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Pat_ExtractInfix"),
              Core.projectionField = (Core.Name "lhs")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "op"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Pat_ExtractInfix"),
              Core.projectionField = (Core.Name "op")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

pat_ExtractWithArgs :: Phantoms.TTerm Syntax.Pat_Extract -> Phantoms.TTerm [Syntax.Pat] -> Phantoms.TTerm Syntax.Pat_Extract
pat_ExtractWithArgs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Pat_Extract"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "fun"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Pat_Extract"),
              Core.projectionField = (Core.Name "fun")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "args"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

pat_ExtractWithFun :: Phantoms.TTerm Syntax.Pat_Extract -> Phantoms.TTerm Syntax.Data -> Phantoms.TTerm Syntax.Pat_Extract
pat_ExtractWithFun original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Pat_Extract"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "fun"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "args"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Pat_Extract"),
              Core.projectionField = (Core.Name "args")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

pat_Given :: Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.Pat_Given
pat_Given tpe =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Pat_Given"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "tpe"),
          Core.fieldTerm = (Phantoms.unTTerm tpe)}]}))

pat_GivenTpe :: Phantoms.TTerm Syntax.Pat_Given -> Phantoms.TTerm Syntax.Type
pat_GivenTpe x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Pat_Given"),
        Core.projectionField = (Core.Name "tpe")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

pat_GivenWithTpe :: Phantoms.TTerm Syntax.Pat_Given -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.Pat_Given
pat_GivenWithTpe original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Pat_Given"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "tpe"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

pat_Interpolate :: Phantoms.TTerm Syntax.Data_Name -> Phantoms.TTerm [Syntax.Lit] -> Phantoms.TTerm Syntax.Pat_Interpolate
pat_Interpolate prefix parts =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Pat_Interpolate"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "prefix"),
          Core.fieldTerm = (Phantoms.unTTerm prefix)},
        Core.Field {
          Core.fieldName = (Core.Name "parts"),
          Core.fieldTerm = (Phantoms.unTTerm parts)}]}))

pat_InterpolateParts :: Phantoms.TTerm Syntax.Pat_Interpolate -> Phantoms.TTerm [Syntax.Lit]
pat_InterpolateParts x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Pat_Interpolate"),
        Core.projectionField = (Core.Name "parts")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

pat_InterpolatePrefix :: Phantoms.TTerm Syntax.Pat_Interpolate -> Phantoms.TTerm Syntax.Data_Name
pat_InterpolatePrefix x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Pat_Interpolate"),
        Core.projectionField = (Core.Name "prefix")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

pat_InterpolateWithParts :: Phantoms.TTerm Syntax.Pat_Interpolate -> Phantoms.TTerm [Syntax.Lit] -> Phantoms.TTerm Syntax.Pat_Interpolate
pat_InterpolateWithParts original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Pat_Interpolate"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "prefix"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Pat_Interpolate"),
              Core.projectionField = (Core.Name "prefix")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "parts"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

pat_InterpolateWithPrefix :: Phantoms.TTerm Syntax.Pat_Interpolate -> Phantoms.TTerm Syntax.Data_Name -> Phantoms.TTerm Syntax.Pat_Interpolate
pat_InterpolateWithPrefix original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Pat_Interpolate"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "prefix"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "parts"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Pat_Interpolate"),
              Core.projectionField = (Core.Name "parts")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

pat_Macro :: Phantoms.TTerm Syntax.Data -> Phantoms.TTerm Syntax.Pat_Macro
pat_Macro body =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Pat_Macro"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm body)}]}))

pat_MacroBody :: Phantoms.TTerm Syntax.Pat_Macro -> Phantoms.TTerm Syntax.Data
pat_MacroBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Pat_Macro"),
        Core.projectionField = (Core.Name "body")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

pat_MacroWithBody :: Phantoms.TTerm Syntax.Pat_Macro -> Phantoms.TTerm Syntax.Data -> Phantoms.TTerm Syntax.Pat_Macro
pat_MacroWithBody original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Pat_Macro"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

pat_Repeated :: Phantoms.TTerm Syntax.Data_Name -> Phantoms.TTerm Syntax.Pat_Repeated
pat_Repeated name =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Pat_Repeated"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)}]}))

pat_RepeatedName :: Phantoms.TTerm Syntax.Pat_Repeated -> Phantoms.TTerm Syntax.Data_Name
pat_RepeatedName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Pat_Repeated"),
        Core.projectionField = (Core.Name "name")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

pat_RepeatedWithName :: Phantoms.TTerm Syntax.Pat_Repeated -> Phantoms.TTerm Syntax.Data_Name -> Phantoms.TTerm Syntax.Pat_Repeated
pat_RepeatedWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Pat_Repeated"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

pat_Tuple :: Phantoms.TTerm [Syntax.Pat] -> Phantoms.TTerm Syntax.Pat_Tuple
pat_Tuple args =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Pat_Tuple"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "args"),
          Core.fieldTerm = (Phantoms.unTTerm args)}]}))

pat_TupleArgs :: Phantoms.TTerm Syntax.Pat_Tuple -> Phantoms.TTerm [Syntax.Pat]
pat_TupleArgs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Pat_Tuple"),
        Core.projectionField = (Core.Name "args")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

pat_TupleWithArgs :: Phantoms.TTerm Syntax.Pat_Tuple -> Phantoms.TTerm [Syntax.Pat] -> Phantoms.TTerm Syntax.Pat_Tuple
pat_TupleWithArgs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Pat_Tuple"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "args"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

pat_Typed :: Phantoms.TTerm Syntax.Pat -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.Pat_Typed
pat_Typed lhs rhs =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Pat_Typed"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Phantoms.unTTerm lhs)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTTerm rhs)}]}))

pat_TypedLhs :: Phantoms.TTerm Syntax.Pat_Typed -> Phantoms.TTerm Syntax.Pat
pat_TypedLhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Pat_Typed"),
        Core.projectionField = (Core.Name "lhs")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

pat_TypedRhs :: Phantoms.TTerm Syntax.Pat_Typed -> Phantoms.TTerm Syntax.Type
pat_TypedRhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Pat_Typed"),
        Core.projectionField = (Core.Name "rhs")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

pat_TypedWithLhs :: Phantoms.TTerm Syntax.Pat_Typed -> Phantoms.TTerm Syntax.Pat -> Phantoms.TTerm Syntax.Pat_Typed
pat_TypedWithLhs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Pat_Typed"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Pat_Typed"),
              Core.projectionField = (Core.Name "rhs")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

pat_TypedWithRhs :: Phantoms.TTerm Syntax.Pat_Typed -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.Pat_Typed
pat_TypedWithRhs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Pat_Typed"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Pat_Typed"),
              Core.projectionField = (Core.Name "lhs")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

pat_Var :: Phantoms.TTerm Syntax.Data_Name -> Phantoms.TTerm Syntax.Pat_Var
pat_Var name =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Pat_Var"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)}]}))

pat_VarName :: Phantoms.TTerm Syntax.Pat_Var -> Phantoms.TTerm Syntax.Data_Name
pat_VarName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Pat_Var"),
        Core.projectionField = (Core.Name "name")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

pat_VarWithName :: Phantoms.TTerm Syntax.Pat_Var -> Phantoms.TTerm Syntax.Data_Name -> Phantoms.TTerm Syntax.Pat_Var
pat_VarWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Pat_Var"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

pat_Xml :: Phantoms.TTerm [Syntax.Lit] -> Phantoms.TTerm [Syntax.Pat] -> Phantoms.TTerm Syntax.Pat_Xml
pat_Xml parts args =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Pat_Xml"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "parts"),
          Core.fieldTerm = (Phantoms.unTTerm parts)},
        Core.Field {
          Core.fieldName = (Core.Name "args"),
          Core.fieldTerm = (Phantoms.unTTerm args)}]}))

pat_XmlArgs :: Phantoms.TTerm Syntax.Pat_Xml -> Phantoms.TTerm [Syntax.Pat]
pat_XmlArgs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Pat_Xml"),
        Core.projectionField = (Core.Name "args")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

pat_XmlParts :: Phantoms.TTerm Syntax.Pat_Xml -> Phantoms.TTerm [Syntax.Lit]
pat_XmlParts x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Pat_Xml"),
        Core.projectionField = (Core.Name "parts")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

pat_XmlWithArgs :: Phantoms.TTerm Syntax.Pat_Xml -> Phantoms.TTerm [Syntax.Pat] -> Phantoms.TTerm Syntax.Pat_Xml
pat_XmlWithArgs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Pat_Xml"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "parts"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Pat_Xml"),
              Core.projectionField = (Core.Name "parts")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "args"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

pat_XmlWithParts :: Phantoms.TTerm Syntax.Pat_Xml -> Phantoms.TTerm [Syntax.Lit] -> Phantoms.TTerm Syntax.Pat_Xml
pat_XmlWithParts original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Pat_Xml"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "parts"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "args"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Pat_Xml"),
              Core.projectionField = (Core.Name "args")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

pkg :: Phantoms.TTerm Syntax.Data_Name -> Phantoms.TTerm Syntax.Data_Ref -> Phantoms.TTerm [Syntax.Stat] -> Phantoms.TTerm Syntax.Pkg
pkg name ref stats =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Pkg"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "ref"),
          Core.fieldTerm = (Phantoms.unTTerm ref)},
        Core.Field {
          Core.fieldName = (Core.Name "stats"),
          Core.fieldTerm = (Phantoms.unTTerm stats)}]}))

pkgName :: Phantoms.TTerm Syntax.Pkg -> Phantoms.TTerm Syntax.Data_Name
pkgName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Pkg"),
        Core.projectionField = (Core.Name "name")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

pkgRef :: Phantoms.TTerm Syntax.Pkg -> Phantoms.TTerm Syntax.Data_Ref
pkgRef x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Pkg"),
        Core.projectionField = (Core.Name "ref")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

pkgStats :: Phantoms.TTerm Syntax.Pkg -> Phantoms.TTerm [Syntax.Stat]
pkgStats x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Pkg"),
        Core.projectionField = (Core.Name "stats")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

pkgWithName :: Phantoms.TTerm Syntax.Pkg -> Phantoms.TTerm Syntax.Data_Name -> Phantoms.TTerm Syntax.Pkg
pkgWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Pkg"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "ref"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Pkg"),
              Core.projectionField = (Core.Name "ref")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "stats"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Pkg"),
              Core.projectionField = (Core.Name "stats")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

pkgWithRef :: Phantoms.TTerm Syntax.Pkg -> Phantoms.TTerm Syntax.Data_Ref -> Phantoms.TTerm Syntax.Pkg
pkgWithRef original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Pkg"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Pkg"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ref"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "stats"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Pkg"),
              Core.projectionField = (Core.Name "stats")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

pkgWithStats :: Phantoms.TTerm Syntax.Pkg -> Phantoms.TTerm [Syntax.Stat] -> Phantoms.TTerm Syntax.Pkg
pkgWithStats original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Pkg"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Pkg"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ref"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Pkg"),
              Core.projectionField = (Core.Name "ref")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "stats"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

pkg_Object :: Phantoms.TTerm [Syntax.Mod] -> Phantoms.TTerm Syntax.Data_Name -> Phantoms.TTerm Syntax.Template -> Phantoms.TTerm Syntax.Pkg_Object
pkg_Object mods name template =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Pkg_Object"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Phantoms.unTTerm mods)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "template"),
          Core.fieldTerm = (Phantoms.unTTerm template)}]}))

pkg_ObjectMods :: Phantoms.TTerm Syntax.Pkg_Object -> Phantoms.TTerm [Syntax.Mod]
pkg_ObjectMods x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Pkg_Object"),
        Core.projectionField = (Core.Name "mods")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

pkg_ObjectName :: Phantoms.TTerm Syntax.Pkg_Object -> Phantoms.TTerm Syntax.Data_Name
pkg_ObjectName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Pkg_Object"),
        Core.projectionField = (Core.Name "name")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

pkg_ObjectTemplate :: Phantoms.TTerm Syntax.Pkg_Object -> Phantoms.TTerm Syntax.Template
pkg_ObjectTemplate x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Pkg_Object"),
        Core.projectionField = (Core.Name "template")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

pkg_ObjectWithMods :: Phantoms.TTerm Syntax.Pkg_Object -> Phantoms.TTerm [Syntax.Mod] -> Phantoms.TTerm Syntax.Pkg_Object
pkg_ObjectWithMods original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Pkg_Object"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Pkg_Object"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "template"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Pkg_Object"),
              Core.projectionField = (Core.Name "template")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

pkg_ObjectWithName :: Phantoms.TTerm Syntax.Pkg_Object -> Phantoms.TTerm Syntax.Data_Name -> Phantoms.TTerm Syntax.Pkg_Object
pkg_ObjectWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Pkg_Object"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Pkg_Object"),
              Core.projectionField = (Core.Name "mods")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "template"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Pkg_Object"),
              Core.projectionField = (Core.Name "template")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

pkg_ObjectWithTemplate :: Phantoms.TTerm Syntax.Pkg_Object -> Phantoms.TTerm Syntax.Template -> Phantoms.TTerm Syntax.Pkg_Object
pkg_ObjectWithTemplate original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Pkg_Object"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Pkg_Object"),
              Core.projectionField = (Core.Name "mods")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Pkg_Object"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "template"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

predefString :: Phantoms.TTerm String -> Phantoms.TTerm Syntax.PredefString
predefString x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.scala.syntax.PredefString"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

quasi :: Phantoms.TTerm () -> Phantoms.TTerm Syntax.Quasi
quasi x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.scala.syntax.Quasi"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

refInit :: Phantoms.TTerm Syntax.Init -> Phantoms.TTerm Syntax.Ref
refInit x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Ref"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "init"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

refName :: Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.Ref
refName x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Ref"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "name"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

scalaSymbol :: Phantoms.TTerm String -> Phantoms.TTerm Syntax.ScalaSymbol
scalaSymbol name =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.ScalaSymbol"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)}]}))

scalaSymbolName :: Phantoms.TTerm Syntax.ScalaSymbol -> Phantoms.TTerm String
scalaSymbolName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.ScalaSymbol"),
        Core.projectionField = (Core.Name "name")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

scalaSymbolWithName :: Phantoms.TTerm Syntax.ScalaSymbol -> Phantoms.TTerm String -> Phantoms.TTerm Syntax.ScalaSymbol
scalaSymbolWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.ScalaSymbol"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

self :: Phantoms.TTerm () -> Phantoms.TTerm Syntax.Self
self x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.scala.syntax.Self"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

source :: Phantoms.TTerm [Syntax.Stat] -> Phantoms.TTerm Syntax.Source
source stats =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Source"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "stats"),
          Core.fieldTerm = (Phantoms.unTTerm stats)}]}))

sourceStats :: Phantoms.TTerm Syntax.Source -> Phantoms.TTerm [Syntax.Stat]
sourceStats x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Source"),
        Core.projectionField = (Core.Name "stats")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

sourceWithStats :: Phantoms.TTerm Syntax.Source -> Phantoms.TTerm [Syntax.Stat] -> Phantoms.TTerm Syntax.Source
sourceWithStats original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Source"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "stats"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

statDecl :: Phantoms.TTerm Syntax.Decl -> Phantoms.TTerm Syntax.Stat
statDecl x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Stat"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "decl"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

statDefn :: Phantoms.TTerm Syntax.Defn -> Phantoms.TTerm Syntax.Stat
statDefn x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Stat"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "defn"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

statImportExport :: Phantoms.TTerm Syntax.ImportExportStat -> Phantoms.TTerm Syntax.Stat
statImportExport x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Stat"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "importExport"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

statTerm :: Phantoms.TTerm Syntax.Data -> Phantoms.TTerm Syntax.Stat
statTerm x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Stat"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "term"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

template :: Phantoms.TTerm [Syntax.Stat] -> Phantoms.TTerm [Syntax.Init] -> Phantoms.TTerm Syntax.Self -> Phantoms.TTerm [Syntax.Stat] -> Phantoms.TTerm Syntax.Template
template early inits self stats =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Template"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "early"),
          Core.fieldTerm = (Phantoms.unTTerm early)},
        Core.Field {
          Core.fieldName = (Core.Name "inits"),
          Core.fieldTerm = (Phantoms.unTTerm inits)},
        Core.Field {
          Core.fieldName = (Core.Name "self"),
          Core.fieldTerm = (Phantoms.unTTerm self)},
        Core.Field {
          Core.fieldName = (Core.Name "stats"),
          Core.fieldTerm = (Phantoms.unTTerm stats)}]}))

templateEarly :: Phantoms.TTerm Syntax.Template -> Phantoms.TTerm [Syntax.Stat]
templateEarly x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Template"),
        Core.projectionField = (Core.Name "early")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

templateInits :: Phantoms.TTerm Syntax.Template -> Phantoms.TTerm [Syntax.Init]
templateInits x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Template"),
        Core.projectionField = (Core.Name "inits")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

templateSelf :: Phantoms.TTerm Syntax.Template -> Phantoms.TTerm Syntax.Self
templateSelf x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Template"),
        Core.projectionField = (Core.Name "self")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

templateStats :: Phantoms.TTerm Syntax.Template -> Phantoms.TTerm [Syntax.Stat]
templateStats x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Template"),
        Core.projectionField = (Core.Name "stats")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

templateWithEarly :: Phantoms.TTerm Syntax.Template -> Phantoms.TTerm [Syntax.Stat] -> Phantoms.TTerm Syntax.Template
templateWithEarly original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Template"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "early"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "inits"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Template"),
              Core.projectionField = (Core.Name "inits")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "self"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Template"),
              Core.projectionField = (Core.Name "self")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "stats"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Template"),
              Core.projectionField = (Core.Name "stats")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

templateWithInits :: Phantoms.TTerm Syntax.Template -> Phantoms.TTerm [Syntax.Init] -> Phantoms.TTerm Syntax.Template
templateWithInits original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Template"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "early"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Template"),
              Core.projectionField = (Core.Name "early")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inits"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "self"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Template"),
              Core.projectionField = (Core.Name "self")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "stats"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Template"),
              Core.projectionField = (Core.Name "stats")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

templateWithSelf :: Phantoms.TTerm Syntax.Template -> Phantoms.TTerm Syntax.Self -> Phantoms.TTerm Syntax.Template
templateWithSelf original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Template"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "early"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Template"),
              Core.projectionField = (Core.Name "early")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inits"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Template"),
              Core.projectionField = (Core.Name "inits")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "self"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "stats"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Template"),
              Core.projectionField = (Core.Name "stats")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

templateWithStats :: Phantoms.TTerm Syntax.Template -> Phantoms.TTerm [Syntax.Stat] -> Phantoms.TTerm Syntax.Template
templateWithStats original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Template"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "early"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Template"),
              Core.projectionField = (Core.Name "early")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inits"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Template"),
              Core.projectionField = (Core.Name "inits")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "self"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Template"),
              Core.projectionField = (Core.Name "self")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "stats"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

treeBounds :: Phantoms.TTerm Syntax.TypeBounds -> Phantoms.TTerm Syntax.Tree
treeBounds x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Tree"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "bounds"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

treeCaseTree :: Phantoms.TTerm Syntax.CaseTree -> Phantoms.TTerm Syntax.Tree
treeCaseTree x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Tree"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "caseTree"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

treeCtor :: Phantoms.TTerm Syntax.Ctor -> Phantoms.TTerm Syntax.Tree
treeCtor x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Tree"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "ctor"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

treeEnumerator :: Phantoms.TTerm Syntax.Enumerator -> Phantoms.TTerm Syntax.Tree
treeEnumerator x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Tree"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "enumerator"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

treeImportee :: Phantoms.TTerm Syntax.Importee -> Phantoms.TTerm Syntax.Tree
treeImportee x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Tree"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "importee"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

treeImporter :: Phantoms.TTerm Syntax.Importer -> Phantoms.TTerm Syntax.Tree
treeImporter x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Tree"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "importer"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

treeMember :: Phantoms.TTerm Syntax.Member -> Phantoms.TTerm Syntax.Tree
treeMember x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Tree"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "member"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

treeMod :: Phantoms.TTerm Syntax.Mod -> Phantoms.TTerm Syntax.Tree
treeMod x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Tree"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "mod"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

treePat :: Phantoms.TTerm Syntax.Pat -> Phantoms.TTerm Syntax.Tree
treePat x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Tree"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "pat"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

treeQuasi :: Phantoms.TTerm Syntax.Quasi -> Phantoms.TTerm Syntax.Tree
treeQuasi x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Tree"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "quasi"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

treeRef :: Phantoms.TTerm Syntax.Ref -> Phantoms.TTerm Syntax.Tree
treeRef x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Tree"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "ref"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

treeSource :: Phantoms.TTerm Syntax.Source -> Phantoms.TTerm Syntax.Tree
treeSource x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Tree"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "source"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

treeStat :: Phantoms.TTerm Syntax.Stat -> Phantoms.TTerm Syntax.Tree
treeStat x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Tree"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "stat"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

treeTemplate :: Phantoms.TTerm Syntax.Template -> Phantoms.TTerm Syntax.Tree
treeTemplate x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Tree"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "template"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

treeType :: Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.Tree
treeType x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Tree"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "type"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

typeAnd :: Phantoms.TTerm Syntax.Type_And -> Phantoms.TTerm Syntax.Type
typeAnd x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "and"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

typeAnnotate :: Phantoms.TTerm Syntax.Type_Annotate -> Phantoms.TTerm Syntax.Type
typeAnnotate x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "annotate"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

typeAnonymousName :: Phantoms.TTerm Syntax.Type_AnonymousName -> Phantoms.TTerm Syntax.Type
typeAnonymousName x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "anonymousName"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

typeApply :: Phantoms.TTerm Syntax.Type_Apply -> Phantoms.TTerm Syntax.Type
typeApply x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "apply"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

typeApplyInfix :: Phantoms.TTerm Syntax.Type_ApplyInfix -> Phantoms.TTerm Syntax.Type
typeApplyInfix x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "applyInfix"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

typeBounds :: Phantoms.TTerm (Maybe Syntax.Type) -> Phantoms.TTerm (Maybe Syntax.Type) -> Phantoms.TTerm Syntax.TypeBounds
typeBounds lo hi =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.TypeBounds"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lo"),
          Core.fieldTerm = (Phantoms.unTTerm lo)},
        Core.Field {
          Core.fieldName = (Core.Name "hi"),
          Core.fieldTerm = (Phantoms.unTTerm hi)}]}))

typeBoundsHi :: Phantoms.TTerm Syntax.TypeBounds -> Phantoms.TTerm (Maybe Syntax.Type)
typeBoundsHi x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.TypeBounds"),
        Core.projectionField = (Core.Name "hi")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

typeBoundsLo :: Phantoms.TTerm Syntax.TypeBounds -> Phantoms.TTerm (Maybe Syntax.Type)
typeBoundsLo x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.TypeBounds"),
        Core.projectionField = (Core.Name "lo")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

typeBoundsWithHi :: Phantoms.TTerm Syntax.TypeBounds -> Phantoms.TTerm (Maybe Syntax.Type) -> Phantoms.TTerm Syntax.TypeBounds
typeBoundsWithHi original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.TypeBounds"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lo"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.TypeBounds"),
              Core.projectionField = (Core.Name "lo")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "hi"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

typeBoundsWithLo :: Phantoms.TTerm Syntax.TypeBounds -> Phantoms.TTerm (Maybe Syntax.Type) -> Phantoms.TTerm Syntax.TypeBounds
typeBoundsWithLo original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.TypeBounds"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lo"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "hi"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.TypeBounds"),
              Core.projectionField = (Core.Name "hi")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

typeByName :: Phantoms.TTerm Syntax.Type_ByName -> Phantoms.TTerm Syntax.Type
typeByName x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "byName"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

typeCase :: Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.TypeCase
typeCase pat body =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.TypeCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pat"),
          Core.fieldTerm = (Phantoms.unTTerm pat)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm body)}]}))

typeCaseBody :: Phantoms.TTerm Syntax.TypeCase -> Phantoms.TTerm Syntax.Type
typeCaseBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.TypeCase"),
        Core.projectionField = (Core.Name "body")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

typeCasePat :: Phantoms.TTerm Syntax.TypeCase -> Phantoms.TTerm Syntax.Type
typeCasePat x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.TypeCase"),
        Core.projectionField = (Core.Name "pat")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

typeCaseWithBody :: Phantoms.TTerm Syntax.TypeCase -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.TypeCase
typeCaseWithBody original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.TypeCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pat"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.TypeCase"),
              Core.projectionField = (Core.Name "pat")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

typeCaseWithPat :: Phantoms.TTerm Syntax.TypeCase -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.TypeCase
typeCaseWithPat original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.TypeCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pat"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.TypeCase"),
              Core.projectionField = (Core.Name "body")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

typeExistential :: Phantoms.TTerm Syntax.Type_Existential -> Phantoms.TTerm Syntax.Type
typeExistential x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "existential"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

typeFunctionType :: Phantoms.TTerm Syntax.Type_FunctionType -> Phantoms.TTerm Syntax.Type
typeFunctionType x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "functionType"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

typeImplicitFunction :: Phantoms.TTerm Syntax.Type_ImplicitFunction -> Phantoms.TTerm Syntax.Type
typeImplicitFunction x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "implicitFunction"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

typeLambda :: Phantoms.TTerm Syntax.Type_Lambda -> Phantoms.TTerm Syntax.Type
typeLambda x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "lambda"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

typeMacro :: Phantoms.TTerm Syntax.Type_Macro -> Phantoms.TTerm Syntax.Type
typeMacro x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "macro"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

typeMatch :: Phantoms.TTerm Syntax.Type_Match -> Phantoms.TTerm Syntax.Type
typeMatch x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "match"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

typeMethod :: Phantoms.TTerm Syntax.Type_Method -> Phantoms.TTerm Syntax.Type
typeMethod x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "method"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

typeOr :: Phantoms.TTerm Syntax.Type_Or -> Phantoms.TTerm Syntax.Type
typeOr x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "or"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

typePlaceholder :: Phantoms.TTerm Syntax.Type_Placeholder -> Phantoms.TTerm Syntax.Type
typePlaceholder x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "placeholder"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

typePolyFunction :: Phantoms.TTerm Syntax.Type_PolyFunction -> Phantoms.TTerm Syntax.Type
typePolyFunction x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "polyFunction"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

typeRef :: Phantoms.TTerm Syntax.Type_Ref -> Phantoms.TTerm Syntax.Type
typeRef x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "ref"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

typeRefine :: Phantoms.TTerm Syntax.Type_Refine -> Phantoms.TTerm Syntax.Type
typeRefine x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "refine"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

typeRepeated :: Phantoms.TTerm Syntax.Type_Repeated -> Phantoms.TTerm Syntax.Type
typeRepeated x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "repeated"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

typeTuple :: Phantoms.TTerm Syntax.Type_Tuple -> Phantoms.TTerm Syntax.Type
typeTuple x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "tuple"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

typeTypedParam :: Phantoms.TTerm Syntax.Type_TypedParam -> Phantoms.TTerm Syntax.Type
typeTypedParam x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "typedParam"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

typeVar :: Phantoms.TTerm Syntax.Type_Var -> Phantoms.TTerm Syntax.Type
typeVar x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "var"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

typeWith :: Phantoms.TTerm Syntax.Type_With -> Phantoms.TTerm Syntax.Type
typeWith x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "with"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

type_And :: Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.Type_And
type_And lhs rhs =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Type_And"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Phantoms.unTTerm lhs)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTTerm rhs)}]}))

type_AndLhs :: Phantoms.TTerm Syntax.Type_And -> Phantoms.TTerm Syntax.Type
type_AndLhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Type_And"),
        Core.projectionField = (Core.Name "lhs")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

type_AndRhs :: Phantoms.TTerm Syntax.Type_And -> Phantoms.TTerm Syntax.Type
type_AndRhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Type_And"),
        Core.projectionField = (Core.Name "rhs")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

type_AndWithLhs :: Phantoms.TTerm Syntax.Type_And -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.Type_And
type_AndWithLhs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Type_And"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Type_And"),
              Core.projectionField = (Core.Name "rhs")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

type_AndWithRhs :: Phantoms.TTerm Syntax.Type_And -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.Type_And
type_AndWithRhs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Type_And"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Type_And"),
              Core.projectionField = (Core.Name "lhs")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

type_Annotate :: Phantoms.TTerm Syntax.Type -> Phantoms.TTerm [Syntax.Mod_Annot] -> Phantoms.TTerm Syntax.Type_Annotate
type_Annotate tpe annots =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Type_Annotate"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "tpe"),
          Core.fieldTerm = (Phantoms.unTTerm tpe)},
        Core.Field {
          Core.fieldName = (Core.Name "annots"),
          Core.fieldTerm = (Phantoms.unTTerm annots)}]}))

type_AnnotateAnnots :: Phantoms.TTerm Syntax.Type_Annotate -> Phantoms.TTerm [Syntax.Mod_Annot]
type_AnnotateAnnots x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Type_Annotate"),
        Core.projectionField = (Core.Name "annots")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

type_AnnotateTpe :: Phantoms.TTerm Syntax.Type_Annotate -> Phantoms.TTerm Syntax.Type
type_AnnotateTpe x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Type_Annotate"),
        Core.projectionField = (Core.Name "tpe")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

type_AnnotateWithAnnots :: Phantoms.TTerm Syntax.Type_Annotate -> Phantoms.TTerm [Syntax.Mod_Annot] -> Phantoms.TTerm Syntax.Type_Annotate
type_AnnotateWithAnnots original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Type_Annotate"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "tpe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Type_Annotate"),
              Core.projectionField = (Core.Name "tpe")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "annots"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

type_AnnotateWithTpe :: Phantoms.TTerm Syntax.Type_Annotate -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.Type_Annotate
type_AnnotateWithTpe original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Type_Annotate"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "tpe"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "annots"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Type_Annotate"),
              Core.projectionField = (Core.Name "annots")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

type_AnonymousName :: Phantoms.TTerm () -> Phantoms.TTerm Syntax.Type_AnonymousName
type_AnonymousName x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.scala.syntax.Type_AnonymousName"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

type_Apply :: Phantoms.TTerm Syntax.Type -> Phantoms.TTerm [Syntax.Type] -> Phantoms.TTerm Syntax.Type_Apply
type_Apply tpe args =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Type_Apply"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "tpe"),
          Core.fieldTerm = (Phantoms.unTTerm tpe)},
        Core.Field {
          Core.fieldName = (Core.Name "args"),
          Core.fieldTerm = (Phantoms.unTTerm args)}]}))

type_ApplyArgs :: Phantoms.TTerm Syntax.Type_Apply -> Phantoms.TTerm [Syntax.Type]
type_ApplyArgs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Type_Apply"),
        Core.projectionField = (Core.Name "args")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

type_ApplyInfix :: Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.Type_Name -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.Type_ApplyInfix
type_ApplyInfix lhs op rhs =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Type_ApplyInfix"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Phantoms.unTTerm lhs)},
        Core.Field {
          Core.fieldName = (Core.Name "op"),
          Core.fieldTerm = (Phantoms.unTTerm op)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTTerm rhs)}]}))

type_ApplyInfixLhs :: Phantoms.TTerm Syntax.Type_ApplyInfix -> Phantoms.TTerm Syntax.Type
type_ApplyInfixLhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Type_ApplyInfix"),
        Core.projectionField = (Core.Name "lhs")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

type_ApplyInfixOp :: Phantoms.TTerm Syntax.Type_ApplyInfix -> Phantoms.TTerm Syntax.Type_Name
type_ApplyInfixOp x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Type_ApplyInfix"),
        Core.projectionField = (Core.Name "op")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

type_ApplyInfixRhs :: Phantoms.TTerm Syntax.Type_ApplyInfix -> Phantoms.TTerm Syntax.Type
type_ApplyInfixRhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Type_ApplyInfix"),
        Core.projectionField = (Core.Name "rhs")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

type_ApplyInfixWithLhs :: Phantoms.TTerm Syntax.Type_ApplyInfix -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.Type_ApplyInfix
type_ApplyInfixWithLhs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Type_ApplyInfix"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "op"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Type_ApplyInfix"),
              Core.projectionField = (Core.Name "op")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Type_ApplyInfix"),
              Core.projectionField = (Core.Name "rhs")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

type_ApplyInfixWithOp :: Phantoms.TTerm Syntax.Type_ApplyInfix -> Phantoms.TTerm Syntax.Type_Name -> Phantoms.TTerm Syntax.Type_ApplyInfix
type_ApplyInfixWithOp original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Type_ApplyInfix"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Type_ApplyInfix"),
              Core.projectionField = (Core.Name "lhs")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "op"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Type_ApplyInfix"),
              Core.projectionField = (Core.Name "rhs")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

type_ApplyInfixWithRhs :: Phantoms.TTerm Syntax.Type_ApplyInfix -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.Type_ApplyInfix
type_ApplyInfixWithRhs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Type_ApplyInfix"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Type_ApplyInfix"),
              Core.projectionField = (Core.Name "lhs")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "op"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Type_ApplyInfix"),
              Core.projectionField = (Core.Name "op")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

type_ApplyTpe :: Phantoms.TTerm Syntax.Type_Apply -> Phantoms.TTerm Syntax.Type
type_ApplyTpe x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Type_Apply"),
        Core.projectionField = (Core.Name "tpe")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

type_ApplyWithArgs :: Phantoms.TTerm Syntax.Type_Apply -> Phantoms.TTerm [Syntax.Type] -> Phantoms.TTerm Syntax.Type_Apply
type_ApplyWithArgs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Type_Apply"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "tpe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Type_Apply"),
              Core.projectionField = (Core.Name "tpe")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "args"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

type_ApplyWithTpe :: Phantoms.TTerm Syntax.Type_Apply -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.Type_Apply
type_ApplyWithTpe original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Type_Apply"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "tpe"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "args"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Type_Apply"),
              Core.projectionField = (Core.Name "args")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

type_ByName :: Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.Type_ByName
type_ByName tpe =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Type_ByName"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "tpe"),
          Core.fieldTerm = (Phantoms.unTTerm tpe)}]}))

type_ByNameTpe :: Phantoms.TTerm Syntax.Type_ByName -> Phantoms.TTerm Syntax.Type
type_ByNameTpe x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Type_ByName"),
        Core.projectionField = (Core.Name "tpe")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

type_ByNameWithTpe :: Phantoms.TTerm Syntax.Type_ByName -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.Type_ByName
type_ByNameWithTpe original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Type_ByName"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "tpe"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

type_ContextFunction :: Phantoms.TTerm [Syntax.Type] -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.Type_ContextFunction
type_ContextFunction params res =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Type_ContextFunction"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Phantoms.unTTerm params)},
        Core.Field {
          Core.fieldName = (Core.Name "res"),
          Core.fieldTerm = (Phantoms.unTTerm res)}]}))

type_ContextFunctionParams :: Phantoms.TTerm Syntax.Type_ContextFunction -> Phantoms.TTerm [Syntax.Type]
type_ContextFunctionParams x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Type_ContextFunction"),
        Core.projectionField = (Core.Name "params")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

type_ContextFunctionRes :: Phantoms.TTerm Syntax.Type_ContextFunction -> Phantoms.TTerm Syntax.Type
type_ContextFunctionRes x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Type_ContextFunction"),
        Core.projectionField = (Core.Name "res")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

type_ContextFunctionWithParams :: Phantoms.TTerm Syntax.Type_ContextFunction -> Phantoms.TTerm [Syntax.Type] -> Phantoms.TTerm Syntax.Type_ContextFunction
type_ContextFunctionWithParams original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Type_ContextFunction"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "res"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Type_ContextFunction"),
              Core.projectionField = (Core.Name "res")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

type_ContextFunctionWithRes :: Phantoms.TTerm Syntax.Type_ContextFunction -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.Type_ContextFunction
type_ContextFunctionWithRes original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Type_ContextFunction"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Type_ContextFunction"),
              Core.projectionField = (Core.Name "params")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "res"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

type_Existential :: Phantoms.TTerm Syntax.Type -> Phantoms.TTerm [Syntax.Stat] -> Phantoms.TTerm Syntax.Type_Existential
type_Existential tpe stats =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Type_Existential"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "tpe"),
          Core.fieldTerm = (Phantoms.unTTerm tpe)},
        Core.Field {
          Core.fieldName = (Core.Name "stats"),
          Core.fieldTerm = (Phantoms.unTTerm stats)}]}))

type_ExistentialStats :: Phantoms.TTerm Syntax.Type_Existential -> Phantoms.TTerm [Syntax.Stat]
type_ExistentialStats x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Type_Existential"),
        Core.projectionField = (Core.Name "stats")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

type_ExistentialTpe :: Phantoms.TTerm Syntax.Type_Existential -> Phantoms.TTerm Syntax.Type
type_ExistentialTpe x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Type_Existential"),
        Core.projectionField = (Core.Name "tpe")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

type_ExistentialWithStats :: Phantoms.TTerm Syntax.Type_Existential -> Phantoms.TTerm [Syntax.Stat] -> Phantoms.TTerm Syntax.Type_Existential
type_ExistentialWithStats original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Type_Existential"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "tpe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Type_Existential"),
              Core.projectionField = (Core.Name "tpe")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "stats"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

type_ExistentialWithTpe :: Phantoms.TTerm Syntax.Type_Existential -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.Type_Existential
type_ExistentialWithTpe original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Type_Existential"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "tpe"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "stats"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Type_Existential"),
              Core.projectionField = (Core.Name "stats")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

type_Function :: Phantoms.TTerm [Syntax.Type] -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.Type_Function
type_Function params res =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Type_Function"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Phantoms.unTTerm params)},
        Core.Field {
          Core.fieldName = (Core.Name "res"),
          Core.fieldTerm = (Phantoms.unTTerm res)}]}))

type_FunctionParams :: Phantoms.TTerm Syntax.Type_Function -> Phantoms.TTerm [Syntax.Type]
type_FunctionParams x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Type_Function"),
        Core.projectionField = (Core.Name "params")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

type_FunctionRes :: Phantoms.TTerm Syntax.Type_Function -> Phantoms.TTerm Syntax.Type
type_FunctionRes x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Type_Function"),
        Core.projectionField = (Core.Name "res")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

type_FunctionTypeContextFunction :: Phantoms.TTerm Syntax.Type_ContextFunction -> Phantoms.TTerm Syntax.Type_FunctionType
type_FunctionTypeContextFunction x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Type_FunctionType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "contextFunction"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

type_FunctionTypeFunction :: Phantoms.TTerm Syntax.Type_Function -> Phantoms.TTerm Syntax.Type_FunctionType
type_FunctionTypeFunction x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Type_FunctionType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "function"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

type_FunctionWithParams :: Phantoms.TTerm Syntax.Type_Function -> Phantoms.TTerm [Syntax.Type] -> Phantoms.TTerm Syntax.Type_Function
type_FunctionWithParams original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Type_Function"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "res"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Type_Function"),
              Core.projectionField = (Core.Name "res")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

type_FunctionWithRes :: Phantoms.TTerm Syntax.Type_Function -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.Type_Function
type_FunctionWithRes original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Type_Function"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Type_Function"),
              Core.projectionField = (Core.Name "params")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "res"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

type_ImplicitFunction :: Phantoms.TTerm [Syntax.Type] -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.Type_ImplicitFunction
type_ImplicitFunction params res =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Type_ImplicitFunction"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Phantoms.unTTerm params)},
        Core.Field {
          Core.fieldName = (Core.Name "res"),
          Core.fieldTerm = (Phantoms.unTTerm res)}]}))

type_ImplicitFunctionParams :: Phantoms.TTerm Syntax.Type_ImplicitFunction -> Phantoms.TTerm [Syntax.Type]
type_ImplicitFunctionParams x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Type_ImplicitFunction"),
        Core.projectionField = (Core.Name "params")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

type_ImplicitFunctionRes :: Phantoms.TTerm Syntax.Type_ImplicitFunction -> Phantoms.TTerm Syntax.Type
type_ImplicitFunctionRes x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Type_ImplicitFunction"),
        Core.projectionField = (Core.Name "res")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

type_ImplicitFunctionWithParams :: Phantoms.TTerm Syntax.Type_ImplicitFunction -> Phantoms.TTerm [Syntax.Type] -> Phantoms.TTerm Syntax.Type_ImplicitFunction
type_ImplicitFunctionWithParams original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Type_ImplicitFunction"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "res"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Type_ImplicitFunction"),
              Core.projectionField = (Core.Name "res")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

type_ImplicitFunctionWithRes :: Phantoms.TTerm Syntax.Type_ImplicitFunction -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.Type_ImplicitFunction
type_ImplicitFunctionWithRes original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Type_ImplicitFunction"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Type_ImplicitFunction"),
              Core.projectionField = (Core.Name "params")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "res"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

type_Lambda :: Phantoms.TTerm [Syntax.Type_Param] -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.Type_Lambda
type_Lambda tparams tpe =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Type_Lambda"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Phantoms.unTTerm tparams)},
        Core.Field {
          Core.fieldName = (Core.Name "tpe"),
          Core.fieldTerm = (Phantoms.unTTerm tpe)}]}))

type_LambdaTparams :: Phantoms.TTerm Syntax.Type_Lambda -> Phantoms.TTerm [Syntax.Type_Param]
type_LambdaTparams x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Type_Lambda"),
        Core.projectionField = (Core.Name "tparams")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

type_LambdaTpe :: Phantoms.TTerm Syntax.Type_Lambda -> Phantoms.TTerm Syntax.Type
type_LambdaTpe x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Type_Lambda"),
        Core.projectionField = (Core.Name "tpe")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

type_LambdaWithTparams :: Phantoms.TTerm Syntax.Type_Lambda -> Phantoms.TTerm [Syntax.Type_Param] -> Phantoms.TTerm Syntax.Type_Lambda
type_LambdaWithTparams original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Type_Lambda"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "tpe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Type_Lambda"),
              Core.projectionField = (Core.Name "tpe")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

type_LambdaWithTpe :: Phantoms.TTerm Syntax.Type_Lambda -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.Type_Lambda
type_LambdaWithTpe original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Type_Lambda"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Type_Lambda"),
              Core.projectionField = (Core.Name "tparams")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tpe"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

type_Macro :: Phantoms.TTerm Syntax.Data -> Phantoms.TTerm Syntax.Type_Macro
type_Macro body =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Type_Macro"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm body)}]}))

type_MacroBody :: Phantoms.TTerm Syntax.Type_Macro -> Phantoms.TTerm Syntax.Data
type_MacroBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Type_Macro"),
        Core.projectionField = (Core.Name "body")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

type_MacroWithBody :: Phantoms.TTerm Syntax.Type_Macro -> Phantoms.TTerm Syntax.Data -> Phantoms.TTerm Syntax.Type_Macro
type_MacroWithBody original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Type_Macro"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

type_Match :: Phantoms.TTerm Syntax.Type -> Phantoms.TTerm [Syntax.TypeCase] -> Phantoms.TTerm Syntax.Type_Match
type_Match tpe cases =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Type_Match"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "tpe"),
          Core.fieldTerm = (Phantoms.unTTerm tpe)},
        Core.Field {
          Core.fieldName = (Core.Name "cases"),
          Core.fieldTerm = (Phantoms.unTTerm cases)}]}))

type_MatchCases :: Phantoms.TTerm Syntax.Type_Match -> Phantoms.TTerm [Syntax.TypeCase]
type_MatchCases x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Type_Match"),
        Core.projectionField = (Core.Name "cases")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

type_MatchTpe :: Phantoms.TTerm Syntax.Type_Match -> Phantoms.TTerm Syntax.Type
type_MatchTpe x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Type_Match"),
        Core.projectionField = (Core.Name "tpe")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

type_MatchWithCases :: Phantoms.TTerm Syntax.Type_Match -> Phantoms.TTerm [Syntax.TypeCase] -> Phantoms.TTerm Syntax.Type_Match
type_MatchWithCases original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Type_Match"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "tpe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Type_Match"),
              Core.projectionField = (Core.Name "tpe")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "cases"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

type_MatchWithTpe :: Phantoms.TTerm Syntax.Type_Match -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.Type_Match
type_MatchWithTpe original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Type_Match"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "tpe"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "cases"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Type_Match"),
              Core.projectionField = (Core.Name "cases")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

type_Method :: Phantoms.TTerm [[Syntax.Data_Param]] -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.Type_Method
type_Method paramss tpe =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Type_Method"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "paramss"),
          Core.fieldTerm = (Phantoms.unTTerm paramss)},
        Core.Field {
          Core.fieldName = (Core.Name "tpe"),
          Core.fieldTerm = (Phantoms.unTTerm tpe)}]}))

type_MethodParamss :: Phantoms.TTerm Syntax.Type_Method -> Phantoms.TTerm [[Syntax.Data_Param]]
type_MethodParamss x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Type_Method"),
        Core.projectionField = (Core.Name "paramss")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

type_MethodTpe :: Phantoms.TTerm Syntax.Type_Method -> Phantoms.TTerm Syntax.Type
type_MethodTpe x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Type_Method"),
        Core.projectionField = (Core.Name "tpe")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

type_MethodWithParamss :: Phantoms.TTerm Syntax.Type_Method -> Phantoms.TTerm [[Syntax.Data_Param]] -> Phantoms.TTerm Syntax.Type_Method
type_MethodWithParamss original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Type_Method"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "paramss"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "tpe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Type_Method"),
              Core.projectionField = (Core.Name "tpe")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

type_MethodWithTpe :: Phantoms.TTerm Syntax.Type_Method -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.Type_Method
type_MethodWithTpe original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Type_Method"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "paramss"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Type_Method"),
              Core.projectionField = (Core.Name "paramss")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tpe"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

type_Name :: Phantoms.TTerm String -> Phantoms.TTerm Syntax.Type_Name
type_Name value =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Type_Name"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm value)}]}))

type_NameValue :: Phantoms.TTerm Syntax.Type_Name -> Phantoms.TTerm String
type_NameValue x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Type_Name"),
        Core.projectionField = (Core.Name "value")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

type_NameWithValue :: Phantoms.TTerm Syntax.Type_Name -> Phantoms.TTerm String -> Phantoms.TTerm Syntax.Type_Name
type_NameWithValue original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Type_Name"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

type_Or :: Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.Type_Or
type_Or lhs rhs =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Type_Or"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Phantoms.unTTerm lhs)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTTerm rhs)}]}))

type_OrLhs :: Phantoms.TTerm Syntax.Type_Or -> Phantoms.TTerm Syntax.Type
type_OrLhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Type_Or"),
        Core.projectionField = (Core.Name "lhs")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

type_OrRhs :: Phantoms.TTerm Syntax.Type_Or -> Phantoms.TTerm Syntax.Type
type_OrRhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Type_Or"),
        Core.projectionField = (Core.Name "rhs")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

type_OrWithLhs :: Phantoms.TTerm Syntax.Type_Or -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.Type_Or
type_OrWithLhs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Type_Or"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Type_Or"),
              Core.projectionField = (Core.Name "rhs")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

type_OrWithRhs :: Phantoms.TTerm Syntax.Type_Or -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.Type_Or
type_OrWithRhs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Type_Or"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Type_Or"),
              Core.projectionField = (Core.Name "lhs")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

type_Param :: Phantoms.TTerm [Syntax.Mod] -> Phantoms.TTerm Syntax.Name -> Phantoms.TTerm [Syntax.Type_Param] -> Phantoms.TTerm [Syntax.TypeBounds] -> Phantoms.TTerm [Syntax.Type] -> Phantoms.TTerm [Syntax.Type] -> Phantoms.TTerm Syntax.Type_Param
type_Param mods name tparams tbounds vbounds cbounds =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Type_Param"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Phantoms.unTTerm mods)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Phantoms.unTTerm tparams)},
        Core.Field {
          Core.fieldName = (Core.Name "tbounds"),
          Core.fieldTerm = (Phantoms.unTTerm tbounds)},
        Core.Field {
          Core.fieldName = (Core.Name "vbounds"),
          Core.fieldTerm = (Phantoms.unTTerm vbounds)},
        Core.Field {
          Core.fieldName = (Core.Name "cbounds"),
          Core.fieldTerm = (Phantoms.unTTerm cbounds)}]}))

type_ParamCbounds :: Phantoms.TTerm Syntax.Type_Param -> Phantoms.TTerm [Syntax.Type]
type_ParamCbounds x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Type_Param"),
        Core.projectionField = (Core.Name "cbounds")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

type_ParamMods :: Phantoms.TTerm Syntax.Type_Param -> Phantoms.TTerm [Syntax.Mod]
type_ParamMods x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Type_Param"),
        Core.projectionField = (Core.Name "mods")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

type_ParamName :: Phantoms.TTerm Syntax.Type_Param -> Phantoms.TTerm Syntax.Name
type_ParamName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Type_Param"),
        Core.projectionField = (Core.Name "name")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

type_ParamTbounds :: Phantoms.TTerm Syntax.Type_Param -> Phantoms.TTerm [Syntax.TypeBounds]
type_ParamTbounds x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Type_Param"),
        Core.projectionField = (Core.Name "tbounds")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

type_ParamTparams :: Phantoms.TTerm Syntax.Type_Param -> Phantoms.TTerm [Syntax.Type_Param]
type_ParamTparams x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Type_Param"),
        Core.projectionField = (Core.Name "tparams")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

type_ParamVbounds :: Phantoms.TTerm Syntax.Type_Param -> Phantoms.TTerm [Syntax.Type]
type_ParamVbounds x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Type_Param"),
        Core.projectionField = (Core.Name "vbounds")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

type_ParamWithCbounds :: Phantoms.TTerm Syntax.Type_Param -> Phantoms.TTerm [Syntax.Type] -> Phantoms.TTerm Syntax.Type_Param
type_ParamWithCbounds original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Type_Param"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Type_Param"),
              Core.projectionField = (Core.Name "mods")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Type_Param"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Type_Param"),
              Core.projectionField = (Core.Name "tparams")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tbounds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Type_Param"),
              Core.projectionField = (Core.Name "tbounds")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "vbounds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Type_Param"),
              Core.projectionField = (Core.Name "vbounds")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "cbounds"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

type_ParamWithMods :: Phantoms.TTerm Syntax.Type_Param -> Phantoms.TTerm [Syntax.Mod] -> Phantoms.TTerm Syntax.Type_Param
type_ParamWithMods original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Type_Param"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Type_Param"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Type_Param"),
              Core.projectionField = (Core.Name "tparams")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tbounds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Type_Param"),
              Core.projectionField = (Core.Name "tbounds")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "vbounds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Type_Param"),
              Core.projectionField = (Core.Name "vbounds")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "cbounds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Type_Param"),
              Core.projectionField = (Core.Name "cbounds")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

type_ParamWithName :: Phantoms.TTerm Syntax.Type_Param -> Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.Type_Param
type_ParamWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Type_Param"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Type_Param"),
              Core.projectionField = (Core.Name "mods")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Type_Param"),
              Core.projectionField = (Core.Name "tparams")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tbounds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Type_Param"),
              Core.projectionField = (Core.Name "tbounds")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "vbounds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Type_Param"),
              Core.projectionField = (Core.Name "vbounds")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "cbounds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Type_Param"),
              Core.projectionField = (Core.Name "cbounds")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

type_ParamWithTbounds :: Phantoms.TTerm Syntax.Type_Param -> Phantoms.TTerm [Syntax.TypeBounds] -> Phantoms.TTerm Syntax.Type_Param
type_ParamWithTbounds original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Type_Param"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Type_Param"),
              Core.projectionField = (Core.Name "mods")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Type_Param"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Type_Param"),
              Core.projectionField = (Core.Name "tparams")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tbounds"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "vbounds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Type_Param"),
              Core.projectionField = (Core.Name "vbounds")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "cbounds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Type_Param"),
              Core.projectionField = (Core.Name "cbounds")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

type_ParamWithTparams :: Phantoms.TTerm Syntax.Type_Param -> Phantoms.TTerm [Syntax.Type_Param] -> Phantoms.TTerm Syntax.Type_Param
type_ParamWithTparams original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Type_Param"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Type_Param"),
              Core.projectionField = (Core.Name "mods")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Type_Param"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "tbounds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Type_Param"),
              Core.projectionField = (Core.Name "tbounds")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "vbounds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Type_Param"),
              Core.projectionField = (Core.Name "vbounds")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "cbounds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Type_Param"),
              Core.projectionField = (Core.Name "cbounds")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

type_ParamWithVbounds :: Phantoms.TTerm Syntax.Type_Param -> Phantoms.TTerm [Syntax.Type] -> Phantoms.TTerm Syntax.Type_Param
type_ParamWithVbounds original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Type_Param"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Type_Param"),
              Core.projectionField = (Core.Name "mods")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Type_Param"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Type_Param"),
              Core.projectionField = (Core.Name "tparams")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tbounds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Type_Param"),
              Core.projectionField = (Core.Name "tbounds")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "vbounds"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "cbounds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Type_Param"),
              Core.projectionField = (Core.Name "cbounds")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

type_Placeholder :: Phantoms.TTerm Syntax.TypeBounds -> Phantoms.TTerm Syntax.Type_Placeholder
type_Placeholder bounds =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Type_Placeholder"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "bounds"),
          Core.fieldTerm = (Phantoms.unTTerm bounds)}]}))

type_PlaceholderBounds :: Phantoms.TTerm Syntax.Type_Placeholder -> Phantoms.TTerm Syntax.TypeBounds
type_PlaceholderBounds x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Type_Placeholder"),
        Core.projectionField = (Core.Name "bounds")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

type_PlaceholderWithBounds :: Phantoms.TTerm Syntax.Type_Placeholder -> Phantoms.TTerm Syntax.TypeBounds -> Phantoms.TTerm Syntax.Type_Placeholder
type_PlaceholderWithBounds original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Type_Placeholder"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "bounds"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

type_PolyFunction :: Phantoms.TTerm [Syntax.Type_Param] -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.Type_PolyFunction
type_PolyFunction tparams tpe =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Type_PolyFunction"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Phantoms.unTTerm tparams)},
        Core.Field {
          Core.fieldName = (Core.Name "tpe"),
          Core.fieldTerm = (Phantoms.unTTerm tpe)}]}))

type_PolyFunctionTparams :: Phantoms.TTerm Syntax.Type_PolyFunction -> Phantoms.TTerm [Syntax.Type_Param]
type_PolyFunctionTparams x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Type_PolyFunction"),
        Core.projectionField = (Core.Name "tparams")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

type_PolyFunctionTpe :: Phantoms.TTerm Syntax.Type_PolyFunction -> Phantoms.TTerm Syntax.Type
type_PolyFunctionTpe x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Type_PolyFunction"),
        Core.projectionField = (Core.Name "tpe")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

type_PolyFunctionWithTparams :: Phantoms.TTerm Syntax.Type_PolyFunction -> Phantoms.TTerm [Syntax.Type_Param] -> Phantoms.TTerm Syntax.Type_PolyFunction
type_PolyFunctionWithTparams original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Type_PolyFunction"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "tpe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Type_PolyFunction"),
              Core.projectionField = (Core.Name "tpe")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

type_PolyFunctionWithTpe :: Phantoms.TTerm Syntax.Type_PolyFunction -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.Type_PolyFunction
type_PolyFunctionWithTpe original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Type_PolyFunction"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Type_PolyFunction"),
              Core.projectionField = (Core.Name "tparams")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tpe"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

type_Project :: Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.Type_Name -> Phantoms.TTerm Syntax.Type_Project
type_Project qual name =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Type_Project"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "qual"),
          Core.fieldTerm = (Phantoms.unTTerm qual)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)}]}))

type_ProjectName :: Phantoms.TTerm Syntax.Type_Project -> Phantoms.TTerm Syntax.Type_Name
type_ProjectName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Type_Project"),
        Core.projectionField = (Core.Name "name")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

type_ProjectQual :: Phantoms.TTerm Syntax.Type_Project -> Phantoms.TTerm Syntax.Type
type_ProjectQual x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Type_Project"),
        Core.projectionField = (Core.Name "qual")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

type_ProjectWithName :: Phantoms.TTerm Syntax.Type_Project -> Phantoms.TTerm Syntax.Type_Name -> Phantoms.TTerm Syntax.Type_Project
type_ProjectWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Type_Project"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "qual"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Type_Project"),
              Core.projectionField = (Core.Name "qual")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

type_ProjectWithQual :: Phantoms.TTerm Syntax.Type_Project -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.Type_Project
type_ProjectWithQual original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Type_Project"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "qual"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Type_Project"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

type_RefName :: Phantoms.TTerm Syntax.Type_Name -> Phantoms.TTerm Syntax.Type_Ref
type_RefName x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Type_Ref"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "name"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

type_RefProject :: Phantoms.TTerm Syntax.Type_Project -> Phantoms.TTerm Syntax.Type_Ref
type_RefProject x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Type_Ref"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "project"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

type_RefSelect :: Phantoms.TTerm Syntax.Type_Select -> Phantoms.TTerm Syntax.Type_Ref
type_RefSelect x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Type_Ref"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "select"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

type_RefSingleton :: Phantoms.TTerm Syntax.Type_Singleton -> Phantoms.TTerm Syntax.Type_Ref
type_RefSingleton x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.scala.syntax.Type_Ref"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "singleton"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

type_Refine :: Phantoms.TTerm (Maybe Syntax.Type) -> Phantoms.TTerm [Syntax.Stat] -> Phantoms.TTerm Syntax.Type_Refine
type_Refine tpe stats =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Type_Refine"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "tpe"),
          Core.fieldTerm = (Phantoms.unTTerm tpe)},
        Core.Field {
          Core.fieldName = (Core.Name "stats"),
          Core.fieldTerm = (Phantoms.unTTerm stats)}]}))

type_RefineStats :: Phantoms.TTerm Syntax.Type_Refine -> Phantoms.TTerm [Syntax.Stat]
type_RefineStats x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Type_Refine"),
        Core.projectionField = (Core.Name "stats")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

type_RefineTpe :: Phantoms.TTerm Syntax.Type_Refine -> Phantoms.TTerm (Maybe Syntax.Type)
type_RefineTpe x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Type_Refine"),
        Core.projectionField = (Core.Name "tpe")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

type_RefineWithStats :: Phantoms.TTerm Syntax.Type_Refine -> Phantoms.TTerm [Syntax.Stat] -> Phantoms.TTerm Syntax.Type_Refine
type_RefineWithStats original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Type_Refine"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "tpe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Type_Refine"),
              Core.projectionField = (Core.Name "tpe")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "stats"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

type_RefineWithTpe :: Phantoms.TTerm Syntax.Type_Refine -> Phantoms.TTerm (Maybe Syntax.Type) -> Phantoms.TTerm Syntax.Type_Refine
type_RefineWithTpe original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Type_Refine"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "tpe"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "stats"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Type_Refine"),
              Core.projectionField = (Core.Name "stats")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

type_Repeated :: Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.Type_Repeated
type_Repeated tpe =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Type_Repeated"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "tpe"),
          Core.fieldTerm = (Phantoms.unTTerm tpe)}]}))

type_RepeatedTpe :: Phantoms.TTerm Syntax.Type_Repeated -> Phantoms.TTerm Syntax.Type
type_RepeatedTpe x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Type_Repeated"),
        Core.projectionField = (Core.Name "tpe")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

type_RepeatedWithTpe :: Phantoms.TTerm Syntax.Type_Repeated -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.Type_Repeated
type_RepeatedWithTpe original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Type_Repeated"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "tpe"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

type_Select :: Phantoms.TTerm Syntax.Data_Ref -> Phantoms.TTerm Syntax.Type_Name -> Phantoms.TTerm Syntax.Type_Select
type_Select qual name =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Type_Select"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "qual"),
          Core.fieldTerm = (Phantoms.unTTerm qual)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)}]}))

type_SelectName :: Phantoms.TTerm Syntax.Type_Select -> Phantoms.TTerm Syntax.Type_Name
type_SelectName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Type_Select"),
        Core.projectionField = (Core.Name "name")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

type_SelectQual :: Phantoms.TTerm Syntax.Type_Select -> Phantoms.TTerm Syntax.Data_Ref
type_SelectQual x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Type_Select"),
        Core.projectionField = (Core.Name "qual")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

type_SelectWithName :: Phantoms.TTerm Syntax.Type_Select -> Phantoms.TTerm Syntax.Type_Name -> Phantoms.TTerm Syntax.Type_Select
type_SelectWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Type_Select"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "qual"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Type_Select"),
              Core.projectionField = (Core.Name "qual")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

type_SelectWithQual :: Phantoms.TTerm Syntax.Type_Select -> Phantoms.TTerm Syntax.Data_Ref -> Phantoms.TTerm Syntax.Type_Select
type_SelectWithQual original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Type_Select"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "qual"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Type_Select"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

type_Singleton :: Phantoms.TTerm Syntax.Data_Ref -> Phantoms.TTerm Syntax.Type_Singleton
type_Singleton ref =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Type_Singleton"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "ref"),
          Core.fieldTerm = (Phantoms.unTTerm ref)}]}))

type_SingletonRef :: Phantoms.TTerm Syntax.Type_Singleton -> Phantoms.TTerm Syntax.Data_Ref
type_SingletonRef x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Type_Singleton"),
        Core.projectionField = (Core.Name "ref")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

type_SingletonWithRef :: Phantoms.TTerm Syntax.Type_Singleton -> Phantoms.TTerm Syntax.Data_Ref -> Phantoms.TTerm Syntax.Type_Singleton
type_SingletonWithRef original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Type_Singleton"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "ref"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

type_Tuple :: Phantoms.TTerm [Syntax.Type] -> Phantoms.TTerm Syntax.Type_Tuple
type_Tuple args =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Type_Tuple"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "args"),
          Core.fieldTerm = (Phantoms.unTTerm args)}]}))

type_TupleArgs :: Phantoms.TTerm Syntax.Type_Tuple -> Phantoms.TTerm [Syntax.Type]
type_TupleArgs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Type_Tuple"),
        Core.projectionField = (Core.Name "args")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

type_TupleWithArgs :: Phantoms.TTerm Syntax.Type_Tuple -> Phantoms.TTerm [Syntax.Type] -> Phantoms.TTerm Syntax.Type_Tuple
type_TupleWithArgs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Type_Tuple"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "args"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

type_TypedParam :: Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.Type_TypedParam
type_TypedParam name typ =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Type_TypedParam"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "typ"),
          Core.fieldTerm = (Phantoms.unTTerm typ)}]}))

type_TypedParamName :: Phantoms.TTerm Syntax.Type_TypedParam -> Phantoms.TTerm Syntax.Name
type_TypedParamName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Type_TypedParam"),
        Core.projectionField = (Core.Name "name")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

type_TypedParamTyp :: Phantoms.TTerm Syntax.Type_TypedParam -> Phantoms.TTerm Syntax.Type
type_TypedParamTyp x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Type_TypedParam"),
        Core.projectionField = (Core.Name "typ")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

type_TypedParamWithName :: Phantoms.TTerm Syntax.Type_TypedParam -> Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.Type_TypedParam
type_TypedParamWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Type_TypedParam"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "typ"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Type_TypedParam"),
              Core.projectionField = (Core.Name "typ")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

type_TypedParamWithTyp :: Phantoms.TTerm Syntax.Type_TypedParam -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.Type_TypedParam
type_TypedParamWithTyp original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Type_TypedParam"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Type_TypedParam"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typ"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

type_Var :: Phantoms.TTerm Syntax.Type_Name -> Phantoms.TTerm Syntax.Type_Var
type_Var name =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Type_Var"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)}]}))

type_VarName :: Phantoms.TTerm Syntax.Type_Var -> Phantoms.TTerm Syntax.Type_Name
type_VarName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Type_Var"),
        Core.projectionField = (Core.Name "name")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

type_VarWithName :: Phantoms.TTerm Syntax.Type_Var -> Phantoms.TTerm Syntax.Type_Name -> Phantoms.TTerm Syntax.Type_Var
type_VarWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Type_Var"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

type_With :: Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.Type_With
type_With lhs rhs =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Type_With"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Phantoms.unTTerm lhs)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTTerm rhs)}]}))

type_WithLhs :: Phantoms.TTerm Syntax.Type_With -> Phantoms.TTerm Syntax.Type
type_WithLhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Type_With"),
        Core.projectionField = (Core.Name "lhs")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

type_WithRhs :: Phantoms.TTerm Syntax.Type_With -> Phantoms.TTerm Syntax.Type
type_WithRhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Type_With"),
        Core.projectionField = (Core.Name "rhs")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

type_WithWithLhs :: Phantoms.TTerm Syntax.Type_With -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.Type_With
type_WithWithLhs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Type_With"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Type_With"),
              Core.projectionField = (Core.Name "rhs")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

type_WithWithRhs :: Phantoms.TTerm Syntax.Type_With -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.Type_With
type_WithWithRhs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.scala.syntax.Type_With"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.scala.syntax.Type_With"),
              Core.projectionField = (Core.Name "lhs")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

unData_Anonymous :: Phantoms.TTerm Syntax.Data_Anonymous -> Phantoms.TTerm ()
unData_Anonymous x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.scala.syntax.Data_Anonymous")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unData_This :: Phantoms.TTerm Syntax.Data_This -> Phantoms.TTerm ()
unData_This x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.scala.syntax.Data_This")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unPredefString :: Phantoms.TTerm Syntax.PredefString -> Phantoms.TTerm String
unPredefString x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.scala.syntax.PredefString")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unQuasi :: Phantoms.TTerm Syntax.Quasi -> Phantoms.TTerm ()
unQuasi x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.scala.syntax.Quasi")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unSelf :: Phantoms.TTerm Syntax.Self -> Phantoms.TTerm ()
unSelf x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.scala.syntax.Self")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unType_AnonymousName :: Phantoms.TTerm Syntax.Type_AnonymousName -> Phantoms.TTerm ()
unType_AnonymousName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.scala.syntax.Type_AnonymousName")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
