-- Note: this is an automatically generated file. Do not edit.

-- | DSL functions for hydra.scala.syntax

module Hydra.Dsl.Scala.Syntax where

import qualified Hydra.Core as Core
import qualified Hydra.Phantoms as Phantoms
import qualified Hydra.Scala.Syntax as Syntax
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
import qualified Data.Int as I

case_ :: Phantoms.TTerm Syntax.Pat -> Phantoms.TTerm (Maybe Syntax.Data) -> Phantoms.TTerm Syntax.Data -> Phantoms.TTerm Syntax.Case
case_ pat cond body =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.Case"),
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
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.Case"),
        Core.projectionField = (Core.Name "body")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

caseCond :: Phantoms.TTerm Syntax.Case -> Phantoms.TTerm (Maybe Syntax.Data)
caseCond x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.Case"),
        Core.projectionField = (Core.Name "cond")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

casePat :: Phantoms.TTerm Syntax.Case -> Phantoms.TTerm Syntax.Pat
casePat x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.Case"),
        Core.projectionField = (Core.Name "pat")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

caseTreeCase :: Phantoms.TTerm Syntax.Case -> Phantoms.TTerm Syntax.CaseTree
caseTreeCase x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.CaseTree"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "case"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

caseTreeTypeCase :: Phantoms.TTerm Syntax.TypeCase -> Phantoms.TTerm Syntax.CaseTree
caseTreeTypeCase x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.CaseTree"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "typeCase"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

caseWithBody :: Phantoms.TTerm Syntax.Case -> Phantoms.TTerm Syntax.Data -> Phantoms.TTerm Syntax.Case
caseWithBody original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.Case"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pat"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.Case"),
              Core.projectionField = (Core.Name "pat")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "cond"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.Case"),
              Core.projectionField = (Core.Name "cond")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

caseWithCond :: Phantoms.TTerm Syntax.Case -> Phantoms.TTerm (Maybe Syntax.Data) -> Phantoms.TTerm Syntax.Case
caseWithCond original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.Case"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pat"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.Case"),
              Core.projectionField = (Core.Name "pat")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "cond"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.Case"),
              Core.projectionField = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

caseWithPat :: Phantoms.TTerm Syntax.Case -> Phantoms.TTerm Syntax.Pat -> Phantoms.TTerm Syntax.Case
caseWithPat original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.Case"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pat"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "cond"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.Case"),
              Core.projectionField = (Core.Name "cond")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.Case"),
              Core.projectionField = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

ctorPrimary :: Phantoms.TTerm Syntax.PrimaryCtor -> Phantoms.TTerm Syntax.Ctor
ctorPrimary x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Ctor"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "primary"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

ctorSecondary :: Phantoms.TTerm Syntax.SecondaryCtor -> Phantoms.TTerm Syntax.Ctor
ctorSecondary x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Ctor"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "secondary"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

primaryCtor :: Phantoms.TTerm [Syntax.Mod] -> Phantoms.TTerm Syntax.Name -> Phantoms.TTerm [[Syntax.ParamData]] -> Phantoms.TTerm Syntax.PrimaryCtor
primaryCtor mods name paramss =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.PrimaryCtor"),
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

primaryCtorMods :: Phantoms.TTerm Syntax.PrimaryCtor -> Phantoms.TTerm [Syntax.Mod]
primaryCtorMods x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.PrimaryCtor"),
        Core.projectionField = (Core.Name "mods")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

primaryCtorName :: Phantoms.TTerm Syntax.PrimaryCtor -> Phantoms.TTerm Syntax.Name
primaryCtorName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.PrimaryCtor"),
        Core.projectionField = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

primaryCtorParamss :: Phantoms.TTerm Syntax.PrimaryCtor -> Phantoms.TTerm [[Syntax.ParamData]]
primaryCtorParamss x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.PrimaryCtor"),
        Core.projectionField = (Core.Name "paramss")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

primaryCtorWithMods :: Phantoms.TTerm Syntax.PrimaryCtor -> Phantoms.TTerm [Syntax.Mod] -> Phantoms.TTerm Syntax.PrimaryCtor
primaryCtorWithMods original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.PrimaryCtor"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.PrimaryCtor"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "paramss"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.PrimaryCtor"),
              Core.projectionField = (Core.Name "paramss")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

primaryCtorWithName :: Phantoms.TTerm Syntax.PrimaryCtor -> Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.PrimaryCtor
primaryCtorWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.PrimaryCtor"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.PrimaryCtor"),
              Core.projectionField = (Core.Name "mods")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "paramss"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.PrimaryCtor"),
              Core.projectionField = (Core.Name "paramss")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

primaryCtorWithParamss :: Phantoms.TTerm Syntax.PrimaryCtor -> Phantoms.TTerm [[Syntax.ParamData]] -> Phantoms.TTerm Syntax.PrimaryCtor
primaryCtorWithParamss original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.PrimaryCtor"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.PrimaryCtor"),
              Core.projectionField = (Core.Name "mods")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.PrimaryCtor"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "paramss"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

secondaryCtor :: Phantoms.TTerm [Syntax.Mod] -> Phantoms.TTerm Syntax.Name -> Phantoms.TTerm [[Syntax.ParamData]] -> Phantoms.TTerm Syntax.Init -> Phantoms.TTerm [Syntax.Stat] -> Phantoms.TTerm Syntax.SecondaryCtor
secondaryCtor mods name paramss init stats =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.SecondaryCtor"),
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

secondaryCtorInit :: Phantoms.TTerm Syntax.SecondaryCtor -> Phantoms.TTerm Syntax.Init
secondaryCtorInit x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.SecondaryCtor"),
        Core.projectionField = (Core.Name "init")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

secondaryCtorMods :: Phantoms.TTerm Syntax.SecondaryCtor -> Phantoms.TTerm [Syntax.Mod]
secondaryCtorMods x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.SecondaryCtor"),
        Core.projectionField = (Core.Name "mods")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

secondaryCtorName :: Phantoms.TTerm Syntax.SecondaryCtor -> Phantoms.TTerm Syntax.Name
secondaryCtorName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.SecondaryCtor"),
        Core.projectionField = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

secondaryCtorParamss :: Phantoms.TTerm Syntax.SecondaryCtor -> Phantoms.TTerm [[Syntax.ParamData]]
secondaryCtorParamss x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.SecondaryCtor"),
        Core.projectionField = (Core.Name "paramss")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

secondaryCtorStats :: Phantoms.TTerm Syntax.SecondaryCtor -> Phantoms.TTerm [Syntax.Stat]
secondaryCtorStats x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.SecondaryCtor"),
        Core.projectionField = (Core.Name "stats")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

secondaryCtorWithInit :: Phantoms.TTerm Syntax.SecondaryCtor -> Phantoms.TTerm Syntax.Init -> Phantoms.TTerm Syntax.SecondaryCtor
secondaryCtorWithInit original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.SecondaryCtor"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.SecondaryCtor"),
              Core.projectionField = (Core.Name "mods")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.SecondaryCtor"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "paramss"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.SecondaryCtor"),
              Core.projectionField = (Core.Name "paramss")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "init"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "stats"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.SecondaryCtor"),
              Core.projectionField = (Core.Name "stats")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

secondaryCtorWithMods :: Phantoms.TTerm Syntax.SecondaryCtor -> Phantoms.TTerm [Syntax.Mod] -> Phantoms.TTerm Syntax.SecondaryCtor
secondaryCtorWithMods original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.SecondaryCtor"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.SecondaryCtor"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "paramss"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.SecondaryCtor"),
              Core.projectionField = (Core.Name "paramss")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "init"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.SecondaryCtor"),
              Core.projectionField = (Core.Name "init")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "stats"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.SecondaryCtor"),
              Core.projectionField = (Core.Name "stats")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

secondaryCtorWithName :: Phantoms.TTerm Syntax.SecondaryCtor -> Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.SecondaryCtor
secondaryCtorWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.SecondaryCtor"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.SecondaryCtor"),
              Core.projectionField = (Core.Name "mods")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "paramss"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.SecondaryCtor"),
              Core.projectionField = (Core.Name "paramss")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "init"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.SecondaryCtor"),
              Core.projectionField = (Core.Name "init")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "stats"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.SecondaryCtor"),
              Core.projectionField = (Core.Name "stats")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

secondaryCtorWithParamss :: Phantoms.TTerm Syntax.SecondaryCtor -> Phantoms.TTerm [[Syntax.ParamData]] -> Phantoms.TTerm Syntax.SecondaryCtor
secondaryCtorWithParamss original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.SecondaryCtor"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.SecondaryCtor"),
              Core.projectionField = (Core.Name "mods")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.SecondaryCtor"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "paramss"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "init"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.SecondaryCtor"),
              Core.projectionField = (Core.Name "init")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "stats"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.SecondaryCtor"),
              Core.projectionField = (Core.Name "stats")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

secondaryCtorWithStats :: Phantoms.TTerm Syntax.SecondaryCtor -> Phantoms.TTerm [Syntax.Stat] -> Phantoms.TTerm Syntax.SecondaryCtor
secondaryCtorWithStats original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.SecondaryCtor"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.SecondaryCtor"),
              Core.projectionField = (Core.Name "mods")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.SecondaryCtor"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "paramss"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.SecondaryCtor"),
              Core.projectionField = (Core.Name "paramss")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "init"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.SecondaryCtor"),
              Core.projectionField = (Core.Name "init")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "stats"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

dataAnnotate :: Phantoms.TTerm Syntax.AnnotateData -> Phantoms.TTerm Syntax.Data
dataAnnotate x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Data"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "annotate"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

dataApply :: Phantoms.TTerm Syntax.ApplyData -> Phantoms.TTerm Syntax.Data
dataApply x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Data"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "apply"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

dataApplyType :: Phantoms.TTerm Syntax.ApplyTypeData -> Phantoms.TTerm Syntax.Data
dataApplyType x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Data"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "applyType"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

dataApplyUsing :: Phantoms.TTerm Syntax.ApplyUsingData -> Phantoms.TTerm Syntax.Data
dataApplyUsing x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Data"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "applyUsing"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

dataAscribe :: Phantoms.TTerm Syntax.AscribeData -> Phantoms.TTerm Syntax.Data
dataAscribe x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Data"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "ascribe"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

dataAssign :: Phantoms.TTerm Syntax.AssignData -> Phantoms.TTerm Syntax.Data
dataAssign x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Data"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "assign"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

dataBlock :: Phantoms.TTerm Syntax.BlockData -> Phantoms.TTerm Syntax.Data
dataBlock x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Data"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "block"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

dataDo :: Phantoms.TTerm Syntax.DoData -> Phantoms.TTerm Syntax.Data
dataDo x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Data"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "do"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

dataEndMarker :: Phantoms.TTerm Syntax.EndMarkerData -> Phantoms.TTerm Syntax.Data
dataEndMarker x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Data"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "endMarker"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

dataEta :: Phantoms.TTerm Syntax.EtaData -> Phantoms.TTerm Syntax.Data
dataEta x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Data"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "eta"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

dataFor :: Phantoms.TTerm Syntax.ForData -> Phantoms.TTerm Syntax.Data
dataFor x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Data"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "for"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

dataForYield :: Phantoms.TTerm Syntax.ForYieldData -> Phantoms.TTerm Syntax.Data
dataForYield x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Data"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "forYield"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

dataFunction :: Phantoms.TTerm Syntax.FunctionData -> Phantoms.TTerm Syntax.Data
dataFunction x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Data"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "function"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

dataContextFunction :: Phantoms.TTerm Syntax.ContextFunctionData -> Phantoms.TTerm Syntax.Data
dataContextFunction x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Data"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "contextFunction"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

dataIf :: Phantoms.TTerm Syntax.IfData -> Phantoms.TTerm Syntax.Data
dataIf x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Data"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "if"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

dataInterpolate :: Phantoms.TTerm Syntax.InterpolateData -> Phantoms.TTerm Syntax.Data
dataInterpolate x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Data"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "interpolate"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

dataLit :: Phantoms.TTerm Syntax.Lit -> Phantoms.TTerm Syntax.Data
dataLit x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Data"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "lit"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

dataMatch :: Phantoms.TTerm Syntax.MatchData -> Phantoms.TTerm Syntax.Data
dataMatch x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Data"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "match"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

dataNew :: Phantoms.TTerm Syntax.NewData -> Phantoms.TTerm Syntax.Data
dataNew x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Data"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "new"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

dataNewAnonymous :: Phantoms.TTerm Syntax.NewAnonymousData -> Phantoms.TTerm Syntax.Data
dataNewAnonymous x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Data"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "newAnonymous"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

dataParam :: Phantoms.TTerm Syntax.ParamData -> Phantoms.TTerm Syntax.Data
dataParam x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Data"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "param"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

dataPartialFunction :: Phantoms.TTerm Syntax.PartialFunctionData -> Phantoms.TTerm Syntax.Data
dataPartialFunction x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Data"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "partialFunction"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

dataPlaceholder :: Phantoms.TTerm Syntax.Data
dataPlaceholder =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Data"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "placeholder"),
        Core.fieldTerm = Core.TermUnit}}))

dataPolyFunction :: Phantoms.TTerm Syntax.PolyFunctionData -> Phantoms.TTerm Syntax.Data
dataPolyFunction x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Data"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "polyFunction"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

dataRef :: Phantoms.TTerm Syntax.RefData -> Phantoms.TTerm Syntax.Data
dataRef x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Data"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "ref"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

dataRepeated :: Phantoms.TTerm Syntax.RepeatedData -> Phantoms.TTerm Syntax.Data
dataRepeated x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Data"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "repeated"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

dataReturn :: Phantoms.TTerm Syntax.ReturnData -> Phantoms.TTerm Syntax.Data
dataReturn x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Data"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "return"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

dataThrow :: Phantoms.TTerm Syntax.ThrowData -> Phantoms.TTerm Syntax.Data
dataThrow x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Data"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "throw"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

dataTry :: Phantoms.TTerm Syntax.TryData -> Phantoms.TTerm Syntax.Data
dataTry x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Data"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "try"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

dataTryWithHandler :: Phantoms.TTerm Syntax.TryWithHandlerData -> Phantoms.TTerm Syntax.Data
dataTryWithHandler x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Data"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "tryWithHandler"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

dataTuple :: Phantoms.TTerm Syntax.TupleData -> Phantoms.TTerm Syntax.Data
dataTuple x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Data"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "tuple"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

dataWhile :: Phantoms.TTerm Syntax.WhileData -> Phantoms.TTerm Syntax.Data
dataWhile x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Data"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "while"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

annotateData :: Phantoms.TTerm Syntax.Data -> Phantoms.TTerm [Syntax.AnnotMod] -> Phantoms.TTerm Syntax.AnnotateData
annotateData expr annots =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.AnnotateData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expr"),
          Core.fieldTerm = (Phantoms.unTTerm expr)},
        Core.Field {
          Core.fieldName = (Core.Name "annots"),
          Core.fieldTerm = (Phantoms.unTTerm annots)}]}))

annotateDataAnnots :: Phantoms.TTerm Syntax.AnnotateData -> Phantoms.TTerm [Syntax.AnnotMod]
annotateDataAnnots x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.AnnotateData"),
        Core.projectionField = (Core.Name "annots")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

annotateDataExpr :: Phantoms.TTerm Syntax.AnnotateData -> Phantoms.TTerm Syntax.Data
annotateDataExpr x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.AnnotateData"),
        Core.projectionField = (Core.Name "expr")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

annotateDataWithAnnots :: Phantoms.TTerm Syntax.AnnotateData -> Phantoms.TTerm [Syntax.AnnotMod] -> Phantoms.TTerm Syntax.AnnotateData
annotateDataWithAnnots original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.AnnotateData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expr"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.AnnotateData"),
              Core.projectionField = (Core.Name "expr")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "annots"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

annotateDataWithExpr :: Phantoms.TTerm Syntax.AnnotateData -> Phantoms.TTerm Syntax.Data -> Phantoms.TTerm Syntax.AnnotateData
annotateDataWithExpr original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.AnnotateData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expr"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "annots"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.AnnotateData"),
              Core.projectionField = (Core.Name "annots")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

anonymousData :: Phantoms.TTerm () -> Phantoms.TTerm Syntax.AnonymousData
anonymousData x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.scala.syntax.AnonymousData"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

applyData :: Phantoms.TTerm Syntax.Data -> Phantoms.TTerm [Syntax.Data] -> Phantoms.TTerm Syntax.ApplyData
applyData fun args =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ApplyData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "fun"),
          Core.fieldTerm = (Phantoms.unTTerm fun)},
        Core.Field {
          Core.fieldName = (Core.Name "args"),
          Core.fieldTerm = (Phantoms.unTTerm args)}]}))

applyDataArgs :: Phantoms.TTerm Syntax.ApplyData -> Phantoms.TTerm [Syntax.Data]
applyDataArgs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ApplyData"),
        Core.projectionField = (Core.Name "args")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

applyDataFun :: Phantoms.TTerm Syntax.ApplyData -> Phantoms.TTerm Syntax.Data
applyDataFun x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ApplyData"),
        Core.projectionField = (Core.Name "fun")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

applyInfixData :: Phantoms.TTerm Syntax.Data -> Phantoms.TTerm Syntax.NameData -> Phantoms.TTerm [Syntax.Type] -> Phantoms.TTerm [Syntax.Data] -> Phantoms.TTerm Syntax.ApplyInfixData
applyInfixData lhs op targs args =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ApplyInfixData"),
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

applyInfixDataArgs :: Phantoms.TTerm Syntax.ApplyInfixData -> Phantoms.TTerm [Syntax.Data]
applyInfixDataArgs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ApplyInfixData"),
        Core.projectionField = (Core.Name "args")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

applyInfixDataLhs :: Phantoms.TTerm Syntax.ApplyInfixData -> Phantoms.TTerm Syntax.Data
applyInfixDataLhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ApplyInfixData"),
        Core.projectionField = (Core.Name "lhs")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

applyInfixDataOp :: Phantoms.TTerm Syntax.ApplyInfixData -> Phantoms.TTerm Syntax.NameData
applyInfixDataOp x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ApplyInfixData"),
        Core.projectionField = (Core.Name "op")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

applyInfixDataTargs :: Phantoms.TTerm Syntax.ApplyInfixData -> Phantoms.TTerm [Syntax.Type]
applyInfixDataTargs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ApplyInfixData"),
        Core.projectionField = (Core.Name "targs")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

applyInfixDataWithArgs :: Phantoms.TTerm Syntax.ApplyInfixData -> Phantoms.TTerm [Syntax.Data] -> Phantoms.TTerm Syntax.ApplyInfixData
applyInfixDataWithArgs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ApplyInfixData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ApplyInfixData"),
              Core.projectionField = (Core.Name "lhs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "op"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ApplyInfixData"),
              Core.projectionField = (Core.Name "op")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "targs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ApplyInfixData"),
              Core.projectionField = (Core.Name "targs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "args"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

applyInfixDataWithLhs :: Phantoms.TTerm Syntax.ApplyInfixData -> Phantoms.TTerm Syntax.Data -> Phantoms.TTerm Syntax.ApplyInfixData
applyInfixDataWithLhs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ApplyInfixData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "op"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ApplyInfixData"),
              Core.projectionField = (Core.Name "op")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "targs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ApplyInfixData"),
              Core.projectionField = (Core.Name "targs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "args"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ApplyInfixData"),
              Core.projectionField = (Core.Name "args")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

applyInfixDataWithOp :: Phantoms.TTerm Syntax.ApplyInfixData -> Phantoms.TTerm Syntax.NameData -> Phantoms.TTerm Syntax.ApplyInfixData
applyInfixDataWithOp original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ApplyInfixData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ApplyInfixData"),
              Core.projectionField = (Core.Name "lhs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "op"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "targs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ApplyInfixData"),
              Core.projectionField = (Core.Name "targs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "args"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ApplyInfixData"),
              Core.projectionField = (Core.Name "args")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

applyInfixDataWithTargs :: Phantoms.TTerm Syntax.ApplyInfixData -> Phantoms.TTerm [Syntax.Type] -> Phantoms.TTerm Syntax.ApplyInfixData
applyInfixDataWithTargs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ApplyInfixData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ApplyInfixData"),
              Core.projectionField = (Core.Name "lhs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "op"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ApplyInfixData"),
              Core.projectionField = (Core.Name "op")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "targs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "args"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ApplyInfixData"),
              Core.projectionField = (Core.Name "args")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

applyTypeData :: Phantoms.TTerm Syntax.Data -> Phantoms.TTerm Syntax.NameData -> Phantoms.TTerm [Syntax.Type] -> Phantoms.TTerm [Syntax.Data] -> Phantoms.TTerm Syntax.ApplyTypeData
applyTypeData lhs op targs args =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ApplyTypeData"),
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

applyTypeDataArgs :: Phantoms.TTerm Syntax.ApplyTypeData -> Phantoms.TTerm [Syntax.Data]
applyTypeDataArgs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ApplyTypeData"),
        Core.projectionField = (Core.Name "args")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

applyTypeDataLhs :: Phantoms.TTerm Syntax.ApplyTypeData -> Phantoms.TTerm Syntax.Data
applyTypeDataLhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ApplyTypeData"),
        Core.projectionField = (Core.Name "lhs")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

applyTypeDataOp :: Phantoms.TTerm Syntax.ApplyTypeData -> Phantoms.TTerm Syntax.NameData
applyTypeDataOp x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ApplyTypeData"),
        Core.projectionField = (Core.Name "op")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

applyTypeDataTargs :: Phantoms.TTerm Syntax.ApplyTypeData -> Phantoms.TTerm [Syntax.Type]
applyTypeDataTargs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ApplyTypeData"),
        Core.projectionField = (Core.Name "targs")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

applyTypeDataWithArgs :: Phantoms.TTerm Syntax.ApplyTypeData -> Phantoms.TTerm [Syntax.Data] -> Phantoms.TTerm Syntax.ApplyTypeData
applyTypeDataWithArgs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ApplyTypeData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ApplyTypeData"),
              Core.projectionField = (Core.Name "lhs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "op"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ApplyTypeData"),
              Core.projectionField = (Core.Name "op")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "targs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ApplyTypeData"),
              Core.projectionField = (Core.Name "targs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "args"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

applyTypeDataWithLhs :: Phantoms.TTerm Syntax.ApplyTypeData -> Phantoms.TTerm Syntax.Data -> Phantoms.TTerm Syntax.ApplyTypeData
applyTypeDataWithLhs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ApplyTypeData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "op"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ApplyTypeData"),
              Core.projectionField = (Core.Name "op")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "targs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ApplyTypeData"),
              Core.projectionField = (Core.Name "targs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "args"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ApplyTypeData"),
              Core.projectionField = (Core.Name "args")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

applyTypeDataWithOp :: Phantoms.TTerm Syntax.ApplyTypeData -> Phantoms.TTerm Syntax.NameData -> Phantoms.TTerm Syntax.ApplyTypeData
applyTypeDataWithOp original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ApplyTypeData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ApplyTypeData"),
              Core.projectionField = (Core.Name "lhs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "op"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "targs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ApplyTypeData"),
              Core.projectionField = (Core.Name "targs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "args"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ApplyTypeData"),
              Core.projectionField = (Core.Name "args")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

applyTypeDataWithTargs :: Phantoms.TTerm Syntax.ApplyTypeData -> Phantoms.TTerm [Syntax.Type] -> Phantoms.TTerm Syntax.ApplyTypeData
applyTypeDataWithTargs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ApplyTypeData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ApplyTypeData"),
              Core.projectionField = (Core.Name "lhs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "op"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ApplyTypeData"),
              Core.projectionField = (Core.Name "op")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "targs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "args"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ApplyTypeData"),
              Core.projectionField = (Core.Name "args")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

applyUnaryData :: Phantoms.TTerm Syntax.NameData -> Phantoms.TTerm Syntax.Data -> Phantoms.TTerm Syntax.ApplyUnaryData
applyUnaryData op arg =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ApplyUnaryData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "op"),
          Core.fieldTerm = (Phantoms.unTTerm op)},
        Core.Field {
          Core.fieldName = (Core.Name "arg"),
          Core.fieldTerm = (Phantoms.unTTerm arg)}]}))

applyUnaryDataArg :: Phantoms.TTerm Syntax.ApplyUnaryData -> Phantoms.TTerm Syntax.Data
applyUnaryDataArg x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ApplyUnaryData"),
        Core.projectionField = (Core.Name "arg")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

applyUnaryDataOp :: Phantoms.TTerm Syntax.ApplyUnaryData -> Phantoms.TTerm Syntax.NameData
applyUnaryDataOp x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ApplyUnaryData"),
        Core.projectionField = (Core.Name "op")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

applyUnaryDataWithArg :: Phantoms.TTerm Syntax.ApplyUnaryData -> Phantoms.TTerm Syntax.Data -> Phantoms.TTerm Syntax.ApplyUnaryData
applyUnaryDataWithArg original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ApplyUnaryData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "op"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ApplyUnaryData"),
              Core.projectionField = (Core.Name "op")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "arg"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

applyUnaryDataWithOp :: Phantoms.TTerm Syntax.ApplyUnaryData -> Phantoms.TTerm Syntax.NameData -> Phantoms.TTerm Syntax.ApplyUnaryData
applyUnaryDataWithOp original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ApplyUnaryData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "op"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "arg"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ApplyUnaryData"),
              Core.projectionField = (Core.Name "arg")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

applyUsingData :: Phantoms.TTerm Syntax.Data -> Phantoms.TTerm [Syntax.Data] -> Phantoms.TTerm Syntax.ApplyUsingData
applyUsingData fun targs =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ApplyUsingData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "fun"),
          Core.fieldTerm = (Phantoms.unTTerm fun)},
        Core.Field {
          Core.fieldName = (Core.Name "targs"),
          Core.fieldTerm = (Phantoms.unTTerm targs)}]}))

applyUsingDataFun :: Phantoms.TTerm Syntax.ApplyUsingData -> Phantoms.TTerm Syntax.Data
applyUsingDataFun x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ApplyUsingData"),
        Core.projectionField = (Core.Name "fun")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

applyUsingDataTargs :: Phantoms.TTerm Syntax.ApplyUsingData -> Phantoms.TTerm [Syntax.Data]
applyUsingDataTargs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ApplyUsingData"),
        Core.projectionField = (Core.Name "targs")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

applyUsingDataWithFun :: Phantoms.TTerm Syntax.ApplyUsingData -> Phantoms.TTerm Syntax.Data -> Phantoms.TTerm Syntax.ApplyUsingData
applyUsingDataWithFun original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ApplyUsingData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "fun"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "targs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ApplyUsingData"),
              Core.projectionField = (Core.Name "targs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

applyUsingDataWithTargs :: Phantoms.TTerm Syntax.ApplyUsingData -> Phantoms.TTerm [Syntax.Data] -> Phantoms.TTerm Syntax.ApplyUsingData
applyUsingDataWithTargs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ApplyUsingData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "fun"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ApplyUsingData"),
              Core.projectionField = (Core.Name "fun")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "targs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

applyDataWithArgs :: Phantoms.TTerm Syntax.ApplyData -> Phantoms.TTerm [Syntax.Data] -> Phantoms.TTerm Syntax.ApplyData
applyDataWithArgs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ApplyData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "fun"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ApplyData"),
              Core.projectionField = (Core.Name "fun")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "args"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

applyDataWithFun :: Phantoms.TTerm Syntax.ApplyData -> Phantoms.TTerm Syntax.Data -> Phantoms.TTerm Syntax.ApplyData
applyDataWithFun original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ApplyData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "fun"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "args"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ApplyData"),
              Core.projectionField = (Core.Name "args")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

ascribeData :: Phantoms.TTerm Syntax.Data -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.AscribeData
ascribeData expr tpe =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.AscribeData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expr"),
          Core.fieldTerm = (Phantoms.unTTerm expr)},
        Core.Field {
          Core.fieldName = (Core.Name "tpe"),
          Core.fieldTerm = (Phantoms.unTTerm tpe)}]}))

ascribeDataExpr :: Phantoms.TTerm Syntax.AscribeData -> Phantoms.TTerm Syntax.Data
ascribeDataExpr x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.AscribeData"),
        Core.projectionField = (Core.Name "expr")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

ascribeDataTpe :: Phantoms.TTerm Syntax.AscribeData -> Phantoms.TTerm Syntax.Type
ascribeDataTpe x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.AscribeData"),
        Core.projectionField = (Core.Name "tpe")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

ascribeDataWithExpr :: Phantoms.TTerm Syntax.AscribeData -> Phantoms.TTerm Syntax.Data -> Phantoms.TTerm Syntax.AscribeData
ascribeDataWithExpr original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.AscribeData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expr"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "tpe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.AscribeData"),
              Core.projectionField = (Core.Name "tpe")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

ascribeDataWithTpe :: Phantoms.TTerm Syntax.AscribeData -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.AscribeData
ascribeDataWithTpe original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.AscribeData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expr"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.AscribeData"),
              Core.projectionField = (Core.Name "expr")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tpe"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

assignData :: Phantoms.TTerm Syntax.Data -> Phantoms.TTerm Syntax.Data -> Phantoms.TTerm Syntax.AssignData
assignData lhs rhs =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.AssignData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Phantoms.unTTerm lhs)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTTerm rhs)}]}))

assignDataLhs :: Phantoms.TTerm Syntax.AssignData -> Phantoms.TTerm Syntax.Data
assignDataLhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.AssignData"),
        Core.projectionField = (Core.Name "lhs")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

assignDataRhs :: Phantoms.TTerm Syntax.AssignData -> Phantoms.TTerm Syntax.Data
assignDataRhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.AssignData"),
        Core.projectionField = (Core.Name "rhs")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

assignDataWithLhs :: Phantoms.TTerm Syntax.AssignData -> Phantoms.TTerm Syntax.Data -> Phantoms.TTerm Syntax.AssignData
assignDataWithLhs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.AssignData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.AssignData"),
              Core.projectionField = (Core.Name "rhs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

assignDataWithRhs :: Phantoms.TTerm Syntax.AssignData -> Phantoms.TTerm Syntax.Data -> Phantoms.TTerm Syntax.AssignData
assignDataWithRhs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.AssignData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.AssignData"),
              Core.projectionField = (Core.Name "lhs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

blockData :: Phantoms.TTerm [Syntax.Stat] -> Phantoms.TTerm Syntax.BlockData
blockData stats =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.BlockData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "stats"),
          Core.fieldTerm = (Phantoms.unTTerm stats)}]}))

blockDataStats :: Phantoms.TTerm Syntax.BlockData -> Phantoms.TTerm [Syntax.Stat]
blockDataStats x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.BlockData"),
        Core.projectionField = (Core.Name "stats")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

blockDataWithStats :: Phantoms.TTerm Syntax.BlockData -> Phantoms.TTerm [Syntax.Stat] -> Phantoms.TTerm Syntax.BlockData
blockDataWithStats original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.BlockData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "stats"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

contextFunctionData :: Phantoms.TTerm [Syntax.ParamData] -> Phantoms.TTerm Syntax.Data -> Phantoms.TTerm Syntax.ContextFunctionData
contextFunctionData params body =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ContextFunctionData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Phantoms.unTTerm params)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm body)}]}))

contextFunctionDataBody :: Phantoms.TTerm Syntax.ContextFunctionData -> Phantoms.TTerm Syntax.Data
contextFunctionDataBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ContextFunctionData"),
        Core.projectionField = (Core.Name "body")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

contextFunctionDataParams :: Phantoms.TTerm Syntax.ContextFunctionData -> Phantoms.TTerm [Syntax.ParamData]
contextFunctionDataParams x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ContextFunctionData"),
        Core.projectionField = (Core.Name "params")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

contextFunctionDataWithBody :: Phantoms.TTerm Syntax.ContextFunctionData -> Phantoms.TTerm Syntax.Data -> Phantoms.TTerm Syntax.ContextFunctionData
contextFunctionDataWithBody original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ContextFunctionData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ContextFunctionData"),
              Core.projectionField = (Core.Name "params")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

contextFunctionDataWithParams :: Phantoms.TTerm Syntax.ContextFunctionData -> Phantoms.TTerm [Syntax.ParamData] -> Phantoms.TTerm Syntax.ContextFunctionData
contextFunctionDataWithParams original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ContextFunctionData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ContextFunctionData"),
              Core.projectionField = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

doData :: Phantoms.TTerm Syntax.Data -> Phantoms.TTerm Syntax.Data -> Phantoms.TTerm Syntax.DoData
doData body expr =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.DoData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm body)},
        Core.Field {
          Core.fieldName = (Core.Name "expr"),
          Core.fieldTerm = (Phantoms.unTTerm expr)}]}))

doDataBody :: Phantoms.TTerm Syntax.DoData -> Phantoms.TTerm Syntax.Data
doDataBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DoData"),
        Core.projectionField = (Core.Name "body")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

doDataExpr :: Phantoms.TTerm Syntax.DoData -> Phantoms.TTerm Syntax.Data
doDataExpr x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DoData"),
        Core.projectionField = (Core.Name "expr")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

doDataWithBody :: Phantoms.TTerm Syntax.DoData -> Phantoms.TTerm Syntax.Data -> Phantoms.TTerm Syntax.DoData
doDataWithBody original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.DoData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "expr"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DoData"),
              Core.projectionField = (Core.Name "expr")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

doDataWithExpr :: Phantoms.TTerm Syntax.DoData -> Phantoms.TTerm Syntax.Data -> Phantoms.TTerm Syntax.DoData
doDataWithExpr original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.DoData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DoData"),
              Core.projectionField = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "expr"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

endMarkerData :: Phantoms.TTerm Syntax.NameData -> Phantoms.TTerm Syntax.EndMarkerData
endMarkerData name =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.EndMarkerData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)}]}))

endMarkerDataName :: Phantoms.TTerm Syntax.EndMarkerData -> Phantoms.TTerm Syntax.NameData
endMarkerDataName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.EndMarkerData"),
        Core.projectionField = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

endMarkerDataWithName :: Phantoms.TTerm Syntax.EndMarkerData -> Phantoms.TTerm Syntax.NameData -> Phantoms.TTerm Syntax.EndMarkerData
endMarkerDataWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.EndMarkerData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

etaData :: Phantoms.TTerm Syntax.Data -> Phantoms.TTerm Syntax.EtaData
etaData expr =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.EtaData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expr"),
          Core.fieldTerm = (Phantoms.unTTerm expr)}]}))

etaDataExpr :: Phantoms.TTerm Syntax.EtaData -> Phantoms.TTerm Syntax.Data
etaDataExpr x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.EtaData"),
        Core.projectionField = (Core.Name "expr")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

etaDataWithExpr :: Phantoms.TTerm Syntax.EtaData -> Phantoms.TTerm Syntax.Data -> Phantoms.TTerm Syntax.EtaData
etaDataWithExpr original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.EtaData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expr"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

forData :: Phantoms.TTerm [Syntax.Enumerator] -> Phantoms.TTerm Syntax.ForData
forData enums =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ForData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "enums"),
          Core.fieldTerm = (Phantoms.unTTerm enums)}]}))

forDataEnums :: Phantoms.TTerm Syntax.ForData -> Phantoms.TTerm [Syntax.Enumerator]
forDataEnums x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ForData"),
        Core.projectionField = (Core.Name "enums")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

forDataWithEnums :: Phantoms.TTerm Syntax.ForData -> Phantoms.TTerm [Syntax.Enumerator] -> Phantoms.TTerm Syntax.ForData
forDataWithEnums original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ForData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "enums"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

forYieldData :: Phantoms.TTerm [Syntax.Enumerator] -> Phantoms.TTerm Syntax.ForYieldData
forYieldData enums =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ForYieldData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "enums"),
          Core.fieldTerm = (Phantoms.unTTerm enums)}]}))

forYieldDataEnums :: Phantoms.TTerm Syntax.ForYieldData -> Phantoms.TTerm [Syntax.Enumerator]
forYieldDataEnums x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ForYieldData"),
        Core.projectionField = (Core.Name "enums")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

forYieldDataWithEnums :: Phantoms.TTerm Syntax.ForYieldData -> Phantoms.TTerm [Syntax.Enumerator] -> Phantoms.TTerm Syntax.ForYieldData
forYieldDataWithEnums original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ForYieldData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "enums"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

functionData :: Phantoms.TTerm [Syntax.ParamData] -> Phantoms.TTerm Syntax.Data -> Phantoms.TTerm Syntax.FunctionData
functionData params body =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.FunctionData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Phantoms.unTTerm params)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm body)}]}))

functionDataBody :: Phantoms.TTerm Syntax.FunctionData -> Phantoms.TTerm Syntax.Data
functionDataBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.FunctionData"),
        Core.projectionField = (Core.Name "body")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

functionDataParams :: Phantoms.TTerm Syntax.FunctionData -> Phantoms.TTerm [Syntax.ParamData]
functionDataParams x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.FunctionData"),
        Core.projectionField = (Core.Name "params")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

functionDataWithBody :: Phantoms.TTerm Syntax.FunctionData -> Phantoms.TTerm Syntax.Data -> Phantoms.TTerm Syntax.FunctionData
functionDataWithBody original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.FunctionData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.FunctionData"),
              Core.projectionField = (Core.Name "params")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

functionDataWithParams :: Phantoms.TTerm Syntax.FunctionData -> Phantoms.TTerm [Syntax.ParamData] -> Phantoms.TTerm Syntax.FunctionData
functionDataWithParams original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.FunctionData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.FunctionData"),
              Core.projectionField = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

ifData :: Phantoms.TTerm Syntax.Data -> Phantoms.TTerm Syntax.Data -> Phantoms.TTerm Syntax.Data -> Phantoms.TTerm Syntax.IfData
ifData cond thenp elsep =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.IfData"),
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

ifDataCond :: Phantoms.TTerm Syntax.IfData -> Phantoms.TTerm Syntax.Data
ifDataCond x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.IfData"),
        Core.projectionField = (Core.Name "cond")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

ifDataElsep :: Phantoms.TTerm Syntax.IfData -> Phantoms.TTerm Syntax.Data
ifDataElsep x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.IfData"),
        Core.projectionField = (Core.Name "elsep")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

ifDataThenp :: Phantoms.TTerm Syntax.IfData -> Phantoms.TTerm Syntax.Data
ifDataThenp x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.IfData"),
        Core.projectionField = (Core.Name "thenp")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

ifDataWithCond :: Phantoms.TTerm Syntax.IfData -> Phantoms.TTerm Syntax.Data -> Phantoms.TTerm Syntax.IfData
ifDataWithCond original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.IfData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "cond"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "thenp"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.IfData"),
              Core.projectionField = (Core.Name "thenp")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "elsep"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.IfData"),
              Core.projectionField = (Core.Name "elsep")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

ifDataWithElsep :: Phantoms.TTerm Syntax.IfData -> Phantoms.TTerm Syntax.Data -> Phantoms.TTerm Syntax.IfData
ifDataWithElsep original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.IfData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "cond"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.IfData"),
              Core.projectionField = (Core.Name "cond")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "thenp"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.IfData"),
              Core.projectionField = (Core.Name "thenp")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "elsep"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

ifDataWithThenp :: Phantoms.TTerm Syntax.IfData -> Phantoms.TTerm Syntax.Data -> Phantoms.TTerm Syntax.IfData
ifDataWithThenp original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.IfData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "cond"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.IfData"),
              Core.projectionField = (Core.Name "cond")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "thenp"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "elsep"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.IfData"),
              Core.projectionField = (Core.Name "elsep")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

interpolateData :: Phantoms.TTerm Syntax.NameData -> Phantoms.TTerm [Syntax.Lit] -> Phantoms.TTerm [Syntax.Data] -> Phantoms.TTerm Syntax.InterpolateData
interpolateData prefix parts args =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.InterpolateData"),
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

interpolateDataArgs :: Phantoms.TTerm Syntax.InterpolateData -> Phantoms.TTerm [Syntax.Data]
interpolateDataArgs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.InterpolateData"),
        Core.projectionField = (Core.Name "args")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

interpolateDataParts :: Phantoms.TTerm Syntax.InterpolateData -> Phantoms.TTerm [Syntax.Lit]
interpolateDataParts x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.InterpolateData"),
        Core.projectionField = (Core.Name "parts")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

interpolateDataPrefix :: Phantoms.TTerm Syntax.InterpolateData -> Phantoms.TTerm Syntax.NameData
interpolateDataPrefix x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.InterpolateData"),
        Core.projectionField = (Core.Name "prefix")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

interpolateDataWithArgs :: Phantoms.TTerm Syntax.InterpolateData -> Phantoms.TTerm [Syntax.Data] -> Phantoms.TTerm Syntax.InterpolateData
interpolateDataWithArgs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.InterpolateData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "prefix"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.InterpolateData"),
              Core.projectionField = (Core.Name "prefix")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "parts"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.InterpolateData"),
              Core.projectionField = (Core.Name "parts")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "args"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

interpolateDataWithParts :: Phantoms.TTerm Syntax.InterpolateData -> Phantoms.TTerm [Syntax.Lit] -> Phantoms.TTerm Syntax.InterpolateData
interpolateDataWithParts original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.InterpolateData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "prefix"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.InterpolateData"),
              Core.projectionField = (Core.Name "prefix")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "parts"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "args"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.InterpolateData"),
              Core.projectionField = (Core.Name "args")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

interpolateDataWithPrefix :: Phantoms.TTerm Syntax.InterpolateData -> Phantoms.TTerm Syntax.NameData -> Phantoms.TTerm Syntax.InterpolateData
interpolateDataWithPrefix original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.InterpolateData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "prefix"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "parts"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.InterpolateData"),
              Core.projectionField = (Core.Name "parts")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "args"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.InterpolateData"),
              Core.projectionField = (Core.Name "args")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

matchData :: Phantoms.TTerm Syntax.Data -> Phantoms.TTerm [Syntax.Case] -> Phantoms.TTerm Syntax.MatchData
matchData expr cases =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.MatchData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expr"),
          Core.fieldTerm = (Phantoms.unTTerm expr)},
        Core.Field {
          Core.fieldName = (Core.Name "cases"),
          Core.fieldTerm = (Phantoms.unTTerm cases)}]}))

matchDataCases :: Phantoms.TTerm Syntax.MatchData -> Phantoms.TTerm [Syntax.Case]
matchDataCases x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.MatchData"),
        Core.projectionField = (Core.Name "cases")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

matchDataExpr :: Phantoms.TTerm Syntax.MatchData -> Phantoms.TTerm Syntax.Data
matchDataExpr x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.MatchData"),
        Core.projectionField = (Core.Name "expr")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

matchDataWithCases :: Phantoms.TTerm Syntax.MatchData -> Phantoms.TTerm [Syntax.Case] -> Phantoms.TTerm Syntax.MatchData
matchDataWithCases original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.MatchData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expr"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.MatchData"),
              Core.projectionField = (Core.Name "expr")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "cases"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

matchDataWithExpr :: Phantoms.TTerm Syntax.MatchData -> Phantoms.TTerm Syntax.Data -> Phantoms.TTerm Syntax.MatchData
matchDataWithExpr original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.MatchData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expr"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "cases"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.MatchData"),
              Core.projectionField = (Core.Name "cases")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

nameData :: Phantoms.TTerm Syntax.PredefString -> Phantoms.TTerm Syntax.NameData
nameData value =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.NameData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm value)}]}))

nameDataValue :: Phantoms.TTerm Syntax.NameData -> Phantoms.TTerm Syntax.PredefString
nameDataValue x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.NameData"),
        Core.projectionField = (Core.Name "value")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

nameDataWithValue :: Phantoms.TTerm Syntax.NameData -> Phantoms.TTerm Syntax.PredefString -> Phantoms.TTerm Syntax.NameData
nameDataWithValue original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.NameData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

newData :: Phantoms.TTerm Syntax.Init -> Phantoms.TTerm Syntax.NewData
newData init =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.NewData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "init"),
          Core.fieldTerm = (Phantoms.unTTerm init)}]}))

newAnonymousData :: Phantoms.TTerm Syntax.Template -> Phantoms.TTerm Syntax.NewAnonymousData
newAnonymousData templ =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.NewAnonymousData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "templ"),
          Core.fieldTerm = (Phantoms.unTTerm templ)}]}))

newAnonymousDataTempl :: Phantoms.TTerm Syntax.NewAnonymousData -> Phantoms.TTerm Syntax.Template
newAnonymousDataTempl x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.NewAnonymousData"),
        Core.projectionField = (Core.Name "templ")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

newAnonymousDataWithTempl :: Phantoms.TTerm Syntax.NewAnonymousData -> Phantoms.TTerm Syntax.Template -> Phantoms.TTerm Syntax.NewAnonymousData
newAnonymousDataWithTempl original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.NewAnonymousData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "templ"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

newDataInit :: Phantoms.TTerm Syntax.NewData -> Phantoms.TTerm Syntax.Init
newDataInit x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.NewData"),
        Core.projectionField = (Core.Name "init")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

newDataWithInit :: Phantoms.TTerm Syntax.NewData -> Phantoms.TTerm Syntax.Init -> Phantoms.TTerm Syntax.NewData
newDataWithInit original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.NewData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "init"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

paramData :: Phantoms.TTerm [Syntax.Mod] -> Phantoms.TTerm Syntax.Name -> Phantoms.TTerm (Maybe Syntax.Type) -> Phantoms.TTerm (Maybe Syntax.Data) -> Phantoms.TTerm Syntax.ParamData
paramData mods name decltpe default_ =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ParamData"),
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

paramDataDecltpe :: Phantoms.TTerm Syntax.ParamData -> Phantoms.TTerm (Maybe Syntax.Type)
paramDataDecltpe x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ParamData"),
        Core.projectionField = (Core.Name "decltpe")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

paramDataDefault :: Phantoms.TTerm Syntax.ParamData -> Phantoms.TTerm (Maybe Syntax.Data)
paramDataDefault x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ParamData"),
        Core.projectionField = (Core.Name "default")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

paramDataMods :: Phantoms.TTerm Syntax.ParamData -> Phantoms.TTerm [Syntax.Mod]
paramDataMods x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ParamData"),
        Core.projectionField = (Core.Name "mods")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

paramDataName :: Phantoms.TTerm Syntax.ParamData -> Phantoms.TTerm Syntax.Name
paramDataName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ParamData"),
        Core.projectionField = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

paramDataWithDecltpe :: Phantoms.TTerm Syntax.ParamData -> Phantoms.TTerm (Maybe Syntax.Type) -> Phantoms.TTerm Syntax.ParamData
paramDataWithDecltpe original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ParamData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ParamData"),
              Core.projectionField = (Core.Name "mods")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ParamData"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "default"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ParamData"),
              Core.projectionField = (Core.Name "default")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

paramDataWithDefault :: Phantoms.TTerm Syntax.ParamData -> Phantoms.TTerm (Maybe Syntax.Data) -> Phantoms.TTerm Syntax.ParamData
paramDataWithDefault original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ParamData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ParamData"),
              Core.projectionField = (Core.Name "mods")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ParamData"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ParamData"),
              Core.projectionField = (Core.Name "decltpe")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "default"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

paramDataWithMods :: Phantoms.TTerm Syntax.ParamData -> Phantoms.TTerm [Syntax.Mod] -> Phantoms.TTerm Syntax.ParamData
paramDataWithMods original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ParamData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ParamData"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ParamData"),
              Core.projectionField = (Core.Name "decltpe")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "default"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ParamData"),
              Core.projectionField = (Core.Name "default")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

paramDataWithName :: Phantoms.TTerm Syntax.ParamData -> Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.ParamData
paramDataWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ParamData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ParamData"),
              Core.projectionField = (Core.Name "mods")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ParamData"),
              Core.projectionField = (Core.Name "decltpe")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "default"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ParamData"),
              Core.projectionField = (Core.Name "default")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

partialFunctionData :: Phantoms.TTerm [Syntax.Case] -> Phantoms.TTerm Syntax.PartialFunctionData
partialFunctionData cases =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.PartialFunctionData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "cases"),
          Core.fieldTerm = (Phantoms.unTTerm cases)}]}))

partialFunctionDataCases :: Phantoms.TTerm Syntax.PartialFunctionData -> Phantoms.TTerm [Syntax.Case]
partialFunctionDataCases x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.PartialFunctionData"),
        Core.projectionField = (Core.Name "cases")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

partialFunctionDataWithCases :: Phantoms.TTerm Syntax.PartialFunctionData -> Phantoms.TTerm [Syntax.Case] -> Phantoms.TTerm Syntax.PartialFunctionData
partialFunctionDataWithCases original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.PartialFunctionData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "cases"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

polyFunctionData :: Phantoms.TTerm [Syntax.ParamType] -> Phantoms.TTerm Syntax.Data -> Phantoms.TTerm Syntax.PolyFunctionData
polyFunctionData tparams body =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.PolyFunctionData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Phantoms.unTTerm tparams)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm body)}]}))

polyFunctionDataBody :: Phantoms.TTerm Syntax.PolyFunctionData -> Phantoms.TTerm Syntax.Data
polyFunctionDataBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.PolyFunctionData"),
        Core.projectionField = (Core.Name "body")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

polyFunctionDataTparams :: Phantoms.TTerm Syntax.PolyFunctionData -> Phantoms.TTerm [Syntax.ParamType]
polyFunctionDataTparams x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.PolyFunctionData"),
        Core.projectionField = (Core.Name "tparams")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

polyFunctionDataWithBody :: Phantoms.TTerm Syntax.PolyFunctionData -> Phantoms.TTerm Syntax.Data -> Phantoms.TTerm Syntax.PolyFunctionData
polyFunctionDataWithBody original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.PolyFunctionData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.PolyFunctionData"),
              Core.projectionField = (Core.Name "tparams")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

polyFunctionDataWithTparams :: Phantoms.TTerm Syntax.PolyFunctionData -> Phantoms.TTerm [Syntax.ParamType] -> Phantoms.TTerm Syntax.PolyFunctionData
polyFunctionDataWithTparams original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.PolyFunctionData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.PolyFunctionData"),
              Core.projectionField = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

refDataAnonymous :: Phantoms.TTerm Syntax.AnonymousData -> Phantoms.TTerm Syntax.RefData
refDataAnonymous x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.RefData"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "anonymous"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

refDataApplyUnary :: Phantoms.TTerm Syntax.ApplyUnaryData -> Phantoms.TTerm Syntax.RefData
refDataApplyUnary x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.RefData"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "applyUnary"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

refDataName :: Phantoms.TTerm Syntax.NameData -> Phantoms.TTerm Syntax.RefData
refDataName x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.RefData"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "name"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

refDataSelect :: Phantoms.TTerm Syntax.SelectData -> Phantoms.TTerm Syntax.RefData
refDataSelect x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.RefData"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "select"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

refDataSuper :: Phantoms.TTerm Syntax.SuperData -> Phantoms.TTerm Syntax.RefData
refDataSuper x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.RefData"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "super"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

refDataThis :: Phantoms.TTerm Syntax.ThisData -> Phantoms.TTerm Syntax.RefData
refDataThis x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.RefData"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "this"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

repeatedData :: Phantoms.TTerm Syntax.Data -> Phantoms.TTerm Syntax.RepeatedData
repeatedData expr =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.RepeatedData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expr"),
          Core.fieldTerm = (Phantoms.unTTerm expr)}]}))

repeatedDataExpr :: Phantoms.TTerm Syntax.RepeatedData -> Phantoms.TTerm Syntax.Data
repeatedDataExpr x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.RepeatedData"),
        Core.projectionField = (Core.Name "expr")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

repeatedDataWithExpr :: Phantoms.TTerm Syntax.RepeatedData -> Phantoms.TTerm Syntax.Data -> Phantoms.TTerm Syntax.RepeatedData
repeatedDataWithExpr original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.RepeatedData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expr"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

returnData :: Phantoms.TTerm Syntax.Data -> Phantoms.TTerm Syntax.ReturnData
returnData expr =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ReturnData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expr"),
          Core.fieldTerm = (Phantoms.unTTerm expr)}]}))

returnDataExpr :: Phantoms.TTerm Syntax.ReturnData -> Phantoms.TTerm Syntax.Data
returnDataExpr x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ReturnData"),
        Core.projectionField = (Core.Name "expr")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

returnDataWithExpr :: Phantoms.TTerm Syntax.ReturnData -> Phantoms.TTerm Syntax.Data -> Phantoms.TTerm Syntax.ReturnData
returnDataWithExpr original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ReturnData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expr"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

selectData :: Phantoms.TTerm Syntax.Data -> Phantoms.TTerm Syntax.NameData -> Phantoms.TTerm Syntax.SelectData
selectData qual name =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.SelectData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "qual"),
          Core.fieldTerm = (Phantoms.unTTerm qual)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)}]}))

selectDataName :: Phantoms.TTerm Syntax.SelectData -> Phantoms.TTerm Syntax.NameData
selectDataName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.SelectData"),
        Core.projectionField = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

selectDataQual :: Phantoms.TTerm Syntax.SelectData -> Phantoms.TTerm Syntax.Data
selectDataQual x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.SelectData"),
        Core.projectionField = (Core.Name "qual")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

selectDataWithName :: Phantoms.TTerm Syntax.SelectData -> Phantoms.TTerm Syntax.NameData -> Phantoms.TTerm Syntax.SelectData
selectDataWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.SelectData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "qual"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.SelectData"),
              Core.projectionField = (Core.Name "qual")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

selectDataWithQual :: Phantoms.TTerm Syntax.SelectData -> Phantoms.TTerm Syntax.Data -> Phantoms.TTerm Syntax.SelectData
selectDataWithQual original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.SelectData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "qual"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.SelectData"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

superData :: Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.SuperData
superData thisp superp =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.SuperData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "thisp"),
          Core.fieldTerm = (Phantoms.unTTerm thisp)},
        Core.Field {
          Core.fieldName = (Core.Name "superp"),
          Core.fieldTerm = (Phantoms.unTTerm superp)}]}))

superDataSuperp :: Phantoms.TTerm Syntax.SuperData -> Phantoms.TTerm Syntax.Name
superDataSuperp x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.SuperData"),
        Core.projectionField = (Core.Name "superp")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

superDataThisp :: Phantoms.TTerm Syntax.SuperData -> Phantoms.TTerm Syntax.Name
superDataThisp x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.SuperData"),
        Core.projectionField = (Core.Name "thisp")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

superDataWithSuperp :: Phantoms.TTerm Syntax.SuperData -> Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.SuperData
superDataWithSuperp original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.SuperData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "thisp"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.SuperData"),
              Core.projectionField = (Core.Name "thisp")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "superp"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

superDataWithThisp :: Phantoms.TTerm Syntax.SuperData -> Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.SuperData
superDataWithThisp original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.SuperData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "thisp"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "superp"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.SuperData"),
              Core.projectionField = (Core.Name "superp")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

thisData :: Phantoms.TTerm () -> Phantoms.TTerm Syntax.ThisData
thisData x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.scala.syntax.ThisData"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

throwData :: Phantoms.TTerm Syntax.Data -> Phantoms.TTerm Syntax.ThrowData
throwData expr =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ThrowData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expr"),
          Core.fieldTerm = (Phantoms.unTTerm expr)}]}))

throwDataExpr :: Phantoms.TTerm Syntax.ThrowData -> Phantoms.TTerm Syntax.Data
throwDataExpr x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ThrowData"),
        Core.projectionField = (Core.Name "expr")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

throwDataWithExpr :: Phantoms.TTerm Syntax.ThrowData -> Phantoms.TTerm Syntax.Data -> Phantoms.TTerm Syntax.ThrowData
throwDataWithExpr original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ThrowData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expr"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

tryData :: Phantoms.TTerm Syntax.Data -> Phantoms.TTerm [Syntax.Case] -> Phantoms.TTerm (Maybe Syntax.Data) -> Phantoms.TTerm Syntax.TryData
tryData expr catchp finallyp =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.TryData"),
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

tryDataCatchp :: Phantoms.TTerm Syntax.TryData -> Phantoms.TTerm [Syntax.Case]
tryDataCatchp x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TryData"),
        Core.projectionField = (Core.Name "catchp")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

tryDataExpr :: Phantoms.TTerm Syntax.TryData -> Phantoms.TTerm Syntax.Data
tryDataExpr x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TryData"),
        Core.projectionField = (Core.Name "expr")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

tryDataFinallyp :: Phantoms.TTerm Syntax.TryData -> Phantoms.TTerm (Maybe Syntax.Data)
tryDataFinallyp x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TryData"),
        Core.projectionField = (Core.Name "finallyp")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

tryDataWithCatchp :: Phantoms.TTerm Syntax.TryData -> Phantoms.TTerm [Syntax.Case] -> Phantoms.TTerm Syntax.TryData
tryDataWithCatchp original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.TryData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expr"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TryData"),
              Core.projectionField = (Core.Name "expr")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "catchp"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "finallyp"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TryData"),
              Core.projectionField = (Core.Name "finallyp")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

tryDataWithExpr :: Phantoms.TTerm Syntax.TryData -> Phantoms.TTerm Syntax.Data -> Phantoms.TTerm Syntax.TryData
tryDataWithExpr original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.TryData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expr"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "catchp"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TryData"),
              Core.projectionField = (Core.Name "catchp")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "finallyp"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TryData"),
              Core.projectionField = (Core.Name "finallyp")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

tryDataWithFinallyp :: Phantoms.TTerm Syntax.TryData -> Phantoms.TTerm (Maybe Syntax.Data) -> Phantoms.TTerm Syntax.TryData
tryDataWithFinallyp original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.TryData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expr"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TryData"),
              Core.projectionField = (Core.Name "expr")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "catchp"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TryData"),
              Core.projectionField = (Core.Name "catchp")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "finallyp"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

tryWithHandlerData :: Phantoms.TTerm Syntax.Data -> Phantoms.TTerm Syntax.Data -> Phantoms.TTerm (Maybe Syntax.Data) -> Phantoms.TTerm Syntax.TryWithHandlerData
tryWithHandlerData expr catchp finallyp =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.TryWithHandlerData"),
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

tryWithHandlerDataCatchp :: Phantoms.TTerm Syntax.TryWithHandlerData -> Phantoms.TTerm Syntax.Data
tryWithHandlerDataCatchp x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TryWithHandlerData"),
        Core.projectionField = (Core.Name "catchp")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

tryWithHandlerDataExpr :: Phantoms.TTerm Syntax.TryWithHandlerData -> Phantoms.TTerm Syntax.Data
tryWithHandlerDataExpr x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TryWithHandlerData"),
        Core.projectionField = (Core.Name "expr")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

tryWithHandlerDataFinallyp :: Phantoms.TTerm Syntax.TryWithHandlerData -> Phantoms.TTerm (Maybe Syntax.Data)
tryWithHandlerDataFinallyp x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TryWithHandlerData"),
        Core.projectionField = (Core.Name "finallyp")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

tryWithHandlerDataWithCatchp :: Phantoms.TTerm Syntax.TryWithHandlerData -> Phantoms.TTerm Syntax.Data -> Phantoms.TTerm Syntax.TryWithHandlerData
tryWithHandlerDataWithCatchp original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.TryWithHandlerData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expr"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TryWithHandlerData"),
              Core.projectionField = (Core.Name "expr")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "catchp"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "finallyp"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TryWithHandlerData"),
              Core.projectionField = (Core.Name "finallyp")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

tryWithHandlerDataWithExpr :: Phantoms.TTerm Syntax.TryWithHandlerData -> Phantoms.TTerm Syntax.Data -> Phantoms.TTerm Syntax.TryWithHandlerData
tryWithHandlerDataWithExpr original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.TryWithHandlerData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expr"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "catchp"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TryWithHandlerData"),
              Core.projectionField = (Core.Name "catchp")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "finallyp"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TryWithHandlerData"),
              Core.projectionField = (Core.Name "finallyp")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

tryWithHandlerDataWithFinallyp :: Phantoms.TTerm Syntax.TryWithHandlerData -> Phantoms.TTerm (Maybe Syntax.Data) -> Phantoms.TTerm Syntax.TryWithHandlerData
tryWithHandlerDataWithFinallyp original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.TryWithHandlerData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expr"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TryWithHandlerData"),
              Core.projectionField = (Core.Name "expr")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "catchp"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TryWithHandlerData"),
              Core.projectionField = (Core.Name "catchp")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "finallyp"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

tupleData :: Phantoms.TTerm [Syntax.Data] -> Phantoms.TTerm Syntax.TupleData
tupleData args =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.TupleData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "args"),
          Core.fieldTerm = (Phantoms.unTTerm args)}]}))

tupleDataArgs :: Phantoms.TTerm Syntax.TupleData -> Phantoms.TTerm [Syntax.Data]
tupleDataArgs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TupleData"),
        Core.projectionField = (Core.Name "args")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

tupleDataWithArgs :: Phantoms.TTerm Syntax.TupleData -> Phantoms.TTerm [Syntax.Data] -> Phantoms.TTerm Syntax.TupleData
tupleDataWithArgs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.TupleData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "args"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

whileData :: Phantoms.TTerm Syntax.Data -> Phantoms.TTerm Syntax.Data -> Phantoms.TTerm Syntax.WhileData
whileData expr body =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.WhileData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expr"),
          Core.fieldTerm = (Phantoms.unTTerm expr)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm body)}]}))

whileDataBody :: Phantoms.TTerm Syntax.WhileData -> Phantoms.TTerm Syntax.Data
whileDataBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.WhileData"),
        Core.projectionField = (Core.Name "body")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

whileDataExpr :: Phantoms.TTerm Syntax.WhileData -> Phantoms.TTerm Syntax.Data
whileDataExpr x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.WhileData"),
        Core.projectionField = (Core.Name "expr")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

whileDataWithBody :: Phantoms.TTerm Syntax.WhileData -> Phantoms.TTerm Syntax.Data -> Phantoms.TTerm Syntax.WhileData
whileDataWithBody original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.WhileData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expr"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.WhileData"),
              Core.projectionField = (Core.Name "expr")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

whileDataWithExpr :: Phantoms.TTerm Syntax.WhileData -> Phantoms.TTerm Syntax.Data -> Phantoms.TTerm Syntax.WhileData
whileDataWithExpr original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.WhileData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expr"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.WhileData"),
              Core.projectionField = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

declDef :: Phantoms.TTerm Syntax.DefDecl -> Phantoms.TTerm Syntax.Decl
declDef x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Decl"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "def"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

declGiven :: Phantoms.TTerm Syntax.GivenDecl -> Phantoms.TTerm Syntax.Decl
declGiven x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Decl"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "given"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

declType :: Phantoms.TTerm Syntax.TypeDecl -> Phantoms.TTerm Syntax.Decl
declType x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Decl"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "type"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

declVal :: Phantoms.TTerm Syntax.ValDecl -> Phantoms.TTerm Syntax.Decl
declVal x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Decl"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "val"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

declVar :: Phantoms.TTerm Syntax.VarDecl -> Phantoms.TTerm Syntax.Decl
declVar x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Decl"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "var"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

defDecl :: Phantoms.TTerm [Syntax.Mod] -> Phantoms.TTerm Syntax.NameData -> Phantoms.TTerm [Syntax.ParamType] -> Phantoms.TTerm [[Syntax.ParamData]] -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.DefDecl
defDecl mods name tparams paramss decltpe =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.DefDecl"),
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

defDeclDecltpe :: Phantoms.TTerm Syntax.DefDecl -> Phantoms.TTerm Syntax.Type
defDeclDecltpe x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DefDecl"),
        Core.projectionField = (Core.Name "decltpe")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

defDeclMods :: Phantoms.TTerm Syntax.DefDecl -> Phantoms.TTerm [Syntax.Mod]
defDeclMods x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DefDecl"),
        Core.projectionField = (Core.Name "mods")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

defDeclName :: Phantoms.TTerm Syntax.DefDecl -> Phantoms.TTerm Syntax.NameData
defDeclName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DefDecl"),
        Core.projectionField = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

defDeclParamss :: Phantoms.TTerm Syntax.DefDecl -> Phantoms.TTerm [[Syntax.ParamData]]
defDeclParamss x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DefDecl"),
        Core.projectionField = (Core.Name "paramss")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

defDeclTparams :: Phantoms.TTerm Syntax.DefDecl -> Phantoms.TTerm [Syntax.ParamType]
defDeclTparams x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DefDecl"),
        Core.projectionField = (Core.Name "tparams")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

defDeclWithDecltpe :: Phantoms.TTerm Syntax.DefDecl -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.DefDecl
defDeclWithDecltpe original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.DefDecl"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DefDecl"),
              Core.projectionField = (Core.Name "mods")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DefDecl"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DefDecl"),
              Core.projectionField = (Core.Name "tparams")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "paramss"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DefDecl"),
              Core.projectionField = (Core.Name "paramss")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

defDeclWithMods :: Phantoms.TTerm Syntax.DefDecl -> Phantoms.TTerm [Syntax.Mod] -> Phantoms.TTerm Syntax.DefDecl
defDeclWithMods original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.DefDecl"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DefDecl"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DefDecl"),
              Core.projectionField = (Core.Name "tparams")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "paramss"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DefDecl"),
              Core.projectionField = (Core.Name "paramss")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DefDecl"),
              Core.projectionField = (Core.Name "decltpe")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

defDeclWithName :: Phantoms.TTerm Syntax.DefDecl -> Phantoms.TTerm Syntax.NameData -> Phantoms.TTerm Syntax.DefDecl
defDeclWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.DefDecl"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DefDecl"),
              Core.projectionField = (Core.Name "mods")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DefDecl"),
              Core.projectionField = (Core.Name "tparams")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "paramss"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DefDecl"),
              Core.projectionField = (Core.Name "paramss")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DefDecl"),
              Core.projectionField = (Core.Name "decltpe")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

defDeclWithParamss :: Phantoms.TTerm Syntax.DefDecl -> Phantoms.TTerm [[Syntax.ParamData]] -> Phantoms.TTerm Syntax.DefDecl
defDeclWithParamss original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.DefDecl"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DefDecl"),
              Core.projectionField = (Core.Name "mods")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DefDecl"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DefDecl"),
              Core.projectionField = (Core.Name "tparams")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "paramss"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DefDecl"),
              Core.projectionField = (Core.Name "decltpe")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

defDeclWithTparams :: Phantoms.TTerm Syntax.DefDecl -> Phantoms.TTerm [Syntax.ParamType] -> Phantoms.TTerm Syntax.DefDecl
defDeclWithTparams original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.DefDecl"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DefDecl"),
              Core.projectionField = (Core.Name "mods")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DefDecl"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "paramss"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DefDecl"),
              Core.projectionField = (Core.Name "paramss")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DefDecl"),
              Core.projectionField = (Core.Name "decltpe")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

givenDecl :: Phantoms.TTerm [Syntax.Mod] -> Phantoms.TTerm Syntax.NameData -> Phantoms.TTerm [Syntax.ParamType] -> Phantoms.TTerm [[Syntax.ParamData]] -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.GivenDecl
givenDecl mods name tparams sparams decltpe =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.GivenDecl"),
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

givenDeclDecltpe :: Phantoms.TTerm Syntax.GivenDecl -> Phantoms.TTerm Syntax.Type
givenDeclDecltpe x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenDecl"),
        Core.projectionField = (Core.Name "decltpe")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

givenDeclMods :: Phantoms.TTerm Syntax.GivenDecl -> Phantoms.TTerm [Syntax.Mod]
givenDeclMods x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenDecl"),
        Core.projectionField = (Core.Name "mods")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

givenDeclName :: Phantoms.TTerm Syntax.GivenDecl -> Phantoms.TTerm Syntax.NameData
givenDeclName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenDecl"),
        Core.projectionField = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

givenDeclSparams :: Phantoms.TTerm Syntax.GivenDecl -> Phantoms.TTerm [[Syntax.ParamData]]
givenDeclSparams x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenDecl"),
        Core.projectionField = (Core.Name "sparams")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

givenDeclTparams :: Phantoms.TTerm Syntax.GivenDecl -> Phantoms.TTerm [Syntax.ParamType]
givenDeclTparams x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenDecl"),
        Core.projectionField = (Core.Name "tparams")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

givenDeclWithDecltpe :: Phantoms.TTerm Syntax.GivenDecl -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.GivenDecl
givenDeclWithDecltpe original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.GivenDecl"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenDecl"),
              Core.projectionField = (Core.Name "mods")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenDecl"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenDecl"),
              Core.projectionField = (Core.Name "tparams")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "sparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenDecl"),
              Core.projectionField = (Core.Name "sparams")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

givenDeclWithMods :: Phantoms.TTerm Syntax.GivenDecl -> Phantoms.TTerm [Syntax.Mod] -> Phantoms.TTerm Syntax.GivenDecl
givenDeclWithMods original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.GivenDecl"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenDecl"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenDecl"),
              Core.projectionField = (Core.Name "tparams")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "sparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenDecl"),
              Core.projectionField = (Core.Name "sparams")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenDecl"),
              Core.projectionField = (Core.Name "decltpe")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

givenDeclWithName :: Phantoms.TTerm Syntax.GivenDecl -> Phantoms.TTerm Syntax.NameData -> Phantoms.TTerm Syntax.GivenDecl
givenDeclWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.GivenDecl"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenDecl"),
              Core.projectionField = (Core.Name "mods")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenDecl"),
              Core.projectionField = (Core.Name "tparams")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "sparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenDecl"),
              Core.projectionField = (Core.Name "sparams")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenDecl"),
              Core.projectionField = (Core.Name "decltpe")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

givenDeclWithSparams :: Phantoms.TTerm Syntax.GivenDecl -> Phantoms.TTerm [[Syntax.ParamData]] -> Phantoms.TTerm Syntax.GivenDecl
givenDeclWithSparams original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.GivenDecl"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenDecl"),
              Core.projectionField = (Core.Name "mods")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenDecl"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenDecl"),
              Core.projectionField = (Core.Name "tparams")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "sparams"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenDecl"),
              Core.projectionField = (Core.Name "decltpe")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

givenDeclWithTparams :: Phantoms.TTerm Syntax.GivenDecl -> Phantoms.TTerm [Syntax.ParamType] -> Phantoms.TTerm Syntax.GivenDecl
givenDeclWithTparams original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.GivenDecl"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenDecl"),
              Core.projectionField = (Core.Name "mods")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenDecl"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "sparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenDecl"),
              Core.projectionField = (Core.Name "sparams")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenDecl"),
              Core.projectionField = (Core.Name "decltpe")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

typeDecl :: Phantoms.TTerm [Syntax.Mod] -> Phantoms.TTerm Syntax.NameType -> Phantoms.TTerm [Syntax.ParamType] -> Phantoms.TTerm Syntax.TypeBounds -> Phantoms.TTerm Syntax.TypeDecl
typeDecl mods name tparams bounds =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.TypeDecl"),
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

typeDeclBounds :: Phantoms.TTerm Syntax.TypeDecl -> Phantoms.TTerm Syntax.TypeBounds
typeDeclBounds x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TypeDecl"),
        Core.projectionField = (Core.Name "bounds")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

typeDeclMods :: Phantoms.TTerm Syntax.TypeDecl -> Phantoms.TTerm [Syntax.Mod]
typeDeclMods x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TypeDecl"),
        Core.projectionField = (Core.Name "mods")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

typeDeclName :: Phantoms.TTerm Syntax.TypeDecl -> Phantoms.TTerm Syntax.NameType
typeDeclName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TypeDecl"),
        Core.projectionField = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

typeDeclTparams :: Phantoms.TTerm Syntax.TypeDecl -> Phantoms.TTerm [Syntax.ParamType]
typeDeclTparams x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TypeDecl"),
        Core.projectionField = (Core.Name "tparams")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

typeDeclWithBounds :: Phantoms.TTerm Syntax.TypeDecl -> Phantoms.TTerm Syntax.TypeBounds -> Phantoms.TTerm Syntax.TypeDecl
typeDeclWithBounds original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.TypeDecl"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TypeDecl"),
              Core.projectionField = (Core.Name "mods")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TypeDecl"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TypeDecl"),
              Core.projectionField = (Core.Name "tparams")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "bounds"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

typeDeclWithMods :: Phantoms.TTerm Syntax.TypeDecl -> Phantoms.TTerm [Syntax.Mod] -> Phantoms.TTerm Syntax.TypeDecl
typeDeclWithMods original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.TypeDecl"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TypeDecl"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TypeDecl"),
              Core.projectionField = (Core.Name "tparams")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "bounds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TypeDecl"),
              Core.projectionField = (Core.Name "bounds")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

typeDeclWithName :: Phantoms.TTerm Syntax.TypeDecl -> Phantoms.TTerm Syntax.NameType -> Phantoms.TTerm Syntax.TypeDecl
typeDeclWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.TypeDecl"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TypeDecl"),
              Core.projectionField = (Core.Name "mods")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TypeDecl"),
              Core.projectionField = (Core.Name "tparams")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "bounds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TypeDecl"),
              Core.projectionField = (Core.Name "bounds")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

typeDeclWithTparams :: Phantoms.TTerm Syntax.TypeDecl -> Phantoms.TTerm [Syntax.ParamType] -> Phantoms.TTerm Syntax.TypeDecl
typeDeclWithTparams original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.TypeDecl"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TypeDecl"),
              Core.projectionField = (Core.Name "mods")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TypeDecl"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "bounds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TypeDecl"),
              Core.projectionField = (Core.Name "bounds")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

valDecl :: Phantoms.TTerm [Syntax.Mod] -> Phantoms.TTerm [Syntax.Pat] -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.ValDecl
valDecl mods pats decltpe =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ValDecl"),
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

valDeclDecltpe :: Phantoms.TTerm Syntax.ValDecl -> Phantoms.TTerm Syntax.Type
valDeclDecltpe x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ValDecl"),
        Core.projectionField = (Core.Name "decltpe")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

valDeclMods :: Phantoms.TTerm Syntax.ValDecl -> Phantoms.TTerm [Syntax.Mod]
valDeclMods x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ValDecl"),
        Core.projectionField = (Core.Name "mods")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

valDeclPats :: Phantoms.TTerm Syntax.ValDecl -> Phantoms.TTerm [Syntax.Pat]
valDeclPats x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ValDecl"),
        Core.projectionField = (Core.Name "pats")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

valDeclWithDecltpe :: Phantoms.TTerm Syntax.ValDecl -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.ValDecl
valDeclWithDecltpe original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ValDecl"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ValDecl"),
              Core.projectionField = (Core.Name "mods")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "pats"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ValDecl"),
              Core.projectionField = (Core.Name "pats")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

valDeclWithMods :: Phantoms.TTerm Syntax.ValDecl -> Phantoms.TTerm [Syntax.Mod] -> Phantoms.TTerm Syntax.ValDecl
valDeclWithMods original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ValDecl"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "pats"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ValDecl"),
              Core.projectionField = (Core.Name "pats")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ValDecl"),
              Core.projectionField = (Core.Name "decltpe")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

valDeclWithPats :: Phantoms.TTerm Syntax.ValDecl -> Phantoms.TTerm [Syntax.Pat] -> Phantoms.TTerm Syntax.ValDecl
valDeclWithPats original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ValDecl"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ValDecl"),
              Core.projectionField = (Core.Name "mods")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "pats"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ValDecl"),
              Core.projectionField = (Core.Name "decltpe")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

varDecl :: Phantoms.TTerm [Syntax.Mod] -> Phantoms.TTerm [Syntax.Pat] -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.VarDecl
varDecl mods pats decltpe =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.VarDecl"),
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

varDeclDecltpe :: Phantoms.TTerm Syntax.VarDecl -> Phantoms.TTerm Syntax.Type
varDeclDecltpe x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.VarDecl"),
        Core.projectionField = (Core.Name "decltpe")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

varDeclMods :: Phantoms.TTerm Syntax.VarDecl -> Phantoms.TTerm [Syntax.Mod]
varDeclMods x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.VarDecl"),
        Core.projectionField = (Core.Name "mods")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

varDeclPats :: Phantoms.TTerm Syntax.VarDecl -> Phantoms.TTerm [Syntax.Pat]
varDeclPats x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.VarDecl"),
        Core.projectionField = (Core.Name "pats")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

varDeclWithDecltpe :: Phantoms.TTerm Syntax.VarDecl -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.VarDecl
varDeclWithDecltpe original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.VarDecl"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.VarDecl"),
              Core.projectionField = (Core.Name "mods")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "pats"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.VarDecl"),
              Core.projectionField = (Core.Name "pats")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

varDeclWithMods :: Phantoms.TTerm Syntax.VarDecl -> Phantoms.TTerm [Syntax.Mod] -> Phantoms.TTerm Syntax.VarDecl
varDeclWithMods original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.VarDecl"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "pats"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.VarDecl"),
              Core.projectionField = (Core.Name "pats")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.VarDecl"),
              Core.projectionField = (Core.Name "decltpe")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

varDeclWithPats :: Phantoms.TTerm Syntax.VarDecl -> Phantoms.TTerm [Syntax.Pat] -> Phantoms.TTerm Syntax.VarDecl
varDeclWithPats original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.VarDecl"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.VarDecl"),
              Core.projectionField = (Core.Name "mods")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "pats"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.VarDecl"),
              Core.projectionField = (Core.Name "decltpe")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

defnClass :: Phantoms.TTerm Syntax.ClassDefn -> Phantoms.TTerm Syntax.Defn
defnClass x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Defn"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "class"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

defnDef :: Phantoms.TTerm Syntax.DefDefn -> Phantoms.TTerm Syntax.Defn
defnDef x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Defn"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "def"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

defnEnum :: Phantoms.TTerm Syntax.EnumDefn -> Phantoms.TTerm Syntax.Defn
defnEnum x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Defn"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "enum"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

defnEnumCase :: Phantoms.TTerm Syntax.EnumCaseDefn -> Phantoms.TTerm Syntax.Defn
defnEnumCase x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Defn"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "enumCase"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

defnExtensionGroup :: Phantoms.TTerm Syntax.ExtensionGroupDefn -> Phantoms.TTerm Syntax.Defn
defnExtensionGroup x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Defn"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "extensionGroup"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

defnGiven :: Phantoms.TTerm Syntax.GivenDefn -> Phantoms.TTerm Syntax.Defn
defnGiven x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Defn"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "given"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

defnGivenAlias :: Phantoms.TTerm Syntax.GivenAliasDefn -> Phantoms.TTerm Syntax.Defn
defnGivenAlias x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Defn"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "givenAlias"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

defnObject :: Phantoms.TTerm Syntax.ObjectDefn -> Phantoms.TTerm Syntax.Defn
defnObject x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Defn"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "object"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

defnRepeatedEnumCase :: Phantoms.TTerm Syntax.RepeatedEnumCaseDefn -> Phantoms.TTerm Syntax.Defn
defnRepeatedEnumCase x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Defn"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "repeatedEnumCase"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

defnTrait :: Phantoms.TTerm Syntax.TraitDefn -> Phantoms.TTerm Syntax.Defn
defnTrait x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Defn"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "trait"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

defnType :: Phantoms.TTerm Syntax.TypeDefn -> Phantoms.TTerm Syntax.Defn
defnType x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Defn"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "type"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

defnVal :: Phantoms.TTerm Syntax.ValDefn -> Phantoms.TTerm Syntax.Defn
defnVal x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Defn"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "val"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

defnVar :: Phantoms.TTerm Syntax.VarDefn -> Phantoms.TTerm Syntax.Defn
defnVar x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Defn"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "var"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

classDefn :: Phantoms.TTerm [Syntax.Mod] -> Phantoms.TTerm Syntax.NameType -> Phantoms.TTerm [Syntax.ParamType] -> Phantoms.TTerm Syntax.PrimaryCtor -> Phantoms.TTerm Syntax.Template -> Phantoms.TTerm Syntax.ClassDefn
classDefn mods name tparams ctor template =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ClassDefn"),
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

classDefnCtor :: Phantoms.TTerm Syntax.ClassDefn -> Phantoms.TTerm Syntax.PrimaryCtor
classDefnCtor x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ClassDefn"),
        Core.projectionField = (Core.Name "ctor")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

classDefnMods :: Phantoms.TTerm Syntax.ClassDefn -> Phantoms.TTerm [Syntax.Mod]
classDefnMods x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ClassDefn"),
        Core.projectionField = (Core.Name "mods")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

classDefnName :: Phantoms.TTerm Syntax.ClassDefn -> Phantoms.TTerm Syntax.NameType
classDefnName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ClassDefn"),
        Core.projectionField = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

classDefnTemplate :: Phantoms.TTerm Syntax.ClassDefn -> Phantoms.TTerm Syntax.Template
classDefnTemplate x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ClassDefn"),
        Core.projectionField = (Core.Name "template")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

classDefnTparams :: Phantoms.TTerm Syntax.ClassDefn -> Phantoms.TTerm [Syntax.ParamType]
classDefnTparams x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ClassDefn"),
        Core.projectionField = (Core.Name "tparams")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

classDefnWithCtor :: Phantoms.TTerm Syntax.ClassDefn -> Phantoms.TTerm Syntax.PrimaryCtor -> Phantoms.TTerm Syntax.ClassDefn
classDefnWithCtor original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ClassDefn"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ClassDefn"),
              Core.projectionField = (Core.Name "mods")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ClassDefn"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ClassDefn"),
              Core.projectionField = (Core.Name "tparams")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ctor"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "template"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ClassDefn"),
              Core.projectionField = (Core.Name "template")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

classDefnWithMods :: Phantoms.TTerm Syntax.ClassDefn -> Phantoms.TTerm [Syntax.Mod] -> Phantoms.TTerm Syntax.ClassDefn
classDefnWithMods original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ClassDefn"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ClassDefn"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ClassDefn"),
              Core.projectionField = (Core.Name "tparams")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ctor"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ClassDefn"),
              Core.projectionField = (Core.Name "ctor")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "template"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ClassDefn"),
              Core.projectionField = (Core.Name "template")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

classDefnWithName :: Phantoms.TTerm Syntax.ClassDefn -> Phantoms.TTerm Syntax.NameType -> Phantoms.TTerm Syntax.ClassDefn
classDefnWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ClassDefn"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ClassDefn"),
              Core.projectionField = (Core.Name "mods")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ClassDefn"),
              Core.projectionField = (Core.Name "tparams")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ctor"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ClassDefn"),
              Core.projectionField = (Core.Name "ctor")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "template"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ClassDefn"),
              Core.projectionField = (Core.Name "template")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

classDefnWithTemplate :: Phantoms.TTerm Syntax.ClassDefn -> Phantoms.TTerm Syntax.Template -> Phantoms.TTerm Syntax.ClassDefn
classDefnWithTemplate original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ClassDefn"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ClassDefn"),
              Core.projectionField = (Core.Name "mods")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ClassDefn"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ClassDefn"),
              Core.projectionField = (Core.Name "tparams")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ctor"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ClassDefn"),
              Core.projectionField = (Core.Name "ctor")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "template"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

classDefnWithTparams :: Phantoms.TTerm Syntax.ClassDefn -> Phantoms.TTerm [Syntax.ParamType] -> Phantoms.TTerm Syntax.ClassDefn
classDefnWithTparams original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ClassDefn"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ClassDefn"),
              Core.projectionField = (Core.Name "mods")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ClassDefn"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "ctor"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ClassDefn"),
              Core.projectionField = (Core.Name "ctor")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "template"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ClassDefn"),
              Core.projectionField = (Core.Name "template")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

defDefn :: Phantoms.TTerm [Syntax.Mod] -> Phantoms.TTerm Syntax.NameData -> Phantoms.TTerm [Syntax.ParamType] -> Phantoms.TTerm [[Syntax.ParamData]] -> Phantoms.TTerm (Maybe Syntax.Type) -> Phantoms.TTerm Syntax.Data -> Phantoms.TTerm Syntax.DefDefn
defDefn mods name tparams paramss decltpe body =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.DefDefn"),
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

defDefnBody :: Phantoms.TTerm Syntax.DefDefn -> Phantoms.TTerm Syntax.Data
defDefnBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DefDefn"),
        Core.projectionField = (Core.Name "body")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

defDefnDecltpe :: Phantoms.TTerm Syntax.DefDefn -> Phantoms.TTerm (Maybe Syntax.Type)
defDefnDecltpe x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DefDefn"),
        Core.projectionField = (Core.Name "decltpe")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

defDefnMods :: Phantoms.TTerm Syntax.DefDefn -> Phantoms.TTerm [Syntax.Mod]
defDefnMods x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DefDefn"),
        Core.projectionField = (Core.Name "mods")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

defDefnName :: Phantoms.TTerm Syntax.DefDefn -> Phantoms.TTerm Syntax.NameData
defDefnName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DefDefn"),
        Core.projectionField = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

defDefnParamss :: Phantoms.TTerm Syntax.DefDefn -> Phantoms.TTerm [[Syntax.ParamData]]
defDefnParamss x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DefDefn"),
        Core.projectionField = (Core.Name "paramss")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

defDefnTparams :: Phantoms.TTerm Syntax.DefDefn -> Phantoms.TTerm [Syntax.ParamType]
defDefnTparams x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DefDefn"),
        Core.projectionField = (Core.Name "tparams")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

defDefnWithBody :: Phantoms.TTerm Syntax.DefDefn -> Phantoms.TTerm Syntax.Data -> Phantoms.TTerm Syntax.DefDefn
defDefnWithBody original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.DefDefn"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DefDefn"),
              Core.projectionField = (Core.Name "mods")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DefDefn"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DefDefn"),
              Core.projectionField = (Core.Name "tparams")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "paramss"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DefDefn"),
              Core.projectionField = (Core.Name "paramss")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DefDefn"),
              Core.projectionField = (Core.Name "decltpe")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

defDefnWithDecltpe :: Phantoms.TTerm Syntax.DefDefn -> Phantoms.TTerm (Maybe Syntax.Type) -> Phantoms.TTerm Syntax.DefDefn
defDefnWithDecltpe original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.DefDefn"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DefDefn"),
              Core.projectionField = (Core.Name "mods")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DefDefn"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DefDefn"),
              Core.projectionField = (Core.Name "tparams")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "paramss"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DefDefn"),
              Core.projectionField = (Core.Name "paramss")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DefDefn"),
              Core.projectionField = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

defDefnWithMods :: Phantoms.TTerm Syntax.DefDefn -> Phantoms.TTerm [Syntax.Mod] -> Phantoms.TTerm Syntax.DefDefn
defDefnWithMods original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.DefDefn"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DefDefn"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DefDefn"),
              Core.projectionField = (Core.Name "tparams")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "paramss"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DefDefn"),
              Core.projectionField = (Core.Name "paramss")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DefDefn"),
              Core.projectionField = (Core.Name "decltpe")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DefDefn"),
              Core.projectionField = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

defDefnWithName :: Phantoms.TTerm Syntax.DefDefn -> Phantoms.TTerm Syntax.NameData -> Phantoms.TTerm Syntax.DefDefn
defDefnWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.DefDefn"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DefDefn"),
              Core.projectionField = (Core.Name "mods")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DefDefn"),
              Core.projectionField = (Core.Name "tparams")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "paramss"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DefDefn"),
              Core.projectionField = (Core.Name "paramss")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DefDefn"),
              Core.projectionField = (Core.Name "decltpe")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DefDefn"),
              Core.projectionField = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

defDefnWithParamss :: Phantoms.TTerm Syntax.DefDefn -> Phantoms.TTerm [[Syntax.ParamData]] -> Phantoms.TTerm Syntax.DefDefn
defDefnWithParamss original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.DefDefn"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DefDefn"),
              Core.projectionField = (Core.Name "mods")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DefDefn"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DefDefn"),
              Core.projectionField = (Core.Name "tparams")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "paramss"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DefDefn"),
              Core.projectionField = (Core.Name "decltpe")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DefDefn"),
              Core.projectionField = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

defDefnWithTparams :: Phantoms.TTerm Syntax.DefDefn -> Phantoms.TTerm [Syntax.ParamType] -> Phantoms.TTerm Syntax.DefDefn
defDefnWithTparams original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.DefDefn"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DefDefn"),
              Core.projectionField = (Core.Name "mods")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DefDefn"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "paramss"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DefDefn"),
              Core.projectionField = (Core.Name "paramss")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DefDefn"),
              Core.projectionField = (Core.Name "decltpe")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DefDefn"),
              Core.projectionField = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

enumDefn :: Phantoms.TTerm [Syntax.Mod] -> Phantoms.TTerm Syntax.NameType -> Phantoms.TTerm [Syntax.ParamType] -> Phantoms.TTerm Syntax.PrimaryCtor -> Phantoms.TTerm Syntax.Template -> Phantoms.TTerm Syntax.EnumDefn
enumDefn mods name tparams ctor template =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.EnumDefn"),
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

enumCaseDefn :: Phantoms.TTerm [Syntax.Mod] -> Phantoms.TTerm Syntax.NameData -> Phantoms.TTerm [Syntax.ParamType] -> Phantoms.TTerm Syntax.PrimaryCtor -> Phantoms.TTerm [Syntax.Init] -> Phantoms.TTerm Syntax.EnumCaseDefn
enumCaseDefn mods name tparams ctor inits =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.EnumCaseDefn"),
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

enumCaseDefnCtor :: Phantoms.TTerm Syntax.EnumCaseDefn -> Phantoms.TTerm Syntax.PrimaryCtor
enumCaseDefnCtor x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.EnumCaseDefn"),
        Core.projectionField = (Core.Name "ctor")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

enumCaseDefnInits :: Phantoms.TTerm Syntax.EnumCaseDefn -> Phantoms.TTerm [Syntax.Init]
enumCaseDefnInits x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.EnumCaseDefn"),
        Core.projectionField = (Core.Name "inits")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

enumCaseDefnMods :: Phantoms.TTerm Syntax.EnumCaseDefn -> Phantoms.TTerm [Syntax.Mod]
enumCaseDefnMods x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.EnumCaseDefn"),
        Core.projectionField = (Core.Name "mods")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

enumCaseDefnName :: Phantoms.TTerm Syntax.EnumCaseDefn -> Phantoms.TTerm Syntax.NameData
enumCaseDefnName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.EnumCaseDefn"),
        Core.projectionField = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

enumCaseDefnTparams :: Phantoms.TTerm Syntax.EnumCaseDefn -> Phantoms.TTerm [Syntax.ParamType]
enumCaseDefnTparams x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.EnumCaseDefn"),
        Core.projectionField = (Core.Name "tparams")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

enumCaseDefnWithCtor :: Phantoms.TTerm Syntax.EnumCaseDefn -> Phantoms.TTerm Syntax.PrimaryCtor -> Phantoms.TTerm Syntax.EnumCaseDefn
enumCaseDefnWithCtor original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.EnumCaseDefn"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.EnumCaseDefn"),
              Core.projectionField = (Core.Name "mods")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.EnumCaseDefn"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.EnumCaseDefn"),
              Core.projectionField = (Core.Name "tparams")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ctor"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "inits"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.EnumCaseDefn"),
              Core.projectionField = (Core.Name "inits")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

enumCaseDefnWithInits :: Phantoms.TTerm Syntax.EnumCaseDefn -> Phantoms.TTerm [Syntax.Init] -> Phantoms.TTerm Syntax.EnumCaseDefn
enumCaseDefnWithInits original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.EnumCaseDefn"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.EnumCaseDefn"),
              Core.projectionField = (Core.Name "mods")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.EnumCaseDefn"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.EnumCaseDefn"),
              Core.projectionField = (Core.Name "tparams")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ctor"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.EnumCaseDefn"),
              Core.projectionField = (Core.Name "ctor")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inits"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

enumCaseDefnWithMods :: Phantoms.TTerm Syntax.EnumCaseDefn -> Phantoms.TTerm [Syntax.Mod] -> Phantoms.TTerm Syntax.EnumCaseDefn
enumCaseDefnWithMods original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.EnumCaseDefn"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.EnumCaseDefn"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.EnumCaseDefn"),
              Core.projectionField = (Core.Name "tparams")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ctor"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.EnumCaseDefn"),
              Core.projectionField = (Core.Name "ctor")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inits"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.EnumCaseDefn"),
              Core.projectionField = (Core.Name "inits")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

enumCaseDefnWithName :: Phantoms.TTerm Syntax.EnumCaseDefn -> Phantoms.TTerm Syntax.NameData -> Phantoms.TTerm Syntax.EnumCaseDefn
enumCaseDefnWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.EnumCaseDefn"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.EnumCaseDefn"),
              Core.projectionField = (Core.Name "mods")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.EnumCaseDefn"),
              Core.projectionField = (Core.Name "tparams")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ctor"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.EnumCaseDefn"),
              Core.projectionField = (Core.Name "ctor")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inits"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.EnumCaseDefn"),
              Core.projectionField = (Core.Name "inits")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

enumCaseDefnWithTparams :: Phantoms.TTerm Syntax.EnumCaseDefn -> Phantoms.TTerm [Syntax.ParamType] -> Phantoms.TTerm Syntax.EnumCaseDefn
enumCaseDefnWithTparams original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.EnumCaseDefn"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.EnumCaseDefn"),
              Core.projectionField = (Core.Name "mods")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.EnumCaseDefn"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "ctor"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.EnumCaseDefn"),
              Core.projectionField = (Core.Name "ctor")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inits"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.EnumCaseDefn"),
              Core.projectionField = (Core.Name "inits")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

enumDefnCtor :: Phantoms.TTerm Syntax.EnumDefn -> Phantoms.TTerm Syntax.PrimaryCtor
enumDefnCtor x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.EnumDefn"),
        Core.projectionField = (Core.Name "ctor")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

enumDefnMods :: Phantoms.TTerm Syntax.EnumDefn -> Phantoms.TTerm [Syntax.Mod]
enumDefnMods x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.EnumDefn"),
        Core.projectionField = (Core.Name "mods")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

enumDefnName :: Phantoms.TTerm Syntax.EnumDefn -> Phantoms.TTerm Syntax.NameType
enumDefnName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.EnumDefn"),
        Core.projectionField = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

enumDefnTemplate :: Phantoms.TTerm Syntax.EnumDefn -> Phantoms.TTerm Syntax.Template
enumDefnTemplate x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.EnumDefn"),
        Core.projectionField = (Core.Name "template")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

enumDefnTparams :: Phantoms.TTerm Syntax.EnumDefn -> Phantoms.TTerm [Syntax.ParamType]
enumDefnTparams x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.EnumDefn"),
        Core.projectionField = (Core.Name "tparams")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

enumDefnWithCtor :: Phantoms.TTerm Syntax.EnumDefn -> Phantoms.TTerm Syntax.PrimaryCtor -> Phantoms.TTerm Syntax.EnumDefn
enumDefnWithCtor original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.EnumDefn"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.EnumDefn"),
              Core.projectionField = (Core.Name "mods")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.EnumDefn"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.EnumDefn"),
              Core.projectionField = (Core.Name "tparams")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ctor"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "template"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.EnumDefn"),
              Core.projectionField = (Core.Name "template")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

enumDefnWithMods :: Phantoms.TTerm Syntax.EnumDefn -> Phantoms.TTerm [Syntax.Mod] -> Phantoms.TTerm Syntax.EnumDefn
enumDefnWithMods original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.EnumDefn"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.EnumDefn"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.EnumDefn"),
              Core.projectionField = (Core.Name "tparams")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ctor"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.EnumDefn"),
              Core.projectionField = (Core.Name "ctor")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "template"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.EnumDefn"),
              Core.projectionField = (Core.Name "template")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

enumDefnWithName :: Phantoms.TTerm Syntax.EnumDefn -> Phantoms.TTerm Syntax.NameType -> Phantoms.TTerm Syntax.EnumDefn
enumDefnWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.EnumDefn"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.EnumDefn"),
              Core.projectionField = (Core.Name "mods")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.EnumDefn"),
              Core.projectionField = (Core.Name "tparams")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ctor"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.EnumDefn"),
              Core.projectionField = (Core.Name "ctor")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "template"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.EnumDefn"),
              Core.projectionField = (Core.Name "template")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

enumDefnWithTemplate :: Phantoms.TTerm Syntax.EnumDefn -> Phantoms.TTerm Syntax.Template -> Phantoms.TTerm Syntax.EnumDefn
enumDefnWithTemplate original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.EnumDefn"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.EnumDefn"),
              Core.projectionField = (Core.Name "mods")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.EnumDefn"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.EnumDefn"),
              Core.projectionField = (Core.Name "tparams")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ctor"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.EnumDefn"),
              Core.projectionField = (Core.Name "ctor")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "template"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

enumDefnWithTparams :: Phantoms.TTerm Syntax.EnumDefn -> Phantoms.TTerm [Syntax.ParamType] -> Phantoms.TTerm Syntax.EnumDefn
enumDefnWithTparams original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.EnumDefn"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.EnumDefn"),
              Core.projectionField = (Core.Name "mods")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.EnumDefn"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "ctor"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.EnumDefn"),
              Core.projectionField = (Core.Name "ctor")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "template"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.EnumDefn"),
              Core.projectionField = (Core.Name "template")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

extensionGroupDefn :: Phantoms.TTerm [Syntax.ParamType] -> Phantoms.TTerm [[Syntax.ParamData]] -> Phantoms.TTerm Syntax.Stat -> Phantoms.TTerm Syntax.ExtensionGroupDefn
extensionGroupDefn tparams parmss body =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ExtensionGroupDefn"),
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

extensionGroupDefnBody :: Phantoms.TTerm Syntax.ExtensionGroupDefn -> Phantoms.TTerm Syntax.Stat
extensionGroupDefnBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ExtensionGroupDefn"),
        Core.projectionField = (Core.Name "body")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

extensionGroupDefnParmss :: Phantoms.TTerm Syntax.ExtensionGroupDefn -> Phantoms.TTerm [[Syntax.ParamData]]
extensionGroupDefnParmss x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ExtensionGroupDefn"),
        Core.projectionField = (Core.Name "parmss")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

extensionGroupDefnTparams :: Phantoms.TTerm Syntax.ExtensionGroupDefn -> Phantoms.TTerm [Syntax.ParamType]
extensionGroupDefnTparams x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ExtensionGroupDefn"),
        Core.projectionField = (Core.Name "tparams")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

extensionGroupDefnWithBody :: Phantoms.TTerm Syntax.ExtensionGroupDefn -> Phantoms.TTerm Syntax.Stat -> Phantoms.TTerm Syntax.ExtensionGroupDefn
extensionGroupDefnWithBody original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ExtensionGroupDefn"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ExtensionGroupDefn"),
              Core.projectionField = (Core.Name "tparams")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "parmss"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ExtensionGroupDefn"),
              Core.projectionField = (Core.Name "parmss")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

extensionGroupDefnWithParmss :: Phantoms.TTerm Syntax.ExtensionGroupDefn -> Phantoms.TTerm [[Syntax.ParamData]] -> Phantoms.TTerm Syntax.ExtensionGroupDefn
extensionGroupDefnWithParmss original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ExtensionGroupDefn"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ExtensionGroupDefn"),
              Core.projectionField = (Core.Name "tparams")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "parmss"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ExtensionGroupDefn"),
              Core.projectionField = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

extensionGroupDefnWithTparams :: Phantoms.TTerm Syntax.ExtensionGroupDefn -> Phantoms.TTerm [Syntax.ParamType] -> Phantoms.TTerm Syntax.ExtensionGroupDefn
extensionGroupDefnWithTparams original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ExtensionGroupDefn"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "parmss"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ExtensionGroupDefn"),
              Core.projectionField = (Core.Name "parmss")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ExtensionGroupDefn"),
              Core.projectionField = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

givenDefn :: Phantoms.TTerm [Syntax.Mod] -> Phantoms.TTerm Syntax.Name -> Phantoms.TTerm [[Syntax.ParamType]] -> Phantoms.TTerm [[Syntax.ParamData]] -> Phantoms.TTerm Syntax.Template -> Phantoms.TTerm Syntax.GivenDefn
givenDefn mods name tparams sparams templ =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.GivenDefn"),
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

givenAliasDefn :: Phantoms.TTerm [Syntax.Mod] -> Phantoms.TTerm Syntax.Name -> Phantoms.TTerm [[Syntax.ParamType]] -> Phantoms.TTerm [[Syntax.ParamData]] -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.Data -> Phantoms.TTerm Syntax.GivenAliasDefn
givenAliasDefn mods name tparams sparams decltpe body =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.GivenAliasDefn"),
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

givenAliasDefnBody :: Phantoms.TTerm Syntax.GivenAliasDefn -> Phantoms.TTerm Syntax.Data
givenAliasDefnBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenAliasDefn"),
        Core.projectionField = (Core.Name "body")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

givenAliasDefnDecltpe :: Phantoms.TTerm Syntax.GivenAliasDefn -> Phantoms.TTerm Syntax.Type
givenAliasDefnDecltpe x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenAliasDefn"),
        Core.projectionField = (Core.Name "decltpe")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

givenAliasDefnMods :: Phantoms.TTerm Syntax.GivenAliasDefn -> Phantoms.TTerm [Syntax.Mod]
givenAliasDefnMods x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenAliasDefn"),
        Core.projectionField = (Core.Name "mods")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

givenAliasDefnName :: Phantoms.TTerm Syntax.GivenAliasDefn -> Phantoms.TTerm Syntax.Name
givenAliasDefnName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenAliasDefn"),
        Core.projectionField = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

givenAliasDefnSparams :: Phantoms.TTerm Syntax.GivenAliasDefn -> Phantoms.TTerm [[Syntax.ParamData]]
givenAliasDefnSparams x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenAliasDefn"),
        Core.projectionField = (Core.Name "sparams")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

givenAliasDefnTparams :: Phantoms.TTerm Syntax.GivenAliasDefn -> Phantoms.TTerm [[Syntax.ParamType]]
givenAliasDefnTparams x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenAliasDefn"),
        Core.projectionField = (Core.Name "tparams")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

givenAliasDefnWithBody :: Phantoms.TTerm Syntax.GivenAliasDefn -> Phantoms.TTerm Syntax.Data -> Phantoms.TTerm Syntax.GivenAliasDefn
givenAliasDefnWithBody original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.GivenAliasDefn"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenAliasDefn"),
              Core.projectionField = (Core.Name "mods")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenAliasDefn"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenAliasDefn"),
              Core.projectionField = (Core.Name "tparams")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "sparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenAliasDefn"),
              Core.projectionField = (Core.Name "sparams")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenAliasDefn"),
              Core.projectionField = (Core.Name "decltpe")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

givenAliasDefnWithDecltpe :: Phantoms.TTerm Syntax.GivenAliasDefn -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.GivenAliasDefn
givenAliasDefnWithDecltpe original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.GivenAliasDefn"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenAliasDefn"),
              Core.projectionField = (Core.Name "mods")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenAliasDefn"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenAliasDefn"),
              Core.projectionField = (Core.Name "tparams")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "sparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenAliasDefn"),
              Core.projectionField = (Core.Name "sparams")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenAliasDefn"),
              Core.projectionField = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

givenAliasDefnWithMods :: Phantoms.TTerm Syntax.GivenAliasDefn -> Phantoms.TTerm [Syntax.Mod] -> Phantoms.TTerm Syntax.GivenAliasDefn
givenAliasDefnWithMods original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.GivenAliasDefn"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenAliasDefn"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenAliasDefn"),
              Core.projectionField = (Core.Name "tparams")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "sparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenAliasDefn"),
              Core.projectionField = (Core.Name "sparams")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenAliasDefn"),
              Core.projectionField = (Core.Name "decltpe")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenAliasDefn"),
              Core.projectionField = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

givenAliasDefnWithName :: Phantoms.TTerm Syntax.GivenAliasDefn -> Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.GivenAliasDefn
givenAliasDefnWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.GivenAliasDefn"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenAliasDefn"),
              Core.projectionField = (Core.Name "mods")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenAliasDefn"),
              Core.projectionField = (Core.Name "tparams")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "sparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenAliasDefn"),
              Core.projectionField = (Core.Name "sparams")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenAliasDefn"),
              Core.projectionField = (Core.Name "decltpe")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenAliasDefn"),
              Core.projectionField = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

givenAliasDefnWithSparams :: Phantoms.TTerm Syntax.GivenAliasDefn -> Phantoms.TTerm [[Syntax.ParamData]] -> Phantoms.TTerm Syntax.GivenAliasDefn
givenAliasDefnWithSparams original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.GivenAliasDefn"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenAliasDefn"),
              Core.projectionField = (Core.Name "mods")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenAliasDefn"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenAliasDefn"),
              Core.projectionField = (Core.Name "tparams")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "sparams"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenAliasDefn"),
              Core.projectionField = (Core.Name "decltpe")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenAliasDefn"),
              Core.projectionField = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

givenAliasDefnWithTparams :: Phantoms.TTerm Syntax.GivenAliasDefn -> Phantoms.TTerm [[Syntax.ParamType]] -> Phantoms.TTerm Syntax.GivenAliasDefn
givenAliasDefnWithTparams original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.GivenAliasDefn"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenAliasDefn"),
              Core.projectionField = (Core.Name "mods")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenAliasDefn"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "sparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenAliasDefn"),
              Core.projectionField = (Core.Name "sparams")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenAliasDefn"),
              Core.projectionField = (Core.Name "decltpe")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenAliasDefn"),
              Core.projectionField = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

givenDefnMods :: Phantoms.TTerm Syntax.GivenDefn -> Phantoms.TTerm [Syntax.Mod]
givenDefnMods x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenDefn"),
        Core.projectionField = (Core.Name "mods")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

givenDefnName :: Phantoms.TTerm Syntax.GivenDefn -> Phantoms.TTerm Syntax.Name
givenDefnName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenDefn"),
        Core.projectionField = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

givenDefnSparams :: Phantoms.TTerm Syntax.GivenDefn -> Phantoms.TTerm [[Syntax.ParamData]]
givenDefnSparams x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenDefn"),
        Core.projectionField = (Core.Name "sparams")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

givenDefnTempl :: Phantoms.TTerm Syntax.GivenDefn -> Phantoms.TTerm Syntax.Template
givenDefnTempl x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenDefn"),
        Core.projectionField = (Core.Name "templ")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

givenDefnTparams :: Phantoms.TTerm Syntax.GivenDefn -> Phantoms.TTerm [[Syntax.ParamType]]
givenDefnTparams x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenDefn"),
        Core.projectionField = (Core.Name "tparams")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

givenDefnWithMods :: Phantoms.TTerm Syntax.GivenDefn -> Phantoms.TTerm [Syntax.Mod] -> Phantoms.TTerm Syntax.GivenDefn
givenDefnWithMods original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.GivenDefn"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenDefn"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenDefn"),
              Core.projectionField = (Core.Name "tparams")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "sparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenDefn"),
              Core.projectionField = (Core.Name "sparams")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "templ"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenDefn"),
              Core.projectionField = (Core.Name "templ")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

givenDefnWithName :: Phantoms.TTerm Syntax.GivenDefn -> Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.GivenDefn
givenDefnWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.GivenDefn"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenDefn"),
              Core.projectionField = (Core.Name "mods")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenDefn"),
              Core.projectionField = (Core.Name "tparams")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "sparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenDefn"),
              Core.projectionField = (Core.Name "sparams")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "templ"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenDefn"),
              Core.projectionField = (Core.Name "templ")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

givenDefnWithSparams :: Phantoms.TTerm Syntax.GivenDefn -> Phantoms.TTerm [[Syntax.ParamData]] -> Phantoms.TTerm Syntax.GivenDefn
givenDefnWithSparams original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.GivenDefn"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenDefn"),
              Core.projectionField = (Core.Name "mods")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenDefn"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenDefn"),
              Core.projectionField = (Core.Name "tparams")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "sparams"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "templ"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenDefn"),
              Core.projectionField = (Core.Name "templ")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

givenDefnWithTempl :: Phantoms.TTerm Syntax.GivenDefn -> Phantoms.TTerm Syntax.Template -> Phantoms.TTerm Syntax.GivenDefn
givenDefnWithTempl original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.GivenDefn"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenDefn"),
              Core.projectionField = (Core.Name "mods")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenDefn"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenDefn"),
              Core.projectionField = (Core.Name "tparams")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "sparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenDefn"),
              Core.projectionField = (Core.Name "sparams")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "templ"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

givenDefnWithTparams :: Phantoms.TTerm Syntax.GivenDefn -> Phantoms.TTerm [[Syntax.ParamType]] -> Phantoms.TTerm Syntax.GivenDefn
givenDefnWithTparams original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.GivenDefn"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenDefn"),
              Core.projectionField = (Core.Name "mods")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenDefn"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "sparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenDefn"),
              Core.projectionField = (Core.Name "sparams")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "templ"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenDefn"),
              Core.projectionField = (Core.Name "templ")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

objectDefn :: Phantoms.TTerm Syntax.NameData -> Phantoms.TTerm Syntax.ObjectDefn
objectDefn name =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ObjectDefn"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)}]}))

objectDefnName :: Phantoms.TTerm Syntax.ObjectDefn -> Phantoms.TTerm Syntax.NameData
objectDefnName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ObjectDefn"),
        Core.projectionField = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

objectDefnWithName :: Phantoms.TTerm Syntax.ObjectDefn -> Phantoms.TTerm Syntax.NameData -> Phantoms.TTerm Syntax.ObjectDefn
objectDefnWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ObjectDefn"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

repeatedEnumCaseDefn :: Phantoms.TTerm [Syntax.Mod] -> Phantoms.TTerm [Syntax.NameData] -> Phantoms.TTerm Syntax.RepeatedEnumCaseDefn
repeatedEnumCaseDefn mods cases =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.RepeatedEnumCaseDefn"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Phantoms.unTTerm mods)},
        Core.Field {
          Core.fieldName = (Core.Name "cases"),
          Core.fieldTerm = (Phantoms.unTTerm cases)}]}))

repeatedEnumCaseDefnCases :: Phantoms.TTerm Syntax.RepeatedEnumCaseDefn -> Phantoms.TTerm [Syntax.NameData]
repeatedEnumCaseDefnCases x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.RepeatedEnumCaseDefn"),
        Core.projectionField = (Core.Name "cases")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

repeatedEnumCaseDefnMods :: Phantoms.TTerm Syntax.RepeatedEnumCaseDefn -> Phantoms.TTerm [Syntax.Mod]
repeatedEnumCaseDefnMods x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.RepeatedEnumCaseDefn"),
        Core.projectionField = (Core.Name "mods")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

repeatedEnumCaseDefnWithCases :: Phantoms.TTerm Syntax.RepeatedEnumCaseDefn -> Phantoms.TTerm [Syntax.NameData] -> Phantoms.TTerm Syntax.RepeatedEnumCaseDefn
repeatedEnumCaseDefnWithCases original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.RepeatedEnumCaseDefn"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.RepeatedEnumCaseDefn"),
              Core.projectionField = (Core.Name "mods")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "cases"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

repeatedEnumCaseDefnWithMods :: Phantoms.TTerm Syntax.RepeatedEnumCaseDefn -> Phantoms.TTerm [Syntax.Mod] -> Phantoms.TTerm Syntax.RepeatedEnumCaseDefn
repeatedEnumCaseDefnWithMods original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.RepeatedEnumCaseDefn"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "cases"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.RepeatedEnumCaseDefn"),
              Core.projectionField = (Core.Name "cases")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

traitDefn :: Phantoms.TTerm [Syntax.Mod] -> Phantoms.TTerm Syntax.NameType -> Phantoms.TTerm [Syntax.ParamType] -> Phantoms.TTerm Syntax.PrimaryCtor -> Phantoms.TTerm Syntax.Template -> Phantoms.TTerm Syntax.TraitDefn
traitDefn mods name tparams ctor template =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.TraitDefn"),
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

traitDefnCtor :: Phantoms.TTerm Syntax.TraitDefn -> Phantoms.TTerm Syntax.PrimaryCtor
traitDefnCtor x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TraitDefn"),
        Core.projectionField = (Core.Name "ctor")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

traitDefnMods :: Phantoms.TTerm Syntax.TraitDefn -> Phantoms.TTerm [Syntax.Mod]
traitDefnMods x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TraitDefn"),
        Core.projectionField = (Core.Name "mods")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

traitDefnName :: Phantoms.TTerm Syntax.TraitDefn -> Phantoms.TTerm Syntax.NameType
traitDefnName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TraitDefn"),
        Core.projectionField = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

traitDefnTemplate :: Phantoms.TTerm Syntax.TraitDefn -> Phantoms.TTerm Syntax.Template
traitDefnTemplate x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TraitDefn"),
        Core.projectionField = (Core.Name "template")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

traitDefnTparams :: Phantoms.TTerm Syntax.TraitDefn -> Phantoms.TTerm [Syntax.ParamType]
traitDefnTparams x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TraitDefn"),
        Core.projectionField = (Core.Name "tparams")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

traitDefnWithCtor :: Phantoms.TTerm Syntax.TraitDefn -> Phantoms.TTerm Syntax.PrimaryCtor -> Phantoms.TTerm Syntax.TraitDefn
traitDefnWithCtor original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.TraitDefn"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TraitDefn"),
              Core.projectionField = (Core.Name "mods")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TraitDefn"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TraitDefn"),
              Core.projectionField = (Core.Name "tparams")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ctor"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "template"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TraitDefn"),
              Core.projectionField = (Core.Name "template")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

traitDefnWithMods :: Phantoms.TTerm Syntax.TraitDefn -> Phantoms.TTerm [Syntax.Mod] -> Phantoms.TTerm Syntax.TraitDefn
traitDefnWithMods original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.TraitDefn"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TraitDefn"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TraitDefn"),
              Core.projectionField = (Core.Name "tparams")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ctor"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TraitDefn"),
              Core.projectionField = (Core.Name "ctor")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "template"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TraitDefn"),
              Core.projectionField = (Core.Name "template")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

traitDefnWithName :: Phantoms.TTerm Syntax.TraitDefn -> Phantoms.TTerm Syntax.NameType -> Phantoms.TTerm Syntax.TraitDefn
traitDefnWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.TraitDefn"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TraitDefn"),
              Core.projectionField = (Core.Name "mods")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TraitDefn"),
              Core.projectionField = (Core.Name "tparams")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ctor"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TraitDefn"),
              Core.projectionField = (Core.Name "ctor")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "template"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TraitDefn"),
              Core.projectionField = (Core.Name "template")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

traitDefnWithTemplate :: Phantoms.TTerm Syntax.TraitDefn -> Phantoms.TTerm Syntax.Template -> Phantoms.TTerm Syntax.TraitDefn
traitDefnWithTemplate original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.TraitDefn"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TraitDefn"),
              Core.projectionField = (Core.Name "mods")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TraitDefn"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TraitDefn"),
              Core.projectionField = (Core.Name "tparams")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ctor"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TraitDefn"),
              Core.projectionField = (Core.Name "ctor")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "template"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

traitDefnWithTparams :: Phantoms.TTerm Syntax.TraitDefn -> Phantoms.TTerm [Syntax.ParamType] -> Phantoms.TTerm Syntax.TraitDefn
traitDefnWithTparams original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.TraitDefn"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TraitDefn"),
              Core.projectionField = (Core.Name "mods")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TraitDefn"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "ctor"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TraitDefn"),
              Core.projectionField = (Core.Name "ctor")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "template"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TraitDefn"),
              Core.projectionField = (Core.Name "template")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

typeDefn :: Phantoms.TTerm [Syntax.Mod] -> Phantoms.TTerm Syntax.NameType -> Phantoms.TTerm [Syntax.ParamType] -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.TypeDefn
typeDefn mods name tparams body =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.TypeDefn"),
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

typeDefnBody :: Phantoms.TTerm Syntax.TypeDefn -> Phantoms.TTerm Syntax.Type
typeDefnBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TypeDefn"),
        Core.projectionField = (Core.Name "body")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

typeDefnMods :: Phantoms.TTerm Syntax.TypeDefn -> Phantoms.TTerm [Syntax.Mod]
typeDefnMods x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TypeDefn"),
        Core.projectionField = (Core.Name "mods")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

typeDefnName :: Phantoms.TTerm Syntax.TypeDefn -> Phantoms.TTerm Syntax.NameType
typeDefnName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TypeDefn"),
        Core.projectionField = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

typeDefnTparams :: Phantoms.TTerm Syntax.TypeDefn -> Phantoms.TTerm [Syntax.ParamType]
typeDefnTparams x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TypeDefn"),
        Core.projectionField = (Core.Name "tparams")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

typeDefnWithBody :: Phantoms.TTerm Syntax.TypeDefn -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.TypeDefn
typeDefnWithBody original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.TypeDefn"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TypeDefn"),
              Core.projectionField = (Core.Name "mods")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TypeDefn"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TypeDefn"),
              Core.projectionField = (Core.Name "tparams")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

typeDefnWithMods :: Phantoms.TTerm Syntax.TypeDefn -> Phantoms.TTerm [Syntax.Mod] -> Phantoms.TTerm Syntax.TypeDefn
typeDefnWithMods original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.TypeDefn"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TypeDefn"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TypeDefn"),
              Core.projectionField = (Core.Name "tparams")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TypeDefn"),
              Core.projectionField = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

typeDefnWithName :: Phantoms.TTerm Syntax.TypeDefn -> Phantoms.TTerm Syntax.NameType -> Phantoms.TTerm Syntax.TypeDefn
typeDefnWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.TypeDefn"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TypeDefn"),
              Core.projectionField = (Core.Name "mods")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TypeDefn"),
              Core.projectionField = (Core.Name "tparams")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TypeDefn"),
              Core.projectionField = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

typeDefnWithTparams :: Phantoms.TTerm Syntax.TypeDefn -> Phantoms.TTerm [Syntax.ParamType] -> Phantoms.TTerm Syntax.TypeDefn
typeDefnWithTparams original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.TypeDefn"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TypeDefn"),
              Core.projectionField = (Core.Name "mods")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TypeDefn"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TypeDefn"),
              Core.projectionField = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

valDefn :: Phantoms.TTerm [Syntax.Mod] -> Phantoms.TTerm [Syntax.Pat] -> Phantoms.TTerm (Maybe Syntax.Type) -> Phantoms.TTerm Syntax.Data -> Phantoms.TTerm Syntax.ValDefn
valDefn mods pats decltpe rhs =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ValDefn"),
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

valDefnDecltpe :: Phantoms.TTerm Syntax.ValDefn -> Phantoms.TTerm (Maybe Syntax.Type)
valDefnDecltpe x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ValDefn"),
        Core.projectionField = (Core.Name "decltpe")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

valDefnMods :: Phantoms.TTerm Syntax.ValDefn -> Phantoms.TTerm [Syntax.Mod]
valDefnMods x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ValDefn"),
        Core.projectionField = (Core.Name "mods")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

valDefnPats :: Phantoms.TTerm Syntax.ValDefn -> Phantoms.TTerm [Syntax.Pat]
valDefnPats x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ValDefn"),
        Core.projectionField = (Core.Name "pats")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

valDefnRhs :: Phantoms.TTerm Syntax.ValDefn -> Phantoms.TTerm Syntax.Data
valDefnRhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ValDefn"),
        Core.projectionField = (Core.Name "rhs")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

valDefnWithDecltpe :: Phantoms.TTerm Syntax.ValDefn -> Phantoms.TTerm (Maybe Syntax.Type) -> Phantoms.TTerm Syntax.ValDefn
valDefnWithDecltpe original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ValDefn"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ValDefn"),
              Core.projectionField = (Core.Name "mods")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "pats"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ValDefn"),
              Core.projectionField = (Core.Name "pats")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ValDefn"),
              Core.projectionField = (Core.Name "rhs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

valDefnWithMods :: Phantoms.TTerm Syntax.ValDefn -> Phantoms.TTerm [Syntax.Mod] -> Phantoms.TTerm Syntax.ValDefn
valDefnWithMods original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ValDefn"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "pats"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ValDefn"),
              Core.projectionField = (Core.Name "pats")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ValDefn"),
              Core.projectionField = (Core.Name "decltpe")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ValDefn"),
              Core.projectionField = (Core.Name "rhs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

valDefnWithPats :: Phantoms.TTerm Syntax.ValDefn -> Phantoms.TTerm [Syntax.Pat] -> Phantoms.TTerm Syntax.ValDefn
valDefnWithPats original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ValDefn"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ValDefn"),
              Core.projectionField = (Core.Name "mods")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "pats"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ValDefn"),
              Core.projectionField = (Core.Name "decltpe")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ValDefn"),
              Core.projectionField = (Core.Name "rhs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

valDefnWithRhs :: Phantoms.TTerm Syntax.ValDefn -> Phantoms.TTerm Syntax.Data -> Phantoms.TTerm Syntax.ValDefn
valDefnWithRhs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ValDefn"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ValDefn"),
              Core.projectionField = (Core.Name "mods")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "pats"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ValDefn"),
              Core.projectionField = (Core.Name "pats")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ValDefn"),
              Core.projectionField = (Core.Name "decltpe")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

varDefn :: Phantoms.TTerm [Syntax.Mod] -> Phantoms.TTerm [Syntax.Pat] -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm (Maybe Syntax.Data) -> Phantoms.TTerm Syntax.VarDefn
varDefn mods pats decltpe rhs =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.VarDefn"),
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

varDefnDecltpe :: Phantoms.TTerm Syntax.VarDefn -> Phantoms.TTerm Syntax.Type
varDefnDecltpe x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.VarDefn"),
        Core.projectionField = (Core.Name "decltpe")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

varDefnMods :: Phantoms.TTerm Syntax.VarDefn -> Phantoms.TTerm [Syntax.Mod]
varDefnMods x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.VarDefn"),
        Core.projectionField = (Core.Name "mods")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

varDefnPats :: Phantoms.TTerm Syntax.VarDefn -> Phantoms.TTerm [Syntax.Pat]
varDefnPats x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.VarDefn"),
        Core.projectionField = (Core.Name "pats")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

varDefnRhs :: Phantoms.TTerm Syntax.VarDefn -> Phantoms.TTerm (Maybe Syntax.Data)
varDefnRhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.VarDefn"),
        Core.projectionField = (Core.Name "rhs")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

varDefnWithDecltpe :: Phantoms.TTerm Syntax.VarDefn -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.VarDefn
varDefnWithDecltpe original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.VarDefn"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.VarDefn"),
              Core.projectionField = (Core.Name "mods")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "pats"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.VarDefn"),
              Core.projectionField = (Core.Name "pats")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.VarDefn"),
              Core.projectionField = (Core.Name "rhs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

varDefnWithMods :: Phantoms.TTerm Syntax.VarDefn -> Phantoms.TTerm [Syntax.Mod] -> Phantoms.TTerm Syntax.VarDefn
varDefnWithMods original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.VarDefn"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "pats"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.VarDefn"),
              Core.projectionField = (Core.Name "pats")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.VarDefn"),
              Core.projectionField = (Core.Name "decltpe")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.VarDefn"),
              Core.projectionField = (Core.Name "rhs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

varDefnWithPats :: Phantoms.TTerm Syntax.VarDefn -> Phantoms.TTerm [Syntax.Pat] -> Phantoms.TTerm Syntax.VarDefn
varDefnWithPats original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.VarDefn"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.VarDefn"),
              Core.projectionField = (Core.Name "mods")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "pats"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.VarDefn"),
              Core.projectionField = (Core.Name "decltpe")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.VarDefn"),
              Core.projectionField = (Core.Name "rhs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

varDefnWithRhs :: Phantoms.TTerm Syntax.VarDefn -> Phantoms.TTerm (Maybe Syntax.Data) -> Phantoms.TTerm Syntax.VarDefn
varDefnWithRhs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.VarDefn"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.VarDefn"),
              Core.projectionField = (Core.Name "mods")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "pats"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.VarDefn"),
              Core.projectionField = (Core.Name "pats")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.VarDefn"),
              Core.projectionField = (Core.Name "decltpe")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

enumeratorCaseGenerator :: Phantoms.TTerm Syntax.CaseGeneratorEnumerator -> Phantoms.TTerm Syntax.Enumerator
enumeratorCaseGenerator x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Enumerator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "caseGenerator"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

enumeratorGenerator :: Phantoms.TTerm Syntax.GeneratorEnumerator -> Phantoms.TTerm Syntax.Enumerator
enumeratorGenerator x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Enumerator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "generator"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

enumeratorGuard :: Phantoms.TTerm Syntax.GuardEnumerator -> Phantoms.TTerm Syntax.Enumerator
enumeratorGuard x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Enumerator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "guard"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

enumeratorVal :: Phantoms.TTerm Syntax.ValEnumerator -> Phantoms.TTerm Syntax.Enumerator
enumeratorVal x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Enumerator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "val"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

caseGeneratorEnumerator :: Phantoms.TTerm Syntax.Pat -> Phantoms.TTerm Syntax.Data -> Phantoms.TTerm Syntax.CaseGeneratorEnumerator
caseGeneratorEnumerator pat rhs =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.CaseGeneratorEnumerator"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pat"),
          Core.fieldTerm = (Phantoms.unTTerm pat)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTTerm rhs)}]}))

caseGeneratorEnumeratorPat :: Phantoms.TTerm Syntax.CaseGeneratorEnumerator -> Phantoms.TTerm Syntax.Pat
caseGeneratorEnumeratorPat x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.CaseGeneratorEnumerator"),
        Core.projectionField = (Core.Name "pat")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

caseGeneratorEnumeratorRhs :: Phantoms.TTerm Syntax.CaseGeneratorEnumerator -> Phantoms.TTerm Syntax.Data
caseGeneratorEnumeratorRhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.CaseGeneratorEnumerator"),
        Core.projectionField = (Core.Name "rhs")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

caseGeneratorEnumeratorWithPat :: Phantoms.TTerm Syntax.CaseGeneratorEnumerator -> Phantoms.TTerm Syntax.Pat -> Phantoms.TTerm Syntax.CaseGeneratorEnumerator
caseGeneratorEnumeratorWithPat original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.CaseGeneratorEnumerator"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pat"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.CaseGeneratorEnumerator"),
              Core.projectionField = (Core.Name "rhs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

caseGeneratorEnumeratorWithRhs :: Phantoms.TTerm Syntax.CaseGeneratorEnumerator -> Phantoms.TTerm Syntax.Data -> Phantoms.TTerm Syntax.CaseGeneratorEnumerator
caseGeneratorEnumeratorWithRhs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.CaseGeneratorEnumerator"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pat"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.CaseGeneratorEnumerator"),
              Core.projectionField = (Core.Name "pat")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

generatorEnumerator :: Phantoms.TTerm Syntax.Pat -> Phantoms.TTerm Syntax.Data -> Phantoms.TTerm Syntax.GeneratorEnumerator
generatorEnumerator pat rhs =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.GeneratorEnumerator"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pat"),
          Core.fieldTerm = (Phantoms.unTTerm pat)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTTerm rhs)}]}))

generatorEnumeratorPat :: Phantoms.TTerm Syntax.GeneratorEnumerator -> Phantoms.TTerm Syntax.Pat
generatorEnumeratorPat x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GeneratorEnumerator"),
        Core.projectionField = (Core.Name "pat")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

generatorEnumeratorRhs :: Phantoms.TTerm Syntax.GeneratorEnumerator -> Phantoms.TTerm Syntax.Data
generatorEnumeratorRhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GeneratorEnumerator"),
        Core.projectionField = (Core.Name "rhs")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

generatorEnumeratorWithPat :: Phantoms.TTerm Syntax.GeneratorEnumerator -> Phantoms.TTerm Syntax.Pat -> Phantoms.TTerm Syntax.GeneratorEnumerator
generatorEnumeratorWithPat original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.GeneratorEnumerator"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pat"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GeneratorEnumerator"),
              Core.projectionField = (Core.Name "rhs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

generatorEnumeratorWithRhs :: Phantoms.TTerm Syntax.GeneratorEnumerator -> Phantoms.TTerm Syntax.Data -> Phantoms.TTerm Syntax.GeneratorEnumerator
generatorEnumeratorWithRhs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.GeneratorEnumerator"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pat"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GeneratorEnumerator"),
              Core.projectionField = (Core.Name "pat")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

guardEnumerator :: Phantoms.TTerm Syntax.Data -> Phantoms.TTerm Syntax.GuardEnumerator
guardEnumerator cond =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.GuardEnumerator"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "cond"),
          Core.fieldTerm = (Phantoms.unTTerm cond)}]}))

guardEnumeratorCond :: Phantoms.TTerm Syntax.GuardEnumerator -> Phantoms.TTerm Syntax.Data
guardEnumeratorCond x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GuardEnumerator"),
        Core.projectionField = (Core.Name "cond")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

guardEnumeratorWithCond :: Phantoms.TTerm Syntax.GuardEnumerator -> Phantoms.TTerm Syntax.Data -> Phantoms.TTerm Syntax.GuardEnumerator
guardEnumeratorWithCond original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.GuardEnumerator"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "cond"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

valEnumerator :: Phantoms.TTerm Syntax.Pat -> Phantoms.TTerm Syntax.Data -> Phantoms.TTerm Syntax.ValEnumerator
valEnumerator pat rhs =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ValEnumerator"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pat"),
          Core.fieldTerm = (Phantoms.unTTerm pat)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTTerm rhs)}]}))

valEnumeratorPat :: Phantoms.TTerm Syntax.ValEnumerator -> Phantoms.TTerm Syntax.Pat
valEnumeratorPat x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ValEnumerator"),
        Core.projectionField = (Core.Name "pat")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

valEnumeratorRhs :: Phantoms.TTerm Syntax.ValEnumerator -> Phantoms.TTerm Syntax.Data
valEnumeratorRhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ValEnumerator"),
        Core.projectionField = (Core.Name "rhs")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

valEnumeratorWithPat :: Phantoms.TTerm Syntax.ValEnumerator -> Phantoms.TTerm Syntax.Pat -> Phantoms.TTerm Syntax.ValEnumerator
valEnumeratorWithPat original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ValEnumerator"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pat"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ValEnumerator"),
              Core.projectionField = (Core.Name "rhs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

valEnumeratorWithRhs :: Phantoms.TTerm Syntax.ValEnumerator -> Phantoms.TTerm Syntax.Data -> Phantoms.TTerm Syntax.ValEnumerator
valEnumeratorWithRhs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ValEnumerator"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pat"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ValEnumerator"),
              Core.projectionField = (Core.Name "pat")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

export :: Phantoms.TTerm [Syntax.Importer] -> Phantoms.TTerm Syntax.Export
export importers =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.Export"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "importers"),
          Core.fieldTerm = (Phantoms.unTTerm importers)}]}))

exportImporters :: Phantoms.TTerm Syntax.Export -> Phantoms.TTerm [Syntax.Importer]
exportImporters x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.Export"),
        Core.projectionField = (Core.Name "importers")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

exportWithImporters :: Phantoms.TTerm Syntax.Export -> Phantoms.TTerm [Syntax.Importer] -> Phantoms.TTerm Syntax.Export
exportWithImporters original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.Export"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "importers"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

import_ :: Phantoms.TTerm [Syntax.Importer] -> Phantoms.TTerm Syntax.Import
import_ importers =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.Import"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "importers"),
          Core.fieldTerm = (Phantoms.unTTerm importers)}]}))

importExportStatExport :: Phantoms.TTerm Syntax.Export -> Phantoms.TTerm Syntax.ImportExportStat
importExportStatExport x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.ImportExportStat"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "export"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

importExportStatImport :: Phantoms.TTerm Syntax.Import -> Phantoms.TTerm Syntax.ImportExportStat
importExportStatImport x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.ImportExportStat"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "import"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

importImporters :: Phantoms.TTerm Syntax.Import -> Phantoms.TTerm [Syntax.Importer]
importImporters x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.Import"),
        Core.projectionField = (Core.Name "importers")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

importWithImporters :: Phantoms.TTerm Syntax.Import -> Phantoms.TTerm [Syntax.Importer] -> Phantoms.TTerm Syntax.Import
importWithImporters original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.Import"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "importers"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

importeeGiven :: Phantoms.TTerm Syntax.GivenImportee -> Phantoms.TTerm Syntax.Importee
importeeGiven x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Importee"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "given"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

importeeGivenAll :: Phantoms.TTerm Syntax.Importee
importeeGivenAll =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Importee"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "givenAll"),
        Core.fieldTerm = Core.TermUnit}}))

importeeName :: Phantoms.TTerm Syntax.NameImportee -> Phantoms.TTerm Syntax.Importee
importeeName x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Importee"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "name"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

importeeRename :: Phantoms.TTerm Syntax.RenameImportee -> Phantoms.TTerm Syntax.Importee
importeeRename x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Importee"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "rename"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

importeeUnimport :: Phantoms.TTerm Syntax.UnimportImportee -> Phantoms.TTerm Syntax.Importee
importeeUnimport x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Importee"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "unimport"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

importeeWildcard :: Phantoms.TTerm Syntax.Importee
importeeWildcard =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Importee"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "wildcard"),
        Core.fieldTerm = Core.TermUnit}}))

givenImportee :: Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.GivenImportee
givenImportee tpe =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.GivenImportee"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "tpe"),
          Core.fieldTerm = (Phantoms.unTTerm tpe)}]}))

givenImporteeTpe :: Phantoms.TTerm Syntax.GivenImportee -> Phantoms.TTerm Syntax.Type
givenImporteeTpe x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenImportee"),
        Core.projectionField = (Core.Name "tpe")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

givenImporteeWithTpe :: Phantoms.TTerm Syntax.GivenImportee -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.GivenImportee
givenImporteeWithTpe original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.GivenImportee"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "tpe"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

nameImportee :: Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.NameImportee
nameImportee name =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.NameImportee"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)}]}))

nameImporteeName :: Phantoms.TTerm Syntax.NameImportee -> Phantoms.TTerm Syntax.Name
nameImporteeName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.NameImportee"),
        Core.projectionField = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

nameImporteeWithName :: Phantoms.TTerm Syntax.NameImportee -> Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.NameImportee
nameImporteeWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.NameImportee"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

renameImportee :: Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.RenameImportee
renameImportee name rename =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.RenameImportee"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "rename"),
          Core.fieldTerm = (Phantoms.unTTerm rename)}]}))

renameImporteeName :: Phantoms.TTerm Syntax.RenameImportee -> Phantoms.TTerm Syntax.Name
renameImporteeName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.RenameImportee"),
        Core.projectionField = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

renameImporteeRename :: Phantoms.TTerm Syntax.RenameImportee -> Phantoms.TTerm Syntax.Name
renameImporteeRename x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.RenameImportee"),
        Core.projectionField = (Core.Name "rename")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

renameImporteeWithName :: Phantoms.TTerm Syntax.RenameImportee -> Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.RenameImportee
renameImporteeWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.RenameImportee"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "rename"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.RenameImportee"),
              Core.projectionField = (Core.Name "rename")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

renameImporteeWithRename :: Phantoms.TTerm Syntax.RenameImportee -> Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.RenameImportee
renameImporteeWithRename original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.RenameImportee"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.RenameImportee"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rename"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

unimportImportee :: Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.UnimportImportee
unimportImportee name =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.UnimportImportee"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)}]}))

unimportImporteeName :: Phantoms.TTerm Syntax.UnimportImportee -> Phantoms.TTerm Syntax.Name
unimportImporteeName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.UnimportImportee"),
        Core.projectionField = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unimportImporteeWithName :: Phantoms.TTerm Syntax.UnimportImportee -> Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.UnimportImportee
unimportImporteeWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.UnimportImportee"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

importer :: Phantoms.TTerm Syntax.RefData -> Phantoms.TTerm [Syntax.Importee] -> Phantoms.TTerm Syntax.Importer
importer ref importees =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.Importer"),
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
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.Importer"),
        Core.projectionField = (Core.Name "importees")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

importerRef :: Phantoms.TTerm Syntax.Importer -> Phantoms.TTerm Syntax.RefData
importerRef x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.Importer"),
        Core.projectionField = (Core.Name "ref")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

importerWithImportees :: Phantoms.TTerm Syntax.Importer -> Phantoms.TTerm [Syntax.Importee] -> Phantoms.TTerm Syntax.Importer
importerWithImportees original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.Importer"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "ref"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.Importer"),
              Core.projectionField = (Core.Name "ref")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "importees"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

importerWithRef :: Phantoms.TTerm Syntax.Importer -> Phantoms.TTerm Syntax.RefData -> Phantoms.TTerm Syntax.Importer
importerWithRef original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.Importer"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "ref"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "importees"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.Importer"),
              Core.projectionField = (Core.Name "importees")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

init :: Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.Name -> Phantoms.TTerm [[Syntax.Data]] -> Phantoms.TTerm Syntax.Init
init tpe name argss =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.Init"),
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
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.Init"),
        Core.projectionField = (Core.Name "argss")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

initName :: Phantoms.TTerm Syntax.Init -> Phantoms.TTerm Syntax.Name
initName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.Init"),
        Core.projectionField = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

initTpe :: Phantoms.TTerm Syntax.Init -> Phantoms.TTerm Syntax.Type
initTpe x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.Init"),
        Core.projectionField = (Core.Name "tpe")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

initWithArgss :: Phantoms.TTerm Syntax.Init -> Phantoms.TTerm [[Syntax.Data]] -> Phantoms.TTerm Syntax.Init
initWithArgss original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.Init"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "tpe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.Init"),
              Core.projectionField = (Core.Name "tpe")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.Init"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "argss"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

initWithName :: Phantoms.TTerm Syntax.Init -> Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.Init
initWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.Init"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "tpe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.Init"),
              Core.projectionField = (Core.Name "tpe")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "argss"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.Init"),
              Core.projectionField = (Core.Name "argss")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

initWithTpe :: Phantoms.TTerm Syntax.Init -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.Init
initWithTpe original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.Init"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "tpe"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.Init"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "argss"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.Init"),
              Core.projectionField = (Core.Name "argss")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

litBoolean :: Phantoms.TTerm Bool -> Phantoms.TTerm Syntax.Lit
litBoolean x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Lit"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "boolean"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

litByte :: Phantoms.TTerm I.Int8 -> Phantoms.TTerm Syntax.Lit
litByte x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Lit"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "byte"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

litBytes :: Phantoms.TTerm [Int] -> Phantoms.TTerm Syntax.Lit
litBytes x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Lit"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "bytes"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

litChar :: Phantoms.TTerm Int -> Phantoms.TTerm Syntax.Lit
litChar x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Lit"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "char"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

litDouble :: Phantoms.TTerm Double -> Phantoms.TTerm Syntax.Lit
litDouble x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Lit"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "double"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

litFloat :: Phantoms.TTerm Float -> Phantoms.TTerm Syntax.Lit
litFloat x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Lit"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "float"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

litInt :: Phantoms.TTerm Int -> Phantoms.TTerm Syntax.Lit
litInt x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Lit"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "int"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

litLong :: Phantoms.TTerm I.Int64 -> Phantoms.TTerm Syntax.Lit
litLong x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Lit"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "long"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

litNull :: Phantoms.TTerm Syntax.Lit
litNull =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Lit"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "null"),
        Core.fieldTerm = Core.TermUnit}}))

litShort :: Phantoms.TTerm I.Int16 -> Phantoms.TTerm Syntax.Lit
litShort x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Lit"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "short"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

litString :: Phantoms.TTerm String -> Phantoms.TTerm Syntax.Lit
litString x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Lit"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "string"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

litSymbol :: Phantoms.TTerm Syntax.ScalaSymbol -> Phantoms.TTerm Syntax.Lit
litSymbol x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Lit"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "symbol"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

litUnit :: Phantoms.TTerm Syntax.Lit
litUnit =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Lit"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "unit"),
        Core.fieldTerm = Core.TermUnit}}))

memberSelf :: Phantoms.TTerm Syntax.Self -> Phantoms.TTerm Syntax.Member
memberSelf x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Member"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "self"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

memberTerm :: Phantoms.TTerm Syntax.DataMember -> Phantoms.TTerm Syntax.Member
memberTerm x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Member"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "term"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

memberTermParam :: Phantoms.TTerm Syntax.ParamData -> Phantoms.TTerm Syntax.Member
memberTermParam x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Member"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "termParam"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

memberType :: Phantoms.TTerm Syntax.TypeMember -> Phantoms.TTerm Syntax.Member
memberType x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Member"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "type"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

memberTypeParam :: Phantoms.TTerm Syntax.ParamType -> Phantoms.TTerm Syntax.Member
memberTypeParam x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Member"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "typeParam"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

dataMemberObject :: Phantoms.TTerm Syntax.ObjectPkg -> Phantoms.TTerm Syntax.DataMember
dataMemberObject x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.DataMember"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "object"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

dataMemberPkg :: Phantoms.TTerm Syntax.Pkg -> Phantoms.TTerm Syntax.DataMember
dataMemberPkg x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.DataMember"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "pkg"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

typeMember :: Phantoms.TTerm Syntax.NameType -> Phantoms.TTerm Syntax.TypeMember
typeMember name =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.TypeMember"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)}]}))

typeMemberName :: Phantoms.TTerm Syntax.TypeMember -> Phantoms.TTerm Syntax.NameType
typeMemberName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TypeMember"),
        Core.projectionField = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

typeMemberWithName :: Phantoms.TTerm Syntax.TypeMember -> Phantoms.TTerm Syntax.NameType -> Phantoms.TTerm Syntax.TypeMember
typeMemberWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.TypeMember"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

modAbstract :: Phantoms.TTerm Syntax.Mod
modAbstract =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Mod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "abstract"),
        Core.fieldTerm = Core.TermUnit}}))

modAnnot :: Phantoms.TTerm Syntax.AnnotMod -> Phantoms.TTerm Syntax.Mod
modAnnot x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Mod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "annot"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

modCase :: Phantoms.TTerm Syntax.Mod
modCase =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Mod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "case"),
        Core.fieldTerm = Core.TermUnit}}))

modContravariant :: Phantoms.TTerm Syntax.Mod
modContravariant =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Mod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "contravariant"),
        Core.fieldTerm = Core.TermUnit}}))

modCovariant :: Phantoms.TTerm Syntax.Mod
modCovariant =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Mod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "covariant"),
        Core.fieldTerm = Core.TermUnit}}))

modFinal :: Phantoms.TTerm Syntax.Mod
modFinal =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Mod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "final"),
        Core.fieldTerm = Core.TermUnit}}))

modImplicit :: Phantoms.TTerm Syntax.Mod
modImplicit =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Mod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "implicit"),
        Core.fieldTerm = Core.TermUnit}}))

modInfix :: Phantoms.TTerm Syntax.Mod
modInfix =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Mod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "infix"),
        Core.fieldTerm = Core.TermUnit}}))

modInline :: Phantoms.TTerm Syntax.Mod
modInline =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Mod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "inline"),
        Core.fieldTerm = Core.TermUnit}}))

modLazy :: Phantoms.TTerm Syntax.Mod
modLazy =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Mod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "lazy"),
        Core.fieldTerm = Core.TermUnit}}))

modOpaque :: Phantoms.TTerm Syntax.Mod
modOpaque =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Mod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "opaque"),
        Core.fieldTerm = Core.TermUnit}}))

modOpen :: Phantoms.TTerm Syntax.Mod
modOpen =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Mod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "open"),
        Core.fieldTerm = Core.TermUnit}}))

modOverride :: Phantoms.TTerm Syntax.Mod
modOverride =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Mod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "override"),
        Core.fieldTerm = Core.TermUnit}}))

modPrivate :: Phantoms.TTerm Syntax.PrivateMod -> Phantoms.TTerm Syntax.Mod
modPrivate x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Mod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "private"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

modProtected :: Phantoms.TTerm Syntax.ProtectedMod -> Phantoms.TTerm Syntax.Mod
modProtected x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Mod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "protected"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

modSealed :: Phantoms.TTerm Syntax.Mod
modSealed =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Mod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "sealed"),
        Core.fieldTerm = Core.TermUnit}}))

modSuper :: Phantoms.TTerm Syntax.Mod
modSuper =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Mod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "super"),
        Core.fieldTerm = Core.TermUnit}}))

modTransparent :: Phantoms.TTerm Syntax.Mod
modTransparent =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Mod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "transparent"),
        Core.fieldTerm = Core.TermUnit}}))

modUsing :: Phantoms.TTerm Syntax.Mod
modUsing =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Mod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "using"),
        Core.fieldTerm = Core.TermUnit}}))

modValParam :: Phantoms.TTerm Syntax.Mod
modValParam =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Mod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "valParam"),
        Core.fieldTerm = Core.TermUnit}}))

modVarParam :: Phantoms.TTerm Syntax.Mod
modVarParam =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Mod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "varParam"),
        Core.fieldTerm = Core.TermUnit}}))

annotMod :: Phantoms.TTerm Syntax.Init -> Phantoms.TTerm Syntax.AnnotMod
annotMod init =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.AnnotMod"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "init"),
          Core.fieldTerm = (Phantoms.unTTerm init)}]}))

annotModInit :: Phantoms.TTerm Syntax.AnnotMod -> Phantoms.TTerm Syntax.Init
annotModInit x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.AnnotMod"),
        Core.projectionField = (Core.Name "init")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

annotModWithInit :: Phantoms.TTerm Syntax.AnnotMod -> Phantoms.TTerm Syntax.Init -> Phantoms.TTerm Syntax.AnnotMod
annotModWithInit original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.AnnotMod"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "init"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

privateMod :: Phantoms.TTerm Syntax.Ref -> Phantoms.TTerm Syntax.PrivateMod
privateMod within =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.PrivateMod"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "within"),
          Core.fieldTerm = (Phantoms.unTTerm within)}]}))

privateModWithWithin :: Phantoms.TTerm Syntax.PrivateMod -> Phantoms.TTerm Syntax.Ref -> Phantoms.TTerm Syntax.PrivateMod
privateModWithWithin original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.PrivateMod"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "within"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

privateModWithin :: Phantoms.TTerm Syntax.PrivateMod -> Phantoms.TTerm Syntax.Ref
privateModWithin x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.PrivateMod"),
        Core.projectionField = (Core.Name "within")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

protectedMod :: Phantoms.TTerm Syntax.Ref -> Phantoms.TTerm Syntax.ProtectedMod
protectedMod within =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ProtectedMod"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "within"),
          Core.fieldTerm = (Phantoms.unTTerm within)}]}))

protectedModWithWithin :: Phantoms.TTerm Syntax.ProtectedMod -> Phantoms.TTerm Syntax.Ref -> Phantoms.TTerm Syntax.ProtectedMod
protectedModWithWithin original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ProtectedMod"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "within"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

protectedModWithin :: Phantoms.TTerm Syntax.ProtectedMod -> Phantoms.TTerm Syntax.Ref
protectedModWithin x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ProtectedMod"),
        Core.projectionField = (Core.Name "within")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

nameAnonymous :: Phantoms.TTerm Syntax.Name
nameAnonymous =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Name"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "anonymous"),
        Core.fieldTerm = Core.TermUnit}}))

nameIndeterminate :: Phantoms.TTerm Syntax.PredefString -> Phantoms.TTerm Syntax.Name
nameIndeterminate x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Name"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "indeterminate"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

nameValue :: Phantoms.TTerm String -> Phantoms.TTerm Syntax.Name
nameValue x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Name"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "value"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

patAlternative :: Phantoms.TTerm Syntax.AlternativePat -> Phantoms.TTerm Syntax.Pat
patAlternative x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Pat"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "alternative"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

patBind :: Phantoms.TTerm Syntax.BindPat -> Phantoms.TTerm Syntax.Pat
patBind x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Pat"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "bind"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

patExtract :: Phantoms.TTerm Syntax.ExtractPat -> Phantoms.TTerm Syntax.Pat
patExtract x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Pat"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "extract"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

patExtractInfix :: Phantoms.TTerm Syntax.ExtractInfixPat -> Phantoms.TTerm Syntax.Pat
patExtractInfix x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Pat"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "extractInfix"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

patGiven :: Phantoms.TTerm Syntax.GivenPat -> Phantoms.TTerm Syntax.Pat
patGiven x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Pat"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "given"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

patInterpolate :: Phantoms.TTerm Syntax.InterpolatePat -> Phantoms.TTerm Syntax.Pat
patInterpolate x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Pat"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "interpolate"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

patRepeated :: Phantoms.TTerm Syntax.RepeatedPat -> Phantoms.TTerm Syntax.Pat
patRepeated x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Pat"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "repeated"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

patSeqWildcard :: Phantoms.TTerm Syntax.Pat
patSeqWildcard =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Pat"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "seqWildcard"),
        Core.fieldTerm = Core.TermUnit}}))

patTuple :: Phantoms.TTerm Syntax.TuplePat -> Phantoms.TTerm Syntax.Pat
patTuple x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Pat"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "tuple"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

patTyped :: Phantoms.TTerm Syntax.TypedPat -> Phantoms.TTerm Syntax.Pat
patTyped x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Pat"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "typed"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

patVar :: Phantoms.TTerm Syntax.VarPat -> Phantoms.TTerm Syntax.Pat
patVar x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Pat"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "var"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

patWildcard :: Phantoms.TTerm Syntax.Pat
patWildcard =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Pat"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "wildcard"),
        Core.fieldTerm = Core.TermUnit}}))

alternativePat :: Phantoms.TTerm Syntax.Pat -> Phantoms.TTerm Syntax.Pat -> Phantoms.TTerm Syntax.AlternativePat
alternativePat lhs rhs =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.AlternativePat"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Phantoms.unTTerm lhs)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTTerm rhs)}]}))

alternativePatLhs :: Phantoms.TTerm Syntax.AlternativePat -> Phantoms.TTerm Syntax.Pat
alternativePatLhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.AlternativePat"),
        Core.projectionField = (Core.Name "lhs")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

alternativePatRhs :: Phantoms.TTerm Syntax.AlternativePat -> Phantoms.TTerm Syntax.Pat
alternativePatRhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.AlternativePat"),
        Core.projectionField = (Core.Name "rhs")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

alternativePatWithLhs :: Phantoms.TTerm Syntax.AlternativePat -> Phantoms.TTerm Syntax.Pat -> Phantoms.TTerm Syntax.AlternativePat
alternativePatWithLhs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.AlternativePat"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.AlternativePat"),
              Core.projectionField = (Core.Name "rhs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

alternativePatWithRhs :: Phantoms.TTerm Syntax.AlternativePat -> Phantoms.TTerm Syntax.Pat -> Phantoms.TTerm Syntax.AlternativePat
alternativePatWithRhs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.AlternativePat"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.AlternativePat"),
              Core.projectionField = (Core.Name "lhs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

bindPat :: Phantoms.TTerm Syntax.Pat -> Phantoms.TTerm Syntax.Pat -> Phantoms.TTerm Syntax.BindPat
bindPat lhs rhs =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.BindPat"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Phantoms.unTTerm lhs)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTTerm rhs)}]}))

bindPatLhs :: Phantoms.TTerm Syntax.BindPat -> Phantoms.TTerm Syntax.Pat
bindPatLhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.BindPat"),
        Core.projectionField = (Core.Name "lhs")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

bindPatRhs :: Phantoms.TTerm Syntax.BindPat -> Phantoms.TTerm Syntax.Pat
bindPatRhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.BindPat"),
        Core.projectionField = (Core.Name "rhs")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

bindPatWithLhs :: Phantoms.TTerm Syntax.BindPat -> Phantoms.TTerm Syntax.Pat -> Phantoms.TTerm Syntax.BindPat
bindPatWithLhs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.BindPat"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.BindPat"),
              Core.projectionField = (Core.Name "rhs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

bindPatWithRhs :: Phantoms.TTerm Syntax.BindPat -> Phantoms.TTerm Syntax.Pat -> Phantoms.TTerm Syntax.BindPat
bindPatWithRhs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.BindPat"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.BindPat"),
              Core.projectionField = (Core.Name "lhs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

extractPat :: Phantoms.TTerm Syntax.Data -> Phantoms.TTerm [Syntax.Pat] -> Phantoms.TTerm Syntax.ExtractPat
extractPat fun args =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ExtractPat"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "fun"),
          Core.fieldTerm = (Phantoms.unTTerm fun)},
        Core.Field {
          Core.fieldName = (Core.Name "args"),
          Core.fieldTerm = (Phantoms.unTTerm args)}]}))

extractPatArgs :: Phantoms.TTerm Syntax.ExtractPat -> Phantoms.TTerm [Syntax.Pat]
extractPatArgs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ExtractPat"),
        Core.projectionField = (Core.Name "args")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

extractPatFun :: Phantoms.TTerm Syntax.ExtractPat -> Phantoms.TTerm Syntax.Data
extractPatFun x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ExtractPat"),
        Core.projectionField = (Core.Name "fun")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

extractInfixPat :: Phantoms.TTerm Syntax.Pat -> Phantoms.TTerm Syntax.NameData -> Phantoms.TTerm [Syntax.Pat] -> Phantoms.TTerm Syntax.ExtractInfixPat
extractInfixPat lhs op rhs =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ExtractInfixPat"),
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

extractInfixPatLhs :: Phantoms.TTerm Syntax.ExtractInfixPat -> Phantoms.TTerm Syntax.Pat
extractInfixPatLhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ExtractInfixPat"),
        Core.projectionField = (Core.Name "lhs")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

extractInfixPatOp :: Phantoms.TTerm Syntax.ExtractInfixPat -> Phantoms.TTerm Syntax.NameData
extractInfixPatOp x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ExtractInfixPat"),
        Core.projectionField = (Core.Name "op")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

extractInfixPatRhs :: Phantoms.TTerm Syntax.ExtractInfixPat -> Phantoms.TTerm [Syntax.Pat]
extractInfixPatRhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ExtractInfixPat"),
        Core.projectionField = (Core.Name "rhs")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

extractInfixPatWithLhs :: Phantoms.TTerm Syntax.ExtractInfixPat -> Phantoms.TTerm Syntax.Pat -> Phantoms.TTerm Syntax.ExtractInfixPat
extractInfixPatWithLhs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ExtractInfixPat"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "op"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ExtractInfixPat"),
              Core.projectionField = (Core.Name "op")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ExtractInfixPat"),
              Core.projectionField = (Core.Name "rhs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

extractInfixPatWithOp :: Phantoms.TTerm Syntax.ExtractInfixPat -> Phantoms.TTerm Syntax.NameData -> Phantoms.TTerm Syntax.ExtractInfixPat
extractInfixPatWithOp original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ExtractInfixPat"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ExtractInfixPat"),
              Core.projectionField = (Core.Name "lhs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "op"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ExtractInfixPat"),
              Core.projectionField = (Core.Name "rhs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

extractInfixPatWithRhs :: Phantoms.TTerm Syntax.ExtractInfixPat -> Phantoms.TTerm [Syntax.Pat] -> Phantoms.TTerm Syntax.ExtractInfixPat
extractInfixPatWithRhs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ExtractInfixPat"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ExtractInfixPat"),
              Core.projectionField = (Core.Name "lhs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "op"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ExtractInfixPat"),
              Core.projectionField = (Core.Name "op")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

extractPatWithArgs :: Phantoms.TTerm Syntax.ExtractPat -> Phantoms.TTerm [Syntax.Pat] -> Phantoms.TTerm Syntax.ExtractPat
extractPatWithArgs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ExtractPat"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "fun"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ExtractPat"),
              Core.projectionField = (Core.Name "fun")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "args"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

extractPatWithFun :: Phantoms.TTerm Syntax.ExtractPat -> Phantoms.TTerm Syntax.Data -> Phantoms.TTerm Syntax.ExtractPat
extractPatWithFun original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ExtractPat"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "fun"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "args"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ExtractPat"),
              Core.projectionField = (Core.Name "args")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

givenPat :: Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.GivenPat
givenPat tpe =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.GivenPat"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "tpe"),
          Core.fieldTerm = (Phantoms.unTTerm tpe)}]}))

givenPatTpe :: Phantoms.TTerm Syntax.GivenPat -> Phantoms.TTerm Syntax.Type
givenPatTpe x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenPat"),
        Core.projectionField = (Core.Name "tpe")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

givenPatWithTpe :: Phantoms.TTerm Syntax.GivenPat -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.GivenPat
givenPatWithTpe original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.GivenPat"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "tpe"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

interpolatePat :: Phantoms.TTerm Syntax.NameData -> Phantoms.TTerm [Syntax.Lit] -> Phantoms.TTerm Syntax.InterpolatePat
interpolatePat prefix parts =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.InterpolatePat"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "prefix"),
          Core.fieldTerm = (Phantoms.unTTerm prefix)},
        Core.Field {
          Core.fieldName = (Core.Name "parts"),
          Core.fieldTerm = (Phantoms.unTTerm parts)}]}))

interpolatePatParts :: Phantoms.TTerm Syntax.InterpolatePat -> Phantoms.TTerm [Syntax.Lit]
interpolatePatParts x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.InterpolatePat"),
        Core.projectionField = (Core.Name "parts")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

interpolatePatPrefix :: Phantoms.TTerm Syntax.InterpolatePat -> Phantoms.TTerm Syntax.NameData
interpolatePatPrefix x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.InterpolatePat"),
        Core.projectionField = (Core.Name "prefix")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

interpolatePatWithParts :: Phantoms.TTerm Syntax.InterpolatePat -> Phantoms.TTerm [Syntax.Lit] -> Phantoms.TTerm Syntax.InterpolatePat
interpolatePatWithParts original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.InterpolatePat"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "prefix"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.InterpolatePat"),
              Core.projectionField = (Core.Name "prefix")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "parts"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

interpolatePatWithPrefix :: Phantoms.TTerm Syntax.InterpolatePat -> Phantoms.TTerm Syntax.NameData -> Phantoms.TTerm Syntax.InterpolatePat
interpolatePatWithPrefix original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.InterpolatePat"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "prefix"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "parts"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.InterpolatePat"),
              Core.projectionField = (Core.Name "parts")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

repeatedPat :: Phantoms.TTerm Syntax.NameData -> Phantoms.TTerm Syntax.RepeatedPat
repeatedPat name =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.RepeatedPat"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)}]}))

repeatedPatName :: Phantoms.TTerm Syntax.RepeatedPat -> Phantoms.TTerm Syntax.NameData
repeatedPatName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.RepeatedPat"),
        Core.projectionField = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

repeatedPatWithName :: Phantoms.TTerm Syntax.RepeatedPat -> Phantoms.TTerm Syntax.NameData -> Phantoms.TTerm Syntax.RepeatedPat
repeatedPatWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.RepeatedPat"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

tuplePat :: Phantoms.TTerm [Syntax.Pat] -> Phantoms.TTerm Syntax.TuplePat
tuplePat args =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.TuplePat"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "args"),
          Core.fieldTerm = (Phantoms.unTTerm args)}]}))

tuplePatArgs :: Phantoms.TTerm Syntax.TuplePat -> Phantoms.TTerm [Syntax.Pat]
tuplePatArgs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TuplePat"),
        Core.projectionField = (Core.Name "args")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

tuplePatWithArgs :: Phantoms.TTerm Syntax.TuplePat -> Phantoms.TTerm [Syntax.Pat] -> Phantoms.TTerm Syntax.TuplePat
tuplePatWithArgs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.TuplePat"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "args"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

typedPat :: Phantoms.TTerm Syntax.Pat -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.TypedPat
typedPat lhs rhs =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.TypedPat"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Phantoms.unTTerm lhs)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTTerm rhs)}]}))

typedPatLhs :: Phantoms.TTerm Syntax.TypedPat -> Phantoms.TTerm Syntax.Pat
typedPatLhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TypedPat"),
        Core.projectionField = (Core.Name "lhs")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

typedPatRhs :: Phantoms.TTerm Syntax.TypedPat -> Phantoms.TTerm Syntax.Type
typedPatRhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TypedPat"),
        Core.projectionField = (Core.Name "rhs")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

typedPatWithLhs :: Phantoms.TTerm Syntax.TypedPat -> Phantoms.TTerm Syntax.Pat -> Phantoms.TTerm Syntax.TypedPat
typedPatWithLhs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.TypedPat"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TypedPat"),
              Core.projectionField = (Core.Name "rhs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

typedPatWithRhs :: Phantoms.TTerm Syntax.TypedPat -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.TypedPat
typedPatWithRhs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.TypedPat"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TypedPat"),
              Core.projectionField = (Core.Name "lhs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

varPat :: Phantoms.TTerm Syntax.NameData -> Phantoms.TTerm Syntax.VarPat
varPat name =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.VarPat"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)}]}))

varPatName :: Phantoms.TTerm Syntax.VarPat -> Phantoms.TTerm Syntax.NameData
varPatName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.VarPat"),
        Core.projectionField = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

varPatWithName :: Phantoms.TTerm Syntax.VarPat -> Phantoms.TTerm Syntax.NameData -> Phantoms.TTerm Syntax.VarPat
varPatWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.VarPat"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

pkg :: Phantoms.TTerm Syntax.NameData -> Phantoms.TTerm Syntax.RefData -> Phantoms.TTerm [Syntax.Stat] -> Phantoms.TTerm Syntax.Pkg
pkg name ref stats =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.Pkg"),
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

pkgName :: Phantoms.TTerm Syntax.Pkg -> Phantoms.TTerm Syntax.NameData
pkgName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.Pkg"),
        Core.projectionField = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

pkgRef :: Phantoms.TTerm Syntax.Pkg -> Phantoms.TTerm Syntax.RefData
pkgRef x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.Pkg"),
        Core.projectionField = (Core.Name "ref")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

pkgStats :: Phantoms.TTerm Syntax.Pkg -> Phantoms.TTerm [Syntax.Stat]
pkgStats x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.Pkg"),
        Core.projectionField = (Core.Name "stats")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

pkgWithName :: Phantoms.TTerm Syntax.Pkg -> Phantoms.TTerm Syntax.NameData -> Phantoms.TTerm Syntax.Pkg
pkgWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.Pkg"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "ref"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.Pkg"),
              Core.projectionField = (Core.Name "ref")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "stats"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.Pkg"),
              Core.projectionField = (Core.Name "stats")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

pkgWithRef :: Phantoms.TTerm Syntax.Pkg -> Phantoms.TTerm Syntax.RefData -> Phantoms.TTerm Syntax.Pkg
pkgWithRef original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.Pkg"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.Pkg"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ref"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "stats"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.Pkg"),
              Core.projectionField = (Core.Name "stats")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

pkgWithStats :: Phantoms.TTerm Syntax.Pkg -> Phantoms.TTerm [Syntax.Stat] -> Phantoms.TTerm Syntax.Pkg
pkgWithStats original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.Pkg"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.Pkg"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ref"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.Pkg"),
              Core.projectionField = (Core.Name "ref")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "stats"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

objectPkg :: Phantoms.TTerm [Syntax.Mod] -> Phantoms.TTerm Syntax.NameData -> Phantoms.TTerm Syntax.Template -> Phantoms.TTerm Syntax.ObjectPkg
objectPkg mods name template =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ObjectPkg"),
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

objectPkgMods :: Phantoms.TTerm Syntax.ObjectPkg -> Phantoms.TTerm [Syntax.Mod]
objectPkgMods x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ObjectPkg"),
        Core.projectionField = (Core.Name "mods")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

objectPkgName :: Phantoms.TTerm Syntax.ObjectPkg -> Phantoms.TTerm Syntax.NameData
objectPkgName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ObjectPkg"),
        Core.projectionField = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

objectPkgTemplate :: Phantoms.TTerm Syntax.ObjectPkg -> Phantoms.TTerm Syntax.Template
objectPkgTemplate x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ObjectPkg"),
        Core.projectionField = (Core.Name "template")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

objectPkgWithMods :: Phantoms.TTerm Syntax.ObjectPkg -> Phantoms.TTerm [Syntax.Mod] -> Phantoms.TTerm Syntax.ObjectPkg
objectPkgWithMods original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ObjectPkg"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ObjectPkg"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "template"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ObjectPkg"),
              Core.projectionField = (Core.Name "template")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

objectPkgWithName :: Phantoms.TTerm Syntax.ObjectPkg -> Phantoms.TTerm Syntax.NameData -> Phantoms.TTerm Syntax.ObjectPkg
objectPkgWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ObjectPkg"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ObjectPkg"),
              Core.projectionField = (Core.Name "mods")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "template"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ObjectPkg"),
              Core.projectionField = (Core.Name "template")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

objectPkgWithTemplate :: Phantoms.TTerm Syntax.ObjectPkg -> Phantoms.TTerm Syntax.Template -> Phantoms.TTerm Syntax.ObjectPkg
objectPkgWithTemplate original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ObjectPkg"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ObjectPkg"),
              Core.projectionField = (Core.Name "mods")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ObjectPkg"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "template"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

predefString :: Phantoms.TTerm String -> Phantoms.TTerm Syntax.PredefString
predefString x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.scala.syntax.PredefString"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

refInit :: Phantoms.TTerm Syntax.Init -> Phantoms.TTerm Syntax.Ref
refInit x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Ref"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "init"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

refName :: Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.Ref
refName x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Ref"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "name"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

scalaSymbol :: Phantoms.TTerm String -> Phantoms.TTerm Syntax.ScalaSymbol
scalaSymbol name =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ScalaSymbol"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)}]}))

scalaSymbolName :: Phantoms.TTerm Syntax.ScalaSymbol -> Phantoms.TTerm String
scalaSymbolName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ScalaSymbol"),
        Core.projectionField = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

scalaSymbolWithName :: Phantoms.TTerm Syntax.ScalaSymbol -> Phantoms.TTerm String -> Phantoms.TTerm Syntax.ScalaSymbol
scalaSymbolWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ScalaSymbol"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

self :: Phantoms.TTerm () -> Phantoms.TTerm Syntax.Self
self x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.scala.syntax.Self"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

source :: Phantoms.TTerm [Syntax.Stat] -> Phantoms.TTerm Syntax.Source
source stats =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.Source"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "stats"),
          Core.fieldTerm = (Phantoms.unTTerm stats)}]}))

sourceStats :: Phantoms.TTerm Syntax.Source -> Phantoms.TTerm [Syntax.Stat]
sourceStats x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.Source"),
        Core.projectionField = (Core.Name "stats")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

sourceWithStats :: Phantoms.TTerm Syntax.Source -> Phantoms.TTerm [Syntax.Stat] -> Phantoms.TTerm Syntax.Source
sourceWithStats original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.Source"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "stats"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

statDecl :: Phantoms.TTerm Syntax.Decl -> Phantoms.TTerm Syntax.Stat
statDecl x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Stat"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "decl"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

statDefn :: Phantoms.TTerm Syntax.Defn -> Phantoms.TTerm Syntax.Stat
statDefn x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Stat"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "defn"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

statImportExport :: Phantoms.TTerm Syntax.ImportExportStat -> Phantoms.TTerm Syntax.Stat
statImportExport x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Stat"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "importExport"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

statTerm :: Phantoms.TTerm Syntax.Data -> Phantoms.TTerm Syntax.Stat
statTerm x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Stat"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "term"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

template :: Phantoms.TTerm [Syntax.Stat] -> Phantoms.TTerm [Syntax.Init] -> Phantoms.TTerm Syntax.Self -> Phantoms.TTerm [Syntax.Stat] -> Phantoms.TTerm Syntax.Template
template early inits self stats =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.Template"),
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
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.Template"),
        Core.projectionField = (Core.Name "early")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

templateInits :: Phantoms.TTerm Syntax.Template -> Phantoms.TTerm [Syntax.Init]
templateInits x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.Template"),
        Core.projectionField = (Core.Name "inits")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

templateSelf :: Phantoms.TTerm Syntax.Template -> Phantoms.TTerm Syntax.Self
templateSelf x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.Template"),
        Core.projectionField = (Core.Name "self")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

templateStats :: Phantoms.TTerm Syntax.Template -> Phantoms.TTerm [Syntax.Stat]
templateStats x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.Template"),
        Core.projectionField = (Core.Name "stats")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

templateWithEarly :: Phantoms.TTerm Syntax.Template -> Phantoms.TTerm [Syntax.Stat] -> Phantoms.TTerm Syntax.Template
templateWithEarly original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.Template"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "early"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "inits"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.Template"),
              Core.projectionField = (Core.Name "inits")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "self"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.Template"),
              Core.projectionField = (Core.Name "self")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "stats"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.Template"),
              Core.projectionField = (Core.Name "stats")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

templateWithInits :: Phantoms.TTerm Syntax.Template -> Phantoms.TTerm [Syntax.Init] -> Phantoms.TTerm Syntax.Template
templateWithInits original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.Template"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "early"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.Template"),
              Core.projectionField = (Core.Name "early")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inits"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "self"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.Template"),
              Core.projectionField = (Core.Name "self")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "stats"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.Template"),
              Core.projectionField = (Core.Name "stats")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

templateWithSelf :: Phantoms.TTerm Syntax.Template -> Phantoms.TTerm Syntax.Self -> Phantoms.TTerm Syntax.Template
templateWithSelf original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.Template"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "early"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.Template"),
              Core.projectionField = (Core.Name "early")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inits"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.Template"),
              Core.projectionField = (Core.Name "inits")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "self"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "stats"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.Template"),
              Core.projectionField = (Core.Name "stats")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

templateWithStats :: Phantoms.TTerm Syntax.Template -> Phantoms.TTerm [Syntax.Stat] -> Phantoms.TTerm Syntax.Template
templateWithStats original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.Template"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "early"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.Template"),
              Core.projectionField = (Core.Name "early")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inits"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.Template"),
              Core.projectionField = (Core.Name "inits")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "self"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.Template"),
              Core.projectionField = (Core.Name "self")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "stats"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

treeBounds :: Phantoms.TTerm Syntax.TypeBounds -> Phantoms.TTerm Syntax.Tree
treeBounds x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Tree"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "bounds"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

treeCaseTree :: Phantoms.TTerm Syntax.CaseTree -> Phantoms.TTerm Syntax.Tree
treeCaseTree x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Tree"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "caseTree"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

treeCtor :: Phantoms.TTerm Syntax.Ctor -> Phantoms.TTerm Syntax.Tree
treeCtor x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Tree"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "ctor"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

treeEnumerator :: Phantoms.TTerm Syntax.Enumerator -> Phantoms.TTerm Syntax.Tree
treeEnumerator x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Tree"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "enumerator"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

treeImportee :: Phantoms.TTerm Syntax.Importee -> Phantoms.TTerm Syntax.Tree
treeImportee x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Tree"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "importee"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

treeImporter :: Phantoms.TTerm Syntax.Importer -> Phantoms.TTerm Syntax.Tree
treeImporter x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Tree"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "importer"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

treeMember :: Phantoms.TTerm Syntax.Member -> Phantoms.TTerm Syntax.Tree
treeMember x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Tree"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "member"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

treeMod :: Phantoms.TTerm Syntax.Mod -> Phantoms.TTerm Syntax.Tree
treeMod x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Tree"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "mod"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

treePat :: Phantoms.TTerm Syntax.Pat -> Phantoms.TTerm Syntax.Tree
treePat x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Tree"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "pat"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

treeRef :: Phantoms.TTerm Syntax.Ref -> Phantoms.TTerm Syntax.Tree
treeRef x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Tree"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "ref"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

treeSource :: Phantoms.TTerm Syntax.Source -> Phantoms.TTerm Syntax.Tree
treeSource x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Tree"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "source"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

treeStat :: Phantoms.TTerm Syntax.Stat -> Phantoms.TTerm Syntax.Tree
treeStat x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Tree"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "stat"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

treeTemplate :: Phantoms.TTerm Syntax.Template -> Phantoms.TTerm Syntax.Tree
treeTemplate x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Tree"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "template"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

treeType :: Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.Tree
treeType x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Tree"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "type"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

typeAnd :: Phantoms.TTerm Syntax.AndType -> Phantoms.TTerm Syntax.Type
typeAnd x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "and"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

typeAnnotate :: Phantoms.TTerm Syntax.AnnotateType -> Phantoms.TTerm Syntax.Type
typeAnnotate x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "annotate"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

typeAnonymousName :: Phantoms.TTerm Syntax.AnonymousNameType -> Phantoms.TTerm Syntax.Type
typeAnonymousName x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "anonymousName"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

typeApply :: Phantoms.TTerm Syntax.ApplyType -> Phantoms.TTerm Syntax.Type
typeApply x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "apply"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

typeApplyInfix :: Phantoms.TTerm Syntax.ApplyInfixType -> Phantoms.TTerm Syntax.Type
typeApplyInfix x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "applyInfix"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

typeBounds :: Phantoms.TTerm (Maybe Syntax.Type) -> Phantoms.TTerm (Maybe Syntax.Type) -> Phantoms.TTerm Syntax.TypeBounds
typeBounds lo hi =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.TypeBounds"),
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
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TypeBounds"),
        Core.projectionField = (Core.Name "hi")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

typeBoundsLo :: Phantoms.TTerm Syntax.TypeBounds -> Phantoms.TTerm (Maybe Syntax.Type)
typeBoundsLo x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TypeBounds"),
        Core.projectionField = (Core.Name "lo")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

typeBoundsWithHi :: Phantoms.TTerm Syntax.TypeBounds -> Phantoms.TTerm (Maybe Syntax.Type) -> Phantoms.TTerm Syntax.TypeBounds
typeBoundsWithHi original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.TypeBounds"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lo"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TypeBounds"),
              Core.projectionField = (Core.Name "lo")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "hi"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

typeBoundsWithLo :: Phantoms.TTerm Syntax.TypeBounds -> Phantoms.TTerm (Maybe Syntax.Type) -> Phantoms.TTerm Syntax.TypeBounds
typeBoundsWithLo original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.TypeBounds"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lo"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "hi"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TypeBounds"),
              Core.projectionField = (Core.Name "hi")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

typeByName :: Phantoms.TTerm Syntax.ByNameType -> Phantoms.TTerm Syntax.Type
typeByName x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "byName"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

typeCase :: Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.TypeCase
typeCase pat body =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.TypeCase"),
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
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TypeCase"),
        Core.projectionField = (Core.Name "body")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

typeCasePat :: Phantoms.TTerm Syntax.TypeCase -> Phantoms.TTerm Syntax.Type
typeCasePat x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TypeCase"),
        Core.projectionField = (Core.Name "pat")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

typeCaseWithBody :: Phantoms.TTerm Syntax.TypeCase -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.TypeCase
typeCaseWithBody original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.TypeCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pat"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TypeCase"),
              Core.projectionField = (Core.Name "pat")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

typeCaseWithPat :: Phantoms.TTerm Syntax.TypeCase -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.TypeCase
typeCaseWithPat original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.TypeCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pat"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TypeCase"),
              Core.projectionField = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

typeExistential :: Phantoms.TTerm Syntax.ExistentialType -> Phantoms.TTerm Syntax.Type
typeExistential x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "existential"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

typeFunction :: Phantoms.TTerm Syntax.FunctionType -> Phantoms.TTerm Syntax.Type
typeFunction x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "function"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

typeContextFunction :: Phantoms.TTerm Syntax.ContextFunctionType -> Phantoms.TTerm Syntax.Type
typeContextFunction x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "contextFunction"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

typeImplicitFunction :: Phantoms.TTerm Syntax.ImplicitFunctionType -> Phantoms.TTerm Syntax.Type
typeImplicitFunction x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "implicitFunction"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

typeLambda :: Phantoms.TTerm Syntax.LambdaType -> Phantoms.TTerm Syntax.Type
typeLambda x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "lambda"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

typeMatch :: Phantoms.TTerm Syntax.MatchType -> Phantoms.TTerm Syntax.Type
typeMatch x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "match"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

typeMethod :: Phantoms.TTerm Syntax.MethodType -> Phantoms.TTerm Syntax.Type
typeMethod x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "method"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

typeOr :: Phantoms.TTerm Syntax.OrType -> Phantoms.TTerm Syntax.Type
typeOr x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "or"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

typePlaceholder :: Phantoms.TTerm Syntax.PlaceholderType -> Phantoms.TTerm Syntax.Type
typePlaceholder x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "placeholder"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

typePolyFunction :: Phantoms.TTerm Syntax.PolyFunctionType -> Phantoms.TTerm Syntax.Type
typePolyFunction x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "polyFunction"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

typeRef :: Phantoms.TTerm Syntax.RefType -> Phantoms.TTerm Syntax.Type
typeRef x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "ref"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

typeRefine :: Phantoms.TTerm Syntax.RefineType -> Phantoms.TTerm Syntax.Type
typeRefine x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "refine"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

typeRepeated :: Phantoms.TTerm Syntax.RepeatedType -> Phantoms.TTerm Syntax.Type
typeRepeated x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "repeated"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

typeTuple :: Phantoms.TTerm Syntax.TupleType -> Phantoms.TTerm Syntax.Type
typeTuple x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "tuple"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

typeTypedParam :: Phantoms.TTerm Syntax.TypedParamType -> Phantoms.TTerm Syntax.Type
typeTypedParam x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "typedParam"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

typeVar :: Phantoms.TTerm Syntax.VarType -> Phantoms.TTerm Syntax.Type
typeVar x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "var"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

typeWith :: Phantoms.TTerm Syntax.WithType -> Phantoms.TTerm Syntax.Type
typeWith x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "with"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

andType :: Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.AndType
andType lhs rhs =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.AndType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Phantoms.unTTerm lhs)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTTerm rhs)}]}))

andTypeLhs :: Phantoms.TTerm Syntax.AndType -> Phantoms.TTerm Syntax.Type
andTypeLhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.AndType"),
        Core.projectionField = (Core.Name "lhs")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

andTypeRhs :: Phantoms.TTerm Syntax.AndType -> Phantoms.TTerm Syntax.Type
andTypeRhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.AndType"),
        Core.projectionField = (Core.Name "rhs")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

andTypeWithLhs :: Phantoms.TTerm Syntax.AndType -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.AndType
andTypeWithLhs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.AndType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.AndType"),
              Core.projectionField = (Core.Name "rhs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

andTypeWithRhs :: Phantoms.TTerm Syntax.AndType -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.AndType
andTypeWithRhs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.AndType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.AndType"),
              Core.projectionField = (Core.Name "lhs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

annotateType :: Phantoms.TTerm Syntax.Type -> Phantoms.TTerm [Syntax.AnnotMod] -> Phantoms.TTerm Syntax.AnnotateType
annotateType tpe annots =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.AnnotateType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "tpe"),
          Core.fieldTerm = (Phantoms.unTTerm tpe)},
        Core.Field {
          Core.fieldName = (Core.Name "annots"),
          Core.fieldTerm = (Phantoms.unTTerm annots)}]}))

annotateTypeAnnots :: Phantoms.TTerm Syntax.AnnotateType -> Phantoms.TTerm [Syntax.AnnotMod]
annotateTypeAnnots x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.AnnotateType"),
        Core.projectionField = (Core.Name "annots")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

annotateTypeTpe :: Phantoms.TTerm Syntax.AnnotateType -> Phantoms.TTerm Syntax.Type
annotateTypeTpe x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.AnnotateType"),
        Core.projectionField = (Core.Name "tpe")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

annotateTypeWithAnnots :: Phantoms.TTerm Syntax.AnnotateType -> Phantoms.TTerm [Syntax.AnnotMod] -> Phantoms.TTerm Syntax.AnnotateType
annotateTypeWithAnnots original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.AnnotateType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "tpe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.AnnotateType"),
              Core.projectionField = (Core.Name "tpe")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "annots"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

annotateTypeWithTpe :: Phantoms.TTerm Syntax.AnnotateType -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.AnnotateType
annotateTypeWithTpe original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.AnnotateType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "tpe"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "annots"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.AnnotateType"),
              Core.projectionField = (Core.Name "annots")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

anonymousNameType :: Phantoms.TTerm () -> Phantoms.TTerm Syntax.AnonymousNameType
anonymousNameType x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.scala.syntax.AnonymousNameType"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

applyType :: Phantoms.TTerm Syntax.Type -> Phantoms.TTerm [Syntax.Type] -> Phantoms.TTerm Syntax.ApplyType
applyType tpe args =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ApplyType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "tpe"),
          Core.fieldTerm = (Phantoms.unTTerm tpe)},
        Core.Field {
          Core.fieldName = (Core.Name "args"),
          Core.fieldTerm = (Phantoms.unTTerm args)}]}))

applyTypeArgs :: Phantoms.TTerm Syntax.ApplyType -> Phantoms.TTerm [Syntax.Type]
applyTypeArgs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ApplyType"),
        Core.projectionField = (Core.Name "args")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

applyInfixType :: Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.NameType -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.ApplyInfixType
applyInfixType lhs op rhs =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ApplyInfixType"),
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

applyInfixTypeLhs :: Phantoms.TTerm Syntax.ApplyInfixType -> Phantoms.TTerm Syntax.Type
applyInfixTypeLhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ApplyInfixType"),
        Core.projectionField = (Core.Name "lhs")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

applyInfixTypeOp :: Phantoms.TTerm Syntax.ApplyInfixType -> Phantoms.TTerm Syntax.NameType
applyInfixTypeOp x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ApplyInfixType"),
        Core.projectionField = (Core.Name "op")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

applyInfixTypeRhs :: Phantoms.TTerm Syntax.ApplyInfixType -> Phantoms.TTerm Syntax.Type
applyInfixTypeRhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ApplyInfixType"),
        Core.projectionField = (Core.Name "rhs")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

applyInfixTypeWithLhs :: Phantoms.TTerm Syntax.ApplyInfixType -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.ApplyInfixType
applyInfixTypeWithLhs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ApplyInfixType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "op"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ApplyInfixType"),
              Core.projectionField = (Core.Name "op")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ApplyInfixType"),
              Core.projectionField = (Core.Name "rhs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

applyInfixTypeWithOp :: Phantoms.TTerm Syntax.ApplyInfixType -> Phantoms.TTerm Syntax.NameType -> Phantoms.TTerm Syntax.ApplyInfixType
applyInfixTypeWithOp original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ApplyInfixType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ApplyInfixType"),
              Core.projectionField = (Core.Name "lhs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "op"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ApplyInfixType"),
              Core.projectionField = (Core.Name "rhs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

applyInfixTypeWithRhs :: Phantoms.TTerm Syntax.ApplyInfixType -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.ApplyInfixType
applyInfixTypeWithRhs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ApplyInfixType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ApplyInfixType"),
              Core.projectionField = (Core.Name "lhs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "op"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ApplyInfixType"),
              Core.projectionField = (Core.Name "op")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

applyTypeTpe :: Phantoms.TTerm Syntax.ApplyType -> Phantoms.TTerm Syntax.Type
applyTypeTpe x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ApplyType"),
        Core.projectionField = (Core.Name "tpe")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

applyTypeWithArgs :: Phantoms.TTerm Syntax.ApplyType -> Phantoms.TTerm [Syntax.Type] -> Phantoms.TTerm Syntax.ApplyType
applyTypeWithArgs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ApplyType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "tpe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ApplyType"),
              Core.projectionField = (Core.Name "tpe")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "args"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

applyTypeWithTpe :: Phantoms.TTerm Syntax.ApplyType -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.ApplyType
applyTypeWithTpe original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ApplyType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "tpe"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "args"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ApplyType"),
              Core.projectionField = (Core.Name "args")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

byNameType :: Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.ByNameType
byNameType tpe =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ByNameType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "tpe"),
          Core.fieldTerm = (Phantoms.unTTerm tpe)}]}))

byNameTypeTpe :: Phantoms.TTerm Syntax.ByNameType -> Phantoms.TTerm Syntax.Type
byNameTypeTpe x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ByNameType"),
        Core.projectionField = (Core.Name "tpe")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

byNameTypeWithTpe :: Phantoms.TTerm Syntax.ByNameType -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.ByNameType
byNameTypeWithTpe original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ByNameType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "tpe"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

contextFunctionType :: Phantoms.TTerm [Syntax.Type] -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.ContextFunctionType
contextFunctionType params res =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ContextFunctionType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Phantoms.unTTerm params)},
        Core.Field {
          Core.fieldName = (Core.Name "res"),
          Core.fieldTerm = (Phantoms.unTTerm res)}]}))

contextFunctionTypeParams :: Phantoms.TTerm Syntax.ContextFunctionType -> Phantoms.TTerm [Syntax.Type]
contextFunctionTypeParams x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ContextFunctionType"),
        Core.projectionField = (Core.Name "params")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

contextFunctionTypeRes :: Phantoms.TTerm Syntax.ContextFunctionType -> Phantoms.TTerm Syntax.Type
contextFunctionTypeRes x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ContextFunctionType"),
        Core.projectionField = (Core.Name "res")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

contextFunctionTypeWithParams :: Phantoms.TTerm Syntax.ContextFunctionType -> Phantoms.TTerm [Syntax.Type] -> Phantoms.TTerm Syntax.ContextFunctionType
contextFunctionTypeWithParams original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ContextFunctionType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "res"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ContextFunctionType"),
              Core.projectionField = (Core.Name "res")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

contextFunctionTypeWithRes :: Phantoms.TTerm Syntax.ContextFunctionType -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.ContextFunctionType
contextFunctionTypeWithRes original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ContextFunctionType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ContextFunctionType"),
              Core.projectionField = (Core.Name "params")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "res"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

existentialType :: Phantoms.TTerm Syntax.Type -> Phantoms.TTerm [Syntax.Stat] -> Phantoms.TTerm Syntax.ExistentialType
existentialType tpe stats =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ExistentialType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "tpe"),
          Core.fieldTerm = (Phantoms.unTTerm tpe)},
        Core.Field {
          Core.fieldName = (Core.Name "stats"),
          Core.fieldTerm = (Phantoms.unTTerm stats)}]}))

existentialTypeStats :: Phantoms.TTerm Syntax.ExistentialType -> Phantoms.TTerm [Syntax.Stat]
existentialTypeStats x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ExistentialType"),
        Core.projectionField = (Core.Name "stats")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

existentialTypeTpe :: Phantoms.TTerm Syntax.ExistentialType -> Phantoms.TTerm Syntax.Type
existentialTypeTpe x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ExistentialType"),
        Core.projectionField = (Core.Name "tpe")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

existentialTypeWithStats :: Phantoms.TTerm Syntax.ExistentialType -> Phantoms.TTerm [Syntax.Stat] -> Phantoms.TTerm Syntax.ExistentialType
existentialTypeWithStats original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ExistentialType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "tpe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ExistentialType"),
              Core.projectionField = (Core.Name "tpe")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "stats"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

existentialTypeWithTpe :: Phantoms.TTerm Syntax.ExistentialType -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.ExistentialType
existentialTypeWithTpe original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ExistentialType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "tpe"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "stats"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ExistentialType"),
              Core.projectionField = (Core.Name "stats")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

functionType :: Phantoms.TTerm [Syntax.Type] -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.FunctionType
functionType params res =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.FunctionType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Phantoms.unTTerm params)},
        Core.Field {
          Core.fieldName = (Core.Name "res"),
          Core.fieldTerm = (Phantoms.unTTerm res)}]}))

functionTypeParams :: Phantoms.TTerm Syntax.FunctionType -> Phantoms.TTerm [Syntax.Type]
functionTypeParams x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.FunctionType"),
        Core.projectionField = (Core.Name "params")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

functionTypeRes :: Phantoms.TTerm Syntax.FunctionType -> Phantoms.TTerm Syntax.Type
functionTypeRes x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.FunctionType"),
        Core.projectionField = (Core.Name "res")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

functionTypeWithParams :: Phantoms.TTerm Syntax.FunctionType -> Phantoms.TTerm [Syntax.Type] -> Phantoms.TTerm Syntax.FunctionType
functionTypeWithParams original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.FunctionType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "res"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.FunctionType"),
              Core.projectionField = (Core.Name "res")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

functionTypeWithRes :: Phantoms.TTerm Syntax.FunctionType -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.FunctionType
functionTypeWithRes original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.FunctionType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.FunctionType"),
              Core.projectionField = (Core.Name "params")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "res"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

implicitFunctionType :: Phantoms.TTerm [Syntax.Type] -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.ImplicitFunctionType
implicitFunctionType params res =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ImplicitFunctionType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Phantoms.unTTerm params)},
        Core.Field {
          Core.fieldName = (Core.Name "res"),
          Core.fieldTerm = (Phantoms.unTTerm res)}]}))

implicitFunctionTypeParams :: Phantoms.TTerm Syntax.ImplicitFunctionType -> Phantoms.TTerm [Syntax.Type]
implicitFunctionTypeParams x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ImplicitFunctionType"),
        Core.projectionField = (Core.Name "params")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

implicitFunctionTypeRes :: Phantoms.TTerm Syntax.ImplicitFunctionType -> Phantoms.TTerm Syntax.Type
implicitFunctionTypeRes x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ImplicitFunctionType"),
        Core.projectionField = (Core.Name "res")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

implicitFunctionTypeWithParams :: Phantoms.TTerm Syntax.ImplicitFunctionType -> Phantoms.TTerm [Syntax.Type] -> Phantoms.TTerm Syntax.ImplicitFunctionType
implicitFunctionTypeWithParams original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ImplicitFunctionType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "res"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ImplicitFunctionType"),
              Core.projectionField = (Core.Name "res")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

implicitFunctionTypeWithRes :: Phantoms.TTerm Syntax.ImplicitFunctionType -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.ImplicitFunctionType
implicitFunctionTypeWithRes original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ImplicitFunctionType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ImplicitFunctionType"),
              Core.projectionField = (Core.Name "params")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "res"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

lambdaType :: Phantoms.TTerm [Syntax.ParamType] -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.LambdaType
lambdaType tparams tpe =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.LambdaType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Phantoms.unTTerm tparams)},
        Core.Field {
          Core.fieldName = (Core.Name "tpe"),
          Core.fieldTerm = (Phantoms.unTTerm tpe)}]}))

lambdaTypeTparams :: Phantoms.TTerm Syntax.LambdaType -> Phantoms.TTerm [Syntax.ParamType]
lambdaTypeTparams x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.LambdaType"),
        Core.projectionField = (Core.Name "tparams")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

lambdaTypeTpe :: Phantoms.TTerm Syntax.LambdaType -> Phantoms.TTerm Syntax.Type
lambdaTypeTpe x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.LambdaType"),
        Core.projectionField = (Core.Name "tpe")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

lambdaTypeWithTparams :: Phantoms.TTerm Syntax.LambdaType -> Phantoms.TTerm [Syntax.ParamType] -> Phantoms.TTerm Syntax.LambdaType
lambdaTypeWithTparams original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.LambdaType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "tpe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.LambdaType"),
              Core.projectionField = (Core.Name "tpe")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

lambdaTypeWithTpe :: Phantoms.TTerm Syntax.LambdaType -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.LambdaType
lambdaTypeWithTpe original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.LambdaType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.LambdaType"),
              Core.projectionField = (Core.Name "tparams")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tpe"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

matchType :: Phantoms.TTerm Syntax.Type -> Phantoms.TTerm [Syntax.TypeCase] -> Phantoms.TTerm Syntax.MatchType
matchType tpe cases =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.MatchType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "tpe"),
          Core.fieldTerm = (Phantoms.unTTerm tpe)},
        Core.Field {
          Core.fieldName = (Core.Name "cases"),
          Core.fieldTerm = (Phantoms.unTTerm cases)}]}))

matchTypeCases :: Phantoms.TTerm Syntax.MatchType -> Phantoms.TTerm [Syntax.TypeCase]
matchTypeCases x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.MatchType"),
        Core.projectionField = (Core.Name "cases")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

matchTypeTpe :: Phantoms.TTerm Syntax.MatchType -> Phantoms.TTerm Syntax.Type
matchTypeTpe x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.MatchType"),
        Core.projectionField = (Core.Name "tpe")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

matchTypeWithCases :: Phantoms.TTerm Syntax.MatchType -> Phantoms.TTerm [Syntax.TypeCase] -> Phantoms.TTerm Syntax.MatchType
matchTypeWithCases original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.MatchType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "tpe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.MatchType"),
              Core.projectionField = (Core.Name "tpe")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "cases"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

matchTypeWithTpe :: Phantoms.TTerm Syntax.MatchType -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.MatchType
matchTypeWithTpe original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.MatchType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "tpe"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "cases"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.MatchType"),
              Core.projectionField = (Core.Name "cases")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

methodType :: Phantoms.TTerm [[Syntax.ParamData]] -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.MethodType
methodType paramss tpe =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.MethodType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "paramss"),
          Core.fieldTerm = (Phantoms.unTTerm paramss)},
        Core.Field {
          Core.fieldName = (Core.Name "tpe"),
          Core.fieldTerm = (Phantoms.unTTerm tpe)}]}))

methodTypeParamss :: Phantoms.TTerm Syntax.MethodType -> Phantoms.TTerm [[Syntax.ParamData]]
methodTypeParamss x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.MethodType"),
        Core.projectionField = (Core.Name "paramss")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

methodTypeTpe :: Phantoms.TTerm Syntax.MethodType -> Phantoms.TTerm Syntax.Type
methodTypeTpe x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.MethodType"),
        Core.projectionField = (Core.Name "tpe")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

methodTypeWithParamss :: Phantoms.TTerm Syntax.MethodType -> Phantoms.TTerm [[Syntax.ParamData]] -> Phantoms.TTerm Syntax.MethodType
methodTypeWithParamss original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.MethodType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "paramss"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "tpe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.MethodType"),
              Core.projectionField = (Core.Name "tpe")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

methodTypeWithTpe :: Phantoms.TTerm Syntax.MethodType -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.MethodType
methodTypeWithTpe original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.MethodType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "paramss"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.MethodType"),
              Core.projectionField = (Core.Name "paramss")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tpe"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

nameType :: Phantoms.TTerm String -> Phantoms.TTerm Syntax.NameType
nameType value =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.NameType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm value)}]}))

nameTypeValue :: Phantoms.TTerm Syntax.NameType -> Phantoms.TTerm String
nameTypeValue x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.NameType"),
        Core.projectionField = (Core.Name "value")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

nameTypeWithValue :: Phantoms.TTerm Syntax.NameType -> Phantoms.TTerm String -> Phantoms.TTerm Syntax.NameType
nameTypeWithValue original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.NameType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

orType :: Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.OrType
orType lhs rhs =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.OrType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Phantoms.unTTerm lhs)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTTerm rhs)}]}))

orTypeLhs :: Phantoms.TTerm Syntax.OrType -> Phantoms.TTerm Syntax.Type
orTypeLhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.OrType"),
        Core.projectionField = (Core.Name "lhs")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

orTypeRhs :: Phantoms.TTerm Syntax.OrType -> Phantoms.TTerm Syntax.Type
orTypeRhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.OrType"),
        Core.projectionField = (Core.Name "rhs")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

orTypeWithLhs :: Phantoms.TTerm Syntax.OrType -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.OrType
orTypeWithLhs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.OrType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.OrType"),
              Core.projectionField = (Core.Name "rhs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

orTypeWithRhs :: Phantoms.TTerm Syntax.OrType -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.OrType
orTypeWithRhs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.OrType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.OrType"),
              Core.projectionField = (Core.Name "lhs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

paramType :: Phantoms.TTerm [Syntax.Mod] -> Phantoms.TTerm Syntax.Name -> Phantoms.TTerm [Syntax.ParamType] -> Phantoms.TTerm [Syntax.TypeBounds] -> Phantoms.TTerm [Syntax.Type] -> Phantoms.TTerm [Syntax.Type] -> Phantoms.TTerm Syntax.ParamType
paramType mods name tparams tbounds vbounds cbounds =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ParamType"),
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

paramTypeCbounds :: Phantoms.TTerm Syntax.ParamType -> Phantoms.TTerm [Syntax.Type]
paramTypeCbounds x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ParamType"),
        Core.projectionField = (Core.Name "cbounds")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

paramTypeMods :: Phantoms.TTerm Syntax.ParamType -> Phantoms.TTerm [Syntax.Mod]
paramTypeMods x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ParamType"),
        Core.projectionField = (Core.Name "mods")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

paramTypeName :: Phantoms.TTerm Syntax.ParamType -> Phantoms.TTerm Syntax.Name
paramTypeName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ParamType"),
        Core.projectionField = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

paramTypeTbounds :: Phantoms.TTerm Syntax.ParamType -> Phantoms.TTerm [Syntax.TypeBounds]
paramTypeTbounds x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ParamType"),
        Core.projectionField = (Core.Name "tbounds")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

paramTypeTparams :: Phantoms.TTerm Syntax.ParamType -> Phantoms.TTerm [Syntax.ParamType]
paramTypeTparams x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ParamType"),
        Core.projectionField = (Core.Name "tparams")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

paramTypeVbounds :: Phantoms.TTerm Syntax.ParamType -> Phantoms.TTerm [Syntax.Type]
paramTypeVbounds x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ParamType"),
        Core.projectionField = (Core.Name "vbounds")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

paramTypeWithCbounds :: Phantoms.TTerm Syntax.ParamType -> Phantoms.TTerm [Syntax.Type] -> Phantoms.TTerm Syntax.ParamType
paramTypeWithCbounds original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ParamType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ParamType"),
              Core.projectionField = (Core.Name "mods")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ParamType"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ParamType"),
              Core.projectionField = (Core.Name "tparams")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tbounds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ParamType"),
              Core.projectionField = (Core.Name "tbounds")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "vbounds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ParamType"),
              Core.projectionField = (Core.Name "vbounds")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "cbounds"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

paramTypeWithMods :: Phantoms.TTerm Syntax.ParamType -> Phantoms.TTerm [Syntax.Mod] -> Phantoms.TTerm Syntax.ParamType
paramTypeWithMods original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ParamType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ParamType"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ParamType"),
              Core.projectionField = (Core.Name "tparams")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tbounds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ParamType"),
              Core.projectionField = (Core.Name "tbounds")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "vbounds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ParamType"),
              Core.projectionField = (Core.Name "vbounds")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "cbounds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ParamType"),
              Core.projectionField = (Core.Name "cbounds")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

paramTypeWithName :: Phantoms.TTerm Syntax.ParamType -> Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.ParamType
paramTypeWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ParamType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ParamType"),
              Core.projectionField = (Core.Name "mods")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ParamType"),
              Core.projectionField = (Core.Name "tparams")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tbounds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ParamType"),
              Core.projectionField = (Core.Name "tbounds")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "vbounds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ParamType"),
              Core.projectionField = (Core.Name "vbounds")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "cbounds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ParamType"),
              Core.projectionField = (Core.Name "cbounds")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

paramTypeWithTbounds :: Phantoms.TTerm Syntax.ParamType -> Phantoms.TTerm [Syntax.TypeBounds] -> Phantoms.TTerm Syntax.ParamType
paramTypeWithTbounds original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ParamType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ParamType"),
              Core.projectionField = (Core.Name "mods")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ParamType"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ParamType"),
              Core.projectionField = (Core.Name "tparams")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tbounds"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "vbounds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ParamType"),
              Core.projectionField = (Core.Name "vbounds")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "cbounds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ParamType"),
              Core.projectionField = (Core.Name "cbounds")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

paramTypeWithTparams :: Phantoms.TTerm Syntax.ParamType -> Phantoms.TTerm [Syntax.ParamType] -> Phantoms.TTerm Syntax.ParamType
paramTypeWithTparams original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ParamType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ParamType"),
              Core.projectionField = (Core.Name "mods")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ParamType"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "tbounds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ParamType"),
              Core.projectionField = (Core.Name "tbounds")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "vbounds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ParamType"),
              Core.projectionField = (Core.Name "vbounds")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "cbounds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ParamType"),
              Core.projectionField = (Core.Name "cbounds")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

paramTypeWithVbounds :: Phantoms.TTerm Syntax.ParamType -> Phantoms.TTerm [Syntax.Type] -> Phantoms.TTerm Syntax.ParamType
paramTypeWithVbounds original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ParamType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ParamType"),
              Core.projectionField = (Core.Name "mods")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ParamType"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ParamType"),
              Core.projectionField = (Core.Name "tparams")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tbounds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ParamType"),
              Core.projectionField = (Core.Name "tbounds")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "vbounds"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "cbounds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ParamType"),
              Core.projectionField = (Core.Name "cbounds")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

placeholderType :: Phantoms.TTerm Syntax.TypeBounds -> Phantoms.TTerm Syntax.PlaceholderType
placeholderType bounds =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.PlaceholderType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "bounds"),
          Core.fieldTerm = (Phantoms.unTTerm bounds)}]}))

placeholderTypeBounds :: Phantoms.TTerm Syntax.PlaceholderType -> Phantoms.TTerm Syntax.TypeBounds
placeholderTypeBounds x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.PlaceholderType"),
        Core.projectionField = (Core.Name "bounds")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

placeholderTypeWithBounds :: Phantoms.TTerm Syntax.PlaceholderType -> Phantoms.TTerm Syntax.TypeBounds -> Phantoms.TTerm Syntax.PlaceholderType
placeholderTypeWithBounds original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.PlaceholderType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "bounds"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

polyFunctionType :: Phantoms.TTerm [Syntax.ParamType] -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.PolyFunctionType
polyFunctionType tparams tpe =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.PolyFunctionType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Phantoms.unTTerm tparams)},
        Core.Field {
          Core.fieldName = (Core.Name "tpe"),
          Core.fieldTerm = (Phantoms.unTTerm tpe)}]}))

polyFunctionTypeTparams :: Phantoms.TTerm Syntax.PolyFunctionType -> Phantoms.TTerm [Syntax.ParamType]
polyFunctionTypeTparams x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.PolyFunctionType"),
        Core.projectionField = (Core.Name "tparams")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

polyFunctionTypeTpe :: Phantoms.TTerm Syntax.PolyFunctionType -> Phantoms.TTerm Syntax.Type
polyFunctionTypeTpe x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.PolyFunctionType"),
        Core.projectionField = (Core.Name "tpe")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

polyFunctionTypeWithTparams :: Phantoms.TTerm Syntax.PolyFunctionType -> Phantoms.TTerm [Syntax.ParamType] -> Phantoms.TTerm Syntax.PolyFunctionType
polyFunctionTypeWithTparams original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.PolyFunctionType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "tpe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.PolyFunctionType"),
              Core.projectionField = (Core.Name "tpe")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

polyFunctionTypeWithTpe :: Phantoms.TTerm Syntax.PolyFunctionType -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.PolyFunctionType
polyFunctionTypeWithTpe original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.PolyFunctionType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.PolyFunctionType"),
              Core.projectionField = (Core.Name "tparams")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tpe"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

projectType :: Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.NameType -> Phantoms.TTerm Syntax.ProjectType
projectType qual name =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ProjectType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "qual"),
          Core.fieldTerm = (Phantoms.unTTerm qual)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)}]}))

projectTypeName :: Phantoms.TTerm Syntax.ProjectType -> Phantoms.TTerm Syntax.NameType
projectTypeName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ProjectType"),
        Core.projectionField = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

projectTypeQual :: Phantoms.TTerm Syntax.ProjectType -> Phantoms.TTerm Syntax.Type
projectTypeQual x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ProjectType"),
        Core.projectionField = (Core.Name "qual")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

projectTypeWithName :: Phantoms.TTerm Syntax.ProjectType -> Phantoms.TTerm Syntax.NameType -> Phantoms.TTerm Syntax.ProjectType
projectTypeWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ProjectType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "qual"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ProjectType"),
              Core.projectionField = (Core.Name "qual")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

projectTypeWithQual :: Phantoms.TTerm Syntax.ProjectType -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.ProjectType
projectTypeWithQual original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ProjectType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "qual"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ProjectType"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

refTypeName :: Phantoms.TTerm Syntax.NameType -> Phantoms.TTerm Syntax.RefType
refTypeName x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.RefType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "name"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

refTypeProject :: Phantoms.TTerm Syntax.ProjectType -> Phantoms.TTerm Syntax.RefType
refTypeProject x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.RefType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "project"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

refTypeSelect :: Phantoms.TTerm Syntax.SelectType -> Phantoms.TTerm Syntax.RefType
refTypeSelect x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.RefType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "select"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

refTypeSingleton :: Phantoms.TTerm Syntax.SingletonType -> Phantoms.TTerm Syntax.RefType
refTypeSingleton x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.RefType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "singleton"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

refineType :: Phantoms.TTerm (Maybe Syntax.Type) -> Phantoms.TTerm [Syntax.Stat] -> Phantoms.TTerm Syntax.RefineType
refineType tpe stats =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.RefineType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "tpe"),
          Core.fieldTerm = (Phantoms.unTTerm tpe)},
        Core.Field {
          Core.fieldName = (Core.Name "stats"),
          Core.fieldTerm = (Phantoms.unTTerm stats)}]}))

refineTypeStats :: Phantoms.TTerm Syntax.RefineType -> Phantoms.TTerm [Syntax.Stat]
refineTypeStats x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.RefineType"),
        Core.projectionField = (Core.Name "stats")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

refineTypeTpe :: Phantoms.TTerm Syntax.RefineType -> Phantoms.TTerm (Maybe Syntax.Type)
refineTypeTpe x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.RefineType"),
        Core.projectionField = (Core.Name "tpe")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

refineTypeWithStats :: Phantoms.TTerm Syntax.RefineType -> Phantoms.TTerm [Syntax.Stat] -> Phantoms.TTerm Syntax.RefineType
refineTypeWithStats original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.RefineType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "tpe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.RefineType"),
              Core.projectionField = (Core.Name "tpe")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "stats"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

refineTypeWithTpe :: Phantoms.TTerm Syntax.RefineType -> Phantoms.TTerm (Maybe Syntax.Type) -> Phantoms.TTerm Syntax.RefineType
refineTypeWithTpe original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.RefineType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "tpe"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "stats"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.RefineType"),
              Core.projectionField = (Core.Name "stats")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

repeatedType :: Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.RepeatedType
repeatedType tpe =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.RepeatedType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "tpe"),
          Core.fieldTerm = (Phantoms.unTTerm tpe)}]}))

repeatedTypeTpe :: Phantoms.TTerm Syntax.RepeatedType -> Phantoms.TTerm Syntax.Type
repeatedTypeTpe x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.RepeatedType"),
        Core.projectionField = (Core.Name "tpe")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

repeatedTypeWithTpe :: Phantoms.TTerm Syntax.RepeatedType -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.RepeatedType
repeatedTypeWithTpe original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.RepeatedType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "tpe"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

selectType :: Phantoms.TTerm Syntax.RefData -> Phantoms.TTerm Syntax.NameType -> Phantoms.TTerm Syntax.SelectType
selectType qual name =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.SelectType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "qual"),
          Core.fieldTerm = (Phantoms.unTTerm qual)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)}]}))

selectTypeName :: Phantoms.TTerm Syntax.SelectType -> Phantoms.TTerm Syntax.NameType
selectTypeName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.SelectType"),
        Core.projectionField = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

selectTypeQual :: Phantoms.TTerm Syntax.SelectType -> Phantoms.TTerm Syntax.RefData
selectTypeQual x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.SelectType"),
        Core.projectionField = (Core.Name "qual")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

selectTypeWithName :: Phantoms.TTerm Syntax.SelectType -> Phantoms.TTerm Syntax.NameType -> Phantoms.TTerm Syntax.SelectType
selectTypeWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.SelectType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "qual"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.SelectType"),
              Core.projectionField = (Core.Name "qual")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

selectTypeWithQual :: Phantoms.TTerm Syntax.SelectType -> Phantoms.TTerm Syntax.RefData -> Phantoms.TTerm Syntax.SelectType
selectTypeWithQual original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.SelectType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "qual"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.SelectType"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

singletonType :: Phantoms.TTerm Syntax.RefData -> Phantoms.TTerm Syntax.SingletonType
singletonType ref =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.SingletonType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "ref"),
          Core.fieldTerm = (Phantoms.unTTerm ref)}]}))

singletonTypeRef :: Phantoms.TTerm Syntax.SingletonType -> Phantoms.TTerm Syntax.RefData
singletonTypeRef x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.SingletonType"),
        Core.projectionField = (Core.Name "ref")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

singletonTypeWithRef :: Phantoms.TTerm Syntax.SingletonType -> Phantoms.TTerm Syntax.RefData -> Phantoms.TTerm Syntax.SingletonType
singletonTypeWithRef original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.SingletonType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "ref"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

tupleType :: Phantoms.TTerm [Syntax.Type] -> Phantoms.TTerm Syntax.TupleType
tupleType args =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.TupleType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "args"),
          Core.fieldTerm = (Phantoms.unTTerm args)}]}))

tupleTypeArgs :: Phantoms.TTerm Syntax.TupleType -> Phantoms.TTerm [Syntax.Type]
tupleTypeArgs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TupleType"),
        Core.projectionField = (Core.Name "args")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

tupleTypeWithArgs :: Phantoms.TTerm Syntax.TupleType -> Phantoms.TTerm [Syntax.Type] -> Phantoms.TTerm Syntax.TupleType
tupleTypeWithArgs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.TupleType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "args"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

typedParamType :: Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.TypedParamType
typedParamType name typ =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.TypedParamType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "typ"),
          Core.fieldTerm = (Phantoms.unTTerm typ)}]}))

typedParamTypeName :: Phantoms.TTerm Syntax.TypedParamType -> Phantoms.TTerm Syntax.Name
typedParamTypeName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TypedParamType"),
        Core.projectionField = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

typedParamTypeTyp :: Phantoms.TTerm Syntax.TypedParamType -> Phantoms.TTerm Syntax.Type
typedParamTypeTyp x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TypedParamType"),
        Core.projectionField = (Core.Name "typ")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

typedParamTypeWithName :: Phantoms.TTerm Syntax.TypedParamType -> Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.TypedParamType
typedParamTypeWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.TypedParamType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "typ"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TypedParamType"),
              Core.projectionField = (Core.Name "typ")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

typedParamTypeWithTyp :: Phantoms.TTerm Syntax.TypedParamType -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.TypedParamType
typedParamTypeWithTyp original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.TypedParamType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TypedParamType"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typ"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

varType :: Phantoms.TTerm Syntax.NameType -> Phantoms.TTerm Syntax.VarType
varType name =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.VarType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)}]}))

varTypeName :: Phantoms.TTerm Syntax.VarType -> Phantoms.TTerm Syntax.NameType
varTypeName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.VarType"),
        Core.projectionField = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

varTypeWithName :: Phantoms.TTerm Syntax.VarType -> Phantoms.TTerm Syntax.NameType -> Phantoms.TTerm Syntax.VarType
varTypeWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.VarType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

withType :: Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.WithType
withType lhs rhs =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.WithType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Phantoms.unTTerm lhs)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTTerm rhs)}]}))

withTypeLhs :: Phantoms.TTerm Syntax.WithType -> Phantoms.TTerm Syntax.Type
withTypeLhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.WithType"),
        Core.projectionField = (Core.Name "lhs")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

withTypeRhs :: Phantoms.TTerm Syntax.WithType -> Phantoms.TTerm Syntax.Type
withTypeRhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.WithType"),
        Core.projectionField = (Core.Name "rhs")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

withTypeWithLhs :: Phantoms.TTerm Syntax.WithType -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.WithType
withTypeWithLhs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.WithType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.WithType"),
              Core.projectionField = (Core.Name "rhs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

withTypeWithRhs :: Phantoms.TTerm Syntax.WithType -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.WithType
withTypeWithRhs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.WithType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.WithType"),
              Core.projectionField = (Core.Name "lhs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

unAnonymousData :: Phantoms.TTerm Syntax.AnonymousData -> Phantoms.TTerm ()
unAnonymousData x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.scala.syntax.AnonymousData")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unThisData :: Phantoms.TTerm Syntax.ThisData -> Phantoms.TTerm ()
unThisData x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.scala.syntax.ThisData")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unPredefString :: Phantoms.TTerm Syntax.PredefString -> Phantoms.TTerm String
unPredefString x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.scala.syntax.PredefString")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unSelf :: Phantoms.TTerm Syntax.Self -> Phantoms.TTerm ()
unSelf x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.scala.syntax.Self")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unAnonymousNameType :: Phantoms.TTerm Syntax.AnonymousNameType -> Phantoms.TTerm ()
unAnonymousNameType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.scala.syntax.AnonymousNameType")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
