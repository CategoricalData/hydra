-- Note: this is an automatically generated file. Do not edit.
-- | DSL functions for hydra.scala.syntax

module Hydra.Dsl.Scala.Syntax where
import qualified Hydra.Core as Core
import qualified Hydra.Phantoms as Phantoms
import qualified Hydra.Scala.Syntax as Syntax
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
import qualified Data.Int as I
-- | DSL constructor for hydra.scala.syntax.AlternativePat
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
-- | DSL accessor for the lhs field of hydra.scala.syntax.AlternativePat
alternativePatLhs :: Phantoms.TTerm Syntax.AlternativePat -> Phantoms.TTerm Syntax.Pat
alternativePatLhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.AlternativePat"),
        Core.projectionFieldName = (Core.Name "lhs")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the rhs field of hydra.scala.syntax.AlternativePat
alternativePatRhs :: Phantoms.TTerm Syntax.AlternativePat -> Phantoms.TTerm Syntax.Pat
alternativePatRhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.AlternativePat"),
        Core.projectionFieldName = (Core.Name "rhs")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the lhs field of hydra.scala.syntax.AlternativePat
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
              Core.projectionFieldName = (Core.Name "rhs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the rhs field of hydra.scala.syntax.AlternativePat
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
              Core.projectionFieldName = (Core.Name "lhs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.scala.syntax.AndType
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
-- | DSL accessor for the lhs field of hydra.scala.syntax.AndType
andTypeLhs :: Phantoms.TTerm Syntax.AndType -> Phantoms.TTerm Syntax.Type
andTypeLhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.AndType"),
        Core.projectionFieldName = (Core.Name "lhs")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the rhs field of hydra.scala.syntax.AndType
andTypeRhs :: Phantoms.TTerm Syntax.AndType -> Phantoms.TTerm Syntax.Type
andTypeRhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.AndType"),
        Core.projectionFieldName = (Core.Name "rhs")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the lhs field of hydra.scala.syntax.AndType
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
              Core.projectionFieldName = (Core.Name "rhs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the rhs field of hydra.scala.syntax.AndType
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
              Core.projectionFieldName = (Core.Name "lhs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.scala.syntax.AnnotMod
annotMod :: Phantoms.TTerm Syntax.Init -> Phantoms.TTerm Syntax.AnnotMod
annotMod init =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.AnnotMod"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "init"),
          Core.fieldTerm = (Phantoms.unTTerm init)}]}))
-- | DSL accessor for the init field of hydra.scala.syntax.AnnotMod
annotModInit :: Phantoms.TTerm Syntax.AnnotMod -> Phantoms.TTerm Syntax.Init
annotModInit x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.AnnotMod"),
        Core.projectionFieldName = (Core.Name "init")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the init field of hydra.scala.syntax.AnnotMod
annotModWithInit :: Phantoms.TTerm Syntax.AnnotMod -> Phantoms.TTerm Syntax.Init -> Phantoms.TTerm Syntax.AnnotMod
annotModWithInit original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.AnnotMod"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "init"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.scala.syntax.AnnotateData
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
-- | DSL accessor for the annots field of hydra.scala.syntax.AnnotateData
annotateDataAnnots :: Phantoms.TTerm Syntax.AnnotateData -> Phantoms.TTerm [Syntax.AnnotMod]
annotateDataAnnots x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.AnnotateData"),
        Core.projectionFieldName = (Core.Name "annots")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the expr field of hydra.scala.syntax.AnnotateData
annotateDataExpr :: Phantoms.TTerm Syntax.AnnotateData -> Phantoms.TTerm Syntax.Data
annotateDataExpr x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.AnnotateData"),
        Core.projectionFieldName = (Core.Name "expr")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the annots field of hydra.scala.syntax.AnnotateData
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
              Core.projectionFieldName = (Core.Name "expr")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "annots"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the expr field of hydra.scala.syntax.AnnotateData
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
              Core.projectionFieldName = (Core.Name "annots")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for hydra.scala.syntax.AnnotateType
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
-- | DSL accessor for the annots field of hydra.scala.syntax.AnnotateType
annotateTypeAnnots :: Phantoms.TTerm Syntax.AnnotateType -> Phantoms.TTerm [Syntax.AnnotMod]
annotateTypeAnnots x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.AnnotateType"),
        Core.projectionFieldName = (Core.Name "annots")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the tpe field of hydra.scala.syntax.AnnotateType
annotateTypeTpe :: Phantoms.TTerm Syntax.AnnotateType -> Phantoms.TTerm Syntax.Type
annotateTypeTpe x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.AnnotateType"),
        Core.projectionFieldName = (Core.Name "tpe")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the annots field of hydra.scala.syntax.AnnotateType
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
              Core.projectionFieldName = (Core.Name "tpe")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "annots"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the tpe field of hydra.scala.syntax.AnnotateType
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
              Core.projectionFieldName = (Core.Name "annots")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for the hydra.scala.syntax.AnonymousData wrapper
anonymousData :: Phantoms.TTerm () -> Phantoms.TTerm Syntax.AnonymousData
anonymousData x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.scala.syntax.AnonymousData"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL constructor for the hydra.scala.syntax.AnonymousNameType wrapper
anonymousNameType :: Phantoms.TTerm () -> Phantoms.TTerm Syntax.AnonymousNameType
anonymousNameType x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.scala.syntax.AnonymousNameType"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL constructor for hydra.scala.syntax.ApplyData
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
-- | DSL accessor for the args field of hydra.scala.syntax.ApplyData
applyDataArgs :: Phantoms.TTerm Syntax.ApplyData -> Phantoms.TTerm [Syntax.Data]
applyDataArgs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ApplyData"),
        Core.projectionFieldName = (Core.Name "args")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the fun field of hydra.scala.syntax.ApplyData
applyDataFun :: Phantoms.TTerm Syntax.ApplyData -> Phantoms.TTerm Syntax.Data
applyDataFun x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ApplyData"),
        Core.projectionFieldName = (Core.Name "fun")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the args field of hydra.scala.syntax.ApplyData
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
              Core.projectionFieldName = (Core.Name "fun")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "args"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the fun field of hydra.scala.syntax.ApplyData
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
              Core.projectionFieldName = (Core.Name "args")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for hydra.scala.syntax.ApplyInfixData
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
-- | DSL accessor for the args field of hydra.scala.syntax.ApplyInfixData
applyInfixDataArgs :: Phantoms.TTerm Syntax.ApplyInfixData -> Phantoms.TTerm [Syntax.Data]
applyInfixDataArgs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ApplyInfixData"),
        Core.projectionFieldName = (Core.Name "args")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the lhs field of hydra.scala.syntax.ApplyInfixData
applyInfixDataLhs :: Phantoms.TTerm Syntax.ApplyInfixData -> Phantoms.TTerm Syntax.Data
applyInfixDataLhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ApplyInfixData"),
        Core.projectionFieldName = (Core.Name "lhs")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the op field of hydra.scala.syntax.ApplyInfixData
applyInfixDataOp :: Phantoms.TTerm Syntax.ApplyInfixData -> Phantoms.TTerm Syntax.NameData
applyInfixDataOp x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ApplyInfixData"),
        Core.projectionFieldName = (Core.Name "op")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the targs field of hydra.scala.syntax.ApplyInfixData
applyInfixDataTargs :: Phantoms.TTerm Syntax.ApplyInfixData -> Phantoms.TTerm [Syntax.Type]
applyInfixDataTargs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ApplyInfixData"),
        Core.projectionFieldName = (Core.Name "targs")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the args field of hydra.scala.syntax.ApplyInfixData
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
              Core.projectionFieldName = (Core.Name "lhs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "op"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ApplyInfixData"),
              Core.projectionFieldName = (Core.Name "op")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "targs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ApplyInfixData"),
              Core.projectionFieldName = (Core.Name "targs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "args"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the lhs field of hydra.scala.syntax.ApplyInfixData
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
              Core.projectionFieldName = (Core.Name "op")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "targs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ApplyInfixData"),
              Core.projectionFieldName = (Core.Name "targs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "args"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ApplyInfixData"),
              Core.projectionFieldName = (Core.Name "args")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the op field of hydra.scala.syntax.ApplyInfixData
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
              Core.projectionFieldName = (Core.Name "lhs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "op"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "targs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ApplyInfixData"),
              Core.projectionFieldName = (Core.Name "targs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "args"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ApplyInfixData"),
              Core.projectionFieldName = (Core.Name "args")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the targs field of hydra.scala.syntax.ApplyInfixData
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
              Core.projectionFieldName = (Core.Name "lhs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "op"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ApplyInfixData"),
              Core.projectionFieldName = (Core.Name "op")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "targs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "args"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ApplyInfixData"),
              Core.projectionFieldName = (Core.Name "args")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for hydra.scala.syntax.ApplyInfixType
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
-- | DSL accessor for the lhs field of hydra.scala.syntax.ApplyInfixType
applyInfixTypeLhs :: Phantoms.TTerm Syntax.ApplyInfixType -> Phantoms.TTerm Syntax.Type
applyInfixTypeLhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ApplyInfixType"),
        Core.projectionFieldName = (Core.Name "lhs")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the op field of hydra.scala.syntax.ApplyInfixType
applyInfixTypeOp :: Phantoms.TTerm Syntax.ApplyInfixType -> Phantoms.TTerm Syntax.NameType
applyInfixTypeOp x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ApplyInfixType"),
        Core.projectionFieldName = (Core.Name "op")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the rhs field of hydra.scala.syntax.ApplyInfixType
applyInfixTypeRhs :: Phantoms.TTerm Syntax.ApplyInfixType -> Phantoms.TTerm Syntax.Type
applyInfixTypeRhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ApplyInfixType"),
        Core.projectionFieldName = (Core.Name "rhs")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the lhs field of hydra.scala.syntax.ApplyInfixType
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
              Core.projectionFieldName = (Core.Name "op")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ApplyInfixType"),
              Core.projectionFieldName = (Core.Name "rhs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the op field of hydra.scala.syntax.ApplyInfixType
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
              Core.projectionFieldName = (Core.Name "lhs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "op"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ApplyInfixType"),
              Core.projectionFieldName = (Core.Name "rhs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the rhs field of hydra.scala.syntax.ApplyInfixType
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
              Core.projectionFieldName = (Core.Name "lhs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "op"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ApplyInfixType"),
              Core.projectionFieldName = (Core.Name "op")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.scala.syntax.ApplyType
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
-- | DSL accessor for the args field of hydra.scala.syntax.ApplyType
applyTypeArgs :: Phantoms.TTerm Syntax.ApplyType -> Phantoms.TTerm [Syntax.Type]
applyTypeArgs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ApplyType"),
        Core.projectionFieldName = (Core.Name "args")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL constructor for hydra.scala.syntax.ApplyTypeData
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
-- | DSL accessor for the args field of hydra.scala.syntax.ApplyTypeData
applyTypeDataArgs :: Phantoms.TTerm Syntax.ApplyTypeData -> Phantoms.TTerm [Syntax.Data]
applyTypeDataArgs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ApplyTypeData"),
        Core.projectionFieldName = (Core.Name "args")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the lhs field of hydra.scala.syntax.ApplyTypeData
applyTypeDataLhs :: Phantoms.TTerm Syntax.ApplyTypeData -> Phantoms.TTerm Syntax.Data
applyTypeDataLhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ApplyTypeData"),
        Core.projectionFieldName = (Core.Name "lhs")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the op field of hydra.scala.syntax.ApplyTypeData
applyTypeDataOp :: Phantoms.TTerm Syntax.ApplyTypeData -> Phantoms.TTerm Syntax.NameData
applyTypeDataOp x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ApplyTypeData"),
        Core.projectionFieldName = (Core.Name "op")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the targs field of hydra.scala.syntax.ApplyTypeData
applyTypeDataTargs :: Phantoms.TTerm Syntax.ApplyTypeData -> Phantoms.TTerm [Syntax.Type]
applyTypeDataTargs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ApplyTypeData"),
        Core.projectionFieldName = (Core.Name "targs")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the args field of hydra.scala.syntax.ApplyTypeData
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
              Core.projectionFieldName = (Core.Name "lhs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "op"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ApplyTypeData"),
              Core.projectionFieldName = (Core.Name "op")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "targs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ApplyTypeData"),
              Core.projectionFieldName = (Core.Name "targs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "args"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the lhs field of hydra.scala.syntax.ApplyTypeData
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
              Core.projectionFieldName = (Core.Name "op")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "targs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ApplyTypeData"),
              Core.projectionFieldName = (Core.Name "targs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "args"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ApplyTypeData"),
              Core.projectionFieldName = (Core.Name "args")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the op field of hydra.scala.syntax.ApplyTypeData
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
              Core.projectionFieldName = (Core.Name "lhs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "op"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "targs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ApplyTypeData"),
              Core.projectionFieldName = (Core.Name "targs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "args"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ApplyTypeData"),
              Core.projectionFieldName = (Core.Name "args")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the targs field of hydra.scala.syntax.ApplyTypeData
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
              Core.projectionFieldName = (Core.Name "lhs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "op"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ApplyTypeData"),
              Core.projectionFieldName = (Core.Name "op")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "targs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "args"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ApplyTypeData"),
              Core.projectionFieldName = (Core.Name "args")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL accessor for the tpe field of hydra.scala.syntax.ApplyType
applyTypeTpe :: Phantoms.TTerm Syntax.ApplyType -> Phantoms.TTerm Syntax.Type
applyTypeTpe x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ApplyType"),
        Core.projectionFieldName = (Core.Name "tpe")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the args field of hydra.scala.syntax.ApplyType
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
              Core.projectionFieldName = (Core.Name "tpe")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "args"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the tpe field of hydra.scala.syntax.ApplyType
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
              Core.projectionFieldName = (Core.Name "args")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for hydra.scala.syntax.ApplyUnaryData
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
-- | DSL accessor for the arg field of hydra.scala.syntax.ApplyUnaryData
applyUnaryDataArg :: Phantoms.TTerm Syntax.ApplyUnaryData -> Phantoms.TTerm Syntax.Data
applyUnaryDataArg x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ApplyUnaryData"),
        Core.projectionFieldName = (Core.Name "arg")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the op field of hydra.scala.syntax.ApplyUnaryData
applyUnaryDataOp :: Phantoms.TTerm Syntax.ApplyUnaryData -> Phantoms.TTerm Syntax.NameData
applyUnaryDataOp x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ApplyUnaryData"),
        Core.projectionFieldName = (Core.Name "op")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the arg field of hydra.scala.syntax.ApplyUnaryData
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
              Core.projectionFieldName = (Core.Name "op")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "arg"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the op field of hydra.scala.syntax.ApplyUnaryData
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
              Core.projectionFieldName = (Core.Name "arg")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for hydra.scala.syntax.ApplyUsingData
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
-- | DSL accessor for the fun field of hydra.scala.syntax.ApplyUsingData
applyUsingDataFun :: Phantoms.TTerm Syntax.ApplyUsingData -> Phantoms.TTerm Syntax.Data
applyUsingDataFun x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ApplyUsingData"),
        Core.projectionFieldName = (Core.Name "fun")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the targs field of hydra.scala.syntax.ApplyUsingData
applyUsingDataTargs :: Phantoms.TTerm Syntax.ApplyUsingData -> Phantoms.TTerm [Syntax.Data]
applyUsingDataTargs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ApplyUsingData"),
        Core.projectionFieldName = (Core.Name "targs")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the fun field of hydra.scala.syntax.ApplyUsingData
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
              Core.projectionFieldName = (Core.Name "targs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the targs field of hydra.scala.syntax.ApplyUsingData
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
              Core.projectionFieldName = (Core.Name "fun")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "targs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.scala.syntax.AscribeData
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
-- | DSL accessor for the expr field of hydra.scala.syntax.AscribeData
ascribeDataExpr :: Phantoms.TTerm Syntax.AscribeData -> Phantoms.TTerm Syntax.Data
ascribeDataExpr x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.AscribeData"),
        Core.projectionFieldName = (Core.Name "expr")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the tpe field of hydra.scala.syntax.AscribeData
ascribeDataTpe :: Phantoms.TTerm Syntax.AscribeData -> Phantoms.TTerm Syntax.Type
ascribeDataTpe x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.AscribeData"),
        Core.projectionFieldName = (Core.Name "tpe")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the expr field of hydra.scala.syntax.AscribeData
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
              Core.projectionFieldName = (Core.Name "tpe")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the tpe field of hydra.scala.syntax.AscribeData
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
              Core.projectionFieldName = (Core.Name "expr")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tpe"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.scala.syntax.AssignData
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
-- | DSL accessor for the lhs field of hydra.scala.syntax.AssignData
assignDataLhs :: Phantoms.TTerm Syntax.AssignData -> Phantoms.TTerm Syntax.Data
assignDataLhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.AssignData"),
        Core.projectionFieldName = (Core.Name "lhs")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the rhs field of hydra.scala.syntax.AssignData
assignDataRhs :: Phantoms.TTerm Syntax.AssignData -> Phantoms.TTerm Syntax.Data
assignDataRhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.AssignData"),
        Core.projectionFieldName = (Core.Name "rhs")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the lhs field of hydra.scala.syntax.AssignData
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
              Core.projectionFieldName = (Core.Name "rhs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the rhs field of hydra.scala.syntax.AssignData
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
              Core.projectionFieldName = (Core.Name "lhs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.scala.syntax.BindPat
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
-- | DSL accessor for the lhs field of hydra.scala.syntax.BindPat
bindPatLhs :: Phantoms.TTerm Syntax.BindPat -> Phantoms.TTerm Syntax.Pat
bindPatLhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.BindPat"),
        Core.projectionFieldName = (Core.Name "lhs")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the rhs field of hydra.scala.syntax.BindPat
bindPatRhs :: Phantoms.TTerm Syntax.BindPat -> Phantoms.TTerm Syntax.Pat
bindPatRhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.BindPat"),
        Core.projectionFieldName = (Core.Name "rhs")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the lhs field of hydra.scala.syntax.BindPat
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
              Core.projectionFieldName = (Core.Name "rhs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the rhs field of hydra.scala.syntax.BindPat
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
              Core.projectionFieldName = (Core.Name "lhs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.scala.syntax.BlockData
blockData :: Phantoms.TTerm [Syntax.Stat] -> Phantoms.TTerm Syntax.BlockData
blockData stats =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.BlockData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "stats"),
          Core.fieldTerm = (Phantoms.unTTerm stats)}]}))
-- | DSL accessor for the stats field of hydra.scala.syntax.BlockData
blockDataStats :: Phantoms.TTerm Syntax.BlockData -> Phantoms.TTerm [Syntax.Stat]
blockDataStats x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.BlockData"),
        Core.projectionFieldName = (Core.Name "stats")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the stats field of hydra.scala.syntax.BlockData
blockDataWithStats :: Phantoms.TTerm Syntax.BlockData -> Phantoms.TTerm [Syntax.Stat] -> Phantoms.TTerm Syntax.BlockData
blockDataWithStats original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.BlockData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "stats"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.scala.syntax.ByNameType
byNameType :: Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.ByNameType
byNameType tpe =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ByNameType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "tpe"),
          Core.fieldTerm = (Phantoms.unTTerm tpe)}]}))
-- | DSL accessor for the tpe field of hydra.scala.syntax.ByNameType
byNameTypeTpe :: Phantoms.TTerm Syntax.ByNameType -> Phantoms.TTerm Syntax.Type
byNameTypeTpe x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ByNameType"),
        Core.projectionFieldName = (Core.Name "tpe")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the tpe field of hydra.scala.syntax.ByNameType
byNameTypeWithTpe :: Phantoms.TTerm Syntax.ByNameType -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.ByNameType
byNameTypeWithTpe original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ByNameType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "tpe"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.scala.syntax.Case
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
-- | DSL accessor for the body field of hydra.scala.syntax.Case
caseBody :: Phantoms.TTerm Syntax.Case -> Phantoms.TTerm Syntax.Data
caseBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.Case"),
        Core.projectionFieldName = (Core.Name "body")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the cond field of hydra.scala.syntax.Case
caseCond :: Phantoms.TTerm Syntax.Case -> Phantoms.TTerm (Maybe Syntax.Data)
caseCond x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.Case"),
        Core.projectionFieldName = (Core.Name "cond")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL constructor for hydra.scala.syntax.CaseGeneratorEnumerator
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
-- | DSL accessor for the pat field of hydra.scala.syntax.CaseGeneratorEnumerator
caseGeneratorEnumeratorPat :: Phantoms.TTerm Syntax.CaseGeneratorEnumerator -> Phantoms.TTerm Syntax.Pat
caseGeneratorEnumeratorPat x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.CaseGeneratorEnumerator"),
        Core.projectionFieldName = (Core.Name "pat")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the rhs field of hydra.scala.syntax.CaseGeneratorEnumerator
caseGeneratorEnumeratorRhs :: Phantoms.TTerm Syntax.CaseGeneratorEnumerator -> Phantoms.TTerm Syntax.Data
caseGeneratorEnumeratorRhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.CaseGeneratorEnumerator"),
        Core.projectionFieldName = (Core.Name "rhs")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the pat field of hydra.scala.syntax.CaseGeneratorEnumerator
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
              Core.projectionFieldName = (Core.Name "rhs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the rhs field of hydra.scala.syntax.CaseGeneratorEnumerator
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
              Core.projectionFieldName = (Core.Name "pat")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL accessor for the pat field of hydra.scala.syntax.Case
casePat :: Phantoms.TTerm Syntax.Case -> Phantoms.TTerm Syntax.Pat
casePat x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.Case"),
        Core.projectionFieldName = (Core.Name "pat")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL injection for the case variant of hydra.scala.syntax.CaseTree
caseTreeCase :: Phantoms.TTerm Syntax.Case -> Phantoms.TTerm Syntax.CaseTree
caseTreeCase x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.CaseTree"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "case"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the typeCase variant of hydra.scala.syntax.CaseTree
caseTreeTypeCase :: Phantoms.TTerm Syntax.TypeCase -> Phantoms.TTerm Syntax.CaseTree
caseTreeTypeCase x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.CaseTree"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "typeCase"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL updater for the body field of hydra.scala.syntax.Case
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
              Core.projectionFieldName = (Core.Name "pat")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "cond"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.Case"),
              Core.projectionFieldName = (Core.Name "cond")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the cond field of hydra.scala.syntax.Case
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
              Core.projectionFieldName = (Core.Name "pat")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "cond"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.Case"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the pat field of hydra.scala.syntax.Case
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
              Core.projectionFieldName = (Core.Name "cond")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.Case"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for hydra.scala.syntax.ClassDefn
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
-- | DSL accessor for the ctor field of hydra.scala.syntax.ClassDefn
classDefnCtor :: Phantoms.TTerm Syntax.ClassDefn -> Phantoms.TTerm Syntax.PrimaryCtor
classDefnCtor x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ClassDefn"),
        Core.projectionFieldName = (Core.Name "ctor")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the mods field of hydra.scala.syntax.ClassDefn
classDefnMods :: Phantoms.TTerm Syntax.ClassDefn -> Phantoms.TTerm [Syntax.Mod]
classDefnMods x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ClassDefn"),
        Core.projectionFieldName = (Core.Name "mods")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the name field of hydra.scala.syntax.ClassDefn
classDefnName :: Phantoms.TTerm Syntax.ClassDefn -> Phantoms.TTerm Syntax.NameType
classDefnName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ClassDefn"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the template field of hydra.scala.syntax.ClassDefn
classDefnTemplate :: Phantoms.TTerm Syntax.ClassDefn -> Phantoms.TTerm Syntax.Template
classDefnTemplate x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ClassDefn"),
        Core.projectionFieldName = (Core.Name "template")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the tparams field of hydra.scala.syntax.ClassDefn
classDefnTparams :: Phantoms.TTerm Syntax.ClassDefn -> Phantoms.TTerm [Syntax.ParamType]
classDefnTparams x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ClassDefn"),
        Core.projectionFieldName = (Core.Name "tparams")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the ctor field of hydra.scala.syntax.ClassDefn
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
              Core.projectionFieldName = (Core.Name "mods")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ClassDefn"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ClassDefn"),
              Core.projectionFieldName = (Core.Name "tparams")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ctor"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "template"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ClassDefn"),
              Core.projectionFieldName = (Core.Name "template")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the mods field of hydra.scala.syntax.ClassDefn
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
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ClassDefn"),
              Core.projectionFieldName = (Core.Name "tparams")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ctor"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ClassDefn"),
              Core.projectionFieldName = (Core.Name "ctor")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "template"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ClassDefn"),
              Core.projectionFieldName = (Core.Name "template")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the name field of hydra.scala.syntax.ClassDefn
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
              Core.projectionFieldName = (Core.Name "mods")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ClassDefn"),
              Core.projectionFieldName = (Core.Name "tparams")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ctor"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ClassDefn"),
              Core.projectionFieldName = (Core.Name "ctor")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "template"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ClassDefn"),
              Core.projectionFieldName = (Core.Name "template")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the template field of hydra.scala.syntax.ClassDefn
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
              Core.projectionFieldName = (Core.Name "mods")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ClassDefn"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ClassDefn"),
              Core.projectionFieldName = (Core.Name "tparams")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ctor"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ClassDefn"),
              Core.projectionFieldName = (Core.Name "ctor")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "template"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the tparams field of hydra.scala.syntax.ClassDefn
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
              Core.projectionFieldName = (Core.Name "mods")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ClassDefn"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "ctor"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ClassDefn"),
              Core.projectionFieldName = (Core.Name "ctor")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "template"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ClassDefn"),
              Core.projectionFieldName = (Core.Name "template")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for hydra.scala.syntax.ContextFunctionData
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
-- | DSL accessor for the body field of hydra.scala.syntax.ContextFunctionData
contextFunctionDataBody :: Phantoms.TTerm Syntax.ContextFunctionData -> Phantoms.TTerm Syntax.Data
contextFunctionDataBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ContextFunctionData"),
        Core.projectionFieldName = (Core.Name "body")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the params field of hydra.scala.syntax.ContextFunctionData
contextFunctionDataParams :: Phantoms.TTerm Syntax.ContextFunctionData -> Phantoms.TTerm [Syntax.ParamData]
contextFunctionDataParams x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ContextFunctionData"),
        Core.projectionFieldName = (Core.Name "params")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the body field of hydra.scala.syntax.ContextFunctionData
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
              Core.projectionFieldName = (Core.Name "params")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the params field of hydra.scala.syntax.ContextFunctionData
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
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for hydra.scala.syntax.ContextFunctionType
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
-- | DSL accessor for the params field of hydra.scala.syntax.ContextFunctionType
contextFunctionTypeParams :: Phantoms.TTerm Syntax.ContextFunctionType -> Phantoms.TTerm [Syntax.Type]
contextFunctionTypeParams x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ContextFunctionType"),
        Core.projectionFieldName = (Core.Name "params")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the res field of hydra.scala.syntax.ContextFunctionType
contextFunctionTypeRes :: Phantoms.TTerm Syntax.ContextFunctionType -> Phantoms.TTerm Syntax.Type
contextFunctionTypeRes x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ContextFunctionType"),
        Core.projectionFieldName = (Core.Name "res")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the params field of hydra.scala.syntax.ContextFunctionType
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
              Core.projectionFieldName = (Core.Name "res")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the res field of hydra.scala.syntax.ContextFunctionType
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
              Core.projectionFieldName = (Core.Name "params")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "res"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL injection for the primary variant of hydra.scala.syntax.Ctor
ctorPrimary :: Phantoms.TTerm Syntax.PrimaryCtor -> Phantoms.TTerm Syntax.Ctor
ctorPrimary x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Ctor"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "primary"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the secondary variant of hydra.scala.syntax.Ctor
ctorSecondary :: Phantoms.TTerm Syntax.SecondaryCtor -> Phantoms.TTerm Syntax.Ctor
ctorSecondary x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Ctor"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "secondary"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the annotate variant of hydra.scala.syntax.Data
dataAnnotate :: Phantoms.TTerm Syntax.AnnotateData -> Phantoms.TTerm Syntax.Data
dataAnnotate x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Data"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "annotate"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the apply variant of hydra.scala.syntax.Data
dataApply :: Phantoms.TTerm Syntax.ApplyData -> Phantoms.TTerm Syntax.Data
dataApply x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Data"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "apply"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the applyType variant of hydra.scala.syntax.Data
dataApplyType :: Phantoms.TTerm Syntax.ApplyTypeData -> Phantoms.TTerm Syntax.Data
dataApplyType x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Data"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "applyType"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the applyUsing variant of hydra.scala.syntax.Data
dataApplyUsing :: Phantoms.TTerm Syntax.ApplyUsingData -> Phantoms.TTerm Syntax.Data
dataApplyUsing x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Data"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "applyUsing"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the ascribe variant of hydra.scala.syntax.Data
dataAscribe :: Phantoms.TTerm Syntax.AscribeData -> Phantoms.TTerm Syntax.Data
dataAscribe x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Data"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "ascribe"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the assign variant of hydra.scala.syntax.Data
dataAssign :: Phantoms.TTerm Syntax.AssignData -> Phantoms.TTerm Syntax.Data
dataAssign x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Data"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "assign"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the block variant of hydra.scala.syntax.Data
dataBlock :: Phantoms.TTerm Syntax.BlockData -> Phantoms.TTerm Syntax.Data
dataBlock x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Data"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "block"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the contextFunction variant of hydra.scala.syntax.Data
dataContextFunction :: Phantoms.TTerm Syntax.ContextFunctionData -> Phantoms.TTerm Syntax.Data
dataContextFunction x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Data"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "contextFunction"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the do variant of hydra.scala.syntax.Data
dataDo :: Phantoms.TTerm Syntax.DoData -> Phantoms.TTerm Syntax.Data
dataDo x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Data"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "do"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the endMarker variant of hydra.scala.syntax.Data
dataEndMarker :: Phantoms.TTerm Syntax.EndMarkerData -> Phantoms.TTerm Syntax.Data
dataEndMarker x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Data"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "endMarker"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the eta variant of hydra.scala.syntax.Data
dataEta :: Phantoms.TTerm Syntax.EtaData -> Phantoms.TTerm Syntax.Data
dataEta x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Data"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "eta"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the for variant of hydra.scala.syntax.Data
dataFor :: Phantoms.TTerm Syntax.ForData -> Phantoms.TTerm Syntax.Data
dataFor x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Data"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "for"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the forYield variant of hydra.scala.syntax.Data
dataForYield :: Phantoms.TTerm Syntax.ForYieldData -> Phantoms.TTerm Syntax.Data
dataForYield x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Data"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "forYield"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the function variant of hydra.scala.syntax.Data
dataFunction :: Phantoms.TTerm Syntax.FunctionData -> Phantoms.TTerm Syntax.Data
dataFunction x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Data"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "function"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the if variant of hydra.scala.syntax.Data
dataIf :: Phantoms.TTerm Syntax.IfData -> Phantoms.TTerm Syntax.Data
dataIf x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Data"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "if"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the interpolate variant of hydra.scala.syntax.Data
dataInterpolate :: Phantoms.TTerm Syntax.InterpolateData -> Phantoms.TTerm Syntax.Data
dataInterpolate x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Data"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "interpolate"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the lit variant of hydra.scala.syntax.Data
dataLit :: Phantoms.TTerm Syntax.Lit -> Phantoms.TTerm Syntax.Data
dataLit x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Data"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "lit"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the match variant of hydra.scala.syntax.Data
dataMatch :: Phantoms.TTerm Syntax.MatchData -> Phantoms.TTerm Syntax.Data
dataMatch x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Data"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "match"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the object variant of hydra.scala.syntax.DataMember
dataMemberObject :: Phantoms.TTerm Syntax.ObjectPkg -> Phantoms.TTerm Syntax.DataMember
dataMemberObject x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.DataMember"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "object"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the pkg variant of hydra.scala.syntax.DataMember
dataMemberPkg :: Phantoms.TTerm Syntax.Pkg -> Phantoms.TTerm Syntax.DataMember
dataMemberPkg x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.DataMember"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "pkg"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the new variant of hydra.scala.syntax.Data
dataNew :: Phantoms.TTerm Syntax.NewData -> Phantoms.TTerm Syntax.Data
dataNew x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Data"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "new"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the newAnonymous variant of hydra.scala.syntax.Data
dataNewAnonymous :: Phantoms.TTerm Syntax.NewAnonymousData -> Phantoms.TTerm Syntax.Data
dataNewAnonymous x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Data"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "newAnonymous"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the param variant of hydra.scala.syntax.Data
dataParam :: Phantoms.TTerm Syntax.ParamData -> Phantoms.TTerm Syntax.Data
dataParam x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Data"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "param"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the partialFunction variant of hydra.scala.syntax.Data
dataPartialFunction :: Phantoms.TTerm Syntax.PartialFunctionData -> Phantoms.TTerm Syntax.Data
dataPartialFunction x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Data"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "partialFunction"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the placeholder variant of hydra.scala.syntax.Data
dataPlaceholder :: Phantoms.TTerm Syntax.Data
dataPlaceholder =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Data"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "placeholder"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the polyFunction variant of hydra.scala.syntax.Data
dataPolyFunction :: Phantoms.TTerm Syntax.PolyFunctionData -> Phantoms.TTerm Syntax.Data
dataPolyFunction x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Data"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "polyFunction"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the ref variant of hydra.scala.syntax.Data
dataRef :: Phantoms.TTerm Syntax.RefData -> Phantoms.TTerm Syntax.Data
dataRef x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Data"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "ref"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the repeated variant of hydra.scala.syntax.Data
dataRepeated :: Phantoms.TTerm Syntax.RepeatedData -> Phantoms.TTerm Syntax.Data
dataRepeated x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Data"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "repeated"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the return variant of hydra.scala.syntax.Data
dataReturn :: Phantoms.TTerm Syntax.ReturnData -> Phantoms.TTerm Syntax.Data
dataReturn x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Data"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "return"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the throw variant of hydra.scala.syntax.Data
dataThrow :: Phantoms.TTerm Syntax.ThrowData -> Phantoms.TTerm Syntax.Data
dataThrow x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Data"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "throw"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the try variant of hydra.scala.syntax.Data
dataTry :: Phantoms.TTerm Syntax.TryData -> Phantoms.TTerm Syntax.Data
dataTry x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Data"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "try"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the tryWithHandler variant of hydra.scala.syntax.Data
dataTryWithHandler :: Phantoms.TTerm Syntax.TryWithHandlerData -> Phantoms.TTerm Syntax.Data
dataTryWithHandler x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Data"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "tryWithHandler"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the tuple variant of hydra.scala.syntax.Data
dataTuple :: Phantoms.TTerm Syntax.TupleData -> Phantoms.TTerm Syntax.Data
dataTuple x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Data"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "tuple"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the while variant of hydra.scala.syntax.Data
dataWhile :: Phantoms.TTerm Syntax.WhileData -> Phantoms.TTerm Syntax.Data
dataWhile x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Data"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "while"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the def variant of hydra.scala.syntax.Decl
declDef :: Phantoms.TTerm Syntax.DefDecl -> Phantoms.TTerm Syntax.Decl
declDef x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Decl"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "def"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the given variant of hydra.scala.syntax.Decl
declGiven :: Phantoms.TTerm Syntax.GivenDecl -> Phantoms.TTerm Syntax.Decl
declGiven x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Decl"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "given"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the type variant of hydra.scala.syntax.Decl
declType :: Phantoms.TTerm Syntax.TypeDecl -> Phantoms.TTerm Syntax.Decl
declType x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Decl"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "type"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the val variant of hydra.scala.syntax.Decl
declVal :: Phantoms.TTerm Syntax.ValDecl -> Phantoms.TTerm Syntax.Decl
declVal x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Decl"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "val"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the var variant of hydra.scala.syntax.Decl
declVar :: Phantoms.TTerm Syntax.VarDecl -> Phantoms.TTerm Syntax.Decl
declVar x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Decl"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "var"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL constructor for hydra.scala.syntax.DefDecl
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
-- | DSL accessor for the decltpe field of hydra.scala.syntax.DefDecl
defDeclDecltpe :: Phantoms.TTerm Syntax.DefDecl -> Phantoms.TTerm Syntax.Type
defDeclDecltpe x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DefDecl"),
        Core.projectionFieldName = (Core.Name "decltpe")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the mods field of hydra.scala.syntax.DefDecl
defDeclMods :: Phantoms.TTerm Syntax.DefDecl -> Phantoms.TTerm [Syntax.Mod]
defDeclMods x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DefDecl"),
        Core.projectionFieldName = (Core.Name "mods")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the name field of hydra.scala.syntax.DefDecl
defDeclName :: Phantoms.TTerm Syntax.DefDecl -> Phantoms.TTerm Syntax.NameData
defDeclName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DefDecl"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the paramss field of hydra.scala.syntax.DefDecl
defDeclParamss :: Phantoms.TTerm Syntax.DefDecl -> Phantoms.TTerm [[Syntax.ParamData]]
defDeclParamss x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DefDecl"),
        Core.projectionFieldName = (Core.Name "paramss")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the tparams field of hydra.scala.syntax.DefDecl
defDeclTparams :: Phantoms.TTerm Syntax.DefDecl -> Phantoms.TTerm [Syntax.ParamType]
defDeclTparams x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DefDecl"),
        Core.projectionFieldName = (Core.Name "tparams")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the decltpe field of hydra.scala.syntax.DefDecl
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
              Core.projectionFieldName = (Core.Name "mods")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DefDecl"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DefDecl"),
              Core.projectionFieldName = (Core.Name "tparams")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "paramss"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DefDecl"),
              Core.projectionFieldName = (Core.Name "paramss")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the mods field of hydra.scala.syntax.DefDecl
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
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DefDecl"),
              Core.projectionFieldName = (Core.Name "tparams")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "paramss"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DefDecl"),
              Core.projectionFieldName = (Core.Name "paramss")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DefDecl"),
              Core.projectionFieldName = (Core.Name "decltpe")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the name field of hydra.scala.syntax.DefDecl
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
              Core.projectionFieldName = (Core.Name "mods")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DefDecl"),
              Core.projectionFieldName = (Core.Name "tparams")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "paramss"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DefDecl"),
              Core.projectionFieldName = (Core.Name "paramss")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DefDecl"),
              Core.projectionFieldName = (Core.Name "decltpe")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the paramss field of hydra.scala.syntax.DefDecl
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
              Core.projectionFieldName = (Core.Name "mods")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DefDecl"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DefDecl"),
              Core.projectionFieldName = (Core.Name "tparams")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "paramss"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DefDecl"),
              Core.projectionFieldName = (Core.Name "decltpe")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the tparams field of hydra.scala.syntax.DefDecl
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
              Core.projectionFieldName = (Core.Name "mods")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DefDecl"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "paramss"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DefDecl"),
              Core.projectionFieldName = (Core.Name "paramss")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DefDecl"),
              Core.projectionFieldName = (Core.Name "decltpe")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for hydra.scala.syntax.DefDefn
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
-- | DSL accessor for the body field of hydra.scala.syntax.DefDefn
defDefnBody :: Phantoms.TTerm Syntax.DefDefn -> Phantoms.TTerm Syntax.Data
defDefnBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DefDefn"),
        Core.projectionFieldName = (Core.Name "body")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the decltpe field of hydra.scala.syntax.DefDefn
defDefnDecltpe :: Phantoms.TTerm Syntax.DefDefn -> Phantoms.TTerm (Maybe Syntax.Type)
defDefnDecltpe x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DefDefn"),
        Core.projectionFieldName = (Core.Name "decltpe")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the mods field of hydra.scala.syntax.DefDefn
defDefnMods :: Phantoms.TTerm Syntax.DefDefn -> Phantoms.TTerm [Syntax.Mod]
defDefnMods x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DefDefn"),
        Core.projectionFieldName = (Core.Name "mods")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the name field of hydra.scala.syntax.DefDefn
defDefnName :: Phantoms.TTerm Syntax.DefDefn -> Phantoms.TTerm Syntax.NameData
defDefnName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DefDefn"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the paramss field of hydra.scala.syntax.DefDefn
defDefnParamss :: Phantoms.TTerm Syntax.DefDefn -> Phantoms.TTerm [[Syntax.ParamData]]
defDefnParamss x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DefDefn"),
        Core.projectionFieldName = (Core.Name "paramss")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the tparams field of hydra.scala.syntax.DefDefn
defDefnTparams :: Phantoms.TTerm Syntax.DefDefn -> Phantoms.TTerm [Syntax.ParamType]
defDefnTparams x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DefDefn"),
        Core.projectionFieldName = (Core.Name "tparams")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the body field of hydra.scala.syntax.DefDefn
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
              Core.projectionFieldName = (Core.Name "mods")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DefDefn"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DefDefn"),
              Core.projectionFieldName = (Core.Name "tparams")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "paramss"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DefDefn"),
              Core.projectionFieldName = (Core.Name "paramss")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DefDefn"),
              Core.projectionFieldName = (Core.Name "decltpe")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the decltpe field of hydra.scala.syntax.DefDefn
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
              Core.projectionFieldName = (Core.Name "mods")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DefDefn"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DefDefn"),
              Core.projectionFieldName = (Core.Name "tparams")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "paramss"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DefDefn"),
              Core.projectionFieldName = (Core.Name "paramss")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DefDefn"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the mods field of hydra.scala.syntax.DefDefn
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
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DefDefn"),
              Core.projectionFieldName = (Core.Name "tparams")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "paramss"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DefDefn"),
              Core.projectionFieldName = (Core.Name "paramss")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DefDefn"),
              Core.projectionFieldName = (Core.Name "decltpe")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DefDefn"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the name field of hydra.scala.syntax.DefDefn
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
              Core.projectionFieldName = (Core.Name "mods")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DefDefn"),
              Core.projectionFieldName = (Core.Name "tparams")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "paramss"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DefDefn"),
              Core.projectionFieldName = (Core.Name "paramss")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DefDefn"),
              Core.projectionFieldName = (Core.Name "decltpe")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DefDefn"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the paramss field of hydra.scala.syntax.DefDefn
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
              Core.projectionFieldName = (Core.Name "mods")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DefDefn"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DefDefn"),
              Core.projectionFieldName = (Core.Name "tparams")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "paramss"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DefDefn"),
              Core.projectionFieldName = (Core.Name "decltpe")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DefDefn"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the tparams field of hydra.scala.syntax.DefDefn
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
              Core.projectionFieldName = (Core.Name "mods")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DefDefn"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "paramss"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DefDefn"),
              Core.projectionFieldName = (Core.Name "paramss")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DefDefn"),
              Core.projectionFieldName = (Core.Name "decltpe")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DefDefn"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL injection for the class variant of hydra.scala.syntax.Defn
defnClass :: Phantoms.TTerm Syntax.ClassDefn -> Phantoms.TTerm Syntax.Defn
defnClass x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Defn"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "class"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the def variant of hydra.scala.syntax.Defn
defnDef :: Phantoms.TTerm Syntax.DefDefn -> Phantoms.TTerm Syntax.Defn
defnDef x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Defn"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "def"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the enum variant of hydra.scala.syntax.Defn
defnEnum :: Phantoms.TTerm Syntax.EnumDefn -> Phantoms.TTerm Syntax.Defn
defnEnum x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Defn"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "enum"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the enumCase variant of hydra.scala.syntax.Defn
defnEnumCase :: Phantoms.TTerm Syntax.EnumCaseDefn -> Phantoms.TTerm Syntax.Defn
defnEnumCase x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Defn"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "enumCase"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the extensionGroup variant of hydra.scala.syntax.Defn
defnExtensionGroup :: Phantoms.TTerm Syntax.ExtensionGroupDefn -> Phantoms.TTerm Syntax.Defn
defnExtensionGroup x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Defn"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "extensionGroup"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the given variant of hydra.scala.syntax.Defn
defnGiven :: Phantoms.TTerm Syntax.GivenDefn -> Phantoms.TTerm Syntax.Defn
defnGiven x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Defn"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "given"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the givenAlias variant of hydra.scala.syntax.Defn
defnGivenAlias :: Phantoms.TTerm Syntax.GivenAliasDefn -> Phantoms.TTerm Syntax.Defn
defnGivenAlias x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Defn"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "givenAlias"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the object variant of hydra.scala.syntax.Defn
defnObject :: Phantoms.TTerm Syntax.ObjectDefn -> Phantoms.TTerm Syntax.Defn
defnObject x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Defn"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "object"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the repeatedEnumCase variant of hydra.scala.syntax.Defn
defnRepeatedEnumCase :: Phantoms.TTerm Syntax.RepeatedEnumCaseDefn -> Phantoms.TTerm Syntax.Defn
defnRepeatedEnumCase x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Defn"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "repeatedEnumCase"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the trait variant of hydra.scala.syntax.Defn
defnTrait :: Phantoms.TTerm Syntax.TraitDefn -> Phantoms.TTerm Syntax.Defn
defnTrait x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Defn"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "trait"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the type variant of hydra.scala.syntax.Defn
defnType :: Phantoms.TTerm Syntax.TypeDefn -> Phantoms.TTerm Syntax.Defn
defnType x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Defn"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "type"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the val variant of hydra.scala.syntax.Defn
defnVal :: Phantoms.TTerm Syntax.ValDefn -> Phantoms.TTerm Syntax.Defn
defnVal x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Defn"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "val"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the var variant of hydra.scala.syntax.Defn
defnVar :: Phantoms.TTerm Syntax.VarDefn -> Phantoms.TTerm Syntax.Defn
defnVar x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Defn"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "var"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL constructor for hydra.scala.syntax.DoData
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
-- | DSL accessor for the body field of hydra.scala.syntax.DoData
doDataBody :: Phantoms.TTerm Syntax.DoData -> Phantoms.TTerm Syntax.Data
doDataBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DoData"),
        Core.projectionFieldName = (Core.Name "body")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the expr field of hydra.scala.syntax.DoData
doDataExpr :: Phantoms.TTerm Syntax.DoData -> Phantoms.TTerm Syntax.Data
doDataExpr x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DoData"),
        Core.projectionFieldName = (Core.Name "expr")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the body field of hydra.scala.syntax.DoData
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
              Core.projectionFieldName = (Core.Name "expr")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the expr field of hydra.scala.syntax.DoData
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
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "expr"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.scala.syntax.EndMarkerData
endMarkerData :: Phantoms.TTerm Syntax.NameData -> Phantoms.TTerm Syntax.EndMarkerData
endMarkerData name =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.EndMarkerData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)}]}))
-- | DSL accessor for the name field of hydra.scala.syntax.EndMarkerData
endMarkerDataName :: Phantoms.TTerm Syntax.EndMarkerData -> Phantoms.TTerm Syntax.NameData
endMarkerDataName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.EndMarkerData"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the name field of hydra.scala.syntax.EndMarkerData
endMarkerDataWithName :: Phantoms.TTerm Syntax.EndMarkerData -> Phantoms.TTerm Syntax.NameData -> Phantoms.TTerm Syntax.EndMarkerData
endMarkerDataWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.EndMarkerData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.scala.syntax.EnumCaseDefn
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
-- | DSL accessor for the ctor field of hydra.scala.syntax.EnumCaseDefn
enumCaseDefnCtor :: Phantoms.TTerm Syntax.EnumCaseDefn -> Phantoms.TTerm Syntax.PrimaryCtor
enumCaseDefnCtor x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.EnumCaseDefn"),
        Core.projectionFieldName = (Core.Name "ctor")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the inits field of hydra.scala.syntax.EnumCaseDefn
enumCaseDefnInits :: Phantoms.TTerm Syntax.EnumCaseDefn -> Phantoms.TTerm [Syntax.Init]
enumCaseDefnInits x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.EnumCaseDefn"),
        Core.projectionFieldName = (Core.Name "inits")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the mods field of hydra.scala.syntax.EnumCaseDefn
enumCaseDefnMods :: Phantoms.TTerm Syntax.EnumCaseDefn -> Phantoms.TTerm [Syntax.Mod]
enumCaseDefnMods x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.EnumCaseDefn"),
        Core.projectionFieldName = (Core.Name "mods")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the name field of hydra.scala.syntax.EnumCaseDefn
enumCaseDefnName :: Phantoms.TTerm Syntax.EnumCaseDefn -> Phantoms.TTerm Syntax.NameData
enumCaseDefnName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.EnumCaseDefn"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the tparams field of hydra.scala.syntax.EnumCaseDefn
enumCaseDefnTparams :: Phantoms.TTerm Syntax.EnumCaseDefn -> Phantoms.TTerm [Syntax.ParamType]
enumCaseDefnTparams x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.EnumCaseDefn"),
        Core.projectionFieldName = (Core.Name "tparams")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the ctor field of hydra.scala.syntax.EnumCaseDefn
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
              Core.projectionFieldName = (Core.Name "mods")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.EnumCaseDefn"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.EnumCaseDefn"),
              Core.projectionFieldName = (Core.Name "tparams")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ctor"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "inits"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.EnumCaseDefn"),
              Core.projectionFieldName = (Core.Name "inits")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the inits field of hydra.scala.syntax.EnumCaseDefn
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
              Core.projectionFieldName = (Core.Name "mods")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.EnumCaseDefn"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.EnumCaseDefn"),
              Core.projectionFieldName = (Core.Name "tparams")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ctor"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.EnumCaseDefn"),
              Core.projectionFieldName = (Core.Name "ctor")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inits"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the mods field of hydra.scala.syntax.EnumCaseDefn
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
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.EnumCaseDefn"),
              Core.projectionFieldName = (Core.Name "tparams")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ctor"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.EnumCaseDefn"),
              Core.projectionFieldName = (Core.Name "ctor")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inits"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.EnumCaseDefn"),
              Core.projectionFieldName = (Core.Name "inits")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the name field of hydra.scala.syntax.EnumCaseDefn
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
              Core.projectionFieldName = (Core.Name "mods")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.EnumCaseDefn"),
              Core.projectionFieldName = (Core.Name "tparams")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ctor"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.EnumCaseDefn"),
              Core.projectionFieldName = (Core.Name "ctor")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inits"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.EnumCaseDefn"),
              Core.projectionFieldName = (Core.Name "inits")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the tparams field of hydra.scala.syntax.EnumCaseDefn
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
              Core.projectionFieldName = (Core.Name "mods")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.EnumCaseDefn"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "ctor"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.EnumCaseDefn"),
              Core.projectionFieldName = (Core.Name "ctor")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inits"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.EnumCaseDefn"),
              Core.projectionFieldName = (Core.Name "inits")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for hydra.scala.syntax.EnumDefn
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
-- | DSL accessor for the ctor field of hydra.scala.syntax.EnumDefn
enumDefnCtor :: Phantoms.TTerm Syntax.EnumDefn -> Phantoms.TTerm Syntax.PrimaryCtor
enumDefnCtor x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.EnumDefn"),
        Core.projectionFieldName = (Core.Name "ctor")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the mods field of hydra.scala.syntax.EnumDefn
enumDefnMods :: Phantoms.TTerm Syntax.EnumDefn -> Phantoms.TTerm [Syntax.Mod]
enumDefnMods x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.EnumDefn"),
        Core.projectionFieldName = (Core.Name "mods")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the name field of hydra.scala.syntax.EnumDefn
enumDefnName :: Phantoms.TTerm Syntax.EnumDefn -> Phantoms.TTerm Syntax.NameType
enumDefnName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.EnumDefn"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the template field of hydra.scala.syntax.EnumDefn
enumDefnTemplate :: Phantoms.TTerm Syntax.EnumDefn -> Phantoms.TTerm Syntax.Template
enumDefnTemplate x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.EnumDefn"),
        Core.projectionFieldName = (Core.Name "template")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the tparams field of hydra.scala.syntax.EnumDefn
enumDefnTparams :: Phantoms.TTerm Syntax.EnumDefn -> Phantoms.TTerm [Syntax.ParamType]
enumDefnTparams x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.EnumDefn"),
        Core.projectionFieldName = (Core.Name "tparams")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the ctor field of hydra.scala.syntax.EnumDefn
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
              Core.projectionFieldName = (Core.Name "mods")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.EnumDefn"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.EnumDefn"),
              Core.projectionFieldName = (Core.Name "tparams")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ctor"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "template"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.EnumDefn"),
              Core.projectionFieldName = (Core.Name "template")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the mods field of hydra.scala.syntax.EnumDefn
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
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.EnumDefn"),
              Core.projectionFieldName = (Core.Name "tparams")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ctor"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.EnumDefn"),
              Core.projectionFieldName = (Core.Name "ctor")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "template"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.EnumDefn"),
              Core.projectionFieldName = (Core.Name "template")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the name field of hydra.scala.syntax.EnumDefn
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
              Core.projectionFieldName = (Core.Name "mods")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.EnumDefn"),
              Core.projectionFieldName = (Core.Name "tparams")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ctor"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.EnumDefn"),
              Core.projectionFieldName = (Core.Name "ctor")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "template"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.EnumDefn"),
              Core.projectionFieldName = (Core.Name "template")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the template field of hydra.scala.syntax.EnumDefn
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
              Core.projectionFieldName = (Core.Name "mods")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.EnumDefn"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.EnumDefn"),
              Core.projectionFieldName = (Core.Name "tparams")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ctor"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.EnumDefn"),
              Core.projectionFieldName = (Core.Name "ctor")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "template"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the tparams field of hydra.scala.syntax.EnumDefn
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
              Core.projectionFieldName = (Core.Name "mods")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.EnumDefn"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "ctor"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.EnumDefn"),
              Core.projectionFieldName = (Core.Name "ctor")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "template"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.EnumDefn"),
              Core.projectionFieldName = (Core.Name "template")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL injection for the caseGenerator variant of hydra.scala.syntax.Enumerator
enumeratorCaseGenerator :: Phantoms.TTerm Syntax.CaseGeneratorEnumerator -> Phantoms.TTerm Syntax.Enumerator
enumeratorCaseGenerator x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Enumerator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "caseGenerator"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the generator variant of hydra.scala.syntax.Enumerator
enumeratorGenerator :: Phantoms.TTerm Syntax.GeneratorEnumerator -> Phantoms.TTerm Syntax.Enumerator
enumeratorGenerator x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Enumerator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "generator"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the guard variant of hydra.scala.syntax.Enumerator
enumeratorGuard :: Phantoms.TTerm Syntax.GuardEnumerator -> Phantoms.TTerm Syntax.Enumerator
enumeratorGuard x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Enumerator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "guard"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the val variant of hydra.scala.syntax.Enumerator
enumeratorVal :: Phantoms.TTerm Syntax.ValEnumerator -> Phantoms.TTerm Syntax.Enumerator
enumeratorVal x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Enumerator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "val"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL constructor for hydra.scala.syntax.EtaData
etaData :: Phantoms.TTerm Syntax.Data -> Phantoms.TTerm Syntax.EtaData
etaData expr =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.EtaData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expr"),
          Core.fieldTerm = (Phantoms.unTTerm expr)}]}))
-- | DSL accessor for the expr field of hydra.scala.syntax.EtaData
etaDataExpr :: Phantoms.TTerm Syntax.EtaData -> Phantoms.TTerm Syntax.Data
etaDataExpr x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.EtaData"),
        Core.projectionFieldName = (Core.Name "expr")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the expr field of hydra.scala.syntax.EtaData
etaDataWithExpr :: Phantoms.TTerm Syntax.EtaData -> Phantoms.TTerm Syntax.Data -> Phantoms.TTerm Syntax.EtaData
etaDataWithExpr original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.EtaData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expr"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.scala.syntax.ExistentialType
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
-- | DSL accessor for the stats field of hydra.scala.syntax.ExistentialType
existentialTypeStats :: Phantoms.TTerm Syntax.ExistentialType -> Phantoms.TTerm [Syntax.Stat]
existentialTypeStats x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ExistentialType"),
        Core.projectionFieldName = (Core.Name "stats")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the tpe field of hydra.scala.syntax.ExistentialType
existentialTypeTpe :: Phantoms.TTerm Syntax.ExistentialType -> Phantoms.TTerm Syntax.Type
existentialTypeTpe x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ExistentialType"),
        Core.projectionFieldName = (Core.Name "tpe")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the stats field of hydra.scala.syntax.ExistentialType
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
              Core.projectionFieldName = (Core.Name "tpe")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "stats"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the tpe field of hydra.scala.syntax.ExistentialType
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
              Core.projectionFieldName = (Core.Name "stats")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for hydra.scala.syntax.Export
export :: Phantoms.TTerm [Syntax.Importer] -> Phantoms.TTerm Syntax.Export
export importers =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.Export"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "importers"),
          Core.fieldTerm = (Phantoms.unTTerm importers)}]}))
-- | DSL accessor for the importers field of hydra.scala.syntax.Export
exportImporters :: Phantoms.TTerm Syntax.Export -> Phantoms.TTerm [Syntax.Importer]
exportImporters x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.Export"),
        Core.projectionFieldName = (Core.Name "importers")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the importers field of hydra.scala.syntax.Export
exportWithImporters :: Phantoms.TTerm Syntax.Export -> Phantoms.TTerm [Syntax.Importer] -> Phantoms.TTerm Syntax.Export
exportWithImporters original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.Export"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "importers"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.scala.syntax.ExtensionGroupDefn
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
-- | DSL accessor for the body field of hydra.scala.syntax.ExtensionGroupDefn
extensionGroupDefnBody :: Phantoms.TTerm Syntax.ExtensionGroupDefn -> Phantoms.TTerm Syntax.Stat
extensionGroupDefnBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ExtensionGroupDefn"),
        Core.projectionFieldName = (Core.Name "body")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the parmss field of hydra.scala.syntax.ExtensionGroupDefn
extensionGroupDefnParmss :: Phantoms.TTerm Syntax.ExtensionGroupDefn -> Phantoms.TTerm [[Syntax.ParamData]]
extensionGroupDefnParmss x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ExtensionGroupDefn"),
        Core.projectionFieldName = (Core.Name "parmss")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the tparams field of hydra.scala.syntax.ExtensionGroupDefn
extensionGroupDefnTparams :: Phantoms.TTerm Syntax.ExtensionGroupDefn -> Phantoms.TTerm [Syntax.ParamType]
extensionGroupDefnTparams x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ExtensionGroupDefn"),
        Core.projectionFieldName = (Core.Name "tparams")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the body field of hydra.scala.syntax.ExtensionGroupDefn
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
              Core.projectionFieldName = (Core.Name "tparams")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "parmss"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ExtensionGroupDefn"),
              Core.projectionFieldName = (Core.Name "parmss")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the parmss field of hydra.scala.syntax.ExtensionGroupDefn
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
              Core.projectionFieldName = (Core.Name "tparams")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "parmss"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ExtensionGroupDefn"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the tparams field of hydra.scala.syntax.ExtensionGroupDefn
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
              Core.projectionFieldName = (Core.Name "parmss")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ExtensionGroupDefn"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for hydra.scala.syntax.ExtractInfixPat
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
-- | DSL accessor for the lhs field of hydra.scala.syntax.ExtractInfixPat
extractInfixPatLhs :: Phantoms.TTerm Syntax.ExtractInfixPat -> Phantoms.TTerm Syntax.Pat
extractInfixPatLhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ExtractInfixPat"),
        Core.projectionFieldName = (Core.Name "lhs")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the op field of hydra.scala.syntax.ExtractInfixPat
extractInfixPatOp :: Phantoms.TTerm Syntax.ExtractInfixPat -> Phantoms.TTerm Syntax.NameData
extractInfixPatOp x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ExtractInfixPat"),
        Core.projectionFieldName = (Core.Name "op")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the rhs field of hydra.scala.syntax.ExtractInfixPat
extractInfixPatRhs :: Phantoms.TTerm Syntax.ExtractInfixPat -> Phantoms.TTerm [Syntax.Pat]
extractInfixPatRhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ExtractInfixPat"),
        Core.projectionFieldName = (Core.Name "rhs")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the lhs field of hydra.scala.syntax.ExtractInfixPat
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
              Core.projectionFieldName = (Core.Name "op")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ExtractInfixPat"),
              Core.projectionFieldName = (Core.Name "rhs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the op field of hydra.scala.syntax.ExtractInfixPat
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
              Core.projectionFieldName = (Core.Name "lhs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "op"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ExtractInfixPat"),
              Core.projectionFieldName = (Core.Name "rhs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the rhs field of hydra.scala.syntax.ExtractInfixPat
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
              Core.projectionFieldName = (Core.Name "lhs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "op"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ExtractInfixPat"),
              Core.projectionFieldName = (Core.Name "op")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.scala.syntax.ExtractPat
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
-- | DSL accessor for the args field of hydra.scala.syntax.ExtractPat
extractPatArgs :: Phantoms.TTerm Syntax.ExtractPat -> Phantoms.TTerm [Syntax.Pat]
extractPatArgs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ExtractPat"),
        Core.projectionFieldName = (Core.Name "args")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the fun field of hydra.scala.syntax.ExtractPat
extractPatFun :: Phantoms.TTerm Syntax.ExtractPat -> Phantoms.TTerm Syntax.Data
extractPatFun x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ExtractPat"),
        Core.projectionFieldName = (Core.Name "fun")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the args field of hydra.scala.syntax.ExtractPat
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
              Core.projectionFieldName = (Core.Name "fun")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "args"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the fun field of hydra.scala.syntax.ExtractPat
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
              Core.projectionFieldName = (Core.Name "args")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for hydra.scala.syntax.ForData
forData :: Phantoms.TTerm [Syntax.Enumerator] -> Phantoms.TTerm Syntax.ForData
forData enums =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ForData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "enums"),
          Core.fieldTerm = (Phantoms.unTTerm enums)}]}))
-- | DSL accessor for the enums field of hydra.scala.syntax.ForData
forDataEnums :: Phantoms.TTerm Syntax.ForData -> Phantoms.TTerm [Syntax.Enumerator]
forDataEnums x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ForData"),
        Core.projectionFieldName = (Core.Name "enums")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the enums field of hydra.scala.syntax.ForData
forDataWithEnums :: Phantoms.TTerm Syntax.ForData -> Phantoms.TTerm [Syntax.Enumerator] -> Phantoms.TTerm Syntax.ForData
forDataWithEnums original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ForData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "enums"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.scala.syntax.ForYieldData
forYieldData :: Phantoms.TTerm [Syntax.Enumerator] -> Phantoms.TTerm Syntax.ForYieldData
forYieldData enums =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ForYieldData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "enums"),
          Core.fieldTerm = (Phantoms.unTTerm enums)}]}))
-- | DSL accessor for the enums field of hydra.scala.syntax.ForYieldData
forYieldDataEnums :: Phantoms.TTerm Syntax.ForYieldData -> Phantoms.TTerm [Syntax.Enumerator]
forYieldDataEnums x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ForYieldData"),
        Core.projectionFieldName = (Core.Name "enums")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the enums field of hydra.scala.syntax.ForYieldData
forYieldDataWithEnums :: Phantoms.TTerm Syntax.ForYieldData -> Phantoms.TTerm [Syntax.Enumerator] -> Phantoms.TTerm Syntax.ForYieldData
forYieldDataWithEnums original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ForYieldData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "enums"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.scala.syntax.FunctionData
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
-- | DSL accessor for the body field of hydra.scala.syntax.FunctionData
functionDataBody :: Phantoms.TTerm Syntax.FunctionData -> Phantoms.TTerm Syntax.Data
functionDataBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.FunctionData"),
        Core.projectionFieldName = (Core.Name "body")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the params field of hydra.scala.syntax.FunctionData
functionDataParams :: Phantoms.TTerm Syntax.FunctionData -> Phantoms.TTerm [Syntax.ParamData]
functionDataParams x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.FunctionData"),
        Core.projectionFieldName = (Core.Name "params")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the body field of hydra.scala.syntax.FunctionData
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
              Core.projectionFieldName = (Core.Name "params")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the params field of hydra.scala.syntax.FunctionData
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
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for hydra.scala.syntax.FunctionType
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
-- | DSL accessor for the params field of hydra.scala.syntax.FunctionType
functionTypeParams :: Phantoms.TTerm Syntax.FunctionType -> Phantoms.TTerm [Syntax.Type]
functionTypeParams x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.FunctionType"),
        Core.projectionFieldName = (Core.Name "params")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the res field of hydra.scala.syntax.FunctionType
functionTypeRes :: Phantoms.TTerm Syntax.FunctionType -> Phantoms.TTerm Syntax.Type
functionTypeRes x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.FunctionType"),
        Core.projectionFieldName = (Core.Name "res")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the params field of hydra.scala.syntax.FunctionType
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
              Core.projectionFieldName = (Core.Name "res")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the res field of hydra.scala.syntax.FunctionType
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
              Core.projectionFieldName = (Core.Name "params")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "res"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.scala.syntax.GeneratorEnumerator
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
-- | DSL accessor for the pat field of hydra.scala.syntax.GeneratorEnumerator
generatorEnumeratorPat :: Phantoms.TTerm Syntax.GeneratorEnumerator -> Phantoms.TTerm Syntax.Pat
generatorEnumeratorPat x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GeneratorEnumerator"),
        Core.projectionFieldName = (Core.Name "pat")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the rhs field of hydra.scala.syntax.GeneratorEnumerator
generatorEnumeratorRhs :: Phantoms.TTerm Syntax.GeneratorEnumerator -> Phantoms.TTerm Syntax.Data
generatorEnumeratorRhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GeneratorEnumerator"),
        Core.projectionFieldName = (Core.Name "rhs")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the pat field of hydra.scala.syntax.GeneratorEnumerator
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
              Core.projectionFieldName = (Core.Name "rhs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the rhs field of hydra.scala.syntax.GeneratorEnumerator
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
              Core.projectionFieldName = (Core.Name "pat")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.scala.syntax.GivenAliasDefn
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
-- | DSL accessor for the body field of hydra.scala.syntax.GivenAliasDefn
givenAliasDefnBody :: Phantoms.TTerm Syntax.GivenAliasDefn -> Phantoms.TTerm Syntax.Data
givenAliasDefnBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenAliasDefn"),
        Core.projectionFieldName = (Core.Name "body")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the decltpe field of hydra.scala.syntax.GivenAliasDefn
givenAliasDefnDecltpe :: Phantoms.TTerm Syntax.GivenAliasDefn -> Phantoms.TTerm Syntax.Type
givenAliasDefnDecltpe x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenAliasDefn"),
        Core.projectionFieldName = (Core.Name "decltpe")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the mods field of hydra.scala.syntax.GivenAliasDefn
givenAliasDefnMods :: Phantoms.TTerm Syntax.GivenAliasDefn -> Phantoms.TTerm [Syntax.Mod]
givenAliasDefnMods x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenAliasDefn"),
        Core.projectionFieldName = (Core.Name "mods")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the name field of hydra.scala.syntax.GivenAliasDefn
givenAliasDefnName :: Phantoms.TTerm Syntax.GivenAliasDefn -> Phantoms.TTerm Syntax.Name
givenAliasDefnName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenAliasDefn"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the sparams field of hydra.scala.syntax.GivenAliasDefn
givenAliasDefnSparams :: Phantoms.TTerm Syntax.GivenAliasDefn -> Phantoms.TTerm [[Syntax.ParamData]]
givenAliasDefnSparams x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenAliasDefn"),
        Core.projectionFieldName = (Core.Name "sparams")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the tparams field of hydra.scala.syntax.GivenAliasDefn
givenAliasDefnTparams :: Phantoms.TTerm Syntax.GivenAliasDefn -> Phantoms.TTerm [[Syntax.ParamType]]
givenAliasDefnTparams x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenAliasDefn"),
        Core.projectionFieldName = (Core.Name "tparams")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the body field of hydra.scala.syntax.GivenAliasDefn
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
              Core.projectionFieldName = (Core.Name "mods")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenAliasDefn"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenAliasDefn"),
              Core.projectionFieldName = (Core.Name "tparams")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "sparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenAliasDefn"),
              Core.projectionFieldName = (Core.Name "sparams")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenAliasDefn"),
              Core.projectionFieldName = (Core.Name "decltpe")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the decltpe field of hydra.scala.syntax.GivenAliasDefn
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
              Core.projectionFieldName = (Core.Name "mods")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenAliasDefn"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenAliasDefn"),
              Core.projectionFieldName = (Core.Name "tparams")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "sparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenAliasDefn"),
              Core.projectionFieldName = (Core.Name "sparams")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenAliasDefn"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the mods field of hydra.scala.syntax.GivenAliasDefn
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
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenAliasDefn"),
              Core.projectionFieldName = (Core.Name "tparams")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "sparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenAliasDefn"),
              Core.projectionFieldName = (Core.Name "sparams")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenAliasDefn"),
              Core.projectionFieldName = (Core.Name "decltpe")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenAliasDefn"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the name field of hydra.scala.syntax.GivenAliasDefn
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
              Core.projectionFieldName = (Core.Name "mods")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenAliasDefn"),
              Core.projectionFieldName = (Core.Name "tparams")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "sparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenAliasDefn"),
              Core.projectionFieldName = (Core.Name "sparams")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenAliasDefn"),
              Core.projectionFieldName = (Core.Name "decltpe")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenAliasDefn"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the sparams field of hydra.scala.syntax.GivenAliasDefn
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
              Core.projectionFieldName = (Core.Name "mods")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenAliasDefn"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenAliasDefn"),
              Core.projectionFieldName = (Core.Name "tparams")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "sparams"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenAliasDefn"),
              Core.projectionFieldName = (Core.Name "decltpe")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenAliasDefn"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the tparams field of hydra.scala.syntax.GivenAliasDefn
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
              Core.projectionFieldName = (Core.Name "mods")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenAliasDefn"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "sparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenAliasDefn"),
              Core.projectionFieldName = (Core.Name "sparams")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenAliasDefn"),
              Core.projectionFieldName = (Core.Name "decltpe")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenAliasDefn"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for hydra.scala.syntax.GivenDecl
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
-- | DSL accessor for the decltpe field of hydra.scala.syntax.GivenDecl
givenDeclDecltpe :: Phantoms.TTerm Syntax.GivenDecl -> Phantoms.TTerm Syntax.Type
givenDeclDecltpe x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenDecl"),
        Core.projectionFieldName = (Core.Name "decltpe")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the mods field of hydra.scala.syntax.GivenDecl
givenDeclMods :: Phantoms.TTerm Syntax.GivenDecl -> Phantoms.TTerm [Syntax.Mod]
givenDeclMods x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenDecl"),
        Core.projectionFieldName = (Core.Name "mods")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the name field of hydra.scala.syntax.GivenDecl
givenDeclName :: Phantoms.TTerm Syntax.GivenDecl -> Phantoms.TTerm Syntax.NameData
givenDeclName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenDecl"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the sparams field of hydra.scala.syntax.GivenDecl
givenDeclSparams :: Phantoms.TTerm Syntax.GivenDecl -> Phantoms.TTerm [[Syntax.ParamData]]
givenDeclSparams x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenDecl"),
        Core.projectionFieldName = (Core.Name "sparams")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the tparams field of hydra.scala.syntax.GivenDecl
givenDeclTparams :: Phantoms.TTerm Syntax.GivenDecl -> Phantoms.TTerm [Syntax.ParamType]
givenDeclTparams x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenDecl"),
        Core.projectionFieldName = (Core.Name "tparams")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the decltpe field of hydra.scala.syntax.GivenDecl
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
              Core.projectionFieldName = (Core.Name "mods")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenDecl"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenDecl"),
              Core.projectionFieldName = (Core.Name "tparams")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "sparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenDecl"),
              Core.projectionFieldName = (Core.Name "sparams")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the mods field of hydra.scala.syntax.GivenDecl
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
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenDecl"),
              Core.projectionFieldName = (Core.Name "tparams")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "sparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenDecl"),
              Core.projectionFieldName = (Core.Name "sparams")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenDecl"),
              Core.projectionFieldName = (Core.Name "decltpe")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the name field of hydra.scala.syntax.GivenDecl
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
              Core.projectionFieldName = (Core.Name "mods")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenDecl"),
              Core.projectionFieldName = (Core.Name "tparams")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "sparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenDecl"),
              Core.projectionFieldName = (Core.Name "sparams")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenDecl"),
              Core.projectionFieldName = (Core.Name "decltpe")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the sparams field of hydra.scala.syntax.GivenDecl
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
              Core.projectionFieldName = (Core.Name "mods")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenDecl"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenDecl"),
              Core.projectionFieldName = (Core.Name "tparams")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "sparams"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenDecl"),
              Core.projectionFieldName = (Core.Name "decltpe")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the tparams field of hydra.scala.syntax.GivenDecl
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
              Core.projectionFieldName = (Core.Name "mods")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenDecl"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "sparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenDecl"),
              Core.projectionFieldName = (Core.Name "sparams")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenDecl"),
              Core.projectionFieldName = (Core.Name "decltpe")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for hydra.scala.syntax.GivenDefn
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
-- | DSL accessor for the mods field of hydra.scala.syntax.GivenDefn
givenDefnMods :: Phantoms.TTerm Syntax.GivenDefn -> Phantoms.TTerm [Syntax.Mod]
givenDefnMods x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenDefn"),
        Core.projectionFieldName = (Core.Name "mods")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the name field of hydra.scala.syntax.GivenDefn
givenDefnName :: Phantoms.TTerm Syntax.GivenDefn -> Phantoms.TTerm Syntax.Name
givenDefnName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenDefn"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the sparams field of hydra.scala.syntax.GivenDefn
givenDefnSparams :: Phantoms.TTerm Syntax.GivenDefn -> Phantoms.TTerm [[Syntax.ParamData]]
givenDefnSparams x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenDefn"),
        Core.projectionFieldName = (Core.Name "sparams")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the templ field of hydra.scala.syntax.GivenDefn
givenDefnTempl :: Phantoms.TTerm Syntax.GivenDefn -> Phantoms.TTerm Syntax.Template
givenDefnTempl x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenDefn"),
        Core.projectionFieldName = (Core.Name "templ")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the tparams field of hydra.scala.syntax.GivenDefn
givenDefnTparams :: Phantoms.TTerm Syntax.GivenDefn -> Phantoms.TTerm [[Syntax.ParamType]]
givenDefnTparams x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenDefn"),
        Core.projectionFieldName = (Core.Name "tparams")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the mods field of hydra.scala.syntax.GivenDefn
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
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenDefn"),
              Core.projectionFieldName = (Core.Name "tparams")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "sparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenDefn"),
              Core.projectionFieldName = (Core.Name "sparams")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "templ"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenDefn"),
              Core.projectionFieldName = (Core.Name "templ")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the name field of hydra.scala.syntax.GivenDefn
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
              Core.projectionFieldName = (Core.Name "mods")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenDefn"),
              Core.projectionFieldName = (Core.Name "tparams")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "sparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenDefn"),
              Core.projectionFieldName = (Core.Name "sparams")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "templ"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenDefn"),
              Core.projectionFieldName = (Core.Name "templ")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the sparams field of hydra.scala.syntax.GivenDefn
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
              Core.projectionFieldName = (Core.Name "mods")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenDefn"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenDefn"),
              Core.projectionFieldName = (Core.Name "tparams")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "sparams"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "templ"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenDefn"),
              Core.projectionFieldName = (Core.Name "templ")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the templ field of hydra.scala.syntax.GivenDefn
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
              Core.projectionFieldName = (Core.Name "mods")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenDefn"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenDefn"),
              Core.projectionFieldName = (Core.Name "tparams")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "sparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenDefn"),
              Core.projectionFieldName = (Core.Name "sparams")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "templ"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the tparams field of hydra.scala.syntax.GivenDefn
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
              Core.projectionFieldName = (Core.Name "mods")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenDefn"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "sparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenDefn"),
              Core.projectionFieldName = (Core.Name "sparams")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "templ"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenDefn"),
              Core.projectionFieldName = (Core.Name "templ")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for hydra.scala.syntax.GivenImportee
givenImportee :: Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.GivenImportee
givenImportee tpe =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.GivenImportee"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "tpe"),
          Core.fieldTerm = (Phantoms.unTTerm tpe)}]}))
-- | DSL accessor for the tpe field of hydra.scala.syntax.GivenImportee
givenImporteeTpe :: Phantoms.TTerm Syntax.GivenImportee -> Phantoms.TTerm Syntax.Type
givenImporteeTpe x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenImportee"),
        Core.projectionFieldName = (Core.Name "tpe")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the tpe field of hydra.scala.syntax.GivenImportee
givenImporteeWithTpe :: Phantoms.TTerm Syntax.GivenImportee -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.GivenImportee
givenImporteeWithTpe original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.GivenImportee"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "tpe"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.scala.syntax.GivenPat
givenPat :: Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.GivenPat
givenPat tpe =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.GivenPat"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "tpe"),
          Core.fieldTerm = (Phantoms.unTTerm tpe)}]}))
-- | DSL accessor for the tpe field of hydra.scala.syntax.GivenPat
givenPatTpe :: Phantoms.TTerm Syntax.GivenPat -> Phantoms.TTerm Syntax.Type
givenPatTpe x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenPat"),
        Core.projectionFieldName = (Core.Name "tpe")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the tpe field of hydra.scala.syntax.GivenPat
givenPatWithTpe :: Phantoms.TTerm Syntax.GivenPat -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.GivenPat
givenPatWithTpe original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.GivenPat"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "tpe"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.scala.syntax.GuardEnumerator
guardEnumerator :: Phantoms.TTerm Syntax.Data -> Phantoms.TTerm Syntax.GuardEnumerator
guardEnumerator cond =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.GuardEnumerator"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "cond"),
          Core.fieldTerm = (Phantoms.unTTerm cond)}]}))
-- | DSL accessor for the cond field of hydra.scala.syntax.GuardEnumerator
guardEnumeratorCond :: Phantoms.TTerm Syntax.GuardEnumerator -> Phantoms.TTerm Syntax.Data
guardEnumeratorCond x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GuardEnumerator"),
        Core.projectionFieldName = (Core.Name "cond")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the cond field of hydra.scala.syntax.GuardEnumerator
guardEnumeratorWithCond :: Phantoms.TTerm Syntax.GuardEnumerator -> Phantoms.TTerm Syntax.Data -> Phantoms.TTerm Syntax.GuardEnumerator
guardEnumeratorWithCond original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.GuardEnumerator"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "cond"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.scala.syntax.IfData
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
-- | DSL accessor for the cond field of hydra.scala.syntax.IfData
ifDataCond :: Phantoms.TTerm Syntax.IfData -> Phantoms.TTerm Syntax.Data
ifDataCond x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.IfData"),
        Core.projectionFieldName = (Core.Name "cond")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the elsep field of hydra.scala.syntax.IfData
ifDataElsep :: Phantoms.TTerm Syntax.IfData -> Phantoms.TTerm Syntax.Data
ifDataElsep x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.IfData"),
        Core.projectionFieldName = (Core.Name "elsep")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the thenp field of hydra.scala.syntax.IfData
ifDataThenp :: Phantoms.TTerm Syntax.IfData -> Phantoms.TTerm Syntax.Data
ifDataThenp x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.IfData"),
        Core.projectionFieldName = (Core.Name "thenp")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the cond field of hydra.scala.syntax.IfData
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
              Core.projectionFieldName = (Core.Name "thenp")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "elsep"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.IfData"),
              Core.projectionFieldName = (Core.Name "elsep")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the elsep field of hydra.scala.syntax.IfData
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
              Core.projectionFieldName = (Core.Name "cond")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "thenp"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.IfData"),
              Core.projectionFieldName = (Core.Name "thenp")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "elsep"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the thenp field of hydra.scala.syntax.IfData
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
              Core.projectionFieldName = (Core.Name "cond")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "thenp"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "elsep"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.IfData"),
              Core.projectionFieldName = (Core.Name "elsep")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for hydra.scala.syntax.ImplicitFunctionType
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
-- | DSL accessor for the params field of hydra.scala.syntax.ImplicitFunctionType
implicitFunctionTypeParams :: Phantoms.TTerm Syntax.ImplicitFunctionType -> Phantoms.TTerm [Syntax.Type]
implicitFunctionTypeParams x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ImplicitFunctionType"),
        Core.projectionFieldName = (Core.Name "params")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the res field of hydra.scala.syntax.ImplicitFunctionType
implicitFunctionTypeRes :: Phantoms.TTerm Syntax.ImplicitFunctionType -> Phantoms.TTerm Syntax.Type
implicitFunctionTypeRes x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ImplicitFunctionType"),
        Core.projectionFieldName = (Core.Name "res")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the params field of hydra.scala.syntax.ImplicitFunctionType
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
              Core.projectionFieldName = (Core.Name "res")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the res field of hydra.scala.syntax.ImplicitFunctionType
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
              Core.projectionFieldName = (Core.Name "params")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "res"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.scala.syntax.Import
import_ :: Phantoms.TTerm [Syntax.Importer] -> Phantoms.TTerm Syntax.Import
import_ importers =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.Import"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "importers"),
          Core.fieldTerm = (Phantoms.unTTerm importers)}]}))
-- | DSL injection for the export variant of hydra.scala.syntax.ImportExportStat
importExportStatExport :: Phantoms.TTerm Syntax.Export -> Phantoms.TTerm Syntax.ImportExportStat
importExportStatExport x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.ImportExportStat"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "export"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the import variant of hydra.scala.syntax.ImportExportStat
importExportStatImport :: Phantoms.TTerm Syntax.Import -> Phantoms.TTerm Syntax.ImportExportStat
importExportStatImport x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.ImportExportStat"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "import"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL accessor for the importers field of hydra.scala.syntax.Import
importImporters :: Phantoms.TTerm Syntax.Import -> Phantoms.TTerm [Syntax.Importer]
importImporters x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.Import"),
        Core.projectionFieldName = (Core.Name "importers")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the importers field of hydra.scala.syntax.Import
importWithImporters :: Phantoms.TTerm Syntax.Import -> Phantoms.TTerm [Syntax.Importer] -> Phantoms.TTerm Syntax.Import
importWithImporters original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.Import"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "importers"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL injection for the given variant of hydra.scala.syntax.Importee
importeeGiven :: Phantoms.TTerm Syntax.GivenImportee -> Phantoms.TTerm Syntax.Importee
importeeGiven x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Importee"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "given"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the givenAll variant of hydra.scala.syntax.Importee
importeeGivenAll :: Phantoms.TTerm Syntax.Importee
importeeGivenAll =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Importee"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "givenAll"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the name variant of hydra.scala.syntax.Importee
importeeName :: Phantoms.TTerm Syntax.NameImportee -> Phantoms.TTerm Syntax.Importee
importeeName x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Importee"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "name"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the rename variant of hydra.scala.syntax.Importee
importeeRename :: Phantoms.TTerm Syntax.RenameImportee -> Phantoms.TTerm Syntax.Importee
importeeRename x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Importee"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "rename"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the unimport variant of hydra.scala.syntax.Importee
importeeUnimport :: Phantoms.TTerm Syntax.UnimportImportee -> Phantoms.TTerm Syntax.Importee
importeeUnimport x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Importee"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "unimport"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the wildcard variant of hydra.scala.syntax.Importee
importeeWildcard :: Phantoms.TTerm Syntax.Importee
importeeWildcard =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Importee"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "wildcard"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL constructor for hydra.scala.syntax.Importer
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
-- | DSL accessor for the importees field of hydra.scala.syntax.Importer
importerImportees :: Phantoms.TTerm Syntax.Importer -> Phantoms.TTerm [Syntax.Importee]
importerImportees x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.Importer"),
        Core.projectionFieldName = (Core.Name "importees")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the ref field of hydra.scala.syntax.Importer
importerRef :: Phantoms.TTerm Syntax.Importer -> Phantoms.TTerm Syntax.RefData
importerRef x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.Importer"),
        Core.projectionFieldName = (Core.Name "ref")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the importees field of hydra.scala.syntax.Importer
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
              Core.projectionFieldName = (Core.Name "ref")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "importees"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the ref field of hydra.scala.syntax.Importer
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
              Core.projectionFieldName = (Core.Name "importees")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for hydra.scala.syntax.Init
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
-- | DSL accessor for the argss field of hydra.scala.syntax.Init
initArgss :: Phantoms.TTerm Syntax.Init -> Phantoms.TTerm [[Syntax.Data]]
initArgss x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.Init"),
        Core.projectionFieldName = (Core.Name "argss")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the name field of hydra.scala.syntax.Init
initName :: Phantoms.TTerm Syntax.Init -> Phantoms.TTerm Syntax.Name
initName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.Init"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the tpe field of hydra.scala.syntax.Init
initTpe :: Phantoms.TTerm Syntax.Init -> Phantoms.TTerm Syntax.Type
initTpe x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.Init"),
        Core.projectionFieldName = (Core.Name "tpe")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the argss field of hydra.scala.syntax.Init
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
              Core.projectionFieldName = (Core.Name "tpe")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.Init"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "argss"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the name field of hydra.scala.syntax.Init
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
              Core.projectionFieldName = (Core.Name "tpe")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "argss"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.Init"),
              Core.projectionFieldName = (Core.Name "argss")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the tpe field of hydra.scala.syntax.Init
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
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "argss"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.Init"),
              Core.projectionFieldName = (Core.Name "argss")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for hydra.scala.syntax.InterpolateData
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
-- | DSL accessor for the args field of hydra.scala.syntax.InterpolateData
interpolateDataArgs :: Phantoms.TTerm Syntax.InterpolateData -> Phantoms.TTerm [Syntax.Data]
interpolateDataArgs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.InterpolateData"),
        Core.projectionFieldName = (Core.Name "args")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the parts field of hydra.scala.syntax.InterpolateData
interpolateDataParts :: Phantoms.TTerm Syntax.InterpolateData -> Phantoms.TTerm [Syntax.Lit]
interpolateDataParts x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.InterpolateData"),
        Core.projectionFieldName = (Core.Name "parts")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the prefix field of hydra.scala.syntax.InterpolateData
interpolateDataPrefix :: Phantoms.TTerm Syntax.InterpolateData -> Phantoms.TTerm Syntax.NameData
interpolateDataPrefix x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.InterpolateData"),
        Core.projectionFieldName = (Core.Name "prefix")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the args field of hydra.scala.syntax.InterpolateData
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
              Core.projectionFieldName = (Core.Name "prefix")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "parts"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.InterpolateData"),
              Core.projectionFieldName = (Core.Name "parts")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "args"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the parts field of hydra.scala.syntax.InterpolateData
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
              Core.projectionFieldName = (Core.Name "prefix")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "parts"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "args"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.InterpolateData"),
              Core.projectionFieldName = (Core.Name "args")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the prefix field of hydra.scala.syntax.InterpolateData
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
              Core.projectionFieldName = (Core.Name "parts")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "args"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.InterpolateData"),
              Core.projectionFieldName = (Core.Name "args")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for hydra.scala.syntax.InterpolatePat
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
-- | DSL accessor for the parts field of hydra.scala.syntax.InterpolatePat
interpolatePatParts :: Phantoms.TTerm Syntax.InterpolatePat -> Phantoms.TTerm [Syntax.Lit]
interpolatePatParts x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.InterpolatePat"),
        Core.projectionFieldName = (Core.Name "parts")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the prefix field of hydra.scala.syntax.InterpolatePat
interpolatePatPrefix :: Phantoms.TTerm Syntax.InterpolatePat -> Phantoms.TTerm Syntax.NameData
interpolatePatPrefix x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.InterpolatePat"),
        Core.projectionFieldName = (Core.Name "prefix")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the parts field of hydra.scala.syntax.InterpolatePat
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
              Core.projectionFieldName = (Core.Name "prefix")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "parts"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the prefix field of hydra.scala.syntax.InterpolatePat
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
              Core.projectionFieldName = (Core.Name "parts")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for hydra.scala.syntax.LambdaType
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
-- | DSL accessor for the tparams field of hydra.scala.syntax.LambdaType
lambdaTypeTparams :: Phantoms.TTerm Syntax.LambdaType -> Phantoms.TTerm [Syntax.ParamType]
lambdaTypeTparams x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.LambdaType"),
        Core.projectionFieldName = (Core.Name "tparams")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the tpe field of hydra.scala.syntax.LambdaType
lambdaTypeTpe :: Phantoms.TTerm Syntax.LambdaType -> Phantoms.TTerm Syntax.Type
lambdaTypeTpe x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.LambdaType"),
        Core.projectionFieldName = (Core.Name "tpe")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the tparams field of hydra.scala.syntax.LambdaType
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
              Core.projectionFieldName = (Core.Name "tpe")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the tpe field of hydra.scala.syntax.LambdaType
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
              Core.projectionFieldName = (Core.Name "tparams")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tpe"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL injection for the boolean variant of hydra.scala.syntax.Lit
litBoolean :: Phantoms.TTerm Bool -> Phantoms.TTerm Syntax.Lit
litBoolean x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Lit"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "boolean"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the byte variant of hydra.scala.syntax.Lit
litByte :: Phantoms.TTerm I.Int8 -> Phantoms.TTerm Syntax.Lit
litByte x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Lit"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "byte"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the bytes variant of hydra.scala.syntax.Lit
litBytes :: Phantoms.TTerm [Int] -> Phantoms.TTerm Syntax.Lit
litBytes x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Lit"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "bytes"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the char variant of hydra.scala.syntax.Lit
litChar :: Phantoms.TTerm Int -> Phantoms.TTerm Syntax.Lit
litChar x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Lit"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "char"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the double variant of hydra.scala.syntax.Lit
litDouble :: Phantoms.TTerm Double -> Phantoms.TTerm Syntax.Lit
litDouble x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Lit"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "double"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the float variant of hydra.scala.syntax.Lit
litFloat :: Phantoms.TTerm Float -> Phantoms.TTerm Syntax.Lit
litFloat x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Lit"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "float"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the int variant of hydra.scala.syntax.Lit
litInt :: Phantoms.TTerm Int -> Phantoms.TTerm Syntax.Lit
litInt x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Lit"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "int"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the long variant of hydra.scala.syntax.Lit
litLong :: Phantoms.TTerm I.Int64 -> Phantoms.TTerm Syntax.Lit
litLong x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Lit"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "long"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the null variant of hydra.scala.syntax.Lit
litNull :: Phantoms.TTerm Syntax.Lit
litNull =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Lit"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "null"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the short variant of hydra.scala.syntax.Lit
litShort :: Phantoms.TTerm I.Int16 -> Phantoms.TTerm Syntax.Lit
litShort x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Lit"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "short"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the string variant of hydra.scala.syntax.Lit
litString :: Phantoms.TTerm String -> Phantoms.TTerm Syntax.Lit
litString x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Lit"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "string"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the symbol variant of hydra.scala.syntax.Lit
litSymbol :: Phantoms.TTerm Syntax.ScalaSymbol -> Phantoms.TTerm Syntax.Lit
litSymbol x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Lit"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "symbol"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the unit variant of hydra.scala.syntax.Lit
litUnit :: Phantoms.TTerm Syntax.Lit
litUnit =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Lit"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "unit"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL constructor for hydra.scala.syntax.MatchData
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
-- | DSL accessor for the cases field of hydra.scala.syntax.MatchData
matchDataCases :: Phantoms.TTerm Syntax.MatchData -> Phantoms.TTerm [Syntax.Case]
matchDataCases x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.MatchData"),
        Core.projectionFieldName = (Core.Name "cases")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the expr field of hydra.scala.syntax.MatchData
matchDataExpr :: Phantoms.TTerm Syntax.MatchData -> Phantoms.TTerm Syntax.Data
matchDataExpr x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.MatchData"),
        Core.projectionFieldName = (Core.Name "expr")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the cases field of hydra.scala.syntax.MatchData
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
              Core.projectionFieldName = (Core.Name "expr")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "cases"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the expr field of hydra.scala.syntax.MatchData
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
              Core.projectionFieldName = (Core.Name "cases")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for hydra.scala.syntax.MatchType
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
-- | DSL accessor for the cases field of hydra.scala.syntax.MatchType
matchTypeCases :: Phantoms.TTerm Syntax.MatchType -> Phantoms.TTerm [Syntax.TypeCase]
matchTypeCases x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.MatchType"),
        Core.projectionFieldName = (Core.Name "cases")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the tpe field of hydra.scala.syntax.MatchType
matchTypeTpe :: Phantoms.TTerm Syntax.MatchType -> Phantoms.TTerm Syntax.Type
matchTypeTpe x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.MatchType"),
        Core.projectionFieldName = (Core.Name "tpe")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the cases field of hydra.scala.syntax.MatchType
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
              Core.projectionFieldName = (Core.Name "tpe")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "cases"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the tpe field of hydra.scala.syntax.MatchType
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
              Core.projectionFieldName = (Core.Name "cases")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL injection for the self variant of hydra.scala.syntax.Member
memberSelf :: Phantoms.TTerm Syntax.Self -> Phantoms.TTerm Syntax.Member
memberSelf x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Member"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "self"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the term variant of hydra.scala.syntax.Member
memberTerm :: Phantoms.TTerm Syntax.DataMember -> Phantoms.TTerm Syntax.Member
memberTerm x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Member"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "term"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the termParam variant of hydra.scala.syntax.Member
memberTermParam :: Phantoms.TTerm Syntax.ParamData -> Phantoms.TTerm Syntax.Member
memberTermParam x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Member"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "termParam"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the type variant of hydra.scala.syntax.Member
memberType :: Phantoms.TTerm Syntax.TypeMember -> Phantoms.TTerm Syntax.Member
memberType x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Member"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "type"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the typeParam variant of hydra.scala.syntax.Member
memberTypeParam :: Phantoms.TTerm Syntax.ParamType -> Phantoms.TTerm Syntax.Member
memberTypeParam x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Member"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "typeParam"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL constructor for hydra.scala.syntax.MethodType
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
-- | DSL accessor for the paramss field of hydra.scala.syntax.MethodType
methodTypeParamss :: Phantoms.TTerm Syntax.MethodType -> Phantoms.TTerm [[Syntax.ParamData]]
methodTypeParamss x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.MethodType"),
        Core.projectionFieldName = (Core.Name "paramss")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the tpe field of hydra.scala.syntax.MethodType
methodTypeTpe :: Phantoms.TTerm Syntax.MethodType -> Phantoms.TTerm Syntax.Type
methodTypeTpe x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.MethodType"),
        Core.projectionFieldName = (Core.Name "tpe")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the paramss field of hydra.scala.syntax.MethodType
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
              Core.projectionFieldName = (Core.Name "tpe")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the tpe field of hydra.scala.syntax.MethodType
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
              Core.projectionFieldName = (Core.Name "paramss")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tpe"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL injection for the abstract variant of hydra.scala.syntax.Mod
modAbstract :: Phantoms.TTerm Syntax.Mod
modAbstract =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Mod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "abstract"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the annot variant of hydra.scala.syntax.Mod
modAnnot :: Phantoms.TTerm Syntax.AnnotMod -> Phantoms.TTerm Syntax.Mod
modAnnot x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Mod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "annot"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the case variant of hydra.scala.syntax.Mod
modCase :: Phantoms.TTerm Syntax.Mod
modCase =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Mod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "case"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the contravariant variant of hydra.scala.syntax.Mod
modContravariant :: Phantoms.TTerm Syntax.Mod
modContravariant =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Mod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "contravariant"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the covariant variant of hydra.scala.syntax.Mod
modCovariant :: Phantoms.TTerm Syntax.Mod
modCovariant =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Mod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "covariant"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the final variant of hydra.scala.syntax.Mod
modFinal :: Phantoms.TTerm Syntax.Mod
modFinal =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Mod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "final"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the implicit variant of hydra.scala.syntax.Mod
modImplicit :: Phantoms.TTerm Syntax.Mod
modImplicit =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Mod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "implicit"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the infix variant of hydra.scala.syntax.Mod
modInfix :: Phantoms.TTerm Syntax.Mod
modInfix =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Mod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "infix"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the inline variant of hydra.scala.syntax.Mod
modInline :: Phantoms.TTerm Syntax.Mod
modInline =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Mod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "inline"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the lazy variant of hydra.scala.syntax.Mod
modLazy :: Phantoms.TTerm Syntax.Mod
modLazy =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Mod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "lazy"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the opaque variant of hydra.scala.syntax.Mod
modOpaque :: Phantoms.TTerm Syntax.Mod
modOpaque =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Mod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "opaque"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the open variant of hydra.scala.syntax.Mod
modOpen :: Phantoms.TTerm Syntax.Mod
modOpen =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Mod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "open"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the override variant of hydra.scala.syntax.Mod
modOverride :: Phantoms.TTerm Syntax.Mod
modOverride =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Mod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "override"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the private variant of hydra.scala.syntax.Mod
modPrivate :: Phantoms.TTerm Syntax.PrivateMod -> Phantoms.TTerm Syntax.Mod
modPrivate x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Mod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "private"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the protected variant of hydra.scala.syntax.Mod
modProtected :: Phantoms.TTerm Syntax.ProtectedMod -> Phantoms.TTerm Syntax.Mod
modProtected x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Mod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "protected"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the sealed variant of hydra.scala.syntax.Mod
modSealed :: Phantoms.TTerm Syntax.Mod
modSealed =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Mod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "sealed"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the super variant of hydra.scala.syntax.Mod
modSuper :: Phantoms.TTerm Syntax.Mod
modSuper =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Mod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "super"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the transparent variant of hydra.scala.syntax.Mod
modTransparent :: Phantoms.TTerm Syntax.Mod
modTransparent =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Mod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "transparent"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the using variant of hydra.scala.syntax.Mod
modUsing :: Phantoms.TTerm Syntax.Mod
modUsing =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Mod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "using"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the valParam variant of hydra.scala.syntax.Mod
modValParam :: Phantoms.TTerm Syntax.Mod
modValParam =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Mod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "valParam"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the varParam variant of hydra.scala.syntax.Mod
modVarParam :: Phantoms.TTerm Syntax.Mod
modVarParam =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Mod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "varParam"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the anonymous variant of hydra.scala.syntax.Name
nameAnonymous :: Phantoms.TTerm Syntax.Name
nameAnonymous =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Name"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "anonymous"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL constructor for hydra.scala.syntax.NameData
nameData :: Phantoms.TTerm Syntax.PredefString -> Phantoms.TTerm Syntax.NameData
nameData value =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.NameData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm value)}]}))
-- | DSL accessor for the value field of hydra.scala.syntax.NameData
nameDataValue :: Phantoms.TTerm Syntax.NameData -> Phantoms.TTerm Syntax.PredefString
nameDataValue x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.NameData"),
        Core.projectionFieldName = (Core.Name "value")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the value field of hydra.scala.syntax.NameData
nameDataWithValue :: Phantoms.TTerm Syntax.NameData -> Phantoms.TTerm Syntax.PredefString -> Phantoms.TTerm Syntax.NameData
nameDataWithValue original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.NameData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.scala.syntax.NameImportee
nameImportee :: Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.NameImportee
nameImportee name =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.NameImportee"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)}]}))
-- | DSL accessor for the name field of hydra.scala.syntax.NameImportee
nameImporteeName :: Phantoms.TTerm Syntax.NameImportee -> Phantoms.TTerm Syntax.Name
nameImporteeName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.NameImportee"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the name field of hydra.scala.syntax.NameImportee
nameImporteeWithName :: Phantoms.TTerm Syntax.NameImportee -> Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.NameImportee
nameImporteeWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.NameImportee"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL injection for the indeterminate variant of hydra.scala.syntax.Name
nameIndeterminate :: Phantoms.TTerm Syntax.PredefString -> Phantoms.TTerm Syntax.Name
nameIndeterminate x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Name"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "indeterminate"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL constructor for hydra.scala.syntax.NameType
nameType :: Phantoms.TTerm String -> Phantoms.TTerm Syntax.NameType
nameType value =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.NameType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm value)}]}))
-- | DSL accessor for the value field of hydra.scala.syntax.NameType
nameTypeValue :: Phantoms.TTerm Syntax.NameType -> Phantoms.TTerm String
nameTypeValue x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.NameType"),
        Core.projectionFieldName = (Core.Name "value")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the value field of hydra.scala.syntax.NameType
nameTypeWithValue :: Phantoms.TTerm Syntax.NameType -> Phantoms.TTerm String -> Phantoms.TTerm Syntax.NameType
nameTypeWithValue original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.NameType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL injection for the value variant of hydra.scala.syntax.Name
nameValue :: Phantoms.TTerm String -> Phantoms.TTerm Syntax.Name
nameValue x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Name"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "value"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL constructor for hydra.scala.syntax.NewAnonymousData
newAnonymousData :: Phantoms.TTerm Syntax.Template -> Phantoms.TTerm Syntax.NewAnonymousData
newAnonymousData templ =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.NewAnonymousData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "templ"),
          Core.fieldTerm = (Phantoms.unTTerm templ)}]}))
-- | DSL accessor for the templ field of hydra.scala.syntax.NewAnonymousData
newAnonymousDataTempl :: Phantoms.TTerm Syntax.NewAnonymousData -> Phantoms.TTerm Syntax.Template
newAnonymousDataTempl x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.NewAnonymousData"),
        Core.projectionFieldName = (Core.Name "templ")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the templ field of hydra.scala.syntax.NewAnonymousData
newAnonymousDataWithTempl :: Phantoms.TTerm Syntax.NewAnonymousData -> Phantoms.TTerm Syntax.Template -> Phantoms.TTerm Syntax.NewAnonymousData
newAnonymousDataWithTempl original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.NewAnonymousData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "templ"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.scala.syntax.NewData
newData :: Phantoms.TTerm Syntax.Init -> Phantoms.TTerm Syntax.NewData
newData init =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.NewData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "init"),
          Core.fieldTerm = (Phantoms.unTTerm init)}]}))
-- | DSL accessor for the init field of hydra.scala.syntax.NewData
newDataInit :: Phantoms.TTerm Syntax.NewData -> Phantoms.TTerm Syntax.Init
newDataInit x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.NewData"),
        Core.projectionFieldName = (Core.Name "init")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the init field of hydra.scala.syntax.NewData
newDataWithInit :: Phantoms.TTerm Syntax.NewData -> Phantoms.TTerm Syntax.Init -> Phantoms.TTerm Syntax.NewData
newDataWithInit original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.NewData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "init"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.scala.syntax.ObjectDefn
objectDefn :: Phantoms.TTerm Syntax.NameData -> Phantoms.TTerm Syntax.ObjectDefn
objectDefn name =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ObjectDefn"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)}]}))
-- | DSL accessor for the name field of hydra.scala.syntax.ObjectDefn
objectDefnName :: Phantoms.TTerm Syntax.ObjectDefn -> Phantoms.TTerm Syntax.NameData
objectDefnName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ObjectDefn"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the name field of hydra.scala.syntax.ObjectDefn
objectDefnWithName :: Phantoms.TTerm Syntax.ObjectDefn -> Phantoms.TTerm Syntax.NameData -> Phantoms.TTerm Syntax.ObjectDefn
objectDefnWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ObjectDefn"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.scala.syntax.ObjectPkg
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
-- | DSL accessor for the mods field of hydra.scala.syntax.ObjectPkg
objectPkgMods :: Phantoms.TTerm Syntax.ObjectPkg -> Phantoms.TTerm [Syntax.Mod]
objectPkgMods x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ObjectPkg"),
        Core.projectionFieldName = (Core.Name "mods")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the name field of hydra.scala.syntax.ObjectPkg
objectPkgName :: Phantoms.TTerm Syntax.ObjectPkg -> Phantoms.TTerm Syntax.NameData
objectPkgName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ObjectPkg"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the template field of hydra.scala.syntax.ObjectPkg
objectPkgTemplate :: Phantoms.TTerm Syntax.ObjectPkg -> Phantoms.TTerm Syntax.Template
objectPkgTemplate x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ObjectPkg"),
        Core.projectionFieldName = (Core.Name "template")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the mods field of hydra.scala.syntax.ObjectPkg
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
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "template"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ObjectPkg"),
              Core.projectionFieldName = (Core.Name "template")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the name field of hydra.scala.syntax.ObjectPkg
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
              Core.projectionFieldName = (Core.Name "mods")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "template"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ObjectPkg"),
              Core.projectionFieldName = (Core.Name "template")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the template field of hydra.scala.syntax.ObjectPkg
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
              Core.projectionFieldName = (Core.Name "mods")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ObjectPkg"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "template"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.scala.syntax.OrType
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
-- | DSL accessor for the lhs field of hydra.scala.syntax.OrType
orTypeLhs :: Phantoms.TTerm Syntax.OrType -> Phantoms.TTerm Syntax.Type
orTypeLhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.OrType"),
        Core.projectionFieldName = (Core.Name "lhs")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the rhs field of hydra.scala.syntax.OrType
orTypeRhs :: Phantoms.TTerm Syntax.OrType -> Phantoms.TTerm Syntax.Type
orTypeRhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.OrType"),
        Core.projectionFieldName = (Core.Name "rhs")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the lhs field of hydra.scala.syntax.OrType
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
              Core.projectionFieldName = (Core.Name "rhs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the rhs field of hydra.scala.syntax.OrType
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
              Core.projectionFieldName = (Core.Name "lhs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.scala.syntax.ParamData
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
-- | DSL accessor for the decltpe field of hydra.scala.syntax.ParamData
paramDataDecltpe :: Phantoms.TTerm Syntax.ParamData -> Phantoms.TTerm (Maybe Syntax.Type)
paramDataDecltpe x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ParamData"),
        Core.projectionFieldName = (Core.Name "decltpe")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the default field of hydra.scala.syntax.ParamData
paramDataDefault :: Phantoms.TTerm Syntax.ParamData -> Phantoms.TTerm (Maybe Syntax.Data)
paramDataDefault x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ParamData"),
        Core.projectionFieldName = (Core.Name "default")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the mods field of hydra.scala.syntax.ParamData
paramDataMods :: Phantoms.TTerm Syntax.ParamData -> Phantoms.TTerm [Syntax.Mod]
paramDataMods x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ParamData"),
        Core.projectionFieldName = (Core.Name "mods")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the name field of hydra.scala.syntax.ParamData
paramDataName :: Phantoms.TTerm Syntax.ParamData -> Phantoms.TTerm Syntax.Name
paramDataName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ParamData"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the decltpe field of hydra.scala.syntax.ParamData
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
              Core.projectionFieldName = (Core.Name "mods")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ParamData"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "default"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ParamData"),
              Core.projectionFieldName = (Core.Name "default")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the default field of hydra.scala.syntax.ParamData
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
              Core.projectionFieldName = (Core.Name "mods")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ParamData"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ParamData"),
              Core.projectionFieldName = (Core.Name "decltpe")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "default"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the mods field of hydra.scala.syntax.ParamData
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
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ParamData"),
              Core.projectionFieldName = (Core.Name "decltpe")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "default"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ParamData"),
              Core.projectionFieldName = (Core.Name "default")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the name field of hydra.scala.syntax.ParamData
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
              Core.projectionFieldName = (Core.Name "mods")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ParamData"),
              Core.projectionFieldName = (Core.Name "decltpe")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "default"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ParamData"),
              Core.projectionFieldName = (Core.Name "default")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for hydra.scala.syntax.ParamType
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
-- | DSL accessor for the cbounds field of hydra.scala.syntax.ParamType
paramTypeCbounds :: Phantoms.TTerm Syntax.ParamType -> Phantoms.TTerm [Syntax.Type]
paramTypeCbounds x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ParamType"),
        Core.projectionFieldName = (Core.Name "cbounds")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the mods field of hydra.scala.syntax.ParamType
paramTypeMods :: Phantoms.TTerm Syntax.ParamType -> Phantoms.TTerm [Syntax.Mod]
paramTypeMods x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ParamType"),
        Core.projectionFieldName = (Core.Name "mods")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the name field of hydra.scala.syntax.ParamType
paramTypeName :: Phantoms.TTerm Syntax.ParamType -> Phantoms.TTerm Syntax.Name
paramTypeName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ParamType"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the tbounds field of hydra.scala.syntax.ParamType
paramTypeTbounds :: Phantoms.TTerm Syntax.ParamType -> Phantoms.TTerm [Syntax.TypeBounds]
paramTypeTbounds x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ParamType"),
        Core.projectionFieldName = (Core.Name "tbounds")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the tparams field of hydra.scala.syntax.ParamType
paramTypeTparams :: Phantoms.TTerm Syntax.ParamType -> Phantoms.TTerm [Syntax.ParamType]
paramTypeTparams x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ParamType"),
        Core.projectionFieldName = (Core.Name "tparams")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the vbounds field of hydra.scala.syntax.ParamType
paramTypeVbounds :: Phantoms.TTerm Syntax.ParamType -> Phantoms.TTerm [Syntax.Type]
paramTypeVbounds x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ParamType"),
        Core.projectionFieldName = (Core.Name "vbounds")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the cbounds field of hydra.scala.syntax.ParamType
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
              Core.projectionFieldName = (Core.Name "mods")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ParamType"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ParamType"),
              Core.projectionFieldName = (Core.Name "tparams")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tbounds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ParamType"),
              Core.projectionFieldName = (Core.Name "tbounds")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "vbounds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ParamType"),
              Core.projectionFieldName = (Core.Name "vbounds")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "cbounds"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the mods field of hydra.scala.syntax.ParamType
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
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ParamType"),
              Core.projectionFieldName = (Core.Name "tparams")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tbounds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ParamType"),
              Core.projectionFieldName = (Core.Name "tbounds")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "vbounds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ParamType"),
              Core.projectionFieldName = (Core.Name "vbounds")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "cbounds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ParamType"),
              Core.projectionFieldName = (Core.Name "cbounds")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the name field of hydra.scala.syntax.ParamType
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
              Core.projectionFieldName = (Core.Name "mods")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ParamType"),
              Core.projectionFieldName = (Core.Name "tparams")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tbounds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ParamType"),
              Core.projectionFieldName = (Core.Name "tbounds")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "vbounds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ParamType"),
              Core.projectionFieldName = (Core.Name "vbounds")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "cbounds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ParamType"),
              Core.projectionFieldName = (Core.Name "cbounds")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the tbounds field of hydra.scala.syntax.ParamType
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
              Core.projectionFieldName = (Core.Name "mods")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ParamType"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ParamType"),
              Core.projectionFieldName = (Core.Name "tparams")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tbounds"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "vbounds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ParamType"),
              Core.projectionFieldName = (Core.Name "vbounds")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "cbounds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ParamType"),
              Core.projectionFieldName = (Core.Name "cbounds")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the tparams field of hydra.scala.syntax.ParamType
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
              Core.projectionFieldName = (Core.Name "mods")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ParamType"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "tbounds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ParamType"),
              Core.projectionFieldName = (Core.Name "tbounds")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "vbounds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ParamType"),
              Core.projectionFieldName = (Core.Name "vbounds")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "cbounds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ParamType"),
              Core.projectionFieldName = (Core.Name "cbounds")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the vbounds field of hydra.scala.syntax.ParamType
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
              Core.projectionFieldName = (Core.Name "mods")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ParamType"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ParamType"),
              Core.projectionFieldName = (Core.Name "tparams")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tbounds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ParamType"),
              Core.projectionFieldName = (Core.Name "tbounds")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "vbounds"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "cbounds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ParamType"),
              Core.projectionFieldName = (Core.Name "cbounds")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for hydra.scala.syntax.PartialFunctionData
partialFunctionData :: Phantoms.TTerm [Syntax.Case] -> Phantoms.TTerm Syntax.PartialFunctionData
partialFunctionData cases =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.PartialFunctionData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "cases"),
          Core.fieldTerm = (Phantoms.unTTerm cases)}]}))
-- | DSL accessor for the cases field of hydra.scala.syntax.PartialFunctionData
partialFunctionDataCases :: Phantoms.TTerm Syntax.PartialFunctionData -> Phantoms.TTerm [Syntax.Case]
partialFunctionDataCases x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.PartialFunctionData"),
        Core.projectionFieldName = (Core.Name "cases")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the cases field of hydra.scala.syntax.PartialFunctionData
partialFunctionDataWithCases :: Phantoms.TTerm Syntax.PartialFunctionData -> Phantoms.TTerm [Syntax.Case] -> Phantoms.TTerm Syntax.PartialFunctionData
partialFunctionDataWithCases original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.PartialFunctionData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "cases"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL injection for the alternative variant of hydra.scala.syntax.Pat
patAlternative :: Phantoms.TTerm Syntax.AlternativePat -> Phantoms.TTerm Syntax.Pat
patAlternative x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Pat"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "alternative"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the bind variant of hydra.scala.syntax.Pat
patBind :: Phantoms.TTerm Syntax.BindPat -> Phantoms.TTerm Syntax.Pat
patBind x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Pat"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "bind"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the extract variant of hydra.scala.syntax.Pat
patExtract :: Phantoms.TTerm Syntax.ExtractPat -> Phantoms.TTerm Syntax.Pat
patExtract x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Pat"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "extract"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the extractInfix variant of hydra.scala.syntax.Pat
patExtractInfix :: Phantoms.TTerm Syntax.ExtractInfixPat -> Phantoms.TTerm Syntax.Pat
patExtractInfix x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Pat"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "extractInfix"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the given variant of hydra.scala.syntax.Pat
patGiven :: Phantoms.TTerm Syntax.GivenPat -> Phantoms.TTerm Syntax.Pat
patGiven x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Pat"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "given"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the interpolate variant of hydra.scala.syntax.Pat
patInterpolate :: Phantoms.TTerm Syntax.InterpolatePat -> Phantoms.TTerm Syntax.Pat
patInterpolate x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Pat"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "interpolate"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the repeated variant of hydra.scala.syntax.Pat
patRepeated :: Phantoms.TTerm Syntax.RepeatedPat -> Phantoms.TTerm Syntax.Pat
patRepeated x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Pat"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "repeated"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the seqWildcard variant of hydra.scala.syntax.Pat
patSeqWildcard :: Phantoms.TTerm Syntax.Pat
patSeqWildcard =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Pat"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "seqWildcard"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the tuple variant of hydra.scala.syntax.Pat
patTuple :: Phantoms.TTerm Syntax.TuplePat -> Phantoms.TTerm Syntax.Pat
patTuple x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Pat"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "tuple"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the typed variant of hydra.scala.syntax.Pat
patTyped :: Phantoms.TTerm Syntax.TypedPat -> Phantoms.TTerm Syntax.Pat
patTyped x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Pat"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "typed"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the var variant of hydra.scala.syntax.Pat
patVar :: Phantoms.TTerm Syntax.VarPat -> Phantoms.TTerm Syntax.Pat
patVar x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Pat"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "var"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the wildcard variant of hydra.scala.syntax.Pat
patWildcard :: Phantoms.TTerm Syntax.Pat
patWildcard =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Pat"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "wildcard"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL constructor for hydra.scala.syntax.Pkg
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
-- | DSL accessor for the name field of hydra.scala.syntax.Pkg
pkgName :: Phantoms.TTerm Syntax.Pkg -> Phantoms.TTerm Syntax.NameData
pkgName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.Pkg"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the ref field of hydra.scala.syntax.Pkg
pkgRef :: Phantoms.TTerm Syntax.Pkg -> Phantoms.TTerm Syntax.RefData
pkgRef x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.Pkg"),
        Core.projectionFieldName = (Core.Name "ref")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the stats field of hydra.scala.syntax.Pkg
pkgStats :: Phantoms.TTerm Syntax.Pkg -> Phantoms.TTerm [Syntax.Stat]
pkgStats x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.Pkg"),
        Core.projectionFieldName = (Core.Name "stats")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the name field of hydra.scala.syntax.Pkg
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
              Core.projectionFieldName = (Core.Name "ref")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "stats"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.Pkg"),
              Core.projectionFieldName = (Core.Name "stats")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the ref field of hydra.scala.syntax.Pkg
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
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ref"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "stats"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.Pkg"),
              Core.projectionFieldName = (Core.Name "stats")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the stats field of hydra.scala.syntax.Pkg
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
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ref"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.Pkg"),
              Core.projectionFieldName = (Core.Name "ref")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "stats"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.scala.syntax.PlaceholderType
placeholderType :: Phantoms.TTerm Syntax.TypeBounds -> Phantoms.TTerm Syntax.PlaceholderType
placeholderType bounds =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.PlaceholderType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "bounds"),
          Core.fieldTerm = (Phantoms.unTTerm bounds)}]}))
-- | DSL accessor for the bounds field of hydra.scala.syntax.PlaceholderType
placeholderTypeBounds :: Phantoms.TTerm Syntax.PlaceholderType -> Phantoms.TTerm Syntax.TypeBounds
placeholderTypeBounds x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.PlaceholderType"),
        Core.projectionFieldName = (Core.Name "bounds")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the bounds field of hydra.scala.syntax.PlaceholderType
placeholderTypeWithBounds :: Phantoms.TTerm Syntax.PlaceholderType -> Phantoms.TTerm Syntax.TypeBounds -> Phantoms.TTerm Syntax.PlaceholderType
placeholderTypeWithBounds original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.PlaceholderType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "bounds"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.scala.syntax.PolyFunctionData
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
-- | DSL accessor for the body field of hydra.scala.syntax.PolyFunctionData
polyFunctionDataBody :: Phantoms.TTerm Syntax.PolyFunctionData -> Phantoms.TTerm Syntax.Data
polyFunctionDataBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.PolyFunctionData"),
        Core.projectionFieldName = (Core.Name "body")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the tparams field of hydra.scala.syntax.PolyFunctionData
polyFunctionDataTparams :: Phantoms.TTerm Syntax.PolyFunctionData -> Phantoms.TTerm [Syntax.ParamType]
polyFunctionDataTparams x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.PolyFunctionData"),
        Core.projectionFieldName = (Core.Name "tparams")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the body field of hydra.scala.syntax.PolyFunctionData
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
              Core.projectionFieldName = (Core.Name "tparams")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the tparams field of hydra.scala.syntax.PolyFunctionData
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
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for hydra.scala.syntax.PolyFunctionType
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
-- | DSL accessor for the tparams field of hydra.scala.syntax.PolyFunctionType
polyFunctionTypeTparams :: Phantoms.TTerm Syntax.PolyFunctionType -> Phantoms.TTerm [Syntax.ParamType]
polyFunctionTypeTparams x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.PolyFunctionType"),
        Core.projectionFieldName = (Core.Name "tparams")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the tpe field of hydra.scala.syntax.PolyFunctionType
polyFunctionTypeTpe :: Phantoms.TTerm Syntax.PolyFunctionType -> Phantoms.TTerm Syntax.Type
polyFunctionTypeTpe x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.PolyFunctionType"),
        Core.projectionFieldName = (Core.Name "tpe")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the tparams field of hydra.scala.syntax.PolyFunctionType
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
              Core.projectionFieldName = (Core.Name "tpe")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the tpe field of hydra.scala.syntax.PolyFunctionType
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
              Core.projectionFieldName = (Core.Name "tparams")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tpe"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for the hydra.scala.syntax.PredefString wrapper
predefString :: Phantoms.TTerm String -> Phantoms.TTerm Syntax.PredefString
predefString x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.scala.syntax.PredefString"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL constructor for hydra.scala.syntax.PrimaryCtor
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
-- | DSL accessor for the mods field of hydra.scala.syntax.PrimaryCtor
primaryCtorMods :: Phantoms.TTerm Syntax.PrimaryCtor -> Phantoms.TTerm [Syntax.Mod]
primaryCtorMods x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.PrimaryCtor"),
        Core.projectionFieldName = (Core.Name "mods")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the name field of hydra.scala.syntax.PrimaryCtor
primaryCtorName :: Phantoms.TTerm Syntax.PrimaryCtor -> Phantoms.TTerm Syntax.Name
primaryCtorName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.PrimaryCtor"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the paramss field of hydra.scala.syntax.PrimaryCtor
primaryCtorParamss :: Phantoms.TTerm Syntax.PrimaryCtor -> Phantoms.TTerm [[Syntax.ParamData]]
primaryCtorParamss x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.PrimaryCtor"),
        Core.projectionFieldName = (Core.Name "paramss")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the mods field of hydra.scala.syntax.PrimaryCtor
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
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "paramss"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.PrimaryCtor"),
              Core.projectionFieldName = (Core.Name "paramss")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the name field of hydra.scala.syntax.PrimaryCtor
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
              Core.projectionFieldName = (Core.Name "mods")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "paramss"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.PrimaryCtor"),
              Core.projectionFieldName = (Core.Name "paramss")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the paramss field of hydra.scala.syntax.PrimaryCtor
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
              Core.projectionFieldName = (Core.Name "mods")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.PrimaryCtor"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "paramss"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.scala.syntax.PrivateMod
privateMod :: Phantoms.TTerm Syntax.Ref -> Phantoms.TTerm Syntax.PrivateMod
privateMod within =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.PrivateMod"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "within"),
          Core.fieldTerm = (Phantoms.unTTerm within)}]}))
-- | DSL updater for the within field of hydra.scala.syntax.PrivateMod
privateModWithWithin :: Phantoms.TTerm Syntax.PrivateMod -> Phantoms.TTerm Syntax.Ref -> Phantoms.TTerm Syntax.PrivateMod
privateModWithWithin original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.PrivateMod"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "within"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL accessor for the within field of hydra.scala.syntax.PrivateMod
privateModWithin :: Phantoms.TTerm Syntax.PrivateMod -> Phantoms.TTerm Syntax.Ref
privateModWithin x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.PrivateMod"),
        Core.projectionFieldName = (Core.Name "within")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL constructor for hydra.scala.syntax.ProjectType
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
-- | DSL accessor for the name field of hydra.scala.syntax.ProjectType
projectTypeName :: Phantoms.TTerm Syntax.ProjectType -> Phantoms.TTerm Syntax.NameType
projectTypeName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ProjectType"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the qual field of hydra.scala.syntax.ProjectType
projectTypeQual :: Phantoms.TTerm Syntax.ProjectType -> Phantoms.TTerm Syntax.Type
projectTypeQual x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ProjectType"),
        Core.projectionFieldName = (Core.Name "qual")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the name field of hydra.scala.syntax.ProjectType
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
              Core.projectionFieldName = (Core.Name "qual")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the qual field of hydra.scala.syntax.ProjectType
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
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for hydra.scala.syntax.ProtectedMod
protectedMod :: Phantoms.TTerm Syntax.Ref -> Phantoms.TTerm Syntax.ProtectedMod
protectedMod within =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ProtectedMod"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "within"),
          Core.fieldTerm = (Phantoms.unTTerm within)}]}))
-- | DSL updater for the within field of hydra.scala.syntax.ProtectedMod
protectedModWithWithin :: Phantoms.TTerm Syntax.ProtectedMod -> Phantoms.TTerm Syntax.Ref -> Phantoms.TTerm Syntax.ProtectedMod
protectedModWithWithin original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ProtectedMod"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "within"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL accessor for the within field of hydra.scala.syntax.ProtectedMod
protectedModWithin :: Phantoms.TTerm Syntax.ProtectedMod -> Phantoms.TTerm Syntax.Ref
protectedModWithin x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ProtectedMod"),
        Core.projectionFieldName = (Core.Name "within")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL injection for the anonymous variant of hydra.scala.syntax.RefData
refDataAnonymous :: Phantoms.TTerm Syntax.AnonymousData -> Phantoms.TTerm Syntax.RefData
refDataAnonymous x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.RefData"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "anonymous"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the applyUnary variant of hydra.scala.syntax.RefData
refDataApplyUnary :: Phantoms.TTerm Syntax.ApplyUnaryData -> Phantoms.TTerm Syntax.RefData
refDataApplyUnary x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.RefData"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "applyUnary"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the name variant of hydra.scala.syntax.RefData
refDataName :: Phantoms.TTerm Syntax.NameData -> Phantoms.TTerm Syntax.RefData
refDataName x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.RefData"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "name"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the select variant of hydra.scala.syntax.RefData
refDataSelect :: Phantoms.TTerm Syntax.SelectData -> Phantoms.TTerm Syntax.RefData
refDataSelect x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.RefData"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "select"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the super variant of hydra.scala.syntax.RefData
refDataSuper :: Phantoms.TTerm Syntax.SuperData -> Phantoms.TTerm Syntax.RefData
refDataSuper x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.RefData"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "super"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the this variant of hydra.scala.syntax.RefData
refDataThis :: Phantoms.TTerm Syntax.ThisData -> Phantoms.TTerm Syntax.RefData
refDataThis x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.RefData"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "this"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the init variant of hydra.scala.syntax.Ref
refInit :: Phantoms.TTerm Syntax.Init -> Phantoms.TTerm Syntax.Ref
refInit x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Ref"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "init"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the name variant of hydra.scala.syntax.Ref
refName :: Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.Ref
refName x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Ref"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "name"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the name variant of hydra.scala.syntax.RefType
refTypeName :: Phantoms.TTerm Syntax.NameType -> Phantoms.TTerm Syntax.RefType
refTypeName x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.RefType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "name"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the project variant of hydra.scala.syntax.RefType
refTypeProject :: Phantoms.TTerm Syntax.ProjectType -> Phantoms.TTerm Syntax.RefType
refTypeProject x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.RefType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "project"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the select variant of hydra.scala.syntax.RefType
refTypeSelect :: Phantoms.TTerm Syntax.SelectType -> Phantoms.TTerm Syntax.RefType
refTypeSelect x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.RefType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "select"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the singleton variant of hydra.scala.syntax.RefType
refTypeSingleton :: Phantoms.TTerm Syntax.SingletonType -> Phantoms.TTerm Syntax.RefType
refTypeSingleton x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.RefType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "singleton"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL constructor for hydra.scala.syntax.RefineType
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
-- | DSL accessor for the stats field of hydra.scala.syntax.RefineType
refineTypeStats :: Phantoms.TTerm Syntax.RefineType -> Phantoms.TTerm [Syntax.Stat]
refineTypeStats x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.RefineType"),
        Core.projectionFieldName = (Core.Name "stats")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the tpe field of hydra.scala.syntax.RefineType
refineTypeTpe :: Phantoms.TTerm Syntax.RefineType -> Phantoms.TTerm (Maybe Syntax.Type)
refineTypeTpe x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.RefineType"),
        Core.projectionFieldName = (Core.Name "tpe")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the stats field of hydra.scala.syntax.RefineType
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
              Core.projectionFieldName = (Core.Name "tpe")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "stats"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the tpe field of hydra.scala.syntax.RefineType
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
              Core.projectionFieldName = (Core.Name "stats")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for hydra.scala.syntax.RenameImportee
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
-- | DSL accessor for the name field of hydra.scala.syntax.RenameImportee
renameImporteeName :: Phantoms.TTerm Syntax.RenameImportee -> Phantoms.TTerm Syntax.Name
renameImporteeName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.RenameImportee"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the rename field of hydra.scala.syntax.RenameImportee
renameImporteeRename :: Phantoms.TTerm Syntax.RenameImportee -> Phantoms.TTerm Syntax.Name
renameImporteeRename x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.RenameImportee"),
        Core.projectionFieldName = (Core.Name "rename")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the name field of hydra.scala.syntax.RenameImportee
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
              Core.projectionFieldName = (Core.Name "rename")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the rename field of hydra.scala.syntax.RenameImportee
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
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rename"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.scala.syntax.RepeatedData
repeatedData :: Phantoms.TTerm Syntax.Data -> Phantoms.TTerm Syntax.RepeatedData
repeatedData expr =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.RepeatedData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expr"),
          Core.fieldTerm = (Phantoms.unTTerm expr)}]}))
-- | DSL accessor for the expr field of hydra.scala.syntax.RepeatedData
repeatedDataExpr :: Phantoms.TTerm Syntax.RepeatedData -> Phantoms.TTerm Syntax.Data
repeatedDataExpr x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.RepeatedData"),
        Core.projectionFieldName = (Core.Name "expr")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the expr field of hydra.scala.syntax.RepeatedData
repeatedDataWithExpr :: Phantoms.TTerm Syntax.RepeatedData -> Phantoms.TTerm Syntax.Data -> Phantoms.TTerm Syntax.RepeatedData
repeatedDataWithExpr original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.RepeatedData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expr"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.scala.syntax.RepeatedEnumCaseDefn
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
-- | DSL accessor for the cases field of hydra.scala.syntax.RepeatedEnumCaseDefn
repeatedEnumCaseDefnCases :: Phantoms.TTerm Syntax.RepeatedEnumCaseDefn -> Phantoms.TTerm [Syntax.NameData]
repeatedEnumCaseDefnCases x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.RepeatedEnumCaseDefn"),
        Core.projectionFieldName = (Core.Name "cases")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the mods field of hydra.scala.syntax.RepeatedEnumCaseDefn
repeatedEnumCaseDefnMods :: Phantoms.TTerm Syntax.RepeatedEnumCaseDefn -> Phantoms.TTerm [Syntax.Mod]
repeatedEnumCaseDefnMods x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.RepeatedEnumCaseDefn"),
        Core.projectionFieldName = (Core.Name "mods")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the cases field of hydra.scala.syntax.RepeatedEnumCaseDefn
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
              Core.projectionFieldName = (Core.Name "mods")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "cases"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the mods field of hydra.scala.syntax.RepeatedEnumCaseDefn
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
              Core.projectionFieldName = (Core.Name "cases")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for hydra.scala.syntax.RepeatedPat
repeatedPat :: Phantoms.TTerm Syntax.NameData -> Phantoms.TTerm Syntax.RepeatedPat
repeatedPat name =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.RepeatedPat"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)}]}))
-- | DSL accessor for the name field of hydra.scala.syntax.RepeatedPat
repeatedPatName :: Phantoms.TTerm Syntax.RepeatedPat -> Phantoms.TTerm Syntax.NameData
repeatedPatName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.RepeatedPat"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the name field of hydra.scala.syntax.RepeatedPat
repeatedPatWithName :: Phantoms.TTerm Syntax.RepeatedPat -> Phantoms.TTerm Syntax.NameData -> Phantoms.TTerm Syntax.RepeatedPat
repeatedPatWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.RepeatedPat"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.scala.syntax.RepeatedType
repeatedType :: Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.RepeatedType
repeatedType tpe =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.RepeatedType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "tpe"),
          Core.fieldTerm = (Phantoms.unTTerm tpe)}]}))
-- | DSL accessor for the tpe field of hydra.scala.syntax.RepeatedType
repeatedTypeTpe :: Phantoms.TTerm Syntax.RepeatedType -> Phantoms.TTerm Syntax.Type
repeatedTypeTpe x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.RepeatedType"),
        Core.projectionFieldName = (Core.Name "tpe")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the tpe field of hydra.scala.syntax.RepeatedType
repeatedTypeWithTpe :: Phantoms.TTerm Syntax.RepeatedType -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.RepeatedType
repeatedTypeWithTpe original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.RepeatedType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "tpe"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.scala.syntax.ReturnData
returnData :: Phantoms.TTerm Syntax.Data -> Phantoms.TTerm Syntax.ReturnData
returnData expr =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ReturnData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expr"),
          Core.fieldTerm = (Phantoms.unTTerm expr)}]}))
-- | DSL accessor for the expr field of hydra.scala.syntax.ReturnData
returnDataExpr :: Phantoms.TTerm Syntax.ReturnData -> Phantoms.TTerm Syntax.Data
returnDataExpr x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ReturnData"),
        Core.projectionFieldName = (Core.Name "expr")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the expr field of hydra.scala.syntax.ReturnData
returnDataWithExpr :: Phantoms.TTerm Syntax.ReturnData -> Phantoms.TTerm Syntax.Data -> Phantoms.TTerm Syntax.ReturnData
returnDataWithExpr original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ReturnData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expr"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.scala.syntax.ScalaSymbol
scalaSymbol :: Phantoms.TTerm String -> Phantoms.TTerm Syntax.ScalaSymbol
scalaSymbol name =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ScalaSymbol"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)}]}))
-- | DSL accessor for the name field of hydra.scala.syntax.ScalaSymbol
scalaSymbolName :: Phantoms.TTerm Syntax.ScalaSymbol -> Phantoms.TTerm String
scalaSymbolName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ScalaSymbol"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the name field of hydra.scala.syntax.ScalaSymbol
scalaSymbolWithName :: Phantoms.TTerm Syntax.ScalaSymbol -> Phantoms.TTerm String -> Phantoms.TTerm Syntax.ScalaSymbol
scalaSymbolWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ScalaSymbol"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.scala.syntax.SecondaryCtor
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
-- | DSL accessor for the init field of hydra.scala.syntax.SecondaryCtor
secondaryCtorInit :: Phantoms.TTerm Syntax.SecondaryCtor -> Phantoms.TTerm Syntax.Init
secondaryCtorInit x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.SecondaryCtor"),
        Core.projectionFieldName = (Core.Name "init")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the mods field of hydra.scala.syntax.SecondaryCtor
secondaryCtorMods :: Phantoms.TTerm Syntax.SecondaryCtor -> Phantoms.TTerm [Syntax.Mod]
secondaryCtorMods x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.SecondaryCtor"),
        Core.projectionFieldName = (Core.Name "mods")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the name field of hydra.scala.syntax.SecondaryCtor
secondaryCtorName :: Phantoms.TTerm Syntax.SecondaryCtor -> Phantoms.TTerm Syntax.Name
secondaryCtorName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.SecondaryCtor"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the paramss field of hydra.scala.syntax.SecondaryCtor
secondaryCtorParamss :: Phantoms.TTerm Syntax.SecondaryCtor -> Phantoms.TTerm [[Syntax.ParamData]]
secondaryCtorParamss x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.SecondaryCtor"),
        Core.projectionFieldName = (Core.Name "paramss")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the stats field of hydra.scala.syntax.SecondaryCtor
secondaryCtorStats :: Phantoms.TTerm Syntax.SecondaryCtor -> Phantoms.TTerm [Syntax.Stat]
secondaryCtorStats x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.SecondaryCtor"),
        Core.projectionFieldName = (Core.Name "stats")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the init field of hydra.scala.syntax.SecondaryCtor
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
              Core.projectionFieldName = (Core.Name "mods")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.SecondaryCtor"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "paramss"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.SecondaryCtor"),
              Core.projectionFieldName = (Core.Name "paramss")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "init"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "stats"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.SecondaryCtor"),
              Core.projectionFieldName = (Core.Name "stats")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the mods field of hydra.scala.syntax.SecondaryCtor
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
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "paramss"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.SecondaryCtor"),
              Core.projectionFieldName = (Core.Name "paramss")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "init"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.SecondaryCtor"),
              Core.projectionFieldName = (Core.Name "init")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "stats"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.SecondaryCtor"),
              Core.projectionFieldName = (Core.Name "stats")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the name field of hydra.scala.syntax.SecondaryCtor
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
              Core.projectionFieldName = (Core.Name "mods")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "paramss"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.SecondaryCtor"),
              Core.projectionFieldName = (Core.Name "paramss")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "init"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.SecondaryCtor"),
              Core.projectionFieldName = (Core.Name "init")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "stats"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.SecondaryCtor"),
              Core.projectionFieldName = (Core.Name "stats")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the paramss field of hydra.scala.syntax.SecondaryCtor
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
              Core.projectionFieldName = (Core.Name "mods")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.SecondaryCtor"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "paramss"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "init"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.SecondaryCtor"),
              Core.projectionFieldName = (Core.Name "init")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "stats"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.SecondaryCtor"),
              Core.projectionFieldName = (Core.Name "stats")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the stats field of hydra.scala.syntax.SecondaryCtor
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
              Core.projectionFieldName = (Core.Name "mods")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.SecondaryCtor"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "paramss"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.SecondaryCtor"),
              Core.projectionFieldName = (Core.Name "paramss")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "init"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.SecondaryCtor"),
              Core.projectionFieldName = (Core.Name "init")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "stats"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.scala.syntax.SelectData
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
-- | DSL accessor for the name field of hydra.scala.syntax.SelectData
selectDataName :: Phantoms.TTerm Syntax.SelectData -> Phantoms.TTerm Syntax.NameData
selectDataName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.SelectData"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the qual field of hydra.scala.syntax.SelectData
selectDataQual :: Phantoms.TTerm Syntax.SelectData -> Phantoms.TTerm Syntax.Data
selectDataQual x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.SelectData"),
        Core.projectionFieldName = (Core.Name "qual")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the name field of hydra.scala.syntax.SelectData
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
              Core.projectionFieldName = (Core.Name "qual")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the qual field of hydra.scala.syntax.SelectData
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
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for hydra.scala.syntax.SelectType
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
-- | DSL accessor for the name field of hydra.scala.syntax.SelectType
selectTypeName :: Phantoms.TTerm Syntax.SelectType -> Phantoms.TTerm Syntax.NameType
selectTypeName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.SelectType"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the qual field of hydra.scala.syntax.SelectType
selectTypeQual :: Phantoms.TTerm Syntax.SelectType -> Phantoms.TTerm Syntax.RefData
selectTypeQual x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.SelectType"),
        Core.projectionFieldName = (Core.Name "qual")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the name field of hydra.scala.syntax.SelectType
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
              Core.projectionFieldName = (Core.Name "qual")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the qual field of hydra.scala.syntax.SelectType
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
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for the hydra.scala.syntax.Self wrapper
self :: Phantoms.TTerm () -> Phantoms.TTerm Syntax.Self
self x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.scala.syntax.Self"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL constructor for hydra.scala.syntax.SingletonType
singletonType :: Phantoms.TTerm Syntax.RefData -> Phantoms.TTerm Syntax.SingletonType
singletonType ref =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.SingletonType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "ref"),
          Core.fieldTerm = (Phantoms.unTTerm ref)}]}))
-- | DSL accessor for the ref field of hydra.scala.syntax.SingletonType
singletonTypeRef :: Phantoms.TTerm Syntax.SingletonType -> Phantoms.TTerm Syntax.RefData
singletonTypeRef x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.SingletonType"),
        Core.projectionFieldName = (Core.Name "ref")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the ref field of hydra.scala.syntax.SingletonType
singletonTypeWithRef :: Phantoms.TTerm Syntax.SingletonType -> Phantoms.TTerm Syntax.RefData -> Phantoms.TTerm Syntax.SingletonType
singletonTypeWithRef original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.SingletonType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "ref"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.scala.syntax.Source
source :: Phantoms.TTerm [Syntax.Stat] -> Phantoms.TTerm Syntax.Source
source stats =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.Source"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "stats"),
          Core.fieldTerm = (Phantoms.unTTerm stats)}]}))
-- | DSL accessor for the stats field of hydra.scala.syntax.Source
sourceStats :: Phantoms.TTerm Syntax.Source -> Phantoms.TTerm [Syntax.Stat]
sourceStats x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.Source"),
        Core.projectionFieldName = (Core.Name "stats")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the stats field of hydra.scala.syntax.Source
sourceWithStats :: Phantoms.TTerm Syntax.Source -> Phantoms.TTerm [Syntax.Stat] -> Phantoms.TTerm Syntax.Source
sourceWithStats original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.Source"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "stats"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL injection for the decl variant of hydra.scala.syntax.Stat
statDecl :: Phantoms.TTerm Syntax.Decl -> Phantoms.TTerm Syntax.Stat
statDecl x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Stat"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "decl"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the defn variant of hydra.scala.syntax.Stat
statDefn :: Phantoms.TTerm Syntax.Defn -> Phantoms.TTerm Syntax.Stat
statDefn x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Stat"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "defn"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the importExport variant of hydra.scala.syntax.Stat
statImportExport :: Phantoms.TTerm Syntax.ImportExportStat -> Phantoms.TTerm Syntax.Stat
statImportExport x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Stat"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "importExport"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the term variant of hydra.scala.syntax.Stat
statTerm :: Phantoms.TTerm Syntax.Data -> Phantoms.TTerm Syntax.Stat
statTerm x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Stat"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "term"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL constructor for hydra.scala.syntax.SuperData
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
-- | DSL accessor for the superp field of hydra.scala.syntax.SuperData
superDataSuperp :: Phantoms.TTerm Syntax.SuperData -> Phantoms.TTerm Syntax.Name
superDataSuperp x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.SuperData"),
        Core.projectionFieldName = (Core.Name "superp")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the thisp field of hydra.scala.syntax.SuperData
superDataThisp :: Phantoms.TTerm Syntax.SuperData -> Phantoms.TTerm Syntax.Name
superDataThisp x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.SuperData"),
        Core.projectionFieldName = (Core.Name "thisp")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the superp field of hydra.scala.syntax.SuperData
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
              Core.projectionFieldName = (Core.Name "thisp")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "superp"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the thisp field of hydra.scala.syntax.SuperData
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
              Core.projectionFieldName = (Core.Name "superp")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for hydra.scala.syntax.Template
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
-- | DSL accessor for the early field of hydra.scala.syntax.Template
templateEarly :: Phantoms.TTerm Syntax.Template -> Phantoms.TTerm [Syntax.Stat]
templateEarly x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.Template"),
        Core.projectionFieldName = (Core.Name "early")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the inits field of hydra.scala.syntax.Template
templateInits :: Phantoms.TTerm Syntax.Template -> Phantoms.TTerm [Syntax.Init]
templateInits x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.Template"),
        Core.projectionFieldName = (Core.Name "inits")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the self field of hydra.scala.syntax.Template
templateSelf :: Phantoms.TTerm Syntax.Template -> Phantoms.TTerm Syntax.Self
templateSelf x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.Template"),
        Core.projectionFieldName = (Core.Name "self")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the stats field of hydra.scala.syntax.Template
templateStats :: Phantoms.TTerm Syntax.Template -> Phantoms.TTerm [Syntax.Stat]
templateStats x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.Template"),
        Core.projectionFieldName = (Core.Name "stats")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the early field of hydra.scala.syntax.Template
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
              Core.projectionFieldName = (Core.Name "inits")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "self"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.Template"),
              Core.projectionFieldName = (Core.Name "self")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "stats"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.Template"),
              Core.projectionFieldName = (Core.Name "stats")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the inits field of hydra.scala.syntax.Template
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
              Core.projectionFieldName = (Core.Name "early")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inits"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "self"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.Template"),
              Core.projectionFieldName = (Core.Name "self")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "stats"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.Template"),
              Core.projectionFieldName = (Core.Name "stats")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the self field of hydra.scala.syntax.Template
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
              Core.projectionFieldName = (Core.Name "early")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inits"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.Template"),
              Core.projectionFieldName = (Core.Name "inits")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "self"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "stats"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.Template"),
              Core.projectionFieldName = (Core.Name "stats")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the stats field of hydra.scala.syntax.Template
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
              Core.projectionFieldName = (Core.Name "early")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inits"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.Template"),
              Core.projectionFieldName = (Core.Name "inits")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "self"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.Template"),
              Core.projectionFieldName = (Core.Name "self")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "stats"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for the hydra.scala.syntax.ThisData wrapper
thisData :: Phantoms.TTerm () -> Phantoms.TTerm Syntax.ThisData
thisData x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.scala.syntax.ThisData"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL constructor for hydra.scala.syntax.ThrowData
throwData :: Phantoms.TTerm Syntax.Data -> Phantoms.TTerm Syntax.ThrowData
throwData expr =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ThrowData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expr"),
          Core.fieldTerm = (Phantoms.unTTerm expr)}]}))
-- | DSL accessor for the expr field of hydra.scala.syntax.ThrowData
throwDataExpr :: Phantoms.TTerm Syntax.ThrowData -> Phantoms.TTerm Syntax.Data
throwDataExpr x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ThrowData"),
        Core.projectionFieldName = (Core.Name "expr")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the expr field of hydra.scala.syntax.ThrowData
throwDataWithExpr :: Phantoms.TTerm Syntax.ThrowData -> Phantoms.TTerm Syntax.Data -> Phantoms.TTerm Syntax.ThrowData
throwDataWithExpr original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ThrowData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expr"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.scala.syntax.TraitDefn
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
-- | DSL accessor for the ctor field of hydra.scala.syntax.TraitDefn
traitDefnCtor :: Phantoms.TTerm Syntax.TraitDefn -> Phantoms.TTerm Syntax.PrimaryCtor
traitDefnCtor x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TraitDefn"),
        Core.projectionFieldName = (Core.Name "ctor")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the mods field of hydra.scala.syntax.TraitDefn
traitDefnMods :: Phantoms.TTerm Syntax.TraitDefn -> Phantoms.TTerm [Syntax.Mod]
traitDefnMods x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TraitDefn"),
        Core.projectionFieldName = (Core.Name "mods")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the name field of hydra.scala.syntax.TraitDefn
traitDefnName :: Phantoms.TTerm Syntax.TraitDefn -> Phantoms.TTerm Syntax.NameType
traitDefnName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TraitDefn"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the template field of hydra.scala.syntax.TraitDefn
traitDefnTemplate :: Phantoms.TTerm Syntax.TraitDefn -> Phantoms.TTerm Syntax.Template
traitDefnTemplate x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TraitDefn"),
        Core.projectionFieldName = (Core.Name "template")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the tparams field of hydra.scala.syntax.TraitDefn
traitDefnTparams :: Phantoms.TTerm Syntax.TraitDefn -> Phantoms.TTerm [Syntax.ParamType]
traitDefnTparams x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TraitDefn"),
        Core.projectionFieldName = (Core.Name "tparams")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the ctor field of hydra.scala.syntax.TraitDefn
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
              Core.projectionFieldName = (Core.Name "mods")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TraitDefn"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TraitDefn"),
              Core.projectionFieldName = (Core.Name "tparams")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ctor"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "template"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TraitDefn"),
              Core.projectionFieldName = (Core.Name "template")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the mods field of hydra.scala.syntax.TraitDefn
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
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TraitDefn"),
              Core.projectionFieldName = (Core.Name "tparams")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ctor"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TraitDefn"),
              Core.projectionFieldName = (Core.Name "ctor")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "template"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TraitDefn"),
              Core.projectionFieldName = (Core.Name "template")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the name field of hydra.scala.syntax.TraitDefn
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
              Core.projectionFieldName = (Core.Name "mods")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TraitDefn"),
              Core.projectionFieldName = (Core.Name "tparams")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ctor"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TraitDefn"),
              Core.projectionFieldName = (Core.Name "ctor")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "template"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TraitDefn"),
              Core.projectionFieldName = (Core.Name "template")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the template field of hydra.scala.syntax.TraitDefn
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
              Core.projectionFieldName = (Core.Name "mods")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TraitDefn"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TraitDefn"),
              Core.projectionFieldName = (Core.Name "tparams")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ctor"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TraitDefn"),
              Core.projectionFieldName = (Core.Name "ctor")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "template"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the tparams field of hydra.scala.syntax.TraitDefn
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
              Core.projectionFieldName = (Core.Name "mods")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TraitDefn"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "ctor"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TraitDefn"),
              Core.projectionFieldName = (Core.Name "ctor")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "template"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TraitDefn"),
              Core.projectionFieldName = (Core.Name "template")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL injection for the bounds variant of hydra.scala.syntax.Tree
treeBounds :: Phantoms.TTerm Syntax.TypeBounds -> Phantoms.TTerm Syntax.Tree
treeBounds x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Tree"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "bounds"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the caseTree variant of hydra.scala.syntax.Tree
treeCaseTree :: Phantoms.TTerm Syntax.CaseTree -> Phantoms.TTerm Syntax.Tree
treeCaseTree x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Tree"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "caseTree"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the ctor variant of hydra.scala.syntax.Tree
treeCtor :: Phantoms.TTerm Syntax.Ctor -> Phantoms.TTerm Syntax.Tree
treeCtor x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Tree"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "ctor"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the enumerator variant of hydra.scala.syntax.Tree
treeEnumerator :: Phantoms.TTerm Syntax.Enumerator -> Phantoms.TTerm Syntax.Tree
treeEnumerator x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Tree"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "enumerator"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the importee variant of hydra.scala.syntax.Tree
treeImportee :: Phantoms.TTerm Syntax.Importee -> Phantoms.TTerm Syntax.Tree
treeImportee x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Tree"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "importee"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the importer variant of hydra.scala.syntax.Tree
treeImporter :: Phantoms.TTerm Syntax.Importer -> Phantoms.TTerm Syntax.Tree
treeImporter x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Tree"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "importer"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the member variant of hydra.scala.syntax.Tree
treeMember :: Phantoms.TTerm Syntax.Member -> Phantoms.TTerm Syntax.Tree
treeMember x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Tree"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "member"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the mod variant of hydra.scala.syntax.Tree
treeMod :: Phantoms.TTerm Syntax.Mod -> Phantoms.TTerm Syntax.Tree
treeMod x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Tree"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "mod"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the pat variant of hydra.scala.syntax.Tree
treePat :: Phantoms.TTerm Syntax.Pat -> Phantoms.TTerm Syntax.Tree
treePat x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Tree"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "pat"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the ref variant of hydra.scala.syntax.Tree
treeRef :: Phantoms.TTerm Syntax.Ref -> Phantoms.TTerm Syntax.Tree
treeRef x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Tree"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "ref"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the source variant of hydra.scala.syntax.Tree
treeSource :: Phantoms.TTerm Syntax.Source -> Phantoms.TTerm Syntax.Tree
treeSource x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Tree"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "source"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the stat variant of hydra.scala.syntax.Tree
treeStat :: Phantoms.TTerm Syntax.Stat -> Phantoms.TTerm Syntax.Tree
treeStat x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Tree"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "stat"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the template variant of hydra.scala.syntax.Tree
treeTemplate :: Phantoms.TTerm Syntax.Template -> Phantoms.TTerm Syntax.Tree
treeTemplate x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Tree"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "template"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the type variant of hydra.scala.syntax.Tree
treeType :: Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.Tree
treeType x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Tree"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "type"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL constructor for hydra.scala.syntax.TryData
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
-- | DSL accessor for the catchp field of hydra.scala.syntax.TryData
tryDataCatchp :: Phantoms.TTerm Syntax.TryData -> Phantoms.TTerm [Syntax.Case]
tryDataCatchp x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TryData"),
        Core.projectionFieldName = (Core.Name "catchp")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the expr field of hydra.scala.syntax.TryData
tryDataExpr :: Phantoms.TTerm Syntax.TryData -> Phantoms.TTerm Syntax.Data
tryDataExpr x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TryData"),
        Core.projectionFieldName = (Core.Name "expr")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the finallyp field of hydra.scala.syntax.TryData
tryDataFinallyp :: Phantoms.TTerm Syntax.TryData -> Phantoms.TTerm (Maybe Syntax.Data)
tryDataFinallyp x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TryData"),
        Core.projectionFieldName = (Core.Name "finallyp")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the catchp field of hydra.scala.syntax.TryData
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
              Core.projectionFieldName = (Core.Name "expr")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "catchp"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "finallyp"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TryData"),
              Core.projectionFieldName = (Core.Name "finallyp")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the expr field of hydra.scala.syntax.TryData
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
              Core.projectionFieldName = (Core.Name "catchp")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "finallyp"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TryData"),
              Core.projectionFieldName = (Core.Name "finallyp")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the finallyp field of hydra.scala.syntax.TryData
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
              Core.projectionFieldName = (Core.Name "expr")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "catchp"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TryData"),
              Core.projectionFieldName = (Core.Name "catchp")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "finallyp"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.scala.syntax.TryWithHandlerData
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
-- | DSL accessor for the catchp field of hydra.scala.syntax.TryWithHandlerData
tryWithHandlerDataCatchp :: Phantoms.TTerm Syntax.TryWithHandlerData -> Phantoms.TTerm Syntax.Data
tryWithHandlerDataCatchp x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TryWithHandlerData"),
        Core.projectionFieldName = (Core.Name "catchp")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the expr field of hydra.scala.syntax.TryWithHandlerData
tryWithHandlerDataExpr :: Phantoms.TTerm Syntax.TryWithHandlerData -> Phantoms.TTerm Syntax.Data
tryWithHandlerDataExpr x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TryWithHandlerData"),
        Core.projectionFieldName = (Core.Name "expr")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the finallyp field of hydra.scala.syntax.TryWithHandlerData
tryWithHandlerDataFinallyp :: Phantoms.TTerm Syntax.TryWithHandlerData -> Phantoms.TTerm (Maybe Syntax.Data)
tryWithHandlerDataFinallyp x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TryWithHandlerData"),
        Core.projectionFieldName = (Core.Name "finallyp")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the catchp field of hydra.scala.syntax.TryWithHandlerData
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
              Core.projectionFieldName = (Core.Name "expr")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "catchp"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "finallyp"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TryWithHandlerData"),
              Core.projectionFieldName = (Core.Name "finallyp")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the expr field of hydra.scala.syntax.TryWithHandlerData
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
              Core.projectionFieldName = (Core.Name "catchp")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "finallyp"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TryWithHandlerData"),
              Core.projectionFieldName = (Core.Name "finallyp")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the finallyp field of hydra.scala.syntax.TryWithHandlerData
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
              Core.projectionFieldName = (Core.Name "expr")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "catchp"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TryWithHandlerData"),
              Core.projectionFieldName = (Core.Name "catchp")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "finallyp"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.scala.syntax.TupleData
tupleData :: Phantoms.TTerm [Syntax.Data] -> Phantoms.TTerm Syntax.TupleData
tupleData args =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.TupleData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "args"),
          Core.fieldTerm = (Phantoms.unTTerm args)}]}))
-- | DSL accessor for the args field of hydra.scala.syntax.TupleData
tupleDataArgs :: Phantoms.TTerm Syntax.TupleData -> Phantoms.TTerm [Syntax.Data]
tupleDataArgs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TupleData"),
        Core.projectionFieldName = (Core.Name "args")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the args field of hydra.scala.syntax.TupleData
tupleDataWithArgs :: Phantoms.TTerm Syntax.TupleData -> Phantoms.TTerm [Syntax.Data] -> Phantoms.TTerm Syntax.TupleData
tupleDataWithArgs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.TupleData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "args"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.scala.syntax.TuplePat
tuplePat :: Phantoms.TTerm [Syntax.Pat] -> Phantoms.TTerm Syntax.TuplePat
tuplePat args =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.TuplePat"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "args"),
          Core.fieldTerm = (Phantoms.unTTerm args)}]}))
-- | DSL accessor for the args field of hydra.scala.syntax.TuplePat
tuplePatArgs :: Phantoms.TTerm Syntax.TuplePat -> Phantoms.TTerm [Syntax.Pat]
tuplePatArgs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TuplePat"),
        Core.projectionFieldName = (Core.Name "args")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the args field of hydra.scala.syntax.TuplePat
tuplePatWithArgs :: Phantoms.TTerm Syntax.TuplePat -> Phantoms.TTerm [Syntax.Pat] -> Phantoms.TTerm Syntax.TuplePat
tuplePatWithArgs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.TuplePat"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "args"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.scala.syntax.TupleType
tupleType :: Phantoms.TTerm [Syntax.Type] -> Phantoms.TTerm Syntax.TupleType
tupleType args =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.TupleType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "args"),
          Core.fieldTerm = (Phantoms.unTTerm args)}]}))
-- | DSL accessor for the args field of hydra.scala.syntax.TupleType
tupleTypeArgs :: Phantoms.TTerm Syntax.TupleType -> Phantoms.TTerm [Syntax.Type]
tupleTypeArgs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TupleType"),
        Core.projectionFieldName = (Core.Name "args")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the args field of hydra.scala.syntax.TupleType
tupleTypeWithArgs :: Phantoms.TTerm Syntax.TupleType -> Phantoms.TTerm [Syntax.Type] -> Phantoms.TTerm Syntax.TupleType
tupleTypeWithArgs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.TupleType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "args"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL injection for the and variant of hydra.scala.syntax.Type
typeAnd :: Phantoms.TTerm Syntax.AndType -> Phantoms.TTerm Syntax.Type
typeAnd x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "and"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the annotate variant of hydra.scala.syntax.Type
typeAnnotate :: Phantoms.TTerm Syntax.AnnotateType -> Phantoms.TTerm Syntax.Type
typeAnnotate x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "annotate"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the anonymousName variant of hydra.scala.syntax.Type
typeAnonymousName :: Phantoms.TTerm Syntax.AnonymousNameType -> Phantoms.TTerm Syntax.Type
typeAnonymousName x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "anonymousName"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the apply variant of hydra.scala.syntax.Type
typeApply :: Phantoms.TTerm Syntax.ApplyType -> Phantoms.TTerm Syntax.Type
typeApply x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "apply"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the applyInfix variant of hydra.scala.syntax.Type
typeApplyInfix :: Phantoms.TTerm Syntax.ApplyInfixType -> Phantoms.TTerm Syntax.Type
typeApplyInfix x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "applyInfix"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL constructor for hydra.scala.syntax.TypeBounds
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
-- | DSL accessor for the hi field of hydra.scala.syntax.TypeBounds
typeBoundsHi :: Phantoms.TTerm Syntax.TypeBounds -> Phantoms.TTerm (Maybe Syntax.Type)
typeBoundsHi x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TypeBounds"),
        Core.projectionFieldName = (Core.Name "hi")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the lo field of hydra.scala.syntax.TypeBounds
typeBoundsLo :: Phantoms.TTerm Syntax.TypeBounds -> Phantoms.TTerm (Maybe Syntax.Type)
typeBoundsLo x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TypeBounds"),
        Core.projectionFieldName = (Core.Name "lo")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the hi field of hydra.scala.syntax.TypeBounds
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
              Core.projectionFieldName = (Core.Name "lo")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "hi"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the lo field of hydra.scala.syntax.TypeBounds
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
              Core.projectionFieldName = (Core.Name "hi")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL injection for the byName variant of hydra.scala.syntax.Type
typeByName :: Phantoms.TTerm Syntax.ByNameType -> Phantoms.TTerm Syntax.Type
typeByName x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "byName"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL constructor for hydra.scala.syntax.TypeCase
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
-- | DSL accessor for the body field of hydra.scala.syntax.TypeCase
typeCaseBody :: Phantoms.TTerm Syntax.TypeCase -> Phantoms.TTerm Syntax.Type
typeCaseBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TypeCase"),
        Core.projectionFieldName = (Core.Name "body")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the pat field of hydra.scala.syntax.TypeCase
typeCasePat :: Phantoms.TTerm Syntax.TypeCase -> Phantoms.TTerm Syntax.Type
typeCasePat x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TypeCase"),
        Core.projectionFieldName = (Core.Name "pat")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the body field of hydra.scala.syntax.TypeCase
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
              Core.projectionFieldName = (Core.Name "pat")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the pat field of hydra.scala.syntax.TypeCase
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
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL injection for the contextFunction variant of hydra.scala.syntax.Type
typeContextFunction :: Phantoms.TTerm Syntax.ContextFunctionType -> Phantoms.TTerm Syntax.Type
typeContextFunction x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "contextFunction"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL constructor for hydra.scala.syntax.TypeDecl
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
-- | DSL accessor for the bounds field of hydra.scala.syntax.TypeDecl
typeDeclBounds :: Phantoms.TTerm Syntax.TypeDecl -> Phantoms.TTerm Syntax.TypeBounds
typeDeclBounds x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TypeDecl"),
        Core.projectionFieldName = (Core.Name "bounds")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the mods field of hydra.scala.syntax.TypeDecl
typeDeclMods :: Phantoms.TTerm Syntax.TypeDecl -> Phantoms.TTerm [Syntax.Mod]
typeDeclMods x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TypeDecl"),
        Core.projectionFieldName = (Core.Name "mods")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the name field of hydra.scala.syntax.TypeDecl
typeDeclName :: Phantoms.TTerm Syntax.TypeDecl -> Phantoms.TTerm Syntax.NameType
typeDeclName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TypeDecl"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the tparams field of hydra.scala.syntax.TypeDecl
typeDeclTparams :: Phantoms.TTerm Syntax.TypeDecl -> Phantoms.TTerm [Syntax.ParamType]
typeDeclTparams x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TypeDecl"),
        Core.projectionFieldName = (Core.Name "tparams")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the bounds field of hydra.scala.syntax.TypeDecl
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
              Core.projectionFieldName = (Core.Name "mods")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TypeDecl"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TypeDecl"),
              Core.projectionFieldName = (Core.Name "tparams")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "bounds"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the mods field of hydra.scala.syntax.TypeDecl
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
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TypeDecl"),
              Core.projectionFieldName = (Core.Name "tparams")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "bounds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TypeDecl"),
              Core.projectionFieldName = (Core.Name "bounds")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the name field of hydra.scala.syntax.TypeDecl
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
              Core.projectionFieldName = (Core.Name "mods")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TypeDecl"),
              Core.projectionFieldName = (Core.Name "tparams")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "bounds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TypeDecl"),
              Core.projectionFieldName = (Core.Name "bounds")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the tparams field of hydra.scala.syntax.TypeDecl
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
              Core.projectionFieldName = (Core.Name "mods")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TypeDecl"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "bounds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TypeDecl"),
              Core.projectionFieldName = (Core.Name "bounds")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for hydra.scala.syntax.TypeDefn
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
-- | DSL accessor for the body field of hydra.scala.syntax.TypeDefn
typeDefnBody :: Phantoms.TTerm Syntax.TypeDefn -> Phantoms.TTerm Syntax.Type
typeDefnBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TypeDefn"),
        Core.projectionFieldName = (Core.Name "body")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the mods field of hydra.scala.syntax.TypeDefn
typeDefnMods :: Phantoms.TTerm Syntax.TypeDefn -> Phantoms.TTerm [Syntax.Mod]
typeDefnMods x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TypeDefn"),
        Core.projectionFieldName = (Core.Name "mods")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the name field of hydra.scala.syntax.TypeDefn
typeDefnName :: Phantoms.TTerm Syntax.TypeDefn -> Phantoms.TTerm Syntax.NameType
typeDefnName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TypeDefn"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the tparams field of hydra.scala.syntax.TypeDefn
typeDefnTparams :: Phantoms.TTerm Syntax.TypeDefn -> Phantoms.TTerm [Syntax.ParamType]
typeDefnTparams x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TypeDefn"),
        Core.projectionFieldName = (Core.Name "tparams")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the body field of hydra.scala.syntax.TypeDefn
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
              Core.projectionFieldName = (Core.Name "mods")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TypeDefn"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TypeDefn"),
              Core.projectionFieldName = (Core.Name "tparams")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the mods field of hydra.scala.syntax.TypeDefn
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
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TypeDefn"),
              Core.projectionFieldName = (Core.Name "tparams")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TypeDefn"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the name field of hydra.scala.syntax.TypeDefn
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
              Core.projectionFieldName = (Core.Name "mods")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TypeDefn"),
              Core.projectionFieldName = (Core.Name "tparams")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TypeDefn"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the tparams field of hydra.scala.syntax.TypeDefn
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
              Core.projectionFieldName = (Core.Name "mods")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TypeDefn"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TypeDefn"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL injection for the existential variant of hydra.scala.syntax.Type
typeExistential :: Phantoms.TTerm Syntax.ExistentialType -> Phantoms.TTerm Syntax.Type
typeExistential x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "existential"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the function variant of hydra.scala.syntax.Type
typeFunction :: Phantoms.TTerm Syntax.FunctionType -> Phantoms.TTerm Syntax.Type
typeFunction x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "function"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the implicitFunction variant of hydra.scala.syntax.Type
typeImplicitFunction :: Phantoms.TTerm Syntax.ImplicitFunctionType -> Phantoms.TTerm Syntax.Type
typeImplicitFunction x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "implicitFunction"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the lambda variant of hydra.scala.syntax.Type
typeLambda :: Phantoms.TTerm Syntax.LambdaType -> Phantoms.TTerm Syntax.Type
typeLambda x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "lambda"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the match variant of hydra.scala.syntax.Type
typeMatch :: Phantoms.TTerm Syntax.MatchType -> Phantoms.TTerm Syntax.Type
typeMatch x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "match"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL constructor for hydra.scala.syntax.TypeMember
typeMember :: Phantoms.TTerm Syntax.NameType -> Phantoms.TTerm Syntax.TypeMember
typeMember name =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.TypeMember"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)}]}))
-- | DSL accessor for the name field of hydra.scala.syntax.TypeMember
typeMemberName :: Phantoms.TTerm Syntax.TypeMember -> Phantoms.TTerm Syntax.NameType
typeMemberName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TypeMember"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the name field of hydra.scala.syntax.TypeMember
typeMemberWithName :: Phantoms.TTerm Syntax.TypeMember -> Phantoms.TTerm Syntax.NameType -> Phantoms.TTerm Syntax.TypeMember
typeMemberWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.TypeMember"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL injection for the method variant of hydra.scala.syntax.Type
typeMethod :: Phantoms.TTerm Syntax.MethodType -> Phantoms.TTerm Syntax.Type
typeMethod x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "method"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the or variant of hydra.scala.syntax.Type
typeOr :: Phantoms.TTerm Syntax.OrType -> Phantoms.TTerm Syntax.Type
typeOr x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "or"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the placeholder variant of hydra.scala.syntax.Type
typePlaceholder :: Phantoms.TTerm Syntax.PlaceholderType -> Phantoms.TTerm Syntax.Type
typePlaceholder x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "placeholder"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the polyFunction variant of hydra.scala.syntax.Type
typePolyFunction :: Phantoms.TTerm Syntax.PolyFunctionType -> Phantoms.TTerm Syntax.Type
typePolyFunction x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "polyFunction"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the ref variant of hydra.scala.syntax.Type
typeRef :: Phantoms.TTerm Syntax.RefType -> Phantoms.TTerm Syntax.Type
typeRef x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "ref"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the refine variant of hydra.scala.syntax.Type
typeRefine :: Phantoms.TTerm Syntax.RefineType -> Phantoms.TTerm Syntax.Type
typeRefine x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "refine"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the repeated variant of hydra.scala.syntax.Type
typeRepeated :: Phantoms.TTerm Syntax.RepeatedType -> Phantoms.TTerm Syntax.Type
typeRepeated x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "repeated"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the tuple variant of hydra.scala.syntax.Type
typeTuple :: Phantoms.TTerm Syntax.TupleType -> Phantoms.TTerm Syntax.Type
typeTuple x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "tuple"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the typedParam variant of hydra.scala.syntax.Type
typeTypedParam :: Phantoms.TTerm Syntax.TypedParamType -> Phantoms.TTerm Syntax.Type
typeTypedParam x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "typedParam"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the var variant of hydra.scala.syntax.Type
typeVar :: Phantoms.TTerm Syntax.VarType -> Phantoms.TTerm Syntax.Type
typeVar x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "var"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the with variant of hydra.scala.syntax.Type
typeWith :: Phantoms.TTerm Syntax.WithType -> Phantoms.TTerm Syntax.Type
typeWith x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "with"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL constructor for hydra.scala.syntax.TypedParamType
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
-- | DSL accessor for the name field of hydra.scala.syntax.TypedParamType
typedParamTypeName :: Phantoms.TTerm Syntax.TypedParamType -> Phantoms.TTerm Syntax.Name
typedParamTypeName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TypedParamType"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the typ field of hydra.scala.syntax.TypedParamType
typedParamTypeTyp :: Phantoms.TTerm Syntax.TypedParamType -> Phantoms.TTerm Syntax.Type
typedParamTypeTyp x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TypedParamType"),
        Core.projectionFieldName = (Core.Name "typ")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the name field of hydra.scala.syntax.TypedParamType
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
              Core.projectionFieldName = (Core.Name "typ")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the typ field of hydra.scala.syntax.TypedParamType
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
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typ"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.scala.syntax.TypedPat
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
-- | DSL accessor for the lhs field of hydra.scala.syntax.TypedPat
typedPatLhs :: Phantoms.TTerm Syntax.TypedPat -> Phantoms.TTerm Syntax.Pat
typedPatLhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TypedPat"),
        Core.projectionFieldName = (Core.Name "lhs")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the rhs field of hydra.scala.syntax.TypedPat
typedPatRhs :: Phantoms.TTerm Syntax.TypedPat -> Phantoms.TTerm Syntax.Type
typedPatRhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TypedPat"),
        Core.projectionFieldName = (Core.Name "rhs")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the lhs field of hydra.scala.syntax.TypedPat
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
              Core.projectionFieldName = (Core.Name "rhs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the rhs field of hydra.scala.syntax.TypedPat
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
              Core.projectionFieldName = (Core.Name "lhs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL accessor for the body of hydra.scala.syntax.AnonymousData
unAnonymousData :: Phantoms.TTerm Syntax.AnonymousData -> Phantoms.TTerm ()
unAnonymousData x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.scala.syntax.AnonymousData")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.scala.syntax.AnonymousNameType
unAnonymousNameType :: Phantoms.TTerm Syntax.AnonymousNameType -> Phantoms.TTerm ()
unAnonymousNameType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.scala.syntax.AnonymousNameType")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.scala.syntax.PredefString
unPredefString :: Phantoms.TTerm Syntax.PredefString -> Phantoms.TTerm String
unPredefString x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.scala.syntax.PredefString")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.scala.syntax.Self
unSelf :: Phantoms.TTerm Syntax.Self -> Phantoms.TTerm ()
unSelf x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.scala.syntax.Self")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.scala.syntax.ThisData
unThisData :: Phantoms.TTerm Syntax.ThisData -> Phantoms.TTerm ()
unThisData x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.scala.syntax.ThisData")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL constructor for hydra.scala.syntax.UnimportImportee
unimportImportee :: Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.UnimportImportee
unimportImportee name =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.UnimportImportee"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)}]}))
-- | DSL accessor for the name field of hydra.scala.syntax.UnimportImportee
unimportImporteeName :: Phantoms.TTerm Syntax.UnimportImportee -> Phantoms.TTerm Syntax.Name
unimportImporteeName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.UnimportImportee"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the name field of hydra.scala.syntax.UnimportImportee
unimportImporteeWithName :: Phantoms.TTerm Syntax.UnimportImportee -> Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.UnimportImportee
unimportImporteeWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.UnimportImportee"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.scala.syntax.ValDecl
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
-- | DSL accessor for the decltpe field of hydra.scala.syntax.ValDecl
valDeclDecltpe :: Phantoms.TTerm Syntax.ValDecl -> Phantoms.TTerm Syntax.Type
valDeclDecltpe x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ValDecl"),
        Core.projectionFieldName = (Core.Name "decltpe")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the mods field of hydra.scala.syntax.ValDecl
valDeclMods :: Phantoms.TTerm Syntax.ValDecl -> Phantoms.TTerm [Syntax.Mod]
valDeclMods x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ValDecl"),
        Core.projectionFieldName = (Core.Name "mods")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the pats field of hydra.scala.syntax.ValDecl
valDeclPats :: Phantoms.TTerm Syntax.ValDecl -> Phantoms.TTerm [Syntax.Pat]
valDeclPats x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ValDecl"),
        Core.projectionFieldName = (Core.Name "pats")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the decltpe field of hydra.scala.syntax.ValDecl
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
              Core.projectionFieldName = (Core.Name "mods")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "pats"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ValDecl"),
              Core.projectionFieldName = (Core.Name "pats")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the mods field of hydra.scala.syntax.ValDecl
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
              Core.projectionFieldName = (Core.Name "pats")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ValDecl"),
              Core.projectionFieldName = (Core.Name "decltpe")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the pats field of hydra.scala.syntax.ValDecl
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
              Core.projectionFieldName = (Core.Name "mods")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "pats"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ValDecl"),
              Core.projectionFieldName = (Core.Name "decltpe")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for hydra.scala.syntax.ValDefn
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
-- | DSL accessor for the decltpe field of hydra.scala.syntax.ValDefn
valDefnDecltpe :: Phantoms.TTerm Syntax.ValDefn -> Phantoms.TTerm (Maybe Syntax.Type)
valDefnDecltpe x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ValDefn"),
        Core.projectionFieldName = (Core.Name "decltpe")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the mods field of hydra.scala.syntax.ValDefn
valDefnMods :: Phantoms.TTerm Syntax.ValDefn -> Phantoms.TTerm [Syntax.Mod]
valDefnMods x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ValDefn"),
        Core.projectionFieldName = (Core.Name "mods")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the pats field of hydra.scala.syntax.ValDefn
valDefnPats :: Phantoms.TTerm Syntax.ValDefn -> Phantoms.TTerm [Syntax.Pat]
valDefnPats x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ValDefn"),
        Core.projectionFieldName = (Core.Name "pats")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the rhs field of hydra.scala.syntax.ValDefn
valDefnRhs :: Phantoms.TTerm Syntax.ValDefn -> Phantoms.TTerm Syntax.Data
valDefnRhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ValDefn"),
        Core.projectionFieldName = (Core.Name "rhs")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the decltpe field of hydra.scala.syntax.ValDefn
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
              Core.projectionFieldName = (Core.Name "mods")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "pats"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ValDefn"),
              Core.projectionFieldName = (Core.Name "pats")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ValDefn"),
              Core.projectionFieldName = (Core.Name "rhs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the mods field of hydra.scala.syntax.ValDefn
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
              Core.projectionFieldName = (Core.Name "pats")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ValDefn"),
              Core.projectionFieldName = (Core.Name "decltpe")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ValDefn"),
              Core.projectionFieldName = (Core.Name "rhs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the pats field of hydra.scala.syntax.ValDefn
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
              Core.projectionFieldName = (Core.Name "mods")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "pats"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ValDefn"),
              Core.projectionFieldName = (Core.Name "decltpe")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ValDefn"),
              Core.projectionFieldName = (Core.Name "rhs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the rhs field of hydra.scala.syntax.ValDefn
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
              Core.projectionFieldName = (Core.Name "mods")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "pats"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ValDefn"),
              Core.projectionFieldName = (Core.Name "pats")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ValDefn"),
              Core.projectionFieldName = (Core.Name "decltpe")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.scala.syntax.ValEnumerator
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
-- | DSL accessor for the pat field of hydra.scala.syntax.ValEnumerator
valEnumeratorPat :: Phantoms.TTerm Syntax.ValEnumerator -> Phantoms.TTerm Syntax.Pat
valEnumeratorPat x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ValEnumerator"),
        Core.projectionFieldName = (Core.Name "pat")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the rhs field of hydra.scala.syntax.ValEnumerator
valEnumeratorRhs :: Phantoms.TTerm Syntax.ValEnumerator -> Phantoms.TTerm Syntax.Data
valEnumeratorRhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ValEnumerator"),
        Core.projectionFieldName = (Core.Name "rhs")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the pat field of hydra.scala.syntax.ValEnumerator
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
              Core.projectionFieldName = (Core.Name "rhs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the rhs field of hydra.scala.syntax.ValEnumerator
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
              Core.projectionFieldName = (Core.Name "pat")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.scala.syntax.VarDecl
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
-- | DSL accessor for the decltpe field of hydra.scala.syntax.VarDecl
varDeclDecltpe :: Phantoms.TTerm Syntax.VarDecl -> Phantoms.TTerm Syntax.Type
varDeclDecltpe x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.VarDecl"),
        Core.projectionFieldName = (Core.Name "decltpe")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the mods field of hydra.scala.syntax.VarDecl
varDeclMods :: Phantoms.TTerm Syntax.VarDecl -> Phantoms.TTerm [Syntax.Mod]
varDeclMods x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.VarDecl"),
        Core.projectionFieldName = (Core.Name "mods")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the pats field of hydra.scala.syntax.VarDecl
varDeclPats :: Phantoms.TTerm Syntax.VarDecl -> Phantoms.TTerm [Syntax.Pat]
varDeclPats x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.VarDecl"),
        Core.projectionFieldName = (Core.Name "pats")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the decltpe field of hydra.scala.syntax.VarDecl
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
              Core.projectionFieldName = (Core.Name "mods")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "pats"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.VarDecl"),
              Core.projectionFieldName = (Core.Name "pats")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the mods field of hydra.scala.syntax.VarDecl
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
              Core.projectionFieldName = (Core.Name "pats")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.VarDecl"),
              Core.projectionFieldName = (Core.Name "decltpe")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the pats field of hydra.scala.syntax.VarDecl
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
              Core.projectionFieldName = (Core.Name "mods")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "pats"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.VarDecl"),
              Core.projectionFieldName = (Core.Name "decltpe")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for hydra.scala.syntax.VarDefn
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
-- | DSL accessor for the decltpe field of hydra.scala.syntax.VarDefn
varDefnDecltpe :: Phantoms.TTerm Syntax.VarDefn -> Phantoms.TTerm Syntax.Type
varDefnDecltpe x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.VarDefn"),
        Core.projectionFieldName = (Core.Name "decltpe")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the mods field of hydra.scala.syntax.VarDefn
varDefnMods :: Phantoms.TTerm Syntax.VarDefn -> Phantoms.TTerm [Syntax.Mod]
varDefnMods x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.VarDefn"),
        Core.projectionFieldName = (Core.Name "mods")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the pats field of hydra.scala.syntax.VarDefn
varDefnPats :: Phantoms.TTerm Syntax.VarDefn -> Phantoms.TTerm [Syntax.Pat]
varDefnPats x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.VarDefn"),
        Core.projectionFieldName = (Core.Name "pats")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the rhs field of hydra.scala.syntax.VarDefn
varDefnRhs :: Phantoms.TTerm Syntax.VarDefn -> Phantoms.TTerm (Maybe Syntax.Data)
varDefnRhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.VarDefn"),
        Core.projectionFieldName = (Core.Name "rhs")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the decltpe field of hydra.scala.syntax.VarDefn
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
              Core.projectionFieldName = (Core.Name "mods")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "pats"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.VarDefn"),
              Core.projectionFieldName = (Core.Name "pats")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.VarDefn"),
              Core.projectionFieldName = (Core.Name "rhs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the mods field of hydra.scala.syntax.VarDefn
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
              Core.projectionFieldName = (Core.Name "pats")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.VarDefn"),
              Core.projectionFieldName = (Core.Name "decltpe")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.VarDefn"),
              Core.projectionFieldName = (Core.Name "rhs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the pats field of hydra.scala.syntax.VarDefn
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
              Core.projectionFieldName = (Core.Name "mods")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "pats"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.VarDefn"),
              Core.projectionFieldName = (Core.Name "decltpe")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.VarDefn"),
              Core.projectionFieldName = (Core.Name "rhs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the rhs field of hydra.scala.syntax.VarDefn
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
              Core.projectionFieldName = (Core.Name "mods")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "pats"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.VarDefn"),
              Core.projectionFieldName = (Core.Name "pats")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.VarDefn"),
              Core.projectionFieldName = (Core.Name "decltpe")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.scala.syntax.VarPat
varPat :: Phantoms.TTerm Syntax.NameData -> Phantoms.TTerm Syntax.VarPat
varPat name =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.VarPat"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)}]}))
-- | DSL accessor for the name field of hydra.scala.syntax.VarPat
varPatName :: Phantoms.TTerm Syntax.VarPat -> Phantoms.TTerm Syntax.NameData
varPatName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.VarPat"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the name field of hydra.scala.syntax.VarPat
varPatWithName :: Phantoms.TTerm Syntax.VarPat -> Phantoms.TTerm Syntax.NameData -> Phantoms.TTerm Syntax.VarPat
varPatWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.VarPat"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.scala.syntax.VarType
varType :: Phantoms.TTerm Syntax.NameType -> Phantoms.TTerm Syntax.VarType
varType name =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.VarType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)}]}))
-- | DSL accessor for the name field of hydra.scala.syntax.VarType
varTypeName :: Phantoms.TTerm Syntax.VarType -> Phantoms.TTerm Syntax.NameType
varTypeName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.VarType"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the name field of hydra.scala.syntax.VarType
varTypeWithName :: Phantoms.TTerm Syntax.VarType -> Phantoms.TTerm Syntax.NameType -> Phantoms.TTerm Syntax.VarType
varTypeWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.VarType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.scala.syntax.WhileData
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
-- | DSL accessor for the body field of hydra.scala.syntax.WhileData
whileDataBody :: Phantoms.TTerm Syntax.WhileData -> Phantoms.TTerm Syntax.Data
whileDataBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.WhileData"),
        Core.projectionFieldName = (Core.Name "body")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the expr field of hydra.scala.syntax.WhileData
whileDataExpr :: Phantoms.TTerm Syntax.WhileData -> Phantoms.TTerm Syntax.Data
whileDataExpr x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.WhileData"),
        Core.projectionFieldName = (Core.Name "expr")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the body field of hydra.scala.syntax.WhileData
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
              Core.projectionFieldName = (Core.Name "expr")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the expr field of hydra.scala.syntax.WhileData
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
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for hydra.scala.syntax.WithType
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
-- | DSL accessor for the lhs field of hydra.scala.syntax.WithType
withTypeLhs :: Phantoms.TTerm Syntax.WithType -> Phantoms.TTerm Syntax.Type
withTypeLhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.WithType"),
        Core.projectionFieldName = (Core.Name "lhs")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the rhs field of hydra.scala.syntax.WithType
withTypeRhs :: Phantoms.TTerm Syntax.WithType -> Phantoms.TTerm Syntax.Type
withTypeRhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.WithType"),
        Core.projectionFieldName = (Core.Name "rhs")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the lhs field of hydra.scala.syntax.WithType
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
              Core.projectionFieldName = (Core.Name "rhs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the rhs field of hydra.scala.syntax.WithType
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
              Core.projectionFieldName = (Core.Name "lhs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
