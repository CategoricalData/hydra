-- Note: this is an automatically generated file. Do not edit.
-- | DSL functions for hydra.scala.syntax

module Hydra.Dsl.Scala.Syntax where
import qualified Hydra.Core as Core
import qualified Hydra.Dsl.Core as DslCore
import qualified Hydra.Scala.Syntax as Syntax
import qualified Hydra.Typed as Typed
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
import qualified Data.Int as I
-- | DSL constructor for hydra.scala.syntax.AlternativePat
alternativePat :: Typed.TypedTerm Syntax.Pat -> Typed.TypedTerm Syntax.Pat -> Typed.TypedTerm Syntax.AlternativePat
alternativePat lhs rhs =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.AlternativePat"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Typed.unTypedTerm lhs)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Typed.unTypedTerm rhs)}]}))
-- | DSL accessor for the lhs field of hydra.scala.syntax.AlternativePat
alternativePatLhs :: Typed.TypedTerm Syntax.AlternativePat -> Typed.TypedTerm Syntax.Pat
alternativePatLhs x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.AlternativePat"),
        Core.projectionFieldName = (Core.Name "lhs")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the rhs field of hydra.scala.syntax.AlternativePat
alternativePatRhs :: Typed.TypedTerm Syntax.AlternativePat -> Typed.TypedTerm Syntax.Pat
alternativePatRhs x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.AlternativePat"),
        Core.projectionFieldName = (Core.Name "rhs")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the lhs field of hydra.scala.syntax.AlternativePat
alternativePatWithLhs :: Typed.TypedTerm Syntax.AlternativePat -> Typed.TypedTerm Syntax.Pat -> Typed.TypedTerm Syntax.AlternativePat
alternativePatWithLhs original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.AlternativePat"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.AlternativePat"),
              Core.projectionFieldName = (Core.Name "rhs")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the rhs field of hydra.scala.syntax.AlternativePat
alternativePatWithRhs :: Typed.TypedTerm Syntax.AlternativePat -> Typed.TypedTerm Syntax.Pat -> Typed.TypedTerm Syntax.AlternativePat
alternativePatWithRhs original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.AlternativePat"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.AlternativePat"),
              Core.projectionFieldName = (Core.Name "lhs")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.scala.syntax.AndType
andType :: Typed.TypedTerm Syntax.Type -> Typed.TypedTerm Syntax.Type -> Typed.TypedTerm Syntax.AndType
andType lhs rhs =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.AndType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Typed.unTypedTerm lhs)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Typed.unTypedTerm rhs)}]}))
-- | DSL accessor for the lhs field of hydra.scala.syntax.AndType
andTypeLhs :: Typed.TypedTerm Syntax.AndType -> Typed.TypedTerm Syntax.Type
andTypeLhs x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.AndType"),
        Core.projectionFieldName = (Core.Name "lhs")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the rhs field of hydra.scala.syntax.AndType
andTypeRhs :: Typed.TypedTerm Syntax.AndType -> Typed.TypedTerm Syntax.Type
andTypeRhs x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.AndType"),
        Core.projectionFieldName = (Core.Name "rhs")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the lhs field of hydra.scala.syntax.AndType
andTypeWithLhs :: Typed.TypedTerm Syntax.AndType -> Typed.TypedTerm Syntax.Type -> Typed.TypedTerm Syntax.AndType
andTypeWithLhs original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.AndType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.AndType"),
              Core.projectionFieldName = (Core.Name "rhs")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the rhs field of hydra.scala.syntax.AndType
andTypeWithRhs :: Typed.TypedTerm Syntax.AndType -> Typed.TypedTerm Syntax.Type -> Typed.TypedTerm Syntax.AndType
andTypeWithRhs original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.AndType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.AndType"),
              Core.projectionFieldName = (Core.Name "lhs")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.scala.syntax.AnnotMod
annotMod :: Typed.TypedTerm Syntax.Init -> Typed.TypedTerm Syntax.AnnotMod
annotMod init =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.AnnotMod"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "init"),
          Core.fieldTerm = (Typed.unTypedTerm init)}]}))
-- | DSL accessor for the init field of hydra.scala.syntax.AnnotMod
annotModInit :: Typed.TypedTerm Syntax.AnnotMod -> Typed.TypedTerm Syntax.Init
annotModInit x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.AnnotMod"),
        Core.projectionFieldName = (Core.Name "init")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the init field of hydra.scala.syntax.AnnotMod
annotModWithInit :: Typed.TypedTerm Syntax.AnnotMod -> Typed.TypedTerm Syntax.Init -> Typed.TypedTerm Syntax.AnnotMod
annotModWithInit original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.AnnotMod"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "init"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.scala.syntax.AnnotateData
annotateData :: Typed.TypedTerm Syntax.Data -> Typed.TypedTerm [Syntax.AnnotMod] -> Typed.TypedTerm Syntax.AnnotateData
annotateData expr annots =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.AnnotateData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expr"),
          Core.fieldTerm = (Typed.unTypedTerm expr)},
        Core.Field {
          Core.fieldName = (Core.Name "annots"),
          Core.fieldTerm = (Typed.unTypedTerm annots)}]}))
-- | DSL accessor for the annots field of hydra.scala.syntax.AnnotateData
annotateDataAnnots :: Typed.TypedTerm Syntax.AnnotateData -> Typed.TypedTerm [Syntax.AnnotMod]
annotateDataAnnots x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.AnnotateData"),
        Core.projectionFieldName = (Core.Name "annots")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the expr field of hydra.scala.syntax.AnnotateData
annotateDataExpr :: Typed.TypedTerm Syntax.AnnotateData -> Typed.TypedTerm Syntax.Data
annotateDataExpr x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.AnnotateData"),
        Core.projectionFieldName = (Core.Name "expr")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the annots field of hydra.scala.syntax.AnnotateData
annotateDataWithAnnots :: Typed.TypedTerm Syntax.AnnotateData -> Typed.TypedTerm [Syntax.AnnotMod] -> Typed.TypedTerm Syntax.AnnotateData
annotateDataWithAnnots original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.AnnotateData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expr"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.AnnotateData"),
              Core.projectionFieldName = (Core.Name "expr")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "annots"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the expr field of hydra.scala.syntax.AnnotateData
annotateDataWithExpr :: Typed.TypedTerm Syntax.AnnotateData -> Typed.TypedTerm Syntax.Data -> Typed.TypedTerm Syntax.AnnotateData
annotateDataWithExpr original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.AnnotateData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expr"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "annots"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.AnnotateData"),
              Core.projectionFieldName = (Core.Name "annots")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.scala.syntax.AnnotateType
annotateType :: Typed.TypedTerm Syntax.Type -> Typed.TypedTerm [Syntax.AnnotMod] -> Typed.TypedTerm Syntax.AnnotateType
annotateType tpe annots =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.AnnotateType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "tpe"),
          Core.fieldTerm = (Typed.unTypedTerm tpe)},
        Core.Field {
          Core.fieldName = (Core.Name "annots"),
          Core.fieldTerm = (Typed.unTypedTerm annots)}]}))
-- | DSL accessor for the annots field of hydra.scala.syntax.AnnotateType
annotateTypeAnnots :: Typed.TypedTerm Syntax.AnnotateType -> Typed.TypedTerm [Syntax.AnnotMod]
annotateTypeAnnots x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.AnnotateType"),
        Core.projectionFieldName = (Core.Name "annots")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the tpe field of hydra.scala.syntax.AnnotateType
annotateTypeTpe :: Typed.TypedTerm Syntax.AnnotateType -> Typed.TypedTerm Syntax.Type
annotateTypeTpe x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.AnnotateType"),
        Core.projectionFieldName = (Core.Name "tpe")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the annots field of hydra.scala.syntax.AnnotateType
annotateTypeWithAnnots :: Typed.TypedTerm Syntax.AnnotateType -> Typed.TypedTerm [Syntax.AnnotMod] -> Typed.TypedTerm Syntax.AnnotateType
annotateTypeWithAnnots original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.AnnotateType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "tpe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.AnnotateType"),
              Core.projectionFieldName = (Core.Name "tpe")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "annots"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the tpe field of hydra.scala.syntax.AnnotateType
annotateTypeWithTpe :: Typed.TypedTerm Syntax.AnnotateType -> Typed.TypedTerm Syntax.Type -> Typed.TypedTerm Syntax.AnnotateType
annotateTypeWithTpe original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.AnnotateType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "tpe"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "annots"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.AnnotateType"),
              Core.projectionFieldName = (Core.Name "annots")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for the hydra.scala.syntax.AnonymousData wrapper
anonymousData :: Typed.TypedTerm () -> Typed.TypedTerm Syntax.AnonymousData
anonymousData x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.scala.syntax.AnonymousData"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for the hydra.scala.syntax.AnonymousNameType wrapper
anonymousNameType :: Typed.TypedTerm () -> Typed.TypedTerm Syntax.AnonymousNameType
anonymousNameType x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.scala.syntax.AnonymousNameType"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.scala.syntax.ApplyData
applyData :: Typed.TypedTerm Syntax.Data -> Typed.TypedTerm [Syntax.Data] -> Typed.TypedTerm Syntax.ApplyData
applyData fun args =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ApplyData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "fun"),
          Core.fieldTerm = (Typed.unTypedTerm fun)},
        Core.Field {
          Core.fieldName = (Core.Name "args"),
          Core.fieldTerm = (Typed.unTypedTerm args)}]}))
-- | DSL accessor for the args field of hydra.scala.syntax.ApplyData
applyDataArgs :: Typed.TypedTerm Syntax.ApplyData -> Typed.TypedTerm [Syntax.Data]
applyDataArgs x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ApplyData"),
        Core.projectionFieldName = (Core.Name "args")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the fun field of hydra.scala.syntax.ApplyData
applyDataFun :: Typed.TypedTerm Syntax.ApplyData -> Typed.TypedTerm Syntax.Data
applyDataFun x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ApplyData"),
        Core.projectionFieldName = (Core.Name "fun")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the args field of hydra.scala.syntax.ApplyData
applyDataWithArgs :: Typed.TypedTerm Syntax.ApplyData -> Typed.TypedTerm [Syntax.Data] -> Typed.TypedTerm Syntax.ApplyData
applyDataWithArgs original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ApplyData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "fun"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ApplyData"),
              Core.projectionFieldName = (Core.Name "fun")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "args"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the fun field of hydra.scala.syntax.ApplyData
applyDataWithFun :: Typed.TypedTerm Syntax.ApplyData -> Typed.TypedTerm Syntax.Data -> Typed.TypedTerm Syntax.ApplyData
applyDataWithFun original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ApplyData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "fun"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "args"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ApplyData"),
              Core.projectionFieldName = (Core.Name "args")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.scala.syntax.ApplyInfixData
applyInfixData :: Typed.TypedTerm Syntax.Data -> Typed.TypedTerm Syntax.NameData -> Typed.TypedTerm [Syntax.Type] -> Typed.TypedTerm [Syntax.Data] -> Typed.TypedTerm Syntax.ApplyInfixData
applyInfixData lhs op targs args =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ApplyInfixData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Typed.unTypedTerm lhs)},
        Core.Field {
          Core.fieldName = (Core.Name "op"),
          Core.fieldTerm = (Typed.unTypedTerm op)},
        Core.Field {
          Core.fieldName = (Core.Name "targs"),
          Core.fieldTerm = (Typed.unTypedTerm targs)},
        Core.Field {
          Core.fieldName = (Core.Name "args"),
          Core.fieldTerm = (Typed.unTypedTerm args)}]}))
-- | DSL accessor for the args field of hydra.scala.syntax.ApplyInfixData
applyInfixDataArgs :: Typed.TypedTerm Syntax.ApplyInfixData -> Typed.TypedTerm [Syntax.Data]
applyInfixDataArgs x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ApplyInfixData"),
        Core.projectionFieldName = (Core.Name "args")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the lhs field of hydra.scala.syntax.ApplyInfixData
applyInfixDataLhs :: Typed.TypedTerm Syntax.ApplyInfixData -> Typed.TypedTerm Syntax.Data
applyInfixDataLhs x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ApplyInfixData"),
        Core.projectionFieldName = (Core.Name "lhs")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the op field of hydra.scala.syntax.ApplyInfixData
applyInfixDataOp :: Typed.TypedTerm Syntax.ApplyInfixData -> Typed.TypedTerm Syntax.NameData
applyInfixDataOp x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ApplyInfixData"),
        Core.projectionFieldName = (Core.Name "op")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the targs field of hydra.scala.syntax.ApplyInfixData
applyInfixDataTargs :: Typed.TypedTerm Syntax.ApplyInfixData -> Typed.TypedTerm [Syntax.Type]
applyInfixDataTargs x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ApplyInfixData"),
        Core.projectionFieldName = (Core.Name "targs")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the args field of hydra.scala.syntax.ApplyInfixData
applyInfixDataWithArgs :: Typed.TypedTerm Syntax.ApplyInfixData -> Typed.TypedTerm [Syntax.Data] -> Typed.TypedTerm Syntax.ApplyInfixData
applyInfixDataWithArgs original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ApplyInfixData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ApplyInfixData"),
              Core.projectionFieldName = (Core.Name "lhs")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "op"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ApplyInfixData"),
              Core.projectionFieldName = (Core.Name "op")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "targs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ApplyInfixData"),
              Core.projectionFieldName = (Core.Name "targs")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "args"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the lhs field of hydra.scala.syntax.ApplyInfixData
applyInfixDataWithLhs :: Typed.TypedTerm Syntax.ApplyInfixData -> Typed.TypedTerm Syntax.Data -> Typed.TypedTerm Syntax.ApplyInfixData
applyInfixDataWithLhs original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ApplyInfixData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "op"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ApplyInfixData"),
              Core.projectionFieldName = (Core.Name "op")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "targs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ApplyInfixData"),
              Core.projectionFieldName = (Core.Name "targs")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "args"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ApplyInfixData"),
              Core.projectionFieldName = (Core.Name "args")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the op field of hydra.scala.syntax.ApplyInfixData
applyInfixDataWithOp :: Typed.TypedTerm Syntax.ApplyInfixData -> Typed.TypedTerm Syntax.NameData -> Typed.TypedTerm Syntax.ApplyInfixData
applyInfixDataWithOp original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ApplyInfixData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ApplyInfixData"),
              Core.projectionFieldName = (Core.Name "lhs")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "op"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "targs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ApplyInfixData"),
              Core.projectionFieldName = (Core.Name "targs")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "args"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ApplyInfixData"),
              Core.projectionFieldName = (Core.Name "args")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the targs field of hydra.scala.syntax.ApplyInfixData
applyInfixDataWithTargs :: Typed.TypedTerm Syntax.ApplyInfixData -> Typed.TypedTerm [Syntax.Type] -> Typed.TypedTerm Syntax.ApplyInfixData
applyInfixDataWithTargs original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ApplyInfixData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ApplyInfixData"),
              Core.projectionFieldName = (Core.Name "lhs")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "op"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ApplyInfixData"),
              Core.projectionFieldName = (Core.Name "op")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "targs"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "args"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ApplyInfixData"),
              Core.projectionFieldName = (Core.Name "args")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.scala.syntax.ApplyInfixType
applyInfixType :: Typed.TypedTerm Syntax.Type -> Typed.TypedTerm Syntax.NameType -> Typed.TypedTerm Syntax.Type -> Typed.TypedTerm Syntax.ApplyInfixType
applyInfixType lhs op rhs =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ApplyInfixType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Typed.unTypedTerm lhs)},
        Core.Field {
          Core.fieldName = (Core.Name "op"),
          Core.fieldTerm = (Typed.unTypedTerm op)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Typed.unTypedTerm rhs)}]}))
-- | DSL accessor for the lhs field of hydra.scala.syntax.ApplyInfixType
applyInfixTypeLhs :: Typed.TypedTerm Syntax.ApplyInfixType -> Typed.TypedTerm Syntax.Type
applyInfixTypeLhs x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ApplyInfixType"),
        Core.projectionFieldName = (Core.Name "lhs")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the op field of hydra.scala.syntax.ApplyInfixType
applyInfixTypeOp :: Typed.TypedTerm Syntax.ApplyInfixType -> Typed.TypedTerm Syntax.NameType
applyInfixTypeOp x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ApplyInfixType"),
        Core.projectionFieldName = (Core.Name "op")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the rhs field of hydra.scala.syntax.ApplyInfixType
applyInfixTypeRhs :: Typed.TypedTerm Syntax.ApplyInfixType -> Typed.TypedTerm Syntax.Type
applyInfixTypeRhs x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ApplyInfixType"),
        Core.projectionFieldName = (Core.Name "rhs")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the lhs field of hydra.scala.syntax.ApplyInfixType
applyInfixTypeWithLhs :: Typed.TypedTerm Syntax.ApplyInfixType -> Typed.TypedTerm Syntax.Type -> Typed.TypedTerm Syntax.ApplyInfixType
applyInfixTypeWithLhs original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ApplyInfixType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "op"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ApplyInfixType"),
              Core.projectionFieldName = (Core.Name "op")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ApplyInfixType"),
              Core.projectionFieldName = (Core.Name "rhs")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the op field of hydra.scala.syntax.ApplyInfixType
applyInfixTypeWithOp :: Typed.TypedTerm Syntax.ApplyInfixType -> Typed.TypedTerm Syntax.NameType -> Typed.TypedTerm Syntax.ApplyInfixType
applyInfixTypeWithOp original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ApplyInfixType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ApplyInfixType"),
              Core.projectionFieldName = (Core.Name "lhs")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "op"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ApplyInfixType"),
              Core.projectionFieldName = (Core.Name "rhs")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the rhs field of hydra.scala.syntax.ApplyInfixType
applyInfixTypeWithRhs :: Typed.TypedTerm Syntax.ApplyInfixType -> Typed.TypedTerm Syntax.Type -> Typed.TypedTerm Syntax.ApplyInfixType
applyInfixTypeWithRhs original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ApplyInfixType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ApplyInfixType"),
              Core.projectionFieldName = (Core.Name "lhs")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "op"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ApplyInfixType"),
              Core.projectionFieldName = (Core.Name "op")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.scala.syntax.ApplyType
applyType :: Typed.TypedTerm Syntax.Type -> Typed.TypedTerm [Syntax.Type] -> Typed.TypedTerm Syntax.ApplyType
applyType tpe args =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ApplyType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "tpe"),
          Core.fieldTerm = (Typed.unTypedTerm tpe)},
        Core.Field {
          Core.fieldName = (Core.Name "args"),
          Core.fieldTerm = (Typed.unTypedTerm args)}]}))
-- | DSL accessor for the args field of hydra.scala.syntax.ApplyType
applyTypeArgs :: Typed.TypedTerm Syntax.ApplyType -> Typed.TypedTerm [Syntax.Type]
applyTypeArgs x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ApplyType"),
        Core.projectionFieldName = (Core.Name "args")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.scala.syntax.ApplyTypeData
applyTypeData :: Typed.TypedTerm Syntax.Data -> Typed.TypedTerm Syntax.NameData -> Typed.TypedTerm [Syntax.Type] -> Typed.TypedTerm [Syntax.Data] -> Typed.TypedTerm Syntax.ApplyTypeData
applyTypeData lhs op targs args =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ApplyTypeData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Typed.unTypedTerm lhs)},
        Core.Field {
          Core.fieldName = (Core.Name "op"),
          Core.fieldTerm = (Typed.unTypedTerm op)},
        Core.Field {
          Core.fieldName = (Core.Name "targs"),
          Core.fieldTerm = (Typed.unTypedTerm targs)},
        Core.Field {
          Core.fieldName = (Core.Name "args"),
          Core.fieldTerm = (Typed.unTypedTerm args)}]}))
-- | DSL accessor for the args field of hydra.scala.syntax.ApplyTypeData
applyTypeDataArgs :: Typed.TypedTerm Syntax.ApplyTypeData -> Typed.TypedTerm [Syntax.Data]
applyTypeDataArgs x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ApplyTypeData"),
        Core.projectionFieldName = (Core.Name "args")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the lhs field of hydra.scala.syntax.ApplyTypeData
applyTypeDataLhs :: Typed.TypedTerm Syntax.ApplyTypeData -> Typed.TypedTerm Syntax.Data
applyTypeDataLhs x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ApplyTypeData"),
        Core.projectionFieldName = (Core.Name "lhs")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the op field of hydra.scala.syntax.ApplyTypeData
applyTypeDataOp :: Typed.TypedTerm Syntax.ApplyTypeData -> Typed.TypedTerm Syntax.NameData
applyTypeDataOp x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ApplyTypeData"),
        Core.projectionFieldName = (Core.Name "op")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the targs field of hydra.scala.syntax.ApplyTypeData
applyTypeDataTargs :: Typed.TypedTerm Syntax.ApplyTypeData -> Typed.TypedTerm [Syntax.Type]
applyTypeDataTargs x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ApplyTypeData"),
        Core.projectionFieldName = (Core.Name "targs")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the args field of hydra.scala.syntax.ApplyTypeData
applyTypeDataWithArgs :: Typed.TypedTerm Syntax.ApplyTypeData -> Typed.TypedTerm [Syntax.Data] -> Typed.TypedTerm Syntax.ApplyTypeData
applyTypeDataWithArgs original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ApplyTypeData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ApplyTypeData"),
              Core.projectionFieldName = (Core.Name "lhs")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "op"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ApplyTypeData"),
              Core.projectionFieldName = (Core.Name "op")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "targs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ApplyTypeData"),
              Core.projectionFieldName = (Core.Name "targs")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "args"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the lhs field of hydra.scala.syntax.ApplyTypeData
applyTypeDataWithLhs :: Typed.TypedTerm Syntax.ApplyTypeData -> Typed.TypedTerm Syntax.Data -> Typed.TypedTerm Syntax.ApplyTypeData
applyTypeDataWithLhs original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ApplyTypeData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "op"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ApplyTypeData"),
              Core.projectionFieldName = (Core.Name "op")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "targs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ApplyTypeData"),
              Core.projectionFieldName = (Core.Name "targs")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "args"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ApplyTypeData"),
              Core.projectionFieldName = (Core.Name "args")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the op field of hydra.scala.syntax.ApplyTypeData
applyTypeDataWithOp :: Typed.TypedTerm Syntax.ApplyTypeData -> Typed.TypedTerm Syntax.NameData -> Typed.TypedTerm Syntax.ApplyTypeData
applyTypeDataWithOp original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ApplyTypeData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ApplyTypeData"),
              Core.projectionFieldName = (Core.Name "lhs")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "op"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "targs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ApplyTypeData"),
              Core.projectionFieldName = (Core.Name "targs")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "args"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ApplyTypeData"),
              Core.projectionFieldName = (Core.Name "args")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the targs field of hydra.scala.syntax.ApplyTypeData
applyTypeDataWithTargs :: Typed.TypedTerm Syntax.ApplyTypeData -> Typed.TypedTerm [Syntax.Type] -> Typed.TypedTerm Syntax.ApplyTypeData
applyTypeDataWithTargs original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ApplyTypeData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ApplyTypeData"),
              Core.projectionFieldName = (Core.Name "lhs")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "op"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ApplyTypeData"),
              Core.projectionFieldName = (Core.Name "op")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "targs"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "args"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ApplyTypeData"),
              Core.projectionFieldName = (Core.Name "args")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL accessor for the tpe field of hydra.scala.syntax.ApplyType
applyTypeTpe :: Typed.TypedTerm Syntax.ApplyType -> Typed.TypedTerm Syntax.Type
applyTypeTpe x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ApplyType"),
        Core.projectionFieldName = (Core.Name "tpe")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the args field of hydra.scala.syntax.ApplyType
applyTypeWithArgs :: Typed.TypedTerm Syntax.ApplyType -> Typed.TypedTerm [Syntax.Type] -> Typed.TypedTerm Syntax.ApplyType
applyTypeWithArgs original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ApplyType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "tpe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ApplyType"),
              Core.projectionFieldName = (Core.Name "tpe")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "args"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the tpe field of hydra.scala.syntax.ApplyType
applyTypeWithTpe :: Typed.TypedTerm Syntax.ApplyType -> Typed.TypedTerm Syntax.Type -> Typed.TypedTerm Syntax.ApplyType
applyTypeWithTpe original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ApplyType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "tpe"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "args"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ApplyType"),
              Core.projectionFieldName = (Core.Name "args")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.scala.syntax.ApplyUnaryData
applyUnaryData :: Typed.TypedTerm Syntax.NameData -> Typed.TypedTerm Syntax.Data -> Typed.TypedTerm Syntax.ApplyUnaryData
applyUnaryData op arg =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ApplyUnaryData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "op"),
          Core.fieldTerm = (Typed.unTypedTerm op)},
        Core.Field {
          Core.fieldName = (Core.Name "arg"),
          Core.fieldTerm = (Typed.unTypedTerm arg)}]}))
-- | DSL accessor for the arg field of hydra.scala.syntax.ApplyUnaryData
applyUnaryDataArg :: Typed.TypedTerm Syntax.ApplyUnaryData -> Typed.TypedTerm Syntax.Data
applyUnaryDataArg x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ApplyUnaryData"),
        Core.projectionFieldName = (Core.Name "arg")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the op field of hydra.scala.syntax.ApplyUnaryData
applyUnaryDataOp :: Typed.TypedTerm Syntax.ApplyUnaryData -> Typed.TypedTerm Syntax.NameData
applyUnaryDataOp x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ApplyUnaryData"),
        Core.projectionFieldName = (Core.Name "op")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the arg field of hydra.scala.syntax.ApplyUnaryData
applyUnaryDataWithArg :: Typed.TypedTerm Syntax.ApplyUnaryData -> Typed.TypedTerm Syntax.Data -> Typed.TypedTerm Syntax.ApplyUnaryData
applyUnaryDataWithArg original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ApplyUnaryData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "op"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ApplyUnaryData"),
              Core.projectionFieldName = (Core.Name "op")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "arg"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the op field of hydra.scala.syntax.ApplyUnaryData
applyUnaryDataWithOp :: Typed.TypedTerm Syntax.ApplyUnaryData -> Typed.TypedTerm Syntax.NameData -> Typed.TypedTerm Syntax.ApplyUnaryData
applyUnaryDataWithOp original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ApplyUnaryData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "op"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "arg"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ApplyUnaryData"),
              Core.projectionFieldName = (Core.Name "arg")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.scala.syntax.ApplyUsingData
applyUsingData :: Typed.TypedTerm Syntax.Data -> Typed.TypedTerm [Syntax.Data] -> Typed.TypedTerm Syntax.ApplyUsingData
applyUsingData fun targs =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ApplyUsingData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "fun"),
          Core.fieldTerm = (Typed.unTypedTerm fun)},
        Core.Field {
          Core.fieldName = (Core.Name "targs"),
          Core.fieldTerm = (Typed.unTypedTerm targs)}]}))
-- | DSL accessor for the fun field of hydra.scala.syntax.ApplyUsingData
applyUsingDataFun :: Typed.TypedTerm Syntax.ApplyUsingData -> Typed.TypedTerm Syntax.Data
applyUsingDataFun x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ApplyUsingData"),
        Core.projectionFieldName = (Core.Name "fun")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the targs field of hydra.scala.syntax.ApplyUsingData
applyUsingDataTargs :: Typed.TypedTerm Syntax.ApplyUsingData -> Typed.TypedTerm [Syntax.Data]
applyUsingDataTargs x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ApplyUsingData"),
        Core.projectionFieldName = (Core.Name "targs")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the fun field of hydra.scala.syntax.ApplyUsingData
applyUsingDataWithFun :: Typed.TypedTerm Syntax.ApplyUsingData -> Typed.TypedTerm Syntax.Data -> Typed.TypedTerm Syntax.ApplyUsingData
applyUsingDataWithFun original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ApplyUsingData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "fun"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "targs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ApplyUsingData"),
              Core.projectionFieldName = (Core.Name "targs")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the targs field of hydra.scala.syntax.ApplyUsingData
applyUsingDataWithTargs :: Typed.TypedTerm Syntax.ApplyUsingData -> Typed.TypedTerm [Syntax.Data] -> Typed.TypedTerm Syntax.ApplyUsingData
applyUsingDataWithTargs original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ApplyUsingData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "fun"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ApplyUsingData"),
              Core.projectionFieldName = (Core.Name "fun")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "targs"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.scala.syntax.AscribeData
ascribeData :: Typed.TypedTerm Syntax.Data -> Typed.TypedTerm Syntax.Type -> Typed.TypedTerm Syntax.AscribeData
ascribeData expr tpe =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.AscribeData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expr"),
          Core.fieldTerm = (Typed.unTypedTerm expr)},
        Core.Field {
          Core.fieldName = (Core.Name "tpe"),
          Core.fieldTerm = (Typed.unTypedTerm tpe)}]}))
-- | DSL accessor for the expr field of hydra.scala.syntax.AscribeData
ascribeDataExpr :: Typed.TypedTerm Syntax.AscribeData -> Typed.TypedTerm Syntax.Data
ascribeDataExpr x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.AscribeData"),
        Core.projectionFieldName = (Core.Name "expr")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the tpe field of hydra.scala.syntax.AscribeData
ascribeDataTpe :: Typed.TypedTerm Syntax.AscribeData -> Typed.TypedTerm Syntax.Type
ascribeDataTpe x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.AscribeData"),
        Core.projectionFieldName = (Core.Name "tpe")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the expr field of hydra.scala.syntax.AscribeData
ascribeDataWithExpr :: Typed.TypedTerm Syntax.AscribeData -> Typed.TypedTerm Syntax.Data -> Typed.TypedTerm Syntax.AscribeData
ascribeDataWithExpr original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.AscribeData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expr"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "tpe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.AscribeData"),
              Core.projectionFieldName = (Core.Name "tpe")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the tpe field of hydra.scala.syntax.AscribeData
ascribeDataWithTpe :: Typed.TypedTerm Syntax.AscribeData -> Typed.TypedTerm Syntax.Type -> Typed.TypedTerm Syntax.AscribeData
ascribeDataWithTpe original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.AscribeData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expr"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.AscribeData"),
              Core.projectionFieldName = (Core.Name "expr")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tpe"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.scala.syntax.AssignData
assignData :: Typed.TypedTerm Syntax.Data -> Typed.TypedTerm Syntax.Data -> Typed.TypedTerm Syntax.AssignData
assignData lhs rhs =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.AssignData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Typed.unTypedTerm lhs)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Typed.unTypedTerm rhs)}]}))
-- | DSL accessor for the lhs field of hydra.scala.syntax.AssignData
assignDataLhs :: Typed.TypedTerm Syntax.AssignData -> Typed.TypedTerm Syntax.Data
assignDataLhs x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.AssignData"),
        Core.projectionFieldName = (Core.Name "lhs")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the rhs field of hydra.scala.syntax.AssignData
assignDataRhs :: Typed.TypedTerm Syntax.AssignData -> Typed.TypedTerm Syntax.Data
assignDataRhs x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.AssignData"),
        Core.projectionFieldName = (Core.Name "rhs")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the lhs field of hydra.scala.syntax.AssignData
assignDataWithLhs :: Typed.TypedTerm Syntax.AssignData -> Typed.TypedTerm Syntax.Data -> Typed.TypedTerm Syntax.AssignData
assignDataWithLhs original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.AssignData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.AssignData"),
              Core.projectionFieldName = (Core.Name "rhs")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the rhs field of hydra.scala.syntax.AssignData
assignDataWithRhs :: Typed.TypedTerm Syntax.AssignData -> Typed.TypedTerm Syntax.Data -> Typed.TypedTerm Syntax.AssignData
assignDataWithRhs original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.AssignData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.AssignData"),
              Core.projectionFieldName = (Core.Name "lhs")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.scala.syntax.BindPat
bindPat :: Typed.TypedTerm Syntax.Pat -> Typed.TypedTerm Syntax.Pat -> Typed.TypedTerm Syntax.BindPat
bindPat lhs rhs =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.BindPat"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Typed.unTypedTerm lhs)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Typed.unTypedTerm rhs)}]}))
-- | DSL accessor for the lhs field of hydra.scala.syntax.BindPat
bindPatLhs :: Typed.TypedTerm Syntax.BindPat -> Typed.TypedTerm Syntax.Pat
bindPatLhs x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.BindPat"),
        Core.projectionFieldName = (Core.Name "lhs")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the rhs field of hydra.scala.syntax.BindPat
bindPatRhs :: Typed.TypedTerm Syntax.BindPat -> Typed.TypedTerm Syntax.Pat
bindPatRhs x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.BindPat"),
        Core.projectionFieldName = (Core.Name "rhs")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the lhs field of hydra.scala.syntax.BindPat
bindPatWithLhs :: Typed.TypedTerm Syntax.BindPat -> Typed.TypedTerm Syntax.Pat -> Typed.TypedTerm Syntax.BindPat
bindPatWithLhs original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.BindPat"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.BindPat"),
              Core.projectionFieldName = (Core.Name "rhs")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the rhs field of hydra.scala.syntax.BindPat
bindPatWithRhs :: Typed.TypedTerm Syntax.BindPat -> Typed.TypedTerm Syntax.Pat -> Typed.TypedTerm Syntax.BindPat
bindPatWithRhs original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.BindPat"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.BindPat"),
              Core.projectionFieldName = (Core.Name "lhs")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.scala.syntax.BlockData
blockData :: Typed.TypedTerm [Syntax.Stat] -> Typed.TypedTerm Syntax.BlockData
blockData stats =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.BlockData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "stats"),
          Core.fieldTerm = (Typed.unTypedTerm stats)}]}))
-- | DSL accessor for the stats field of hydra.scala.syntax.BlockData
blockDataStats :: Typed.TypedTerm Syntax.BlockData -> Typed.TypedTerm [Syntax.Stat]
blockDataStats x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.BlockData"),
        Core.projectionFieldName = (Core.Name "stats")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the stats field of hydra.scala.syntax.BlockData
blockDataWithStats :: Typed.TypedTerm Syntax.BlockData -> Typed.TypedTerm [Syntax.Stat] -> Typed.TypedTerm Syntax.BlockData
blockDataWithStats original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.BlockData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "stats"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.scala.syntax.ByNameType
byNameType :: Typed.TypedTerm Syntax.Type -> Typed.TypedTerm Syntax.ByNameType
byNameType tpe =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ByNameType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "tpe"),
          Core.fieldTerm = (Typed.unTypedTerm tpe)}]}))
-- | DSL accessor for the tpe field of hydra.scala.syntax.ByNameType
byNameTypeTpe :: Typed.TypedTerm Syntax.ByNameType -> Typed.TypedTerm Syntax.Type
byNameTypeTpe x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ByNameType"),
        Core.projectionFieldName = (Core.Name "tpe")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the tpe field of hydra.scala.syntax.ByNameType
byNameTypeWithTpe :: Typed.TypedTerm Syntax.ByNameType -> Typed.TypedTerm Syntax.Type -> Typed.TypedTerm Syntax.ByNameType
byNameTypeWithTpe original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ByNameType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "tpe"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.scala.syntax.Case
case_ :: Typed.TypedTerm Syntax.Pat -> Typed.TypedTerm (Maybe Syntax.Data) -> Typed.TypedTerm Syntax.Data -> Typed.TypedTerm Syntax.Case
case_ pat cond body =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.Case"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pat"),
          Core.fieldTerm = (Typed.unTypedTerm pat)},
        Core.Field {
          Core.fieldName = (Core.Name "cond"),
          Core.fieldTerm = (Typed.unTypedTerm cond)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm body)}]}))
-- | DSL accessor for the body field of hydra.scala.syntax.Case
caseBody :: Typed.TypedTerm Syntax.Case -> Typed.TypedTerm Syntax.Data
caseBody x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.Case"),
        Core.projectionFieldName = (Core.Name "body")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the cond field of hydra.scala.syntax.Case
caseCond :: Typed.TypedTerm Syntax.Case -> Typed.TypedTerm (Maybe Syntax.Data)
caseCond x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.Case"),
        Core.projectionFieldName = (Core.Name "cond")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.scala.syntax.CaseGeneratorEnumerator
caseGeneratorEnumerator :: Typed.TypedTerm Syntax.Pat -> Typed.TypedTerm Syntax.Data -> Typed.TypedTerm Syntax.CaseGeneratorEnumerator
caseGeneratorEnumerator pat rhs =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.CaseGeneratorEnumerator"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pat"),
          Core.fieldTerm = (Typed.unTypedTerm pat)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Typed.unTypedTerm rhs)}]}))
-- | DSL accessor for the pat field of hydra.scala.syntax.CaseGeneratorEnumerator
caseGeneratorEnumeratorPat :: Typed.TypedTerm Syntax.CaseGeneratorEnumerator -> Typed.TypedTerm Syntax.Pat
caseGeneratorEnumeratorPat x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.CaseGeneratorEnumerator"),
        Core.projectionFieldName = (Core.Name "pat")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the rhs field of hydra.scala.syntax.CaseGeneratorEnumerator
caseGeneratorEnumeratorRhs :: Typed.TypedTerm Syntax.CaseGeneratorEnumerator -> Typed.TypedTerm Syntax.Data
caseGeneratorEnumeratorRhs x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.CaseGeneratorEnumerator"),
        Core.projectionFieldName = (Core.Name "rhs")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the pat field of hydra.scala.syntax.CaseGeneratorEnumerator
caseGeneratorEnumeratorWithPat :: Typed.TypedTerm Syntax.CaseGeneratorEnumerator -> Typed.TypedTerm Syntax.Pat -> Typed.TypedTerm Syntax.CaseGeneratorEnumerator
caseGeneratorEnumeratorWithPat original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.CaseGeneratorEnumerator"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pat"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.CaseGeneratorEnumerator"),
              Core.projectionFieldName = (Core.Name "rhs")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the rhs field of hydra.scala.syntax.CaseGeneratorEnumerator
caseGeneratorEnumeratorWithRhs :: Typed.TypedTerm Syntax.CaseGeneratorEnumerator -> Typed.TypedTerm Syntax.Data -> Typed.TypedTerm Syntax.CaseGeneratorEnumerator
caseGeneratorEnumeratorWithRhs original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.CaseGeneratorEnumerator"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pat"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.CaseGeneratorEnumerator"),
              Core.projectionFieldName = (Core.Name "pat")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL accessor for the pat field of hydra.scala.syntax.Case
casePat :: Typed.TypedTerm Syntax.Case -> Typed.TypedTerm Syntax.Pat
casePat x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.Case"),
        Core.projectionFieldName = (Core.Name "pat")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL injection for the case variant of hydra.scala.syntax.CaseTree
caseTreeCase :: Typed.TypedTerm Syntax.Case -> Typed.TypedTerm Syntax.CaseTree
caseTreeCase x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.CaseTree"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "case"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the typeCase variant of hydra.scala.syntax.CaseTree
caseTreeTypeCase :: Typed.TypedTerm Syntax.TypeCase -> Typed.TypedTerm Syntax.CaseTree
caseTreeTypeCase x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.CaseTree"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "typeCase"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL updater for the body field of hydra.scala.syntax.Case
caseWithBody :: Typed.TypedTerm Syntax.Case -> Typed.TypedTerm Syntax.Data -> Typed.TypedTerm Syntax.Case
caseWithBody original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.Case"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pat"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.Case"),
              Core.projectionFieldName = (Core.Name "pat")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "cond"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.Case"),
              Core.projectionFieldName = (Core.Name "cond")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the cond field of hydra.scala.syntax.Case
caseWithCond :: Typed.TypedTerm Syntax.Case -> Typed.TypedTerm (Maybe Syntax.Data) -> Typed.TypedTerm Syntax.Case
caseWithCond original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.Case"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pat"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.Case"),
              Core.projectionFieldName = (Core.Name "pat")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "cond"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.Case"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the pat field of hydra.scala.syntax.Case
caseWithPat :: Typed.TypedTerm Syntax.Case -> Typed.TypedTerm Syntax.Pat -> Typed.TypedTerm Syntax.Case
caseWithPat original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.Case"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pat"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "cond"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.Case"),
              Core.projectionFieldName = (Core.Name "cond")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.Case"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.scala.syntax.ClassDefn
classDefn :: Typed.TypedTerm [Syntax.Mod] -> Typed.TypedTerm Syntax.NameType -> Typed.TypedTerm [Syntax.ParamType] -> Typed.TypedTerm Syntax.PrimaryCtor -> Typed.TypedTerm Syntax.Template -> Typed.TypedTerm Syntax.ClassDefn
classDefn mods name tparams ctor template =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ClassDefn"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Typed.unTypedTerm mods)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Typed.unTypedTerm tparams)},
        Core.Field {
          Core.fieldName = (Core.Name "ctor"),
          Core.fieldTerm = (Typed.unTypedTerm ctor)},
        Core.Field {
          Core.fieldName = (Core.Name "template"),
          Core.fieldTerm = (Typed.unTypedTerm template)}]}))
-- | DSL accessor for the ctor field of hydra.scala.syntax.ClassDefn
classDefnCtor :: Typed.TypedTerm Syntax.ClassDefn -> Typed.TypedTerm Syntax.PrimaryCtor
classDefnCtor x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ClassDefn"),
        Core.projectionFieldName = (Core.Name "ctor")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the mods field of hydra.scala.syntax.ClassDefn
classDefnMods :: Typed.TypedTerm Syntax.ClassDefn -> Typed.TypedTerm [Syntax.Mod]
classDefnMods x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ClassDefn"),
        Core.projectionFieldName = (Core.Name "mods")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the name field of hydra.scala.syntax.ClassDefn
classDefnName :: Typed.TypedTerm Syntax.ClassDefn -> Typed.TypedTerm Syntax.NameType
classDefnName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ClassDefn"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the template field of hydra.scala.syntax.ClassDefn
classDefnTemplate :: Typed.TypedTerm Syntax.ClassDefn -> Typed.TypedTerm Syntax.Template
classDefnTemplate x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ClassDefn"),
        Core.projectionFieldName = (Core.Name "template")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the tparams field of hydra.scala.syntax.ClassDefn
classDefnTparams :: Typed.TypedTerm Syntax.ClassDefn -> Typed.TypedTerm [Syntax.ParamType]
classDefnTparams x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ClassDefn"),
        Core.projectionFieldName = (Core.Name "tparams")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the ctor field of hydra.scala.syntax.ClassDefn
classDefnWithCtor :: Typed.TypedTerm Syntax.ClassDefn -> Typed.TypedTerm Syntax.PrimaryCtor -> Typed.TypedTerm Syntax.ClassDefn
classDefnWithCtor original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ClassDefn"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ClassDefn"),
              Core.projectionFieldName = (Core.Name "mods")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ClassDefn"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ClassDefn"),
              Core.projectionFieldName = (Core.Name "tparams")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ctor"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "template"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ClassDefn"),
              Core.projectionFieldName = (Core.Name "template")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the mods field of hydra.scala.syntax.ClassDefn
classDefnWithMods :: Typed.TypedTerm Syntax.ClassDefn -> Typed.TypedTerm [Syntax.Mod] -> Typed.TypedTerm Syntax.ClassDefn
classDefnWithMods original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ClassDefn"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ClassDefn"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ClassDefn"),
              Core.projectionFieldName = (Core.Name "tparams")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ctor"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ClassDefn"),
              Core.projectionFieldName = (Core.Name "ctor")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "template"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ClassDefn"),
              Core.projectionFieldName = (Core.Name "template")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the name field of hydra.scala.syntax.ClassDefn
classDefnWithName :: Typed.TypedTerm Syntax.ClassDefn -> Typed.TypedTerm Syntax.NameType -> Typed.TypedTerm Syntax.ClassDefn
classDefnWithName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ClassDefn"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ClassDefn"),
              Core.projectionFieldName = (Core.Name "mods")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ClassDefn"),
              Core.projectionFieldName = (Core.Name "tparams")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ctor"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ClassDefn"),
              Core.projectionFieldName = (Core.Name "ctor")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "template"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ClassDefn"),
              Core.projectionFieldName = (Core.Name "template")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the template field of hydra.scala.syntax.ClassDefn
classDefnWithTemplate :: Typed.TypedTerm Syntax.ClassDefn -> Typed.TypedTerm Syntax.Template -> Typed.TypedTerm Syntax.ClassDefn
classDefnWithTemplate original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ClassDefn"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ClassDefn"),
              Core.projectionFieldName = (Core.Name "mods")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ClassDefn"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ClassDefn"),
              Core.projectionFieldName = (Core.Name "tparams")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ctor"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ClassDefn"),
              Core.projectionFieldName = (Core.Name "ctor")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "template"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the tparams field of hydra.scala.syntax.ClassDefn
classDefnWithTparams :: Typed.TypedTerm Syntax.ClassDefn -> Typed.TypedTerm [Syntax.ParamType] -> Typed.TypedTerm Syntax.ClassDefn
classDefnWithTparams original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ClassDefn"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ClassDefn"),
              Core.projectionFieldName = (Core.Name "mods")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ClassDefn"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "ctor"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ClassDefn"),
              Core.projectionFieldName = (Core.Name "ctor")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "template"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ClassDefn"),
              Core.projectionFieldName = (Core.Name "template")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.scala.syntax.ContextFunctionData
contextFunctionData :: Typed.TypedTerm [Syntax.ParamData] -> Typed.TypedTerm Syntax.Data -> Typed.TypedTerm Syntax.ContextFunctionData
contextFunctionData params body =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ContextFunctionData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Typed.unTypedTerm params)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm body)}]}))
-- | DSL accessor for the body field of hydra.scala.syntax.ContextFunctionData
contextFunctionDataBody :: Typed.TypedTerm Syntax.ContextFunctionData -> Typed.TypedTerm Syntax.Data
contextFunctionDataBody x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ContextFunctionData"),
        Core.projectionFieldName = (Core.Name "body")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the params field of hydra.scala.syntax.ContextFunctionData
contextFunctionDataParams :: Typed.TypedTerm Syntax.ContextFunctionData -> Typed.TypedTerm [Syntax.ParamData]
contextFunctionDataParams x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ContextFunctionData"),
        Core.projectionFieldName = (Core.Name "params")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the body field of hydra.scala.syntax.ContextFunctionData
contextFunctionDataWithBody :: Typed.TypedTerm Syntax.ContextFunctionData -> Typed.TypedTerm Syntax.Data -> Typed.TypedTerm Syntax.ContextFunctionData
contextFunctionDataWithBody original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ContextFunctionData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ContextFunctionData"),
              Core.projectionFieldName = (Core.Name "params")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the params field of hydra.scala.syntax.ContextFunctionData
contextFunctionDataWithParams :: Typed.TypedTerm Syntax.ContextFunctionData -> Typed.TypedTerm [Syntax.ParamData] -> Typed.TypedTerm Syntax.ContextFunctionData
contextFunctionDataWithParams original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ContextFunctionData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ContextFunctionData"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.scala.syntax.ContextFunctionType
contextFunctionType :: Typed.TypedTerm [Syntax.Type] -> Typed.TypedTerm Syntax.Type -> Typed.TypedTerm Syntax.ContextFunctionType
contextFunctionType params res =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ContextFunctionType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Typed.unTypedTerm params)},
        Core.Field {
          Core.fieldName = (Core.Name "res"),
          Core.fieldTerm = (Typed.unTypedTerm res)}]}))
-- | DSL accessor for the params field of hydra.scala.syntax.ContextFunctionType
contextFunctionTypeParams :: Typed.TypedTerm Syntax.ContextFunctionType -> Typed.TypedTerm [Syntax.Type]
contextFunctionTypeParams x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ContextFunctionType"),
        Core.projectionFieldName = (Core.Name "params")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the res field of hydra.scala.syntax.ContextFunctionType
contextFunctionTypeRes :: Typed.TypedTerm Syntax.ContextFunctionType -> Typed.TypedTerm Syntax.Type
contextFunctionTypeRes x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ContextFunctionType"),
        Core.projectionFieldName = (Core.Name "res")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the params field of hydra.scala.syntax.ContextFunctionType
contextFunctionTypeWithParams :: Typed.TypedTerm Syntax.ContextFunctionType -> Typed.TypedTerm [Syntax.Type] -> Typed.TypedTerm Syntax.ContextFunctionType
contextFunctionTypeWithParams original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ContextFunctionType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "res"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ContextFunctionType"),
              Core.projectionFieldName = (Core.Name "res")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the res field of hydra.scala.syntax.ContextFunctionType
contextFunctionTypeWithRes :: Typed.TypedTerm Syntax.ContextFunctionType -> Typed.TypedTerm Syntax.Type -> Typed.TypedTerm Syntax.ContextFunctionType
contextFunctionTypeWithRes original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ContextFunctionType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ContextFunctionType"),
              Core.projectionFieldName = (Core.Name "params")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "res"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL injection for the primary variant of hydra.scala.syntax.Ctor
ctorPrimary :: Typed.TypedTerm Syntax.PrimaryCtor -> Typed.TypedTerm Syntax.Ctor
ctorPrimary x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Ctor"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "primary"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the secondary variant of hydra.scala.syntax.Ctor
ctorSecondary :: Typed.TypedTerm Syntax.SecondaryCtor -> Typed.TypedTerm Syntax.Ctor
ctorSecondary x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Ctor"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "secondary"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the annotate variant of hydra.scala.syntax.Data
dataAnnotate :: Typed.TypedTerm Syntax.AnnotateData -> Typed.TypedTerm Syntax.Data
dataAnnotate x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Data"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "annotate"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the apply variant of hydra.scala.syntax.Data
dataApply :: Typed.TypedTerm Syntax.ApplyData -> Typed.TypedTerm Syntax.Data
dataApply x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Data"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "apply"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the applyType variant of hydra.scala.syntax.Data
dataApplyType :: Typed.TypedTerm Syntax.ApplyTypeData -> Typed.TypedTerm Syntax.Data
dataApplyType x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Data"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "applyType"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the applyUsing variant of hydra.scala.syntax.Data
dataApplyUsing :: Typed.TypedTerm Syntax.ApplyUsingData -> Typed.TypedTerm Syntax.Data
dataApplyUsing x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Data"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "applyUsing"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the ascribe variant of hydra.scala.syntax.Data
dataAscribe :: Typed.TypedTerm Syntax.AscribeData -> Typed.TypedTerm Syntax.Data
dataAscribe x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Data"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "ascribe"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the assign variant of hydra.scala.syntax.Data
dataAssign :: Typed.TypedTerm Syntax.AssignData -> Typed.TypedTerm Syntax.Data
dataAssign x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Data"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "assign"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the block variant of hydra.scala.syntax.Data
dataBlock :: Typed.TypedTerm Syntax.BlockData -> Typed.TypedTerm Syntax.Data
dataBlock x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Data"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "block"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the contextFunction variant of hydra.scala.syntax.Data
dataContextFunction :: Typed.TypedTerm Syntax.ContextFunctionData -> Typed.TypedTerm Syntax.Data
dataContextFunction x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Data"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "contextFunction"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the do variant of hydra.scala.syntax.Data
dataDo :: Typed.TypedTerm Syntax.DoData -> Typed.TypedTerm Syntax.Data
dataDo x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Data"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "do"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the endMarker variant of hydra.scala.syntax.Data
dataEndMarker :: Typed.TypedTerm Syntax.EndMarkerData -> Typed.TypedTerm Syntax.Data
dataEndMarker x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Data"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "endMarker"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the eta variant of hydra.scala.syntax.Data
dataEta :: Typed.TypedTerm Syntax.EtaData -> Typed.TypedTerm Syntax.Data
dataEta x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Data"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "eta"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the for variant of hydra.scala.syntax.Data
dataFor :: Typed.TypedTerm Syntax.ForData -> Typed.TypedTerm Syntax.Data
dataFor x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Data"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "for"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the forYield variant of hydra.scala.syntax.Data
dataForYield :: Typed.TypedTerm Syntax.ForYieldData -> Typed.TypedTerm Syntax.Data
dataForYield x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Data"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "forYield"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the function variant of hydra.scala.syntax.Data
dataFunction :: Typed.TypedTerm Syntax.FunctionData -> Typed.TypedTerm Syntax.Data
dataFunction x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Data"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "function"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the if variant of hydra.scala.syntax.Data
dataIf :: Typed.TypedTerm Syntax.IfData -> Typed.TypedTerm Syntax.Data
dataIf x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Data"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "if"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the interpolate variant of hydra.scala.syntax.Data
dataInterpolate :: Typed.TypedTerm Syntax.InterpolateData -> Typed.TypedTerm Syntax.Data
dataInterpolate x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Data"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "interpolate"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the lit variant of hydra.scala.syntax.Data
dataLit :: Typed.TypedTerm Syntax.Lit -> Typed.TypedTerm Syntax.Data
dataLit x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Data"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "lit"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the match variant of hydra.scala.syntax.Data
dataMatch :: Typed.TypedTerm Syntax.MatchData -> Typed.TypedTerm Syntax.Data
dataMatch x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Data"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "match"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the object variant of hydra.scala.syntax.DataMember
dataMemberObject :: Typed.TypedTerm Syntax.ObjectPkg -> Typed.TypedTerm Syntax.DataMember
dataMemberObject x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.DataMember"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "object"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the pkg variant of hydra.scala.syntax.DataMember
dataMemberPkg :: Typed.TypedTerm Syntax.Pkg -> Typed.TypedTerm Syntax.DataMember
dataMemberPkg x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.DataMember"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "pkg"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the new variant of hydra.scala.syntax.Data
dataNew :: Typed.TypedTerm Syntax.NewData -> Typed.TypedTerm Syntax.Data
dataNew x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Data"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "new"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the newAnonymous variant of hydra.scala.syntax.Data
dataNewAnonymous :: Typed.TypedTerm Syntax.NewAnonymousData -> Typed.TypedTerm Syntax.Data
dataNewAnonymous x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Data"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "newAnonymous"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the param variant of hydra.scala.syntax.Data
dataParam :: Typed.TypedTerm Syntax.ParamData -> Typed.TypedTerm Syntax.Data
dataParam x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Data"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "param"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the partialFunction variant of hydra.scala.syntax.Data
dataPartialFunction :: Typed.TypedTerm Syntax.PartialFunctionData -> Typed.TypedTerm Syntax.Data
dataPartialFunction x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Data"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "partialFunction"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the placeholder variant of hydra.scala.syntax.Data
dataPlaceholder :: Typed.TypedTerm Syntax.Data
dataPlaceholder =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Data"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "placeholder"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the polyFunction variant of hydra.scala.syntax.Data
dataPolyFunction :: Typed.TypedTerm Syntax.PolyFunctionData -> Typed.TypedTerm Syntax.Data
dataPolyFunction x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Data"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "polyFunction"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the ref variant of hydra.scala.syntax.Data
dataRef :: Typed.TypedTerm Syntax.RefData -> Typed.TypedTerm Syntax.Data
dataRef x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Data"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "ref"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the repeated variant of hydra.scala.syntax.Data
dataRepeated :: Typed.TypedTerm Syntax.RepeatedData -> Typed.TypedTerm Syntax.Data
dataRepeated x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Data"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "repeated"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the return variant of hydra.scala.syntax.Data
dataReturn :: Typed.TypedTerm Syntax.ReturnData -> Typed.TypedTerm Syntax.Data
dataReturn x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Data"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "return"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the throw variant of hydra.scala.syntax.Data
dataThrow :: Typed.TypedTerm Syntax.ThrowData -> Typed.TypedTerm Syntax.Data
dataThrow x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Data"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "throw"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the try variant of hydra.scala.syntax.Data
dataTry :: Typed.TypedTerm Syntax.TryData -> Typed.TypedTerm Syntax.Data
dataTry x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Data"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "try"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the tryWithHandler variant of hydra.scala.syntax.Data
dataTryWithHandler :: Typed.TypedTerm Syntax.TryWithHandlerData -> Typed.TypedTerm Syntax.Data
dataTryWithHandler x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Data"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "tryWithHandler"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the tuple variant of hydra.scala.syntax.Data
dataTuple :: Typed.TypedTerm Syntax.TupleData -> Typed.TypedTerm Syntax.Data
dataTuple x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Data"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "tuple"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the while variant of hydra.scala.syntax.Data
dataWhile :: Typed.TypedTerm Syntax.WhileData -> Typed.TypedTerm Syntax.Data
dataWhile x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Data"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "while"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the def variant of hydra.scala.syntax.Decl
declDef :: Typed.TypedTerm Syntax.DefDecl -> Typed.TypedTerm Syntax.Decl
declDef x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Decl"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "def"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the given variant of hydra.scala.syntax.Decl
declGiven :: Typed.TypedTerm Syntax.GivenDecl -> Typed.TypedTerm Syntax.Decl
declGiven x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Decl"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "given"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the type variant of hydra.scala.syntax.Decl
declType :: Typed.TypedTerm Syntax.TypeDecl -> Typed.TypedTerm Syntax.Decl
declType x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Decl"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "type"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the val variant of hydra.scala.syntax.Decl
declVal :: Typed.TypedTerm Syntax.ValDecl -> Typed.TypedTerm Syntax.Decl
declVal x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Decl"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "val"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the var variant of hydra.scala.syntax.Decl
declVar :: Typed.TypedTerm Syntax.VarDecl -> Typed.TypedTerm Syntax.Decl
declVar x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Decl"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "var"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for hydra.scala.syntax.DefDecl
defDecl :: Typed.TypedTerm [Syntax.Mod] -> Typed.TypedTerm Syntax.NameData -> Typed.TypedTerm [Syntax.ParamType] -> Typed.TypedTerm [[Syntax.ParamData]] -> Typed.TypedTerm Syntax.Type -> Typed.TypedTerm Syntax.DefDecl
defDecl mods name tparams paramss decltpe =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.DefDecl"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Typed.unTypedTerm mods)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Typed.unTypedTerm tparams)},
        Core.Field {
          Core.fieldName = (Core.Name "paramss"),
          Core.fieldTerm = (Typed.unTypedTerm paramss)},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Typed.unTypedTerm decltpe)}]}))
-- | DSL accessor for the decltpe field of hydra.scala.syntax.DefDecl
defDeclDecltpe :: Typed.TypedTerm Syntax.DefDecl -> Typed.TypedTerm Syntax.Type
defDeclDecltpe x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DefDecl"),
        Core.projectionFieldName = (Core.Name "decltpe")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the mods field of hydra.scala.syntax.DefDecl
defDeclMods :: Typed.TypedTerm Syntax.DefDecl -> Typed.TypedTerm [Syntax.Mod]
defDeclMods x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DefDecl"),
        Core.projectionFieldName = (Core.Name "mods")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the name field of hydra.scala.syntax.DefDecl
defDeclName :: Typed.TypedTerm Syntax.DefDecl -> Typed.TypedTerm Syntax.NameData
defDeclName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DefDecl"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the paramss field of hydra.scala.syntax.DefDecl
defDeclParamss :: Typed.TypedTerm Syntax.DefDecl -> Typed.TypedTerm [[Syntax.ParamData]]
defDeclParamss x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DefDecl"),
        Core.projectionFieldName = (Core.Name "paramss")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the tparams field of hydra.scala.syntax.DefDecl
defDeclTparams :: Typed.TypedTerm Syntax.DefDecl -> Typed.TypedTerm [Syntax.ParamType]
defDeclTparams x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DefDecl"),
        Core.projectionFieldName = (Core.Name "tparams")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the decltpe field of hydra.scala.syntax.DefDecl
defDeclWithDecltpe :: Typed.TypedTerm Syntax.DefDecl -> Typed.TypedTerm Syntax.Type -> Typed.TypedTerm Syntax.DefDecl
defDeclWithDecltpe original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.DefDecl"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DefDecl"),
              Core.projectionFieldName = (Core.Name "mods")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DefDecl"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DefDecl"),
              Core.projectionFieldName = (Core.Name "tparams")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "paramss"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DefDecl"),
              Core.projectionFieldName = (Core.Name "paramss")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the mods field of hydra.scala.syntax.DefDecl
defDeclWithMods :: Typed.TypedTerm Syntax.DefDecl -> Typed.TypedTerm [Syntax.Mod] -> Typed.TypedTerm Syntax.DefDecl
defDeclWithMods original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.DefDecl"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DefDecl"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DefDecl"),
              Core.projectionFieldName = (Core.Name "tparams")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "paramss"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DefDecl"),
              Core.projectionFieldName = (Core.Name "paramss")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DefDecl"),
              Core.projectionFieldName = (Core.Name "decltpe")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the name field of hydra.scala.syntax.DefDecl
defDeclWithName :: Typed.TypedTerm Syntax.DefDecl -> Typed.TypedTerm Syntax.NameData -> Typed.TypedTerm Syntax.DefDecl
defDeclWithName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.DefDecl"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DefDecl"),
              Core.projectionFieldName = (Core.Name "mods")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DefDecl"),
              Core.projectionFieldName = (Core.Name "tparams")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "paramss"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DefDecl"),
              Core.projectionFieldName = (Core.Name "paramss")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DefDecl"),
              Core.projectionFieldName = (Core.Name "decltpe")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the paramss field of hydra.scala.syntax.DefDecl
defDeclWithParamss :: Typed.TypedTerm Syntax.DefDecl -> Typed.TypedTerm [[Syntax.ParamData]] -> Typed.TypedTerm Syntax.DefDecl
defDeclWithParamss original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.DefDecl"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DefDecl"),
              Core.projectionFieldName = (Core.Name "mods")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DefDecl"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DefDecl"),
              Core.projectionFieldName = (Core.Name "tparams")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "paramss"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DefDecl"),
              Core.projectionFieldName = (Core.Name "decltpe")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the tparams field of hydra.scala.syntax.DefDecl
defDeclWithTparams :: Typed.TypedTerm Syntax.DefDecl -> Typed.TypedTerm [Syntax.ParamType] -> Typed.TypedTerm Syntax.DefDecl
defDeclWithTparams original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.DefDecl"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DefDecl"),
              Core.projectionFieldName = (Core.Name "mods")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DefDecl"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "paramss"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DefDecl"),
              Core.projectionFieldName = (Core.Name "paramss")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DefDecl"),
              Core.projectionFieldName = (Core.Name "decltpe")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.scala.syntax.DefDefn
defDefn :: Typed.TypedTerm [Syntax.Mod] -> Typed.TypedTerm Syntax.NameData -> Typed.TypedTerm [Syntax.ParamType] -> Typed.TypedTerm [[Syntax.ParamData]] -> Typed.TypedTerm (Maybe Syntax.Type) -> Typed.TypedTerm Syntax.Data -> Typed.TypedTerm Syntax.DefDefn
defDefn mods name tparams paramss decltpe body =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.DefDefn"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Typed.unTypedTerm mods)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Typed.unTypedTerm tparams)},
        Core.Field {
          Core.fieldName = (Core.Name "paramss"),
          Core.fieldTerm = (Typed.unTypedTerm paramss)},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Typed.unTypedTerm decltpe)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm body)}]}))
-- | DSL accessor for the body field of hydra.scala.syntax.DefDefn
defDefnBody :: Typed.TypedTerm Syntax.DefDefn -> Typed.TypedTerm Syntax.Data
defDefnBody x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DefDefn"),
        Core.projectionFieldName = (Core.Name "body")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the decltpe field of hydra.scala.syntax.DefDefn
defDefnDecltpe :: Typed.TypedTerm Syntax.DefDefn -> Typed.TypedTerm (Maybe Syntax.Type)
defDefnDecltpe x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DefDefn"),
        Core.projectionFieldName = (Core.Name "decltpe")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the mods field of hydra.scala.syntax.DefDefn
defDefnMods :: Typed.TypedTerm Syntax.DefDefn -> Typed.TypedTerm [Syntax.Mod]
defDefnMods x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DefDefn"),
        Core.projectionFieldName = (Core.Name "mods")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the name field of hydra.scala.syntax.DefDefn
defDefnName :: Typed.TypedTerm Syntax.DefDefn -> Typed.TypedTerm Syntax.NameData
defDefnName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DefDefn"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the paramss field of hydra.scala.syntax.DefDefn
defDefnParamss :: Typed.TypedTerm Syntax.DefDefn -> Typed.TypedTerm [[Syntax.ParamData]]
defDefnParamss x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DefDefn"),
        Core.projectionFieldName = (Core.Name "paramss")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the tparams field of hydra.scala.syntax.DefDefn
defDefnTparams :: Typed.TypedTerm Syntax.DefDefn -> Typed.TypedTerm [Syntax.ParamType]
defDefnTparams x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DefDefn"),
        Core.projectionFieldName = (Core.Name "tparams")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the body field of hydra.scala.syntax.DefDefn
defDefnWithBody :: Typed.TypedTerm Syntax.DefDefn -> Typed.TypedTerm Syntax.Data -> Typed.TypedTerm Syntax.DefDefn
defDefnWithBody original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.DefDefn"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DefDefn"),
              Core.projectionFieldName = (Core.Name "mods")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DefDefn"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DefDefn"),
              Core.projectionFieldName = (Core.Name "tparams")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "paramss"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DefDefn"),
              Core.projectionFieldName = (Core.Name "paramss")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DefDefn"),
              Core.projectionFieldName = (Core.Name "decltpe")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the decltpe field of hydra.scala.syntax.DefDefn
defDefnWithDecltpe :: Typed.TypedTerm Syntax.DefDefn -> Typed.TypedTerm (Maybe Syntax.Type) -> Typed.TypedTerm Syntax.DefDefn
defDefnWithDecltpe original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.DefDefn"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DefDefn"),
              Core.projectionFieldName = (Core.Name "mods")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DefDefn"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DefDefn"),
              Core.projectionFieldName = (Core.Name "tparams")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "paramss"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DefDefn"),
              Core.projectionFieldName = (Core.Name "paramss")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DefDefn"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the mods field of hydra.scala.syntax.DefDefn
defDefnWithMods :: Typed.TypedTerm Syntax.DefDefn -> Typed.TypedTerm [Syntax.Mod] -> Typed.TypedTerm Syntax.DefDefn
defDefnWithMods original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.DefDefn"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DefDefn"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DefDefn"),
              Core.projectionFieldName = (Core.Name "tparams")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "paramss"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DefDefn"),
              Core.projectionFieldName = (Core.Name "paramss")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DefDefn"),
              Core.projectionFieldName = (Core.Name "decltpe")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DefDefn"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the name field of hydra.scala.syntax.DefDefn
defDefnWithName :: Typed.TypedTerm Syntax.DefDefn -> Typed.TypedTerm Syntax.NameData -> Typed.TypedTerm Syntax.DefDefn
defDefnWithName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.DefDefn"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DefDefn"),
              Core.projectionFieldName = (Core.Name "mods")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DefDefn"),
              Core.projectionFieldName = (Core.Name "tparams")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "paramss"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DefDefn"),
              Core.projectionFieldName = (Core.Name "paramss")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DefDefn"),
              Core.projectionFieldName = (Core.Name "decltpe")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DefDefn"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the paramss field of hydra.scala.syntax.DefDefn
defDefnWithParamss :: Typed.TypedTerm Syntax.DefDefn -> Typed.TypedTerm [[Syntax.ParamData]] -> Typed.TypedTerm Syntax.DefDefn
defDefnWithParamss original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.DefDefn"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DefDefn"),
              Core.projectionFieldName = (Core.Name "mods")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DefDefn"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DefDefn"),
              Core.projectionFieldName = (Core.Name "tparams")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "paramss"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DefDefn"),
              Core.projectionFieldName = (Core.Name "decltpe")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DefDefn"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the tparams field of hydra.scala.syntax.DefDefn
defDefnWithTparams :: Typed.TypedTerm Syntax.DefDefn -> Typed.TypedTerm [Syntax.ParamType] -> Typed.TypedTerm Syntax.DefDefn
defDefnWithTparams original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.DefDefn"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DefDefn"),
              Core.projectionFieldName = (Core.Name "mods")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DefDefn"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "paramss"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DefDefn"),
              Core.projectionFieldName = (Core.Name "paramss")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DefDefn"),
              Core.projectionFieldName = (Core.Name "decltpe")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DefDefn"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL injection for the class variant of hydra.scala.syntax.Defn
defnClass :: Typed.TypedTerm Syntax.ClassDefn -> Typed.TypedTerm Syntax.Defn
defnClass x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Defn"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "class"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the def variant of hydra.scala.syntax.Defn
defnDef :: Typed.TypedTerm Syntax.DefDefn -> Typed.TypedTerm Syntax.Defn
defnDef x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Defn"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "def"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the enum variant of hydra.scala.syntax.Defn
defnEnum :: Typed.TypedTerm Syntax.EnumDefn -> Typed.TypedTerm Syntax.Defn
defnEnum x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Defn"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "enum"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the enumCase variant of hydra.scala.syntax.Defn
defnEnumCase :: Typed.TypedTerm Syntax.EnumCaseDefn -> Typed.TypedTerm Syntax.Defn
defnEnumCase x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Defn"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "enumCase"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the extensionGroup variant of hydra.scala.syntax.Defn
defnExtensionGroup :: Typed.TypedTerm Syntax.ExtensionGroupDefn -> Typed.TypedTerm Syntax.Defn
defnExtensionGroup x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Defn"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "extensionGroup"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the given variant of hydra.scala.syntax.Defn
defnGiven :: Typed.TypedTerm Syntax.GivenDefn -> Typed.TypedTerm Syntax.Defn
defnGiven x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Defn"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "given"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the givenAlias variant of hydra.scala.syntax.Defn
defnGivenAlias :: Typed.TypedTerm Syntax.GivenAliasDefn -> Typed.TypedTerm Syntax.Defn
defnGivenAlias x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Defn"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "givenAlias"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the object variant of hydra.scala.syntax.Defn
defnObject :: Typed.TypedTerm Syntax.ObjectDefn -> Typed.TypedTerm Syntax.Defn
defnObject x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Defn"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "object"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the repeatedEnumCase variant of hydra.scala.syntax.Defn
defnRepeatedEnumCase :: Typed.TypedTerm Syntax.RepeatedEnumCaseDefn -> Typed.TypedTerm Syntax.Defn
defnRepeatedEnumCase x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Defn"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "repeatedEnumCase"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the trait variant of hydra.scala.syntax.Defn
defnTrait :: Typed.TypedTerm Syntax.TraitDefn -> Typed.TypedTerm Syntax.Defn
defnTrait x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Defn"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "trait"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the type variant of hydra.scala.syntax.Defn
defnType :: Typed.TypedTerm Syntax.TypeDefn -> Typed.TypedTerm Syntax.Defn
defnType x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Defn"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "type"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the val variant of hydra.scala.syntax.Defn
defnVal :: Typed.TypedTerm Syntax.ValDefn -> Typed.TypedTerm Syntax.Defn
defnVal x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Defn"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "val"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the var variant of hydra.scala.syntax.Defn
defnVar :: Typed.TypedTerm Syntax.VarDefn -> Typed.TypedTerm Syntax.Defn
defnVar x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Defn"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "var"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for hydra.scala.syntax.DoData
doData :: Typed.TypedTerm Syntax.Data -> Typed.TypedTerm Syntax.Data -> Typed.TypedTerm Syntax.DoData
doData body expr =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.DoData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm body)},
        Core.Field {
          Core.fieldName = (Core.Name "expr"),
          Core.fieldTerm = (Typed.unTypedTerm expr)}]}))
-- | DSL accessor for the body field of hydra.scala.syntax.DoData
doDataBody :: Typed.TypedTerm Syntax.DoData -> Typed.TypedTerm Syntax.Data
doDataBody x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DoData"),
        Core.projectionFieldName = (Core.Name "body")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the expr field of hydra.scala.syntax.DoData
doDataExpr :: Typed.TypedTerm Syntax.DoData -> Typed.TypedTerm Syntax.Data
doDataExpr x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DoData"),
        Core.projectionFieldName = (Core.Name "expr")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the body field of hydra.scala.syntax.DoData
doDataWithBody :: Typed.TypedTerm Syntax.DoData -> Typed.TypedTerm Syntax.Data -> Typed.TypedTerm Syntax.DoData
doDataWithBody original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.DoData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "expr"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DoData"),
              Core.projectionFieldName = (Core.Name "expr")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the expr field of hydra.scala.syntax.DoData
doDataWithExpr :: Typed.TypedTerm Syntax.DoData -> Typed.TypedTerm Syntax.Data -> Typed.TypedTerm Syntax.DoData
doDataWithExpr original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.DoData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.DoData"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "expr"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.scala.syntax.EndMarkerData
endMarkerData :: Typed.TypedTerm Syntax.NameData -> Typed.TypedTerm Syntax.EndMarkerData
endMarkerData name =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.EndMarkerData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm name)}]}))
-- | DSL accessor for the name field of hydra.scala.syntax.EndMarkerData
endMarkerDataName :: Typed.TypedTerm Syntax.EndMarkerData -> Typed.TypedTerm Syntax.NameData
endMarkerDataName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.EndMarkerData"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the name field of hydra.scala.syntax.EndMarkerData
endMarkerDataWithName :: Typed.TypedTerm Syntax.EndMarkerData -> Typed.TypedTerm Syntax.NameData -> Typed.TypedTerm Syntax.EndMarkerData
endMarkerDataWithName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.EndMarkerData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.scala.syntax.EnumCaseDefn
enumCaseDefn :: Typed.TypedTerm [Syntax.Mod] -> Typed.TypedTerm Syntax.NameData -> Typed.TypedTerm [Syntax.ParamType] -> Typed.TypedTerm Syntax.PrimaryCtor -> Typed.TypedTerm [Syntax.Init] -> Typed.TypedTerm Syntax.EnumCaseDefn
enumCaseDefn mods name tparams ctor inits =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.EnumCaseDefn"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Typed.unTypedTerm mods)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Typed.unTypedTerm tparams)},
        Core.Field {
          Core.fieldName = (Core.Name "ctor"),
          Core.fieldTerm = (Typed.unTypedTerm ctor)},
        Core.Field {
          Core.fieldName = (Core.Name "inits"),
          Core.fieldTerm = (Typed.unTypedTerm inits)}]}))
-- | DSL accessor for the ctor field of hydra.scala.syntax.EnumCaseDefn
enumCaseDefnCtor :: Typed.TypedTerm Syntax.EnumCaseDefn -> Typed.TypedTerm Syntax.PrimaryCtor
enumCaseDefnCtor x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.EnumCaseDefn"),
        Core.projectionFieldName = (Core.Name "ctor")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the inits field of hydra.scala.syntax.EnumCaseDefn
enumCaseDefnInits :: Typed.TypedTerm Syntax.EnumCaseDefn -> Typed.TypedTerm [Syntax.Init]
enumCaseDefnInits x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.EnumCaseDefn"),
        Core.projectionFieldName = (Core.Name "inits")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the mods field of hydra.scala.syntax.EnumCaseDefn
enumCaseDefnMods :: Typed.TypedTerm Syntax.EnumCaseDefn -> Typed.TypedTerm [Syntax.Mod]
enumCaseDefnMods x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.EnumCaseDefn"),
        Core.projectionFieldName = (Core.Name "mods")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the name field of hydra.scala.syntax.EnumCaseDefn
enumCaseDefnName :: Typed.TypedTerm Syntax.EnumCaseDefn -> Typed.TypedTerm Syntax.NameData
enumCaseDefnName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.EnumCaseDefn"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the tparams field of hydra.scala.syntax.EnumCaseDefn
enumCaseDefnTparams :: Typed.TypedTerm Syntax.EnumCaseDefn -> Typed.TypedTerm [Syntax.ParamType]
enumCaseDefnTparams x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.EnumCaseDefn"),
        Core.projectionFieldName = (Core.Name "tparams")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the ctor field of hydra.scala.syntax.EnumCaseDefn
enumCaseDefnWithCtor :: Typed.TypedTerm Syntax.EnumCaseDefn -> Typed.TypedTerm Syntax.PrimaryCtor -> Typed.TypedTerm Syntax.EnumCaseDefn
enumCaseDefnWithCtor original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.EnumCaseDefn"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.EnumCaseDefn"),
              Core.projectionFieldName = (Core.Name "mods")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.EnumCaseDefn"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.EnumCaseDefn"),
              Core.projectionFieldName = (Core.Name "tparams")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ctor"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "inits"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.EnumCaseDefn"),
              Core.projectionFieldName = (Core.Name "inits")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the inits field of hydra.scala.syntax.EnumCaseDefn
enumCaseDefnWithInits :: Typed.TypedTerm Syntax.EnumCaseDefn -> Typed.TypedTerm [Syntax.Init] -> Typed.TypedTerm Syntax.EnumCaseDefn
enumCaseDefnWithInits original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.EnumCaseDefn"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.EnumCaseDefn"),
              Core.projectionFieldName = (Core.Name "mods")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.EnumCaseDefn"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.EnumCaseDefn"),
              Core.projectionFieldName = (Core.Name "tparams")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ctor"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.EnumCaseDefn"),
              Core.projectionFieldName = (Core.Name "ctor")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inits"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the mods field of hydra.scala.syntax.EnumCaseDefn
enumCaseDefnWithMods :: Typed.TypedTerm Syntax.EnumCaseDefn -> Typed.TypedTerm [Syntax.Mod] -> Typed.TypedTerm Syntax.EnumCaseDefn
enumCaseDefnWithMods original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.EnumCaseDefn"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.EnumCaseDefn"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.EnumCaseDefn"),
              Core.projectionFieldName = (Core.Name "tparams")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ctor"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.EnumCaseDefn"),
              Core.projectionFieldName = (Core.Name "ctor")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inits"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.EnumCaseDefn"),
              Core.projectionFieldName = (Core.Name "inits")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the name field of hydra.scala.syntax.EnumCaseDefn
enumCaseDefnWithName :: Typed.TypedTerm Syntax.EnumCaseDefn -> Typed.TypedTerm Syntax.NameData -> Typed.TypedTerm Syntax.EnumCaseDefn
enumCaseDefnWithName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.EnumCaseDefn"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.EnumCaseDefn"),
              Core.projectionFieldName = (Core.Name "mods")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.EnumCaseDefn"),
              Core.projectionFieldName = (Core.Name "tparams")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ctor"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.EnumCaseDefn"),
              Core.projectionFieldName = (Core.Name "ctor")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inits"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.EnumCaseDefn"),
              Core.projectionFieldName = (Core.Name "inits")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the tparams field of hydra.scala.syntax.EnumCaseDefn
enumCaseDefnWithTparams :: Typed.TypedTerm Syntax.EnumCaseDefn -> Typed.TypedTerm [Syntax.ParamType] -> Typed.TypedTerm Syntax.EnumCaseDefn
enumCaseDefnWithTparams original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.EnumCaseDefn"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.EnumCaseDefn"),
              Core.projectionFieldName = (Core.Name "mods")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.EnumCaseDefn"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "ctor"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.EnumCaseDefn"),
              Core.projectionFieldName = (Core.Name "ctor")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inits"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.EnumCaseDefn"),
              Core.projectionFieldName = (Core.Name "inits")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.scala.syntax.EnumDefn
enumDefn :: Typed.TypedTerm [Syntax.Mod] -> Typed.TypedTerm Syntax.NameType -> Typed.TypedTerm [Syntax.ParamType] -> Typed.TypedTerm Syntax.PrimaryCtor -> Typed.TypedTerm Syntax.Template -> Typed.TypedTerm Syntax.EnumDefn
enumDefn mods name tparams ctor template =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.EnumDefn"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Typed.unTypedTerm mods)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Typed.unTypedTerm tparams)},
        Core.Field {
          Core.fieldName = (Core.Name "ctor"),
          Core.fieldTerm = (Typed.unTypedTerm ctor)},
        Core.Field {
          Core.fieldName = (Core.Name "template"),
          Core.fieldTerm = (Typed.unTypedTerm template)}]}))
-- | DSL accessor for the ctor field of hydra.scala.syntax.EnumDefn
enumDefnCtor :: Typed.TypedTerm Syntax.EnumDefn -> Typed.TypedTerm Syntax.PrimaryCtor
enumDefnCtor x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.EnumDefn"),
        Core.projectionFieldName = (Core.Name "ctor")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the mods field of hydra.scala.syntax.EnumDefn
enumDefnMods :: Typed.TypedTerm Syntax.EnumDefn -> Typed.TypedTerm [Syntax.Mod]
enumDefnMods x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.EnumDefn"),
        Core.projectionFieldName = (Core.Name "mods")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the name field of hydra.scala.syntax.EnumDefn
enumDefnName :: Typed.TypedTerm Syntax.EnumDefn -> Typed.TypedTerm Syntax.NameType
enumDefnName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.EnumDefn"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the template field of hydra.scala.syntax.EnumDefn
enumDefnTemplate :: Typed.TypedTerm Syntax.EnumDefn -> Typed.TypedTerm Syntax.Template
enumDefnTemplate x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.EnumDefn"),
        Core.projectionFieldName = (Core.Name "template")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the tparams field of hydra.scala.syntax.EnumDefn
enumDefnTparams :: Typed.TypedTerm Syntax.EnumDefn -> Typed.TypedTerm [Syntax.ParamType]
enumDefnTparams x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.EnumDefn"),
        Core.projectionFieldName = (Core.Name "tparams")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the ctor field of hydra.scala.syntax.EnumDefn
enumDefnWithCtor :: Typed.TypedTerm Syntax.EnumDefn -> Typed.TypedTerm Syntax.PrimaryCtor -> Typed.TypedTerm Syntax.EnumDefn
enumDefnWithCtor original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.EnumDefn"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.EnumDefn"),
              Core.projectionFieldName = (Core.Name "mods")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.EnumDefn"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.EnumDefn"),
              Core.projectionFieldName = (Core.Name "tparams")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ctor"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "template"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.EnumDefn"),
              Core.projectionFieldName = (Core.Name "template")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the mods field of hydra.scala.syntax.EnumDefn
enumDefnWithMods :: Typed.TypedTerm Syntax.EnumDefn -> Typed.TypedTerm [Syntax.Mod] -> Typed.TypedTerm Syntax.EnumDefn
enumDefnWithMods original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.EnumDefn"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.EnumDefn"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.EnumDefn"),
              Core.projectionFieldName = (Core.Name "tparams")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ctor"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.EnumDefn"),
              Core.projectionFieldName = (Core.Name "ctor")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "template"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.EnumDefn"),
              Core.projectionFieldName = (Core.Name "template")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the name field of hydra.scala.syntax.EnumDefn
enumDefnWithName :: Typed.TypedTerm Syntax.EnumDefn -> Typed.TypedTerm Syntax.NameType -> Typed.TypedTerm Syntax.EnumDefn
enumDefnWithName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.EnumDefn"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.EnumDefn"),
              Core.projectionFieldName = (Core.Name "mods")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.EnumDefn"),
              Core.projectionFieldName = (Core.Name "tparams")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ctor"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.EnumDefn"),
              Core.projectionFieldName = (Core.Name "ctor")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "template"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.EnumDefn"),
              Core.projectionFieldName = (Core.Name "template")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the template field of hydra.scala.syntax.EnumDefn
enumDefnWithTemplate :: Typed.TypedTerm Syntax.EnumDefn -> Typed.TypedTerm Syntax.Template -> Typed.TypedTerm Syntax.EnumDefn
enumDefnWithTemplate original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.EnumDefn"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.EnumDefn"),
              Core.projectionFieldName = (Core.Name "mods")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.EnumDefn"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.EnumDefn"),
              Core.projectionFieldName = (Core.Name "tparams")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ctor"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.EnumDefn"),
              Core.projectionFieldName = (Core.Name "ctor")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "template"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the tparams field of hydra.scala.syntax.EnumDefn
enumDefnWithTparams :: Typed.TypedTerm Syntax.EnumDefn -> Typed.TypedTerm [Syntax.ParamType] -> Typed.TypedTerm Syntax.EnumDefn
enumDefnWithTparams original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.EnumDefn"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.EnumDefn"),
              Core.projectionFieldName = (Core.Name "mods")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.EnumDefn"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "ctor"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.EnumDefn"),
              Core.projectionFieldName = (Core.Name "ctor")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "template"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.EnumDefn"),
              Core.projectionFieldName = (Core.Name "template")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL injection for the caseGenerator variant of hydra.scala.syntax.Enumerator
enumeratorCaseGenerator :: Typed.TypedTerm Syntax.CaseGeneratorEnumerator -> Typed.TypedTerm Syntax.Enumerator
enumeratorCaseGenerator x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Enumerator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "caseGenerator"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the generator variant of hydra.scala.syntax.Enumerator
enumeratorGenerator :: Typed.TypedTerm Syntax.GeneratorEnumerator -> Typed.TypedTerm Syntax.Enumerator
enumeratorGenerator x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Enumerator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "generator"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the guard variant of hydra.scala.syntax.Enumerator
enumeratorGuard :: Typed.TypedTerm Syntax.GuardEnumerator -> Typed.TypedTerm Syntax.Enumerator
enumeratorGuard x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Enumerator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "guard"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the val variant of hydra.scala.syntax.Enumerator
enumeratorVal :: Typed.TypedTerm Syntax.ValEnumerator -> Typed.TypedTerm Syntax.Enumerator
enumeratorVal x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Enumerator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "val"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for hydra.scala.syntax.EtaData
etaData :: Typed.TypedTerm Syntax.Data -> Typed.TypedTerm Syntax.EtaData
etaData expr =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.EtaData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expr"),
          Core.fieldTerm = (Typed.unTypedTerm expr)}]}))
-- | DSL accessor for the expr field of hydra.scala.syntax.EtaData
etaDataExpr :: Typed.TypedTerm Syntax.EtaData -> Typed.TypedTerm Syntax.Data
etaDataExpr x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.EtaData"),
        Core.projectionFieldName = (Core.Name "expr")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the expr field of hydra.scala.syntax.EtaData
etaDataWithExpr :: Typed.TypedTerm Syntax.EtaData -> Typed.TypedTerm Syntax.Data -> Typed.TypedTerm Syntax.EtaData
etaDataWithExpr original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.EtaData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expr"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.scala.syntax.ExistentialType
existentialType :: Typed.TypedTerm Syntax.Type -> Typed.TypedTerm [Syntax.Stat] -> Typed.TypedTerm Syntax.ExistentialType
existentialType tpe stats =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ExistentialType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "tpe"),
          Core.fieldTerm = (Typed.unTypedTerm tpe)},
        Core.Field {
          Core.fieldName = (Core.Name "stats"),
          Core.fieldTerm = (Typed.unTypedTerm stats)}]}))
-- | DSL accessor for the stats field of hydra.scala.syntax.ExistentialType
existentialTypeStats :: Typed.TypedTerm Syntax.ExistentialType -> Typed.TypedTerm [Syntax.Stat]
existentialTypeStats x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ExistentialType"),
        Core.projectionFieldName = (Core.Name "stats")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the tpe field of hydra.scala.syntax.ExistentialType
existentialTypeTpe :: Typed.TypedTerm Syntax.ExistentialType -> Typed.TypedTerm Syntax.Type
existentialTypeTpe x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ExistentialType"),
        Core.projectionFieldName = (Core.Name "tpe")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the stats field of hydra.scala.syntax.ExistentialType
existentialTypeWithStats :: Typed.TypedTerm Syntax.ExistentialType -> Typed.TypedTerm [Syntax.Stat] -> Typed.TypedTerm Syntax.ExistentialType
existentialTypeWithStats original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ExistentialType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "tpe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ExistentialType"),
              Core.projectionFieldName = (Core.Name "tpe")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "stats"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the tpe field of hydra.scala.syntax.ExistentialType
existentialTypeWithTpe :: Typed.TypedTerm Syntax.ExistentialType -> Typed.TypedTerm Syntax.Type -> Typed.TypedTerm Syntax.ExistentialType
existentialTypeWithTpe original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ExistentialType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "tpe"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "stats"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ExistentialType"),
              Core.projectionFieldName = (Core.Name "stats")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.scala.syntax.Export
export :: Typed.TypedTerm [Syntax.Importer] -> Typed.TypedTerm Syntax.Export
export importers =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.Export"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "importers"),
          Core.fieldTerm = (Typed.unTypedTerm importers)}]}))
-- | DSL accessor for the importers field of hydra.scala.syntax.Export
exportImporters :: Typed.TypedTerm Syntax.Export -> Typed.TypedTerm [Syntax.Importer]
exportImporters x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.Export"),
        Core.projectionFieldName = (Core.Name "importers")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the importers field of hydra.scala.syntax.Export
exportWithImporters :: Typed.TypedTerm Syntax.Export -> Typed.TypedTerm [Syntax.Importer] -> Typed.TypedTerm Syntax.Export
exportWithImporters original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.Export"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "importers"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.scala.syntax.ExtensionGroupDefn
extensionGroupDefn :: Typed.TypedTerm [Syntax.ParamType] -> Typed.TypedTerm [[Syntax.ParamData]] -> Typed.TypedTerm Syntax.Stat -> Typed.TypedTerm Syntax.ExtensionGroupDefn
extensionGroupDefn tparams parmss body =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ExtensionGroupDefn"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Typed.unTypedTerm tparams)},
        Core.Field {
          Core.fieldName = (Core.Name "parmss"),
          Core.fieldTerm = (Typed.unTypedTerm parmss)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm body)}]}))
-- | DSL accessor for the body field of hydra.scala.syntax.ExtensionGroupDefn
extensionGroupDefnBody :: Typed.TypedTerm Syntax.ExtensionGroupDefn -> Typed.TypedTerm Syntax.Stat
extensionGroupDefnBody x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ExtensionGroupDefn"),
        Core.projectionFieldName = (Core.Name "body")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the parmss field of hydra.scala.syntax.ExtensionGroupDefn
extensionGroupDefnParmss :: Typed.TypedTerm Syntax.ExtensionGroupDefn -> Typed.TypedTerm [[Syntax.ParamData]]
extensionGroupDefnParmss x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ExtensionGroupDefn"),
        Core.projectionFieldName = (Core.Name "parmss")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the tparams field of hydra.scala.syntax.ExtensionGroupDefn
extensionGroupDefnTparams :: Typed.TypedTerm Syntax.ExtensionGroupDefn -> Typed.TypedTerm [Syntax.ParamType]
extensionGroupDefnTparams x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ExtensionGroupDefn"),
        Core.projectionFieldName = (Core.Name "tparams")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the body field of hydra.scala.syntax.ExtensionGroupDefn
extensionGroupDefnWithBody :: Typed.TypedTerm Syntax.ExtensionGroupDefn -> Typed.TypedTerm Syntax.Stat -> Typed.TypedTerm Syntax.ExtensionGroupDefn
extensionGroupDefnWithBody original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ExtensionGroupDefn"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ExtensionGroupDefn"),
              Core.projectionFieldName = (Core.Name "tparams")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "parmss"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ExtensionGroupDefn"),
              Core.projectionFieldName = (Core.Name "parmss")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the parmss field of hydra.scala.syntax.ExtensionGroupDefn
extensionGroupDefnWithParmss :: Typed.TypedTerm Syntax.ExtensionGroupDefn -> Typed.TypedTerm [[Syntax.ParamData]] -> Typed.TypedTerm Syntax.ExtensionGroupDefn
extensionGroupDefnWithParmss original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ExtensionGroupDefn"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ExtensionGroupDefn"),
              Core.projectionFieldName = (Core.Name "tparams")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "parmss"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ExtensionGroupDefn"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the tparams field of hydra.scala.syntax.ExtensionGroupDefn
extensionGroupDefnWithTparams :: Typed.TypedTerm Syntax.ExtensionGroupDefn -> Typed.TypedTerm [Syntax.ParamType] -> Typed.TypedTerm Syntax.ExtensionGroupDefn
extensionGroupDefnWithTparams original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ExtensionGroupDefn"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "parmss"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ExtensionGroupDefn"),
              Core.projectionFieldName = (Core.Name "parmss")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ExtensionGroupDefn"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.scala.syntax.ExtractInfixPat
extractInfixPat :: Typed.TypedTerm Syntax.Pat -> Typed.TypedTerm Syntax.NameData -> Typed.TypedTerm [Syntax.Pat] -> Typed.TypedTerm Syntax.ExtractInfixPat
extractInfixPat lhs op rhs =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ExtractInfixPat"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Typed.unTypedTerm lhs)},
        Core.Field {
          Core.fieldName = (Core.Name "op"),
          Core.fieldTerm = (Typed.unTypedTerm op)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Typed.unTypedTerm rhs)}]}))
-- | DSL accessor for the lhs field of hydra.scala.syntax.ExtractInfixPat
extractInfixPatLhs :: Typed.TypedTerm Syntax.ExtractInfixPat -> Typed.TypedTerm Syntax.Pat
extractInfixPatLhs x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ExtractInfixPat"),
        Core.projectionFieldName = (Core.Name "lhs")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the op field of hydra.scala.syntax.ExtractInfixPat
extractInfixPatOp :: Typed.TypedTerm Syntax.ExtractInfixPat -> Typed.TypedTerm Syntax.NameData
extractInfixPatOp x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ExtractInfixPat"),
        Core.projectionFieldName = (Core.Name "op")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the rhs field of hydra.scala.syntax.ExtractInfixPat
extractInfixPatRhs :: Typed.TypedTerm Syntax.ExtractInfixPat -> Typed.TypedTerm [Syntax.Pat]
extractInfixPatRhs x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ExtractInfixPat"),
        Core.projectionFieldName = (Core.Name "rhs")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the lhs field of hydra.scala.syntax.ExtractInfixPat
extractInfixPatWithLhs :: Typed.TypedTerm Syntax.ExtractInfixPat -> Typed.TypedTerm Syntax.Pat -> Typed.TypedTerm Syntax.ExtractInfixPat
extractInfixPatWithLhs original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ExtractInfixPat"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "op"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ExtractInfixPat"),
              Core.projectionFieldName = (Core.Name "op")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ExtractInfixPat"),
              Core.projectionFieldName = (Core.Name "rhs")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the op field of hydra.scala.syntax.ExtractInfixPat
extractInfixPatWithOp :: Typed.TypedTerm Syntax.ExtractInfixPat -> Typed.TypedTerm Syntax.NameData -> Typed.TypedTerm Syntax.ExtractInfixPat
extractInfixPatWithOp original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ExtractInfixPat"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ExtractInfixPat"),
              Core.projectionFieldName = (Core.Name "lhs")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "op"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ExtractInfixPat"),
              Core.projectionFieldName = (Core.Name "rhs")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the rhs field of hydra.scala.syntax.ExtractInfixPat
extractInfixPatWithRhs :: Typed.TypedTerm Syntax.ExtractInfixPat -> Typed.TypedTerm [Syntax.Pat] -> Typed.TypedTerm Syntax.ExtractInfixPat
extractInfixPatWithRhs original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ExtractInfixPat"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ExtractInfixPat"),
              Core.projectionFieldName = (Core.Name "lhs")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "op"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ExtractInfixPat"),
              Core.projectionFieldName = (Core.Name "op")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.scala.syntax.ExtractPat
extractPat :: Typed.TypedTerm Syntax.Data -> Typed.TypedTerm [Syntax.Pat] -> Typed.TypedTerm Syntax.ExtractPat
extractPat fun args =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ExtractPat"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "fun"),
          Core.fieldTerm = (Typed.unTypedTerm fun)},
        Core.Field {
          Core.fieldName = (Core.Name "args"),
          Core.fieldTerm = (Typed.unTypedTerm args)}]}))
-- | DSL accessor for the args field of hydra.scala.syntax.ExtractPat
extractPatArgs :: Typed.TypedTerm Syntax.ExtractPat -> Typed.TypedTerm [Syntax.Pat]
extractPatArgs x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ExtractPat"),
        Core.projectionFieldName = (Core.Name "args")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the fun field of hydra.scala.syntax.ExtractPat
extractPatFun :: Typed.TypedTerm Syntax.ExtractPat -> Typed.TypedTerm Syntax.Data
extractPatFun x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ExtractPat"),
        Core.projectionFieldName = (Core.Name "fun")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the args field of hydra.scala.syntax.ExtractPat
extractPatWithArgs :: Typed.TypedTerm Syntax.ExtractPat -> Typed.TypedTerm [Syntax.Pat] -> Typed.TypedTerm Syntax.ExtractPat
extractPatWithArgs original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ExtractPat"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "fun"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ExtractPat"),
              Core.projectionFieldName = (Core.Name "fun")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "args"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the fun field of hydra.scala.syntax.ExtractPat
extractPatWithFun :: Typed.TypedTerm Syntax.ExtractPat -> Typed.TypedTerm Syntax.Data -> Typed.TypedTerm Syntax.ExtractPat
extractPatWithFun original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ExtractPat"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "fun"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "args"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ExtractPat"),
              Core.projectionFieldName = (Core.Name "args")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.scala.syntax.ForData
forData :: Typed.TypedTerm [Syntax.Enumerator] -> Typed.TypedTerm Syntax.ForData
forData enums =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ForData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "enums"),
          Core.fieldTerm = (Typed.unTypedTerm enums)}]}))
-- | DSL accessor for the enums field of hydra.scala.syntax.ForData
forDataEnums :: Typed.TypedTerm Syntax.ForData -> Typed.TypedTerm [Syntax.Enumerator]
forDataEnums x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ForData"),
        Core.projectionFieldName = (Core.Name "enums")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the enums field of hydra.scala.syntax.ForData
forDataWithEnums :: Typed.TypedTerm Syntax.ForData -> Typed.TypedTerm [Syntax.Enumerator] -> Typed.TypedTerm Syntax.ForData
forDataWithEnums original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ForData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "enums"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.scala.syntax.ForYieldData
forYieldData :: Typed.TypedTerm [Syntax.Enumerator] -> Typed.TypedTerm Syntax.ForYieldData
forYieldData enums =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ForYieldData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "enums"),
          Core.fieldTerm = (Typed.unTypedTerm enums)}]}))
-- | DSL accessor for the enums field of hydra.scala.syntax.ForYieldData
forYieldDataEnums :: Typed.TypedTerm Syntax.ForYieldData -> Typed.TypedTerm [Syntax.Enumerator]
forYieldDataEnums x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ForYieldData"),
        Core.projectionFieldName = (Core.Name "enums")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the enums field of hydra.scala.syntax.ForYieldData
forYieldDataWithEnums :: Typed.TypedTerm Syntax.ForYieldData -> Typed.TypedTerm [Syntax.Enumerator] -> Typed.TypedTerm Syntax.ForYieldData
forYieldDataWithEnums original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ForYieldData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "enums"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.scala.syntax.FunctionData
functionData :: Typed.TypedTerm [Syntax.ParamData] -> Typed.TypedTerm Syntax.Data -> Typed.TypedTerm Syntax.FunctionData
functionData params body =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.FunctionData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Typed.unTypedTerm params)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm body)}]}))
-- | DSL accessor for the body field of hydra.scala.syntax.FunctionData
functionDataBody :: Typed.TypedTerm Syntax.FunctionData -> Typed.TypedTerm Syntax.Data
functionDataBody x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.FunctionData"),
        Core.projectionFieldName = (Core.Name "body")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the params field of hydra.scala.syntax.FunctionData
functionDataParams :: Typed.TypedTerm Syntax.FunctionData -> Typed.TypedTerm [Syntax.ParamData]
functionDataParams x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.FunctionData"),
        Core.projectionFieldName = (Core.Name "params")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the body field of hydra.scala.syntax.FunctionData
functionDataWithBody :: Typed.TypedTerm Syntax.FunctionData -> Typed.TypedTerm Syntax.Data -> Typed.TypedTerm Syntax.FunctionData
functionDataWithBody original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.FunctionData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.FunctionData"),
              Core.projectionFieldName = (Core.Name "params")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the params field of hydra.scala.syntax.FunctionData
functionDataWithParams :: Typed.TypedTerm Syntax.FunctionData -> Typed.TypedTerm [Syntax.ParamData] -> Typed.TypedTerm Syntax.FunctionData
functionDataWithParams original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.FunctionData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.FunctionData"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.scala.syntax.FunctionType
functionType :: Typed.TypedTerm [Syntax.Type] -> Typed.TypedTerm Syntax.Type -> Typed.TypedTerm Syntax.FunctionType
functionType params res =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.FunctionType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Typed.unTypedTerm params)},
        Core.Field {
          Core.fieldName = (Core.Name "res"),
          Core.fieldTerm = (Typed.unTypedTerm res)}]}))
-- | DSL accessor for the params field of hydra.scala.syntax.FunctionType
functionTypeParams :: Typed.TypedTerm Syntax.FunctionType -> Typed.TypedTerm [Syntax.Type]
functionTypeParams x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.FunctionType"),
        Core.projectionFieldName = (Core.Name "params")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the res field of hydra.scala.syntax.FunctionType
functionTypeRes :: Typed.TypedTerm Syntax.FunctionType -> Typed.TypedTerm Syntax.Type
functionTypeRes x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.FunctionType"),
        Core.projectionFieldName = (Core.Name "res")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the params field of hydra.scala.syntax.FunctionType
functionTypeWithParams :: Typed.TypedTerm Syntax.FunctionType -> Typed.TypedTerm [Syntax.Type] -> Typed.TypedTerm Syntax.FunctionType
functionTypeWithParams original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.FunctionType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "res"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.FunctionType"),
              Core.projectionFieldName = (Core.Name "res")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the res field of hydra.scala.syntax.FunctionType
functionTypeWithRes :: Typed.TypedTerm Syntax.FunctionType -> Typed.TypedTerm Syntax.Type -> Typed.TypedTerm Syntax.FunctionType
functionTypeWithRes original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.FunctionType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.FunctionType"),
              Core.projectionFieldName = (Core.Name "params")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "res"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.scala.syntax.GeneratorEnumerator
generatorEnumerator :: Typed.TypedTerm Syntax.Pat -> Typed.TypedTerm Syntax.Data -> Typed.TypedTerm Syntax.GeneratorEnumerator
generatorEnumerator pat rhs =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.GeneratorEnumerator"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pat"),
          Core.fieldTerm = (Typed.unTypedTerm pat)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Typed.unTypedTerm rhs)}]}))
-- | DSL accessor for the pat field of hydra.scala.syntax.GeneratorEnumerator
generatorEnumeratorPat :: Typed.TypedTerm Syntax.GeneratorEnumerator -> Typed.TypedTerm Syntax.Pat
generatorEnumeratorPat x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GeneratorEnumerator"),
        Core.projectionFieldName = (Core.Name "pat")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the rhs field of hydra.scala.syntax.GeneratorEnumerator
generatorEnumeratorRhs :: Typed.TypedTerm Syntax.GeneratorEnumerator -> Typed.TypedTerm Syntax.Data
generatorEnumeratorRhs x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GeneratorEnumerator"),
        Core.projectionFieldName = (Core.Name "rhs")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the pat field of hydra.scala.syntax.GeneratorEnumerator
generatorEnumeratorWithPat :: Typed.TypedTerm Syntax.GeneratorEnumerator -> Typed.TypedTerm Syntax.Pat -> Typed.TypedTerm Syntax.GeneratorEnumerator
generatorEnumeratorWithPat original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.GeneratorEnumerator"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pat"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GeneratorEnumerator"),
              Core.projectionFieldName = (Core.Name "rhs")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the rhs field of hydra.scala.syntax.GeneratorEnumerator
generatorEnumeratorWithRhs :: Typed.TypedTerm Syntax.GeneratorEnumerator -> Typed.TypedTerm Syntax.Data -> Typed.TypedTerm Syntax.GeneratorEnumerator
generatorEnumeratorWithRhs original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.GeneratorEnumerator"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pat"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GeneratorEnumerator"),
              Core.projectionFieldName = (Core.Name "pat")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.scala.syntax.GivenAliasDefn
givenAliasDefn :: Typed.TypedTerm [Syntax.Mod] -> Typed.TypedTerm Syntax.Name -> Typed.TypedTerm [[Syntax.ParamType]] -> Typed.TypedTerm [[Syntax.ParamData]] -> Typed.TypedTerm Syntax.Type -> Typed.TypedTerm Syntax.Data -> Typed.TypedTerm Syntax.GivenAliasDefn
givenAliasDefn mods name tparams sparams decltpe body =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.GivenAliasDefn"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Typed.unTypedTerm mods)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Typed.unTypedTerm tparams)},
        Core.Field {
          Core.fieldName = (Core.Name "sparams"),
          Core.fieldTerm = (Typed.unTypedTerm sparams)},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Typed.unTypedTerm decltpe)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm body)}]}))
-- | DSL accessor for the body field of hydra.scala.syntax.GivenAliasDefn
givenAliasDefnBody :: Typed.TypedTerm Syntax.GivenAliasDefn -> Typed.TypedTerm Syntax.Data
givenAliasDefnBody x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenAliasDefn"),
        Core.projectionFieldName = (Core.Name "body")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the decltpe field of hydra.scala.syntax.GivenAliasDefn
givenAliasDefnDecltpe :: Typed.TypedTerm Syntax.GivenAliasDefn -> Typed.TypedTerm Syntax.Type
givenAliasDefnDecltpe x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenAliasDefn"),
        Core.projectionFieldName = (Core.Name "decltpe")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the mods field of hydra.scala.syntax.GivenAliasDefn
givenAliasDefnMods :: Typed.TypedTerm Syntax.GivenAliasDefn -> Typed.TypedTerm [Syntax.Mod]
givenAliasDefnMods x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenAliasDefn"),
        Core.projectionFieldName = (Core.Name "mods")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the name field of hydra.scala.syntax.GivenAliasDefn
givenAliasDefnName :: Typed.TypedTerm Syntax.GivenAliasDefn -> Typed.TypedTerm Syntax.Name
givenAliasDefnName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenAliasDefn"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the sparams field of hydra.scala.syntax.GivenAliasDefn
givenAliasDefnSparams :: Typed.TypedTerm Syntax.GivenAliasDefn -> Typed.TypedTerm [[Syntax.ParamData]]
givenAliasDefnSparams x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenAliasDefn"),
        Core.projectionFieldName = (Core.Name "sparams")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the tparams field of hydra.scala.syntax.GivenAliasDefn
givenAliasDefnTparams :: Typed.TypedTerm Syntax.GivenAliasDefn -> Typed.TypedTerm [[Syntax.ParamType]]
givenAliasDefnTparams x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenAliasDefn"),
        Core.projectionFieldName = (Core.Name "tparams")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the body field of hydra.scala.syntax.GivenAliasDefn
givenAliasDefnWithBody :: Typed.TypedTerm Syntax.GivenAliasDefn -> Typed.TypedTerm Syntax.Data -> Typed.TypedTerm Syntax.GivenAliasDefn
givenAliasDefnWithBody original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.GivenAliasDefn"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenAliasDefn"),
              Core.projectionFieldName = (Core.Name "mods")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenAliasDefn"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenAliasDefn"),
              Core.projectionFieldName = (Core.Name "tparams")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "sparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenAliasDefn"),
              Core.projectionFieldName = (Core.Name "sparams")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenAliasDefn"),
              Core.projectionFieldName = (Core.Name "decltpe")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the decltpe field of hydra.scala.syntax.GivenAliasDefn
givenAliasDefnWithDecltpe :: Typed.TypedTerm Syntax.GivenAliasDefn -> Typed.TypedTerm Syntax.Type -> Typed.TypedTerm Syntax.GivenAliasDefn
givenAliasDefnWithDecltpe original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.GivenAliasDefn"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenAliasDefn"),
              Core.projectionFieldName = (Core.Name "mods")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenAliasDefn"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenAliasDefn"),
              Core.projectionFieldName = (Core.Name "tparams")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "sparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenAliasDefn"),
              Core.projectionFieldName = (Core.Name "sparams")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenAliasDefn"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the mods field of hydra.scala.syntax.GivenAliasDefn
givenAliasDefnWithMods :: Typed.TypedTerm Syntax.GivenAliasDefn -> Typed.TypedTerm [Syntax.Mod] -> Typed.TypedTerm Syntax.GivenAliasDefn
givenAliasDefnWithMods original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.GivenAliasDefn"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenAliasDefn"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenAliasDefn"),
              Core.projectionFieldName = (Core.Name "tparams")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "sparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenAliasDefn"),
              Core.projectionFieldName = (Core.Name "sparams")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenAliasDefn"),
              Core.projectionFieldName = (Core.Name "decltpe")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenAliasDefn"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the name field of hydra.scala.syntax.GivenAliasDefn
givenAliasDefnWithName :: Typed.TypedTerm Syntax.GivenAliasDefn -> Typed.TypedTerm Syntax.Name -> Typed.TypedTerm Syntax.GivenAliasDefn
givenAliasDefnWithName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.GivenAliasDefn"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenAliasDefn"),
              Core.projectionFieldName = (Core.Name "mods")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenAliasDefn"),
              Core.projectionFieldName = (Core.Name "tparams")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "sparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenAliasDefn"),
              Core.projectionFieldName = (Core.Name "sparams")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenAliasDefn"),
              Core.projectionFieldName = (Core.Name "decltpe")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenAliasDefn"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the sparams field of hydra.scala.syntax.GivenAliasDefn
givenAliasDefnWithSparams :: Typed.TypedTerm Syntax.GivenAliasDefn -> Typed.TypedTerm [[Syntax.ParamData]] -> Typed.TypedTerm Syntax.GivenAliasDefn
givenAliasDefnWithSparams original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.GivenAliasDefn"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenAliasDefn"),
              Core.projectionFieldName = (Core.Name "mods")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenAliasDefn"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenAliasDefn"),
              Core.projectionFieldName = (Core.Name "tparams")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "sparams"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenAliasDefn"),
              Core.projectionFieldName = (Core.Name "decltpe")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenAliasDefn"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the tparams field of hydra.scala.syntax.GivenAliasDefn
givenAliasDefnWithTparams :: Typed.TypedTerm Syntax.GivenAliasDefn -> Typed.TypedTerm [[Syntax.ParamType]] -> Typed.TypedTerm Syntax.GivenAliasDefn
givenAliasDefnWithTparams original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.GivenAliasDefn"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenAliasDefn"),
              Core.projectionFieldName = (Core.Name "mods")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenAliasDefn"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "sparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenAliasDefn"),
              Core.projectionFieldName = (Core.Name "sparams")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenAliasDefn"),
              Core.projectionFieldName = (Core.Name "decltpe")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenAliasDefn"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.scala.syntax.GivenDecl
givenDecl :: Typed.TypedTerm [Syntax.Mod] -> Typed.TypedTerm Syntax.NameData -> Typed.TypedTerm [Syntax.ParamType] -> Typed.TypedTerm [[Syntax.ParamData]] -> Typed.TypedTerm Syntax.Type -> Typed.TypedTerm Syntax.GivenDecl
givenDecl mods name tparams sparams decltpe =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.GivenDecl"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Typed.unTypedTerm mods)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Typed.unTypedTerm tparams)},
        Core.Field {
          Core.fieldName = (Core.Name "sparams"),
          Core.fieldTerm = (Typed.unTypedTerm sparams)},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Typed.unTypedTerm decltpe)}]}))
-- | DSL accessor for the decltpe field of hydra.scala.syntax.GivenDecl
givenDeclDecltpe :: Typed.TypedTerm Syntax.GivenDecl -> Typed.TypedTerm Syntax.Type
givenDeclDecltpe x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenDecl"),
        Core.projectionFieldName = (Core.Name "decltpe")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the mods field of hydra.scala.syntax.GivenDecl
givenDeclMods :: Typed.TypedTerm Syntax.GivenDecl -> Typed.TypedTerm [Syntax.Mod]
givenDeclMods x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenDecl"),
        Core.projectionFieldName = (Core.Name "mods")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the name field of hydra.scala.syntax.GivenDecl
givenDeclName :: Typed.TypedTerm Syntax.GivenDecl -> Typed.TypedTerm Syntax.NameData
givenDeclName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenDecl"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the sparams field of hydra.scala.syntax.GivenDecl
givenDeclSparams :: Typed.TypedTerm Syntax.GivenDecl -> Typed.TypedTerm [[Syntax.ParamData]]
givenDeclSparams x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenDecl"),
        Core.projectionFieldName = (Core.Name "sparams")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the tparams field of hydra.scala.syntax.GivenDecl
givenDeclTparams :: Typed.TypedTerm Syntax.GivenDecl -> Typed.TypedTerm [Syntax.ParamType]
givenDeclTparams x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenDecl"),
        Core.projectionFieldName = (Core.Name "tparams")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the decltpe field of hydra.scala.syntax.GivenDecl
givenDeclWithDecltpe :: Typed.TypedTerm Syntax.GivenDecl -> Typed.TypedTerm Syntax.Type -> Typed.TypedTerm Syntax.GivenDecl
givenDeclWithDecltpe original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.GivenDecl"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenDecl"),
              Core.projectionFieldName = (Core.Name "mods")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenDecl"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenDecl"),
              Core.projectionFieldName = (Core.Name "tparams")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "sparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenDecl"),
              Core.projectionFieldName = (Core.Name "sparams")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the mods field of hydra.scala.syntax.GivenDecl
givenDeclWithMods :: Typed.TypedTerm Syntax.GivenDecl -> Typed.TypedTerm [Syntax.Mod] -> Typed.TypedTerm Syntax.GivenDecl
givenDeclWithMods original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.GivenDecl"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenDecl"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenDecl"),
              Core.projectionFieldName = (Core.Name "tparams")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "sparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenDecl"),
              Core.projectionFieldName = (Core.Name "sparams")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenDecl"),
              Core.projectionFieldName = (Core.Name "decltpe")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the name field of hydra.scala.syntax.GivenDecl
givenDeclWithName :: Typed.TypedTerm Syntax.GivenDecl -> Typed.TypedTerm Syntax.NameData -> Typed.TypedTerm Syntax.GivenDecl
givenDeclWithName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.GivenDecl"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenDecl"),
              Core.projectionFieldName = (Core.Name "mods")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenDecl"),
              Core.projectionFieldName = (Core.Name "tparams")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "sparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenDecl"),
              Core.projectionFieldName = (Core.Name "sparams")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenDecl"),
              Core.projectionFieldName = (Core.Name "decltpe")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the sparams field of hydra.scala.syntax.GivenDecl
givenDeclWithSparams :: Typed.TypedTerm Syntax.GivenDecl -> Typed.TypedTerm [[Syntax.ParamData]] -> Typed.TypedTerm Syntax.GivenDecl
givenDeclWithSparams original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.GivenDecl"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenDecl"),
              Core.projectionFieldName = (Core.Name "mods")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenDecl"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenDecl"),
              Core.projectionFieldName = (Core.Name "tparams")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "sparams"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenDecl"),
              Core.projectionFieldName = (Core.Name "decltpe")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the tparams field of hydra.scala.syntax.GivenDecl
givenDeclWithTparams :: Typed.TypedTerm Syntax.GivenDecl -> Typed.TypedTerm [Syntax.ParamType] -> Typed.TypedTerm Syntax.GivenDecl
givenDeclWithTparams original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.GivenDecl"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenDecl"),
              Core.projectionFieldName = (Core.Name "mods")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenDecl"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "sparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenDecl"),
              Core.projectionFieldName = (Core.Name "sparams")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenDecl"),
              Core.projectionFieldName = (Core.Name "decltpe")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.scala.syntax.GivenDefn
givenDefn :: Typed.TypedTerm [Syntax.Mod] -> Typed.TypedTerm Syntax.Name -> Typed.TypedTerm [[Syntax.ParamType]] -> Typed.TypedTerm [[Syntax.ParamData]] -> Typed.TypedTerm Syntax.Template -> Typed.TypedTerm Syntax.GivenDefn
givenDefn mods name tparams sparams templ =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.GivenDefn"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Typed.unTypedTerm mods)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Typed.unTypedTerm tparams)},
        Core.Field {
          Core.fieldName = (Core.Name "sparams"),
          Core.fieldTerm = (Typed.unTypedTerm sparams)},
        Core.Field {
          Core.fieldName = (Core.Name "templ"),
          Core.fieldTerm = (Typed.unTypedTerm templ)}]}))
-- | DSL accessor for the mods field of hydra.scala.syntax.GivenDefn
givenDefnMods :: Typed.TypedTerm Syntax.GivenDefn -> Typed.TypedTerm [Syntax.Mod]
givenDefnMods x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenDefn"),
        Core.projectionFieldName = (Core.Name "mods")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the name field of hydra.scala.syntax.GivenDefn
givenDefnName :: Typed.TypedTerm Syntax.GivenDefn -> Typed.TypedTerm Syntax.Name
givenDefnName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenDefn"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the sparams field of hydra.scala.syntax.GivenDefn
givenDefnSparams :: Typed.TypedTerm Syntax.GivenDefn -> Typed.TypedTerm [[Syntax.ParamData]]
givenDefnSparams x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenDefn"),
        Core.projectionFieldName = (Core.Name "sparams")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the templ field of hydra.scala.syntax.GivenDefn
givenDefnTempl :: Typed.TypedTerm Syntax.GivenDefn -> Typed.TypedTerm Syntax.Template
givenDefnTempl x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenDefn"),
        Core.projectionFieldName = (Core.Name "templ")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the tparams field of hydra.scala.syntax.GivenDefn
givenDefnTparams :: Typed.TypedTerm Syntax.GivenDefn -> Typed.TypedTerm [[Syntax.ParamType]]
givenDefnTparams x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenDefn"),
        Core.projectionFieldName = (Core.Name "tparams")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the mods field of hydra.scala.syntax.GivenDefn
givenDefnWithMods :: Typed.TypedTerm Syntax.GivenDefn -> Typed.TypedTerm [Syntax.Mod] -> Typed.TypedTerm Syntax.GivenDefn
givenDefnWithMods original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.GivenDefn"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenDefn"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenDefn"),
              Core.projectionFieldName = (Core.Name "tparams")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "sparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenDefn"),
              Core.projectionFieldName = (Core.Name "sparams")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "templ"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenDefn"),
              Core.projectionFieldName = (Core.Name "templ")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the name field of hydra.scala.syntax.GivenDefn
givenDefnWithName :: Typed.TypedTerm Syntax.GivenDefn -> Typed.TypedTerm Syntax.Name -> Typed.TypedTerm Syntax.GivenDefn
givenDefnWithName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.GivenDefn"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenDefn"),
              Core.projectionFieldName = (Core.Name "mods")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenDefn"),
              Core.projectionFieldName = (Core.Name "tparams")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "sparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenDefn"),
              Core.projectionFieldName = (Core.Name "sparams")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "templ"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenDefn"),
              Core.projectionFieldName = (Core.Name "templ")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the sparams field of hydra.scala.syntax.GivenDefn
givenDefnWithSparams :: Typed.TypedTerm Syntax.GivenDefn -> Typed.TypedTerm [[Syntax.ParamData]] -> Typed.TypedTerm Syntax.GivenDefn
givenDefnWithSparams original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.GivenDefn"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenDefn"),
              Core.projectionFieldName = (Core.Name "mods")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenDefn"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenDefn"),
              Core.projectionFieldName = (Core.Name "tparams")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "sparams"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "templ"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenDefn"),
              Core.projectionFieldName = (Core.Name "templ")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the templ field of hydra.scala.syntax.GivenDefn
givenDefnWithTempl :: Typed.TypedTerm Syntax.GivenDefn -> Typed.TypedTerm Syntax.Template -> Typed.TypedTerm Syntax.GivenDefn
givenDefnWithTempl original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.GivenDefn"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenDefn"),
              Core.projectionFieldName = (Core.Name "mods")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenDefn"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenDefn"),
              Core.projectionFieldName = (Core.Name "tparams")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "sparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenDefn"),
              Core.projectionFieldName = (Core.Name "sparams")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "templ"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the tparams field of hydra.scala.syntax.GivenDefn
givenDefnWithTparams :: Typed.TypedTerm Syntax.GivenDefn -> Typed.TypedTerm [[Syntax.ParamType]] -> Typed.TypedTerm Syntax.GivenDefn
givenDefnWithTparams original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.GivenDefn"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenDefn"),
              Core.projectionFieldName = (Core.Name "mods")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenDefn"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "sparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenDefn"),
              Core.projectionFieldName = (Core.Name "sparams")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "templ"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenDefn"),
              Core.projectionFieldName = (Core.Name "templ")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.scala.syntax.GivenImportee
givenImportee :: Typed.TypedTerm Syntax.Type -> Typed.TypedTerm Syntax.GivenImportee
givenImportee tpe =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.GivenImportee"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "tpe"),
          Core.fieldTerm = (Typed.unTypedTerm tpe)}]}))
-- | DSL accessor for the tpe field of hydra.scala.syntax.GivenImportee
givenImporteeTpe :: Typed.TypedTerm Syntax.GivenImportee -> Typed.TypedTerm Syntax.Type
givenImporteeTpe x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenImportee"),
        Core.projectionFieldName = (Core.Name "tpe")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the tpe field of hydra.scala.syntax.GivenImportee
givenImporteeWithTpe :: Typed.TypedTerm Syntax.GivenImportee -> Typed.TypedTerm Syntax.Type -> Typed.TypedTerm Syntax.GivenImportee
givenImporteeWithTpe original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.GivenImportee"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "tpe"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.scala.syntax.GivenPat
givenPat :: Typed.TypedTerm Syntax.Type -> Typed.TypedTerm Syntax.GivenPat
givenPat tpe =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.GivenPat"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "tpe"),
          Core.fieldTerm = (Typed.unTypedTerm tpe)}]}))
-- | DSL accessor for the tpe field of hydra.scala.syntax.GivenPat
givenPatTpe :: Typed.TypedTerm Syntax.GivenPat -> Typed.TypedTerm Syntax.Type
givenPatTpe x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GivenPat"),
        Core.projectionFieldName = (Core.Name "tpe")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the tpe field of hydra.scala.syntax.GivenPat
givenPatWithTpe :: Typed.TypedTerm Syntax.GivenPat -> Typed.TypedTerm Syntax.Type -> Typed.TypedTerm Syntax.GivenPat
givenPatWithTpe original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.GivenPat"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "tpe"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.scala.syntax.GuardEnumerator
guardEnumerator :: Typed.TypedTerm Syntax.Data -> Typed.TypedTerm Syntax.GuardEnumerator
guardEnumerator cond =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.GuardEnumerator"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "cond"),
          Core.fieldTerm = (Typed.unTypedTerm cond)}]}))
-- | DSL accessor for the cond field of hydra.scala.syntax.GuardEnumerator
guardEnumeratorCond :: Typed.TypedTerm Syntax.GuardEnumerator -> Typed.TypedTerm Syntax.Data
guardEnumeratorCond x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.GuardEnumerator"),
        Core.projectionFieldName = (Core.Name "cond")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the cond field of hydra.scala.syntax.GuardEnumerator
guardEnumeratorWithCond :: Typed.TypedTerm Syntax.GuardEnumerator -> Typed.TypedTerm Syntax.Data -> Typed.TypedTerm Syntax.GuardEnumerator
guardEnumeratorWithCond original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.GuardEnumerator"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "cond"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.scala.syntax.IfData
ifData :: Typed.TypedTerm Syntax.Data -> Typed.TypedTerm Syntax.Data -> Typed.TypedTerm Syntax.Data -> Typed.TypedTerm Syntax.IfData
ifData cond thenp elsep =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.IfData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "cond"),
          Core.fieldTerm = (Typed.unTypedTerm cond)},
        Core.Field {
          Core.fieldName = (Core.Name "thenp"),
          Core.fieldTerm = (Typed.unTypedTerm thenp)},
        Core.Field {
          Core.fieldName = (Core.Name "elsep"),
          Core.fieldTerm = (Typed.unTypedTerm elsep)}]}))
-- | DSL accessor for the cond field of hydra.scala.syntax.IfData
ifDataCond :: Typed.TypedTerm Syntax.IfData -> Typed.TypedTerm Syntax.Data
ifDataCond x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.IfData"),
        Core.projectionFieldName = (Core.Name "cond")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the elsep field of hydra.scala.syntax.IfData
ifDataElsep :: Typed.TypedTerm Syntax.IfData -> Typed.TypedTerm Syntax.Data
ifDataElsep x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.IfData"),
        Core.projectionFieldName = (Core.Name "elsep")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the thenp field of hydra.scala.syntax.IfData
ifDataThenp :: Typed.TypedTerm Syntax.IfData -> Typed.TypedTerm Syntax.Data
ifDataThenp x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.IfData"),
        Core.projectionFieldName = (Core.Name "thenp")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the cond field of hydra.scala.syntax.IfData
ifDataWithCond :: Typed.TypedTerm Syntax.IfData -> Typed.TypedTerm Syntax.Data -> Typed.TypedTerm Syntax.IfData
ifDataWithCond original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.IfData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "cond"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "thenp"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.IfData"),
              Core.projectionFieldName = (Core.Name "thenp")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "elsep"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.IfData"),
              Core.projectionFieldName = (Core.Name "elsep")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the elsep field of hydra.scala.syntax.IfData
ifDataWithElsep :: Typed.TypedTerm Syntax.IfData -> Typed.TypedTerm Syntax.Data -> Typed.TypedTerm Syntax.IfData
ifDataWithElsep original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.IfData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "cond"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.IfData"),
              Core.projectionFieldName = (Core.Name "cond")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "thenp"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.IfData"),
              Core.projectionFieldName = (Core.Name "thenp")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "elsep"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the thenp field of hydra.scala.syntax.IfData
ifDataWithThenp :: Typed.TypedTerm Syntax.IfData -> Typed.TypedTerm Syntax.Data -> Typed.TypedTerm Syntax.IfData
ifDataWithThenp original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.IfData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "cond"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.IfData"),
              Core.projectionFieldName = (Core.Name "cond")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "thenp"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "elsep"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.IfData"),
              Core.projectionFieldName = (Core.Name "elsep")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.scala.syntax.ImplicitFunctionType
implicitFunctionType :: Typed.TypedTerm [Syntax.Type] -> Typed.TypedTerm Syntax.Type -> Typed.TypedTerm Syntax.ImplicitFunctionType
implicitFunctionType params res =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ImplicitFunctionType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Typed.unTypedTerm params)},
        Core.Field {
          Core.fieldName = (Core.Name "res"),
          Core.fieldTerm = (Typed.unTypedTerm res)}]}))
-- | DSL accessor for the params field of hydra.scala.syntax.ImplicitFunctionType
implicitFunctionTypeParams :: Typed.TypedTerm Syntax.ImplicitFunctionType -> Typed.TypedTerm [Syntax.Type]
implicitFunctionTypeParams x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ImplicitFunctionType"),
        Core.projectionFieldName = (Core.Name "params")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the res field of hydra.scala.syntax.ImplicitFunctionType
implicitFunctionTypeRes :: Typed.TypedTerm Syntax.ImplicitFunctionType -> Typed.TypedTerm Syntax.Type
implicitFunctionTypeRes x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ImplicitFunctionType"),
        Core.projectionFieldName = (Core.Name "res")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the params field of hydra.scala.syntax.ImplicitFunctionType
implicitFunctionTypeWithParams :: Typed.TypedTerm Syntax.ImplicitFunctionType -> Typed.TypedTerm [Syntax.Type] -> Typed.TypedTerm Syntax.ImplicitFunctionType
implicitFunctionTypeWithParams original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ImplicitFunctionType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "res"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ImplicitFunctionType"),
              Core.projectionFieldName = (Core.Name "res")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the res field of hydra.scala.syntax.ImplicitFunctionType
implicitFunctionTypeWithRes :: Typed.TypedTerm Syntax.ImplicitFunctionType -> Typed.TypedTerm Syntax.Type -> Typed.TypedTerm Syntax.ImplicitFunctionType
implicitFunctionTypeWithRes original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ImplicitFunctionType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ImplicitFunctionType"),
              Core.projectionFieldName = (Core.Name "params")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "res"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.scala.syntax.Import
import_ :: Typed.TypedTerm [Syntax.Importer] -> Typed.TypedTerm Syntax.Import
import_ importers =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.Import"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "importers"),
          Core.fieldTerm = (Typed.unTypedTerm importers)}]}))
-- | DSL injection for the export variant of hydra.scala.syntax.ImportExportStat
importExportStatExport :: Typed.TypedTerm Syntax.Export -> Typed.TypedTerm Syntax.ImportExportStat
importExportStatExport x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.ImportExportStat"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "export"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the import variant of hydra.scala.syntax.ImportExportStat
importExportStatImport :: Typed.TypedTerm Syntax.Import -> Typed.TypedTerm Syntax.ImportExportStat
importExportStatImport x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.ImportExportStat"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "import"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL accessor for the importers field of hydra.scala.syntax.Import
importImporters :: Typed.TypedTerm Syntax.Import -> Typed.TypedTerm [Syntax.Importer]
importImporters x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.Import"),
        Core.projectionFieldName = (Core.Name "importers")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the importers field of hydra.scala.syntax.Import
importWithImporters :: Typed.TypedTerm Syntax.Import -> Typed.TypedTerm [Syntax.Importer] -> Typed.TypedTerm Syntax.Import
importWithImporters original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.Import"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "importers"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL injection for the given variant of hydra.scala.syntax.Importee
importeeGiven :: Typed.TypedTerm Syntax.GivenImportee -> Typed.TypedTerm Syntax.Importee
importeeGiven x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Importee"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "given"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the givenAll variant of hydra.scala.syntax.Importee
importeeGivenAll :: Typed.TypedTerm Syntax.Importee
importeeGivenAll =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Importee"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "givenAll"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the name variant of hydra.scala.syntax.Importee
importeeName :: Typed.TypedTerm Syntax.NameImportee -> Typed.TypedTerm Syntax.Importee
importeeName x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Importee"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "name"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the rename variant of hydra.scala.syntax.Importee
importeeRename :: Typed.TypedTerm Syntax.RenameImportee -> Typed.TypedTerm Syntax.Importee
importeeRename x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Importee"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "rename"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the unimport variant of hydra.scala.syntax.Importee
importeeUnimport :: Typed.TypedTerm Syntax.UnimportImportee -> Typed.TypedTerm Syntax.Importee
importeeUnimport x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Importee"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "unimport"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the wildcard variant of hydra.scala.syntax.Importee
importeeWildcard :: Typed.TypedTerm Syntax.Importee
importeeWildcard =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Importee"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "wildcard"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL constructor for hydra.scala.syntax.Importer
importer :: Typed.TypedTerm Syntax.RefData -> Typed.TypedTerm [Syntax.Importee] -> Typed.TypedTerm Syntax.Importer
importer ref importees =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.Importer"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "ref"),
          Core.fieldTerm = (Typed.unTypedTerm ref)},
        Core.Field {
          Core.fieldName = (Core.Name "importees"),
          Core.fieldTerm = (Typed.unTypedTerm importees)}]}))
-- | DSL accessor for the importees field of hydra.scala.syntax.Importer
importerImportees :: Typed.TypedTerm Syntax.Importer -> Typed.TypedTerm [Syntax.Importee]
importerImportees x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.Importer"),
        Core.projectionFieldName = (Core.Name "importees")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the ref field of hydra.scala.syntax.Importer
importerRef :: Typed.TypedTerm Syntax.Importer -> Typed.TypedTerm Syntax.RefData
importerRef x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.Importer"),
        Core.projectionFieldName = (Core.Name "ref")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the importees field of hydra.scala.syntax.Importer
importerWithImportees :: Typed.TypedTerm Syntax.Importer -> Typed.TypedTerm [Syntax.Importee] -> Typed.TypedTerm Syntax.Importer
importerWithImportees original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.Importer"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "ref"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.Importer"),
              Core.projectionFieldName = (Core.Name "ref")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "importees"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the ref field of hydra.scala.syntax.Importer
importerWithRef :: Typed.TypedTerm Syntax.Importer -> Typed.TypedTerm Syntax.RefData -> Typed.TypedTerm Syntax.Importer
importerWithRef original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.Importer"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "ref"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "importees"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.Importer"),
              Core.projectionFieldName = (Core.Name "importees")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.scala.syntax.Init
init :: Typed.TypedTerm Syntax.Type -> Typed.TypedTerm Syntax.Name -> Typed.TypedTerm [[Syntax.Data]] -> Typed.TypedTerm Syntax.Init
init tpe name argss =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.Init"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "tpe"),
          Core.fieldTerm = (Typed.unTypedTerm tpe)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "argss"),
          Core.fieldTerm = (Typed.unTypedTerm argss)}]}))
-- | DSL accessor for the argss field of hydra.scala.syntax.Init
initArgss :: Typed.TypedTerm Syntax.Init -> Typed.TypedTerm [[Syntax.Data]]
initArgss x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.Init"),
        Core.projectionFieldName = (Core.Name "argss")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the name field of hydra.scala.syntax.Init
initName :: Typed.TypedTerm Syntax.Init -> Typed.TypedTerm Syntax.Name
initName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.Init"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the tpe field of hydra.scala.syntax.Init
initTpe :: Typed.TypedTerm Syntax.Init -> Typed.TypedTerm Syntax.Type
initTpe x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.Init"),
        Core.projectionFieldName = (Core.Name "tpe")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the argss field of hydra.scala.syntax.Init
initWithArgss :: Typed.TypedTerm Syntax.Init -> Typed.TypedTerm [[Syntax.Data]] -> Typed.TypedTerm Syntax.Init
initWithArgss original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.Init"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "tpe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.Init"),
              Core.projectionFieldName = (Core.Name "tpe")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.Init"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "argss"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the name field of hydra.scala.syntax.Init
initWithName :: Typed.TypedTerm Syntax.Init -> Typed.TypedTerm Syntax.Name -> Typed.TypedTerm Syntax.Init
initWithName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.Init"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "tpe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.Init"),
              Core.projectionFieldName = (Core.Name "tpe")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "argss"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.Init"),
              Core.projectionFieldName = (Core.Name "argss")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the tpe field of hydra.scala.syntax.Init
initWithTpe :: Typed.TypedTerm Syntax.Init -> Typed.TypedTerm Syntax.Type -> Typed.TypedTerm Syntax.Init
initWithTpe original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.Init"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "tpe"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.Init"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "argss"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.Init"),
              Core.projectionFieldName = (Core.Name "argss")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.scala.syntax.InterpolateData
interpolateData :: Typed.TypedTerm Syntax.NameData -> Typed.TypedTerm [Syntax.Lit] -> Typed.TypedTerm [Syntax.Data] -> Typed.TypedTerm Syntax.InterpolateData
interpolateData prefix parts args =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.InterpolateData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "prefix"),
          Core.fieldTerm = (Typed.unTypedTerm prefix)},
        Core.Field {
          Core.fieldName = (Core.Name "parts"),
          Core.fieldTerm = (Typed.unTypedTerm parts)},
        Core.Field {
          Core.fieldName = (Core.Name "args"),
          Core.fieldTerm = (Typed.unTypedTerm args)}]}))
-- | DSL accessor for the args field of hydra.scala.syntax.InterpolateData
interpolateDataArgs :: Typed.TypedTerm Syntax.InterpolateData -> Typed.TypedTerm [Syntax.Data]
interpolateDataArgs x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.InterpolateData"),
        Core.projectionFieldName = (Core.Name "args")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the parts field of hydra.scala.syntax.InterpolateData
interpolateDataParts :: Typed.TypedTerm Syntax.InterpolateData -> Typed.TypedTerm [Syntax.Lit]
interpolateDataParts x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.InterpolateData"),
        Core.projectionFieldName = (Core.Name "parts")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the prefix field of hydra.scala.syntax.InterpolateData
interpolateDataPrefix :: Typed.TypedTerm Syntax.InterpolateData -> Typed.TypedTerm Syntax.NameData
interpolateDataPrefix x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.InterpolateData"),
        Core.projectionFieldName = (Core.Name "prefix")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the args field of hydra.scala.syntax.InterpolateData
interpolateDataWithArgs :: Typed.TypedTerm Syntax.InterpolateData -> Typed.TypedTerm [Syntax.Data] -> Typed.TypedTerm Syntax.InterpolateData
interpolateDataWithArgs original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.InterpolateData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "prefix"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.InterpolateData"),
              Core.projectionFieldName = (Core.Name "prefix")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "parts"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.InterpolateData"),
              Core.projectionFieldName = (Core.Name "parts")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "args"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the parts field of hydra.scala.syntax.InterpolateData
interpolateDataWithParts :: Typed.TypedTerm Syntax.InterpolateData -> Typed.TypedTerm [Syntax.Lit] -> Typed.TypedTerm Syntax.InterpolateData
interpolateDataWithParts original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.InterpolateData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "prefix"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.InterpolateData"),
              Core.projectionFieldName = (Core.Name "prefix")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "parts"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "args"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.InterpolateData"),
              Core.projectionFieldName = (Core.Name "args")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the prefix field of hydra.scala.syntax.InterpolateData
interpolateDataWithPrefix :: Typed.TypedTerm Syntax.InterpolateData -> Typed.TypedTerm Syntax.NameData -> Typed.TypedTerm Syntax.InterpolateData
interpolateDataWithPrefix original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.InterpolateData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "prefix"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "parts"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.InterpolateData"),
              Core.projectionFieldName = (Core.Name "parts")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "args"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.InterpolateData"),
              Core.projectionFieldName = (Core.Name "args")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.scala.syntax.InterpolatePat
interpolatePat :: Typed.TypedTerm Syntax.NameData -> Typed.TypedTerm [Syntax.Lit] -> Typed.TypedTerm Syntax.InterpolatePat
interpolatePat prefix parts =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.InterpolatePat"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "prefix"),
          Core.fieldTerm = (Typed.unTypedTerm prefix)},
        Core.Field {
          Core.fieldName = (Core.Name "parts"),
          Core.fieldTerm = (Typed.unTypedTerm parts)}]}))
-- | DSL accessor for the parts field of hydra.scala.syntax.InterpolatePat
interpolatePatParts :: Typed.TypedTerm Syntax.InterpolatePat -> Typed.TypedTerm [Syntax.Lit]
interpolatePatParts x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.InterpolatePat"),
        Core.projectionFieldName = (Core.Name "parts")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the prefix field of hydra.scala.syntax.InterpolatePat
interpolatePatPrefix :: Typed.TypedTerm Syntax.InterpolatePat -> Typed.TypedTerm Syntax.NameData
interpolatePatPrefix x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.InterpolatePat"),
        Core.projectionFieldName = (Core.Name "prefix")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the parts field of hydra.scala.syntax.InterpolatePat
interpolatePatWithParts :: Typed.TypedTerm Syntax.InterpolatePat -> Typed.TypedTerm [Syntax.Lit] -> Typed.TypedTerm Syntax.InterpolatePat
interpolatePatWithParts original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.InterpolatePat"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "prefix"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.InterpolatePat"),
              Core.projectionFieldName = (Core.Name "prefix")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "parts"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the prefix field of hydra.scala.syntax.InterpolatePat
interpolatePatWithPrefix :: Typed.TypedTerm Syntax.InterpolatePat -> Typed.TypedTerm Syntax.NameData -> Typed.TypedTerm Syntax.InterpolatePat
interpolatePatWithPrefix original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.InterpolatePat"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "prefix"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "parts"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.InterpolatePat"),
              Core.projectionFieldName = (Core.Name "parts")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.scala.syntax.LambdaType
lambdaType :: Typed.TypedTerm [Syntax.ParamType] -> Typed.TypedTerm Syntax.Type -> Typed.TypedTerm Syntax.LambdaType
lambdaType tparams tpe =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.LambdaType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Typed.unTypedTerm tparams)},
        Core.Field {
          Core.fieldName = (Core.Name "tpe"),
          Core.fieldTerm = (Typed.unTypedTerm tpe)}]}))
-- | DSL accessor for the tparams field of hydra.scala.syntax.LambdaType
lambdaTypeTparams :: Typed.TypedTerm Syntax.LambdaType -> Typed.TypedTerm [Syntax.ParamType]
lambdaTypeTparams x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.LambdaType"),
        Core.projectionFieldName = (Core.Name "tparams")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the tpe field of hydra.scala.syntax.LambdaType
lambdaTypeTpe :: Typed.TypedTerm Syntax.LambdaType -> Typed.TypedTerm Syntax.Type
lambdaTypeTpe x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.LambdaType"),
        Core.projectionFieldName = (Core.Name "tpe")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the tparams field of hydra.scala.syntax.LambdaType
lambdaTypeWithTparams :: Typed.TypedTerm Syntax.LambdaType -> Typed.TypedTerm [Syntax.ParamType] -> Typed.TypedTerm Syntax.LambdaType
lambdaTypeWithTparams original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.LambdaType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "tpe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.LambdaType"),
              Core.projectionFieldName = (Core.Name "tpe")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the tpe field of hydra.scala.syntax.LambdaType
lambdaTypeWithTpe :: Typed.TypedTerm Syntax.LambdaType -> Typed.TypedTerm Syntax.Type -> Typed.TypedTerm Syntax.LambdaType
lambdaTypeWithTpe original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.LambdaType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.LambdaType"),
              Core.projectionFieldName = (Core.Name "tparams")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tpe"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL injection for the boolean variant of hydra.scala.syntax.Lit
litBoolean :: Typed.TypedTerm Bool -> Typed.TypedTerm Syntax.Lit
litBoolean x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Lit"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "boolean"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the byte variant of hydra.scala.syntax.Lit
litByte :: Typed.TypedTerm I.Int8 -> Typed.TypedTerm Syntax.Lit
litByte x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Lit"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "byte"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the bytes variant of hydra.scala.syntax.Lit
litBytes :: Typed.TypedTerm [Int] -> Typed.TypedTerm Syntax.Lit
litBytes x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Lit"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "bytes"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the char variant of hydra.scala.syntax.Lit
litChar :: Typed.TypedTerm Int -> Typed.TypedTerm Syntax.Lit
litChar x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Lit"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "char"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the double variant of hydra.scala.syntax.Lit
litDouble :: Typed.TypedTerm Double -> Typed.TypedTerm Syntax.Lit
litDouble x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Lit"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "double"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the float variant of hydra.scala.syntax.Lit
litFloat :: Typed.TypedTerm Float -> Typed.TypedTerm Syntax.Lit
litFloat x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Lit"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "float"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the int variant of hydra.scala.syntax.Lit
litInt :: Typed.TypedTerm Int -> Typed.TypedTerm Syntax.Lit
litInt x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Lit"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "int"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the long variant of hydra.scala.syntax.Lit
litLong :: Typed.TypedTerm I.Int64 -> Typed.TypedTerm Syntax.Lit
litLong x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Lit"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "long"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the null variant of hydra.scala.syntax.Lit
litNull :: Typed.TypedTerm Syntax.Lit
litNull =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Lit"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "null"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the short variant of hydra.scala.syntax.Lit
litShort :: Typed.TypedTerm I.Int16 -> Typed.TypedTerm Syntax.Lit
litShort x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Lit"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "short"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the string variant of hydra.scala.syntax.Lit
litString :: Typed.TypedTerm String -> Typed.TypedTerm Syntax.Lit
litString x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Lit"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "string"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the symbol variant of hydra.scala.syntax.Lit
litSymbol :: Typed.TypedTerm Syntax.ScalaSymbol -> Typed.TypedTerm Syntax.Lit
litSymbol x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Lit"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "symbol"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the unit variant of hydra.scala.syntax.Lit
litUnit :: Typed.TypedTerm Syntax.Lit
litUnit =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Lit"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "unit"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL constructor for hydra.scala.syntax.MatchData
matchData :: Typed.TypedTerm Syntax.Data -> Typed.TypedTerm [Syntax.Case] -> Typed.TypedTerm Syntax.MatchData
matchData expr cases =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.MatchData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expr"),
          Core.fieldTerm = (Typed.unTypedTerm expr)},
        Core.Field {
          Core.fieldName = (Core.Name "cases"),
          Core.fieldTerm = (Typed.unTypedTerm cases)}]}))
-- | DSL accessor for the cases field of hydra.scala.syntax.MatchData
matchDataCases :: Typed.TypedTerm Syntax.MatchData -> Typed.TypedTerm [Syntax.Case]
matchDataCases x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.MatchData"),
        Core.projectionFieldName = (Core.Name "cases")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the expr field of hydra.scala.syntax.MatchData
matchDataExpr :: Typed.TypedTerm Syntax.MatchData -> Typed.TypedTerm Syntax.Data
matchDataExpr x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.MatchData"),
        Core.projectionFieldName = (Core.Name "expr")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the cases field of hydra.scala.syntax.MatchData
matchDataWithCases :: Typed.TypedTerm Syntax.MatchData -> Typed.TypedTerm [Syntax.Case] -> Typed.TypedTerm Syntax.MatchData
matchDataWithCases original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.MatchData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expr"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.MatchData"),
              Core.projectionFieldName = (Core.Name "expr")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "cases"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the expr field of hydra.scala.syntax.MatchData
matchDataWithExpr :: Typed.TypedTerm Syntax.MatchData -> Typed.TypedTerm Syntax.Data -> Typed.TypedTerm Syntax.MatchData
matchDataWithExpr original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.MatchData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expr"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "cases"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.MatchData"),
              Core.projectionFieldName = (Core.Name "cases")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.scala.syntax.MatchType
matchType :: Typed.TypedTerm Syntax.Type -> Typed.TypedTerm [Syntax.TypeCase] -> Typed.TypedTerm Syntax.MatchType
matchType tpe cases =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.MatchType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "tpe"),
          Core.fieldTerm = (Typed.unTypedTerm tpe)},
        Core.Field {
          Core.fieldName = (Core.Name "cases"),
          Core.fieldTerm = (Typed.unTypedTerm cases)}]}))
-- | DSL accessor for the cases field of hydra.scala.syntax.MatchType
matchTypeCases :: Typed.TypedTerm Syntax.MatchType -> Typed.TypedTerm [Syntax.TypeCase]
matchTypeCases x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.MatchType"),
        Core.projectionFieldName = (Core.Name "cases")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the tpe field of hydra.scala.syntax.MatchType
matchTypeTpe :: Typed.TypedTerm Syntax.MatchType -> Typed.TypedTerm Syntax.Type
matchTypeTpe x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.MatchType"),
        Core.projectionFieldName = (Core.Name "tpe")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the cases field of hydra.scala.syntax.MatchType
matchTypeWithCases :: Typed.TypedTerm Syntax.MatchType -> Typed.TypedTerm [Syntax.TypeCase] -> Typed.TypedTerm Syntax.MatchType
matchTypeWithCases original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.MatchType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "tpe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.MatchType"),
              Core.projectionFieldName = (Core.Name "tpe")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "cases"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the tpe field of hydra.scala.syntax.MatchType
matchTypeWithTpe :: Typed.TypedTerm Syntax.MatchType -> Typed.TypedTerm Syntax.Type -> Typed.TypedTerm Syntax.MatchType
matchTypeWithTpe original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.MatchType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "tpe"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "cases"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.MatchType"),
              Core.projectionFieldName = (Core.Name "cases")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL injection for the self variant of hydra.scala.syntax.Member
memberSelf :: Typed.TypedTerm Syntax.Self -> Typed.TypedTerm Syntax.Member
memberSelf x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Member"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "self"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the term variant of hydra.scala.syntax.Member
memberTerm :: Typed.TypedTerm Syntax.DataMember -> Typed.TypedTerm Syntax.Member
memberTerm x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Member"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "term"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the termParam variant of hydra.scala.syntax.Member
memberTermParam :: Typed.TypedTerm Syntax.ParamData -> Typed.TypedTerm Syntax.Member
memberTermParam x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Member"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "termParam"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the type variant of hydra.scala.syntax.Member
memberType :: Typed.TypedTerm Syntax.TypeMember -> Typed.TypedTerm Syntax.Member
memberType x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Member"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "type"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the typeParam variant of hydra.scala.syntax.Member
memberTypeParam :: Typed.TypedTerm Syntax.ParamType -> Typed.TypedTerm Syntax.Member
memberTypeParam x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Member"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "typeParam"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for hydra.scala.syntax.MethodType
methodType :: Typed.TypedTerm [[Syntax.ParamData]] -> Typed.TypedTerm Syntax.Type -> Typed.TypedTerm Syntax.MethodType
methodType paramss tpe =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.MethodType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "paramss"),
          Core.fieldTerm = (Typed.unTypedTerm paramss)},
        Core.Field {
          Core.fieldName = (Core.Name "tpe"),
          Core.fieldTerm = (Typed.unTypedTerm tpe)}]}))
-- | DSL accessor for the paramss field of hydra.scala.syntax.MethodType
methodTypeParamss :: Typed.TypedTerm Syntax.MethodType -> Typed.TypedTerm [[Syntax.ParamData]]
methodTypeParamss x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.MethodType"),
        Core.projectionFieldName = (Core.Name "paramss")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the tpe field of hydra.scala.syntax.MethodType
methodTypeTpe :: Typed.TypedTerm Syntax.MethodType -> Typed.TypedTerm Syntax.Type
methodTypeTpe x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.MethodType"),
        Core.projectionFieldName = (Core.Name "tpe")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the paramss field of hydra.scala.syntax.MethodType
methodTypeWithParamss :: Typed.TypedTerm Syntax.MethodType -> Typed.TypedTerm [[Syntax.ParamData]] -> Typed.TypedTerm Syntax.MethodType
methodTypeWithParamss original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.MethodType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "paramss"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "tpe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.MethodType"),
              Core.projectionFieldName = (Core.Name "tpe")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the tpe field of hydra.scala.syntax.MethodType
methodTypeWithTpe :: Typed.TypedTerm Syntax.MethodType -> Typed.TypedTerm Syntax.Type -> Typed.TypedTerm Syntax.MethodType
methodTypeWithTpe original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.MethodType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "paramss"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.MethodType"),
              Core.projectionFieldName = (Core.Name "paramss")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tpe"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL injection for the abstract variant of hydra.scala.syntax.Mod
modAbstract :: Typed.TypedTerm Syntax.Mod
modAbstract =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Mod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "abstract"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the annot variant of hydra.scala.syntax.Mod
modAnnot :: Typed.TypedTerm Syntax.AnnotMod -> Typed.TypedTerm Syntax.Mod
modAnnot x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Mod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "annot"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the case variant of hydra.scala.syntax.Mod
modCase :: Typed.TypedTerm Syntax.Mod
modCase =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Mod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "case"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the contravariant variant of hydra.scala.syntax.Mod
modContravariant :: Typed.TypedTerm Syntax.Mod
modContravariant =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Mod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "contravariant"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the covariant variant of hydra.scala.syntax.Mod
modCovariant :: Typed.TypedTerm Syntax.Mod
modCovariant =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Mod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "covariant"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the final variant of hydra.scala.syntax.Mod
modFinal :: Typed.TypedTerm Syntax.Mod
modFinal =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Mod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "final"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the implicit variant of hydra.scala.syntax.Mod
modImplicit :: Typed.TypedTerm Syntax.Mod
modImplicit =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Mod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "implicit"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the infix variant of hydra.scala.syntax.Mod
modInfix :: Typed.TypedTerm Syntax.Mod
modInfix =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Mod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "infix"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the inline variant of hydra.scala.syntax.Mod
modInline :: Typed.TypedTerm Syntax.Mod
modInline =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Mod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "inline"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the lazy variant of hydra.scala.syntax.Mod
modLazy :: Typed.TypedTerm Syntax.Mod
modLazy =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Mod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "lazy"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the opaque variant of hydra.scala.syntax.Mod
modOpaque :: Typed.TypedTerm Syntax.Mod
modOpaque =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Mod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "opaque"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the open variant of hydra.scala.syntax.Mod
modOpen :: Typed.TypedTerm Syntax.Mod
modOpen =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Mod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "open"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the override variant of hydra.scala.syntax.Mod
modOverride :: Typed.TypedTerm Syntax.Mod
modOverride =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Mod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "override"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the private variant of hydra.scala.syntax.Mod
modPrivate :: Typed.TypedTerm Syntax.PrivateMod -> Typed.TypedTerm Syntax.Mod
modPrivate x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Mod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "private"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the protected variant of hydra.scala.syntax.Mod
modProtected :: Typed.TypedTerm Syntax.ProtectedMod -> Typed.TypedTerm Syntax.Mod
modProtected x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Mod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "protected"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the sealed variant of hydra.scala.syntax.Mod
modSealed :: Typed.TypedTerm Syntax.Mod
modSealed =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Mod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "sealed"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the super variant of hydra.scala.syntax.Mod
modSuper :: Typed.TypedTerm Syntax.Mod
modSuper =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Mod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "super"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the transparent variant of hydra.scala.syntax.Mod
modTransparent :: Typed.TypedTerm Syntax.Mod
modTransparent =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Mod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "transparent"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the using variant of hydra.scala.syntax.Mod
modUsing :: Typed.TypedTerm Syntax.Mod
modUsing =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Mod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "using"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the valParam variant of hydra.scala.syntax.Mod
modValParam :: Typed.TypedTerm Syntax.Mod
modValParam =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Mod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "valParam"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the varParam variant of hydra.scala.syntax.Mod
modVarParam :: Typed.TypedTerm Syntax.Mod
modVarParam =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Mod"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "varParam"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the anonymous variant of hydra.scala.syntax.Name
nameAnonymous :: Typed.TypedTerm Syntax.Name
nameAnonymous =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Name"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "anonymous"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL constructor for hydra.scala.syntax.NameData
nameData :: Typed.TypedTerm Syntax.PredefString -> Typed.TypedTerm Syntax.NameData
nameData value =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.NameData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Typed.unTypedTerm value)}]}))
-- | DSL accessor for the value field of hydra.scala.syntax.NameData
nameDataValue :: Typed.TypedTerm Syntax.NameData -> Typed.TypedTerm Syntax.PredefString
nameDataValue x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.NameData"),
        Core.projectionFieldName = (Core.Name "value")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the value field of hydra.scala.syntax.NameData
nameDataWithValue :: Typed.TypedTerm Syntax.NameData -> Typed.TypedTerm Syntax.PredefString -> Typed.TypedTerm Syntax.NameData
nameDataWithValue original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.NameData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.scala.syntax.NameImportee
nameImportee :: Typed.TypedTerm Syntax.Name -> Typed.TypedTerm Syntax.NameImportee
nameImportee name =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.NameImportee"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm name)}]}))
-- | DSL accessor for the name field of hydra.scala.syntax.NameImportee
nameImporteeName :: Typed.TypedTerm Syntax.NameImportee -> Typed.TypedTerm Syntax.Name
nameImporteeName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.NameImportee"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the name field of hydra.scala.syntax.NameImportee
nameImporteeWithName :: Typed.TypedTerm Syntax.NameImportee -> Typed.TypedTerm Syntax.Name -> Typed.TypedTerm Syntax.NameImportee
nameImporteeWithName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.NameImportee"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL injection for the indeterminate variant of hydra.scala.syntax.Name
nameIndeterminate :: Typed.TypedTerm Syntax.PredefString -> Typed.TypedTerm Syntax.Name
nameIndeterminate x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Name"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "indeterminate"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for hydra.scala.syntax.NameType
nameType :: Typed.TypedTerm String -> Typed.TypedTerm Syntax.NameType
nameType value =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.NameType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Typed.unTypedTerm value)}]}))
-- | DSL accessor for the value field of hydra.scala.syntax.NameType
nameTypeValue :: Typed.TypedTerm Syntax.NameType -> Typed.TypedTerm String
nameTypeValue x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.NameType"),
        Core.projectionFieldName = (Core.Name "value")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the value field of hydra.scala.syntax.NameType
nameTypeWithValue :: Typed.TypedTerm Syntax.NameType -> Typed.TypedTerm String -> Typed.TypedTerm Syntax.NameType
nameTypeWithValue original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.NameType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL injection for the value variant of hydra.scala.syntax.Name
nameValue :: Typed.TypedTerm String -> Typed.TypedTerm Syntax.Name
nameValue x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Name"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "value"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for hydra.scala.syntax.NewAnonymousData
newAnonymousData :: Typed.TypedTerm Syntax.Template -> Typed.TypedTerm Syntax.NewAnonymousData
newAnonymousData templ =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.NewAnonymousData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "templ"),
          Core.fieldTerm = (Typed.unTypedTerm templ)}]}))
-- | DSL accessor for the templ field of hydra.scala.syntax.NewAnonymousData
newAnonymousDataTempl :: Typed.TypedTerm Syntax.NewAnonymousData -> Typed.TypedTerm Syntax.Template
newAnonymousDataTempl x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.NewAnonymousData"),
        Core.projectionFieldName = (Core.Name "templ")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the templ field of hydra.scala.syntax.NewAnonymousData
newAnonymousDataWithTempl :: Typed.TypedTerm Syntax.NewAnonymousData -> Typed.TypedTerm Syntax.Template -> Typed.TypedTerm Syntax.NewAnonymousData
newAnonymousDataWithTempl original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.NewAnonymousData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "templ"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.scala.syntax.NewData
newData :: Typed.TypedTerm Syntax.Init -> Typed.TypedTerm Syntax.NewData
newData init =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.NewData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "init"),
          Core.fieldTerm = (Typed.unTypedTerm init)}]}))
-- | DSL accessor for the init field of hydra.scala.syntax.NewData
newDataInit :: Typed.TypedTerm Syntax.NewData -> Typed.TypedTerm Syntax.Init
newDataInit x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.NewData"),
        Core.projectionFieldName = (Core.Name "init")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the init field of hydra.scala.syntax.NewData
newDataWithInit :: Typed.TypedTerm Syntax.NewData -> Typed.TypedTerm Syntax.Init -> Typed.TypedTerm Syntax.NewData
newDataWithInit original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.NewData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "init"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.scala.syntax.ObjectDefn
objectDefn :: Typed.TypedTerm Syntax.NameData -> Typed.TypedTerm Syntax.ObjectDefn
objectDefn name =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ObjectDefn"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm name)}]}))
-- | DSL accessor for the name field of hydra.scala.syntax.ObjectDefn
objectDefnName :: Typed.TypedTerm Syntax.ObjectDefn -> Typed.TypedTerm Syntax.NameData
objectDefnName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ObjectDefn"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the name field of hydra.scala.syntax.ObjectDefn
objectDefnWithName :: Typed.TypedTerm Syntax.ObjectDefn -> Typed.TypedTerm Syntax.NameData -> Typed.TypedTerm Syntax.ObjectDefn
objectDefnWithName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ObjectDefn"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.scala.syntax.ObjectPkg
objectPkg :: Typed.TypedTerm [Syntax.Mod] -> Typed.TypedTerm Syntax.NameData -> Typed.TypedTerm Syntax.Template -> Typed.TypedTerm Syntax.ObjectPkg
objectPkg mods name template =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ObjectPkg"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Typed.unTypedTerm mods)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "template"),
          Core.fieldTerm = (Typed.unTypedTerm template)}]}))
-- | DSL accessor for the mods field of hydra.scala.syntax.ObjectPkg
objectPkgMods :: Typed.TypedTerm Syntax.ObjectPkg -> Typed.TypedTerm [Syntax.Mod]
objectPkgMods x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ObjectPkg"),
        Core.projectionFieldName = (Core.Name "mods")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the name field of hydra.scala.syntax.ObjectPkg
objectPkgName :: Typed.TypedTerm Syntax.ObjectPkg -> Typed.TypedTerm Syntax.NameData
objectPkgName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ObjectPkg"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the template field of hydra.scala.syntax.ObjectPkg
objectPkgTemplate :: Typed.TypedTerm Syntax.ObjectPkg -> Typed.TypedTerm Syntax.Template
objectPkgTemplate x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ObjectPkg"),
        Core.projectionFieldName = (Core.Name "template")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the mods field of hydra.scala.syntax.ObjectPkg
objectPkgWithMods :: Typed.TypedTerm Syntax.ObjectPkg -> Typed.TypedTerm [Syntax.Mod] -> Typed.TypedTerm Syntax.ObjectPkg
objectPkgWithMods original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ObjectPkg"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ObjectPkg"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "template"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ObjectPkg"),
              Core.projectionFieldName = (Core.Name "template")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the name field of hydra.scala.syntax.ObjectPkg
objectPkgWithName :: Typed.TypedTerm Syntax.ObjectPkg -> Typed.TypedTerm Syntax.NameData -> Typed.TypedTerm Syntax.ObjectPkg
objectPkgWithName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ObjectPkg"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ObjectPkg"),
              Core.projectionFieldName = (Core.Name "mods")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "template"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ObjectPkg"),
              Core.projectionFieldName = (Core.Name "template")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the template field of hydra.scala.syntax.ObjectPkg
objectPkgWithTemplate :: Typed.TypedTerm Syntax.ObjectPkg -> Typed.TypedTerm Syntax.Template -> Typed.TypedTerm Syntax.ObjectPkg
objectPkgWithTemplate original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ObjectPkg"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ObjectPkg"),
              Core.projectionFieldName = (Core.Name "mods")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ObjectPkg"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "template"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.scala.syntax.OrType
orType :: Typed.TypedTerm Syntax.Type -> Typed.TypedTerm Syntax.Type -> Typed.TypedTerm Syntax.OrType
orType lhs rhs =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.OrType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Typed.unTypedTerm lhs)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Typed.unTypedTerm rhs)}]}))
-- | DSL accessor for the lhs field of hydra.scala.syntax.OrType
orTypeLhs :: Typed.TypedTerm Syntax.OrType -> Typed.TypedTerm Syntax.Type
orTypeLhs x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.OrType"),
        Core.projectionFieldName = (Core.Name "lhs")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the rhs field of hydra.scala.syntax.OrType
orTypeRhs :: Typed.TypedTerm Syntax.OrType -> Typed.TypedTerm Syntax.Type
orTypeRhs x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.OrType"),
        Core.projectionFieldName = (Core.Name "rhs")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the lhs field of hydra.scala.syntax.OrType
orTypeWithLhs :: Typed.TypedTerm Syntax.OrType -> Typed.TypedTerm Syntax.Type -> Typed.TypedTerm Syntax.OrType
orTypeWithLhs original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.OrType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.OrType"),
              Core.projectionFieldName = (Core.Name "rhs")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the rhs field of hydra.scala.syntax.OrType
orTypeWithRhs :: Typed.TypedTerm Syntax.OrType -> Typed.TypedTerm Syntax.Type -> Typed.TypedTerm Syntax.OrType
orTypeWithRhs original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.OrType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.OrType"),
              Core.projectionFieldName = (Core.Name "lhs")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.scala.syntax.ParamData
paramData :: Typed.TypedTerm [Syntax.Mod] -> Typed.TypedTerm Syntax.Name -> Typed.TypedTerm (Maybe Syntax.Type) -> Typed.TypedTerm (Maybe Syntax.Data) -> Typed.TypedTerm Syntax.ParamData
paramData mods name decltpe default_ =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ParamData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Typed.unTypedTerm mods)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Typed.unTypedTerm decltpe)},
        Core.Field {
          Core.fieldName = (Core.Name "default"),
          Core.fieldTerm = (Typed.unTypedTerm default_)}]}))
-- | DSL accessor for the decltpe field of hydra.scala.syntax.ParamData
paramDataDecltpe :: Typed.TypedTerm Syntax.ParamData -> Typed.TypedTerm (Maybe Syntax.Type)
paramDataDecltpe x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ParamData"),
        Core.projectionFieldName = (Core.Name "decltpe")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the default field of hydra.scala.syntax.ParamData
paramDataDefault :: Typed.TypedTerm Syntax.ParamData -> Typed.TypedTerm (Maybe Syntax.Data)
paramDataDefault x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ParamData"),
        Core.projectionFieldName = (Core.Name "default")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the mods field of hydra.scala.syntax.ParamData
paramDataMods :: Typed.TypedTerm Syntax.ParamData -> Typed.TypedTerm [Syntax.Mod]
paramDataMods x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ParamData"),
        Core.projectionFieldName = (Core.Name "mods")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the name field of hydra.scala.syntax.ParamData
paramDataName :: Typed.TypedTerm Syntax.ParamData -> Typed.TypedTerm Syntax.Name
paramDataName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ParamData"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the decltpe field of hydra.scala.syntax.ParamData
paramDataWithDecltpe :: Typed.TypedTerm Syntax.ParamData -> Typed.TypedTerm (Maybe Syntax.Type) -> Typed.TypedTerm Syntax.ParamData
paramDataWithDecltpe original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ParamData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ParamData"),
              Core.projectionFieldName = (Core.Name "mods")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ParamData"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "default"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ParamData"),
              Core.projectionFieldName = (Core.Name "default")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the default field of hydra.scala.syntax.ParamData
paramDataWithDefault :: Typed.TypedTerm Syntax.ParamData -> Typed.TypedTerm (Maybe Syntax.Data) -> Typed.TypedTerm Syntax.ParamData
paramDataWithDefault original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ParamData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ParamData"),
              Core.projectionFieldName = (Core.Name "mods")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ParamData"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ParamData"),
              Core.projectionFieldName = (Core.Name "decltpe")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "default"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the mods field of hydra.scala.syntax.ParamData
paramDataWithMods :: Typed.TypedTerm Syntax.ParamData -> Typed.TypedTerm [Syntax.Mod] -> Typed.TypedTerm Syntax.ParamData
paramDataWithMods original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ParamData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ParamData"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ParamData"),
              Core.projectionFieldName = (Core.Name "decltpe")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "default"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ParamData"),
              Core.projectionFieldName = (Core.Name "default")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the name field of hydra.scala.syntax.ParamData
paramDataWithName :: Typed.TypedTerm Syntax.ParamData -> Typed.TypedTerm Syntax.Name -> Typed.TypedTerm Syntax.ParamData
paramDataWithName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ParamData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ParamData"),
              Core.projectionFieldName = (Core.Name "mods")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ParamData"),
              Core.projectionFieldName = (Core.Name "decltpe")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "default"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ParamData"),
              Core.projectionFieldName = (Core.Name "default")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.scala.syntax.ParamType
paramType :: Typed.TypedTerm [Syntax.Mod] -> Typed.TypedTerm Syntax.Name -> Typed.TypedTerm [Syntax.ParamType] -> Typed.TypedTerm [Syntax.TypeBounds] -> Typed.TypedTerm [Syntax.Type] -> Typed.TypedTerm [Syntax.Type] -> Typed.TypedTerm Syntax.ParamType
paramType mods name tparams tbounds vbounds cbounds =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ParamType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Typed.unTypedTerm mods)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Typed.unTypedTerm tparams)},
        Core.Field {
          Core.fieldName = (Core.Name "tbounds"),
          Core.fieldTerm = (Typed.unTypedTerm tbounds)},
        Core.Field {
          Core.fieldName = (Core.Name "vbounds"),
          Core.fieldTerm = (Typed.unTypedTerm vbounds)},
        Core.Field {
          Core.fieldName = (Core.Name "cbounds"),
          Core.fieldTerm = (Typed.unTypedTerm cbounds)}]}))
-- | DSL accessor for the cbounds field of hydra.scala.syntax.ParamType
paramTypeCbounds :: Typed.TypedTerm Syntax.ParamType -> Typed.TypedTerm [Syntax.Type]
paramTypeCbounds x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ParamType"),
        Core.projectionFieldName = (Core.Name "cbounds")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the mods field of hydra.scala.syntax.ParamType
paramTypeMods :: Typed.TypedTerm Syntax.ParamType -> Typed.TypedTerm [Syntax.Mod]
paramTypeMods x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ParamType"),
        Core.projectionFieldName = (Core.Name "mods")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the name field of hydra.scala.syntax.ParamType
paramTypeName :: Typed.TypedTerm Syntax.ParamType -> Typed.TypedTerm Syntax.Name
paramTypeName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ParamType"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the tbounds field of hydra.scala.syntax.ParamType
paramTypeTbounds :: Typed.TypedTerm Syntax.ParamType -> Typed.TypedTerm [Syntax.TypeBounds]
paramTypeTbounds x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ParamType"),
        Core.projectionFieldName = (Core.Name "tbounds")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the tparams field of hydra.scala.syntax.ParamType
paramTypeTparams :: Typed.TypedTerm Syntax.ParamType -> Typed.TypedTerm [Syntax.ParamType]
paramTypeTparams x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ParamType"),
        Core.projectionFieldName = (Core.Name "tparams")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the vbounds field of hydra.scala.syntax.ParamType
paramTypeVbounds :: Typed.TypedTerm Syntax.ParamType -> Typed.TypedTerm [Syntax.Type]
paramTypeVbounds x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ParamType"),
        Core.projectionFieldName = (Core.Name "vbounds")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the cbounds field of hydra.scala.syntax.ParamType
paramTypeWithCbounds :: Typed.TypedTerm Syntax.ParamType -> Typed.TypedTerm [Syntax.Type] -> Typed.TypedTerm Syntax.ParamType
paramTypeWithCbounds original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ParamType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ParamType"),
              Core.projectionFieldName = (Core.Name "mods")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ParamType"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ParamType"),
              Core.projectionFieldName = (Core.Name "tparams")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tbounds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ParamType"),
              Core.projectionFieldName = (Core.Name "tbounds")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "vbounds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ParamType"),
              Core.projectionFieldName = (Core.Name "vbounds")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "cbounds"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the mods field of hydra.scala.syntax.ParamType
paramTypeWithMods :: Typed.TypedTerm Syntax.ParamType -> Typed.TypedTerm [Syntax.Mod] -> Typed.TypedTerm Syntax.ParamType
paramTypeWithMods original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ParamType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ParamType"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ParamType"),
              Core.projectionFieldName = (Core.Name "tparams")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tbounds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ParamType"),
              Core.projectionFieldName = (Core.Name "tbounds")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "vbounds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ParamType"),
              Core.projectionFieldName = (Core.Name "vbounds")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "cbounds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ParamType"),
              Core.projectionFieldName = (Core.Name "cbounds")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the name field of hydra.scala.syntax.ParamType
paramTypeWithName :: Typed.TypedTerm Syntax.ParamType -> Typed.TypedTerm Syntax.Name -> Typed.TypedTerm Syntax.ParamType
paramTypeWithName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ParamType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ParamType"),
              Core.projectionFieldName = (Core.Name "mods")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ParamType"),
              Core.projectionFieldName = (Core.Name "tparams")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tbounds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ParamType"),
              Core.projectionFieldName = (Core.Name "tbounds")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "vbounds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ParamType"),
              Core.projectionFieldName = (Core.Name "vbounds")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "cbounds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ParamType"),
              Core.projectionFieldName = (Core.Name "cbounds")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the tbounds field of hydra.scala.syntax.ParamType
paramTypeWithTbounds :: Typed.TypedTerm Syntax.ParamType -> Typed.TypedTerm [Syntax.TypeBounds] -> Typed.TypedTerm Syntax.ParamType
paramTypeWithTbounds original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ParamType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ParamType"),
              Core.projectionFieldName = (Core.Name "mods")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ParamType"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ParamType"),
              Core.projectionFieldName = (Core.Name "tparams")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tbounds"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "vbounds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ParamType"),
              Core.projectionFieldName = (Core.Name "vbounds")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "cbounds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ParamType"),
              Core.projectionFieldName = (Core.Name "cbounds")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the tparams field of hydra.scala.syntax.ParamType
paramTypeWithTparams :: Typed.TypedTerm Syntax.ParamType -> Typed.TypedTerm [Syntax.ParamType] -> Typed.TypedTerm Syntax.ParamType
paramTypeWithTparams original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ParamType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ParamType"),
              Core.projectionFieldName = (Core.Name "mods")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ParamType"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "tbounds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ParamType"),
              Core.projectionFieldName = (Core.Name "tbounds")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "vbounds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ParamType"),
              Core.projectionFieldName = (Core.Name "vbounds")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "cbounds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ParamType"),
              Core.projectionFieldName = (Core.Name "cbounds")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the vbounds field of hydra.scala.syntax.ParamType
paramTypeWithVbounds :: Typed.TypedTerm Syntax.ParamType -> Typed.TypedTerm [Syntax.Type] -> Typed.TypedTerm Syntax.ParamType
paramTypeWithVbounds original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ParamType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ParamType"),
              Core.projectionFieldName = (Core.Name "mods")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ParamType"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ParamType"),
              Core.projectionFieldName = (Core.Name "tparams")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tbounds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ParamType"),
              Core.projectionFieldName = (Core.Name "tbounds")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "vbounds"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "cbounds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ParamType"),
              Core.projectionFieldName = (Core.Name "cbounds")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.scala.syntax.PartialFunctionData
partialFunctionData :: Typed.TypedTerm [Syntax.Case] -> Typed.TypedTerm Syntax.PartialFunctionData
partialFunctionData cases =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.PartialFunctionData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "cases"),
          Core.fieldTerm = (Typed.unTypedTerm cases)}]}))
-- | DSL accessor for the cases field of hydra.scala.syntax.PartialFunctionData
partialFunctionDataCases :: Typed.TypedTerm Syntax.PartialFunctionData -> Typed.TypedTerm [Syntax.Case]
partialFunctionDataCases x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.PartialFunctionData"),
        Core.projectionFieldName = (Core.Name "cases")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the cases field of hydra.scala.syntax.PartialFunctionData
partialFunctionDataWithCases :: Typed.TypedTerm Syntax.PartialFunctionData -> Typed.TypedTerm [Syntax.Case] -> Typed.TypedTerm Syntax.PartialFunctionData
partialFunctionDataWithCases original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.PartialFunctionData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "cases"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL injection for the alternative variant of hydra.scala.syntax.Pat
patAlternative :: Typed.TypedTerm Syntax.AlternativePat -> Typed.TypedTerm Syntax.Pat
patAlternative x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Pat"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "alternative"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the bind variant of hydra.scala.syntax.Pat
patBind :: Typed.TypedTerm Syntax.BindPat -> Typed.TypedTerm Syntax.Pat
patBind x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Pat"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "bind"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the extract variant of hydra.scala.syntax.Pat
patExtract :: Typed.TypedTerm Syntax.ExtractPat -> Typed.TypedTerm Syntax.Pat
patExtract x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Pat"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "extract"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the extractInfix variant of hydra.scala.syntax.Pat
patExtractInfix :: Typed.TypedTerm Syntax.ExtractInfixPat -> Typed.TypedTerm Syntax.Pat
patExtractInfix x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Pat"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "extractInfix"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the given variant of hydra.scala.syntax.Pat
patGiven :: Typed.TypedTerm Syntax.GivenPat -> Typed.TypedTerm Syntax.Pat
patGiven x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Pat"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "given"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the interpolate variant of hydra.scala.syntax.Pat
patInterpolate :: Typed.TypedTerm Syntax.InterpolatePat -> Typed.TypedTerm Syntax.Pat
patInterpolate x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Pat"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "interpolate"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the repeated variant of hydra.scala.syntax.Pat
patRepeated :: Typed.TypedTerm Syntax.RepeatedPat -> Typed.TypedTerm Syntax.Pat
patRepeated x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Pat"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "repeated"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the seqWildcard variant of hydra.scala.syntax.Pat
patSeqWildcard :: Typed.TypedTerm Syntax.Pat
patSeqWildcard =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Pat"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "seqWildcard"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the tuple variant of hydra.scala.syntax.Pat
patTuple :: Typed.TypedTerm Syntax.TuplePat -> Typed.TypedTerm Syntax.Pat
patTuple x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Pat"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "tuple"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the typed variant of hydra.scala.syntax.Pat
patTyped :: Typed.TypedTerm Syntax.TypedPat -> Typed.TypedTerm Syntax.Pat
patTyped x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Pat"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "typed"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the var variant of hydra.scala.syntax.Pat
patVar :: Typed.TypedTerm Syntax.VarPat -> Typed.TypedTerm Syntax.Pat
patVar x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Pat"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "var"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the wildcard variant of hydra.scala.syntax.Pat
patWildcard :: Typed.TypedTerm Syntax.Pat
patWildcard =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Pat"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "wildcard"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL constructor for hydra.scala.syntax.Pkg
pkg :: Typed.TypedTerm Syntax.NameData -> Typed.TypedTerm Syntax.RefData -> Typed.TypedTerm [Syntax.Stat] -> Typed.TypedTerm Syntax.Pkg
pkg name ref stats =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.Pkg"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "ref"),
          Core.fieldTerm = (Typed.unTypedTerm ref)},
        Core.Field {
          Core.fieldName = (Core.Name "stats"),
          Core.fieldTerm = (Typed.unTypedTerm stats)}]}))
-- | DSL accessor for the name field of hydra.scala.syntax.Pkg
pkgName :: Typed.TypedTerm Syntax.Pkg -> Typed.TypedTerm Syntax.NameData
pkgName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.Pkg"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the ref field of hydra.scala.syntax.Pkg
pkgRef :: Typed.TypedTerm Syntax.Pkg -> Typed.TypedTerm Syntax.RefData
pkgRef x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.Pkg"),
        Core.projectionFieldName = (Core.Name "ref")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the stats field of hydra.scala.syntax.Pkg
pkgStats :: Typed.TypedTerm Syntax.Pkg -> Typed.TypedTerm [Syntax.Stat]
pkgStats x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.Pkg"),
        Core.projectionFieldName = (Core.Name "stats")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the name field of hydra.scala.syntax.Pkg
pkgWithName :: Typed.TypedTerm Syntax.Pkg -> Typed.TypedTerm Syntax.NameData -> Typed.TypedTerm Syntax.Pkg
pkgWithName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.Pkg"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "ref"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.Pkg"),
              Core.projectionFieldName = (Core.Name "ref")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "stats"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.Pkg"),
              Core.projectionFieldName = (Core.Name "stats")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the ref field of hydra.scala.syntax.Pkg
pkgWithRef :: Typed.TypedTerm Syntax.Pkg -> Typed.TypedTerm Syntax.RefData -> Typed.TypedTerm Syntax.Pkg
pkgWithRef original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.Pkg"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.Pkg"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ref"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "stats"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.Pkg"),
              Core.projectionFieldName = (Core.Name "stats")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the stats field of hydra.scala.syntax.Pkg
pkgWithStats :: Typed.TypedTerm Syntax.Pkg -> Typed.TypedTerm [Syntax.Stat] -> Typed.TypedTerm Syntax.Pkg
pkgWithStats original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.Pkg"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.Pkg"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ref"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.Pkg"),
              Core.projectionFieldName = (Core.Name "ref")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "stats"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.scala.syntax.PlaceholderType
placeholderType :: Typed.TypedTerm Syntax.TypeBounds -> Typed.TypedTerm Syntax.PlaceholderType
placeholderType bounds =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.PlaceholderType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "bounds"),
          Core.fieldTerm = (Typed.unTypedTerm bounds)}]}))
-- | DSL accessor for the bounds field of hydra.scala.syntax.PlaceholderType
placeholderTypeBounds :: Typed.TypedTerm Syntax.PlaceholderType -> Typed.TypedTerm Syntax.TypeBounds
placeholderTypeBounds x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.PlaceholderType"),
        Core.projectionFieldName = (Core.Name "bounds")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the bounds field of hydra.scala.syntax.PlaceholderType
placeholderTypeWithBounds :: Typed.TypedTerm Syntax.PlaceholderType -> Typed.TypedTerm Syntax.TypeBounds -> Typed.TypedTerm Syntax.PlaceholderType
placeholderTypeWithBounds original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.PlaceholderType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "bounds"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.scala.syntax.PolyFunctionData
polyFunctionData :: Typed.TypedTerm [Syntax.ParamType] -> Typed.TypedTerm Syntax.Data -> Typed.TypedTerm Syntax.PolyFunctionData
polyFunctionData tparams body =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.PolyFunctionData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Typed.unTypedTerm tparams)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm body)}]}))
-- | DSL accessor for the body field of hydra.scala.syntax.PolyFunctionData
polyFunctionDataBody :: Typed.TypedTerm Syntax.PolyFunctionData -> Typed.TypedTerm Syntax.Data
polyFunctionDataBody x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.PolyFunctionData"),
        Core.projectionFieldName = (Core.Name "body")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the tparams field of hydra.scala.syntax.PolyFunctionData
polyFunctionDataTparams :: Typed.TypedTerm Syntax.PolyFunctionData -> Typed.TypedTerm [Syntax.ParamType]
polyFunctionDataTparams x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.PolyFunctionData"),
        Core.projectionFieldName = (Core.Name "tparams")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the body field of hydra.scala.syntax.PolyFunctionData
polyFunctionDataWithBody :: Typed.TypedTerm Syntax.PolyFunctionData -> Typed.TypedTerm Syntax.Data -> Typed.TypedTerm Syntax.PolyFunctionData
polyFunctionDataWithBody original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.PolyFunctionData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.PolyFunctionData"),
              Core.projectionFieldName = (Core.Name "tparams")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the tparams field of hydra.scala.syntax.PolyFunctionData
polyFunctionDataWithTparams :: Typed.TypedTerm Syntax.PolyFunctionData -> Typed.TypedTerm [Syntax.ParamType] -> Typed.TypedTerm Syntax.PolyFunctionData
polyFunctionDataWithTparams original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.PolyFunctionData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.PolyFunctionData"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.scala.syntax.PolyFunctionType
polyFunctionType :: Typed.TypedTerm [Syntax.ParamType] -> Typed.TypedTerm Syntax.Type -> Typed.TypedTerm Syntax.PolyFunctionType
polyFunctionType tparams tpe =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.PolyFunctionType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Typed.unTypedTerm tparams)},
        Core.Field {
          Core.fieldName = (Core.Name "tpe"),
          Core.fieldTerm = (Typed.unTypedTerm tpe)}]}))
-- | DSL accessor for the tparams field of hydra.scala.syntax.PolyFunctionType
polyFunctionTypeTparams :: Typed.TypedTerm Syntax.PolyFunctionType -> Typed.TypedTerm [Syntax.ParamType]
polyFunctionTypeTparams x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.PolyFunctionType"),
        Core.projectionFieldName = (Core.Name "tparams")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the tpe field of hydra.scala.syntax.PolyFunctionType
polyFunctionTypeTpe :: Typed.TypedTerm Syntax.PolyFunctionType -> Typed.TypedTerm Syntax.Type
polyFunctionTypeTpe x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.PolyFunctionType"),
        Core.projectionFieldName = (Core.Name "tpe")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the tparams field of hydra.scala.syntax.PolyFunctionType
polyFunctionTypeWithTparams :: Typed.TypedTerm Syntax.PolyFunctionType -> Typed.TypedTerm [Syntax.ParamType] -> Typed.TypedTerm Syntax.PolyFunctionType
polyFunctionTypeWithTparams original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.PolyFunctionType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "tpe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.PolyFunctionType"),
              Core.projectionFieldName = (Core.Name "tpe")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the tpe field of hydra.scala.syntax.PolyFunctionType
polyFunctionTypeWithTpe :: Typed.TypedTerm Syntax.PolyFunctionType -> Typed.TypedTerm Syntax.Type -> Typed.TypedTerm Syntax.PolyFunctionType
polyFunctionTypeWithTpe original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.PolyFunctionType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.PolyFunctionType"),
              Core.projectionFieldName = (Core.Name "tparams")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tpe"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for the hydra.scala.syntax.PredefString wrapper
predefString :: Typed.TypedTerm String -> Typed.TypedTerm Syntax.PredefString
predefString x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.scala.syntax.PredefString"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.scala.syntax.PrimaryCtor
primaryCtor :: Typed.TypedTerm [Syntax.Mod] -> Typed.TypedTerm Syntax.Name -> Typed.TypedTerm [[Syntax.ParamData]] -> Typed.TypedTerm Syntax.PrimaryCtor
primaryCtor mods name paramss =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.PrimaryCtor"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Typed.unTypedTerm mods)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "paramss"),
          Core.fieldTerm = (Typed.unTypedTerm paramss)}]}))
-- | DSL accessor for the mods field of hydra.scala.syntax.PrimaryCtor
primaryCtorMods :: Typed.TypedTerm Syntax.PrimaryCtor -> Typed.TypedTerm [Syntax.Mod]
primaryCtorMods x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.PrimaryCtor"),
        Core.projectionFieldName = (Core.Name "mods")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the name field of hydra.scala.syntax.PrimaryCtor
primaryCtorName :: Typed.TypedTerm Syntax.PrimaryCtor -> Typed.TypedTerm Syntax.Name
primaryCtorName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.PrimaryCtor"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the paramss field of hydra.scala.syntax.PrimaryCtor
primaryCtorParamss :: Typed.TypedTerm Syntax.PrimaryCtor -> Typed.TypedTerm [[Syntax.ParamData]]
primaryCtorParamss x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.PrimaryCtor"),
        Core.projectionFieldName = (Core.Name "paramss")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the mods field of hydra.scala.syntax.PrimaryCtor
primaryCtorWithMods :: Typed.TypedTerm Syntax.PrimaryCtor -> Typed.TypedTerm [Syntax.Mod] -> Typed.TypedTerm Syntax.PrimaryCtor
primaryCtorWithMods original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.PrimaryCtor"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.PrimaryCtor"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "paramss"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.PrimaryCtor"),
              Core.projectionFieldName = (Core.Name "paramss")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the name field of hydra.scala.syntax.PrimaryCtor
primaryCtorWithName :: Typed.TypedTerm Syntax.PrimaryCtor -> Typed.TypedTerm Syntax.Name -> Typed.TypedTerm Syntax.PrimaryCtor
primaryCtorWithName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.PrimaryCtor"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.PrimaryCtor"),
              Core.projectionFieldName = (Core.Name "mods")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "paramss"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.PrimaryCtor"),
              Core.projectionFieldName = (Core.Name "paramss")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the paramss field of hydra.scala.syntax.PrimaryCtor
primaryCtorWithParamss :: Typed.TypedTerm Syntax.PrimaryCtor -> Typed.TypedTerm [[Syntax.ParamData]] -> Typed.TypedTerm Syntax.PrimaryCtor
primaryCtorWithParamss original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.PrimaryCtor"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.PrimaryCtor"),
              Core.projectionFieldName = (Core.Name "mods")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.PrimaryCtor"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "paramss"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.scala.syntax.PrivateMod
privateMod :: Typed.TypedTerm Syntax.Ref -> Typed.TypedTerm Syntax.PrivateMod
privateMod within =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.PrivateMod"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "within"),
          Core.fieldTerm = (Typed.unTypedTerm within)}]}))
-- | DSL updater for the within field of hydra.scala.syntax.PrivateMod
privateModWithWithin :: Typed.TypedTerm Syntax.PrivateMod -> Typed.TypedTerm Syntax.Ref -> Typed.TypedTerm Syntax.PrivateMod
privateModWithWithin original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.PrivateMod"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "within"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL accessor for the within field of hydra.scala.syntax.PrivateMod
privateModWithin :: Typed.TypedTerm Syntax.PrivateMod -> Typed.TypedTerm Syntax.Ref
privateModWithin x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.PrivateMod"),
        Core.projectionFieldName = (Core.Name "within")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.scala.syntax.ProjectType
projectType :: Typed.TypedTerm Syntax.Type -> Typed.TypedTerm Syntax.NameType -> Typed.TypedTerm Syntax.ProjectType
projectType qual name =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ProjectType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "qual"),
          Core.fieldTerm = (Typed.unTypedTerm qual)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm name)}]}))
-- | DSL accessor for the name field of hydra.scala.syntax.ProjectType
projectTypeName :: Typed.TypedTerm Syntax.ProjectType -> Typed.TypedTerm Syntax.NameType
projectTypeName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ProjectType"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the qual field of hydra.scala.syntax.ProjectType
projectTypeQual :: Typed.TypedTerm Syntax.ProjectType -> Typed.TypedTerm Syntax.Type
projectTypeQual x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ProjectType"),
        Core.projectionFieldName = (Core.Name "qual")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the name field of hydra.scala.syntax.ProjectType
projectTypeWithName :: Typed.TypedTerm Syntax.ProjectType -> Typed.TypedTerm Syntax.NameType -> Typed.TypedTerm Syntax.ProjectType
projectTypeWithName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ProjectType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "qual"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ProjectType"),
              Core.projectionFieldName = (Core.Name "qual")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the qual field of hydra.scala.syntax.ProjectType
projectTypeWithQual :: Typed.TypedTerm Syntax.ProjectType -> Typed.TypedTerm Syntax.Type -> Typed.TypedTerm Syntax.ProjectType
projectTypeWithQual original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ProjectType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "qual"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ProjectType"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.scala.syntax.ProtectedMod
protectedMod :: Typed.TypedTerm Syntax.Ref -> Typed.TypedTerm Syntax.ProtectedMod
protectedMod within =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ProtectedMod"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "within"),
          Core.fieldTerm = (Typed.unTypedTerm within)}]}))
-- | DSL updater for the within field of hydra.scala.syntax.ProtectedMod
protectedModWithWithin :: Typed.TypedTerm Syntax.ProtectedMod -> Typed.TypedTerm Syntax.Ref -> Typed.TypedTerm Syntax.ProtectedMod
protectedModWithWithin original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ProtectedMod"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "within"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL accessor for the within field of hydra.scala.syntax.ProtectedMod
protectedModWithin :: Typed.TypedTerm Syntax.ProtectedMod -> Typed.TypedTerm Syntax.Ref
protectedModWithin x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ProtectedMod"),
        Core.projectionFieldName = (Core.Name "within")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL injection for the anonymous variant of hydra.scala.syntax.RefData
refDataAnonymous :: Typed.TypedTerm Syntax.AnonymousData -> Typed.TypedTerm Syntax.RefData
refDataAnonymous x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.RefData"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "anonymous"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the applyUnary variant of hydra.scala.syntax.RefData
refDataApplyUnary :: Typed.TypedTerm Syntax.ApplyUnaryData -> Typed.TypedTerm Syntax.RefData
refDataApplyUnary x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.RefData"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "applyUnary"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the name variant of hydra.scala.syntax.RefData
refDataName :: Typed.TypedTerm Syntax.NameData -> Typed.TypedTerm Syntax.RefData
refDataName x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.RefData"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "name"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the select variant of hydra.scala.syntax.RefData
refDataSelect :: Typed.TypedTerm Syntax.SelectData -> Typed.TypedTerm Syntax.RefData
refDataSelect x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.RefData"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "select"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the super variant of hydra.scala.syntax.RefData
refDataSuper :: Typed.TypedTerm Syntax.SuperData -> Typed.TypedTerm Syntax.RefData
refDataSuper x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.RefData"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "super"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the this variant of hydra.scala.syntax.RefData
refDataThis :: Typed.TypedTerm Syntax.ThisData -> Typed.TypedTerm Syntax.RefData
refDataThis x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.RefData"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "this"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the init variant of hydra.scala.syntax.Ref
refInit :: Typed.TypedTerm Syntax.Init -> Typed.TypedTerm Syntax.Ref
refInit x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Ref"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "init"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the name variant of hydra.scala.syntax.Ref
refName :: Typed.TypedTerm Syntax.Name -> Typed.TypedTerm Syntax.Ref
refName x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Ref"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "name"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the name variant of hydra.scala.syntax.RefType
refTypeName :: Typed.TypedTerm Syntax.NameType -> Typed.TypedTerm Syntax.RefType
refTypeName x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.RefType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "name"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the project variant of hydra.scala.syntax.RefType
refTypeProject :: Typed.TypedTerm Syntax.ProjectType -> Typed.TypedTerm Syntax.RefType
refTypeProject x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.RefType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "project"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the select variant of hydra.scala.syntax.RefType
refTypeSelect :: Typed.TypedTerm Syntax.SelectType -> Typed.TypedTerm Syntax.RefType
refTypeSelect x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.RefType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "select"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the singleton variant of hydra.scala.syntax.RefType
refTypeSingleton :: Typed.TypedTerm Syntax.SingletonType -> Typed.TypedTerm Syntax.RefType
refTypeSingleton x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.RefType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "singleton"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for hydra.scala.syntax.RefineType
refineType :: Typed.TypedTerm (Maybe Syntax.Type) -> Typed.TypedTerm [Syntax.Stat] -> Typed.TypedTerm Syntax.RefineType
refineType tpe stats =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.RefineType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "tpe"),
          Core.fieldTerm = (Typed.unTypedTerm tpe)},
        Core.Field {
          Core.fieldName = (Core.Name "stats"),
          Core.fieldTerm = (Typed.unTypedTerm stats)}]}))
-- | DSL accessor for the stats field of hydra.scala.syntax.RefineType
refineTypeStats :: Typed.TypedTerm Syntax.RefineType -> Typed.TypedTerm [Syntax.Stat]
refineTypeStats x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.RefineType"),
        Core.projectionFieldName = (Core.Name "stats")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the tpe field of hydra.scala.syntax.RefineType
refineTypeTpe :: Typed.TypedTerm Syntax.RefineType -> Typed.TypedTerm (Maybe Syntax.Type)
refineTypeTpe x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.RefineType"),
        Core.projectionFieldName = (Core.Name "tpe")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the stats field of hydra.scala.syntax.RefineType
refineTypeWithStats :: Typed.TypedTerm Syntax.RefineType -> Typed.TypedTerm [Syntax.Stat] -> Typed.TypedTerm Syntax.RefineType
refineTypeWithStats original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.RefineType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "tpe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.RefineType"),
              Core.projectionFieldName = (Core.Name "tpe")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "stats"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the tpe field of hydra.scala.syntax.RefineType
refineTypeWithTpe :: Typed.TypedTerm Syntax.RefineType -> Typed.TypedTerm (Maybe Syntax.Type) -> Typed.TypedTerm Syntax.RefineType
refineTypeWithTpe original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.RefineType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "tpe"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "stats"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.RefineType"),
              Core.projectionFieldName = (Core.Name "stats")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.scala.syntax.RenameImportee
renameImportee :: Typed.TypedTerm Syntax.Name -> Typed.TypedTerm Syntax.Name -> Typed.TypedTerm Syntax.RenameImportee
renameImportee name rename =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.RenameImportee"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "rename"),
          Core.fieldTerm = (Typed.unTypedTerm rename)}]}))
-- | DSL accessor for the name field of hydra.scala.syntax.RenameImportee
renameImporteeName :: Typed.TypedTerm Syntax.RenameImportee -> Typed.TypedTerm Syntax.Name
renameImporteeName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.RenameImportee"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the rename field of hydra.scala.syntax.RenameImportee
renameImporteeRename :: Typed.TypedTerm Syntax.RenameImportee -> Typed.TypedTerm Syntax.Name
renameImporteeRename x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.RenameImportee"),
        Core.projectionFieldName = (Core.Name "rename")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the name field of hydra.scala.syntax.RenameImportee
renameImporteeWithName :: Typed.TypedTerm Syntax.RenameImportee -> Typed.TypedTerm Syntax.Name -> Typed.TypedTerm Syntax.RenameImportee
renameImporteeWithName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.RenameImportee"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "rename"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.RenameImportee"),
              Core.projectionFieldName = (Core.Name "rename")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the rename field of hydra.scala.syntax.RenameImportee
renameImporteeWithRename :: Typed.TypedTerm Syntax.RenameImportee -> Typed.TypedTerm Syntax.Name -> Typed.TypedTerm Syntax.RenameImportee
renameImporteeWithRename original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.RenameImportee"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.RenameImportee"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rename"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.scala.syntax.RepeatedData
repeatedData :: Typed.TypedTerm Syntax.Data -> Typed.TypedTerm Syntax.RepeatedData
repeatedData expr =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.RepeatedData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expr"),
          Core.fieldTerm = (Typed.unTypedTerm expr)}]}))
-- | DSL accessor for the expr field of hydra.scala.syntax.RepeatedData
repeatedDataExpr :: Typed.TypedTerm Syntax.RepeatedData -> Typed.TypedTerm Syntax.Data
repeatedDataExpr x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.RepeatedData"),
        Core.projectionFieldName = (Core.Name "expr")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the expr field of hydra.scala.syntax.RepeatedData
repeatedDataWithExpr :: Typed.TypedTerm Syntax.RepeatedData -> Typed.TypedTerm Syntax.Data -> Typed.TypedTerm Syntax.RepeatedData
repeatedDataWithExpr original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.RepeatedData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expr"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.scala.syntax.RepeatedEnumCaseDefn
repeatedEnumCaseDefn :: Typed.TypedTerm [Syntax.Mod] -> Typed.TypedTerm [Syntax.NameData] -> Typed.TypedTerm Syntax.RepeatedEnumCaseDefn
repeatedEnumCaseDefn mods cases =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.RepeatedEnumCaseDefn"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Typed.unTypedTerm mods)},
        Core.Field {
          Core.fieldName = (Core.Name "cases"),
          Core.fieldTerm = (Typed.unTypedTerm cases)}]}))
-- | DSL accessor for the cases field of hydra.scala.syntax.RepeatedEnumCaseDefn
repeatedEnumCaseDefnCases :: Typed.TypedTerm Syntax.RepeatedEnumCaseDefn -> Typed.TypedTerm [Syntax.NameData]
repeatedEnumCaseDefnCases x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.RepeatedEnumCaseDefn"),
        Core.projectionFieldName = (Core.Name "cases")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the mods field of hydra.scala.syntax.RepeatedEnumCaseDefn
repeatedEnumCaseDefnMods :: Typed.TypedTerm Syntax.RepeatedEnumCaseDefn -> Typed.TypedTerm [Syntax.Mod]
repeatedEnumCaseDefnMods x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.RepeatedEnumCaseDefn"),
        Core.projectionFieldName = (Core.Name "mods")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the cases field of hydra.scala.syntax.RepeatedEnumCaseDefn
repeatedEnumCaseDefnWithCases :: Typed.TypedTerm Syntax.RepeatedEnumCaseDefn -> Typed.TypedTerm [Syntax.NameData] -> Typed.TypedTerm Syntax.RepeatedEnumCaseDefn
repeatedEnumCaseDefnWithCases original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.RepeatedEnumCaseDefn"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.RepeatedEnumCaseDefn"),
              Core.projectionFieldName = (Core.Name "mods")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "cases"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the mods field of hydra.scala.syntax.RepeatedEnumCaseDefn
repeatedEnumCaseDefnWithMods :: Typed.TypedTerm Syntax.RepeatedEnumCaseDefn -> Typed.TypedTerm [Syntax.Mod] -> Typed.TypedTerm Syntax.RepeatedEnumCaseDefn
repeatedEnumCaseDefnWithMods original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.RepeatedEnumCaseDefn"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "cases"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.RepeatedEnumCaseDefn"),
              Core.projectionFieldName = (Core.Name "cases")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.scala.syntax.RepeatedPat
repeatedPat :: Typed.TypedTerm Syntax.NameData -> Typed.TypedTerm Syntax.RepeatedPat
repeatedPat name =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.RepeatedPat"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm name)}]}))
-- | DSL accessor for the name field of hydra.scala.syntax.RepeatedPat
repeatedPatName :: Typed.TypedTerm Syntax.RepeatedPat -> Typed.TypedTerm Syntax.NameData
repeatedPatName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.RepeatedPat"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the name field of hydra.scala.syntax.RepeatedPat
repeatedPatWithName :: Typed.TypedTerm Syntax.RepeatedPat -> Typed.TypedTerm Syntax.NameData -> Typed.TypedTerm Syntax.RepeatedPat
repeatedPatWithName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.RepeatedPat"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.scala.syntax.RepeatedType
repeatedType :: Typed.TypedTerm Syntax.Type -> Typed.TypedTerm Syntax.RepeatedType
repeatedType tpe =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.RepeatedType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "tpe"),
          Core.fieldTerm = (Typed.unTypedTerm tpe)}]}))
-- | DSL accessor for the tpe field of hydra.scala.syntax.RepeatedType
repeatedTypeTpe :: Typed.TypedTerm Syntax.RepeatedType -> Typed.TypedTerm Syntax.Type
repeatedTypeTpe x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.RepeatedType"),
        Core.projectionFieldName = (Core.Name "tpe")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the tpe field of hydra.scala.syntax.RepeatedType
repeatedTypeWithTpe :: Typed.TypedTerm Syntax.RepeatedType -> Typed.TypedTerm Syntax.Type -> Typed.TypedTerm Syntax.RepeatedType
repeatedTypeWithTpe original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.RepeatedType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "tpe"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.scala.syntax.ReturnData
returnData :: Typed.TypedTerm Syntax.Data -> Typed.TypedTerm Syntax.ReturnData
returnData expr =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ReturnData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expr"),
          Core.fieldTerm = (Typed.unTypedTerm expr)}]}))
-- | DSL accessor for the expr field of hydra.scala.syntax.ReturnData
returnDataExpr :: Typed.TypedTerm Syntax.ReturnData -> Typed.TypedTerm Syntax.Data
returnDataExpr x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ReturnData"),
        Core.projectionFieldName = (Core.Name "expr")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the expr field of hydra.scala.syntax.ReturnData
returnDataWithExpr :: Typed.TypedTerm Syntax.ReturnData -> Typed.TypedTerm Syntax.Data -> Typed.TypedTerm Syntax.ReturnData
returnDataWithExpr original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ReturnData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expr"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.scala.syntax.ScalaSymbol
scalaSymbol :: Typed.TypedTerm String -> Typed.TypedTerm Syntax.ScalaSymbol
scalaSymbol name =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ScalaSymbol"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm name)}]}))
-- | DSL accessor for the name field of hydra.scala.syntax.ScalaSymbol
scalaSymbolName :: Typed.TypedTerm Syntax.ScalaSymbol -> Typed.TypedTerm String
scalaSymbolName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ScalaSymbol"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the name field of hydra.scala.syntax.ScalaSymbol
scalaSymbolWithName :: Typed.TypedTerm Syntax.ScalaSymbol -> Typed.TypedTerm String -> Typed.TypedTerm Syntax.ScalaSymbol
scalaSymbolWithName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ScalaSymbol"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.scala.syntax.SecondaryCtor
secondaryCtor :: Typed.TypedTerm [Syntax.Mod] -> Typed.TypedTerm Syntax.Name -> Typed.TypedTerm [[Syntax.ParamData]] -> Typed.TypedTerm Syntax.Init -> Typed.TypedTerm [Syntax.Stat] -> Typed.TypedTerm Syntax.SecondaryCtor
secondaryCtor mods name paramss init stats =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.SecondaryCtor"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Typed.unTypedTerm mods)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "paramss"),
          Core.fieldTerm = (Typed.unTypedTerm paramss)},
        Core.Field {
          Core.fieldName = (Core.Name "init"),
          Core.fieldTerm = (Typed.unTypedTerm init)},
        Core.Field {
          Core.fieldName = (Core.Name "stats"),
          Core.fieldTerm = (Typed.unTypedTerm stats)}]}))
-- | DSL accessor for the init field of hydra.scala.syntax.SecondaryCtor
secondaryCtorInit :: Typed.TypedTerm Syntax.SecondaryCtor -> Typed.TypedTerm Syntax.Init
secondaryCtorInit x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.SecondaryCtor"),
        Core.projectionFieldName = (Core.Name "init")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the mods field of hydra.scala.syntax.SecondaryCtor
secondaryCtorMods :: Typed.TypedTerm Syntax.SecondaryCtor -> Typed.TypedTerm [Syntax.Mod]
secondaryCtorMods x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.SecondaryCtor"),
        Core.projectionFieldName = (Core.Name "mods")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the name field of hydra.scala.syntax.SecondaryCtor
secondaryCtorName :: Typed.TypedTerm Syntax.SecondaryCtor -> Typed.TypedTerm Syntax.Name
secondaryCtorName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.SecondaryCtor"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the paramss field of hydra.scala.syntax.SecondaryCtor
secondaryCtorParamss :: Typed.TypedTerm Syntax.SecondaryCtor -> Typed.TypedTerm [[Syntax.ParamData]]
secondaryCtorParamss x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.SecondaryCtor"),
        Core.projectionFieldName = (Core.Name "paramss")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the stats field of hydra.scala.syntax.SecondaryCtor
secondaryCtorStats :: Typed.TypedTerm Syntax.SecondaryCtor -> Typed.TypedTerm [Syntax.Stat]
secondaryCtorStats x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.SecondaryCtor"),
        Core.projectionFieldName = (Core.Name "stats")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the init field of hydra.scala.syntax.SecondaryCtor
secondaryCtorWithInit :: Typed.TypedTerm Syntax.SecondaryCtor -> Typed.TypedTerm Syntax.Init -> Typed.TypedTerm Syntax.SecondaryCtor
secondaryCtorWithInit original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.SecondaryCtor"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.SecondaryCtor"),
              Core.projectionFieldName = (Core.Name "mods")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.SecondaryCtor"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "paramss"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.SecondaryCtor"),
              Core.projectionFieldName = (Core.Name "paramss")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "init"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "stats"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.SecondaryCtor"),
              Core.projectionFieldName = (Core.Name "stats")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the mods field of hydra.scala.syntax.SecondaryCtor
secondaryCtorWithMods :: Typed.TypedTerm Syntax.SecondaryCtor -> Typed.TypedTerm [Syntax.Mod] -> Typed.TypedTerm Syntax.SecondaryCtor
secondaryCtorWithMods original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.SecondaryCtor"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.SecondaryCtor"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "paramss"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.SecondaryCtor"),
              Core.projectionFieldName = (Core.Name "paramss")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "init"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.SecondaryCtor"),
              Core.projectionFieldName = (Core.Name "init")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "stats"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.SecondaryCtor"),
              Core.projectionFieldName = (Core.Name "stats")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the name field of hydra.scala.syntax.SecondaryCtor
secondaryCtorWithName :: Typed.TypedTerm Syntax.SecondaryCtor -> Typed.TypedTerm Syntax.Name -> Typed.TypedTerm Syntax.SecondaryCtor
secondaryCtorWithName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.SecondaryCtor"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.SecondaryCtor"),
              Core.projectionFieldName = (Core.Name "mods")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "paramss"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.SecondaryCtor"),
              Core.projectionFieldName = (Core.Name "paramss")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "init"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.SecondaryCtor"),
              Core.projectionFieldName = (Core.Name "init")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "stats"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.SecondaryCtor"),
              Core.projectionFieldName = (Core.Name "stats")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the paramss field of hydra.scala.syntax.SecondaryCtor
secondaryCtorWithParamss :: Typed.TypedTerm Syntax.SecondaryCtor -> Typed.TypedTerm [[Syntax.ParamData]] -> Typed.TypedTerm Syntax.SecondaryCtor
secondaryCtorWithParamss original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.SecondaryCtor"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.SecondaryCtor"),
              Core.projectionFieldName = (Core.Name "mods")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.SecondaryCtor"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "paramss"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "init"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.SecondaryCtor"),
              Core.projectionFieldName = (Core.Name "init")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "stats"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.SecondaryCtor"),
              Core.projectionFieldName = (Core.Name "stats")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the stats field of hydra.scala.syntax.SecondaryCtor
secondaryCtorWithStats :: Typed.TypedTerm Syntax.SecondaryCtor -> Typed.TypedTerm [Syntax.Stat] -> Typed.TypedTerm Syntax.SecondaryCtor
secondaryCtorWithStats original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.SecondaryCtor"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.SecondaryCtor"),
              Core.projectionFieldName = (Core.Name "mods")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.SecondaryCtor"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "paramss"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.SecondaryCtor"),
              Core.projectionFieldName = (Core.Name "paramss")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "init"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.SecondaryCtor"),
              Core.projectionFieldName = (Core.Name "init")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "stats"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.scala.syntax.SelectData
selectData :: Typed.TypedTerm Syntax.Data -> Typed.TypedTerm Syntax.NameData -> Typed.TypedTerm Syntax.SelectData
selectData qual name =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.SelectData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "qual"),
          Core.fieldTerm = (Typed.unTypedTerm qual)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm name)}]}))
-- | DSL accessor for the name field of hydra.scala.syntax.SelectData
selectDataName :: Typed.TypedTerm Syntax.SelectData -> Typed.TypedTerm Syntax.NameData
selectDataName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.SelectData"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the qual field of hydra.scala.syntax.SelectData
selectDataQual :: Typed.TypedTerm Syntax.SelectData -> Typed.TypedTerm Syntax.Data
selectDataQual x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.SelectData"),
        Core.projectionFieldName = (Core.Name "qual")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the name field of hydra.scala.syntax.SelectData
selectDataWithName :: Typed.TypedTerm Syntax.SelectData -> Typed.TypedTerm Syntax.NameData -> Typed.TypedTerm Syntax.SelectData
selectDataWithName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.SelectData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "qual"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.SelectData"),
              Core.projectionFieldName = (Core.Name "qual")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the qual field of hydra.scala.syntax.SelectData
selectDataWithQual :: Typed.TypedTerm Syntax.SelectData -> Typed.TypedTerm Syntax.Data -> Typed.TypedTerm Syntax.SelectData
selectDataWithQual original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.SelectData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "qual"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.SelectData"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.scala.syntax.SelectType
selectType :: Typed.TypedTerm Syntax.RefData -> Typed.TypedTerm Syntax.NameType -> Typed.TypedTerm Syntax.SelectType
selectType qual name =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.SelectType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "qual"),
          Core.fieldTerm = (Typed.unTypedTerm qual)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm name)}]}))
-- | DSL accessor for the name field of hydra.scala.syntax.SelectType
selectTypeName :: Typed.TypedTerm Syntax.SelectType -> Typed.TypedTerm Syntax.NameType
selectTypeName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.SelectType"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the qual field of hydra.scala.syntax.SelectType
selectTypeQual :: Typed.TypedTerm Syntax.SelectType -> Typed.TypedTerm Syntax.RefData
selectTypeQual x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.SelectType"),
        Core.projectionFieldName = (Core.Name "qual")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the name field of hydra.scala.syntax.SelectType
selectTypeWithName :: Typed.TypedTerm Syntax.SelectType -> Typed.TypedTerm Syntax.NameType -> Typed.TypedTerm Syntax.SelectType
selectTypeWithName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.SelectType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "qual"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.SelectType"),
              Core.projectionFieldName = (Core.Name "qual")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the qual field of hydra.scala.syntax.SelectType
selectTypeWithQual :: Typed.TypedTerm Syntax.SelectType -> Typed.TypedTerm Syntax.RefData -> Typed.TypedTerm Syntax.SelectType
selectTypeWithQual original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.SelectType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "qual"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.SelectType"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for the hydra.scala.syntax.Self wrapper
self :: Typed.TypedTerm () -> Typed.TypedTerm Syntax.Self
self x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.scala.syntax.Self"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.scala.syntax.SingletonType
singletonType :: Typed.TypedTerm Syntax.RefData -> Typed.TypedTerm Syntax.SingletonType
singletonType ref =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.SingletonType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "ref"),
          Core.fieldTerm = (Typed.unTypedTerm ref)}]}))
-- | DSL accessor for the ref field of hydra.scala.syntax.SingletonType
singletonTypeRef :: Typed.TypedTerm Syntax.SingletonType -> Typed.TypedTerm Syntax.RefData
singletonTypeRef x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.SingletonType"),
        Core.projectionFieldName = (Core.Name "ref")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the ref field of hydra.scala.syntax.SingletonType
singletonTypeWithRef :: Typed.TypedTerm Syntax.SingletonType -> Typed.TypedTerm Syntax.RefData -> Typed.TypedTerm Syntax.SingletonType
singletonTypeWithRef original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.SingletonType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "ref"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.scala.syntax.Source
source :: Typed.TypedTerm [Syntax.Stat] -> Typed.TypedTerm Syntax.Source
source stats =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.Source"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "stats"),
          Core.fieldTerm = (Typed.unTypedTerm stats)}]}))
-- | DSL accessor for the stats field of hydra.scala.syntax.Source
sourceStats :: Typed.TypedTerm Syntax.Source -> Typed.TypedTerm [Syntax.Stat]
sourceStats x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.Source"),
        Core.projectionFieldName = (Core.Name "stats")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the stats field of hydra.scala.syntax.Source
sourceWithStats :: Typed.TypedTerm Syntax.Source -> Typed.TypedTerm [Syntax.Stat] -> Typed.TypedTerm Syntax.Source
sourceWithStats original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.Source"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "stats"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL injection for the decl variant of hydra.scala.syntax.Stat
statDecl :: Typed.TypedTerm Syntax.Decl -> Typed.TypedTerm Syntax.Stat
statDecl x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Stat"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "decl"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the defn variant of hydra.scala.syntax.Stat
statDefn :: Typed.TypedTerm Syntax.Defn -> Typed.TypedTerm Syntax.Stat
statDefn x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Stat"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "defn"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the importExport variant of hydra.scala.syntax.Stat
statImportExport :: Typed.TypedTerm Syntax.ImportExportStat -> Typed.TypedTerm Syntax.Stat
statImportExport x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Stat"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "importExport"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the term variant of hydra.scala.syntax.Stat
statTerm :: Typed.TypedTerm Syntax.Data -> Typed.TypedTerm Syntax.Stat
statTerm x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Stat"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "term"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for hydra.scala.syntax.SuperData
superData :: Typed.TypedTerm Syntax.Name -> Typed.TypedTerm Syntax.Name -> Typed.TypedTerm Syntax.SuperData
superData thisp superp =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.SuperData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "thisp"),
          Core.fieldTerm = (Typed.unTypedTerm thisp)},
        Core.Field {
          Core.fieldName = (Core.Name "superp"),
          Core.fieldTerm = (Typed.unTypedTerm superp)}]}))
-- | DSL accessor for the superp field of hydra.scala.syntax.SuperData
superDataSuperp :: Typed.TypedTerm Syntax.SuperData -> Typed.TypedTerm Syntax.Name
superDataSuperp x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.SuperData"),
        Core.projectionFieldName = (Core.Name "superp")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the thisp field of hydra.scala.syntax.SuperData
superDataThisp :: Typed.TypedTerm Syntax.SuperData -> Typed.TypedTerm Syntax.Name
superDataThisp x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.SuperData"),
        Core.projectionFieldName = (Core.Name "thisp")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the superp field of hydra.scala.syntax.SuperData
superDataWithSuperp :: Typed.TypedTerm Syntax.SuperData -> Typed.TypedTerm Syntax.Name -> Typed.TypedTerm Syntax.SuperData
superDataWithSuperp original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.SuperData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "thisp"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.SuperData"),
              Core.projectionFieldName = (Core.Name "thisp")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "superp"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the thisp field of hydra.scala.syntax.SuperData
superDataWithThisp :: Typed.TypedTerm Syntax.SuperData -> Typed.TypedTerm Syntax.Name -> Typed.TypedTerm Syntax.SuperData
superDataWithThisp original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.SuperData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "thisp"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "superp"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.SuperData"),
              Core.projectionFieldName = (Core.Name "superp")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.scala.syntax.Template
template :: Typed.TypedTerm [Syntax.Stat] -> Typed.TypedTerm [Syntax.Init] -> Typed.TypedTerm Syntax.Self -> Typed.TypedTerm [Syntax.Stat] -> Typed.TypedTerm Syntax.Template
template early inits self stats =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.Template"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "early"),
          Core.fieldTerm = (Typed.unTypedTerm early)},
        Core.Field {
          Core.fieldName = (Core.Name "inits"),
          Core.fieldTerm = (Typed.unTypedTerm inits)},
        Core.Field {
          Core.fieldName = (Core.Name "self"),
          Core.fieldTerm = (Typed.unTypedTerm self)},
        Core.Field {
          Core.fieldName = (Core.Name "stats"),
          Core.fieldTerm = (Typed.unTypedTerm stats)}]}))
-- | DSL accessor for the early field of hydra.scala.syntax.Template
templateEarly :: Typed.TypedTerm Syntax.Template -> Typed.TypedTerm [Syntax.Stat]
templateEarly x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.Template"),
        Core.projectionFieldName = (Core.Name "early")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the inits field of hydra.scala.syntax.Template
templateInits :: Typed.TypedTerm Syntax.Template -> Typed.TypedTerm [Syntax.Init]
templateInits x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.Template"),
        Core.projectionFieldName = (Core.Name "inits")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the self field of hydra.scala.syntax.Template
templateSelf :: Typed.TypedTerm Syntax.Template -> Typed.TypedTerm Syntax.Self
templateSelf x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.Template"),
        Core.projectionFieldName = (Core.Name "self")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the stats field of hydra.scala.syntax.Template
templateStats :: Typed.TypedTerm Syntax.Template -> Typed.TypedTerm [Syntax.Stat]
templateStats x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.Template"),
        Core.projectionFieldName = (Core.Name "stats")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the early field of hydra.scala.syntax.Template
templateWithEarly :: Typed.TypedTerm Syntax.Template -> Typed.TypedTerm [Syntax.Stat] -> Typed.TypedTerm Syntax.Template
templateWithEarly original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.Template"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "early"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "inits"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.Template"),
              Core.projectionFieldName = (Core.Name "inits")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "self"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.Template"),
              Core.projectionFieldName = (Core.Name "self")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "stats"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.Template"),
              Core.projectionFieldName = (Core.Name "stats")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the inits field of hydra.scala.syntax.Template
templateWithInits :: Typed.TypedTerm Syntax.Template -> Typed.TypedTerm [Syntax.Init] -> Typed.TypedTerm Syntax.Template
templateWithInits original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.Template"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "early"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.Template"),
              Core.projectionFieldName = (Core.Name "early")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inits"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "self"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.Template"),
              Core.projectionFieldName = (Core.Name "self")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "stats"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.Template"),
              Core.projectionFieldName = (Core.Name "stats")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the self field of hydra.scala.syntax.Template
templateWithSelf :: Typed.TypedTerm Syntax.Template -> Typed.TypedTerm Syntax.Self -> Typed.TypedTerm Syntax.Template
templateWithSelf original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.Template"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "early"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.Template"),
              Core.projectionFieldName = (Core.Name "early")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inits"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.Template"),
              Core.projectionFieldName = (Core.Name "inits")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "self"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "stats"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.Template"),
              Core.projectionFieldName = (Core.Name "stats")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the stats field of hydra.scala.syntax.Template
templateWithStats :: Typed.TypedTerm Syntax.Template -> Typed.TypedTerm [Syntax.Stat] -> Typed.TypedTerm Syntax.Template
templateWithStats original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.Template"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "early"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.Template"),
              Core.projectionFieldName = (Core.Name "early")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inits"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.Template"),
              Core.projectionFieldName = (Core.Name "inits")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "self"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.Template"),
              Core.projectionFieldName = (Core.Name "self")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "stats"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for the hydra.scala.syntax.ThisData wrapper
thisData :: Typed.TypedTerm () -> Typed.TypedTerm Syntax.ThisData
thisData x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.scala.syntax.ThisData"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.scala.syntax.ThrowData
throwData :: Typed.TypedTerm Syntax.Data -> Typed.TypedTerm Syntax.ThrowData
throwData expr =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ThrowData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expr"),
          Core.fieldTerm = (Typed.unTypedTerm expr)}]}))
-- | DSL accessor for the expr field of hydra.scala.syntax.ThrowData
throwDataExpr :: Typed.TypedTerm Syntax.ThrowData -> Typed.TypedTerm Syntax.Data
throwDataExpr x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ThrowData"),
        Core.projectionFieldName = (Core.Name "expr")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the expr field of hydra.scala.syntax.ThrowData
throwDataWithExpr :: Typed.TypedTerm Syntax.ThrowData -> Typed.TypedTerm Syntax.Data -> Typed.TypedTerm Syntax.ThrowData
throwDataWithExpr original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ThrowData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expr"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.scala.syntax.TraitDefn
traitDefn :: Typed.TypedTerm [Syntax.Mod] -> Typed.TypedTerm Syntax.NameType -> Typed.TypedTerm [Syntax.ParamType] -> Typed.TypedTerm Syntax.PrimaryCtor -> Typed.TypedTerm Syntax.Template -> Typed.TypedTerm Syntax.TraitDefn
traitDefn mods name tparams ctor template =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.TraitDefn"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Typed.unTypedTerm mods)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Typed.unTypedTerm tparams)},
        Core.Field {
          Core.fieldName = (Core.Name "ctor"),
          Core.fieldTerm = (Typed.unTypedTerm ctor)},
        Core.Field {
          Core.fieldName = (Core.Name "template"),
          Core.fieldTerm = (Typed.unTypedTerm template)}]}))
-- | DSL accessor for the ctor field of hydra.scala.syntax.TraitDefn
traitDefnCtor :: Typed.TypedTerm Syntax.TraitDefn -> Typed.TypedTerm Syntax.PrimaryCtor
traitDefnCtor x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TraitDefn"),
        Core.projectionFieldName = (Core.Name "ctor")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the mods field of hydra.scala.syntax.TraitDefn
traitDefnMods :: Typed.TypedTerm Syntax.TraitDefn -> Typed.TypedTerm [Syntax.Mod]
traitDefnMods x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TraitDefn"),
        Core.projectionFieldName = (Core.Name "mods")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the name field of hydra.scala.syntax.TraitDefn
traitDefnName :: Typed.TypedTerm Syntax.TraitDefn -> Typed.TypedTerm Syntax.NameType
traitDefnName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TraitDefn"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the template field of hydra.scala.syntax.TraitDefn
traitDefnTemplate :: Typed.TypedTerm Syntax.TraitDefn -> Typed.TypedTerm Syntax.Template
traitDefnTemplate x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TraitDefn"),
        Core.projectionFieldName = (Core.Name "template")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the tparams field of hydra.scala.syntax.TraitDefn
traitDefnTparams :: Typed.TypedTerm Syntax.TraitDefn -> Typed.TypedTerm [Syntax.ParamType]
traitDefnTparams x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TraitDefn"),
        Core.projectionFieldName = (Core.Name "tparams")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the ctor field of hydra.scala.syntax.TraitDefn
traitDefnWithCtor :: Typed.TypedTerm Syntax.TraitDefn -> Typed.TypedTerm Syntax.PrimaryCtor -> Typed.TypedTerm Syntax.TraitDefn
traitDefnWithCtor original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.TraitDefn"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TraitDefn"),
              Core.projectionFieldName = (Core.Name "mods")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TraitDefn"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TraitDefn"),
              Core.projectionFieldName = (Core.Name "tparams")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ctor"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "template"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TraitDefn"),
              Core.projectionFieldName = (Core.Name "template")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the mods field of hydra.scala.syntax.TraitDefn
traitDefnWithMods :: Typed.TypedTerm Syntax.TraitDefn -> Typed.TypedTerm [Syntax.Mod] -> Typed.TypedTerm Syntax.TraitDefn
traitDefnWithMods original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.TraitDefn"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TraitDefn"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TraitDefn"),
              Core.projectionFieldName = (Core.Name "tparams")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ctor"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TraitDefn"),
              Core.projectionFieldName = (Core.Name "ctor")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "template"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TraitDefn"),
              Core.projectionFieldName = (Core.Name "template")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the name field of hydra.scala.syntax.TraitDefn
traitDefnWithName :: Typed.TypedTerm Syntax.TraitDefn -> Typed.TypedTerm Syntax.NameType -> Typed.TypedTerm Syntax.TraitDefn
traitDefnWithName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.TraitDefn"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TraitDefn"),
              Core.projectionFieldName = (Core.Name "mods")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TraitDefn"),
              Core.projectionFieldName = (Core.Name "tparams")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ctor"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TraitDefn"),
              Core.projectionFieldName = (Core.Name "ctor")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "template"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TraitDefn"),
              Core.projectionFieldName = (Core.Name "template")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the template field of hydra.scala.syntax.TraitDefn
traitDefnWithTemplate :: Typed.TypedTerm Syntax.TraitDefn -> Typed.TypedTerm Syntax.Template -> Typed.TypedTerm Syntax.TraitDefn
traitDefnWithTemplate original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.TraitDefn"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TraitDefn"),
              Core.projectionFieldName = (Core.Name "mods")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TraitDefn"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TraitDefn"),
              Core.projectionFieldName = (Core.Name "tparams")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ctor"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TraitDefn"),
              Core.projectionFieldName = (Core.Name "ctor")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "template"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the tparams field of hydra.scala.syntax.TraitDefn
traitDefnWithTparams :: Typed.TypedTerm Syntax.TraitDefn -> Typed.TypedTerm [Syntax.ParamType] -> Typed.TypedTerm Syntax.TraitDefn
traitDefnWithTparams original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.TraitDefn"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TraitDefn"),
              Core.projectionFieldName = (Core.Name "mods")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TraitDefn"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "ctor"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TraitDefn"),
              Core.projectionFieldName = (Core.Name "ctor")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "template"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TraitDefn"),
              Core.projectionFieldName = (Core.Name "template")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL injection for the bounds variant of hydra.scala.syntax.Tree
treeBounds :: Typed.TypedTerm Syntax.TypeBounds -> Typed.TypedTerm Syntax.Tree
treeBounds x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Tree"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "bounds"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the caseTree variant of hydra.scala.syntax.Tree
treeCaseTree :: Typed.TypedTerm Syntax.CaseTree -> Typed.TypedTerm Syntax.Tree
treeCaseTree x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Tree"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "caseTree"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the ctor variant of hydra.scala.syntax.Tree
treeCtor :: Typed.TypedTerm Syntax.Ctor -> Typed.TypedTerm Syntax.Tree
treeCtor x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Tree"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "ctor"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the enumerator variant of hydra.scala.syntax.Tree
treeEnumerator :: Typed.TypedTerm Syntax.Enumerator -> Typed.TypedTerm Syntax.Tree
treeEnumerator x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Tree"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "enumerator"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the importee variant of hydra.scala.syntax.Tree
treeImportee :: Typed.TypedTerm Syntax.Importee -> Typed.TypedTerm Syntax.Tree
treeImportee x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Tree"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "importee"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the importer variant of hydra.scala.syntax.Tree
treeImporter :: Typed.TypedTerm Syntax.Importer -> Typed.TypedTerm Syntax.Tree
treeImporter x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Tree"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "importer"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the member variant of hydra.scala.syntax.Tree
treeMember :: Typed.TypedTerm Syntax.Member -> Typed.TypedTerm Syntax.Tree
treeMember x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Tree"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "member"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the mod variant of hydra.scala.syntax.Tree
treeMod :: Typed.TypedTerm Syntax.Mod -> Typed.TypedTerm Syntax.Tree
treeMod x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Tree"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "mod"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the pat variant of hydra.scala.syntax.Tree
treePat :: Typed.TypedTerm Syntax.Pat -> Typed.TypedTerm Syntax.Tree
treePat x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Tree"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "pat"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the ref variant of hydra.scala.syntax.Tree
treeRef :: Typed.TypedTerm Syntax.Ref -> Typed.TypedTerm Syntax.Tree
treeRef x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Tree"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "ref"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the source variant of hydra.scala.syntax.Tree
treeSource :: Typed.TypedTerm Syntax.Source -> Typed.TypedTerm Syntax.Tree
treeSource x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Tree"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "source"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the stat variant of hydra.scala.syntax.Tree
treeStat :: Typed.TypedTerm Syntax.Stat -> Typed.TypedTerm Syntax.Tree
treeStat x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Tree"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "stat"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the template variant of hydra.scala.syntax.Tree
treeTemplate :: Typed.TypedTerm Syntax.Template -> Typed.TypedTerm Syntax.Tree
treeTemplate x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Tree"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "template"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the type variant of hydra.scala.syntax.Tree
treeType :: Typed.TypedTerm Syntax.Type -> Typed.TypedTerm Syntax.Tree
treeType x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Tree"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "type"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for hydra.scala.syntax.TryData
tryData :: Typed.TypedTerm Syntax.Data -> Typed.TypedTerm [Syntax.Case] -> Typed.TypedTerm (Maybe Syntax.Data) -> Typed.TypedTerm Syntax.TryData
tryData expr catchp finallyp =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.TryData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expr"),
          Core.fieldTerm = (Typed.unTypedTerm expr)},
        Core.Field {
          Core.fieldName = (Core.Name "catchp"),
          Core.fieldTerm = (Typed.unTypedTerm catchp)},
        Core.Field {
          Core.fieldName = (Core.Name "finallyp"),
          Core.fieldTerm = (Typed.unTypedTerm finallyp)}]}))
-- | DSL accessor for the catchp field of hydra.scala.syntax.TryData
tryDataCatchp :: Typed.TypedTerm Syntax.TryData -> Typed.TypedTerm [Syntax.Case]
tryDataCatchp x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TryData"),
        Core.projectionFieldName = (Core.Name "catchp")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the expr field of hydra.scala.syntax.TryData
tryDataExpr :: Typed.TypedTerm Syntax.TryData -> Typed.TypedTerm Syntax.Data
tryDataExpr x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TryData"),
        Core.projectionFieldName = (Core.Name "expr")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the finallyp field of hydra.scala.syntax.TryData
tryDataFinallyp :: Typed.TypedTerm Syntax.TryData -> Typed.TypedTerm (Maybe Syntax.Data)
tryDataFinallyp x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TryData"),
        Core.projectionFieldName = (Core.Name "finallyp")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the catchp field of hydra.scala.syntax.TryData
tryDataWithCatchp :: Typed.TypedTerm Syntax.TryData -> Typed.TypedTerm [Syntax.Case] -> Typed.TypedTerm Syntax.TryData
tryDataWithCatchp original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.TryData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expr"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TryData"),
              Core.projectionFieldName = (Core.Name "expr")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "catchp"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "finallyp"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TryData"),
              Core.projectionFieldName = (Core.Name "finallyp")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the expr field of hydra.scala.syntax.TryData
tryDataWithExpr :: Typed.TypedTerm Syntax.TryData -> Typed.TypedTerm Syntax.Data -> Typed.TypedTerm Syntax.TryData
tryDataWithExpr original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.TryData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expr"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "catchp"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TryData"),
              Core.projectionFieldName = (Core.Name "catchp")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "finallyp"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TryData"),
              Core.projectionFieldName = (Core.Name "finallyp")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the finallyp field of hydra.scala.syntax.TryData
tryDataWithFinallyp :: Typed.TypedTerm Syntax.TryData -> Typed.TypedTerm (Maybe Syntax.Data) -> Typed.TypedTerm Syntax.TryData
tryDataWithFinallyp original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.TryData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expr"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TryData"),
              Core.projectionFieldName = (Core.Name "expr")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "catchp"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TryData"),
              Core.projectionFieldName = (Core.Name "catchp")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "finallyp"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.scala.syntax.TryWithHandlerData
tryWithHandlerData :: Typed.TypedTerm Syntax.Data -> Typed.TypedTerm Syntax.Data -> Typed.TypedTerm (Maybe Syntax.Data) -> Typed.TypedTerm Syntax.TryWithHandlerData
tryWithHandlerData expr catchp finallyp =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.TryWithHandlerData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expr"),
          Core.fieldTerm = (Typed.unTypedTerm expr)},
        Core.Field {
          Core.fieldName = (Core.Name "catchp"),
          Core.fieldTerm = (Typed.unTypedTerm catchp)},
        Core.Field {
          Core.fieldName = (Core.Name "finallyp"),
          Core.fieldTerm = (Typed.unTypedTerm finallyp)}]}))
-- | DSL accessor for the catchp field of hydra.scala.syntax.TryWithHandlerData
tryWithHandlerDataCatchp :: Typed.TypedTerm Syntax.TryWithHandlerData -> Typed.TypedTerm Syntax.Data
tryWithHandlerDataCatchp x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TryWithHandlerData"),
        Core.projectionFieldName = (Core.Name "catchp")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the expr field of hydra.scala.syntax.TryWithHandlerData
tryWithHandlerDataExpr :: Typed.TypedTerm Syntax.TryWithHandlerData -> Typed.TypedTerm Syntax.Data
tryWithHandlerDataExpr x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TryWithHandlerData"),
        Core.projectionFieldName = (Core.Name "expr")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the finallyp field of hydra.scala.syntax.TryWithHandlerData
tryWithHandlerDataFinallyp :: Typed.TypedTerm Syntax.TryWithHandlerData -> Typed.TypedTerm (Maybe Syntax.Data)
tryWithHandlerDataFinallyp x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TryWithHandlerData"),
        Core.projectionFieldName = (Core.Name "finallyp")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the catchp field of hydra.scala.syntax.TryWithHandlerData
tryWithHandlerDataWithCatchp :: Typed.TypedTerm Syntax.TryWithHandlerData -> Typed.TypedTerm Syntax.Data -> Typed.TypedTerm Syntax.TryWithHandlerData
tryWithHandlerDataWithCatchp original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.TryWithHandlerData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expr"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TryWithHandlerData"),
              Core.projectionFieldName = (Core.Name "expr")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "catchp"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "finallyp"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TryWithHandlerData"),
              Core.projectionFieldName = (Core.Name "finallyp")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the expr field of hydra.scala.syntax.TryWithHandlerData
tryWithHandlerDataWithExpr :: Typed.TypedTerm Syntax.TryWithHandlerData -> Typed.TypedTerm Syntax.Data -> Typed.TypedTerm Syntax.TryWithHandlerData
tryWithHandlerDataWithExpr original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.TryWithHandlerData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expr"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "catchp"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TryWithHandlerData"),
              Core.projectionFieldName = (Core.Name "catchp")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "finallyp"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TryWithHandlerData"),
              Core.projectionFieldName = (Core.Name "finallyp")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the finallyp field of hydra.scala.syntax.TryWithHandlerData
tryWithHandlerDataWithFinallyp :: Typed.TypedTerm Syntax.TryWithHandlerData -> Typed.TypedTerm (Maybe Syntax.Data) -> Typed.TypedTerm Syntax.TryWithHandlerData
tryWithHandlerDataWithFinallyp original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.TryWithHandlerData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expr"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TryWithHandlerData"),
              Core.projectionFieldName = (Core.Name "expr")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "catchp"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TryWithHandlerData"),
              Core.projectionFieldName = (Core.Name "catchp")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "finallyp"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.scala.syntax.TupleData
tupleData :: Typed.TypedTerm [Syntax.Data] -> Typed.TypedTerm Syntax.TupleData
tupleData args =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.TupleData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "args"),
          Core.fieldTerm = (Typed.unTypedTerm args)}]}))
-- | DSL accessor for the args field of hydra.scala.syntax.TupleData
tupleDataArgs :: Typed.TypedTerm Syntax.TupleData -> Typed.TypedTerm [Syntax.Data]
tupleDataArgs x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TupleData"),
        Core.projectionFieldName = (Core.Name "args")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the args field of hydra.scala.syntax.TupleData
tupleDataWithArgs :: Typed.TypedTerm Syntax.TupleData -> Typed.TypedTerm [Syntax.Data] -> Typed.TypedTerm Syntax.TupleData
tupleDataWithArgs original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.TupleData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "args"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.scala.syntax.TuplePat
tuplePat :: Typed.TypedTerm [Syntax.Pat] -> Typed.TypedTerm Syntax.TuplePat
tuplePat args =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.TuplePat"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "args"),
          Core.fieldTerm = (Typed.unTypedTerm args)}]}))
-- | DSL accessor for the args field of hydra.scala.syntax.TuplePat
tuplePatArgs :: Typed.TypedTerm Syntax.TuplePat -> Typed.TypedTerm [Syntax.Pat]
tuplePatArgs x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TuplePat"),
        Core.projectionFieldName = (Core.Name "args")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the args field of hydra.scala.syntax.TuplePat
tuplePatWithArgs :: Typed.TypedTerm Syntax.TuplePat -> Typed.TypedTerm [Syntax.Pat] -> Typed.TypedTerm Syntax.TuplePat
tuplePatWithArgs original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.TuplePat"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "args"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.scala.syntax.TupleType
tupleType :: Typed.TypedTerm [Syntax.Type] -> Typed.TypedTerm Syntax.TupleType
tupleType args =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.TupleType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "args"),
          Core.fieldTerm = (Typed.unTypedTerm args)}]}))
-- | DSL accessor for the args field of hydra.scala.syntax.TupleType
tupleTypeArgs :: Typed.TypedTerm Syntax.TupleType -> Typed.TypedTerm [Syntax.Type]
tupleTypeArgs x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TupleType"),
        Core.projectionFieldName = (Core.Name "args")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the args field of hydra.scala.syntax.TupleType
tupleTypeWithArgs :: Typed.TypedTerm Syntax.TupleType -> Typed.TypedTerm [Syntax.Type] -> Typed.TypedTerm Syntax.TupleType
tupleTypeWithArgs original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.TupleType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "args"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL injection for the and variant of hydra.scala.syntax.Type
typeAnd :: Typed.TypedTerm Syntax.AndType -> Typed.TypedTerm Syntax.Type
typeAnd x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "and"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the annotate variant of hydra.scala.syntax.Type
typeAnnotate :: Typed.TypedTerm Syntax.AnnotateType -> Typed.TypedTerm Syntax.Type
typeAnnotate x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "annotate"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the anonymousName variant of hydra.scala.syntax.Type
typeAnonymousName :: Typed.TypedTerm Syntax.AnonymousNameType -> Typed.TypedTerm Syntax.Type
typeAnonymousName x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "anonymousName"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the apply variant of hydra.scala.syntax.Type
typeApply :: Typed.TypedTerm Syntax.ApplyType -> Typed.TypedTerm Syntax.Type
typeApply x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "apply"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the applyInfix variant of hydra.scala.syntax.Type
typeApplyInfix :: Typed.TypedTerm Syntax.ApplyInfixType -> Typed.TypedTerm Syntax.Type
typeApplyInfix x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "applyInfix"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for hydra.scala.syntax.TypeBounds
typeBounds :: Typed.TypedTerm (Maybe Syntax.Type) -> Typed.TypedTerm (Maybe Syntax.Type) -> Typed.TypedTerm Syntax.TypeBounds
typeBounds lo hi =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.TypeBounds"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lo"),
          Core.fieldTerm = (Typed.unTypedTerm lo)},
        Core.Field {
          Core.fieldName = (Core.Name "hi"),
          Core.fieldTerm = (Typed.unTypedTerm hi)}]}))
-- | DSL accessor for the hi field of hydra.scala.syntax.TypeBounds
typeBoundsHi :: Typed.TypedTerm Syntax.TypeBounds -> Typed.TypedTerm (Maybe Syntax.Type)
typeBoundsHi x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TypeBounds"),
        Core.projectionFieldName = (Core.Name "hi")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the lo field of hydra.scala.syntax.TypeBounds
typeBoundsLo :: Typed.TypedTerm Syntax.TypeBounds -> Typed.TypedTerm (Maybe Syntax.Type)
typeBoundsLo x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TypeBounds"),
        Core.projectionFieldName = (Core.Name "lo")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the hi field of hydra.scala.syntax.TypeBounds
typeBoundsWithHi :: Typed.TypedTerm Syntax.TypeBounds -> Typed.TypedTerm (Maybe Syntax.Type) -> Typed.TypedTerm Syntax.TypeBounds
typeBoundsWithHi original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.TypeBounds"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lo"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TypeBounds"),
              Core.projectionFieldName = (Core.Name "lo")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "hi"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the lo field of hydra.scala.syntax.TypeBounds
typeBoundsWithLo :: Typed.TypedTerm Syntax.TypeBounds -> Typed.TypedTerm (Maybe Syntax.Type) -> Typed.TypedTerm Syntax.TypeBounds
typeBoundsWithLo original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.TypeBounds"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lo"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "hi"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TypeBounds"),
              Core.projectionFieldName = (Core.Name "hi")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL injection for the byName variant of hydra.scala.syntax.Type
typeByName :: Typed.TypedTerm Syntax.ByNameType -> Typed.TypedTerm Syntax.Type
typeByName x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "byName"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for hydra.scala.syntax.TypeCase
typeCase :: Typed.TypedTerm Syntax.Type -> Typed.TypedTerm Syntax.Type -> Typed.TypedTerm Syntax.TypeCase
typeCase pat body =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.TypeCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pat"),
          Core.fieldTerm = (Typed.unTypedTerm pat)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm body)}]}))
-- | DSL accessor for the body field of hydra.scala.syntax.TypeCase
typeCaseBody :: Typed.TypedTerm Syntax.TypeCase -> Typed.TypedTerm Syntax.Type
typeCaseBody x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TypeCase"),
        Core.projectionFieldName = (Core.Name "body")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the pat field of hydra.scala.syntax.TypeCase
typeCasePat :: Typed.TypedTerm Syntax.TypeCase -> Typed.TypedTerm Syntax.Type
typeCasePat x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TypeCase"),
        Core.projectionFieldName = (Core.Name "pat")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the body field of hydra.scala.syntax.TypeCase
typeCaseWithBody :: Typed.TypedTerm Syntax.TypeCase -> Typed.TypedTerm Syntax.Type -> Typed.TypedTerm Syntax.TypeCase
typeCaseWithBody original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.TypeCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pat"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TypeCase"),
              Core.projectionFieldName = (Core.Name "pat")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the pat field of hydra.scala.syntax.TypeCase
typeCaseWithPat :: Typed.TypedTerm Syntax.TypeCase -> Typed.TypedTerm Syntax.Type -> Typed.TypedTerm Syntax.TypeCase
typeCaseWithPat original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.TypeCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pat"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TypeCase"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL injection for the contextFunction variant of hydra.scala.syntax.Type
typeContextFunction :: Typed.TypedTerm Syntax.ContextFunctionType -> Typed.TypedTerm Syntax.Type
typeContextFunction x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "contextFunction"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for hydra.scala.syntax.TypeDecl
typeDecl :: Typed.TypedTerm [Syntax.Mod] -> Typed.TypedTerm Syntax.NameType -> Typed.TypedTerm [Syntax.ParamType] -> Typed.TypedTerm Syntax.TypeBounds -> Typed.TypedTerm Syntax.TypeDecl
typeDecl mods name tparams bounds =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.TypeDecl"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Typed.unTypedTerm mods)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Typed.unTypedTerm tparams)},
        Core.Field {
          Core.fieldName = (Core.Name "bounds"),
          Core.fieldTerm = (Typed.unTypedTerm bounds)}]}))
-- | DSL accessor for the bounds field of hydra.scala.syntax.TypeDecl
typeDeclBounds :: Typed.TypedTerm Syntax.TypeDecl -> Typed.TypedTerm Syntax.TypeBounds
typeDeclBounds x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TypeDecl"),
        Core.projectionFieldName = (Core.Name "bounds")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the mods field of hydra.scala.syntax.TypeDecl
typeDeclMods :: Typed.TypedTerm Syntax.TypeDecl -> Typed.TypedTerm [Syntax.Mod]
typeDeclMods x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TypeDecl"),
        Core.projectionFieldName = (Core.Name "mods")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the name field of hydra.scala.syntax.TypeDecl
typeDeclName :: Typed.TypedTerm Syntax.TypeDecl -> Typed.TypedTerm Syntax.NameType
typeDeclName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TypeDecl"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the tparams field of hydra.scala.syntax.TypeDecl
typeDeclTparams :: Typed.TypedTerm Syntax.TypeDecl -> Typed.TypedTerm [Syntax.ParamType]
typeDeclTparams x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TypeDecl"),
        Core.projectionFieldName = (Core.Name "tparams")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the bounds field of hydra.scala.syntax.TypeDecl
typeDeclWithBounds :: Typed.TypedTerm Syntax.TypeDecl -> Typed.TypedTerm Syntax.TypeBounds -> Typed.TypedTerm Syntax.TypeDecl
typeDeclWithBounds original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.TypeDecl"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TypeDecl"),
              Core.projectionFieldName = (Core.Name "mods")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TypeDecl"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TypeDecl"),
              Core.projectionFieldName = (Core.Name "tparams")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "bounds"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the mods field of hydra.scala.syntax.TypeDecl
typeDeclWithMods :: Typed.TypedTerm Syntax.TypeDecl -> Typed.TypedTerm [Syntax.Mod] -> Typed.TypedTerm Syntax.TypeDecl
typeDeclWithMods original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.TypeDecl"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TypeDecl"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TypeDecl"),
              Core.projectionFieldName = (Core.Name "tparams")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "bounds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TypeDecl"),
              Core.projectionFieldName = (Core.Name "bounds")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the name field of hydra.scala.syntax.TypeDecl
typeDeclWithName :: Typed.TypedTerm Syntax.TypeDecl -> Typed.TypedTerm Syntax.NameType -> Typed.TypedTerm Syntax.TypeDecl
typeDeclWithName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.TypeDecl"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TypeDecl"),
              Core.projectionFieldName = (Core.Name "mods")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TypeDecl"),
              Core.projectionFieldName = (Core.Name "tparams")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "bounds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TypeDecl"),
              Core.projectionFieldName = (Core.Name "bounds")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the tparams field of hydra.scala.syntax.TypeDecl
typeDeclWithTparams :: Typed.TypedTerm Syntax.TypeDecl -> Typed.TypedTerm [Syntax.ParamType] -> Typed.TypedTerm Syntax.TypeDecl
typeDeclWithTparams original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.TypeDecl"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TypeDecl"),
              Core.projectionFieldName = (Core.Name "mods")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TypeDecl"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "bounds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TypeDecl"),
              Core.projectionFieldName = (Core.Name "bounds")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.scala.syntax.TypeDefn
typeDefn :: Typed.TypedTerm [Syntax.Mod] -> Typed.TypedTerm Syntax.NameType -> Typed.TypedTerm [Syntax.ParamType] -> Typed.TypedTerm Syntax.Type -> Typed.TypedTerm Syntax.TypeDefn
typeDefn mods name tparams body =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.TypeDefn"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Typed.unTypedTerm mods)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Typed.unTypedTerm tparams)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm body)}]}))
-- | DSL accessor for the body field of hydra.scala.syntax.TypeDefn
typeDefnBody :: Typed.TypedTerm Syntax.TypeDefn -> Typed.TypedTerm Syntax.Type
typeDefnBody x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TypeDefn"),
        Core.projectionFieldName = (Core.Name "body")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the mods field of hydra.scala.syntax.TypeDefn
typeDefnMods :: Typed.TypedTerm Syntax.TypeDefn -> Typed.TypedTerm [Syntax.Mod]
typeDefnMods x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TypeDefn"),
        Core.projectionFieldName = (Core.Name "mods")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the name field of hydra.scala.syntax.TypeDefn
typeDefnName :: Typed.TypedTerm Syntax.TypeDefn -> Typed.TypedTerm Syntax.NameType
typeDefnName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TypeDefn"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the tparams field of hydra.scala.syntax.TypeDefn
typeDefnTparams :: Typed.TypedTerm Syntax.TypeDefn -> Typed.TypedTerm [Syntax.ParamType]
typeDefnTparams x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TypeDefn"),
        Core.projectionFieldName = (Core.Name "tparams")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the body field of hydra.scala.syntax.TypeDefn
typeDefnWithBody :: Typed.TypedTerm Syntax.TypeDefn -> Typed.TypedTerm Syntax.Type -> Typed.TypedTerm Syntax.TypeDefn
typeDefnWithBody original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.TypeDefn"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TypeDefn"),
              Core.projectionFieldName = (Core.Name "mods")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TypeDefn"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TypeDefn"),
              Core.projectionFieldName = (Core.Name "tparams")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the mods field of hydra.scala.syntax.TypeDefn
typeDefnWithMods :: Typed.TypedTerm Syntax.TypeDefn -> Typed.TypedTerm [Syntax.Mod] -> Typed.TypedTerm Syntax.TypeDefn
typeDefnWithMods original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.TypeDefn"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TypeDefn"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TypeDefn"),
              Core.projectionFieldName = (Core.Name "tparams")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TypeDefn"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the name field of hydra.scala.syntax.TypeDefn
typeDefnWithName :: Typed.TypedTerm Syntax.TypeDefn -> Typed.TypedTerm Syntax.NameType -> Typed.TypedTerm Syntax.TypeDefn
typeDefnWithName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.TypeDefn"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TypeDefn"),
              Core.projectionFieldName = (Core.Name "mods")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TypeDefn"),
              Core.projectionFieldName = (Core.Name "tparams")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TypeDefn"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the tparams field of hydra.scala.syntax.TypeDefn
typeDefnWithTparams :: Typed.TypedTerm Syntax.TypeDefn -> Typed.TypedTerm [Syntax.ParamType] -> Typed.TypedTerm Syntax.TypeDefn
typeDefnWithTparams original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.TypeDefn"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TypeDefn"),
              Core.projectionFieldName = (Core.Name "mods")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TypeDefn"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tparams"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TypeDefn"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL injection for the existential variant of hydra.scala.syntax.Type
typeExistential :: Typed.TypedTerm Syntax.ExistentialType -> Typed.TypedTerm Syntax.Type
typeExistential x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "existential"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the function variant of hydra.scala.syntax.Type
typeFunction :: Typed.TypedTerm Syntax.FunctionType -> Typed.TypedTerm Syntax.Type
typeFunction x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "function"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the implicitFunction variant of hydra.scala.syntax.Type
typeImplicitFunction :: Typed.TypedTerm Syntax.ImplicitFunctionType -> Typed.TypedTerm Syntax.Type
typeImplicitFunction x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "implicitFunction"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the lambda variant of hydra.scala.syntax.Type
typeLambda :: Typed.TypedTerm Syntax.LambdaType -> Typed.TypedTerm Syntax.Type
typeLambda x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "lambda"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the match variant of hydra.scala.syntax.Type
typeMatch :: Typed.TypedTerm Syntax.MatchType -> Typed.TypedTerm Syntax.Type
typeMatch x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "match"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for hydra.scala.syntax.TypeMember
typeMember :: Typed.TypedTerm Syntax.NameType -> Typed.TypedTerm Syntax.TypeMember
typeMember name =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.TypeMember"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm name)}]}))
-- | DSL accessor for the name field of hydra.scala.syntax.TypeMember
typeMemberName :: Typed.TypedTerm Syntax.TypeMember -> Typed.TypedTerm Syntax.NameType
typeMemberName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TypeMember"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the name field of hydra.scala.syntax.TypeMember
typeMemberWithName :: Typed.TypedTerm Syntax.TypeMember -> Typed.TypedTerm Syntax.NameType -> Typed.TypedTerm Syntax.TypeMember
typeMemberWithName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.TypeMember"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL injection for the method variant of hydra.scala.syntax.Type
typeMethod :: Typed.TypedTerm Syntax.MethodType -> Typed.TypedTerm Syntax.Type
typeMethod x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "method"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the or variant of hydra.scala.syntax.Type
typeOr :: Typed.TypedTerm Syntax.OrType -> Typed.TypedTerm Syntax.Type
typeOr x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "or"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the placeholder variant of hydra.scala.syntax.Type
typePlaceholder :: Typed.TypedTerm Syntax.PlaceholderType -> Typed.TypedTerm Syntax.Type
typePlaceholder x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "placeholder"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the polyFunction variant of hydra.scala.syntax.Type
typePolyFunction :: Typed.TypedTerm Syntax.PolyFunctionType -> Typed.TypedTerm Syntax.Type
typePolyFunction x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "polyFunction"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the ref variant of hydra.scala.syntax.Type
typeRef :: Typed.TypedTerm Syntax.RefType -> Typed.TypedTerm Syntax.Type
typeRef x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "ref"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the refine variant of hydra.scala.syntax.Type
typeRefine :: Typed.TypedTerm Syntax.RefineType -> Typed.TypedTerm Syntax.Type
typeRefine x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "refine"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the repeated variant of hydra.scala.syntax.Type
typeRepeated :: Typed.TypedTerm Syntax.RepeatedType -> Typed.TypedTerm Syntax.Type
typeRepeated x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "repeated"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the tuple variant of hydra.scala.syntax.Type
typeTuple :: Typed.TypedTerm Syntax.TupleType -> Typed.TypedTerm Syntax.Type
typeTuple x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "tuple"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the typedParam variant of hydra.scala.syntax.Type
typeTypedParam :: Typed.TypedTerm Syntax.TypedParamType -> Typed.TypedTerm Syntax.Type
typeTypedParam x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "typedParam"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the var variant of hydra.scala.syntax.Type
typeVar :: Typed.TypedTerm Syntax.VarType -> Typed.TypedTerm Syntax.Type
typeVar x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "var"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the with variant of hydra.scala.syntax.Type
typeWith :: Typed.TypedTerm Syntax.WithType -> Typed.TypedTerm Syntax.Type
typeWith x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.scala.syntax.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "with"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for hydra.scala.syntax.TypedParamType
typedParamType :: Typed.TypedTerm Syntax.Name -> Typed.TypedTerm Syntax.Type -> Typed.TypedTerm Syntax.TypedParamType
typedParamType name typ =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.TypedParamType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "typ"),
          Core.fieldTerm = (Typed.unTypedTerm typ)}]}))
-- | DSL accessor for the name field of hydra.scala.syntax.TypedParamType
typedParamTypeName :: Typed.TypedTerm Syntax.TypedParamType -> Typed.TypedTerm Syntax.Name
typedParamTypeName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TypedParamType"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the typ field of hydra.scala.syntax.TypedParamType
typedParamTypeTyp :: Typed.TypedTerm Syntax.TypedParamType -> Typed.TypedTerm Syntax.Type
typedParamTypeTyp x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TypedParamType"),
        Core.projectionFieldName = (Core.Name "typ")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the name field of hydra.scala.syntax.TypedParamType
typedParamTypeWithName :: Typed.TypedTerm Syntax.TypedParamType -> Typed.TypedTerm Syntax.Name -> Typed.TypedTerm Syntax.TypedParamType
typedParamTypeWithName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.TypedParamType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "typ"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TypedParamType"),
              Core.projectionFieldName = (Core.Name "typ")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the typ field of hydra.scala.syntax.TypedParamType
typedParamTypeWithTyp :: Typed.TypedTerm Syntax.TypedParamType -> Typed.TypedTerm Syntax.Type -> Typed.TypedTerm Syntax.TypedParamType
typedParamTypeWithTyp original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.TypedParamType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TypedParamType"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typ"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.scala.syntax.TypedPat
typedPat :: Typed.TypedTerm Syntax.Pat -> Typed.TypedTerm Syntax.Type -> Typed.TypedTerm Syntax.TypedPat
typedPat lhs rhs =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.TypedPat"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Typed.unTypedTerm lhs)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Typed.unTypedTerm rhs)}]}))
-- | DSL accessor for the lhs field of hydra.scala.syntax.TypedPat
typedPatLhs :: Typed.TypedTerm Syntax.TypedPat -> Typed.TypedTerm Syntax.Pat
typedPatLhs x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TypedPat"),
        Core.projectionFieldName = (Core.Name "lhs")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the rhs field of hydra.scala.syntax.TypedPat
typedPatRhs :: Typed.TypedTerm Syntax.TypedPat -> Typed.TypedTerm Syntax.Type
typedPatRhs x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TypedPat"),
        Core.projectionFieldName = (Core.Name "rhs")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the lhs field of hydra.scala.syntax.TypedPat
typedPatWithLhs :: Typed.TypedTerm Syntax.TypedPat -> Typed.TypedTerm Syntax.Pat -> Typed.TypedTerm Syntax.TypedPat
typedPatWithLhs original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.TypedPat"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TypedPat"),
              Core.projectionFieldName = (Core.Name "rhs")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the rhs field of hydra.scala.syntax.TypedPat
typedPatWithRhs :: Typed.TypedTerm Syntax.TypedPat -> Typed.TypedTerm Syntax.Type -> Typed.TypedTerm Syntax.TypedPat
typedPatWithRhs original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.TypedPat"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.TypedPat"),
              Core.projectionFieldName = (Core.Name "lhs")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL accessor for the body of hydra.scala.syntax.AnonymousData
unAnonymousData :: Typed.TypedTerm Syntax.AnonymousData -> Typed.TypedTerm ()
unAnonymousData x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.scala.syntax.AnonymousData")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.scala.syntax.AnonymousNameType
unAnonymousNameType :: Typed.TypedTerm Syntax.AnonymousNameType -> Typed.TypedTerm ()
unAnonymousNameType x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.scala.syntax.AnonymousNameType")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.scala.syntax.PredefString
unPredefString :: Typed.TypedTerm Syntax.PredefString -> Typed.TypedTerm String
unPredefString x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.scala.syntax.PredefString")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.scala.syntax.Self
unSelf :: Typed.TypedTerm Syntax.Self -> Typed.TypedTerm ()
unSelf x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.scala.syntax.Self")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.scala.syntax.ThisData
unThisData :: Typed.TypedTerm Syntax.ThisData -> Typed.TypedTerm ()
unThisData x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.scala.syntax.ThisData")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.scala.syntax.UnimportImportee
unimportImportee :: Typed.TypedTerm Syntax.Name -> Typed.TypedTerm Syntax.UnimportImportee
unimportImportee name =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.UnimportImportee"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm name)}]}))
-- | DSL accessor for the name field of hydra.scala.syntax.UnimportImportee
unimportImporteeName :: Typed.TypedTerm Syntax.UnimportImportee -> Typed.TypedTerm Syntax.Name
unimportImporteeName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.UnimportImportee"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the name field of hydra.scala.syntax.UnimportImportee
unimportImporteeWithName :: Typed.TypedTerm Syntax.UnimportImportee -> Typed.TypedTerm Syntax.Name -> Typed.TypedTerm Syntax.UnimportImportee
unimportImporteeWithName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.UnimportImportee"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.scala.syntax.ValDecl
valDecl :: Typed.TypedTerm [Syntax.Mod] -> Typed.TypedTerm [Syntax.Pat] -> Typed.TypedTerm Syntax.Type -> Typed.TypedTerm Syntax.ValDecl
valDecl mods pats decltpe =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ValDecl"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Typed.unTypedTerm mods)},
        Core.Field {
          Core.fieldName = (Core.Name "pats"),
          Core.fieldTerm = (Typed.unTypedTerm pats)},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Typed.unTypedTerm decltpe)}]}))
-- | DSL accessor for the decltpe field of hydra.scala.syntax.ValDecl
valDeclDecltpe :: Typed.TypedTerm Syntax.ValDecl -> Typed.TypedTerm Syntax.Type
valDeclDecltpe x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ValDecl"),
        Core.projectionFieldName = (Core.Name "decltpe")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the mods field of hydra.scala.syntax.ValDecl
valDeclMods :: Typed.TypedTerm Syntax.ValDecl -> Typed.TypedTerm [Syntax.Mod]
valDeclMods x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ValDecl"),
        Core.projectionFieldName = (Core.Name "mods")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the pats field of hydra.scala.syntax.ValDecl
valDeclPats :: Typed.TypedTerm Syntax.ValDecl -> Typed.TypedTerm [Syntax.Pat]
valDeclPats x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ValDecl"),
        Core.projectionFieldName = (Core.Name "pats")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the decltpe field of hydra.scala.syntax.ValDecl
valDeclWithDecltpe :: Typed.TypedTerm Syntax.ValDecl -> Typed.TypedTerm Syntax.Type -> Typed.TypedTerm Syntax.ValDecl
valDeclWithDecltpe original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ValDecl"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ValDecl"),
              Core.projectionFieldName = (Core.Name "mods")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "pats"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ValDecl"),
              Core.projectionFieldName = (Core.Name "pats")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the mods field of hydra.scala.syntax.ValDecl
valDeclWithMods :: Typed.TypedTerm Syntax.ValDecl -> Typed.TypedTerm [Syntax.Mod] -> Typed.TypedTerm Syntax.ValDecl
valDeclWithMods original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ValDecl"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "pats"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ValDecl"),
              Core.projectionFieldName = (Core.Name "pats")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ValDecl"),
              Core.projectionFieldName = (Core.Name "decltpe")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the pats field of hydra.scala.syntax.ValDecl
valDeclWithPats :: Typed.TypedTerm Syntax.ValDecl -> Typed.TypedTerm [Syntax.Pat] -> Typed.TypedTerm Syntax.ValDecl
valDeclWithPats original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ValDecl"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ValDecl"),
              Core.projectionFieldName = (Core.Name "mods")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "pats"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ValDecl"),
              Core.projectionFieldName = (Core.Name "decltpe")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.scala.syntax.ValDefn
valDefn :: Typed.TypedTerm [Syntax.Mod] -> Typed.TypedTerm [Syntax.Pat] -> Typed.TypedTerm (Maybe Syntax.Type) -> Typed.TypedTerm Syntax.Data -> Typed.TypedTerm Syntax.ValDefn
valDefn mods pats decltpe rhs =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ValDefn"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Typed.unTypedTerm mods)},
        Core.Field {
          Core.fieldName = (Core.Name "pats"),
          Core.fieldTerm = (Typed.unTypedTerm pats)},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Typed.unTypedTerm decltpe)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Typed.unTypedTerm rhs)}]}))
-- | DSL accessor for the decltpe field of hydra.scala.syntax.ValDefn
valDefnDecltpe :: Typed.TypedTerm Syntax.ValDefn -> Typed.TypedTerm (Maybe Syntax.Type)
valDefnDecltpe x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ValDefn"),
        Core.projectionFieldName = (Core.Name "decltpe")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the mods field of hydra.scala.syntax.ValDefn
valDefnMods :: Typed.TypedTerm Syntax.ValDefn -> Typed.TypedTerm [Syntax.Mod]
valDefnMods x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ValDefn"),
        Core.projectionFieldName = (Core.Name "mods")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the pats field of hydra.scala.syntax.ValDefn
valDefnPats :: Typed.TypedTerm Syntax.ValDefn -> Typed.TypedTerm [Syntax.Pat]
valDefnPats x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ValDefn"),
        Core.projectionFieldName = (Core.Name "pats")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the rhs field of hydra.scala.syntax.ValDefn
valDefnRhs :: Typed.TypedTerm Syntax.ValDefn -> Typed.TypedTerm Syntax.Data
valDefnRhs x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ValDefn"),
        Core.projectionFieldName = (Core.Name "rhs")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the decltpe field of hydra.scala.syntax.ValDefn
valDefnWithDecltpe :: Typed.TypedTerm Syntax.ValDefn -> Typed.TypedTerm (Maybe Syntax.Type) -> Typed.TypedTerm Syntax.ValDefn
valDefnWithDecltpe original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ValDefn"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ValDefn"),
              Core.projectionFieldName = (Core.Name "mods")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "pats"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ValDefn"),
              Core.projectionFieldName = (Core.Name "pats")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ValDefn"),
              Core.projectionFieldName = (Core.Name "rhs")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the mods field of hydra.scala.syntax.ValDefn
valDefnWithMods :: Typed.TypedTerm Syntax.ValDefn -> Typed.TypedTerm [Syntax.Mod] -> Typed.TypedTerm Syntax.ValDefn
valDefnWithMods original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ValDefn"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "pats"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ValDefn"),
              Core.projectionFieldName = (Core.Name "pats")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ValDefn"),
              Core.projectionFieldName = (Core.Name "decltpe")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ValDefn"),
              Core.projectionFieldName = (Core.Name "rhs")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the pats field of hydra.scala.syntax.ValDefn
valDefnWithPats :: Typed.TypedTerm Syntax.ValDefn -> Typed.TypedTerm [Syntax.Pat] -> Typed.TypedTerm Syntax.ValDefn
valDefnWithPats original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ValDefn"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ValDefn"),
              Core.projectionFieldName = (Core.Name "mods")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "pats"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ValDefn"),
              Core.projectionFieldName = (Core.Name "decltpe")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ValDefn"),
              Core.projectionFieldName = (Core.Name "rhs")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the rhs field of hydra.scala.syntax.ValDefn
valDefnWithRhs :: Typed.TypedTerm Syntax.ValDefn -> Typed.TypedTerm Syntax.Data -> Typed.TypedTerm Syntax.ValDefn
valDefnWithRhs original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ValDefn"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ValDefn"),
              Core.projectionFieldName = (Core.Name "mods")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "pats"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ValDefn"),
              Core.projectionFieldName = (Core.Name "pats")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ValDefn"),
              Core.projectionFieldName = (Core.Name "decltpe")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.scala.syntax.ValEnumerator
valEnumerator :: Typed.TypedTerm Syntax.Pat -> Typed.TypedTerm Syntax.Data -> Typed.TypedTerm Syntax.ValEnumerator
valEnumerator pat rhs =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ValEnumerator"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pat"),
          Core.fieldTerm = (Typed.unTypedTerm pat)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Typed.unTypedTerm rhs)}]}))
-- | DSL accessor for the pat field of hydra.scala.syntax.ValEnumerator
valEnumeratorPat :: Typed.TypedTerm Syntax.ValEnumerator -> Typed.TypedTerm Syntax.Pat
valEnumeratorPat x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ValEnumerator"),
        Core.projectionFieldName = (Core.Name "pat")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the rhs field of hydra.scala.syntax.ValEnumerator
valEnumeratorRhs :: Typed.TypedTerm Syntax.ValEnumerator -> Typed.TypedTerm Syntax.Data
valEnumeratorRhs x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ValEnumerator"),
        Core.projectionFieldName = (Core.Name "rhs")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the pat field of hydra.scala.syntax.ValEnumerator
valEnumeratorWithPat :: Typed.TypedTerm Syntax.ValEnumerator -> Typed.TypedTerm Syntax.Pat -> Typed.TypedTerm Syntax.ValEnumerator
valEnumeratorWithPat original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ValEnumerator"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pat"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ValEnumerator"),
              Core.projectionFieldName = (Core.Name "rhs")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the rhs field of hydra.scala.syntax.ValEnumerator
valEnumeratorWithRhs :: Typed.TypedTerm Syntax.ValEnumerator -> Typed.TypedTerm Syntax.Data -> Typed.TypedTerm Syntax.ValEnumerator
valEnumeratorWithRhs original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.ValEnumerator"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pat"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.ValEnumerator"),
              Core.projectionFieldName = (Core.Name "pat")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.scala.syntax.VarDecl
varDecl :: Typed.TypedTerm [Syntax.Mod] -> Typed.TypedTerm [Syntax.Pat] -> Typed.TypedTerm Syntax.Type -> Typed.TypedTerm Syntax.VarDecl
varDecl mods pats decltpe =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.VarDecl"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Typed.unTypedTerm mods)},
        Core.Field {
          Core.fieldName = (Core.Name "pats"),
          Core.fieldTerm = (Typed.unTypedTerm pats)},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Typed.unTypedTerm decltpe)}]}))
-- | DSL accessor for the decltpe field of hydra.scala.syntax.VarDecl
varDeclDecltpe :: Typed.TypedTerm Syntax.VarDecl -> Typed.TypedTerm Syntax.Type
varDeclDecltpe x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.VarDecl"),
        Core.projectionFieldName = (Core.Name "decltpe")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the mods field of hydra.scala.syntax.VarDecl
varDeclMods :: Typed.TypedTerm Syntax.VarDecl -> Typed.TypedTerm [Syntax.Mod]
varDeclMods x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.VarDecl"),
        Core.projectionFieldName = (Core.Name "mods")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the pats field of hydra.scala.syntax.VarDecl
varDeclPats :: Typed.TypedTerm Syntax.VarDecl -> Typed.TypedTerm [Syntax.Pat]
varDeclPats x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.VarDecl"),
        Core.projectionFieldName = (Core.Name "pats")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the decltpe field of hydra.scala.syntax.VarDecl
varDeclWithDecltpe :: Typed.TypedTerm Syntax.VarDecl -> Typed.TypedTerm Syntax.Type -> Typed.TypedTerm Syntax.VarDecl
varDeclWithDecltpe original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.VarDecl"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.VarDecl"),
              Core.projectionFieldName = (Core.Name "mods")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "pats"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.VarDecl"),
              Core.projectionFieldName = (Core.Name "pats")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the mods field of hydra.scala.syntax.VarDecl
varDeclWithMods :: Typed.TypedTerm Syntax.VarDecl -> Typed.TypedTerm [Syntax.Mod] -> Typed.TypedTerm Syntax.VarDecl
varDeclWithMods original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.VarDecl"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "pats"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.VarDecl"),
              Core.projectionFieldName = (Core.Name "pats")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.VarDecl"),
              Core.projectionFieldName = (Core.Name "decltpe")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the pats field of hydra.scala.syntax.VarDecl
varDeclWithPats :: Typed.TypedTerm Syntax.VarDecl -> Typed.TypedTerm [Syntax.Pat] -> Typed.TypedTerm Syntax.VarDecl
varDeclWithPats original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.VarDecl"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.VarDecl"),
              Core.projectionFieldName = (Core.Name "mods")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "pats"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.VarDecl"),
              Core.projectionFieldName = (Core.Name "decltpe")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.scala.syntax.VarDefn
varDefn :: Typed.TypedTerm [Syntax.Mod] -> Typed.TypedTerm [Syntax.Pat] -> Typed.TypedTerm Syntax.Type -> Typed.TypedTerm (Maybe Syntax.Data) -> Typed.TypedTerm Syntax.VarDefn
varDefn mods pats decltpe rhs =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.VarDefn"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Typed.unTypedTerm mods)},
        Core.Field {
          Core.fieldName = (Core.Name "pats"),
          Core.fieldTerm = (Typed.unTypedTerm pats)},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Typed.unTypedTerm decltpe)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Typed.unTypedTerm rhs)}]}))
-- | DSL accessor for the decltpe field of hydra.scala.syntax.VarDefn
varDefnDecltpe :: Typed.TypedTerm Syntax.VarDefn -> Typed.TypedTerm Syntax.Type
varDefnDecltpe x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.VarDefn"),
        Core.projectionFieldName = (Core.Name "decltpe")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the mods field of hydra.scala.syntax.VarDefn
varDefnMods :: Typed.TypedTerm Syntax.VarDefn -> Typed.TypedTerm [Syntax.Mod]
varDefnMods x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.VarDefn"),
        Core.projectionFieldName = (Core.Name "mods")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the pats field of hydra.scala.syntax.VarDefn
varDefnPats :: Typed.TypedTerm Syntax.VarDefn -> Typed.TypedTerm [Syntax.Pat]
varDefnPats x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.VarDefn"),
        Core.projectionFieldName = (Core.Name "pats")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the rhs field of hydra.scala.syntax.VarDefn
varDefnRhs :: Typed.TypedTerm Syntax.VarDefn -> Typed.TypedTerm (Maybe Syntax.Data)
varDefnRhs x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.VarDefn"),
        Core.projectionFieldName = (Core.Name "rhs")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the decltpe field of hydra.scala.syntax.VarDefn
varDefnWithDecltpe :: Typed.TypedTerm Syntax.VarDefn -> Typed.TypedTerm Syntax.Type -> Typed.TypedTerm Syntax.VarDefn
varDefnWithDecltpe original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.VarDefn"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.VarDefn"),
              Core.projectionFieldName = (Core.Name "mods")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "pats"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.VarDefn"),
              Core.projectionFieldName = (Core.Name "pats")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.VarDefn"),
              Core.projectionFieldName = (Core.Name "rhs")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the mods field of hydra.scala.syntax.VarDefn
varDefnWithMods :: Typed.TypedTerm Syntax.VarDefn -> Typed.TypedTerm [Syntax.Mod] -> Typed.TypedTerm Syntax.VarDefn
varDefnWithMods original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.VarDefn"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "pats"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.VarDefn"),
              Core.projectionFieldName = (Core.Name "pats")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.VarDefn"),
              Core.projectionFieldName = (Core.Name "decltpe")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.VarDefn"),
              Core.projectionFieldName = (Core.Name "rhs")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the pats field of hydra.scala.syntax.VarDefn
varDefnWithPats :: Typed.TypedTerm Syntax.VarDefn -> Typed.TypedTerm [Syntax.Pat] -> Typed.TypedTerm Syntax.VarDefn
varDefnWithPats original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.VarDefn"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.VarDefn"),
              Core.projectionFieldName = (Core.Name "mods")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "pats"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.VarDefn"),
              Core.projectionFieldName = (Core.Name "decltpe")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.VarDefn"),
              Core.projectionFieldName = (Core.Name "rhs")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the rhs field of hydra.scala.syntax.VarDefn
varDefnWithRhs :: Typed.TypedTerm Syntax.VarDefn -> Typed.TypedTerm (Maybe Syntax.Data) -> Typed.TypedTerm Syntax.VarDefn
varDefnWithRhs original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.VarDefn"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "mods"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.VarDefn"),
              Core.projectionFieldName = (Core.Name "mods")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "pats"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.VarDefn"),
              Core.projectionFieldName = (Core.Name "pats")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "decltpe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.VarDefn"),
              Core.projectionFieldName = (Core.Name "decltpe")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.scala.syntax.VarPat
varPat :: Typed.TypedTerm Syntax.NameData -> Typed.TypedTerm Syntax.VarPat
varPat name =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.VarPat"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm name)}]}))
-- | DSL accessor for the name field of hydra.scala.syntax.VarPat
varPatName :: Typed.TypedTerm Syntax.VarPat -> Typed.TypedTerm Syntax.NameData
varPatName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.VarPat"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the name field of hydra.scala.syntax.VarPat
varPatWithName :: Typed.TypedTerm Syntax.VarPat -> Typed.TypedTerm Syntax.NameData -> Typed.TypedTerm Syntax.VarPat
varPatWithName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.VarPat"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.scala.syntax.VarType
varType :: Typed.TypedTerm Syntax.NameType -> Typed.TypedTerm Syntax.VarType
varType name =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.VarType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm name)}]}))
-- | DSL accessor for the name field of hydra.scala.syntax.VarType
varTypeName :: Typed.TypedTerm Syntax.VarType -> Typed.TypedTerm Syntax.NameType
varTypeName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.VarType"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the name field of hydra.scala.syntax.VarType
varTypeWithName :: Typed.TypedTerm Syntax.VarType -> Typed.TypedTerm Syntax.NameType -> Typed.TypedTerm Syntax.VarType
varTypeWithName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.VarType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.scala.syntax.WhileData
whileData :: Typed.TypedTerm Syntax.Data -> Typed.TypedTerm Syntax.Data -> Typed.TypedTerm Syntax.WhileData
whileData expr body =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.WhileData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expr"),
          Core.fieldTerm = (Typed.unTypedTerm expr)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm body)}]}))
-- | DSL accessor for the body field of hydra.scala.syntax.WhileData
whileDataBody :: Typed.TypedTerm Syntax.WhileData -> Typed.TypedTerm Syntax.Data
whileDataBody x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.WhileData"),
        Core.projectionFieldName = (Core.Name "body")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the expr field of hydra.scala.syntax.WhileData
whileDataExpr :: Typed.TypedTerm Syntax.WhileData -> Typed.TypedTerm Syntax.Data
whileDataExpr x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.WhileData"),
        Core.projectionFieldName = (Core.Name "expr")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the body field of hydra.scala.syntax.WhileData
whileDataWithBody :: Typed.TypedTerm Syntax.WhileData -> Typed.TypedTerm Syntax.Data -> Typed.TypedTerm Syntax.WhileData
whileDataWithBody original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.WhileData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expr"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.WhileData"),
              Core.projectionFieldName = (Core.Name "expr")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the expr field of hydra.scala.syntax.WhileData
whileDataWithExpr :: Typed.TypedTerm Syntax.WhileData -> Typed.TypedTerm Syntax.Data -> Typed.TypedTerm Syntax.WhileData
whileDataWithExpr original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.WhileData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expr"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.WhileData"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.scala.syntax.WithType
withType :: Typed.TypedTerm Syntax.Type -> Typed.TypedTerm Syntax.Type -> Typed.TypedTerm Syntax.WithType
withType lhs rhs =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.WithType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Typed.unTypedTerm lhs)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Typed.unTypedTerm rhs)}]}))
-- | DSL accessor for the lhs field of hydra.scala.syntax.WithType
withTypeLhs :: Typed.TypedTerm Syntax.WithType -> Typed.TypedTerm Syntax.Type
withTypeLhs x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.WithType"),
        Core.projectionFieldName = (Core.Name "lhs")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the rhs field of hydra.scala.syntax.WithType
withTypeRhs :: Typed.TypedTerm Syntax.WithType -> Typed.TypedTerm Syntax.Type
withTypeRhs x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.scala.syntax.WithType"),
        Core.projectionFieldName = (Core.Name "rhs")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the lhs field of hydra.scala.syntax.WithType
withTypeWithLhs :: Typed.TypedTerm Syntax.WithType -> Typed.TypedTerm Syntax.Type -> Typed.TypedTerm Syntax.WithType
withTypeWithLhs original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.WithType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.WithType"),
              Core.projectionFieldName = (Core.Name "rhs")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the rhs field of hydra.scala.syntax.WithType
withTypeWithRhs :: Typed.TypedTerm Syntax.WithType -> Typed.TypedTerm Syntax.Type -> Typed.TypedTerm Syntax.WithType
withTypeWithRhs original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.scala.syntax.WithType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.scala.syntax.WithType"),
              Core.projectionFieldName = (Core.Name "lhs")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
