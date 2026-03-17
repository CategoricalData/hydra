-- Note: this is an automatically generated file. Do not edit.

-- | DSL functions for hydra.ext.go.syntax

module Hydra.Dsl.Ext.Go.Syntax where

import qualified Hydra.Core as Core
import qualified Hydra.Ext.Go.Syntax as Syntax
import qualified Hydra.Phantoms as Phantoms
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

annotatedDeclaration :: (Phantoms.TTerm String -> Phantoms.TTerm Syntax.TopLevelDecl -> Phantoms.TTerm Syntax.AnnotatedDeclaration)
annotatedDeclaration comment declaration = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.AnnotatedDeclaration"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "comment"),
      Core.fieldTerm = (Phantoms.unTTerm comment)},
    Core.Field {
      Core.fieldName = (Core.Name "declaration"),
      Core.fieldTerm = (Phantoms.unTTerm declaration)}]})))

annotatedDeclarationComment :: (Phantoms.TTerm Syntax.AnnotatedDeclaration -> Phantoms.TTerm String)
annotatedDeclarationComment x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.AnnotatedDeclaration"),
    Core.projectionField = (Core.Name "comment")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

annotatedDeclarationDeclaration :: (Phantoms.TTerm Syntax.AnnotatedDeclaration -> Phantoms.TTerm Syntax.TopLevelDecl)
annotatedDeclarationDeclaration x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.AnnotatedDeclaration"),
    Core.projectionField = (Core.Name "declaration")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

annotatedDeclarationWithComment :: (Phantoms.TTerm Syntax.AnnotatedDeclaration -> Phantoms.TTerm String -> Phantoms.TTerm Syntax.AnnotatedDeclaration)
annotatedDeclarationWithComment original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.AnnotatedDeclaration"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "comment"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "declaration"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.AnnotatedDeclaration"),
          Core.projectionField = (Core.Name "declaration")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

annotatedDeclarationWithDeclaration :: (Phantoms.TTerm Syntax.AnnotatedDeclaration -> Phantoms.TTerm Syntax.TopLevelDecl -> Phantoms.TTerm Syntax.AnnotatedDeclaration)
annotatedDeclarationWithDeclaration original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.AnnotatedDeclaration"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "comment"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.AnnotatedDeclaration"),
          Core.projectionField = (Core.Name "comment")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "declaration"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

module_ :: (Phantoms.TTerm Syntax.PackageClause -> Phantoms.TTerm [Syntax.ImportDecl] -> Phantoms.TTerm [Syntax.TopLevelDecl] -> Phantoms.TTerm Syntax.Module)
module_ package imports declarations = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.Module"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "package"),
      Core.fieldTerm = (Phantoms.unTTerm package)},
    Core.Field {
      Core.fieldName = (Core.Name "imports"),
      Core.fieldTerm = (Phantoms.unTTerm imports)},
    Core.Field {
      Core.fieldName = (Core.Name "declarations"),
      Core.fieldTerm = (Phantoms.unTTerm declarations)}]})))

modulePackage :: (Phantoms.TTerm Syntax.Module -> Phantoms.TTerm Syntax.PackageClause)
modulePackage x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.Module"),
    Core.projectionField = (Core.Name "package")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

moduleImports :: (Phantoms.TTerm Syntax.Module -> Phantoms.TTerm [Syntax.ImportDecl])
moduleImports x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.Module"),
    Core.projectionField = (Core.Name "imports")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

moduleDeclarations :: (Phantoms.TTerm Syntax.Module -> Phantoms.TTerm [Syntax.TopLevelDecl])
moduleDeclarations x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.Module"),
    Core.projectionField = (Core.Name "declarations")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

moduleWithPackage :: (Phantoms.TTerm Syntax.Module -> Phantoms.TTerm Syntax.PackageClause -> Phantoms.TTerm Syntax.Module)
moduleWithPackage original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.Module"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "package"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "imports"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.Module"),
          Core.projectionField = (Core.Name "imports")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "declarations"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.Module"),
          Core.projectionField = (Core.Name "declarations")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

moduleWithImports :: (Phantoms.TTerm Syntax.Module -> Phantoms.TTerm [Syntax.ImportDecl] -> Phantoms.TTerm Syntax.Module)
moduleWithImports original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.Module"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "package"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.Module"),
          Core.projectionField = (Core.Name "package")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "imports"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "declarations"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.Module"),
          Core.projectionField = (Core.Name "declarations")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

moduleWithDeclarations :: (Phantoms.TTerm Syntax.Module -> Phantoms.TTerm [Syntax.TopLevelDecl] -> Phantoms.TTerm Syntax.Module)
moduleWithDeclarations original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.Module"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "package"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.Module"),
          Core.projectionField = (Core.Name "package")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "imports"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.Module"),
          Core.projectionField = (Core.Name "imports")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "declarations"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

identifier :: (Phantoms.TTerm String -> Phantoms.TTerm Syntax.Identifier)
identifier x = (Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
  Core.wrappedTermTypeName = (Core.Name "hydra.ext.go.syntax.Identifier"),
  Core.wrappedTermBody = (Phantoms.unTTerm x)})))

unIdentifier :: (Phantoms.TTerm Syntax.Identifier -> Phantoms.TTerm String)
unIdentifier x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.go.syntax.Identifier")))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

intLit :: (Phantoms.TTerm Integer -> Phantoms.TTerm Syntax.IntLit)
intLit x = (Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
  Core.wrappedTermTypeName = (Core.Name "hydra.ext.go.syntax.IntLit"),
  Core.wrappedTermBody = (Phantoms.unTTerm x)})))

unIntLit :: (Phantoms.TTerm Syntax.IntLit -> Phantoms.TTerm Integer)
unIntLit x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.go.syntax.IntLit")))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

floatLit :: (Phantoms.TTerm Double -> Phantoms.TTerm Syntax.FloatLit)
floatLit x = (Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
  Core.wrappedTermTypeName = (Core.Name "hydra.ext.go.syntax.FloatLit"),
  Core.wrappedTermBody = (Phantoms.unTTerm x)})))

unFloatLit :: (Phantoms.TTerm Syntax.FloatLit -> Phantoms.TTerm Double)
unFloatLit x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.go.syntax.FloatLit")))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

imaginaryLit :: (Phantoms.TTerm Double -> Phantoms.TTerm Syntax.ImaginaryLit)
imaginaryLit x = (Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
  Core.wrappedTermTypeName = (Core.Name "hydra.ext.go.syntax.ImaginaryLit"),
  Core.wrappedTermBody = (Phantoms.unTTerm x)})))

unImaginaryLit :: (Phantoms.TTerm Syntax.ImaginaryLit -> Phantoms.TTerm Double)
unImaginaryLit x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.go.syntax.ImaginaryLit")))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

runeLit :: (Phantoms.TTerm Int -> Phantoms.TTerm Syntax.RuneLit)
runeLit x = (Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
  Core.wrappedTermTypeName = (Core.Name "hydra.ext.go.syntax.RuneLit"),
  Core.wrappedTermBody = (Phantoms.unTTerm x)})))

unRuneLit :: (Phantoms.TTerm Syntax.RuneLit -> Phantoms.TTerm Int)
unRuneLit x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.go.syntax.RuneLit")))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

stringLitRaw :: (Phantoms.TTerm Syntax.RawStringLit -> Phantoms.TTerm Syntax.StringLit)
stringLitRaw x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.go.syntax.StringLit"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "raw"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

stringLitInterpreted :: (Phantoms.TTerm Syntax.InterpretedStringLit -> Phantoms.TTerm Syntax.StringLit)
stringLitInterpreted x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.go.syntax.StringLit"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "interpreted"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

rawStringLit :: (Phantoms.TTerm String -> Phantoms.TTerm Syntax.RawStringLit)
rawStringLit x = (Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
  Core.wrappedTermTypeName = (Core.Name "hydra.ext.go.syntax.RawStringLit"),
  Core.wrappedTermBody = (Phantoms.unTTerm x)})))

unRawStringLit :: (Phantoms.TTerm Syntax.RawStringLit -> Phantoms.TTerm String)
unRawStringLit x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.go.syntax.RawStringLit")))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

interpretedStringLit :: (Phantoms.TTerm String -> Phantoms.TTerm Syntax.InterpretedStringLit)
interpretedStringLit x = (Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
  Core.wrappedTermTypeName = (Core.Name "hydra.ext.go.syntax.InterpretedStringLit"),
  Core.wrappedTermBody = (Phantoms.unTTerm x)})))

unInterpretedStringLit :: (Phantoms.TTerm Syntax.InterpretedStringLit -> Phantoms.TTerm String)
unInterpretedStringLit x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.go.syntax.InterpretedStringLit")))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

sourceFile :: (Phantoms.TTerm Syntax.PackageClause -> Phantoms.TTerm [Syntax.ImportDecl] -> Phantoms.TTerm [Syntax.TopLevelDecl] -> Phantoms.TTerm Syntax.SourceFile)
sourceFile package imports declarations = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.SourceFile"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "package"),
      Core.fieldTerm = (Phantoms.unTTerm package)},
    Core.Field {
      Core.fieldName = (Core.Name "imports"),
      Core.fieldTerm = (Phantoms.unTTerm imports)},
    Core.Field {
      Core.fieldName = (Core.Name "declarations"),
      Core.fieldTerm = (Phantoms.unTTerm declarations)}]})))

sourceFilePackage :: (Phantoms.TTerm Syntax.SourceFile -> Phantoms.TTerm Syntax.PackageClause)
sourceFilePackage x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.SourceFile"),
    Core.projectionField = (Core.Name "package")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

sourceFileImports :: (Phantoms.TTerm Syntax.SourceFile -> Phantoms.TTerm [Syntax.ImportDecl])
sourceFileImports x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.SourceFile"),
    Core.projectionField = (Core.Name "imports")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

sourceFileDeclarations :: (Phantoms.TTerm Syntax.SourceFile -> Phantoms.TTerm [Syntax.TopLevelDecl])
sourceFileDeclarations x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.SourceFile"),
    Core.projectionField = (Core.Name "declarations")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

sourceFileWithPackage :: (Phantoms.TTerm Syntax.SourceFile -> Phantoms.TTerm Syntax.PackageClause -> Phantoms.TTerm Syntax.SourceFile)
sourceFileWithPackage original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.SourceFile"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "package"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "imports"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.SourceFile"),
          Core.projectionField = (Core.Name "imports")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "declarations"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.SourceFile"),
          Core.projectionField = (Core.Name "declarations")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

sourceFileWithImports :: (Phantoms.TTerm Syntax.SourceFile -> Phantoms.TTerm [Syntax.ImportDecl] -> Phantoms.TTerm Syntax.SourceFile)
sourceFileWithImports original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.SourceFile"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "package"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.SourceFile"),
          Core.projectionField = (Core.Name "package")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "imports"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "declarations"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.SourceFile"),
          Core.projectionField = (Core.Name "declarations")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

sourceFileWithDeclarations :: (Phantoms.TTerm Syntax.SourceFile -> Phantoms.TTerm [Syntax.TopLevelDecl] -> Phantoms.TTerm Syntax.SourceFile)
sourceFileWithDeclarations original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.SourceFile"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "package"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.SourceFile"),
          Core.projectionField = (Core.Name "package")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "imports"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.SourceFile"),
          Core.projectionField = (Core.Name "imports")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "declarations"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

packageClause :: (Phantoms.TTerm Syntax.Identifier -> Phantoms.TTerm Syntax.PackageClause)
packageClause x = (Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
  Core.wrappedTermTypeName = (Core.Name "hydra.ext.go.syntax.PackageClause"),
  Core.wrappedTermBody = (Phantoms.unTTerm x)})))

unPackageClause :: (Phantoms.TTerm Syntax.PackageClause -> Phantoms.TTerm Syntax.Identifier)
unPackageClause x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.go.syntax.PackageClause")))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

importDecl :: (Phantoms.TTerm [Syntax.ImportSpec] -> Phantoms.TTerm Syntax.ImportDecl)
importDecl x = (Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
  Core.wrappedTermTypeName = (Core.Name "hydra.ext.go.syntax.ImportDecl"),
  Core.wrappedTermBody = (Phantoms.unTTerm x)})))

unImportDecl :: (Phantoms.TTerm Syntax.ImportDecl -> Phantoms.TTerm [Syntax.ImportSpec])
unImportDecl x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.go.syntax.ImportDecl")))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

importSpec :: (Phantoms.TTerm (Maybe Syntax.ImportAlias) -> Phantoms.TTerm Syntax.ImportPath -> Phantoms.TTerm Syntax.ImportSpec)
importSpec alias path = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.ImportSpec"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "alias"),
      Core.fieldTerm = (Phantoms.unTTerm alias)},
    Core.Field {
      Core.fieldName = (Core.Name "path"),
      Core.fieldTerm = (Phantoms.unTTerm path)}]})))

importSpecAlias :: (Phantoms.TTerm Syntax.ImportSpec -> Phantoms.TTerm (Maybe Syntax.ImportAlias))
importSpecAlias x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.ImportSpec"),
    Core.projectionField = (Core.Name "alias")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

importSpecPath :: (Phantoms.TTerm Syntax.ImportSpec -> Phantoms.TTerm Syntax.ImportPath)
importSpecPath x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.ImportSpec"),
    Core.projectionField = (Core.Name "path")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

importSpecWithAlias :: (Phantoms.TTerm Syntax.ImportSpec -> Phantoms.TTerm (Maybe Syntax.ImportAlias) -> Phantoms.TTerm Syntax.ImportSpec)
importSpecWithAlias original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.ImportSpec"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "alias"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "path"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.ImportSpec"),
          Core.projectionField = (Core.Name "path")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

importSpecWithPath :: (Phantoms.TTerm Syntax.ImportSpec -> Phantoms.TTerm Syntax.ImportPath -> Phantoms.TTerm Syntax.ImportSpec)
importSpecWithPath original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.ImportSpec"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "alias"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.ImportSpec"),
          Core.projectionField = (Core.Name "alias")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "path"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

importAliasDot :: (Phantoms.TTerm Syntax.ImportAlias)
importAliasDot = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.go.syntax.ImportAlias"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "dot"),
    Core.fieldTerm = Core.TermUnit}})))

importAliasName :: (Phantoms.TTerm Syntax.Identifier -> Phantoms.TTerm Syntax.ImportAlias)
importAliasName x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.go.syntax.ImportAlias"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "name"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

importPath :: (Phantoms.TTerm Syntax.StringLit -> Phantoms.TTerm Syntax.ImportPath)
importPath x = (Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
  Core.wrappedTermTypeName = (Core.Name "hydra.ext.go.syntax.ImportPath"),
  Core.wrappedTermBody = (Phantoms.unTTerm x)})))

unImportPath :: (Phantoms.TTerm Syntax.ImportPath -> Phantoms.TTerm Syntax.StringLit)
unImportPath x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.go.syntax.ImportPath")))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

declarationConst :: (Phantoms.TTerm Syntax.ConstDecl -> Phantoms.TTerm Syntax.Declaration)
declarationConst x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.go.syntax.Declaration"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "const"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

declarationType :: (Phantoms.TTerm Syntax.TypeDecl -> Phantoms.TTerm Syntax.Declaration)
declarationType x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.go.syntax.Declaration"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "type"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

declarationVar :: (Phantoms.TTerm Syntax.VarDecl -> Phantoms.TTerm Syntax.Declaration)
declarationVar x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.go.syntax.Declaration"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "var"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

topLevelDeclDeclaration :: (Phantoms.TTerm Syntax.Declaration -> Phantoms.TTerm Syntax.TopLevelDecl)
topLevelDeclDeclaration x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.go.syntax.TopLevelDecl"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "declaration"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

topLevelDeclFunction :: (Phantoms.TTerm Syntax.FunctionDecl -> Phantoms.TTerm Syntax.TopLevelDecl)
topLevelDeclFunction x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.go.syntax.TopLevelDecl"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "function"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

topLevelDeclMethod :: (Phantoms.TTerm Syntax.MethodDecl -> Phantoms.TTerm Syntax.TopLevelDecl)
topLevelDeclMethod x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.go.syntax.TopLevelDecl"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "method"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

constDecl :: (Phantoms.TTerm [Syntax.ConstSpec] -> Phantoms.TTerm Syntax.ConstDecl)
constDecl x = (Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
  Core.wrappedTermTypeName = (Core.Name "hydra.ext.go.syntax.ConstDecl"),
  Core.wrappedTermBody = (Phantoms.unTTerm x)})))

unConstDecl :: (Phantoms.TTerm Syntax.ConstDecl -> Phantoms.TTerm [Syntax.ConstSpec])
unConstDecl x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.go.syntax.ConstDecl")))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

constSpec :: (Phantoms.TTerm [Syntax.Identifier] -> Phantoms.TTerm (Maybe Syntax.Type) -> Phantoms.TTerm [Syntax.Expression] -> Phantoms.TTerm Syntax.ConstSpec)
constSpec names type_ values = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.ConstSpec"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "names"),
      Core.fieldTerm = (Phantoms.unTTerm names)},
    Core.Field {
      Core.fieldName = (Core.Name "type"),
      Core.fieldTerm = (Phantoms.unTTerm type_)},
    Core.Field {
      Core.fieldName = (Core.Name "values"),
      Core.fieldTerm = (Phantoms.unTTerm values)}]})))

constSpecNames :: (Phantoms.TTerm Syntax.ConstSpec -> Phantoms.TTerm [Syntax.Identifier])
constSpecNames x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.ConstSpec"),
    Core.projectionField = (Core.Name "names")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

constSpecType :: (Phantoms.TTerm Syntax.ConstSpec -> Phantoms.TTerm (Maybe Syntax.Type))
constSpecType x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.ConstSpec"),
    Core.projectionField = (Core.Name "type")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

constSpecValues :: (Phantoms.TTerm Syntax.ConstSpec -> Phantoms.TTerm [Syntax.Expression])
constSpecValues x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.ConstSpec"),
    Core.projectionField = (Core.Name "values")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

constSpecWithNames :: (Phantoms.TTerm Syntax.ConstSpec -> Phantoms.TTerm [Syntax.Identifier] -> Phantoms.TTerm Syntax.ConstSpec)
constSpecWithNames original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.ConstSpec"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "names"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "type"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.ConstSpec"),
          Core.projectionField = (Core.Name "type")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "values"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.ConstSpec"),
          Core.projectionField = (Core.Name "values")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

constSpecWithType :: (Phantoms.TTerm Syntax.ConstSpec -> Phantoms.TTerm (Maybe Syntax.Type) -> Phantoms.TTerm Syntax.ConstSpec)
constSpecWithType original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.ConstSpec"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "names"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.ConstSpec"),
          Core.projectionField = (Core.Name "names")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "type"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "values"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.ConstSpec"),
          Core.projectionField = (Core.Name "values")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

constSpecWithValues :: (Phantoms.TTerm Syntax.ConstSpec -> Phantoms.TTerm [Syntax.Expression] -> Phantoms.TTerm Syntax.ConstSpec)
constSpecWithValues original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.ConstSpec"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "names"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.ConstSpec"),
          Core.projectionField = (Core.Name "names")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "type"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.ConstSpec"),
          Core.projectionField = (Core.Name "type")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "values"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

varDecl :: (Phantoms.TTerm [Syntax.VarSpec] -> Phantoms.TTerm Syntax.VarDecl)
varDecl x = (Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
  Core.wrappedTermTypeName = (Core.Name "hydra.ext.go.syntax.VarDecl"),
  Core.wrappedTermBody = (Phantoms.unTTerm x)})))

unVarDecl :: (Phantoms.TTerm Syntax.VarDecl -> Phantoms.TTerm [Syntax.VarSpec])
unVarDecl x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.go.syntax.VarDecl")))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

varSpec :: (Phantoms.TTerm [Syntax.Identifier] -> Phantoms.TTerm (Maybe Syntax.Type) -> Phantoms.TTerm [Syntax.Expression] -> Phantoms.TTerm Syntax.VarSpec)
varSpec names type_ values = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.VarSpec"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "names"),
      Core.fieldTerm = (Phantoms.unTTerm names)},
    Core.Field {
      Core.fieldName = (Core.Name "type"),
      Core.fieldTerm = (Phantoms.unTTerm type_)},
    Core.Field {
      Core.fieldName = (Core.Name "values"),
      Core.fieldTerm = (Phantoms.unTTerm values)}]})))

varSpecNames :: (Phantoms.TTerm Syntax.VarSpec -> Phantoms.TTerm [Syntax.Identifier])
varSpecNames x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.VarSpec"),
    Core.projectionField = (Core.Name "names")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

varSpecType :: (Phantoms.TTerm Syntax.VarSpec -> Phantoms.TTerm (Maybe Syntax.Type))
varSpecType x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.VarSpec"),
    Core.projectionField = (Core.Name "type")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

varSpecValues :: (Phantoms.TTerm Syntax.VarSpec -> Phantoms.TTerm [Syntax.Expression])
varSpecValues x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.VarSpec"),
    Core.projectionField = (Core.Name "values")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

varSpecWithNames :: (Phantoms.TTerm Syntax.VarSpec -> Phantoms.TTerm [Syntax.Identifier] -> Phantoms.TTerm Syntax.VarSpec)
varSpecWithNames original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.VarSpec"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "names"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "type"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.VarSpec"),
          Core.projectionField = (Core.Name "type")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "values"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.VarSpec"),
          Core.projectionField = (Core.Name "values")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

varSpecWithType :: (Phantoms.TTerm Syntax.VarSpec -> Phantoms.TTerm (Maybe Syntax.Type) -> Phantoms.TTerm Syntax.VarSpec)
varSpecWithType original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.VarSpec"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "names"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.VarSpec"),
          Core.projectionField = (Core.Name "names")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "type"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "values"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.VarSpec"),
          Core.projectionField = (Core.Name "values")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

varSpecWithValues :: (Phantoms.TTerm Syntax.VarSpec -> Phantoms.TTerm [Syntax.Expression] -> Phantoms.TTerm Syntax.VarSpec)
varSpecWithValues original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.VarSpec"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "names"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.VarSpec"),
          Core.projectionField = (Core.Name "names")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "type"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.VarSpec"),
          Core.projectionField = (Core.Name "type")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "values"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

shortVarDecl :: (Phantoms.TTerm [Syntax.Identifier] -> Phantoms.TTerm [Syntax.Expression] -> Phantoms.TTerm Syntax.ShortVarDecl)
shortVarDecl names values = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.ShortVarDecl"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "names"),
      Core.fieldTerm = (Phantoms.unTTerm names)},
    Core.Field {
      Core.fieldName = (Core.Name "values"),
      Core.fieldTerm = (Phantoms.unTTerm values)}]})))

shortVarDeclNames :: (Phantoms.TTerm Syntax.ShortVarDecl -> Phantoms.TTerm [Syntax.Identifier])
shortVarDeclNames x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.ShortVarDecl"),
    Core.projectionField = (Core.Name "names")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

shortVarDeclValues :: (Phantoms.TTerm Syntax.ShortVarDecl -> Phantoms.TTerm [Syntax.Expression])
shortVarDeclValues x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.ShortVarDecl"),
    Core.projectionField = (Core.Name "values")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

shortVarDeclWithNames :: (Phantoms.TTerm Syntax.ShortVarDecl -> Phantoms.TTerm [Syntax.Identifier] -> Phantoms.TTerm Syntax.ShortVarDecl)
shortVarDeclWithNames original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.ShortVarDecl"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "names"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "values"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.ShortVarDecl"),
          Core.projectionField = (Core.Name "values")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

shortVarDeclWithValues :: (Phantoms.TTerm Syntax.ShortVarDecl -> Phantoms.TTerm [Syntax.Expression] -> Phantoms.TTerm Syntax.ShortVarDecl)
shortVarDeclWithValues original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.ShortVarDecl"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "names"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.ShortVarDecl"),
          Core.projectionField = (Core.Name "names")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "values"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

typeDecl :: (Phantoms.TTerm [Syntax.TypeSpec] -> Phantoms.TTerm Syntax.TypeDecl)
typeDecl x = (Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
  Core.wrappedTermTypeName = (Core.Name "hydra.ext.go.syntax.TypeDecl"),
  Core.wrappedTermBody = (Phantoms.unTTerm x)})))

unTypeDecl :: (Phantoms.TTerm Syntax.TypeDecl -> Phantoms.TTerm [Syntax.TypeSpec])
unTypeDecl x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.go.syntax.TypeDecl")))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

typeSpecAlias :: (Phantoms.TTerm Syntax.AliasDecl -> Phantoms.TTerm Syntax.TypeSpec)
typeSpecAlias x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.go.syntax.TypeSpec"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "alias"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

typeSpecDefinition :: (Phantoms.TTerm Syntax.TypeDef -> Phantoms.TTerm Syntax.TypeSpec)
typeSpecDefinition x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.go.syntax.TypeSpec"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "definition"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

aliasDecl :: (Phantoms.TTerm Syntax.Identifier -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.AliasDecl)
aliasDecl name type_ = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.AliasDecl"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Phantoms.unTTerm name)},
    Core.Field {
      Core.fieldName = (Core.Name "type"),
      Core.fieldTerm = (Phantoms.unTTerm type_)}]})))

aliasDeclName :: (Phantoms.TTerm Syntax.AliasDecl -> Phantoms.TTerm Syntax.Identifier)
aliasDeclName x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.AliasDecl"),
    Core.projectionField = (Core.Name "name")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

aliasDeclType :: (Phantoms.TTerm Syntax.AliasDecl -> Phantoms.TTerm Syntax.Type)
aliasDeclType x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.AliasDecl"),
    Core.projectionField = (Core.Name "type")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

aliasDeclWithName :: (Phantoms.TTerm Syntax.AliasDecl -> Phantoms.TTerm Syntax.Identifier -> Phantoms.TTerm Syntax.AliasDecl)
aliasDeclWithName original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.AliasDecl"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "type"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.AliasDecl"),
          Core.projectionField = (Core.Name "type")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

aliasDeclWithType :: (Phantoms.TTerm Syntax.AliasDecl -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.AliasDecl)
aliasDeclWithType original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.AliasDecl"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.AliasDecl"),
          Core.projectionField = (Core.Name "name")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "type"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

typeDef :: (Phantoms.TTerm Syntax.Identifier -> Phantoms.TTerm (Maybe Syntax.TypeParameters) -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.TypeDef)
typeDef name typeParams type_ = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.TypeDef"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Phantoms.unTTerm name)},
    Core.Field {
      Core.fieldName = (Core.Name "typeParams"),
      Core.fieldTerm = (Phantoms.unTTerm typeParams)},
    Core.Field {
      Core.fieldName = (Core.Name "type"),
      Core.fieldTerm = (Phantoms.unTTerm type_)}]})))

typeDefName :: (Phantoms.TTerm Syntax.TypeDef -> Phantoms.TTerm Syntax.Identifier)
typeDefName x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.TypeDef"),
    Core.projectionField = (Core.Name "name")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

typeDefTypeParams :: (Phantoms.TTerm Syntax.TypeDef -> Phantoms.TTerm (Maybe Syntax.TypeParameters))
typeDefTypeParams x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.TypeDef"),
    Core.projectionField = (Core.Name "typeParams")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

typeDefType :: (Phantoms.TTerm Syntax.TypeDef -> Phantoms.TTerm Syntax.Type)
typeDefType x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.TypeDef"),
    Core.projectionField = (Core.Name "type")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

typeDefWithName :: (Phantoms.TTerm Syntax.TypeDef -> Phantoms.TTerm Syntax.Identifier -> Phantoms.TTerm Syntax.TypeDef)
typeDefWithName original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.TypeDef"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "typeParams"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.TypeDef"),
          Core.projectionField = (Core.Name "typeParams")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "type"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.TypeDef"),
          Core.projectionField = (Core.Name "type")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

typeDefWithTypeParams :: (Phantoms.TTerm Syntax.TypeDef -> Phantoms.TTerm (Maybe Syntax.TypeParameters) -> Phantoms.TTerm Syntax.TypeDef)
typeDefWithTypeParams original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.TypeDef"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.TypeDef"),
          Core.projectionField = (Core.Name "name")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "typeParams"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "type"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.TypeDef"),
          Core.projectionField = (Core.Name "type")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

typeDefWithType :: (Phantoms.TTerm Syntax.TypeDef -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.TypeDef)
typeDefWithType original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.TypeDef"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.TypeDef"),
          Core.projectionField = (Core.Name "name")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "typeParams"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.TypeDef"),
          Core.projectionField = (Core.Name "typeParams")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "type"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

typeParameters :: (Phantoms.TTerm [Syntax.TypeParamDecl] -> Phantoms.TTerm Syntax.TypeParameters)
typeParameters x = (Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
  Core.wrappedTermTypeName = (Core.Name "hydra.ext.go.syntax.TypeParameters"),
  Core.wrappedTermBody = (Phantoms.unTTerm x)})))

unTypeParameters :: (Phantoms.TTerm Syntax.TypeParameters -> Phantoms.TTerm [Syntax.TypeParamDecl])
unTypeParameters x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.go.syntax.TypeParameters")))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

typeParamDecl :: (Phantoms.TTerm [Syntax.Identifier] -> Phantoms.TTerm Syntax.TypeConstraint -> Phantoms.TTerm Syntax.TypeParamDecl)
typeParamDecl names constraint = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.TypeParamDecl"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "names"),
      Core.fieldTerm = (Phantoms.unTTerm names)},
    Core.Field {
      Core.fieldName = (Core.Name "constraint"),
      Core.fieldTerm = (Phantoms.unTTerm constraint)}]})))

typeParamDeclNames :: (Phantoms.TTerm Syntax.TypeParamDecl -> Phantoms.TTerm [Syntax.Identifier])
typeParamDeclNames x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.TypeParamDecl"),
    Core.projectionField = (Core.Name "names")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

typeParamDeclConstraint :: (Phantoms.TTerm Syntax.TypeParamDecl -> Phantoms.TTerm Syntax.TypeConstraint)
typeParamDeclConstraint x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.TypeParamDecl"),
    Core.projectionField = (Core.Name "constraint")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

typeParamDeclWithNames :: (Phantoms.TTerm Syntax.TypeParamDecl -> Phantoms.TTerm [Syntax.Identifier] -> Phantoms.TTerm Syntax.TypeParamDecl)
typeParamDeclWithNames original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.TypeParamDecl"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "names"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "constraint"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.TypeParamDecl"),
          Core.projectionField = (Core.Name "constraint")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

typeParamDeclWithConstraint :: (Phantoms.TTerm Syntax.TypeParamDecl -> Phantoms.TTerm Syntax.TypeConstraint -> Phantoms.TTerm Syntax.TypeParamDecl)
typeParamDeclWithConstraint original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.TypeParamDecl"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "names"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.TypeParamDecl"),
          Core.projectionField = (Core.Name "names")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "constraint"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

typeConstraint :: (Phantoms.TTerm Syntax.TypeElem -> Phantoms.TTerm Syntax.TypeConstraint)
typeConstraint x = (Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
  Core.wrappedTermTypeName = (Core.Name "hydra.ext.go.syntax.TypeConstraint"),
  Core.wrappedTermBody = (Phantoms.unTTerm x)})))

unTypeConstraint :: (Phantoms.TTerm Syntax.TypeConstraint -> Phantoms.TTerm Syntax.TypeElem)
unTypeConstraint x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.go.syntax.TypeConstraint")))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

functionDecl :: (Phantoms.TTerm Syntax.Identifier -> Phantoms.TTerm (Maybe Syntax.TypeParameters) -> Phantoms.TTerm Syntax.Signature -> Phantoms.TTerm (Maybe Syntax.FunctionBody) -> Phantoms.TTerm Syntax.FunctionDecl)
functionDecl name typeParams signature body = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.FunctionDecl"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Phantoms.unTTerm name)},
    Core.Field {
      Core.fieldName = (Core.Name "typeParams"),
      Core.fieldTerm = (Phantoms.unTTerm typeParams)},
    Core.Field {
      Core.fieldName = (Core.Name "signature"),
      Core.fieldTerm = (Phantoms.unTTerm signature)},
    Core.Field {
      Core.fieldName = (Core.Name "body"),
      Core.fieldTerm = (Phantoms.unTTerm body)}]})))

functionDeclName :: (Phantoms.TTerm Syntax.FunctionDecl -> Phantoms.TTerm Syntax.Identifier)
functionDeclName x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.FunctionDecl"),
    Core.projectionField = (Core.Name "name")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

functionDeclTypeParams :: (Phantoms.TTerm Syntax.FunctionDecl -> Phantoms.TTerm (Maybe Syntax.TypeParameters))
functionDeclTypeParams x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.FunctionDecl"),
    Core.projectionField = (Core.Name "typeParams")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

functionDeclSignature :: (Phantoms.TTerm Syntax.FunctionDecl -> Phantoms.TTerm Syntax.Signature)
functionDeclSignature x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.FunctionDecl"),
    Core.projectionField = (Core.Name "signature")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

functionDeclBody :: (Phantoms.TTerm Syntax.FunctionDecl -> Phantoms.TTerm (Maybe Syntax.FunctionBody))
functionDeclBody x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.FunctionDecl"),
    Core.projectionField = (Core.Name "body")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

functionDeclWithName :: (Phantoms.TTerm Syntax.FunctionDecl -> Phantoms.TTerm Syntax.Identifier -> Phantoms.TTerm Syntax.FunctionDecl)
functionDeclWithName original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.FunctionDecl"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "typeParams"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.FunctionDecl"),
          Core.projectionField = (Core.Name "typeParams")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "signature"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.FunctionDecl"),
          Core.projectionField = (Core.Name "signature")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "body"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.FunctionDecl"),
          Core.projectionField = (Core.Name "body")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

functionDeclWithTypeParams :: (Phantoms.TTerm Syntax.FunctionDecl -> Phantoms.TTerm (Maybe Syntax.TypeParameters) -> Phantoms.TTerm Syntax.FunctionDecl)
functionDeclWithTypeParams original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.FunctionDecl"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.FunctionDecl"),
          Core.projectionField = (Core.Name "name")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "typeParams"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "signature"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.FunctionDecl"),
          Core.projectionField = (Core.Name "signature")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "body"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.FunctionDecl"),
          Core.projectionField = (Core.Name "body")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

functionDeclWithSignature :: (Phantoms.TTerm Syntax.FunctionDecl -> Phantoms.TTerm Syntax.Signature -> Phantoms.TTerm Syntax.FunctionDecl)
functionDeclWithSignature original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.FunctionDecl"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.FunctionDecl"),
          Core.projectionField = (Core.Name "name")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "typeParams"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.FunctionDecl"),
          Core.projectionField = (Core.Name "typeParams")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "signature"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "body"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.FunctionDecl"),
          Core.projectionField = (Core.Name "body")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

functionDeclWithBody :: (Phantoms.TTerm Syntax.FunctionDecl -> Phantoms.TTerm (Maybe Syntax.FunctionBody) -> Phantoms.TTerm Syntax.FunctionDecl)
functionDeclWithBody original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.FunctionDecl"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.FunctionDecl"),
          Core.projectionField = (Core.Name "name")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "typeParams"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.FunctionDecl"),
          Core.projectionField = (Core.Name "typeParams")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "signature"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.FunctionDecl"),
          Core.projectionField = (Core.Name "signature")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "body"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

functionBody :: (Phantoms.TTerm Syntax.Block -> Phantoms.TTerm Syntax.FunctionBody)
functionBody x = (Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
  Core.wrappedTermTypeName = (Core.Name "hydra.ext.go.syntax.FunctionBody"),
  Core.wrappedTermBody = (Phantoms.unTTerm x)})))

unFunctionBody :: (Phantoms.TTerm Syntax.FunctionBody -> Phantoms.TTerm Syntax.Block)
unFunctionBody x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.go.syntax.FunctionBody")))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

methodDecl :: (Phantoms.TTerm Syntax.Receiver -> Phantoms.TTerm Syntax.Identifier -> Phantoms.TTerm Syntax.Signature -> Phantoms.TTerm (Maybe Syntax.FunctionBody) -> Phantoms.TTerm Syntax.MethodDecl)
methodDecl receiver name signature body = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.MethodDecl"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "receiver"),
      Core.fieldTerm = (Phantoms.unTTerm receiver)},
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Phantoms.unTTerm name)},
    Core.Field {
      Core.fieldName = (Core.Name "signature"),
      Core.fieldTerm = (Phantoms.unTTerm signature)},
    Core.Field {
      Core.fieldName = (Core.Name "body"),
      Core.fieldTerm = (Phantoms.unTTerm body)}]})))

methodDeclReceiver :: (Phantoms.TTerm Syntax.MethodDecl -> Phantoms.TTerm Syntax.Receiver)
methodDeclReceiver x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.MethodDecl"),
    Core.projectionField = (Core.Name "receiver")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

methodDeclName :: (Phantoms.TTerm Syntax.MethodDecl -> Phantoms.TTerm Syntax.Identifier)
methodDeclName x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.MethodDecl"),
    Core.projectionField = (Core.Name "name")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

methodDeclSignature :: (Phantoms.TTerm Syntax.MethodDecl -> Phantoms.TTerm Syntax.Signature)
methodDeclSignature x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.MethodDecl"),
    Core.projectionField = (Core.Name "signature")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

methodDeclBody :: (Phantoms.TTerm Syntax.MethodDecl -> Phantoms.TTerm (Maybe Syntax.FunctionBody))
methodDeclBody x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.MethodDecl"),
    Core.projectionField = (Core.Name "body")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

methodDeclWithReceiver :: (Phantoms.TTerm Syntax.MethodDecl -> Phantoms.TTerm Syntax.Receiver -> Phantoms.TTerm Syntax.MethodDecl)
methodDeclWithReceiver original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.MethodDecl"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "receiver"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.MethodDecl"),
          Core.projectionField = (Core.Name "name")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "signature"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.MethodDecl"),
          Core.projectionField = (Core.Name "signature")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "body"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.MethodDecl"),
          Core.projectionField = (Core.Name "body")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

methodDeclWithName :: (Phantoms.TTerm Syntax.MethodDecl -> Phantoms.TTerm Syntax.Identifier -> Phantoms.TTerm Syntax.MethodDecl)
methodDeclWithName original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.MethodDecl"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "receiver"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.MethodDecl"),
          Core.projectionField = (Core.Name "receiver")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "signature"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.MethodDecl"),
          Core.projectionField = (Core.Name "signature")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "body"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.MethodDecl"),
          Core.projectionField = (Core.Name "body")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

methodDeclWithSignature :: (Phantoms.TTerm Syntax.MethodDecl -> Phantoms.TTerm Syntax.Signature -> Phantoms.TTerm Syntax.MethodDecl)
methodDeclWithSignature original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.MethodDecl"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "receiver"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.MethodDecl"),
          Core.projectionField = (Core.Name "receiver")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.MethodDecl"),
          Core.projectionField = (Core.Name "name")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "signature"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "body"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.MethodDecl"),
          Core.projectionField = (Core.Name "body")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

methodDeclWithBody :: (Phantoms.TTerm Syntax.MethodDecl -> Phantoms.TTerm (Maybe Syntax.FunctionBody) -> Phantoms.TTerm Syntax.MethodDecl)
methodDeclWithBody original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.MethodDecl"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "receiver"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.MethodDecl"),
          Core.projectionField = (Core.Name "receiver")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.MethodDecl"),
          Core.projectionField = (Core.Name "name")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "signature"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.MethodDecl"),
          Core.projectionField = (Core.Name "signature")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "body"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

receiver :: (Phantoms.TTerm (Maybe Syntax.Identifier) -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.Receiver)
receiver name type_ = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.Receiver"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Phantoms.unTTerm name)},
    Core.Field {
      Core.fieldName = (Core.Name "type"),
      Core.fieldTerm = (Phantoms.unTTerm type_)}]})))

receiverName :: (Phantoms.TTerm Syntax.Receiver -> Phantoms.TTerm (Maybe Syntax.Identifier))
receiverName x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.Receiver"),
    Core.projectionField = (Core.Name "name")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

receiverType :: (Phantoms.TTerm Syntax.Receiver -> Phantoms.TTerm Syntax.Type)
receiverType x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.Receiver"),
    Core.projectionField = (Core.Name "type")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

receiverWithName :: (Phantoms.TTerm Syntax.Receiver -> Phantoms.TTerm (Maybe Syntax.Identifier) -> Phantoms.TTerm Syntax.Receiver)
receiverWithName original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.Receiver"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "type"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.Receiver"),
          Core.projectionField = (Core.Name "type")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

receiverWithType :: (Phantoms.TTerm Syntax.Receiver -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.Receiver)
receiverWithType original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.Receiver"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.Receiver"),
          Core.projectionField = (Core.Name "name")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "type"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

typeName :: (Phantoms.TTerm Syntax.TypeName -> Phantoms.TTerm Syntax.Type)
typeName x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.go.syntax.Type"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "name"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

typeLiteral :: (Phantoms.TTerm Syntax.TypeLit -> Phantoms.TTerm Syntax.Type)
typeLiteral x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.go.syntax.Type"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "literal"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

typeParen :: (Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.Type)
typeParen x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.go.syntax.Type"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "paren"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

typeName_ :: (Phantoms.TTerm Syntax.QualifiedIdent -> Phantoms.TTerm [Syntax.Type] -> Phantoms.TTerm Syntax.TypeName)
typeName_ name typeArgs = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.TypeName"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Phantoms.unTTerm name)},
    Core.Field {
      Core.fieldName = (Core.Name "typeArgs"),
      Core.fieldTerm = (Phantoms.unTTerm typeArgs)}]})))

typeNameName :: (Phantoms.TTerm Syntax.TypeName -> Phantoms.TTerm Syntax.QualifiedIdent)
typeNameName x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.TypeName"),
    Core.projectionField = (Core.Name "name")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

typeNameTypeArgs :: (Phantoms.TTerm Syntax.TypeName -> Phantoms.TTerm [Syntax.Type])
typeNameTypeArgs x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.TypeName"),
    Core.projectionField = (Core.Name "typeArgs")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

typeNameWithName :: (Phantoms.TTerm Syntax.TypeName -> Phantoms.TTerm Syntax.QualifiedIdent -> Phantoms.TTerm Syntax.TypeName)
typeNameWithName original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.TypeName"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "typeArgs"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.TypeName"),
          Core.projectionField = (Core.Name "typeArgs")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

typeNameWithTypeArgs :: (Phantoms.TTerm Syntax.TypeName -> Phantoms.TTerm [Syntax.Type] -> Phantoms.TTerm Syntax.TypeName)
typeNameWithTypeArgs original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.TypeName"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.TypeName"),
          Core.projectionField = (Core.Name "name")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "typeArgs"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

qualifiedIdent :: (Phantoms.TTerm (Maybe Syntax.Identifier) -> Phantoms.TTerm Syntax.Identifier -> Phantoms.TTerm Syntax.QualifiedIdent)
qualifiedIdent package name = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.QualifiedIdent"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "package"),
      Core.fieldTerm = (Phantoms.unTTerm package)},
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Phantoms.unTTerm name)}]})))

qualifiedIdentPackage :: (Phantoms.TTerm Syntax.QualifiedIdent -> Phantoms.TTerm (Maybe Syntax.Identifier))
qualifiedIdentPackage x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.QualifiedIdent"),
    Core.projectionField = (Core.Name "package")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

qualifiedIdentName :: (Phantoms.TTerm Syntax.QualifiedIdent -> Phantoms.TTerm Syntax.Identifier)
qualifiedIdentName x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.QualifiedIdent"),
    Core.projectionField = (Core.Name "name")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

qualifiedIdentWithPackage :: (Phantoms.TTerm Syntax.QualifiedIdent -> Phantoms.TTerm (Maybe Syntax.Identifier) -> Phantoms.TTerm Syntax.QualifiedIdent)
qualifiedIdentWithPackage original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.QualifiedIdent"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "package"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.QualifiedIdent"),
          Core.projectionField = (Core.Name "name")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

qualifiedIdentWithName :: (Phantoms.TTerm Syntax.QualifiedIdent -> Phantoms.TTerm Syntax.Identifier -> Phantoms.TTerm Syntax.QualifiedIdent)
qualifiedIdentWithName original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.QualifiedIdent"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "package"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.QualifiedIdent"),
          Core.projectionField = (Core.Name "package")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

typeLitArray :: (Phantoms.TTerm Syntax.ArrayType -> Phantoms.TTerm Syntax.TypeLit)
typeLitArray x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.go.syntax.TypeLit"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "array"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

typeLitStruct :: (Phantoms.TTerm Syntax.StructType -> Phantoms.TTerm Syntax.TypeLit)
typeLitStruct x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.go.syntax.TypeLit"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "struct"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

typeLitPointer :: (Phantoms.TTerm Syntax.PointerType -> Phantoms.TTerm Syntax.TypeLit)
typeLitPointer x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.go.syntax.TypeLit"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "pointer"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

typeLitFunction :: (Phantoms.TTerm Syntax.FunctionType -> Phantoms.TTerm Syntax.TypeLit)
typeLitFunction x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.go.syntax.TypeLit"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "function"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

typeLitInterface :: (Phantoms.TTerm Syntax.InterfaceType -> Phantoms.TTerm Syntax.TypeLit)
typeLitInterface x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.go.syntax.TypeLit"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "interface"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

typeLitSlice :: (Phantoms.TTerm Syntax.SliceType -> Phantoms.TTerm Syntax.TypeLit)
typeLitSlice x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.go.syntax.TypeLit"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "slice"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

typeLitMap :: (Phantoms.TTerm Syntax.MapType -> Phantoms.TTerm Syntax.TypeLit)
typeLitMap x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.go.syntax.TypeLit"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "map"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

typeLitChannel :: (Phantoms.TTerm Syntax.ChannelType -> Phantoms.TTerm Syntax.TypeLit)
typeLitChannel x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.go.syntax.TypeLit"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "channel"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

arrayType :: (Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.ArrayType)
arrayType length element = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.ArrayType"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "length"),
      Core.fieldTerm = (Phantoms.unTTerm length)},
    Core.Field {
      Core.fieldName = (Core.Name "element"),
      Core.fieldTerm = (Phantoms.unTTerm element)}]})))

arrayTypeLength :: (Phantoms.TTerm Syntax.ArrayType -> Phantoms.TTerm Syntax.Expression)
arrayTypeLength x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.ArrayType"),
    Core.projectionField = (Core.Name "length")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

arrayTypeElement :: (Phantoms.TTerm Syntax.ArrayType -> Phantoms.TTerm Syntax.Type)
arrayTypeElement x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.ArrayType"),
    Core.projectionField = (Core.Name "element")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

arrayTypeWithLength :: (Phantoms.TTerm Syntax.ArrayType -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.ArrayType)
arrayTypeWithLength original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.ArrayType"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "length"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "element"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.ArrayType"),
          Core.projectionField = (Core.Name "element")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

arrayTypeWithElement :: (Phantoms.TTerm Syntax.ArrayType -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.ArrayType)
arrayTypeWithElement original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.ArrayType"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "length"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.ArrayType"),
          Core.projectionField = (Core.Name "length")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "element"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

sliceType :: (Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.SliceType)
sliceType x = (Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
  Core.wrappedTermTypeName = (Core.Name "hydra.ext.go.syntax.SliceType"),
  Core.wrappedTermBody = (Phantoms.unTTerm x)})))

unSliceType :: (Phantoms.TTerm Syntax.SliceType -> Phantoms.TTerm Syntax.Type)
unSliceType x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.go.syntax.SliceType")))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

structType :: (Phantoms.TTerm [Syntax.FieldDecl] -> Phantoms.TTerm Syntax.StructType)
structType x = (Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
  Core.wrappedTermTypeName = (Core.Name "hydra.ext.go.syntax.StructType"),
  Core.wrappedTermBody = (Phantoms.unTTerm x)})))

unStructType :: (Phantoms.TTerm Syntax.StructType -> Phantoms.TTerm [Syntax.FieldDecl])
unStructType x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.go.syntax.StructType")))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

fieldDeclNamed :: (Phantoms.TTerm Syntax.NamedField -> Phantoms.TTerm Syntax.FieldDecl)
fieldDeclNamed x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.go.syntax.FieldDecl"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "named"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

fieldDeclEmbedded :: (Phantoms.TTerm Syntax.EmbeddedField -> Phantoms.TTerm Syntax.FieldDecl)
fieldDeclEmbedded x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.go.syntax.FieldDecl"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "embedded"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

namedField :: (Phantoms.TTerm [Syntax.Identifier] -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm (Maybe Syntax.Tag) -> Phantoms.TTerm Syntax.NamedField)
namedField names type_ tag = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.NamedField"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "names"),
      Core.fieldTerm = (Phantoms.unTTerm names)},
    Core.Field {
      Core.fieldName = (Core.Name "type"),
      Core.fieldTerm = (Phantoms.unTTerm type_)},
    Core.Field {
      Core.fieldName = (Core.Name "tag"),
      Core.fieldTerm = (Phantoms.unTTerm tag)}]})))

namedFieldNames :: (Phantoms.TTerm Syntax.NamedField -> Phantoms.TTerm [Syntax.Identifier])
namedFieldNames x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.NamedField"),
    Core.projectionField = (Core.Name "names")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

namedFieldType :: (Phantoms.TTerm Syntax.NamedField -> Phantoms.TTerm Syntax.Type)
namedFieldType x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.NamedField"),
    Core.projectionField = (Core.Name "type")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

namedFieldTag :: (Phantoms.TTerm Syntax.NamedField -> Phantoms.TTerm (Maybe Syntax.Tag))
namedFieldTag x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.NamedField"),
    Core.projectionField = (Core.Name "tag")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

namedFieldWithNames :: (Phantoms.TTerm Syntax.NamedField -> Phantoms.TTerm [Syntax.Identifier] -> Phantoms.TTerm Syntax.NamedField)
namedFieldWithNames original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.NamedField"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "names"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "type"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.NamedField"),
          Core.projectionField = (Core.Name "type")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "tag"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.NamedField"),
          Core.projectionField = (Core.Name "tag")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

namedFieldWithType :: (Phantoms.TTerm Syntax.NamedField -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.NamedField)
namedFieldWithType original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.NamedField"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "names"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.NamedField"),
          Core.projectionField = (Core.Name "names")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "type"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "tag"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.NamedField"),
          Core.projectionField = (Core.Name "tag")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

namedFieldWithTag :: (Phantoms.TTerm Syntax.NamedField -> Phantoms.TTerm (Maybe Syntax.Tag) -> Phantoms.TTerm Syntax.NamedField)
namedFieldWithTag original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.NamedField"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "names"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.NamedField"),
          Core.projectionField = (Core.Name "names")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "type"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.NamedField"),
          Core.projectionField = (Core.Name "type")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "tag"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

embeddedField :: (Phantoms.TTerm Bool -> Phantoms.TTerm Syntax.TypeName -> Phantoms.TTerm (Maybe Syntax.Tag) -> Phantoms.TTerm Syntax.EmbeddedField)
embeddedField pointer type_ tag = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.EmbeddedField"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "pointer"),
      Core.fieldTerm = (Phantoms.unTTerm pointer)},
    Core.Field {
      Core.fieldName = (Core.Name "type"),
      Core.fieldTerm = (Phantoms.unTTerm type_)},
    Core.Field {
      Core.fieldName = (Core.Name "tag"),
      Core.fieldTerm = (Phantoms.unTTerm tag)}]})))

embeddedFieldPointer :: (Phantoms.TTerm Syntax.EmbeddedField -> Phantoms.TTerm Bool)
embeddedFieldPointer x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.EmbeddedField"),
    Core.projectionField = (Core.Name "pointer")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

embeddedFieldType :: (Phantoms.TTerm Syntax.EmbeddedField -> Phantoms.TTerm Syntax.TypeName)
embeddedFieldType x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.EmbeddedField"),
    Core.projectionField = (Core.Name "type")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

embeddedFieldTag :: (Phantoms.TTerm Syntax.EmbeddedField -> Phantoms.TTerm (Maybe Syntax.Tag))
embeddedFieldTag x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.EmbeddedField"),
    Core.projectionField = (Core.Name "tag")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

embeddedFieldWithPointer :: (Phantoms.TTerm Syntax.EmbeddedField -> Phantoms.TTerm Bool -> Phantoms.TTerm Syntax.EmbeddedField)
embeddedFieldWithPointer original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.EmbeddedField"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "pointer"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "type"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.EmbeddedField"),
          Core.projectionField = (Core.Name "type")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "tag"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.EmbeddedField"),
          Core.projectionField = (Core.Name "tag")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

embeddedFieldWithType :: (Phantoms.TTerm Syntax.EmbeddedField -> Phantoms.TTerm Syntax.TypeName -> Phantoms.TTerm Syntax.EmbeddedField)
embeddedFieldWithType original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.EmbeddedField"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "pointer"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.EmbeddedField"),
          Core.projectionField = (Core.Name "pointer")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "type"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "tag"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.EmbeddedField"),
          Core.projectionField = (Core.Name "tag")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

embeddedFieldWithTag :: (Phantoms.TTerm Syntax.EmbeddedField -> Phantoms.TTerm (Maybe Syntax.Tag) -> Phantoms.TTerm Syntax.EmbeddedField)
embeddedFieldWithTag original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.EmbeddedField"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "pointer"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.EmbeddedField"),
          Core.projectionField = (Core.Name "pointer")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "type"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.EmbeddedField"),
          Core.projectionField = (Core.Name "type")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "tag"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

tag :: (Phantoms.TTerm Syntax.StringLit -> Phantoms.TTerm Syntax.Tag)
tag x = (Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
  Core.wrappedTermTypeName = (Core.Name "hydra.ext.go.syntax.Tag"),
  Core.wrappedTermBody = (Phantoms.unTTerm x)})))

unTag :: (Phantoms.TTerm Syntax.Tag -> Phantoms.TTerm Syntax.StringLit)
unTag x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.go.syntax.Tag")))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

pointerType :: (Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.PointerType)
pointerType x = (Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
  Core.wrappedTermTypeName = (Core.Name "hydra.ext.go.syntax.PointerType"),
  Core.wrappedTermBody = (Phantoms.unTTerm x)})))

unPointerType :: (Phantoms.TTerm Syntax.PointerType -> Phantoms.TTerm Syntax.Type)
unPointerType x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.go.syntax.PointerType")))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

functionType :: (Phantoms.TTerm Syntax.Signature -> Phantoms.TTerm Syntax.FunctionType)
functionType x = (Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
  Core.wrappedTermTypeName = (Core.Name "hydra.ext.go.syntax.FunctionType"),
  Core.wrappedTermBody = (Phantoms.unTTerm x)})))

unFunctionType :: (Phantoms.TTerm Syntax.FunctionType -> Phantoms.TTerm Syntax.Signature)
unFunctionType x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.go.syntax.FunctionType")))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

signature :: (Phantoms.TTerm Syntax.Parameters -> Phantoms.TTerm (Maybe Syntax.Result) -> Phantoms.TTerm Syntax.Signature)
signature parameters result = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.Signature"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "parameters"),
      Core.fieldTerm = (Phantoms.unTTerm parameters)},
    Core.Field {
      Core.fieldName = (Core.Name "result"),
      Core.fieldTerm = (Phantoms.unTTerm result)}]})))

signatureParameters :: (Phantoms.TTerm Syntax.Signature -> Phantoms.TTerm Syntax.Parameters)
signatureParameters x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.Signature"),
    Core.projectionField = (Core.Name "parameters")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

signatureResult :: (Phantoms.TTerm Syntax.Signature -> Phantoms.TTerm (Maybe Syntax.Result))
signatureResult x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.Signature"),
    Core.projectionField = (Core.Name "result")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

signatureWithParameters :: (Phantoms.TTerm Syntax.Signature -> Phantoms.TTerm Syntax.Parameters -> Phantoms.TTerm Syntax.Signature)
signatureWithParameters original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.Signature"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "parameters"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "result"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.Signature"),
          Core.projectionField = (Core.Name "result")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

signatureWithResult :: (Phantoms.TTerm Syntax.Signature -> Phantoms.TTerm (Maybe Syntax.Result) -> Phantoms.TTerm Syntax.Signature)
signatureWithResult original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.Signature"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "parameters"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.Signature"),
          Core.projectionField = (Core.Name "parameters")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "result"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

resultParameters :: (Phantoms.TTerm Syntax.Parameters -> Phantoms.TTerm Syntax.Result)
resultParameters x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.go.syntax.Result"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "parameters"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

resultType :: (Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.Result)
resultType x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.go.syntax.Result"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "type"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

parameters :: (Phantoms.TTerm [Syntax.ParameterDecl] -> Phantoms.TTerm Syntax.Parameters)
parameters x = (Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
  Core.wrappedTermTypeName = (Core.Name "hydra.ext.go.syntax.Parameters"),
  Core.wrappedTermBody = (Phantoms.unTTerm x)})))

unParameters :: (Phantoms.TTerm Syntax.Parameters -> Phantoms.TTerm [Syntax.ParameterDecl])
unParameters x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.go.syntax.Parameters")))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

parameterDecl :: (Phantoms.TTerm [Syntax.Identifier] -> Phantoms.TTerm Bool -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.ParameterDecl)
parameterDecl names variadic type_ = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.ParameterDecl"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "names"),
      Core.fieldTerm = (Phantoms.unTTerm names)},
    Core.Field {
      Core.fieldName = (Core.Name "variadic"),
      Core.fieldTerm = (Phantoms.unTTerm variadic)},
    Core.Field {
      Core.fieldName = (Core.Name "type"),
      Core.fieldTerm = (Phantoms.unTTerm type_)}]})))

parameterDeclNames :: (Phantoms.TTerm Syntax.ParameterDecl -> Phantoms.TTerm [Syntax.Identifier])
parameterDeclNames x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.ParameterDecl"),
    Core.projectionField = (Core.Name "names")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

parameterDeclVariadic :: (Phantoms.TTerm Syntax.ParameterDecl -> Phantoms.TTerm Bool)
parameterDeclVariadic x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.ParameterDecl"),
    Core.projectionField = (Core.Name "variadic")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

parameterDeclType :: (Phantoms.TTerm Syntax.ParameterDecl -> Phantoms.TTerm Syntax.Type)
parameterDeclType x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.ParameterDecl"),
    Core.projectionField = (Core.Name "type")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

parameterDeclWithNames :: (Phantoms.TTerm Syntax.ParameterDecl -> Phantoms.TTerm [Syntax.Identifier] -> Phantoms.TTerm Syntax.ParameterDecl)
parameterDeclWithNames original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.ParameterDecl"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "names"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "variadic"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.ParameterDecl"),
          Core.projectionField = (Core.Name "variadic")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "type"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.ParameterDecl"),
          Core.projectionField = (Core.Name "type")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

parameterDeclWithVariadic :: (Phantoms.TTerm Syntax.ParameterDecl -> Phantoms.TTerm Bool -> Phantoms.TTerm Syntax.ParameterDecl)
parameterDeclWithVariadic original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.ParameterDecl"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "names"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.ParameterDecl"),
          Core.projectionField = (Core.Name "names")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "variadic"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "type"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.ParameterDecl"),
          Core.projectionField = (Core.Name "type")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

parameterDeclWithType :: (Phantoms.TTerm Syntax.ParameterDecl -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.ParameterDecl)
parameterDeclWithType original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.ParameterDecl"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "names"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.ParameterDecl"),
          Core.projectionField = (Core.Name "names")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "variadic"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.ParameterDecl"),
          Core.projectionField = (Core.Name "variadic")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "type"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

interfaceType :: (Phantoms.TTerm [Syntax.InterfaceElem] -> Phantoms.TTerm Syntax.InterfaceType)
interfaceType x = (Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
  Core.wrappedTermTypeName = (Core.Name "hydra.ext.go.syntax.InterfaceType"),
  Core.wrappedTermBody = (Phantoms.unTTerm x)})))

unInterfaceType :: (Phantoms.TTerm Syntax.InterfaceType -> Phantoms.TTerm [Syntax.InterfaceElem])
unInterfaceType x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.go.syntax.InterfaceType")))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

interfaceElemMethod :: (Phantoms.TTerm Syntax.MethodElem -> Phantoms.TTerm Syntax.InterfaceElem)
interfaceElemMethod x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.go.syntax.InterfaceElem"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "method"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

interfaceElemType :: (Phantoms.TTerm Syntax.TypeElem -> Phantoms.TTerm Syntax.InterfaceElem)
interfaceElemType x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.go.syntax.InterfaceElem"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "type"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

methodElem :: (Phantoms.TTerm Syntax.Identifier -> Phantoms.TTerm Syntax.Signature -> Phantoms.TTerm Syntax.MethodElem)
methodElem name signature = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.MethodElem"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Phantoms.unTTerm name)},
    Core.Field {
      Core.fieldName = (Core.Name "signature"),
      Core.fieldTerm = (Phantoms.unTTerm signature)}]})))

methodElemName :: (Phantoms.TTerm Syntax.MethodElem -> Phantoms.TTerm Syntax.Identifier)
methodElemName x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.MethodElem"),
    Core.projectionField = (Core.Name "name")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

methodElemSignature :: (Phantoms.TTerm Syntax.MethodElem -> Phantoms.TTerm Syntax.Signature)
methodElemSignature x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.MethodElem"),
    Core.projectionField = (Core.Name "signature")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

methodElemWithName :: (Phantoms.TTerm Syntax.MethodElem -> Phantoms.TTerm Syntax.Identifier -> Phantoms.TTerm Syntax.MethodElem)
methodElemWithName original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.MethodElem"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "signature"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.MethodElem"),
          Core.projectionField = (Core.Name "signature")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

methodElemWithSignature :: (Phantoms.TTerm Syntax.MethodElem -> Phantoms.TTerm Syntax.Signature -> Phantoms.TTerm Syntax.MethodElem)
methodElemWithSignature original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.MethodElem"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.MethodElem"),
          Core.projectionField = (Core.Name "name")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "signature"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

typeElem :: (Phantoms.TTerm [Syntax.TypeTerm] -> Phantoms.TTerm Syntax.TypeElem)
typeElem x = (Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
  Core.wrappedTermTypeName = (Core.Name "hydra.ext.go.syntax.TypeElem"),
  Core.wrappedTermBody = (Phantoms.unTTerm x)})))

unTypeElem :: (Phantoms.TTerm Syntax.TypeElem -> Phantoms.TTerm [Syntax.TypeTerm])
unTypeElem x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.go.syntax.TypeElem")))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

typeTerm :: (Phantoms.TTerm Bool -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.TypeTerm)
typeTerm underlying type_ = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.TypeTerm"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "underlying"),
      Core.fieldTerm = (Phantoms.unTTerm underlying)},
    Core.Field {
      Core.fieldName = (Core.Name "type"),
      Core.fieldTerm = (Phantoms.unTTerm type_)}]})))

typeTermUnderlying :: (Phantoms.TTerm Syntax.TypeTerm -> Phantoms.TTerm Bool)
typeTermUnderlying x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.TypeTerm"),
    Core.projectionField = (Core.Name "underlying")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

typeTermType :: (Phantoms.TTerm Syntax.TypeTerm -> Phantoms.TTerm Syntax.Type)
typeTermType x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.TypeTerm"),
    Core.projectionField = (Core.Name "type")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

typeTermWithUnderlying :: (Phantoms.TTerm Syntax.TypeTerm -> Phantoms.TTerm Bool -> Phantoms.TTerm Syntax.TypeTerm)
typeTermWithUnderlying original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.TypeTerm"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "underlying"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "type"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.TypeTerm"),
          Core.projectionField = (Core.Name "type")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

typeTermWithType :: (Phantoms.TTerm Syntax.TypeTerm -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.TypeTerm)
typeTermWithType original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.TypeTerm"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "underlying"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.TypeTerm"),
          Core.projectionField = (Core.Name "underlying")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "type"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

mapType :: (Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.MapType)
mapType key value = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.MapType"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "key"),
      Core.fieldTerm = (Phantoms.unTTerm key)},
    Core.Field {
      Core.fieldName = (Core.Name "value"),
      Core.fieldTerm = (Phantoms.unTTerm value)}]})))

mapTypeKey :: (Phantoms.TTerm Syntax.MapType -> Phantoms.TTerm Syntax.Type)
mapTypeKey x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.MapType"),
    Core.projectionField = (Core.Name "key")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

mapTypeValue :: (Phantoms.TTerm Syntax.MapType -> Phantoms.TTerm Syntax.Type)
mapTypeValue x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.MapType"),
    Core.projectionField = (Core.Name "value")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

mapTypeWithKey :: (Phantoms.TTerm Syntax.MapType -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.MapType)
mapTypeWithKey original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.MapType"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "key"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "value"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.MapType"),
          Core.projectionField = (Core.Name "value")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

mapTypeWithValue :: (Phantoms.TTerm Syntax.MapType -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.MapType)
mapTypeWithValue original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.MapType"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "key"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.MapType"),
          Core.projectionField = (Core.Name "key")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "value"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

channelType :: (Phantoms.TTerm Syntax.ChannelDirection -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.ChannelType)
channelType direction element = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.ChannelType"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "direction"),
      Core.fieldTerm = (Phantoms.unTTerm direction)},
    Core.Field {
      Core.fieldName = (Core.Name "element"),
      Core.fieldTerm = (Phantoms.unTTerm element)}]})))

channelTypeDirection :: (Phantoms.TTerm Syntax.ChannelType -> Phantoms.TTerm Syntax.ChannelDirection)
channelTypeDirection x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.ChannelType"),
    Core.projectionField = (Core.Name "direction")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

channelTypeElement :: (Phantoms.TTerm Syntax.ChannelType -> Phantoms.TTerm Syntax.Type)
channelTypeElement x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.ChannelType"),
    Core.projectionField = (Core.Name "element")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

channelTypeWithDirection :: (Phantoms.TTerm Syntax.ChannelType -> Phantoms.TTerm Syntax.ChannelDirection -> Phantoms.TTerm Syntax.ChannelType)
channelTypeWithDirection original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.ChannelType"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "direction"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "element"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.ChannelType"),
          Core.projectionField = (Core.Name "element")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

channelTypeWithElement :: (Phantoms.TTerm Syntax.ChannelType -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.ChannelType)
channelTypeWithElement original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.ChannelType"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "direction"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.ChannelType"),
          Core.projectionField = (Core.Name "direction")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "element"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

channelDirectionBidirectional :: (Phantoms.TTerm Syntax.ChannelDirection)
channelDirectionBidirectional = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.go.syntax.ChannelDirection"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "bidirectional"),
    Core.fieldTerm = Core.TermUnit}})))

channelDirectionSend :: (Phantoms.TTerm Syntax.ChannelDirection)
channelDirectionSend = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.go.syntax.ChannelDirection"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "send"),
    Core.fieldTerm = Core.TermUnit}})))

channelDirectionReceive :: (Phantoms.TTerm Syntax.ChannelDirection)
channelDirectionReceive = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.go.syntax.ChannelDirection"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "receive"),
    Core.fieldTerm = Core.TermUnit}})))

expressionUnary :: (Phantoms.TTerm Syntax.UnaryExpr -> Phantoms.TTerm Syntax.Expression)
expressionUnary x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.go.syntax.Expression"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "unary"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

expressionBinary :: (Phantoms.TTerm Syntax.BinaryExpr -> Phantoms.TTerm Syntax.Expression)
expressionBinary x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.go.syntax.Expression"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "binary"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

unaryExprPrimary :: (Phantoms.TTerm Syntax.PrimaryExpr -> Phantoms.TTerm Syntax.UnaryExpr)
unaryExprPrimary x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.go.syntax.UnaryExpr"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "primary"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

unaryExprOp :: (Phantoms.TTerm Syntax.UnaryOperation -> Phantoms.TTerm Syntax.UnaryExpr)
unaryExprOp x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.go.syntax.UnaryExpr"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "op"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

unaryOperation :: (Phantoms.TTerm Syntax.UnaryOp -> Phantoms.TTerm Syntax.UnaryExpr -> Phantoms.TTerm Syntax.UnaryOperation)
unaryOperation op operand = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.UnaryOperation"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "op"),
      Core.fieldTerm = (Phantoms.unTTerm op)},
    Core.Field {
      Core.fieldName = (Core.Name "operand"),
      Core.fieldTerm = (Phantoms.unTTerm operand)}]})))

unaryOperationOp :: (Phantoms.TTerm Syntax.UnaryOperation -> Phantoms.TTerm Syntax.UnaryOp)
unaryOperationOp x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.UnaryOperation"),
    Core.projectionField = (Core.Name "op")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

unaryOperationOperand :: (Phantoms.TTerm Syntax.UnaryOperation -> Phantoms.TTerm Syntax.UnaryExpr)
unaryOperationOperand x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.UnaryOperation"),
    Core.projectionField = (Core.Name "operand")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

unaryOperationWithOp :: (Phantoms.TTerm Syntax.UnaryOperation -> Phantoms.TTerm Syntax.UnaryOp -> Phantoms.TTerm Syntax.UnaryOperation)
unaryOperationWithOp original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.UnaryOperation"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "op"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "operand"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.UnaryOperation"),
          Core.projectionField = (Core.Name "operand")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

unaryOperationWithOperand :: (Phantoms.TTerm Syntax.UnaryOperation -> Phantoms.TTerm Syntax.UnaryExpr -> Phantoms.TTerm Syntax.UnaryOperation)
unaryOperationWithOperand original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.UnaryOperation"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "op"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.UnaryOperation"),
          Core.projectionField = (Core.Name "op")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "operand"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

binaryExpr :: (Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.BinaryOp -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.BinaryExpr)
binaryExpr left op right = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.BinaryExpr"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "left"),
      Core.fieldTerm = (Phantoms.unTTerm left)},
    Core.Field {
      Core.fieldName = (Core.Name "op"),
      Core.fieldTerm = (Phantoms.unTTerm op)},
    Core.Field {
      Core.fieldName = (Core.Name "right"),
      Core.fieldTerm = (Phantoms.unTTerm right)}]})))

binaryExprLeft :: (Phantoms.TTerm Syntax.BinaryExpr -> Phantoms.TTerm Syntax.Expression)
binaryExprLeft x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.BinaryExpr"),
    Core.projectionField = (Core.Name "left")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

binaryExprOp :: (Phantoms.TTerm Syntax.BinaryExpr -> Phantoms.TTerm Syntax.BinaryOp)
binaryExprOp x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.BinaryExpr"),
    Core.projectionField = (Core.Name "op")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

binaryExprRight :: (Phantoms.TTerm Syntax.BinaryExpr -> Phantoms.TTerm Syntax.Expression)
binaryExprRight x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.BinaryExpr"),
    Core.projectionField = (Core.Name "right")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

binaryExprWithLeft :: (Phantoms.TTerm Syntax.BinaryExpr -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.BinaryExpr)
binaryExprWithLeft original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.BinaryExpr"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "left"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "op"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.BinaryExpr"),
          Core.projectionField = (Core.Name "op")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "right"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.BinaryExpr"),
          Core.projectionField = (Core.Name "right")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

binaryExprWithOp :: (Phantoms.TTerm Syntax.BinaryExpr -> Phantoms.TTerm Syntax.BinaryOp -> Phantoms.TTerm Syntax.BinaryExpr)
binaryExprWithOp original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.BinaryExpr"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "left"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.BinaryExpr"),
          Core.projectionField = (Core.Name "left")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "op"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "right"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.BinaryExpr"),
          Core.projectionField = (Core.Name "right")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

binaryExprWithRight :: (Phantoms.TTerm Syntax.BinaryExpr -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.BinaryExpr)
binaryExprWithRight original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.BinaryExpr"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "left"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.BinaryExpr"),
          Core.projectionField = (Core.Name "left")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "op"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.BinaryExpr"),
          Core.projectionField = (Core.Name "op")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "right"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

binaryOpOr :: (Phantoms.TTerm Syntax.BinaryOp)
binaryOpOr = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.go.syntax.BinaryOp"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "or"),
    Core.fieldTerm = Core.TermUnit}})))

binaryOpAnd :: (Phantoms.TTerm Syntax.BinaryOp)
binaryOpAnd = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.go.syntax.BinaryOp"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "and"),
    Core.fieldTerm = Core.TermUnit}})))

binaryOpEqual :: (Phantoms.TTerm Syntax.BinaryOp)
binaryOpEqual = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.go.syntax.BinaryOp"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "equal"),
    Core.fieldTerm = Core.TermUnit}})))

binaryOpNotEqual :: (Phantoms.TTerm Syntax.BinaryOp)
binaryOpNotEqual = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.go.syntax.BinaryOp"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "notEqual"),
    Core.fieldTerm = Core.TermUnit}})))

binaryOpLess :: (Phantoms.TTerm Syntax.BinaryOp)
binaryOpLess = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.go.syntax.BinaryOp"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "less"),
    Core.fieldTerm = Core.TermUnit}})))

binaryOpLessEqual :: (Phantoms.TTerm Syntax.BinaryOp)
binaryOpLessEqual = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.go.syntax.BinaryOp"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "lessEqual"),
    Core.fieldTerm = Core.TermUnit}})))

binaryOpGreater :: (Phantoms.TTerm Syntax.BinaryOp)
binaryOpGreater = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.go.syntax.BinaryOp"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "greater"),
    Core.fieldTerm = Core.TermUnit}})))

binaryOpGreaterEqual :: (Phantoms.TTerm Syntax.BinaryOp)
binaryOpGreaterEqual = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.go.syntax.BinaryOp"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "greaterEqual"),
    Core.fieldTerm = Core.TermUnit}})))

binaryOpAdd :: (Phantoms.TTerm Syntax.BinaryOp)
binaryOpAdd = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.go.syntax.BinaryOp"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "add"),
    Core.fieldTerm = Core.TermUnit}})))

binaryOpSubtract :: (Phantoms.TTerm Syntax.BinaryOp)
binaryOpSubtract = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.go.syntax.BinaryOp"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "subtract"),
    Core.fieldTerm = Core.TermUnit}})))

binaryOpBitwiseOr :: (Phantoms.TTerm Syntax.BinaryOp)
binaryOpBitwiseOr = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.go.syntax.BinaryOp"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "bitwiseOr"),
    Core.fieldTerm = Core.TermUnit}})))

binaryOpBitwiseXor :: (Phantoms.TTerm Syntax.BinaryOp)
binaryOpBitwiseXor = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.go.syntax.BinaryOp"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "bitwiseXor"),
    Core.fieldTerm = Core.TermUnit}})))

binaryOpMultiply :: (Phantoms.TTerm Syntax.BinaryOp)
binaryOpMultiply = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.go.syntax.BinaryOp"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "multiply"),
    Core.fieldTerm = Core.TermUnit}})))

binaryOpDivide :: (Phantoms.TTerm Syntax.BinaryOp)
binaryOpDivide = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.go.syntax.BinaryOp"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "divide"),
    Core.fieldTerm = Core.TermUnit}})))

binaryOpRemainder :: (Phantoms.TTerm Syntax.BinaryOp)
binaryOpRemainder = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.go.syntax.BinaryOp"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "remainder"),
    Core.fieldTerm = Core.TermUnit}})))

binaryOpLeftShift :: (Phantoms.TTerm Syntax.BinaryOp)
binaryOpLeftShift = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.go.syntax.BinaryOp"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "leftShift"),
    Core.fieldTerm = Core.TermUnit}})))

binaryOpRightShift :: (Phantoms.TTerm Syntax.BinaryOp)
binaryOpRightShift = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.go.syntax.BinaryOp"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "rightShift"),
    Core.fieldTerm = Core.TermUnit}})))

binaryOpBitwiseAnd :: (Phantoms.TTerm Syntax.BinaryOp)
binaryOpBitwiseAnd = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.go.syntax.BinaryOp"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "bitwiseAnd"),
    Core.fieldTerm = Core.TermUnit}})))

binaryOpBitClear :: (Phantoms.TTerm Syntax.BinaryOp)
binaryOpBitClear = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.go.syntax.BinaryOp"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "bitClear"),
    Core.fieldTerm = Core.TermUnit}})))

primaryExprOperand :: (Phantoms.TTerm Syntax.Operand -> Phantoms.TTerm Syntax.PrimaryExpr)
primaryExprOperand x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.go.syntax.PrimaryExpr"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "operand"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

primaryExprConversion :: (Phantoms.TTerm Syntax.Conversion -> Phantoms.TTerm Syntax.PrimaryExpr)
primaryExprConversion x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.go.syntax.PrimaryExpr"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "conversion"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

primaryExprMethodExpr :: (Phantoms.TTerm Syntax.MethodExpr -> Phantoms.TTerm Syntax.PrimaryExpr)
primaryExprMethodExpr x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.go.syntax.PrimaryExpr"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "methodExpr"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

primaryExprSelector :: (Phantoms.TTerm Syntax.SelectorExpr -> Phantoms.TTerm Syntax.PrimaryExpr)
primaryExprSelector x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.go.syntax.PrimaryExpr"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "selector"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

primaryExprIndex :: (Phantoms.TTerm Syntax.IndexExpr -> Phantoms.TTerm Syntax.PrimaryExpr)
primaryExprIndex x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.go.syntax.PrimaryExpr"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "index"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

primaryExprSlice :: (Phantoms.TTerm Syntax.SliceExpr -> Phantoms.TTerm Syntax.PrimaryExpr)
primaryExprSlice x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.go.syntax.PrimaryExpr"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "slice"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

primaryExprTypeAssertion :: (Phantoms.TTerm Syntax.TypeAssertionExpr -> Phantoms.TTerm Syntax.PrimaryExpr)
primaryExprTypeAssertion x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.go.syntax.PrimaryExpr"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "typeAssertion"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

primaryExprCall :: (Phantoms.TTerm Syntax.CallExpr -> Phantoms.TTerm Syntax.PrimaryExpr)
primaryExprCall x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.go.syntax.PrimaryExpr"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "call"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

selectorExpr :: (Phantoms.TTerm Syntax.PrimaryExpr -> Phantoms.TTerm Syntax.Identifier -> Phantoms.TTerm Syntax.SelectorExpr)
selectorExpr expr selector = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.SelectorExpr"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "expr"),
      Core.fieldTerm = (Phantoms.unTTerm expr)},
    Core.Field {
      Core.fieldName = (Core.Name "selector"),
      Core.fieldTerm = (Phantoms.unTTerm selector)}]})))

selectorExprExpr :: (Phantoms.TTerm Syntax.SelectorExpr -> Phantoms.TTerm Syntax.PrimaryExpr)
selectorExprExpr x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.SelectorExpr"),
    Core.projectionField = (Core.Name "expr")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

selectorExprSelector :: (Phantoms.TTerm Syntax.SelectorExpr -> Phantoms.TTerm Syntax.Identifier)
selectorExprSelector x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.SelectorExpr"),
    Core.projectionField = (Core.Name "selector")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

selectorExprWithExpr :: (Phantoms.TTerm Syntax.SelectorExpr -> Phantoms.TTerm Syntax.PrimaryExpr -> Phantoms.TTerm Syntax.SelectorExpr)
selectorExprWithExpr original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.SelectorExpr"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "expr"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "selector"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.SelectorExpr"),
          Core.projectionField = (Core.Name "selector")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

selectorExprWithSelector :: (Phantoms.TTerm Syntax.SelectorExpr -> Phantoms.TTerm Syntax.Identifier -> Phantoms.TTerm Syntax.SelectorExpr)
selectorExprWithSelector original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.SelectorExpr"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "expr"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.SelectorExpr"),
          Core.projectionField = (Core.Name "expr")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "selector"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

indexExpr :: (Phantoms.TTerm Syntax.PrimaryExpr -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.IndexExpr)
indexExpr expr index = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.IndexExpr"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "expr"),
      Core.fieldTerm = (Phantoms.unTTerm expr)},
    Core.Field {
      Core.fieldName = (Core.Name "index"),
      Core.fieldTerm = (Phantoms.unTTerm index)}]})))

indexExprExpr :: (Phantoms.TTerm Syntax.IndexExpr -> Phantoms.TTerm Syntax.PrimaryExpr)
indexExprExpr x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.IndexExpr"),
    Core.projectionField = (Core.Name "expr")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

indexExprIndex :: (Phantoms.TTerm Syntax.IndexExpr -> Phantoms.TTerm Syntax.Expression)
indexExprIndex x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.IndexExpr"),
    Core.projectionField = (Core.Name "index")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

indexExprWithExpr :: (Phantoms.TTerm Syntax.IndexExpr -> Phantoms.TTerm Syntax.PrimaryExpr -> Phantoms.TTerm Syntax.IndexExpr)
indexExprWithExpr original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.IndexExpr"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "expr"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "index"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.IndexExpr"),
          Core.projectionField = (Core.Name "index")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

indexExprWithIndex :: (Phantoms.TTerm Syntax.IndexExpr -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.IndexExpr)
indexExprWithIndex original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.IndexExpr"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "expr"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.IndexExpr"),
          Core.projectionField = (Core.Name "expr")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "index"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

sliceExpr :: (Phantoms.TTerm Syntax.PrimaryExpr -> Phantoms.TTerm Syntax.Slice -> Phantoms.TTerm Syntax.SliceExpr)
sliceExpr expr slice = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.SliceExpr"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "expr"),
      Core.fieldTerm = (Phantoms.unTTerm expr)},
    Core.Field {
      Core.fieldName = (Core.Name "slice"),
      Core.fieldTerm = (Phantoms.unTTerm slice)}]})))

sliceExprExpr :: (Phantoms.TTerm Syntax.SliceExpr -> Phantoms.TTerm Syntax.PrimaryExpr)
sliceExprExpr x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.SliceExpr"),
    Core.projectionField = (Core.Name "expr")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

sliceExprSlice :: (Phantoms.TTerm Syntax.SliceExpr -> Phantoms.TTerm Syntax.Slice)
sliceExprSlice x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.SliceExpr"),
    Core.projectionField = (Core.Name "slice")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

sliceExprWithExpr :: (Phantoms.TTerm Syntax.SliceExpr -> Phantoms.TTerm Syntax.PrimaryExpr -> Phantoms.TTerm Syntax.SliceExpr)
sliceExprWithExpr original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.SliceExpr"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "expr"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "slice"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.SliceExpr"),
          Core.projectionField = (Core.Name "slice")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

sliceExprWithSlice :: (Phantoms.TTerm Syntax.SliceExpr -> Phantoms.TTerm Syntax.Slice -> Phantoms.TTerm Syntax.SliceExpr)
sliceExprWithSlice original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.SliceExpr"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "expr"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.SliceExpr"),
          Core.projectionField = (Core.Name "expr")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "slice"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

typeAssertionExpr :: (Phantoms.TTerm Syntax.PrimaryExpr -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.TypeAssertionExpr)
typeAssertionExpr expr type_ = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.TypeAssertionExpr"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "expr"),
      Core.fieldTerm = (Phantoms.unTTerm expr)},
    Core.Field {
      Core.fieldName = (Core.Name "type"),
      Core.fieldTerm = (Phantoms.unTTerm type_)}]})))

typeAssertionExprExpr :: (Phantoms.TTerm Syntax.TypeAssertionExpr -> Phantoms.TTerm Syntax.PrimaryExpr)
typeAssertionExprExpr x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.TypeAssertionExpr"),
    Core.projectionField = (Core.Name "expr")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

typeAssertionExprType :: (Phantoms.TTerm Syntax.TypeAssertionExpr -> Phantoms.TTerm Syntax.Type)
typeAssertionExprType x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.TypeAssertionExpr"),
    Core.projectionField = (Core.Name "type")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

typeAssertionExprWithExpr :: (Phantoms.TTerm Syntax.TypeAssertionExpr -> Phantoms.TTerm Syntax.PrimaryExpr -> Phantoms.TTerm Syntax.TypeAssertionExpr)
typeAssertionExprWithExpr original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.TypeAssertionExpr"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "expr"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "type"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.TypeAssertionExpr"),
          Core.projectionField = (Core.Name "type")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

typeAssertionExprWithType :: (Phantoms.TTerm Syntax.TypeAssertionExpr -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.TypeAssertionExpr)
typeAssertionExprWithType original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.TypeAssertionExpr"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "expr"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.TypeAssertionExpr"),
          Core.projectionField = (Core.Name "expr")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "type"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

callExpr :: (Phantoms.TTerm Syntax.PrimaryExpr -> Phantoms.TTerm Syntax.Arguments -> Phantoms.TTerm Syntax.CallExpr)
callExpr function arguments = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.CallExpr"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "function"),
      Core.fieldTerm = (Phantoms.unTTerm function)},
    Core.Field {
      Core.fieldName = (Core.Name "arguments"),
      Core.fieldTerm = (Phantoms.unTTerm arguments)}]})))

callExprFunction :: (Phantoms.TTerm Syntax.CallExpr -> Phantoms.TTerm Syntax.PrimaryExpr)
callExprFunction x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.CallExpr"),
    Core.projectionField = (Core.Name "function")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

callExprArguments :: (Phantoms.TTerm Syntax.CallExpr -> Phantoms.TTerm Syntax.Arguments)
callExprArguments x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.CallExpr"),
    Core.projectionField = (Core.Name "arguments")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

callExprWithFunction :: (Phantoms.TTerm Syntax.CallExpr -> Phantoms.TTerm Syntax.PrimaryExpr -> Phantoms.TTerm Syntax.CallExpr)
callExprWithFunction original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.CallExpr"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "function"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "arguments"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.CallExpr"),
          Core.projectionField = (Core.Name "arguments")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

callExprWithArguments :: (Phantoms.TTerm Syntax.CallExpr -> Phantoms.TTerm Syntax.Arguments -> Phantoms.TTerm Syntax.CallExpr)
callExprWithArguments original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.CallExpr"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "function"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.CallExpr"),
          Core.projectionField = (Core.Name "function")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "arguments"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

operandLiteral :: (Phantoms.TTerm Syntax.Literal -> Phantoms.TTerm Syntax.Operand)
operandLiteral x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.go.syntax.Operand"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "literal"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

operandName :: (Phantoms.TTerm Syntax.OperandName -> Phantoms.TTerm Syntax.Operand)
operandName x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.go.syntax.Operand"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "name"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

operandParen :: (Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.Operand)
operandParen x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.go.syntax.Operand"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "paren"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

operandName_ :: (Phantoms.TTerm Syntax.QualifiedIdent -> Phantoms.TTerm [Syntax.Type] -> Phantoms.TTerm Syntax.OperandName)
operandName_ name typeArgs = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.OperandName"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Phantoms.unTTerm name)},
    Core.Field {
      Core.fieldName = (Core.Name "typeArgs"),
      Core.fieldTerm = (Phantoms.unTTerm typeArgs)}]})))

operandNameName :: (Phantoms.TTerm Syntax.OperandName -> Phantoms.TTerm Syntax.QualifiedIdent)
operandNameName x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.OperandName"),
    Core.projectionField = (Core.Name "name")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

operandNameTypeArgs :: (Phantoms.TTerm Syntax.OperandName -> Phantoms.TTerm [Syntax.Type])
operandNameTypeArgs x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.OperandName"),
    Core.projectionField = (Core.Name "typeArgs")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

operandNameWithName :: (Phantoms.TTerm Syntax.OperandName -> Phantoms.TTerm Syntax.QualifiedIdent -> Phantoms.TTerm Syntax.OperandName)
operandNameWithName original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.OperandName"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "typeArgs"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.OperandName"),
          Core.projectionField = (Core.Name "typeArgs")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

operandNameWithTypeArgs :: (Phantoms.TTerm Syntax.OperandName -> Phantoms.TTerm [Syntax.Type] -> Phantoms.TTerm Syntax.OperandName)
operandNameWithTypeArgs original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.OperandName"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.OperandName"),
          Core.projectionField = (Core.Name "name")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "typeArgs"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

literalBasic :: (Phantoms.TTerm Syntax.BasicLit -> Phantoms.TTerm Syntax.Literal)
literalBasic x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.go.syntax.Literal"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "basic"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

literalComposite :: (Phantoms.TTerm Syntax.CompositeLit -> Phantoms.TTerm Syntax.Literal)
literalComposite x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.go.syntax.Literal"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "composite"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

literalFunction :: (Phantoms.TTerm Syntax.FunctionLit -> Phantoms.TTerm Syntax.Literal)
literalFunction x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.go.syntax.Literal"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "function"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

basicLitInt :: (Phantoms.TTerm Syntax.IntLit -> Phantoms.TTerm Syntax.BasicLit)
basicLitInt x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.go.syntax.BasicLit"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "int"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

basicLitFloat :: (Phantoms.TTerm Syntax.FloatLit -> Phantoms.TTerm Syntax.BasicLit)
basicLitFloat x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.go.syntax.BasicLit"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "float"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

basicLitImaginary :: (Phantoms.TTerm Syntax.ImaginaryLit -> Phantoms.TTerm Syntax.BasicLit)
basicLitImaginary x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.go.syntax.BasicLit"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "imaginary"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

basicLitRune :: (Phantoms.TTerm Syntax.RuneLit -> Phantoms.TTerm Syntax.BasicLit)
basicLitRune x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.go.syntax.BasicLit"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "rune"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

basicLitString :: (Phantoms.TTerm Syntax.StringLit -> Phantoms.TTerm Syntax.BasicLit)
basicLitString x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.go.syntax.BasicLit"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "string"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

compositeLit :: (Phantoms.TTerm Syntax.LiteralType -> Phantoms.TTerm Syntax.LiteralValue -> Phantoms.TTerm Syntax.CompositeLit)
compositeLit type_ value = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.CompositeLit"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "type"),
      Core.fieldTerm = (Phantoms.unTTerm type_)},
    Core.Field {
      Core.fieldName = (Core.Name "value"),
      Core.fieldTerm = (Phantoms.unTTerm value)}]})))

compositeLitType :: (Phantoms.TTerm Syntax.CompositeLit -> Phantoms.TTerm Syntax.LiteralType)
compositeLitType x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.CompositeLit"),
    Core.projectionField = (Core.Name "type")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

compositeLitValue :: (Phantoms.TTerm Syntax.CompositeLit -> Phantoms.TTerm Syntax.LiteralValue)
compositeLitValue x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.CompositeLit"),
    Core.projectionField = (Core.Name "value")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

compositeLitWithType :: (Phantoms.TTerm Syntax.CompositeLit -> Phantoms.TTerm Syntax.LiteralType -> Phantoms.TTerm Syntax.CompositeLit)
compositeLitWithType original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.CompositeLit"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "type"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "value"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.CompositeLit"),
          Core.projectionField = (Core.Name "value")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

compositeLitWithValue :: (Phantoms.TTerm Syntax.CompositeLit -> Phantoms.TTerm Syntax.LiteralValue -> Phantoms.TTerm Syntax.CompositeLit)
compositeLitWithValue original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.CompositeLit"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "type"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.CompositeLit"),
          Core.projectionField = (Core.Name "type")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "value"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

literalTypeStruct :: (Phantoms.TTerm Syntax.StructType -> Phantoms.TTerm Syntax.LiteralType)
literalTypeStruct x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.go.syntax.LiteralType"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "struct"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

literalTypeArray :: (Phantoms.TTerm Syntax.ArrayType -> Phantoms.TTerm Syntax.LiteralType)
literalTypeArray x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.go.syntax.LiteralType"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "array"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

literalTypeInferredArray :: (Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.LiteralType)
literalTypeInferredArray x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.go.syntax.LiteralType"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "inferredArray"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

literalTypeSlice :: (Phantoms.TTerm Syntax.SliceType -> Phantoms.TTerm Syntax.LiteralType)
literalTypeSlice x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.go.syntax.LiteralType"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "slice"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

literalTypeMap :: (Phantoms.TTerm Syntax.MapType -> Phantoms.TTerm Syntax.LiteralType)
literalTypeMap x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.go.syntax.LiteralType"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "map"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

literalTypeName :: (Phantoms.TTerm Syntax.TypeName -> Phantoms.TTerm Syntax.LiteralType)
literalTypeName x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.go.syntax.LiteralType"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "name"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

literalValue :: (Phantoms.TTerm [Syntax.KeyedElement] -> Phantoms.TTerm Syntax.LiteralValue)
literalValue x = (Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
  Core.wrappedTermTypeName = (Core.Name "hydra.ext.go.syntax.LiteralValue"),
  Core.wrappedTermBody = (Phantoms.unTTerm x)})))

unLiteralValue :: (Phantoms.TTerm Syntax.LiteralValue -> Phantoms.TTerm [Syntax.KeyedElement])
unLiteralValue x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.go.syntax.LiteralValue")))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

elementList :: (Phantoms.TTerm [Syntax.KeyedElement] -> Phantoms.TTerm Syntax.ElementList)
elementList x = (Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
  Core.wrappedTermTypeName = (Core.Name "hydra.ext.go.syntax.ElementList"),
  Core.wrappedTermBody = (Phantoms.unTTerm x)})))

unElementList :: (Phantoms.TTerm Syntax.ElementList -> Phantoms.TTerm [Syntax.KeyedElement])
unElementList x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.go.syntax.ElementList")))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

keyedElement :: (Phantoms.TTerm (Maybe Syntax.Key) -> Phantoms.TTerm Syntax.Element -> Phantoms.TTerm Syntax.KeyedElement)
keyedElement key element = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.KeyedElement"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "key"),
      Core.fieldTerm = (Phantoms.unTTerm key)},
    Core.Field {
      Core.fieldName = (Core.Name "element"),
      Core.fieldTerm = (Phantoms.unTTerm element)}]})))

keyedElementKey :: (Phantoms.TTerm Syntax.KeyedElement -> Phantoms.TTerm (Maybe Syntax.Key))
keyedElementKey x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.KeyedElement"),
    Core.projectionField = (Core.Name "key")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

keyedElementElement :: (Phantoms.TTerm Syntax.KeyedElement -> Phantoms.TTerm Syntax.Element)
keyedElementElement x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.KeyedElement"),
    Core.projectionField = (Core.Name "element")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

keyedElementWithKey :: (Phantoms.TTerm Syntax.KeyedElement -> Phantoms.TTerm (Maybe Syntax.Key) -> Phantoms.TTerm Syntax.KeyedElement)
keyedElementWithKey original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.KeyedElement"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "key"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "element"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.KeyedElement"),
          Core.projectionField = (Core.Name "element")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

keyedElementWithElement :: (Phantoms.TTerm Syntax.KeyedElement -> Phantoms.TTerm Syntax.Element -> Phantoms.TTerm Syntax.KeyedElement)
keyedElementWithElement original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.KeyedElement"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "key"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.KeyedElement"),
          Core.projectionField = (Core.Name "key")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "element"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

keyField :: (Phantoms.TTerm Syntax.Identifier -> Phantoms.TTerm Syntax.Key)
keyField x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.go.syntax.Key"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "field"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

keyExpression :: (Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.Key)
keyExpression x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.go.syntax.Key"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "expression"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

keyLiteral :: (Phantoms.TTerm Syntax.LiteralValue -> Phantoms.TTerm Syntax.Key)
keyLiteral x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.go.syntax.Key"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "literal"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

elementExpression :: (Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.Element)
elementExpression x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.go.syntax.Element"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "expression"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

elementLiteral :: (Phantoms.TTerm Syntax.LiteralValue -> Phantoms.TTerm Syntax.Element)
elementLiteral x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.go.syntax.Element"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "literal"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

functionLit :: (Phantoms.TTerm Syntax.Signature -> Phantoms.TTerm Syntax.FunctionBody -> Phantoms.TTerm Syntax.FunctionLit)
functionLit signature body = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.FunctionLit"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "signature"),
      Core.fieldTerm = (Phantoms.unTTerm signature)},
    Core.Field {
      Core.fieldName = (Core.Name "body"),
      Core.fieldTerm = (Phantoms.unTTerm body)}]})))

functionLitSignature :: (Phantoms.TTerm Syntax.FunctionLit -> Phantoms.TTerm Syntax.Signature)
functionLitSignature x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.FunctionLit"),
    Core.projectionField = (Core.Name "signature")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

functionLitBody :: (Phantoms.TTerm Syntax.FunctionLit -> Phantoms.TTerm Syntax.FunctionBody)
functionLitBody x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.FunctionLit"),
    Core.projectionField = (Core.Name "body")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

functionLitWithSignature :: (Phantoms.TTerm Syntax.FunctionLit -> Phantoms.TTerm Syntax.Signature -> Phantoms.TTerm Syntax.FunctionLit)
functionLitWithSignature original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.FunctionLit"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "signature"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "body"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.FunctionLit"),
          Core.projectionField = (Core.Name "body")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

functionLitWithBody :: (Phantoms.TTerm Syntax.FunctionLit -> Phantoms.TTerm Syntax.FunctionBody -> Phantoms.TTerm Syntax.FunctionLit)
functionLitWithBody original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.FunctionLit"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "signature"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.FunctionLit"),
          Core.projectionField = (Core.Name "signature")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "body"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

selector :: (Phantoms.TTerm Syntax.Identifier -> Phantoms.TTerm Syntax.Selector)
selector x = (Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
  Core.wrappedTermTypeName = (Core.Name "hydra.ext.go.syntax.Selector"),
  Core.wrappedTermBody = (Phantoms.unTTerm x)})))

unSelector :: (Phantoms.TTerm Syntax.Selector -> Phantoms.TTerm Syntax.Identifier)
unSelector x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.go.syntax.Selector")))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

index :: (Phantoms.TTerm [Syntax.Expression] -> Phantoms.TTerm Syntax.Index)
index x = (Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
  Core.wrappedTermTypeName = (Core.Name "hydra.ext.go.syntax.Index"),
  Core.wrappedTermBody = (Phantoms.unTTerm x)})))

unIndex :: (Phantoms.TTerm Syntax.Index -> Phantoms.TTerm [Syntax.Expression])
unIndex x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.go.syntax.Index")))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

sliceSimple :: (Phantoms.TTerm Syntax.SimpleSlice -> Phantoms.TTerm Syntax.Slice)
sliceSimple x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.go.syntax.Slice"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "simple"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

sliceFull :: (Phantoms.TTerm Syntax.FullSlice -> Phantoms.TTerm Syntax.Slice)
sliceFull x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.go.syntax.Slice"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "full"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

simpleSlice :: (Phantoms.TTerm (Maybe Syntax.Expression) -> Phantoms.TTerm (Maybe Syntax.Expression) -> Phantoms.TTerm Syntax.SimpleSlice)
simpleSlice low high = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.SimpleSlice"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "low"),
      Core.fieldTerm = (Phantoms.unTTerm low)},
    Core.Field {
      Core.fieldName = (Core.Name "high"),
      Core.fieldTerm = (Phantoms.unTTerm high)}]})))

simpleSliceLow :: (Phantoms.TTerm Syntax.SimpleSlice -> Phantoms.TTerm (Maybe Syntax.Expression))
simpleSliceLow x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.SimpleSlice"),
    Core.projectionField = (Core.Name "low")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

simpleSliceHigh :: (Phantoms.TTerm Syntax.SimpleSlice -> Phantoms.TTerm (Maybe Syntax.Expression))
simpleSliceHigh x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.SimpleSlice"),
    Core.projectionField = (Core.Name "high")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

simpleSliceWithLow :: (Phantoms.TTerm Syntax.SimpleSlice -> Phantoms.TTerm (Maybe Syntax.Expression) -> Phantoms.TTerm Syntax.SimpleSlice)
simpleSliceWithLow original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.SimpleSlice"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "low"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "high"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.SimpleSlice"),
          Core.projectionField = (Core.Name "high")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

simpleSliceWithHigh :: (Phantoms.TTerm Syntax.SimpleSlice -> Phantoms.TTerm (Maybe Syntax.Expression) -> Phantoms.TTerm Syntax.SimpleSlice)
simpleSliceWithHigh original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.SimpleSlice"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "low"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.SimpleSlice"),
          Core.projectionField = (Core.Name "low")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "high"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

fullSlice :: (Phantoms.TTerm (Maybe Syntax.Expression) -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.FullSlice)
fullSlice low high max = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.FullSlice"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "low"),
      Core.fieldTerm = (Phantoms.unTTerm low)},
    Core.Field {
      Core.fieldName = (Core.Name "high"),
      Core.fieldTerm = (Phantoms.unTTerm high)},
    Core.Field {
      Core.fieldName = (Core.Name "max"),
      Core.fieldTerm = (Phantoms.unTTerm max)}]})))

fullSliceLow :: (Phantoms.TTerm Syntax.FullSlice -> Phantoms.TTerm (Maybe Syntax.Expression))
fullSliceLow x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.FullSlice"),
    Core.projectionField = (Core.Name "low")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

fullSliceHigh :: (Phantoms.TTerm Syntax.FullSlice -> Phantoms.TTerm Syntax.Expression)
fullSliceHigh x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.FullSlice"),
    Core.projectionField = (Core.Name "high")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

fullSliceMax :: (Phantoms.TTerm Syntax.FullSlice -> Phantoms.TTerm Syntax.Expression)
fullSliceMax x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.FullSlice"),
    Core.projectionField = (Core.Name "max")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

fullSliceWithLow :: (Phantoms.TTerm Syntax.FullSlice -> Phantoms.TTerm (Maybe Syntax.Expression) -> Phantoms.TTerm Syntax.FullSlice)
fullSliceWithLow original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.FullSlice"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "low"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "high"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.FullSlice"),
          Core.projectionField = (Core.Name "high")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "max"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.FullSlice"),
          Core.projectionField = (Core.Name "max")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

fullSliceWithHigh :: (Phantoms.TTerm Syntax.FullSlice -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.FullSlice)
fullSliceWithHigh original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.FullSlice"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "low"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.FullSlice"),
          Core.projectionField = (Core.Name "low")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "high"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "max"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.FullSlice"),
          Core.projectionField = (Core.Name "max")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

fullSliceWithMax :: (Phantoms.TTerm Syntax.FullSlice -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.FullSlice)
fullSliceWithMax original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.FullSlice"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "low"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.FullSlice"),
          Core.projectionField = (Core.Name "low")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "high"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.FullSlice"),
          Core.projectionField = (Core.Name "high")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "max"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

typeAssertion :: (Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.TypeAssertion)
typeAssertion x = (Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
  Core.wrappedTermTypeName = (Core.Name "hydra.ext.go.syntax.TypeAssertion"),
  Core.wrappedTermBody = (Phantoms.unTTerm x)})))

unTypeAssertion :: (Phantoms.TTerm Syntax.TypeAssertion -> Phantoms.TTerm Syntax.Type)
unTypeAssertion x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.go.syntax.TypeAssertion")))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

arguments :: (Phantoms.TTerm (Maybe Syntax.Type) -> Phantoms.TTerm [Syntax.Expression] -> Phantoms.TTerm Bool -> Phantoms.TTerm Syntax.Arguments)
arguments typeArg expressions ellipsis = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.Arguments"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "typeArg"),
      Core.fieldTerm = (Phantoms.unTTerm typeArg)},
    Core.Field {
      Core.fieldName = (Core.Name "expressions"),
      Core.fieldTerm = (Phantoms.unTTerm expressions)},
    Core.Field {
      Core.fieldName = (Core.Name "ellipsis"),
      Core.fieldTerm = (Phantoms.unTTerm ellipsis)}]})))

argumentsTypeArg :: (Phantoms.TTerm Syntax.Arguments -> Phantoms.TTerm (Maybe Syntax.Type))
argumentsTypeArg x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.Arguments"),
    Core.projectionField = (Core.Name "typeArg")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

argumentsExpressions :: (Phantoms.TTerm Syntax.Arguments -> Phantoms.TTerm [Syntax.Expression])
argumentsExpressions x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.Arguments"),
    Core.projectionField = (Core.Name "expressions")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

argumentsEllipsis :: (Phantoms.TTerm Syntax.Arguments -> Phantoms.TTerm Bool)
argumentsEllipsis x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.Arguments"),
    Core.projectionField = (Core.Name "ellipsis")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

argumentsWithTypeArg :: (Phantoms.TTerm Syntax.Arguments -> Phantoms.TTerm (Maybe Syntax.Type) -> Phantoms.TTerm Syntax.Arguments)
argumentsWithTypeArg original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.Arguments"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "typeArg"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "expressions"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.Arguments"),
          Core.projectionField = (Core.Name "expressions")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "ellipsis"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.Arguments"),
          Core.projectionField = (Core.Name "ellipsis")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

argumentsWithExpressions :: (Phantoms.TTerm Syntax.Arguments -> Phantoms.TTerm [Syntax.Expression] -> Phantoms.TTerm Syntax.Arguments)
argumentsWithExpressions original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.Arguments"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "typeArg"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.Arguments"),
          Core.projectionField = (Core.Name "typeArg")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "expressions"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "ellipsis"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.Arguments"),
          Core.projectionField = (Core.Name "ellipsis")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

argumentsWithEllipsis :: (Phantoms.TTerm Syntax.Arguments -> Phantoms.TTerm Bool -> Phantoms.TTerm Syntax.Arguments)
argumentsWithEllipsis original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.Arguments"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "typeArg"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.Arguments"),
          Core.projectionField = (Core.Name "typeArg")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "expressions"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.Arguments"),
          Core.projectionField = (Core.Name "expressions")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "ellipsis"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

methodExpr :: (Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.Identifier -> Phantoms.TTerm Syntax.MethodExpr)
methodExpr receiver method = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.MethodExpr"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "receiver"),
      Core.fieldTerm = (Phantoms.unTTerm receiver)},
    Core.Field {
      Core.fieldName = (Core.Name "method"),
      Core.fieldTerm = (Phantoms.unTTerm method)}]})))

methodExprReceiver :: (Phantoms.TTerm Syntax.MethodExpr -> Phantoms.TTerm Syntax.Type)
methodExprReceiver x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.MethodExpr"),
    Core.projectionField = (Core.Name "receiver")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

methodExprMethod :: (Phantoms.TTerm Syntax.MethodExpr -> Phantoms.TTerm Syntax.Identifier)
methodExprMethod x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.MethodExpr"),
    Core.projectionField = (Core.Name "method")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

methodExprWithReceiver :: (Phantoms.TTerm Syntax.MethodExpr -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.MethodExpr)
methodExprWithReceiver original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.MethodExpr"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "receiver"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "method"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.MethodExpr"),
          Core.projectionField = (Core.Name "method")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

methodExprWithMethod :: (Phantoms.TTerm Syntax.MethodExpr -> Phantoms.TTerm Syntax.Identifier -> Phantoms.TTerm Syntax.MethodExpr)
methodExprWithMethod original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.MethodExpr"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "receiver"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.MethodExpr"),
          Core.projectionField = (Core.Name "receiver")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "method"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

conversion :: (Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.Conversion)
conversion type_ expression = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.Conversion"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "type"),
      Core.fieldTerm = (Phantoms.unTTerm type_)},
    Core.Field {
      Core.fieldName = (Core.Name "expression"),
      Core.fieldTerm = (Phantoms.unTTerm expression)}]})))

conversionType :: (Phantoms.TTerm Syntax.Conversion -> Phantoms.TTerm Syntax.Type)
conversionType x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.Conversion"),
    Core.projectionField = (Core.Name "type")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

conversionExpression :: (Phantoms.TTerm Syntax.Conversion -> Phantoms.TTerm Syntax.Expression)
conversionExpression x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.Conversion"),
    Core.projectionField = (Core.Name "expression")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

conversionWithType :: (Phantoms.TTerm Syntax.Conversion -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.Conversion)
conversionWithType original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.Conversion"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "type"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "expression"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.Conversion"),
          Core.projectionField = (Core.Name "expression")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

conversionWithExpression :: (Phantoms.TTerm Syntax.Conversion -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.Conversion)
conversionWithExpression original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.Conversion"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "type"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.Conversion"),
          Core.projectionField = (Core.Name "type")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "expression"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

statementDeclaration :: (Phantoms.TTerm Syntax.Declaration -> Phantoms.TTerm Syntax.Statement)
statementDeclaration x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.go.syntax.Statement"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "declaration"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

statementLabeled :: (Phantoms.TTerm Syntax.LabeledStmt -> Phantoms.TTerm Syntax.Statement)
statementLabeled x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.go.syntax.Statement"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "labeled"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

statementSimple :: (Phantoms.TTerm Syntax.SimpleStmt -> Phantoms.TTerm Syntax.Statement)
statementSimple x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.go.syntax.Statement"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "simple"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

statementGo :: (Phantoms.TTerm Syntax.GoStmt -> Phantoms.TTerm Syntax.Statement)
statementGo x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.go.syntax.Statement"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "go"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

statementReturn :: (Phantoms.TTerm Syntax.ReturnStmt -> Phantoms.TTerm Syntax.Statement)
statementReturn x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.go.syntax.Statement"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "return"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

statementBreak :: (Phantoms.TTerm Syntax.BreakStmt -> Phantoms.TTerm Syntax.Statement)
statementBreak x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.go.syntax.Statement"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "break"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

statementContinue :: (Phantoms.TTerm Syntax.ContinueStmt -> Phantoms.TTerm Syntax.Statement)
statementContinue x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.go.syntax.Statement"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "continue"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

statementGoto :: (Phantoms.TTerm Syntax.GotoStmt -> Phantoms.TTerm Syntax.Statement)
statementGoto x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.go.syntax.Statement"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "goto"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

statementFallthrough :: (Phantoms.TTerm Syntax.FallthroughStmt -> Phantoms.TTerm Syntax.Statement)
statementFallthrough x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.go.syntax.Statement"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "fallthrough"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

statementBlock :: (Phantoms.TTerm Syntax.Block -> Phantoms.TTerm Syntax.Statement)
statementBlock x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.go.syntax.Statement"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "block"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

statementIf :: (Phantoms.TTerm Syntax.IfStmt -> Phantoms.TTerm Syntax.Statement)
statementIf x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.go.syntax.Statement"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "if"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

statementSwitch :: (Phantoms.TTerm Syntax.SwitchStmt -> Phantoms.TTerm Syntax.Statement)
statementSwitch x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.go.syntax.Statement"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "switch"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

statementSelect :: (Phantoms.TTerm Syntax.SelectStmt -> Phantoms.TTerm Syntax.Statement)
statementSelect x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.go.syntax.Statement"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "select"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

statementFor :: (Phantoms.TTerm Syntax.ForStmt -> Phantoms.TTerm Syntax.Statement)
statementFor x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.go.syntax.Statement"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "for"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

statementDefer :: (Phantoms.TTerm Syntax.DeferStmt -> Phantoms.TTerm Syntax.Statement)
statementDefer x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.go.syntax.Statement"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "defer"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

simpleStmtEmpty :: (Phantoms.TTerm Syntax.EmptyStmt -> Phantoms.TTerm Syntax.SimpleStmt)
simpleStmtEmpty x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.go.syntax.SimpleStmt"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "empty"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

simpleStmtExpression :: (Phantoms.TTerm Syntax.ExpressionStmt -> Phantoms.TTerm Syntax.SimpleStmt)
simpleStmtExpression x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.go.syntax.SimpleStmt"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "expression"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

simpleStmtSend :: (Phantoms.TTerm Syntax.SendStmt -> Phantoms.TTerm Syntax.SimpleStmt)
simpleStmtSend x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.go.syntax.SimpleStmt"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "send"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

simpleStmtIncDec :: (Phantoms.TTerm Syntax.IncDecStmt -> Phantoms.TTerm Syntax.SimpleStmt)
simpleStmtIncDec x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.go.syntax.SimpleStmt"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "incDec"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

simpleStmtAssignment :: (Phantoms.TTerm Syntax.Assignment -> Phantoms.TTerm Syntax.SimpleStmt)
simpleStmtAssignment x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.go.syntax.SimpleStmt"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "assignment"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

simpleStmtShortVarDecl :: (Phantoms.TTerm Syntax.ShortVarDecl -> Phantoms.TTerm Syntax.SimpleStmt)
simpleStmtShortVarDecl x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.go.syntax.SimpleStmt"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "shortVarDecl"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

emptyStmt :: (Phantoms.TTerm () -> Phantoms.TTerm Syntax.EmptyStmt)
emptyStmt x = (Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
  Core.wrappedTermTypeName = (Core.Name "hydra.ext.go.syntax.EmptyStmt"),
  Core.wrappedTermBody = (Phantoms.unTTerm x)})))

unEmptyStmt :: (Phantoms.TTerm Syntax.EmptyStmt -> Phantoms.TTerm ())
unEmptyStmt x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.go.syntax.EmptyStmt")))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

labeledStmt :: (Phantoms.TTerm Syntax.Identifier -> Phantoms.TTerm Syntax.Statement -> Phantoms.TTerm Syntax.LabeledStmt)
labeledStmt label statement = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.LabeledStmt"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "label"),
      Core.fieldTerm = (Phantoms.unTTerm label)},
    Core.Field {
      Core.fieldName = (Core.Name "statement"),
      Core.fieldTerm = (Phantoms.unTTerm statement)}]})))

labeledStmtLabel :: (Phantoms.TTerm Syntax.LabeledStmt -> Phantoms.TTerm Syntax.Identifier)
labeledStmtLabel x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.LabeledStmt"),
    Core.projectionField = (Core.Name "label")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

labeledStmtStatement :: (Phantoms.TTerm Syntax.LabeledStmt -> Phantoms.TTerm Syntax.Statement)
labeledStmtStatement x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.LabeledStmt"),
    Core.projectionField = (Core.Name "statement")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

labeledStmtWithLabel :: (Phantoms.TTerm Syntax.LabeledStmt -> Phantoms.TTerm Syntax.Identifier -> Phantoms.TTerm Syntax.LabeledStmt)
labeledStmtWithLabel original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.LabeledStmt"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "label"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "statement"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.LabeledStmt"),
          Core.projectionField = (Core.Name "statement")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

labeledStmtWithStatement :: (Phantoms.TTerm Syntax.LabeledStmt -> Phantoms.TTerm Syntax.Statement -> Phantoms.TTerm Syntax.LabeledStmt)
labeledStmtWithStatement original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.LabeledStmt"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "label"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.LabeledStmt"),
          Core.projectionField = (Core.Name "label")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "statement"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

expressionStmt :: (Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.ExpressionStmt)
expressionStmt x = (Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
  Core.wrappedTermTypeName = (Core.Name "hydra.ext.go.syntax.ExpressionStmt"),
  Core.wrappedTermBody = (Phantoms.unTTerm x)})))

unExpressionStmt :: (Phantoms.TTerm Syntax.ExpressionStmt -> Phantoms.TTerm Syntax.Expression)
unExpressionStmt x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.go.syntax.ExpressionStmt")))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

sendStmt :: (Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.SendStmt)
sendStmt channel value = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.SendStmt"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "channel"),
      Core.fieldTerm = (Phantoms.unTTerm channel)},
    Core.Field {
      Core.fieldName = (Core.Name "value"),
      Core.fieldTerm = (Phantoms.unTTerm value)}]})))

sendStmtChannel :: (Phantoms.TTerm Syntax.SendStmt -> Phantoms.TTerm Syntax.Expression)
sendStmtChannel x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.SendStmt"),
    Core.projectionField = (Core.Name "channel")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

sendStmtValue :: (Phantoms.TTerm Syntax.SendStmt -> Phantoms.TTerm Syntax.Expression)
sendStmtValue x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.SendStmt"),
    Core.projectionField = (Core.Name "value")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

sendStmtWithChannel :: (Phantoms.TTerm Syntax.SendStmt -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.SendStmt)
sendStmtWithChannel original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.SendStmt"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "channel"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "value"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.SendStmt"),
          Core.projectionField = (Core.Name "value")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

sendStmtWithValue :: (Phantoms.TTerm Syntax.SendStmt -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.SendStmt)
sendStmtWithValue original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.SendStmt"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "channel"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.SendStmt"),
          Core.projectionField = (Core.Name "channel")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "value"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

incDecStmt :: (Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Bool -> Phantoms.TTerm Syntax.IncDecStmt)
incDecStmt expression increment = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.IncDecStmt"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "expression"),
      Core.fieldTerm = (Phantoms.unTTerm expression)},
    Core.Field {
      Core.fieldName = (Core.Name "increment"),
      Core.fieldTerm = (Phantoms.unTTerm increment)}]})))

incDecStmtExpression :: (Phantoms.TTerm Syntax.IncDecStmt -> Phantoms.TTerm Syntax.Expression)
incDecStmtExpression x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.IncDecStmt"),
    Core.projectionField = (Core.Name "expression")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

incDecStmtIncrement :: (Phantoms.TTerm Syntax.IncDecStmt -> Phantoms.TTerm Bool)
incDecStmtIncrement x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.IncDecStmt"),
    Core.projectionField = (Core.Name "increment")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

incDecStmtWithExpression :: (Phantoms.TTerm Syntax.IncDecStmt -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.IncDecStmt)
incDecStmtWithExpression original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.IncDecStmt"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "expression"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "increment"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.IncDecStmt"),
          Core.projectionField = (Core.Name "increment")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

incDecStmtWithIncrement :: (Phantoms.TTerm Syntax.IncDecStmt -> Phantoms.TTerm Bool -> Phantoms.TTerm Syntax.IncDecStmt)
incDecStmtWithIncrement original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.IncDecStmt"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "expression"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.IncDecStmt"),
          Core.projectionField = (Core.Name "expression")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "increment"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

assignment :: (Phantoms.TTerm [Syntax.Expression] -> Phantoms.TTerm Syntax.AssignOp -> Phantoms.TTerm [Syntax.Expression] -> Phantoms.TTerm Syntax.Assignment)
assignment lhs op rhs = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.Assignment"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "lhs"),
      Core.fieldTerm = (Phantoms.unTTerm lhs)},
    Core.Field {
      Core.fieldName = (Core.Name "op"),
      Core.fieldTerm = (Phantoms.unTTerm op)},
    Core.Field {
      Core.fieldName = (Core.Name "rhs"),
      Core.fieldTerm = (Phantoms.unTTerm rhs)}]})))

assignmentLhs :: (Phantoms.TTerm Syntax.Assignment -> Phantoms.TTerm [Syntax.Expression])
assignmentLhs x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.Assignment"),
    Core.projectionField = (Core.Name "lhs")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

assignmentOp :: (Phantoms.TTerm Syntax.Assignment -> Phantoms.TTerm Syntax.AssignOp)
assignmentOp x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.Assignment"),
    Core.projectionField = (Core.Name "op")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

assignmentRhs :: (Phantoms.TTerm Syntax.Assignment -> Phantoms.TTerm [Syntax.Expression])
assignmentRhs x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.Assignment"),
    Core.projectionField = (Core.Name "rhs")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

assignmentWithLhs :: (Phantoms.TTerm Syntax.Assignment -> Phantoms.TTerm [Syntax.Expression] -> Phantoms.TTerm Syntax.Assignment)
assignmentWithLhs original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.Assignment"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "lhs"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "op"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.Assignment"),
          Core.projectionField = (Core.Name "op")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "rhs"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.Assignment"),
          Core.projectionField = (Core.Name "rhs")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

assignmentWithOp :: (Phantoms.TTerm Syntax.Assignment -> Phantoms.TTerm Syntax.AssignOp -> Phantoms.TTerm Syntax.Assignment)
assignmentWithOp original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.Assignment"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "lhs"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.Assignment"),
          Core.projectionField = (Core.Name "lhs")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "op"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "rhs"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.Assignment"),
          Core.projectionField = (Core.Name "rhs")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

assignmentWithRhs :: (Phantoms.TTerm Syntax.Assignment -> Phantoms.TTerm [Syntax.Expression] -> Phantoms.TTerm Syntax.Assignment)
assignmentWithRhs original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.Assignment"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "lhs"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.Assignment"),
          Core.projectionField = (Core.Name "lhs")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "op"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.Assignment"),
          Core.projectionField = (Core.Name "op")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "rhs"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

assignOpSimple :: (Phantoms.TTerm Syntax.AssignOp)
assignOpSimple = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.go.syntax.AssignOp"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "simple"),
    Core.fieldTerm = Core.TermUnit}})))

assignOpAdd :: (Phantoms.TTerm Syntax.AddOp -> Phantoms.TTerm Syntax.AssignOp)
assignOpAdd x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.go.syntax.AssignOp"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "add"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

assignOpMul :: (Phantoms.TTerm Syntax.MulOp -> Phantoms.TTerm Syntax.AssignOp)
assignOpMul x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.go.syntax.AssignOp"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "mul"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

ifStmt :: (Phantoms.TTerm (Maybe Syntax.SimpleStmt) -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.Block -> Phantoms.TTerm (Maybe Syntax.ElseClause) -> Phantoms.TTerm Syntax.IfStmt)
ifStmt init condition then_ else_ = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.IfStmt"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "init"),
      Core.fieldTerm = (Phantoms.unTTerm init)},
    Core.Field {
      Core.fieldName = (Core.Name "condition"),
      Core.fieldTerm = (Phantoms.unTTerm condition)},
    Core.Field {
      Core.fieldName = (Core.Name "then"),
      Core.fieldTerm = (Phantoms.unTTerm then_)},
    Core.Field {
      Core.fieldName = (Core.Name "else"),
      Core.fieldTerm = (Phantoms.unTTerm else_)}]})))

ifStmtInit :: (Phantoms.TTerm Syntax.IfStmt -> Phantoms.TTerm (Maybe Syntax.SimpleStmt))
ifStmtInit x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.IfStmt"),
    Core.projectionField = (Core.Name "init")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

ifStmtCondition :: (Phantoms.TTerm Syntax.IfStmt -> Phantoms.TTerm Syntax.Expression)
ifStmtCondition x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.IfStmt"),
    Core.projectionField = (Core.Name "condition")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

ifStmtThen :: (Phantoms.TTerm Syntax.IfStmt -> Phantoms.TTerm Syntax.Block)
ifStmtThen x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.IfStmt"),
    Core.projectionField = (Core.Name "then")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

ifStmtElse :: (Phantoms.TTerm Syntax.IfStmt -> Phantoms.TTerm (Maybe Syntax.ElseClause))
ifStmtElse x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.IfStmt"),
    Core.projectionField = (Core.Name "else")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

ifStmtWithInit :: (Phantoms.TTerm Syntax.IfStmt -> Phantoms.TTerm (Maybe Syntax.SimpleStmt) -> Phantoms.TTerm Syntax.IfStmt)
ifStmtWithInit original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.IfStmt"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "init"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "condition"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.IfStmt"),
          Core.projectionField = (Core.Name "condition")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "then"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.IfStmt"),
          Core.projectionField = (Core.Name "then")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "else"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.IfStmt"),
          Core.projectionField = (Core.Name "else")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

ifStmtWithCondition :: (Phantoms.TTerm Syntax.IfStmt -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.IfStmt)
ifStmtWithCondition original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.IfStmt"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "init"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.IfStmt"),
          Core.projectionField = (Core.Name "init")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "condition"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "then"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.IfStmt"),
          Core.projectionField = (Core.Name "then")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "else"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.IfStmt"),
          Core.projectionField = (Core.Name "else")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

ifStmtWithThen :: (Phantoms.TTerm Syntax.IfStmt -> Phantoms.TTerm Syntax.Block -> Phantoms.TTerm Syntax.IfStmt)
ifStmtWithThen original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.IfStmt"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "init"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.IfStmt"),
          Core.projectionField = (Core.Name "init")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "condition"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.IfStmt"),
          Core.projectionField = (Core.Name "condition")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "then"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "else"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.IfStmt"),
          Core.projectionField = (Core.Name "else")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

ifStmtWithElse :: (Phantoms.TTerm Syntax.IfStmt -> Phantoms.TTerm (Maybe Syntax.ElseClause) -> Phantoms.TTerm Syntax.IfStmt)
ifStmtWithElse original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.IfStmt"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "init"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.IfStmt"),
          Core.projectionField = (Core.Name "init")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "condition"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.IfStmt"),
          Core.projectionField = (Core.Name "condition")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "then"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.IfStmt"),
          Core.projectionField = (Core.Name "then")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "else"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

elseClauseIf :: (Phantoms.TTerm Syntax.IfStmt -> Phantoms.TTerm Syntax.ElseClause)
elseClauseIf x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.go.syntax.ElseClause"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "if"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

elseClauseBlock :: (Phantoms.TTerm Syntax.Block -> Phantoms.TTerm Syntax.ElseClause)
elseClauseBlock x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.go.syntax.ElseClause"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "block"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

switchStmtExpression :: (Phantoms.TTerm Syntax.ExprSwitchStmt -> Phantoms.TTerm Syntax.SwitchStmt)
switchStmtExpression x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.go.syntax.SwitchStmt"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "expression"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

switchStmtType :: (Phantoms.TTerm Syntax.TypeSwitchStmt -> Phantoms.TTerm Syntax.SwitchStmt)
switchStmtType x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.go.syntax.SwitchStmt"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "type"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

exprSwitchStmt :: (Phantoms.TTerm (Maybe Syntax.SimpleStmt) -> Phantoms.TTerm (Maybe Syntax.Expression) -> Phantoms.TTerm [Syntax.ExprCaseClause] -> Phantoms.TTerm Syntax.ExprSwitchStmt)
exprSwitchStmt init expression cases = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.ExprSwitchStmt"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "init"),
      Core.fieldTerm = (Phantoms.unTTerm init)},
    Core.Field {
      Core.fieldName = (Core.Name "expression"),
      Core.fieldTerm = (Phantoms.unTTerm expression)},
    Core.Field {
      Core.fieldName = (Core.Name "cases"),
      Core.fieldTerm = (Phantoms.unTTerm cases)}]})))

exprSwitchStmtInit :: (Phantoms.TTerm Syntax.ExprSwitchStmt -> Phantoms.TTerm (Maybe Syntax.SimpleStmt))
exprSwitchStmtInit x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.ExprSwitchStmt"),
    Core.projectionField = (Core.Name "init")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

exprSwitchStmtExpression :: (Phantoms.TTerm Syntax.ExprSwitchStmt -> Phantoms.TTerm (Maybe Syntax.Expression))
exprSwitchStmtExpression x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.ExprSwitchStmt"),
    Core.projectionField = (Core.Name "expression")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

exprSwitchStmtCases :: (Phantoms.TTerm Syntax.ExprSwitchStmt -> Phantoms.TTerm [Syntax.ExprCaseClause])
exprSwitchStmtCases x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.ExprSwitchStmt"),
    Core.projectionField = (Core.Name "cases")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

exprSwitchStmtWithInit :: (Phantoms.TTerm Syntax.ExprSwitchStmt -> Phantoms.TTerm (Maybe Syntax.SimpleStmt) -> Phantoms.TTerm Syntax.ExprSwitchStmt)
exprSwitchStmtWithInit original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.ExprSwitchStmt"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "init"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "expression"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.ExprSwitchStmt"),
          Core.projectionField = (Core.Name "expression")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "cases"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.ExprSwitchStmt"),
          Core.projectionField = (Core.Name "cases")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

exprSwitchStmtWithExpression :: (Phantoms.TTerm Syntax.ExprSwitchStmt -> Phantoms.TTerm (Maybe Syntax.Expression) -> Phantoms.TTerm Syntax.ExprSwitchStmt)
exprSwitchStmtWithExpression original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.ExprSwitchStmt"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "init"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.ExprSwitchStmt"),
          Core.projectionField = (Core.Name "init")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "expression"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "cases"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.ExprSwitchStmt"),
          Core.projectionField = (Core.Name "cases")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

exprSwitchStmtWithCases :: (Phantoms.TTerm Syntax.ExprSwitchStmt -> Phantoms.TTerm [Syntax.ExprCaseClause] -> Phantoms.TTerm Syntax.ExprSwitchStmt)
exprSwitchStmtWithCases original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.ExprSwitchStmt"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "init"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.ExprSwitchStmt"),
          Core.projectionField = (Core.Name "init")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "expression"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.ExprSwitchStmt"),
          Core.projectionField = (Core.Name "expression")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "cases"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

exprCaseClause :: (Phantoms.TTerm (Maybe [Syntax.Expression]) -> Phantoms.TTerm [Syntax.Statement] -> Phantoms.TTerm Syntax.ExprCaseClause)
exprCaseClause case_ statements = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.ExprCaseClause"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "case"),
      Core.fieldTerm = (Phantoms.unTTerm case_)},
    Core.Field {
      Core.fieldName = (Core.Name "statements"),
      Core.fieldTerm = (Phantoms.unTTerm statements)}]})))

exprCaseClauseCase :: (Phantoms.TTerm Syntax.ExprCaseClause -> Phantoms.TTerm (Maybe [Syntax.Expression]))
exprCaseClauseCase x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.ExprCaseClause"),
    Core.projectionField = (Core.Name "case")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

exprCaseClauseStatements :: (Phantoms.TTerm Syntax.ExprCaseClause -> Phantoms.TTerm [Syntax.Statement])
exprCaseClauseStatements x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.ExprCaseClause"),
    Core.projectionField = (Core.Name "statements")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

exprCaseClauseWithCase :: (Phantoms.TTerm Syntax.ExprCaseClause -> Phantoms.TTerm (Maybe [Syntax.Expression]) -> Phantoms.TTerm Syntax.ExprCaseClause)
exprCaseClauseWithCase original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.ExprCaseClause"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "case"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "statements"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.ExprCaseClause"),
          Core.projectionField = (Core.Name "statements")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

exprCaseClauseWithStatements :: (Phantoms.TTerm Syntax.ExprCaseClause -> Phantoms.TTerm [Syntax.Statement] -> Phantoms.TTerm Syntax.ExprCaseClause)
exprCaseClauseWithStatements original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.ExprCaseClause"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "case"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.ExprCaseClause"),
          Core.projectionField = (Core.Name "case")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "statements"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

typeSwitchStmt :: (Phantoms.TTerm (Maybe Syntax.SimpleStmt) -> Phantoms.TTerm Syntax.TypeSwitchGuard -> Phantoms.TTerm [Syntax.TypeCaseClause] -> Phantoms.TTerm Syntax.TypeSwitchStmt)
typeSwitchStmt init guard cases = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.TypeSwitchStmt"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "init"),
      Core.fieldTerm = (Phantoms.unTTerm init)},
    Core.Field {
      Core.fieldName = (Core.Name "guard"),
      Core.fieldTerm = (Phantoms.unTTerm guard)},
    Core.Field {
      Core.fieldName = (Core.Name "cases"),
      Core.fieldTerm = (Phantoms.unTTerm cases)}]})))

typeSwitchStmtInit :: (Phantoms.TTerm Syntax.TypeSwitchStmt -> Phantoms.TTerm (Maybe Syntax.SimpleStmt))
typeSwitchStmtInit x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.TypeSwitchStmt"),
    Core.projectionField = (Core.Name "init")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

typeSwitchStmtGuard :: (Phantoms.TTerm Syntax.TypeSwitchStmt -> Phantoms.TTerm Syntax.TypeSwitchGuard)
typeSwitchStmtGuard x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.TypeSwitchStmt"),
    Core.projectionField = (Core.Name "guard")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

typeSwitchStmtCases :: (Phantoms.TTerm Syntax.TypeSwitchStmt -> Phantoms.TTerm [Syntax.TypeCaseClause])
typeSwitchStmtCases x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.TypeSwitchStmt"),
    Core.projectionField = (Core.Name "cases")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

typeSwitchStmtWithInit :: (Phantoms.TTerm Syntax.TypeSwitchStmt -> Phantoms.TTerm (Maybe Syntax.SimpleStmt) -> Phantoms.TTerm Syntax.TypeSwitchStmt)
typeSwitchStmtWithInit original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.TypeSwitchStmt"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "init"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "guard"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.TypeSwitchStmt"),
          Core.projectionField = (Core.Name "guard")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "cases"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.TypeSwitchStmt"),
          Core.projectionField = (Core.Name "cases")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

typeSwitchStmtWithGuard :: (Phantoms.TTerm Syntax.TypeSwitchStmt -> Phantoms.TTerm Syntax.TypeSwitchGuard -> Phantoms.TTerm Syntax.TypeSwitchStmt)
typeSwitchStmtWithGuard original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.TypeSwitchStmt"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "init"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.TypeSwitchStmt"),
          Core.projectionField = (Core.Name "init")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "guard"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "cases"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.TypeSwitchStmt"),
          Core.projectionField = (Core.Name "cases")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

typeSwitchStmtWithCases :: (Phantoms.TTerm Syntax.TypeSwitchStmt -> Phantoms.TTerm [Syntax.TypeCaseClause] -> Phantoms.TTerm Syntax.TypeSwitchStmt)
typeSwitchStmtWithCases original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.TypeSwitchStmt"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "init"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.TypeSwitchStmt"),
          Core.projectionField = (Core.Name "init")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "guard"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.TypeSwitchStmt"),
          Core.projectionField = (Core.Name "guard")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "cases"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

typeSwitchGuard :: (Phantoms.TTerm (Maybe Syntax.Identifier) -> Phantoms.TTerm Syntax.PrimaryExpr -> Phantoms.TTerm Syntax.TypeSwitchGuard)
typeSwitchGuard name expression = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.TypeSwitchGuard"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Phantoms.unTTerm name)},
    Core.Field {
      Core.fieldName = (Core.Name "expression"),
      Core.fieldTerm = (Phantoms.unTTerm expression)}]})))

typeSwitchGuardName :: (Phantoms.TTerm Syntax.TypeSwitchGuard -> Phantoms.TTerm (Maybe Syntax.Identifier))
typeSwitchGuardName x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.TypeSwitchGuard"),
    Core.projectionField = (Core.Name "name")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

typeSwitchGuardExpression :: (Phantoms.TTerm Syntax.TypeSwitchGuard -> Phantoms.TTerm Syntax.PrimaryExpr)
typeSwitchGuardExpression x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.TypeSwitchGuard"),
    Core.projectionField = (Core.Name "expression")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

typeSwitchGuardWithName :: (Phantoms.TTerm Syntax.TypeSwitchGuard -> Phantoms.TTerm (Maybe Syntax.Identifier) -> Phantoms.TTerm Syntax.TypeSwitchGuard)
typeSwitchGuardWithName original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.TypeSwitchGuard"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "expression"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.TypeSwitchGuard"),
          Core.projectionField = (Core.Name "expression")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

typeSwitchGuardWithExpression :: (Phantoms.TTerm Syntax.TypeSwitchGuard -> Phantoms.TTerm Syntax.PrimaryExpr -> Phantoms.TTerm Syntax.TypeSwitchGuard)
typeSwitchGuardWithExpression original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.TypeSwitchGuard"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.TypeSwitchGuard"),
          Core.projectionField = (Core.Name "name")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "expression"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

typeCaseClause :: (Phantoms.TTerm (Maybe [Syntax.Type]) -> Phantoms.TTerm [Syntax.Statement] -> Phantoms.TTerm Syntax.TypeCaseClause)
typeCaseClause case_ statements = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.TypeCaseClause"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "case"),
      Core.fieldTerm = (Phantoms.unTTerm case_)},
    Core.Field {
      Core.fieldName = (Core.Name "statements"),
      Core.fieldTerm = (Phantoms.unTTerm statements)}]})))

typeCaseClauseCase :: (Phantoms.TTerm Syntax.TypeCaseClause -> Phantoms.TTerm (Maybe [Syntax.Type]))
typeCaseClauseCase x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.TypeCaseClause"),
    Core.projectionField = (Core.Name "case")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

typeCaseClauseStatements :: (Phantoms.TTerm Syntax.TypeCaseClause -> Phantoms.TTerm [Syntax.Statement])
typeCaseClauseStatements x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.TypeCaseClause"),
    Core.projectionField = (Core.Name "statements")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

typeCaseClauseWithCase :: (Phantoms.TTerm Syntax.TypeCaseClause -> Phantoms.TTerm (Maybe [Syntax.Type]) -> Phantoms.TTerm Syntax.TypeCaseClause)
typeCaseClauseWithCase original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.TypeCaseClause"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "case"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "statements"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.TypeCaseClause"),
          Core.projectionField = (Core.Name "statements")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

typeCaseClauseWithStatements :: (Phantoms.TTerm Syntax.TypeCaseClause -> Phantoms.TTerm [Syntax.Statement] -> Phantoms.TTerm Syntax.TypeCaseClause)
typeCaseClauseWithStatements original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.TypeCaseClause"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "case"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.TypeCaseClause"),
          Core.projectionField = (Core.Name "case")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "statements"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

forStmt :: (Phantoms.TTerm (Maybe Syntax.ForClauseOrRange) -> Phantoms.TTerm Syntax.Block -> Phantoms.TTerm Syntax.ForStmt)
forStmt clause body = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.ForStmt"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "clause"),
      Core.fieldTerm = (Phantoms.unTTerm clause)},
    Core.Field {
      Core.fieldName = (Core.Name "body"),
      Core.fieldTerm = (Phantoms.unTTerm body)}]})))

forStmtClause :: (Phantoms.TTerm Syntax.ForStmt -> Phantoms.TTerm (Maybe Syntax.ForClauseOrRange))
forStmtClause x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.ForStmt"),
    Core.projectionField = (Core.Name "clause")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

forStmtBody :: (Phantoms.TTerm Syntax.ForStmt -> Phantoms.TTerm Syntax.Block)
forStmtBody x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.ForStmt"),
    Core.projectionField = (Core.Name "body")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

forStmtWithClause :: (Phantoms.TTerm Syntax.ForStmt -> Phantoms.TTerm (Maybe Syntax.ForClauseOrRange) -> Phantoms.TTerm Syntax.ForStmt)
forStmtWithClause original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.ForStmt"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "clause"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "body"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.ForStmt"),
          Core.projectionField = (Core.Name "body")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

forStmtWithBody :: (Phantoms.TTerm Syntax.ForStmt -> Phantoms.TTerm Syntax.Block -> Phantoms.TTerm Syntax.ForStmt)
forStmtWithBody original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.ForStmt"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "clause"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.ForStmt"),
          Core.projectionField = (Core.Name "clause")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "body"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

forClauseOrRangeCondition :: (Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.ForClauseOrRange)
forClauseOrRangeCondition x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.go.syntax.ForClauseOrRange"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "condition"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

forClauseOrRangeClause :: (Phantoms.TTerm Syntax.ForClause -> Phantoms.TTerm Syntax.ForClauseOrRange)
forClauseOrRangeClause x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.go.syntax.ForClauseOrRange"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "clause"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

forClauseOrRangeRange :: (Phantoms.TTerm Syntax.RangeClause -> Phantoms.TTerm Syntax.ForClauseOrRange)
forClauseOrRangeRange x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.go.syntax.ForClauseOrRange"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "range"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

forClause :: (Phantoms.TTerm (Maybe Syntax.SimpleStmt) -> Phantoms.TTerm (Maybe Syntax.Expression) -> Phantoms.TTerm (Maybe Syntax.SimpleStmt) -> Phantoms.TTerm Syntax.ForClause)
forClause init condition post = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.ForClause"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "init"),
      Core.fieldTerm = (Phantoms.unTTerm init)},
    Core.Field {
      Core.fieldName = (Core.Name "condition"),
      Core.fieldTerm = (Phantoms.unTTerm condition)},
    Core.Field {
      Core.fieldName = (Core.Name "post"),
      Core.fieldTerm = (Phantoms.unTTerm post)}]})))

forClauseInit :: (Phantoms.TTerm Syntax.ForClause -> Phantoms.TTerm (Maybe Syntax.SimpleStmt))
forClauseInit x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.ForClause"),
    Core.projectionField = (Core.Name "init")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

forClauseCondition :: (Phantoms.TTerm Syntax.ForClause -> Phantoms.TTerm (Maybe Syntax.Expression))
forClauseCondition x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.ForClause"),
    Core.projectionField = (Core.Name "condition")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

forClausePost :: (Phantoms.TTerm Syntax.ForClause -> Phantoms.TTerm (Maybe Syntax.SimpleStmt))
forClausePost x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.ForClause"),
    Core.projectionField = (Core.Name "post")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

forClauseWithInit :: (Phantoms.TTerm Syntax.ForClause -> Phantoms.TTerm (Maybe Syntax.SimpleStmt) -> Phantoms.TTerm Syntax.ForClause)
forClauseWithInit original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.ForClause"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "init"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "condition"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.ForClause"),
          Core.projectionField = (Core.Name "condition")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "post"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.ForClause"),
          Core.projectionField = (Core.Name "post")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

forClauseWithCondition :: (Phantoms.TTerm Syntax.ForClause -> Phantoms.TTerm (Maybe Syntax.Expression) -> Phantoms.TTerm Syntax.ForClause)
forClauseWithCondition original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.ForClause"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "init"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.ForClause"),
          Core.projectionField = (Core.Name "init")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "condition"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "post"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.ForClause"),
          Core.projectionField = (Core.Name "post")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

forClauseWithPost :: (Phantoms.TTerm Syntax.ForClause -> Phantoms.TTerm (Maybe Syntax.SimpleStmt) -> Phantoms.TTerm Syntax.ForClause)
forClauseWithPost original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.ForClause"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "init"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.ForClause"),
          Core.projectionField = (Core.Name "init")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "condition"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.ForClause"),
          Core.projectionField = (Core.Name "condition")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "post"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

rangeClause :: (Phantoms.TTerm (Maybe Syntax.RangeVars) -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.RangeClause)
rangeClause vars expression = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.RangeClause"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "vars"),
      Core.fieldTerm = (Phantoms.unTTerm vars)},
    Core.Field {
      Core.fieldName = (Core.Name "expression"),
      Core.fieldTerm = (Phantoms.unTTerm expression)}]})))

rangeClauseVars :: (Phantoms.TTerm Syntax.RangeClause -> Phantoms.TTerm (Maybe Syntax.RangeVars))
rangeClauseVars x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.RangeClause"),
    Core.projectionField = (Core.Name "vars")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

rangeClauseExpression :: (Phantoms.TTerm Syntax.RangeClause -> Phantoms.TTerm Syntax.Expression)
rangeClauseExpression x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.RangeClause"),
    Core.projectionField = (Core.Name "expression")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

rangeClauseWithVars :: (Phantoms.TTerm Syntax.RangeClause -> Phantoms.TTerm (Maybe Syntax.RangeVars) -> Phantoms.TTerm Syntax.RangeClause)
rangeClauseWithVars original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.RangeClause"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "vars"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "expression"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.RangeClause"),
          Core.projectionField = (Core.Name "expression")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

rangeClauseWithExpression :: (Phantoms.TTerm Syntax.RangeClause -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.RangeClause)
rangeClauseWithExpression original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.RangeClause"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "vars"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.RangeClause"),
          Core.projectionField = (Core.Name "vars")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "expression"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

rangeVarsAssign :: (Phantoms.TTerm [Syntax.Expression] -> Phantoms.TTerm Syntax.RangeVars)
rangeVarsAssign x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.go.syntax.RangeVars"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "assign"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

rangeVarsDeclare :: (Phantoms.TTerm [Syntax.Identifier] -> Phantoms.TTerm Syntax.RangeVars)
rangeVarsDeclare x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.go.syntax.RangeVars"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "declare"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

goStmt :: (Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.GoStmt)
goStmt x = (Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
  Core.wrappedTermTypeName = (Core.Name "hydra.ext.go.syntax.GoStmt"),
  Core.wrappedTermBody = (Phantoms.unTTerm x)})))

unGoStmt :: (Phantoms.TTerm Syntax.GoStmt -> Phantoms.TTerm Syntax.Expression)
unGoStmt x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.go.syntax.GoStmt")))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

selectStmt :: (Phantoms.TTerm [Syntax.CommClause] -> Phantoms.TTerm Syntax.SelectStmt)
selectStmt x = (Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
  Core.wrappedTermTypeName = (Core.Name "hydra.ext.go.syntax.SelectStmt"),
  Core.wrappedTermBody = (Phantoms.unTTerm x)})))

unSelectStmt :: (Phantoms.TTerm Syntax.SelectStmt -> Phantoms.TTerm [Syntax.CommClause])
unSelectStmt x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.go.syntax.SelectStmt")))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

commClause :: (Phantoms.TTerm Syntax.CommCase -> Phantoms.TTerm [Syntax.Statement] -> Phantoms.TTerm Syntax.CommClause)
commClause case_ statements = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.CommClause"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "case"),
      Core.fieldTerm = (Phantoms.unTTerm case_)},
    Core.Field {
      Core.fieldName = (Core.Name "statements"),
      Core.fieldTerm = (Phantoms.unTTerm statements)}]})))

commClauseCase :: (Phantoms.TTerm Syntax.CommClause -> Phantoms.TTerm Syntax.CommCase)
commClauseCase x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.CommClause"),
    Core.projectionField = (Core.Name "case")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

commClauseStatements :: (Phantoms.TTerm Syntax.CommClause -> Phantoms.TTerm [Syntax.Statement])
commClauseStatements x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.CommClause"),
    Core.projectionField = (Core.Name "statements")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

commClauseWithCase :: (Phantoms.TTerm Syntax.CommClause -> Phantoms.TTerm Syntax.CommCase -> Phantoms.TTerm Syntax.CommClause)
commClauseWithCase original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.CommClause"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "case"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "statements"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.CommClause"),
          Core.projectionField = (Core.Name "statements")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

commClauseWithStatements :: (Phantoms.TTerm Syntax.CommClause -> Phantoms.TTerm [Syntax.Statement] -> Phantoms.TTerm Syntax.CommClause)
commClauseWithStatements original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.CommClause"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "case"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.CommClause"),
          Core.projectionField = (Core.Name "case")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "statements"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

commCaseSend :: (Phantoms.TTerm Syntax.SendStmt -> Phantoms.TTerm Syntax.CommCase)
commCaseSend x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.go.syntax.CommCase"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "send"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

commCaseReceive :: (Phantoms.TTerm Syntax.ReceiveCase -> Phantoms.TTerm Syntax.CommCase)
commCaseReceive x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.go.syntax.CommCase"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "receive"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

commCaseDefault :: (Phantoms.TTerm Syntax.CommCase)
commCaseDefault = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.go.syntax.CommCase"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "default"),
    Core.fieldTerm = Core.TermUnit}})))

receiveCase :: (Phantoms.TTerm (Maybe Syntax.RangeVars) -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.ReceiveCase)
receiveCase vars expression = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.ReceiveCase"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "vars"),
      Core.fieldTerm = (Phantoms.unTTerm vars)},
    Core.Field {
      Core.fieldName = (Core.Name "expression"),
      Core.fieldTerm = (Phantoms.unTTerm expression)}]})))

receiveCaseVars :: (Phantoms.TTerm Syntax.ReceiveCase -> Phantoms.TTerm (Maybe Syntax.RangeVars))
receiveCaseVars x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.ReceiveCase"),
    Core.projectionField = (Core.Name "vars")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

receiveCaseExpression :: (Phantoms.TTerm Syntax.ReceiveCase -> Phantoms.TTerm Syntax.Expression)
receiveCaseExpression x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.ReceiveCase"),
    Core.projectionField = (Core.Name "expression")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

receiveCaseWithVars :: (Phantoms.TTerm Syntax.ReceiveCase -> Phantoms.TTerm (Maybe Syntax.RangeVars) -> Phantoms.TTerm Syntax.ReceiveCase)
receiveCaseWithVars original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.ReceiveCase"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "vars"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "expression"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.ReceiveCase"),
          Core.projectionField = (Core.Name "expression")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

receiveCaseWithExpression :: (Phantoms.TTerm Syntax.ReceiveCase -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.ReceiveCase)
receiveCaseWithExpression original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.go.syntax.ReceiveCase"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "vars"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.go.syntax.ReceiveCase"),
          Core.projectionField = (Core.Name "vars")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "expression"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

returnStmt :: (Phantoms.TTerm [Syntax.Expression] -> Phantoms.TTerm Syntax.ReturnStmt)
returnStmt x = (Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
  Core.wrappedTermTypeName = (Core.Name "hydra.ext.go.syntax.ReturnStmt"),
  Core.wrappedTermBody = (Phantoms.unTTerm x)})))

unReturnStmt :: (Phantoms.TTerm Syntax.ReturnStmt -> Phantoms.TTerm [Syntax.Expression])
unReturnStmt x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.go.syntax.ReturnStmt")))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

breakStmt :: (Phantoms.TTerm (Maybe Syntax.Identifier) -> Phantoms.TTerm Syntax.BreakStmt)
breakStmt x = (Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
  Core.wrappedTermTypeName = (Core.Name "hydra.ext.go.syntax.BreakStmt"),
  Core.wrappedTermBody = (Phantoms.unTTerm x)})))

unBreakStmt :: (Phantoms.TTerm Syntax.BreakStmt -> Phantoms.TTerm (Maybe Syntax.Identifier))
unBreakStmt x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.go.syntax.BreakStmt")))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

continueStmt :: (Phantoms.TTerm (Maybe Syntax.Identifier) -> Phantoms.TTerm Syntax.ContinueStmt)
continueStmt x = (Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
  Core.wrappedTermTypeName = (Core.Name "hydra.ext.go.syntax.ContinueStmt"),
  Core.wrappedTermBody = (Phantoms.unTTerm x)})))

unContinueStmt :: (Phantoms.TTerm Syntax.ContinueStmt -> Phantoms.TTerm (Maybe Syntax.Identifier))
unContinueStmt x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.go.syntax.ContinueStmt")))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

gotoStmt :: (Phantoms.TTerm Syntax.Identifier -> Phantoms.TTerm Syntax.GotoStmt)
gotoStmt x = (Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
  Core.wrappedTermTypeName = (Core.Name "hydra.ext.go.syntax.GotoStmt"),
  Core.wrappedTermBody = (Phantoms.unTTerm x)})))

unGotoStmt :: (Phantoms.TTerm Syntax.GotoStmt -> Phantoms.TTerm Syntax.Identifier)
unGotoStmt x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.go.syntax.GotoStmt")))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

fallthroughStmt :: (Phantoms.TTerm () -> Phantoms.TTerm Syntax.FallthroughStmt)
fallthroughStmt x = (Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
  Core.wrappedTermTypeName = (Core.Name "hydra.ext.go.syntax.FallthroughStmt"),
  Core.wrappedTermBody = (Phantoms.unTTerm x)})))

unFallthroughStmt :: (Phantoms.TTerm Syntax.FallthroughStmt -> Phantoms.TTerm ())
unFallthroughStmt x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.go.syntax.FallthroughStmt")))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

deferStmt :: (Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.DeferStmt)
deferStmt x = (Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
  Core.wrappedTermTypeName = (Core.Name "hydra.ext.go.syntax.DeferStmt"),
  Core.wrappedTermBody = (Phantoms.unTTerm x)})))

unDeferStmt :: (Phantoms.TTerm Syntax.DeferStmt -> Phantoms.TTerm Syntax.Expression)
unDeferStmt x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.go.syntax.DeferStmt")))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

block :: (Phantoms.TTerm [Syntax.Statement] -> Phantoms.TTerm Syntax.Block)
block x = (Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
  Core.wrappedTermTypeName = (Core.Name "hydra.ext.go.syntax.Block"),
  Core.wrappedTermBody = (Phantoms.unTTerm x)})))

unBlock :: (Phantoms.TTerm Syntax.Block -> Phantoms.TTerm [Syntax.Statement])
unBlock x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.go.syntax.Block")))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

unaryOpPlus :: (Phantoms.TTerm Syntax.UnaryOp)
unaryOpPlus = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.go.syntax.UnaryOp"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "plus"),
    Core.fieldTerm = Core.TermUnit}})))

unaryOpMinus :: (Phantoms.TTerm Syntax.UnaryOp)
unaryOpMinus = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.go.syntax.UnaryOp"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "minus"),
    Core.fieldTerm = Core.TermUnit}})))

unaryOpNot :: (Phantoms.TTerm Syntax.UnaryOp)
unaryOpNot = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.go.syntax.UnaryOp"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "not"),
    Core.fieldTerm = Core.TermUnit}})))

unaryOpXor :: (Phantoms.TTerm Syntax.UnaryOp)
unaryOpXor = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.go.syntax.UnaryOp"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "xor"),
    Core.fieldTerm = Core.TermUnit}})))

unaryOpDeref :: (Phantoms.TTerm Syntax.UnaryOp)
unaryOpDeref = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.go.syntax.UnaryOp"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "deref"),
    Core.fieldTerm = Core.TermUnit}})))

unaryOpAddressOf :: (Phantoms.TTerm Syntax.UnaryOp)
unaryOpAddressOf = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.go.syntax.UnaryOp"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "addressOf"),
    Core.fieldTerm = Core.TermUnit}})))

unaryOpReceive :: (Phantoms.TTerm Syntax.UnaryOp)
unaryOpReceive = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.go.syntax.UnaryOp"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "receive"),
    Core.fieldTerm = Core.TermUnit}})))

mulOpMultiply :: (Phantoms.TTerm Syntax.MulOp)
mulOpMultiply = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.go.syntax.MulOp"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "multiply"),
    Core.fieldTerm = Core.TermUnit}})))

mulOpDivide :: (Phantoms.TTerm Syntax.MulOp)
mulOpDivide = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.go.syntax.MulOp"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "divide"),
    Core.fieldTerm = Core.TermUnit}})))

mulOpRemainder :: (Phantoms.TTerm Syntax.MulOp)
mulOpRemainder = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.go.syntax.MulOp"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "remainder"),
    Core.fieldTerm = Core.TermUnit}})))

mulOpLeftShift :: (Phantoms.TTerm Syntax.MulOp)
mulOpLeftShift = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.go.syntax.MulOp"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "leftShift"),
    Core.fieldTerm = Core.TermUnit}})))

mulOpRightShift :: (Phantoms.TTerm Syntax.MulOp)
mulOpRightShift = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.go.syntax.MulOp"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "rightShift"),
    Core.fieldTerm = Core.TermUnit}})))

mulOpBitwiseAnd :: (Phantoms.TTerm Syntax.MulOp)
mulOpBitwiseAnd = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.go.syntax.MulOp"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "bitwiseAnd"),
    Core.fieldTerm = Core.TermUnit}})))

mulOpBitClear :: (Phantoms.TTerm Syntax.MulOp)
mulOpBitClear = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.go.syntax.MulOp"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "bitClear"),
    Core.fieldTerm = Core.TermUnit}})))

addOpAdd :: (Phantoms.TTerm Syntax.AddOp)
addOpAdd = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.go.syntax.AddOp"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "add"),
    Core.fieldTerm = Core.TermUnit}})))

addOpSubtract :: (Phantoms.TTerm Syntax.AddOp)
addOpSubtract = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.go.syntax.AddOp"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "subtract"),
    Core.fieldTerm = Core.TermUnit}})))

addOpBitwiseOr :: (Phantoms.TTerm Syntax.AddOp)
addOpBitwiseOr = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.go.syntax.AddOp"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "bitwiseOr"),
    Core.fieldTerm = Core.TermUnit}})))

addOpBitwiseXor :: (Phantoms.TTerm Syntax.AddOp)
addOpBitwiseXor = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.go.syntax.AddOp"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "bitwiseXor"),
    Core.fieldTerm = Core.TermUnit}})))

relOpEqual :: (Phantoms.TTerm Syntax.RelOp)
relOpEqual = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.go.syntax.RelOp"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "equal"),
    Core.fieldTerm = Core.TermUnit}})))

relOpNotEqual :: (Phantoms.TTerm Syntax.RelOp)
relOpNotEqual = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.go.syntax.RelOp"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "notEqual"),
    Core.fieldTerm = Core.TermUnit}})))

relOpLess :: (Phantoms.TTerm Syntax.RelOp)
relOpLess = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.go.syntax.RelOp"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "less"),
    Core.fieldTerm = Core.TermUnit}})))

relOpLessEqual :: (Phantoms.TTerm Syntax.RelOp)
relOpLessEqual = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.go.syntax.RelOp"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "lessEqual"),
    Core.fieldTerm = Core.TermUnit}})))

relOpGreater :: (Phantoms.TTerm Syntax.RelOp)
relOpGreater = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.go.syntax.RelOp"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "greater"),
    Core.fieldTerm = Core.TermUnit}})))

relOpGreaterEqual :: (Phantoms.TTerm Syntax.RelOp)
relOpGreaterEqual = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.go.syntax.RelOp"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "greaterEqual"),
    Core.fieldTerm = Core.TermUnit}})))
