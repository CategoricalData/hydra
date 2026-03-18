-- Note: this is an automatically generated file. Do not edit.

-- | DSL functions for hydra.ext.lisp.syntax

module Hydra.Dsl.Ext.Lisp.Syntax where

import qualified Hydra.Core as Core
import qualified Hydra.Ext.Lisp.Syntax as Syntax
import qualified Hydra.Phantoms as Phantoms
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

program :: Phantoms.TTerm Syntax.Dialect -> Phantoms.TTerm (Maybe Syntax.ModuleDeclaration) -> Phantoms.TTerm [Syntax.ImportDeclaration] -> Phantoms.TTerm [Syntax.ExportDeclaration] -> Phantoms.TTerm [Syntax.TopLevelFormWithComments] -> Phantoms.TTerm Syntax.Program
program dialect module_ imports exports forms =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.lisp.syntax.Program"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "dialect"),
          Core.fieldTerm = (Phantoms.unTTerm dialect)},
        Core.Field {
          Core.fieldName = (Core.Name "module"),
          Core.fieldTerm = (Phantoms.unTTerm module_)},
        Core.Field {
          Core.fieldName = (Core.Name "imports"),
          Core.fieldTerm = (Phantoms.unTTerm imports)},
        Core.Field {
          Core.fieldName = (Core.Name "exports"),
          Core.fieldTerm = (Phantoms.unTTerm exports)},
        Core.Field {
          Core.fieldName = (Core.Name "forms"),
          Core.fieldTerm = (Phantoms.unTTerm forms)}]}))

programDialect :: Phantoms.TTerm Syntax.Program -> Phantoms.TTerm Syntax.Dialect
programDialect x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.Program"),
        Core.projectionField = (Core.Name "dialect")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

programModule :: Phantoms.TTerm Syntax.Program -> Phantoms.TTerm (Maybe Syntax.ModuleDeclaration)
programModule x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.Program"),
        Core.projectionField = (Core.Name "module")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

programImports :: Phantoms.TTerm Syntax.Program -> Phantoms.TTerm [Syntax.ImportDeclaration]
programImports x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.Program"),
        Core.projectionField = (Core.Name "imports")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

programExports :: Phantoms.TTerm Syntax.Program -> Phantoms.TTerm [Syntax.ExportDeclaration]
programExports x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.Program"),
        Core.projectionField = (Core.Name "exports")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

programForms :: Phantoms.TTerm Syntax.Program -> Phantoms.TTerm [Syntax.TopLevelFormWithComments]
programForms x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.Program"),
        Core.projectionField = (Core.Name "forms")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

programWithDialect :: Phantoms.TTerm Syntax.Program -> Phantoms.TTerm Syntax.Dialect -> Phantoms.TTerm Syntax.Program
programWithDialect original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.lisp.syntax.Program"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "dialect"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "module"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.Program"),
              Core.projectionField = (Core.Name "module")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "imports"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.Program"),
              Core.projectionField = (Core.Name "imports")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "exports"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.Program"),
              Core.projectionField = (Core.Name "exports")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "forms"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.Program"),
              Core.projectionField = (Core.Name "forms")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

programWithModule :: Phantoms.TTerm Syntax.Program -> Phantoms.TTerm (Maybe Syntax.ModuleDeclaration) -> Phantoms.TTerm Syntax.Program
programWithModule original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.lisp.syntax.Program"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "dialect"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.Program"),
              Core.projectionField = (Core.Name "dialect")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "module"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "imports"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.Program"),
              Core.projectionField = (Core.Name "imports")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "exports"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.Program"),
              Core.projectionField = (Core.Name "exports")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "forms"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.Program"),
              Core.projectionField = (Core.Name "forms")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

programWithImports :: Phantoms.TTerm Syntax.Program -> Phantoms.TTerm [Syntax.ImportDeclaration] -> Phantoms.TTerm Syntax.Program
programWithImports original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.lisp.syntax.Program"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "dialect"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.Program"),
              Core.projectionField = (Core.Name "dialect")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "module"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.Program"),
              Core.projectionField = (Core.Name "module")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "imports"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "exports"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.Program"),
              Core.projectionField = (Core.Name "exports")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "forms"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.Program"),
              Core.projectionField = (Core.Name "forms")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

programWithExports :: Phantoms.TTerm Syntax.Program -> Phantoms.TTerm [Syntax.ExportDeclaration] -> Phantoms.TTerm Syntax.Program
programWithExports original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.lisp.syntax.Program"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "dialect"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.Program"),
              Core.projectionField = (Core.Name "dialect")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "module"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.Program"),
              Core.projectionField = (Core.Name "module")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "imports"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.Program"),
              Core.projectionField = (Core.Name "imports")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "exports"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "forms"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.Program"),
              Core.projectionField = (Core.Name "forms")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

programWithForms :: Phantoms.TTerm Syntax.Program -> Phantoms.TTerm [Syntax.TopLevelFormWithComments] -> Phantoms.TTerm Syntax.Program
programWithForms original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.lisp.syntax.Program"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "dialect"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.Program"),
              Core.projectionField = (Core.Name "dialect")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "module"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.Program"),
              Core.projectionField = (Core.Name "module")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "imports"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.Program"),
              Core.projectionField = (Core.Name "imports")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "exports"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.Program"),
              Core.projectionField = (Core.Name "exports")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "forms"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

topLevelFormFunction :: Phantoms.TTerm Syntax.FunctionDefinition -> Phantoms.TTerm Syntax.TopLevelForm
topLevelFormFunction x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.lisp.syntax.TopLevelForm"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "function"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

topLevelFormVariable :: Phantoms.TTerm Syntax.VariableDefinition -> Phantoms.TTerm Syntax.TopLevelForm
topLevelFormVariable x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.lisp.syntax.TopLevelForm"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "variable"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

topLevelFormConstant :: Phantoms.TTerm Syntax.ConstantDefinition -> Phantoms.TTerm Syntax.TopLevelForm
topLevelFormConstant x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.lisp.syntax.TopLevelForm"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "constant"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

topLevelFormRecordType :: Phantoms.TTerm Syntax.RecordTypeDefinition -> Phantoms.TTerm Syntax.TopLevelForm
topLevelFormRecordType x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.lisp.syntax.TopLevelForm"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "recordType"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

topLevelFormMacro :: Phantoms.TTerm Syntax.MacroDefinition -> Phantoms.TTerm Syntax.TopLevelForm
topLevelFormMacro x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.lisp.syntax.TopLevelForm"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "macro"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

topLevelFormExpression :: Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.TopLevelForm
topLevelFormExpression x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.lisp.syntax.TopLevelForm"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "expression"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

topLevelFormWithComments :: Phantoms.TTerm (Maybe Syntax.Docstring) -> Phantoms.TTerm (Maybe Syntax.Comment) -> Phantoms.TTerm Syntax.TopLevelForm -> Phantoms.TTerm Syntax.TopLevelFormWithComments
topLevelFormWithComments doc comment form =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.lisp.syntax.TopLevelFormWithComments"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Phantoms.unTTerm doc)},
        Core.Field {
          Core.fieldName = (Core.Name "comment"),
          Core.fieldTerm = (Phantoms.unTTerm comment)},
        Core.Field {
          Core.fieldName = (Core.Name "form"),
          Core.fieldTerm = (Phantoms.unTTerm form)}]}))

topLevelFormWithCommentsDoc :: Phantoms.TTerm Syntax.TopLevelFormWithComments -> Phantoms.TTerm (Maybe Syntax.Docstring)
topLevelFormWithCommentsDoc x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.TopLevelFormWithComments"),
        Core.projectionField = (Core.Name "doc")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

topLevelFormWithCommentsComment :: Phantoms.TTerm Syntax.TopLevelFormWithComments -> Phantoms.TTerm (Maybe Syntax.Comment)
topLevelFormWithCommentsComment x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.TopLevelFormWithComments"),
        Core.projectionField = (Core.Name "comment")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

topLevelFormWithCommentsForm :: Phantoms.TTerm Syntax.TopLevelFormWithComments -> Phantoms.TTerm Syntax.TopLevelForm
topLevelFormWithCommentsForm x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.TopLevelFormWithComments"),
        Core.projectionField = (Core.Name "form")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

topLevelFormWithCommentsWithDoc :: Phantoms.TTerm Syntax.TopLevelFormWithComments -> Phantoms.TTerm (Maybe Syntax.Docstring) -> Phantoms.TTerm Syntax.TopLevelFormWithComments
topLevelFormWithCommentsWithDoc original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.lisp.syntax.TopLevelFormWithComments"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "comment"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.TopLevelFormWithComments"),
              Core.projectionField = (Core.Name "comment")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "form"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.TopLevelFormWithComments"),
              Core.projectionField = (Core.Name "form")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

topLevelFormWithCommentsWithComment :: Phantoms.TTerm Syntax.TopLevelFormWithComments -> Phantoms.TTerm (Maybe Syntax.Comment) -> Phantoms.TTerm Syntax.TopLevelFormWithComments
topLevelFormWithCommentsWithComment original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.lisp.syntax.TopLevelFormWithComments"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.TopLevelFormWithComments"),
              Core.projectionField = (Core.Name "doc")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "comment"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "form"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.TopLevelFormWithComments"),
              Core.projectionField = (Core.Name "form")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

topLevelFormWithCommentsWithForm :: Phantoms.TTerm Syntax.TopLevelFormWithComments -> Phantoms.TTerm Syntax.TopLevelForm -> Phantoms.TTerm Syntax.TopLevelFormWithComments
topLevelFormWithCommentsWithForm original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.lisp.syntax.TopLevelFormWithComments"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.TopLevelFormWithComments"),
              Core.projectionField = (Core.Name "doc")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "comment"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.TopLevelFormWithComments"),
              Core.projectionField = (Core.Name "comment")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "form"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

functionDefinition :: Phantoms.TTerm Syntax.Symbol -> Phantoms.TTerm [Syntax.Symbol] -> Phantoms.TTerm (Maybe Syntax.Symbol) -> Phantoms.TTerm (Maybe Syntax.Docstring) -> Phantoms.TTerm [Syntax.TypeHint] -> Phantoms.TTerm [Syntax.Expression] -> Phantoms.TTerm Syntax.FunctionDefinition
functionDefinition name params restParam doc typeHints body =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.lisp.syntax.FunctionDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Phantoms.unTTerm params)},
        Core.Field {
          Core.fieldName = (Core.Name "restParam"),
          Core.fieldTerm = (Phantoms.unTTerm restParam)},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Phantoms.unTTerm doc)},
        Core.Field {
          Core.fieldName = (Core.Name "typeHints"),
          Core.fieldTerm = (Phantoms.unTTerm typeHints)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm body)}]}))

functionDefinitionName :: Phantoms.TTerm Syntax.FunctionDefinition -> Phantoms.TTerm Syntax.Symbol
functionDefinitionName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.FunctionDefinition"),
        Core.projectionField = (Core.Name "name")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

functionDefinitionParams :: Phantoms.TTerm Syntax.FunctionDefinition -> Phantoms.TTerm [Syntax.Symbol]
functionDefinitionParams x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.FunctionDefinition"),
        Core.projectionField = (Core.Name "params")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

functionDefinitionRestParam :: Phantoms.TTerm Syntax.FunctionDefinition -> Phantoms.TTerm (Maybe Syntax.Symbol)
functionDefinitionRestParam x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.FunctionDefinition"),
        Core.projectionField = (Core.Name "restParam")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

functionDefinitionDoc :: Phantoms.TTerm Syntax.FunctionDefinition -> Phantoms.TTerm (Maybe Syntax.Docstring)
functionDefinitionDoc x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.FunctionDefinition"),
        Core.projectionField = (Core.Name "doc")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

functionDefinitionTypeHints :: Phantoms.TTerm Syntax.FunctionDefinition -> Phantoms.TTerm [Syntax.TypeHint]
functionDefinitionTypeHints x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.FunctionDefinition"),
        Core.projectionField = (Core.Name "typeHints")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

functionDefinitionBody :: Phantoms.TTerm Syntax.FunctionDefinition -> Phantoms.TTerm [Syntax.Expression]
functionDefinitionBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.FunctionDefinition"),
        Core.projectionField = (Core.Name "body")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

functionDefinitionWithName :: Phantoms.TTerm Syntax.FunctionDefinition -> Phantoms.TTerm Syntax.Symbol -> Phantoms.TTerm Syntax.FunctionDefinition
functionDefinitionWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.lisp.syntax.FunctionDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.FunctionDefinition"),
              Core.projectionField = (Core.Name "params")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "restParam"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.FunctionDefinition"),
              Core.projectionField = (Core.Name "restParam")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.FunctionDefinition"),
              Core.projectionField = (Core.Name "doc")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeHints"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.FunctionDefinition"),
              Core.projectionField = (Core.Name "typeHints")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.FunctionDefinition"),
              Core.projectionField = (Core.Name "body")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

functionDefinitionWithParams :: Phantoms.TTerm Syntax.FunctionDefinition -> Phantoms.TTerm [Syntax.Symbol] -> Phantoms.TTerm Syntax.FunctionDefinition
functionDefinitionWithParams original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.lisp.syntax.FunctionDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.FunctionDefinition"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "restParam"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.FunctionDefinition"),
              Core.projectionField = (Core.Name "restParam")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.FunctionDefinition"),
              Core.projectionField = (Core.Name "doc")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeHints"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.FunctionDefinition"),
              Core.projectionField = (Core.Name "typeHints")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.FunctionDefinition"),
              Core.projectionField = (Core.Name "body")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

functionDefinitionWithRestParam :: Phantoms.TTerm Syntax.FunctionDefinition -> Phantoms.TTerm (Maybe Syntax.Symbol) -> Phantoms.TTerm Syntax.FunctionDefinition
functionDefinitionWithRestParam original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.lisp.syntax.FunctionDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.FunctionDefinition"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.FunctionDefinition"),
              Core.projectionField = (Core.Name "params")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "restParam"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.FunctionDefinition"),
              Core.projectionField = (Core.Name "doc")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeHints"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.FunctionDefinition"),
              Core.projectionField = (Core.Name "typeHints")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.FunctionDefinition"),
              Core.projectionField = (Core.Name "body")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

functionDefinitionWithDoc :: Phantoms.TTerm Syntax.FunctionDefinition -> Phantoms.TTerm (Maybe Syntax.Docstring) -> Phantoms.TTerm Syntax.FunctionDefinition
functionDefinitionWithDoc original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.lisp.syntax.FunctionDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.FunctionDefinition"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.FunctionDefinition"),
              Core.projectionField = (Core.Name "params")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "restParam"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.FunctionDefinition"),
              Core.projectionField = (Core.Name "restParam")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "typeHints"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.FunctionDefinition"),
              Core.projectionField = (Core.Name "typeHints")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.FunctionDefinition"),
              Core.projectionField = (Core.Name "body")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

functionDefinitionWithTypeHints :: Phantoms.TTerm Syntax.FunctionDefinition -> Phantoms.TTerm [Syntax.TypeHint] -> Phantoms.TTerm Syntax.FunctionDefinition
functionDefinitionWithTypeHints original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.lisp.syntax.FunctionDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.FunctionDefinition"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.FunctionDefinition"),
              Core.projectionField = (Core.Name "params")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "restParam"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.FunctionDefinition"),
              Core.projectionField = (Core.Name "restParam")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.FunctionDefinition"),
              Core.projectionField = (Core.Name "doc")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeHints"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.FunctionDefinition"),
              Core.projectionField = (Core.Name "body")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

functionDefinitionWithBody :: Phantoms.TTerm Syntax.FunctionDefinition -> Phantoms.TTerm [Syntax.Expression] -> Phantoms.TTerm Syntax.FunctionDefinition
functionDefinitionWithBody original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.lisp.syntax.FunctionDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.FunctionDefinition"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.FunctionDefinition"),
              Core.projectionField = (Core.Name "params")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "restParam"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.FunctionDefinition"),
              Core.projectionField = (Core.Name "restParam")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.FunctionDefinition"),
              Core.projectionField = (Core.Name "doc")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeHints"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.FunctionDefinition"),
              Core.projectionField = (Core.Name "typeHints")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

variableDefinition :: Phantoms.TTerm Syntax.Symbol -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm (Maybe Syntax.Docstring) -> Phantoms.TTerm Syntax.VariableDefinition
variableDefinition name value doc =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.lisp.syntax.VariableDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm value)},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Phantoms.unTTerm doc)}]}))

variableDefinitionName :: Phantoms.TTerm Syntax.VariableDefinition -> Phantoms.TTerm Syntax.Symbol
variableDefinitionName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.VariableDefinition"),
        Core.projectionField = (Core.Name "name")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

variableDefinitionValue :: Phantoms.TTerm Syntax.VariableDefinition -> Phantoms.TTerm Syntax.Expression
variableDefinitionValue x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.VariableDefinition"),
        Core.projectionField = (Core.Name "value")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

variableDefinitionDoc :: Phantoms.TTerm Syntax.VariableDefinition -> Phantoms.TTerm (Maybe Syntax.Docstring)
variableDefinitionDoc x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.VariableDefinition"),
        Core.projectionField = (Core.Name "doc")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

variableDefinitionWithName :: Phantoms.TTerm Syntax.VariableDefinition -> Phantoms.TTerm Syntax.Symbol -> Phantoms.TTerm Syntax.VariableDefinition
variableDefinitionWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.lisp.syntax.VariableDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.VariableDefinition"),
              Core.projectionField = (Core.Name "value")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.VariableDefinition"),
              Core.projectionField = (Core.Name "doc")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

variableDefinitionWithValue :: Phantoms.TTerm Syntax.VariableDefinition -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.VariableDefinition
variableDefinitionWithValue original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.lisp.syntax.VariableDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.VariableDefinition"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.VariableDefinition"),
              Core.projectionField = (Core.Name "doc")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

variableDefinitionWithDoc :: Phantoms.TTerm Syntax.VariableDefinition -> Phantoms.TTerm (Maybe Syntax.Docstring) -> Phantoms.TTerm Syntax.VariableDefinition
variableDefinitionWithDoc original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.lisp.syntax.VariableDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.VariableDefinition"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.VariableDefinition"),
              Core.projectionField = (Core.Name "value")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

constantDefinition :: Phantoms.TTerm Syntax.Symbol -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm (Maybe Syntax.Docstring) -> Phantoms.TTerm Syntax.ConstantDefinition
constantDefinition name value doc =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.lisp.syntax.ConstantDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm value)},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Phantoms.unTTerm doc)}]}))

constantDefinitionName :: Phantoms.TTerm Syntax.ConstantDefinition -> Phantoms.TTerm Syntax.Symbol
constantDefinitionName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.ConstantDefinition"),
        Core.projectionField = (Core.Name "name")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

constantDefinitionValue :: Phantoms.TTerm Syntax.ConstantDefinition -> Phantoms.TTerm Syntax.Expression
constantDefinitionValue x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.ConstantDefinition"),
        Core.projectionField = (Core.Name "value")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

constantDefinitionDoc :: Phantoms.TTerm Syntax.ConstantDefinition -> Phantoms.TTerm (Maybe Syntax.Docstring)
constantDefinitionDoc x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.ConstantDefinition"),
        Core.projectionField = (Core.Name "doc")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

constantDefinitionWithName :: Phantoms.TTerm Syntax.ConstantDefinition -> Phantoms.TTerm Syntax.Symbol -> Phantoms.TTerm Syntax.ConstantDefinition
constantDefinitionWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.lisp.syntax.ConstantDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.ConstantDefinition"),
              Core.projectionField = (Core.Name "value")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.ConstantDefinition"),
              Core.projectionField = (Core.Name "doc")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

constantDefinitionWithValue :: Phantoms.TTerm Syntax.ConstantDefinition -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.ConstantDefinition
constantDefinitionWithValue original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.lisp.syntax.ConstantDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.ConstantDefinition"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.ConstantDefinition"),
              Core.projectionField = (Core.Name "doc")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

constantDefinitionWithDoc :: Phantoms.TTerm Syntax.ConstantDefinition -> Phantoms.TTerm (Maybe Syntax.Docstring) -> Phantoms.TTerm Syntax.ConstantDefinition
constantDefinitionWithDoc original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.lisp.syntax.ConstantDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.ConstantDefinition"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.ConstantDefinition"),
              Core.projectionField = (Core.Name "value")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

recordTypeDefinition :: Phantoms.TTerm Syntax.Symbol -> Phantoms.TTerm [Syntax.FieldDefinition] -> Phantoms.TTerm (Maybe Syntax.Docstring) -> Phantoms.TTerm Syntax.RecordTypeDefinition
recordTypeDefinition name fields doc =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.lisp.syntax.RecordTypeDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "fields"),
          Core.fieldTerm = (Phantoms.unTTerm fields)},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Phantoms.unTTerm doc)}]}))

recordTypeDefinitionName :: Phantoms.TTerm Syntax.RecordTypeDefinition -> Phantoms.TTerm Syntax.Symbol
recordTypeDefinitionName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.RecordTypeDefinition"),
        Core.projectionField = (Core.Name "name")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

recordTypeDefinitionFields :: Phantoms.TTerm Syntax.RecordTypeDefinition -> Phantoms.TTerm [Syntax.FieldDefinition]
recordTypeDefinitionFields x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.RecordTypeDefinition"),
        Core.projectionField = (Core.Name "fields")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

recordTypeDefinitionDoc :: Phantoms.TTerm Syntax.RecordTypeDefinition -> Phantoms.TTerm (Maybe Syntax.Docstring)
recordTypeDefinitionDoc x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.RecordTypeDefinition"),
        Core.projectionField = (Core.Name "doc")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

recordTypeDefinitionWithName :: Phantoms.TTerm Syntax.RecordTypeDefinition -> Phantoms.TTerm Syntax.Symbol -> Phantoms.TTerm Syntax.RecordTypeDefinition
recordTypeDefinitionWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.lisp.syntax.RecordTypeDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "fields"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.RecordTypeDefinition"),
              Core.projectionField = (Core.Name "fields")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.RecordTypeDefinition"),
              Core.projectionField = (Core.Name "doc")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

recordTypeDefinitionWithFields :: Phantoms.TTerm Syntax.RecordTypeDefinition -> Phantoms.TTerm [Syntax.FieldDefinition] -> Phantoms.TTerm Syntax.RecordTypeDefinition
recordTypeDefinitionWithFields original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.lisp.syntax.RecordTypeDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.RecordTypeDefinition"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "fields"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.RecordTypeDefinition"),
              Core.projectionField = (Core.Name "doc")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

recordTypeDefinitionWithDoc :: Phantoms.TTerm Syntax.RecordTypeDefinition -> Phantoms.TTerm (Maybe Syntax.Docstring) -> Phantoms.TTerm Syntax.RecordTypeDefinition
recordTypeDefinitionWithDoc original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.lisp.syntax.RecordTypeDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.RecordTypeDefinition"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "fields"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.RecordTypeDefinition"),
              Core.projectionField = (Core.Name "fields")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

fieldDefinition :: Phantoms.TTerm Syntax.Symbol -> Phantoms.TTerm (Maybe Syntax.Expression) -> Phantoms.TTerm Syntax.FieldDefinition
fieldDefinition name defaultValue =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.lisp.syntax.FieldDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "defaultValue"),
          Core.fieldTerm = (Phantoms.unTTerm defaultValue)}]}))

fieldDefinitionName :: Phantoms.TTerm Syntax.FieldDefinition -> Phantoms.TTerm Syntax.Symbol
fieldDefinitionName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.FieldDefinition"),
        Core.projectionField = (Core.Name "name")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

fieldDefinitionDefaultValue :: Phantoms.TTerm Syntax.FieldDefinition -> Phantoms.TTerm (Maybe Syntax.Expression)
fieldDefinitionDefaultValue x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.FieldDefinition"),
        Core.projectionField = (Core.Name "defaultValue")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

fieldDefinitionWithName :: Phantoms.TTerm Syntax.FieldDefinition -> Phantoms.TTerm Syntax.Symbol -> Phantoms.TTerm Syntax.FieldDefinition
fieldDefinitionWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.lisp.syntax.FieldDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "defaultValue"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.FieldDefinition"),
              Core.projectionField = (Core.Name "defaultValue")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

fieldDefinitionWithDefaultValue :: Phantoms.TTerm Syntax.FieldDefinition -> Phantoms.TTerm (Maybe Syntax.Expression) -> Phantoms.TTerm Syntax.FieldDefinition
fieldDefinitionWithDefaultValue original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.lisp.syntax.FieldDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.FieldDefinition"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "defaultValue"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

macroDefinition :: Phantoms.TTerm Syntax.Symbol -> Phantoms.TTerm [Syntax.Symbol] -> Phantoms.TTerm (Maybe Syntax.Symbol) -> Phantoms.TTerm [Syntax.Expression] -> Phantoms.TTerm Syntax.MacroDefinition
macroDefinition name params restParam body =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.lisp.syntax.MacroDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Phantoms.unTTerm params)},
        Core.Field {
          Core.fieldName = (Core.Name "restParam"),
          Core.fieldTerm = (Phantoms.unTTerm restParam)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm body)}]}))

macroDefinitionName :: Phantoms.TTerm Syntax.MacroDefinition -> Phantoms.TTerm Syntax.Symbol
macroDefinitionName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.MacroDefinition"),
        Core.projectionField = (Core.Name "name")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

macroDefinitionParams :: Phantoms.TTerm Syntax.MacroDefinition -> Phantoms.TTerm [Syntax.Symbol]
macroDefinitionParams x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.MacroDefinition"),
        Core.projectionField = (Core.Name "params")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

macroDefinitionRestParam :: Phantoms.TTerm Syntax.MacroDefinition -> Phantoms.TTerm (Maybe Syntax.Symbol)
macroDefinitionRestParam x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.MacroDefinition"),
        Core.projectionField = (Core.Name "restParam")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

macroDefinitionBody :: Phantoms.TTerm Syntax.MacroDefinition -> Phantoms.TTerm [Syntax.Expression]
macroDefinitionBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.MacroDefinition"),
        Core.projectionField = (Core.Name "body")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

macroDefinitionWithName :: Phantoms.TTerm Syntax.MacroDefinition -> Phantoms.TTerm Syntax.Symbol -> Phantoms.TTerm Syntax.MacroDefinition
macroDefinitionWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.lisp.syntax.MacroDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.MacroDefinition"),
              Core.projectionField = (Core.Name "params")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "restParam"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.MacroDefinition"),
              Core.projectionField = (Core.Name "restParam")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.MacroDefinition"),
              Core.projectionField = (Core.Name "body")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

macroDefinitionWithParams :: Phantoms.TTerm Syntax.MacroDefinition -> Phantoms.TTerm [Syntax.Symbol] -> Phantoms.TTerm Syntax.MacroDefinition
macroDefinitionWithParams original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.lisp.syntax.MacroDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.MacroDefinition"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "restParam"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.MacroDefinition"),
              Core.projectionField = (Core.Name "restParam")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.MacroDefinition"),
              Core.projectionField = (Core.Name "body")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

macroDefinitionWithRestParam :: Phantoms.TTerm Syntax.MacroDefinition -> Phantoms.TTerm (Maybe Syntax.Symbol) -> Phantoms.TTerm Syntax.MacroDefinition
macroDefinitionWithRestParam original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.lisp.syntax.MacroDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.MacroDefinition"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.MacroDefinition"),
              Core.projectionField = (Core.Name "params")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "restParam"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.MacroDefinition"),
              Core.projectionField = (Core.Name "body")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

macroDefinitionWithBody :: Phantoms.TTerm Syntax.MacroDefinition -> Phantoms.TTerm [Syntax.Expression] -> Phantoms.TTerm Syntax.MacroDefinition
macroDefinitionWithBody original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.lisp.syntax.MacroDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.MacroDefinition"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.MacroDefinition"),
              Core.projectionField = (Core.Name "params")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "restParam"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.MacroDefinition"),
              Core.projectionField = (Core.Name "restParam")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

expressionApplication :: Phantoms.TTerm Syntax.Application -> Phantoms.TTerm Syntax.Expression
expressionApplication x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.lisp.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "application"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

expressionLambda :: Phantoms.TTerm Syntax.Lambda -> Phantoms.TTerm Syntax.Expression
expressionLambda x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.lisp.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "lambda"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

expressionLet :: Phantoms.TTerm Syntax.LetExpression -> Phantoms.TTerm Syntax.Expression
expressionLet x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.lisp.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "let"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

expressionIf :: Phantoms.TTerm Syntax.IfExpression -> Phantoms.TTerm Syntax.Expression
expressionIf x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.lisp.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "if"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

expressionCond :: Phantoms.TTerm Syntax.CondExpression -> Phantoms.TTerm Syntax.Expression
expressionCond x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.lisp.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "cond"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

expressionCase :: Phantoms.TTerm Syntax.CaseExpression -> Phantoms.TTerm Syntax.Expression
expressionCase x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.lisp.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "case"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

expressionAnd :: Phantoms.TTerm Syntax.AndExpression -> Phantoms.TTerm Syntax.Expression
expressionAnd x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.lisp.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "and"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

expressionOr :: Phantoms.TTerm Syntax.OrExpression -> Phantoms.TTerm Syntax.Expression
expressionOr x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.lisp.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "or"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

expressionNot :: Phantoms.TTerm Syntax.NotExpression -> Phantoms.TTerm Syntax.Expression
expressionNot x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.lisp.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "not"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

expressionDo :: Phantoms.TTerm Syntax.DoExpression -> Phantoms.TTerm Syntax.Expression
expressionDo x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.lisp.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "do"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

expressionBegin :: Phantoms.TTerm Syntax.BeginExpression -> Phantoms.TTerm Syntax.Expression
expressionBegin x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.lisp.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "begin"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

expressionVariable :: Phantoms.TTerm Syntax.VariableReference -> Phantoms.TTerm Syntax.Expression
expressionVariable x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.lisp.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "variable"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

expressionLiteral :: Phantoms.TTerm Syntax.Literal -> Phantoms.TTerm Syntax.Expression
expressionLiteral x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.lisp.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "literal"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

expressionList :: Phantoms.TTerm Syntax.ListLiteral -> Phantoms.TTerm Syntax.Expression
expressionList x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.lisp.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "list"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

expressionVector :: Phantoms.TTerm Syntax.VectorLiteral -> Phantoms.TTerm Syntax.Expression
expressionVector x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.lisp.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "vector"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

expressionMap :: Phantoms.TTerm Syntax.MapLiteral -> Phantoms.TTerm Syntax.Expression
expressionMap x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.lisp.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "map"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

expressionSet :: Phantoms.TTerm Syntax.SetLiteral -> Phantoms.TTerm Syntax.Expression
expressionSet x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.lisp.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "set"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

expressionCons :: Phantoms.TTerm Syntax.ConsExpression -> Phantoms.TTerm Syntax.Expression
expressionCons x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.lisp.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "cons"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

expressionDottedPair :: Phantoms.TTerm Syntax.DottedPair -> Phantoms.TTerm Syntax.Expression
expressionDottedPair x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.lisp.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "dottedPair"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

expressionFieldAccess :: Phantoms.TTerm Syntax.FieldAccess -> Phantoms.TTerm Syntax.Expression
expressionFieldAccess x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.lisp.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "fieldAccess"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

expressionTypeAnnotation :: Phantoms.TTerm Syntax.TypeAnnotation -> Phantoms.TTerm Syntax.Expression
expressionTypeAnnotation x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.lisp.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "typeAnnotation"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

expressionQuote :: Phantoms.TTerm Syntax.QuoteExpression -> Phantoms.TTerm Syntax.Expression
expressionQuote x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.lisp.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "quote"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

expressionQuasiquote :: Phantoms.TTerm Syntax.QuasiquoteExpression -> Phantoms.TTerm Syntax.Expression
expressionQuasiquote x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.lisp.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "quasiquote"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

expressionUnquote :: Phantoms.TTerm Syntax.UnquoteExpression -> Phantoms.TTerm Syntax.Expression
expressionUnquote x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.lisp.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "unquote"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

expressionSplicingUnquote :: Phantoms.TTerm Syntax.SplicingUnquoteExpression -> Phantoms.TTerm Syntax.Expression
expressionSplicingUnquote x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.lisp.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "splicingUnquote"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

expressionSExpression :: Phantoms.TTerm Syntax.SExpression -> Phantoms.TTerm Syntax.Expression
expressionSExpression x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.lisp.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "sExpression"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

application :: Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm [Syntax.Expression] -> Phantoms.TTerm Syntax.Application
application function arguments =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.lisp.syntax.Application"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "function"),
          Core.fieldTerm = (Phantoms.unTTerm function)},
        Core.Field {
          Core.fieldName = (Core.Name "arguments"),
          Core.fieldTerm = (Phantoms.unTTerm arguments)}]}))

applicationFunction :: Phantoms.TTerm Syntax.Application -> Phantoms.TTerm Syntax.Expression
applicationFunction x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.Application"),
        Core.projectionField = (Core.Name "function")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

applicationArguments :: Phantoms.TTerm Syntax.Application -> Phantoms.TTerm [Syntax.Expression]
applicationArguments x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.Application"),
        Core.projectionField = (Core.Name "arguments")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

applicationWithFunction :: Phantoms.TTerm Syntax.Application -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.Application
applicationWithFunction original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.lisp.syntax.Application"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "function"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "arguments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.Application"),
              Core.projectionField = (Core.Name "arguments")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

applicationWithArguments :: Phantoms.TTerm Syntax.Application -> Phantoms.TTerm [Syntax.Expression] -> Phantoms.TTerm Syntax.Application
applicationWithArguments original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.lisp.syntax.Application"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "function"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.Application"),
              Core.projectionField = (Core.Name "function")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "arguments"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

lambda :: Phantoms.TTerm (Maybe Syntax.Symbol) -> Phantoms.TTerm [Syntax.Symbol] -> Phantoms.TTerm (Maybe Syntax.Symbol) -> Phantoms.TTerm [Syntax.Expression] -> Phantoms.TTerm Syntax.Lambda
lambda name params restParam body =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.lisp.syntax.Lambda"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Phantoms.unTTerm params)},
        Core.Field {
          Core.fieldName = (Core.Name "restParam"),
          Core.fieldTerm = (Phantoms.unTTerm restParam)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm body)}]}))

lambdaName :: Phantoms.TTerm Syntax.Lambda -> Phantoms.TTerm (Maybe Syntax.Symbol)
lambdaName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.Lambda"),
        Core.projectionField = (Core.Name "name")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

lambdaParams :: Phantoms.TTerm Syntax.Lambda -> Phantoms.TTerm [Syntax.Symbol]
lambdaParams x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.Lambda"),
        Core.projectionField = (Core.Name "params")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

lambdaRestParam :: Phantoms.TTerm Syntax.Lambda -> Phantoms.TTerm (Maybe Syntax.Symbol)
lambdaRestParam x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.Lambda"),
        Core.projectionField = (Core.Name "restParam")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

lambdaBody :: Phantoms.TTerm Syntax.Lambda -> Phantoms.TTerm [Syntax.Expression]
lambdaBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.Lambda"),
        Core.projectionField = (Core.Name "body")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

lambdaWithName :: Phantoms.TTerm Syntax.Lambda -> Phantoms.TTerm (Maybe Syntax.Symbol) -> Phantoms.TTerm Syntax.Lambda
lambdaWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.lisp.syntax.Lambda"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.Lambda"),
              Core.projectionField = (Core.Name "params")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "restParam"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.Lambda"),
              Core.projectionField = (Core.Name "restParam")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.Lambda"),
              Core.projectionField = (Core.Name "body")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

lambdaWithParams :: Phantoms.TTerm Syntax.Lambda -> Phantoms.TTerm [Syntax.Symbol] -> Phantoms.TTerm Syntax.Lambda
lambdaWithParams original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.lisp.syntax.Lambda"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.Lambda"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "restParam"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.Lambda"),
              Core.projectionField = (Core.Name "restParam")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.Lambda"),
              Core.projectionField = (Core.Name "body")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

lambdaWithRestParam :: Phantoms.TTerm Syntax.Lambda -> Phantoms.TTerm (Maybe Syntax.Symbol) -> Phantoms.TTerm Syntax.Lambda
lambdaWithRestParam original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.lisp.syntax.Lambda"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.Lambda"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.Lambda"),
              Core.projectionField = (Core.Name "params")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "restParam"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.Lambda"),
              Core.projectionField = (Core.Name "body")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

lambdaWithBody :: Phantoms.TTerm Syntax.Lambda -> Phantoms.TTerm [Syntax.Expression] -> Phantoms.TTerm Syntax.Lambda
lambdaWithBody original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.lisp.syntax.Lambda"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.Lambda"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.Lambda"),
              Core.projectionField = (Core.Name "params")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "restParam"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.Lambda"),
              Core.projectionField = (Core.Name "restParam")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

variableReference :: Phantoms.TTerm Syntax.Symbol -> Phantoms.TTerm Bool -> Phantoms.TTerm Syntax.VariableReference
variableReference name functionNamespace =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.lisp.syntax.VariableReference"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "functionNamespace"),
          Core.fieldTerm = (Phantoms.unTTerm functionNamespace)}]}))

variableReferenceName :: Phantoms.TTerm Syntax.VariableReference -> Phantoms.TTerm Syntax.Symbol
variableReferenceName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.VariableReference"),
        Core.projectionField = (Core.Name "name")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

variableReferenceFunctionNamespace :: Phantoms.TTerm Syntax.VariableReference -> Phantoms.TTerm Bool
variableReferenceFunctionNamespace x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.VariableReference"),
        Core.projectionField = (Core.Name "functionNamespace")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

variableReferenceWithName :: Phantoms.TTerm Syntax.VariableReference -> Phantoms.TTerm Syntax.Symbol -> Phantoms.TTerm Syntax.VariableReference
variableReferenceWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.lisp.syntax.VariableReference"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "functionNamespace"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.VariableReference"),
              Core.projectionField = (Core.Name "functionNamespace")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

variableReferenceWithFunctionNamespace :: Phantoms.TTerm Syntax.VariableReference -> Phantoms.TTerm Bool -> Phantoms.TTerm Syntax.VariableReference
variableReferenceWithFunctionNamespace original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.lisp.syntax.VariableReference"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.VariableReference"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "functionNamespace"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

fieldAccess :: Phantoms.TTerm Syntax.Symbol -> Phantoms.TTerm Syntax.Symbol -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.FieldAccess
fieldAccess recordType field target =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.lisp.syntax.FieldAccess"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "recordType"),
          Core.fieldTerm = (Phantoms.unTTerm recordType)},
        Core.Field {
          Core.fieldName = (Core.Name "field"),
          Core.fieldTerm = (Phantoms.unTTerm field)},
        Core.Field {
          Core.fieldName = (Core.Name "target"),
          Core.fieldTerm = (Phantoms.unTTerm target)}]}))

fieldAccessRecordType :: Phantoms.TTerm Syntax.FieldAccess -> Phantoms.TTerm Syntax.Symbol
fieldAccessRecordType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.FieldAccess"),
        Core.projectionField = (Core.Name "recordType")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

fieldAccessField :: Phantoms.TTerm Syntax.FieldAccess -> Phantoms.TTerm Syntax.Symbol
fieldAccessField x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.FieldAccess"),
        Core.projectionField = (Core.Name "field")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

fieldAccessTarget :: Phantoms.TTerm Syntax.FieldAccess -> Phantoms.TTerm Syntax.Expression
fieldAccessTarget x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.FieldAccess"),
        Core.projectionField = (Core.Name "target")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

fieldAccessWithRecordType :: Phantoms.TTerm Syntax.FieldAccess -> Phantoms.TTerm Syntax.Symbol -> Phantoms.TTerm Syntax.FieldAccess
fieldAccessWithRecordType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.lisp.syntax.FieldAccess"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "recordType"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "field"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.FieldAccess"),
              Core.projectionField = (Core.Name "field")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "target"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.FieldAccess"),
              Core.projectionField = (Core.Name "target")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

fieldAccessWithField :: Phantoms.TTerm Syntax.FieldAccess -> Phantoms.TTerm Syntax.Symbol -> Phantoms.TTerm Syntax.FieldAccess
fieldAccessWithField original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.lisp.syntax.FieldAccess"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "recordType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.FieldAccess"),
              Core.projectionField = (Core.Name "recordType")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "field"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "target"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.FieldAccess"),
              Core.projectionField = (Core.Name "target")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

fieldAccessWithTarget :: Phantoms.TTerm Syntax.FieldAccess -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.FieldAccess
fieldAccessWithTarget original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.lisp.syntax.FieldAccess"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "recordType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.FieldAccess"),
              Core.projectionField = (Core.Name "recordType")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "field"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.FieldAccess"),
              Core.projectionField = (Core.Name "field")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "target"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

typeAnnotation :: Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.TypeSpecifier -> Phantoms.TTerm Syntax.TypeAnnotation
typeAnnotation expression type_ =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.lisp.syntax.TypeAnnotation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTTerm expression)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm type_)}]}))

typeAnnotationExpression :: Phantoms.TTerm Syntax.TypeAnnotation -> Phantoms.TTerm Syntax.Expression
typeAnnotationExpression x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.TypeAnnotation"),
        Core.projectionField = (Core.Name "expression")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

typeAnnotationType :: Phantoms.TTerm Syntax.TypeAnnotation -> Phantoms.TTerm Syntax.TypeSpecifier
typeAnnotationType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.TypeAnnotation"),
        Core.projectionField = (Core.Name "type")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

typeAnnotationWithExpression :: Phantoms.TTerm Syntax.TypeAnnotation -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.TypeAnnotation
typeAnnotationWithExpression original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.lisp.syntax.TypeAnnotation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.TypeAnnotation"),
              Core.projectionField = (Core.Name "type")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

typeAnnotationWithType :: Phantoms.TTerm Syntax.TypeAnnotation -> Phantoms.TTerm Syntax.TypeSpecifier -> Phantoms.TTerm Syntax.TypeAnnotation
typeAnnotationWithType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.lisp.syntax.TypeAnnotation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.TypeAnnotation"),
              Core.projectionField = (Core.Name "expression")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

ifExpression :: Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm (Maybe Syntax.Expression) -> Phantoms.TTerm Syntax.IfExpression
ifExpression condition then_ else_ =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.lisp.syntax.IfExpression"),
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

ifExpressionCondition :: Phantoms.TTerm Syntax.IfExpression -> Phantoms.TTerm Syntax.Expression
ifExpressionCondition x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.IfExpression"),
        Core.projectionField = (Core.Name "condition")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

ifExpressionThen :: Phantoms.TTerm Syntax.IfExpression -> Phantoms.TTerm Syntax.Expression
ifExpressionThen x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.IfExpression"),
        Core.projectionField = (Core.Name "then")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

ifExpressionElse :: Phantoms.TTerm Syntax.IfExpression -> Phantoms.TTerm (Maybe Syntax.Expression)
ifExpressionElse x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.IfExpression"),
        Core.projectionField = (Core.Name "else")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

ifExpressionWithCondition :: Phantoms.TTerm Syntax.IfExpression -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.IfExpression
ifExpressionWithCondition original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.lisp.syntax.IfExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "condition"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "then"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.IfExpression"),
              Core.projectionField = (Core.Name "then")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "else"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.IfExpression"),
              Core.projectionField = (Core.Name "else")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

ifExpressionWithThen :: Phantoms.TTerm Syntax.IfExpression -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.IfExpression
ifExpressionWithThen original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.lisp.syntax.IfExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "condition"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.IfExpression"),
              Core.projectionField = (Core.Name "condition")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "then"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "else"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.IfExpression"),
              Core.projectionField = (Core.Name "else")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

ifExpressionWithElse :: Phantoms.TTerm Syntax.IfExpression -> Phantoms.TTerm (Maybe Syntax.Expression) -> Phantoms.TTerm Syntax.IfExpression
ifExpressionWithElse original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.lisp.syntax.IfExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "condition"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.IfExpression"),
              Core.projectionField = (Core.Name "condition")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "then"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.IfExpression"),
              Core.projectionField = (Core.Name "then")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "else"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

condExpression :: Phantoms.TTerm [Syntax.CondClause] -> Phantoms.TTerm (Maybe Syntax.Expression) -> Phantoms.TTerm Syntax.CondExpression
condExpression clauses default_ =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.lisp.syntax.CondExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "clauses"),
          Core.fieldTerm = (Phantoms.unTTerm clauses)},
        Core.Field {
          Core.fieldName = (Core.Name "default"),
          Core.fieldTerm = (Phantoms.unTTerm default_)}]}))

condExpressionClauses :: Phantoms.TTerm Syntax.CondExpression -> Phantoms.TTerm [Syntax.CondClause]
condExpressionClauses x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.CondExpression"),
        Core.projectionField = (Core.Name "clauses")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

condExpressionDefault :: Phantoms.TTerm Syntax.CondExpression -> Phantoms.TTerm (Maybe Syntax.Expression)
condExpressionDefault x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.CondExpression"),
        Core.projectionField = (Core.Name "default")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

condExpressionWithClauses :: Phantoms.TTerm Syntax.CondExpression -> Phantoms.TTerm [Syntax.CondClause] -> Phantoms.TTerm Syntax.CondExpression
condExpressionWithClauses original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.lisp.syntax.CondExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "clauses"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "default"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.CondExpression"),
              Core.projectionField = (Core.Name "default")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

condExpressionWithDefault :: Phantoms.TTerm Syntax.CondExpression -> Phantoms.TTerm (Maybe Syntax.Expression) -> Phantoms.TTerm Syntax.CondExpression
condExpressionWithDefault original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.lisp.syntax.CondExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "clauses"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.CondExpression"),
              Core.projectionField = (Core.Name "clauses")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "default"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

condClause :: Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.CondClause
condClause condition body =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.lisp.syntax.CondClause"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "condition"),
          Core.fieldTerm = (Phantoms.unTTerm condition)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm body)}]}))

condClauseCondition :: Phantoms.TTerm Syntax.CondClause -> Phantoms.TTerm Syntax.Expression
condClauseCondition x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.CondClause"),
        Core.projectionField = (Core.Name "condition")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

condClauseBody :: Phantoms.TTerm Syntax.CondClause -> Phantoms.TTerm Syntax.Expression
condClauseBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.CondClause"),
        Core.projectionField = (Core.Name "body")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

condClauseWithCondition :: Phantoms.TTerm Syntax.CondClause -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.CondClause
condClauseWithCondition original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.lisp.syntax.CondClause"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "condition"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.CondClause"),
              Core.projectionField = (Core.Name "body")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

condClauseWithBody :: Phantoms.TTerm Syntax.CondClause -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.CondClause
condClauseWithBody original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.lisp.syntax.CondClause"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "condition"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.CondClause"),
              Core.projectionField = (Core.Name "condition")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

caseExpression :: Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm [Syntax.CaseClause] -> Phantoms.TTerm (Maybe Syntax.Expression) -> Phantoms.TTerm Syntax.CaseExpression
caseExpression scrutinee clauses default_ =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.lisp.syntax.CaseExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "scrutinee"),
          Core.fieldTerm = (Phantoms.unTTerm scrutinee)},
        Core.Field {
          Core.fieldName = (Core.Name "clauses"),
          Core.fieldTerm = (Phantoms.unTTerm clauses)},
        Core.Field {
          Core.fieldName = (Core.Name "default"),
          Core.fieldTerm = (Phantoms.unTTerm default_)}]}))

caseExpressionScrutinee :: Phantoms.TTerm Syntax.CaseExpression -> Phantoms.TTerm Syntax.Expression
caseExpressionScrutinee x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.CaseExpression"),
        Core.projectionField = (Core.Name "scrutinee")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

caseExpressionClauses :: Phantoms.TTerm Syntax.CaseExpression -> Phantoms.TTerm [Syntax.CaseClause]
caseExpressionClauses x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.CaseExpression"),
        Core.projectionField = (Core.Name "clauses")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

caseExpressionDefault :: Phantoms.TTerm Syntax.CaseExpression -> Phantoms.TTerm (Maybe Syntax.Expression)
caseExpressionDefault x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.CaseExpression"),
        Core.projectionField = (Core.Name "default")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

caseExpressionWithScrutinee :: Phantoms.TTerm Syntax.CaseExpression -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.CaseExpression
caseExpressionWithScrutinee original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.lisp.syntax.CaseExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "scrutinee"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "clauses"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.CaseExpression"),
              Core.projectionField = (Core.Name "clauses")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "default"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.CaseExpression"),
              Core.projectionField = (Core.Name "default")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

caseExpressionWithClauses :: Phantoms.TTerm Syntax.CaseExpression -> Phantoms.TTerm [Syntax.CaseClause] -> Phantoms.TTerm Syntax.CaseExpression
caseExpressionWithClauses original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.lisp.syntax.CaseExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "scrutinee"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.CaseExpression"),
              Core.projectionField = (Core.Name "scrutinee")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "clauses"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "default"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.CaseExpression"),
              Core.projectionField = (Core.Name "default")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

caseExpressionWithDefault :: Phantoms.TTerm Syntax.CaseExpression -> Phantoms.TTerm (Maybe Syntax.Expression) -> Phantoms.TTerm Syntax.CaseExpression
caseExpressionWithDefault original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.lisp.syntax.CaseExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "scrutinee"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.CaseExpression"),
              Core.projectionField = (Core.Name "scrutinee")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "clauses"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.CaseExpression"),
              Core.projectionField = (Core.Name "clauses")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "default"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

caseClause :: Phantoms.TTerm [Syntax.Expression] -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.CaseClause
caseClause keys body =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.lisp.syntax.CaseClause"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "keys"),
          Core.fieldTerm = (Phantoms.unTTerm keys)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm body)}]}))

caseClauseKeys :: Phantoms.TTerm Syntax.CaseClause -> Phantoms.TTerm [Syntax.Expression]
caseClauseKeys x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.CaseClause"),
        Core.projectionField = (Core.Name "keys")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

caseClauseBody :: Phantoms.TTerm Syntax.CaseClause -> Phantoms.TTerm Syntax.Expression
caseClauseBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.CaseClause"),
        Core.projectionField = (Core.Name "body")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

caseClauseWithKeys :: Phantoms.TTerm Syntax.CaseClause -> Phantoms.TTerm [Syntax.Expression] -> Phantoms.TTerm Syntax.CaseClause
caseClauseWithKeys original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.lisp.syntax.CaseClause"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "keys"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.CaseClause"),
              Core.projectionField = (Core.Name "body")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

caseClauseWithBody :: Phantoms.TTerm Syntax.CaseClause -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.CaseClause
caseClauseWithBody original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.lisp.syntax.CaseClause"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "keys"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.CaseClause"),
              Core.projectionField = (Core.Name "keys")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

andExpression :: Phantoms.TTerm [Syntax.Expression] -> Phantoms.TTerm Syntax.AndExpression
andExpression expressions =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.lisp.syntax.AndExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expressions"),
          Core.fieldTerm = (Phantoms.unTTerm expressions)}]}))

andExpressionExpressions :: Phantoms.TTerm Syntax.AndExpression -> Phantoms.TTerm [Syntax.Expression]
andExpressionExpressions x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.AndExpression"),
        Core.projectionField = (Core.Name "expressions")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

andExpressionWithExpressions :: Phantoms.TTerm Syntax.AndExpression -> Phantoms.TTerm [Syntax.Expression] -> Phantoms.TTerm Syntax.AndExpression
andExpressionWithExpressions original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.lisp.syntax.AndExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expressions"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

orExpression :: Phantoms.TTerm [Syntax.Expression] -> Phantoms.TTerm Syntax.OrExpression
orExpression expressions =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.lisp.syntax.OrExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expressions"),
          Core.fieldTerm = (Phantoms.unTTerm expressions)}]}))

orExpressionExpressions :: Phantoms.TTerm Syntax.OrExpression -> Phantoms.TTerm [Syntax.Expression]
orExpressionExpressions x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.OrExpression"),
        Core.projectionField = (Core.Name "expressions")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

orExpressionWithExpressions :: Phantoms.TTerm Syntax.OrExpression -> Phantoms.TTerm [Syntax.Expression] -> Phantoms.TTerm Syntax.OrExpression
orExpressionWithExpressions original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.lisp.syntax.OrExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expressions"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

notExpression :: Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.NotExpression
notExpression expression =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.lisp.syntax.NotExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTTerm expression)}]}))

notExpressionExpression :: Phantoms.TTerm Syntax.NotExpression -> Phantoms.TTerm Syntax.Expression
notExpressionExpression x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.NotExpression"),
        Core.projectionField = (Core.Name "expression")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

notExpressionWithExpression :: Phantoms.TTerm Syntax.NotExpression -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.NotExpression
notExpressionWithExpression original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.lisp.syntax.NotExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

doExpression :: Phantoms.TTerm [Syntax.Expression] -> Phantoms.TTerm Syntax.DoExpression
doExpression expressions =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.lisp.syntax.DoExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expressions"),
          Core.fieldTerm = (Phantoms.unTTerm expressions)}]}))

doExpressionExpressions :: Phantoms.TTerm Syntax.DoExpression -> Phantoms.TTerm [Syntax.Expression]
doExpressionExpressions x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.DoExpression"),
        Core.projectionField = (Core.Name "expressions")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

doExpressionWithExpressions :: Phantoms.TTerm Syntax.DoExpression -> Phantoms.TTerm [Syntax.Expression] -> Phantoms.TTerm Syntax.DoExpression
doExpressionWithExpressions original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.lisp.syntax.DoExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expressions"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

beginExpression :: Phantoms.TTerm [Syntax.Expression] -> Phantoms.TTerm Syntax.BeginExpression
beginExpression expressions =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.lisp.syntax.BeginExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expressions"),
          Core.fieldTerm = (Phantoms.unTTerm expressions)}]}))

beginExpressionExpressions :: Phantoms.TTerm Syntax.BeginExpression -> Phantoms.TTerm [Syntax.Expression]
beginExpressionExpressions x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.BeginExpression"),
        Core.projectionField = (Core.Name "expressions")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

beginExpressionWithExpressions :: Phantoms.TTerm Syntax.BeginExpression -> Phantoms.TTerm [Syntax.Expression] -> Phantoms.TTerm Syntax.BeginExpression
beginExpressionWithExpressions original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.lisp.syntax.BeginExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expressions"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

quoteExpression :: Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.QuoteExpression
quoteExpression body =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.lisp.syntax.QuoteExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm body)}]}))

quoteExpressionBody :: Phantoms.TTerm Syntax.QuoteExpression -> Phantoms.TTerm Syntax.Expression
quoteExpressionBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.QuoteExpression"),
        Core.projectionField = (Core.Name "body")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

quoteExpressionWithBody :: Phantoms.TTerm Syntax.QuoteExpression -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.QuoteExpression
quoteExpressionWithBody original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.lisp.syntax.QuoteExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

quasiquoteExpression :: Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.QuasiquoteExpression
quasiquoteExpression body =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.lisp.syntax.QuasiquoteExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm body)}]}))

quasiquoteExpressionBody :: Phantoms.TTerm Syntax.QuasiquoteExpression -> Phantoms.TTerm Syntax.Expression
quasiquoteExpressionBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.QuasiquoteExpression"),
        Core.projectionField = (Core.Name "body")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

quasiquoteExpressionWithBody :: Phantoms.TTerm Syntax.QuasiquoteExpression -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.QuasiquoteExpression
quasiquoteExpressionWithBody original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.lisp.syntax.QuasiquoteExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

unquoteExpression :: Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.UnquoteExpression
unquoteExpression body =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.lisp.syntax.UnquoteExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm body)}]}))

unquoteExpressionBody :: Phantoms.TTerm Syntax.UnquoteExpression -> Phantoms.TTerm Syntax.Expression
unquoteExpressionBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.UnquoteExpression"),
        Core.projectionField = (Core.Name "body")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unquoteExpressionWithBody :: Phantoms.TTerm Syntax.UnquoteExpression -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.UnquoteExpression
unquoteExpressionWithBody original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.lisp.syntax.UnquoteExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

splicingUnquoteExpression :: Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.SplicingUnquoteExpression
splicingUnquoteExpression body =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.lisp.syntax.SplicingUnquoteExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm body)}]}))

splicingUnquoteExpressionBody :: Phantoms.TTerm Syntax.SplicingUnquoteExpression -> Phantoms.TTerm Syntax.Expression
splicingUnquoteExpressionBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.SplicingUnquoteExpression"),
        Core.projectionField = (Core.Name "body")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

splicingUnquoteExpressionWithBody :: Phantoms.TTerm Syntax.SplicingUnquoteExpression -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.SplicingUnquoteExpression
splicingUnquoteExpressionWithBody original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.lisp.syntax.SplicingUnquoteExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

letExpression :: Phantoms.TTerm Syntax.LetKind -> Phantoms.TTerm [Syntax.LetBinding] -> Phantoms.TTerm [Syntax.Expression] -> Phantoms.TTerm Syntax.LetExpression
letExpression kind bindings body =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.lisp.syntax.LetExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "kind"),
          Core.fieldTerm = (Phantoms.unTTerm kind)},
        Core.Field {
          Core.fieldName = (Core.Name "bindings"),
          Core.fieldTerm = (Phantoms.unTTerm bindings)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm body)}]}))

letExpressionKind :: Phantoms.TTerm Syntax.LetExpression -> Phantoms.TTerm Syntax.LetKind
letExpressionKind x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.LetExpression"),
        Core.projectionField = (Core.Name "kind")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

letExpressionBindings :: Phantoms.TTerm Syntax.LetExpression -> Phantoms.TTerm [Syntax.LetBinding]
letExpressionBindings x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.LetExpression"),
        Core.projectionField = (Core.Name "bindings")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

letExpressionBody :: Phantoms.TTerm Syntax.LetExpression -> Phantoms.TTerm [Syntax.Expression]
letExpressionBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.LetExpression"),
        Core.projectionField = (Core.Name "body")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

letExpressionWithKind :: Phantoms.TTerm Syntax.LetExpression -> Phantoms.TTerm Syntax.LetKind -> Phantoms.TTerm Syntax.LetExpression
letExpressionWithKind original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.lisp.syntax.LetExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "kind"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "bindings"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.LetExpression"),
              Core.projectionField = (Core.Name "bindings")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.LetExpression"),
              Core.projectionField = (Core.Name "body")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

letExpressionWithBindings :: Phantoms.TTerm Syntax.LetExpression -> Phantoms.TTerm [Syntax.LetBinding] -> Phantoms.TTerm Syntax.LetExpression
letExpressionWithBindings original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.lisp.syntax.LetExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "kind"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.LetExpression"),
              Core.projectionField = (Core.Name "kind")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "bindings"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.LetExpression"),
              Core.projectionField = (Core.Name "body")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

letExpressionWithBody :: Phantoms.TTerm Syntax.LetExpression -> Phantoms.TTerm [Syntax.Expression] -> Phantoms.TTerm Syntax.LetExpression
letExpressionWithBody original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.lisp.syntax.LetExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "kind"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.LetExpression"),
              Core.projectionField = (Core.Name "kind")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "bindings"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.LetExpression"),
              Core.projectionField = (Core.Name "bindings")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

letKindParallel :: Phantoms.TTerm Syntax.LetKind
letKindParallel =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.lisp.syntax.LetKind"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "parallel"),
        Core.fieldTerm = Core.TermUnit}}))

letKindSequential :: Phantoms.TTerm Syntax.LetKind
letKindSequential =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.lisp.syntax.LetKind"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "sequential"),
        Core.fieldTerm = Core.TermUnit}}))

letKindRecursive :: Phantoms.TTerm Syntax.LetKind
letKindRecursive =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.lisp.syntax.LetKind"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "recursive"),
        Core.fieldTerm = Core.TermUnit}}))

letBindingSimple :: Phantoms.TTerm Syntax.SimpleBinding -> Phantoms.TTerm Syntax.LetBinding
letBindingSimple x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.lisp.syntax.LetBinding"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "simple"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

letBindingDestructuring :: Phantoms.TTerm Syntax.DestructuringBinding -> Phantoms.TTerm Syntax.LetBinding
letBindingDestructuring x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.lisp.syntax.LetBinding"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "destructuring"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

simpleBinding :: Phantoms.TTerm Syntax.Symbol -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.SimpleBinding
simpleBinding name value =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.lisp.syntax.SimpleBinding"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm value)}]}))

simpleBindingName :: Phantoms.TTerm Syntax.SimpleBinding -> Phantoms.TTerm Syntax.Symbol
simpleBindingName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.SimpleBinding"),
        Core.projectionField = (Core.Name "name")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

simpleBindingValue :: Phantoms.TTerm Syntax.SimpleBinding -> Phantoms.TTerm Syntax.Expression
simpleBindingValue x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.SimpleBinding"),
        Core.projectionField = (Core.Name "value")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

simpleBindingWithName :: Phantoms.TTerm Syntax.SimpleBinding -> Phantoms.TTerm Syntax.Symbol -> Phantoms.TTerm Syntax.SimpleBinding
simpleBindingWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.lisp.syntax.SimpleBinding"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.SimpleBinding"),
              Core.projectionField = (Core.Name "value")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

simpleBindingWithValue :: Phantoms.TTerm Syntax.SimpleBinding -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.SimpleBinding
simpleBindingWithValue original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.lisp.syntax.SimpleBinding"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.SimpleBinding"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

destructuringBinding :: Phantoms.TTerm Syntax.DestructuringPattern -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.DestructuringBinding
destructuringBinding pattern value =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.lisp.syntax.DestructuringBinding"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Phantoms.unTTerm pattern)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm value)}]}))

destructuringBindingPattern :: Phantoms.TTerm Syntax.DestructuringBinding -> Phantoms.TTerm Syntax.DestructuringPattern
destructuringBindingPattern x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.DestructuringBinding"),
        Core.projectionField = (Core.Name "pattern")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

destructuringBindingValue :: Phantoms.TTerm Syntax.DestructuringBinding -> Phantoms.TTerm Syntax.Expression
destructuringBindingValue x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.DestructuringBinding"),
        Core.projectionField = (Core.Name "value")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

destructuringBindingWithPattern :: Phantoms.TTerm Syntax.DestructuringBinding -> Phantoms.TTerm Syntax.DestructuringPattern -> Phantoms.TTerm Syntax.DestructuringBinding
destructuringBindingWithPattern original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.lisp.syntax.DestructuringBinding"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.DestructuringBinding"),
              Core.projectionField = (Core.Name "value")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

destructuringBindingWithValue :: Phantoms.TTerm Syntax.DestructuringBinding -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.DestructuringBinding
destructuringBindingWithValue original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.lisp.syntax.DestructuringBinding"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.DestructuringBinding"),
              Core.projectionField = (Core.Name "pattern")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

destructuringPatternSequential :: Phantoms.TTerm [Syntax.Symbol] -> Phantoms.TTerm Syntax.DestructuringPattern
destructuringPatternSequential x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.lisp.syntax.DestructuringPattern"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "sequential"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

destructuringPatternAssociative :: Phantoms.TTerm [Syntax.Symbol] -> Phantoms.TTerm Syntax.DestructuringPattern
destructuringPatternAssociative x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.lisp.syntax.DestructuringPattern"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "associative"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

destructuringPatternRest :: Phantoms.TTerm [Syntax.Symbol] -> Phantoms.TTerm Syntax.DestructuringPattern
destructuringPatternRest x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.lisp.syntax.DestructuringPattern"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "rest"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

patternConstructor :: Phantoms.TTerm Syntax.ConstructorPattern -> Phantoms.TTerm Syntax.Pattern
patternConstructor x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.lisp.syntax.Pattern"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "constructor"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

patternLiteral :: Phantoms.TTerm Syntax.LiteralPattern -> Phantoms.TTerm Syntax.Pattern
patternLiteral x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.lisp.syntax.Pattern"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "literal"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

patternWildcard :: Phantoms.TTerm Syntax.WildcardPattern -> Phantoms.TTerm Syntax.Pattern
patternWildcard x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.lisp.syntax.Pattern"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "wildcard"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

patternVariable :: Phantoms.TTerm Syntax.Symbol -> Phantoms.TTerm Syntax.Pattern
patternVariable x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.lisp.syntax.Pattern"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "variable"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

constructorPattern :: Phantoms.TTerm Syntax.Symbol -> Phantoms.TTerm [Syntax.Pattern] -> Phantoms.TTerm Syntax.ConstructorPattern
constructorPattern constructor arguments =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.lisp.syntax.ConstructorPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "constructor"),
          Core.fieldTerm = (Phantoms.unTTerm constructor)},
        Core.Field {
          Core.fieldName = (Core.Name "arguments"),
          Core.fieldTerm = (Phantoms.unTTerm arguments)}]}))

constructorPatternConstructor :: Phantoms.TTerm Syntax.ConstructorPattern -> Phantoms.TTerm Syntax.Symbol
constructorPatternConstructor x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.ConstructorPattern"),
        Core.projectionField = (Core.Name "constructor")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

constructorPatternArguments :: Phantoms.TTerm Syntax.ConstructorPattern -> Phantoms.TTerm [Syntax.Pattern]
constructorPatternArguments x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.ConstructorPattern"),
        Core.projectionField = (Core.Name "arguments")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

constructorPatternWithConstructor :: Phantoms.TTerm Syntax.ConstructorPattern -> Phantoms.TTerm Syntax.Symbol -> Phantoms.TTerm Syntax.ConstructorPattern
constructorPatternWithConstructor original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.lisp.syntax.ConstructorPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "constructor"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "arguments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.ConstructorPattern"),
              Core.projectionField = (Core.Name "arguments")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

constructorPatternWithArguments :: Phantoms.TTerm Syntax.ConstructorPattern -> Phantoms.TTerm [Syntax.Pattern] -> Phantoms.TTerm Syntax.ConstructorPattern
constructorPatternWithArguments original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.lisp.syntax.ConstructorPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "constructor"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.ConstructorPattern"),
              Core.projectionField = (Core.Name "constructor")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "arguments"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

literalPattern :: Phantoms.TTerm Syntax.Literal -> Phantoms.TTerm Syntax.LiteralPattern
literalPattern value =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.lisp.syntax.LiteralPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm value)}]}))

literalPatternValue :: Phantoms.TTerm Syntax.LiteralPattern -> Phantoms.TTerm Syntax.Literal
literalPatternValue x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.LiteralPattern"),
        Core.projectionField = (Core.Name "value")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

literalPatternWithValue :: Phantoms.TTerm Syntax.LiteralPattern -> Phantoms.TTerm Syntax.Literal -> Phantoms.TTerm Syntax.LiteralPattern
literalPatternWithValue original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.lisp.syntax.LiteralPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

wildcardPattern :: Phantoms.TTerm Syntax.WildcardPattern
wildcardPattern =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.lisp.syntax.WildcardPattern"),
      Core.recordFields = []}))

literalInteger :: Phantoms.TTerm Syntax.IntegerLiteral -> Phantoms.TTerm Syntax.Literal
literalInteger x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.lisp.syntax.Literal"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "integer"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

literalFloat :: Phantoms.TTerm Syntax.FloatLiteral -> Phantoms.TTerm Syntax.Literal
literalFloat x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.lisp.syntax.Literal"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "float"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

literalString :: Phantoms.TTerm String -> Phantoms.TTerm Syntax.Literal
literalString x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.lisp.syntax.Literal"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "string"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

literalCharacter :: Phantoms.TTerm Syntax.CharacterLiteral -> Phantoms.TTerm Syntax.Literal
literalCharacter x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.lisp.syntax.Literal"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "character"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

literalBoolean :: Phantoms.TTerm Bool -> Phantoms.TTerm Syntax.Literal
literalBoolean x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.lisp.syntax.Literal"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "boolean"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

literalNil :: Phantoms.TTerm Syntax.Literal
literalNil =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.lisp.syntax.Literal"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "nil"),
        Core.fieldTerm = Core.TermUnit}}))

literalKeyword :: Phantoms.TTerm Syntax.Keyword -> Phantoms.TTerm Syntax.Literal
literalKeyword x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.lisp.syntax.Literal"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "keyword"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

literalSymbol :: Phantoms.TTerm Syntax.Symbol -> Phantoms.TTerm Syntax.Literal
literalSymbol x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.lisp.syntax.Literal"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "symbol"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

integerLiteral :: Phantoms.TTerm Integer -> Phantoms.TTerm Bool -> Phantoms.TTerm Syntax.IntegerLiteral
integerLiteral value bigint =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.lisp.syntax.IntegerLiteral"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm value)},
        Core.Field {
          Core.fieldName = (Core.Name "bigint"),
          Core.fieldTerm = (Phantoms.unTTerm bigint)}]}))

integerLiteralValue :: Phantoms.TTerm Syntax.IntegerLiteral -> Phantoms.TTerm Integer
integerLiteralValue x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.IntegerLiteral"),
        Core.projectionField = (Core.Name "value")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

integerLiteralBigint :: Phantoms.TTerm Syntax.IntegerLiteral -> Phantoms.TTerm Bool
integerLiteralBigint x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.IntegerLiteral"),
        Core.projectionField = (Core.Name "bigint")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

integerLiteralWithValue :: Phantoms.TTerm Syntax.IntegerLiteral -> Phantoms.TTerm Integer -> Phantoms.TTerm Syntax.IntegerLiteral
integerLiteralWithValue original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.lisp.syntax.IntegerLiteral"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "bigint"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.IntegerLiteral"),
              Core.projectionField = (Core.Name "bigint")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

integerLiteralWithBigint :: Phantoms.TTerm Syntax.IntegerLiteral -> Phantoms.TTerm Bool -> Phantoms.TTerm Syntax.IntegerLiteral
integerLiteralWithBigint original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.lisp.syntax.IntegerLiteral"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.IntegerLiteral"),
              Core.projectionField = (Core.Name "value")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "bigint"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

floatLiteral :: Phantoms.TTerm Double -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Syntax.FloatLiteral
floatLiteral value precision =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.lisp.syntax.FloatLiteral"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm value)},
        Core.Field {
          Core.fieldName = (Core.Name "precision"),
          Core.fieldTerm = (Phantoms.unTTerm precision)}]}))

floatLiteralValue :: Phantoms.TTerm Syntax.FloatLiteral -> Phantoms.TTerm Double
floatLiteralValue x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.FloatLiteral"),
        Core.projectionField = (Core.Name "value")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

floatLiteralPrecision :: Phantoms.TTerm Syntax.FloatLiteral -> Phantoms.TTerm (Maybe String)
floatLiteralPrecision x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.FloatLiteral"),
        Core.projectionField = (Core.Name "precision")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

floatLiteralWithValue :: Phantoms.TTerm Syntax.FloatLiteral -> Phantoms.TTerm Double -> Phantoms.TTerm Syntax.FloatLiteral
floatLiteralWithValue original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.lisp.syntax.FloatLiteral"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "precision"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.FloatLiteral"),
              Core.projectionField = (Core.Name "precision")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

floatLiteralWithPrecision :: Phantoms.TTerm Syntax.FloatLiteral -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Syntax.FloatLiteral
floatLiteralWithPrecision original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.lisp.syntax.FloatLiteral"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.FloatLiteral"),
              Core.projectionField = (Core.Name "value")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "precision"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

characterLiteral :: Phantoms.TTerm String -> Phantoms.TTerm Syntax.CharacterLiteral
characterLiteral value =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.lisp.syntax.CharacterLiteral"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm value)}]}))

characterLiteralValue :: Phantoms.TTerm Syntax.CharacterLiteral -> Phantoms.TTerm String
characterLiteralValue x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.CharacterLiteral"),
        Core.projectionField = (Core.Name "value")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

characterLiteralWithValue :: Phantoms.TTerm Syntax.CharacterLiteral -> Phantoms.TTerm String -> Phantoms.TTerm Syntax.CharacterLiteral
characterLiteralWithValue original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.lisp.syntax.CharacterLiteral"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

booleanStyleTrueFalse :: Phantoms.TTerm Syntax.BooleanStyle
booleanStyleTrueFalse =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.lisp.syntax.BooleanStyle"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "trueFalse"),
        Core.fieldTerm = Core.TermUnit}}))

booleanStyleTNil :: Phantoms.TTerm Syntax.BooleanStyle
booleanStyleTNil =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.lisp.syntax.BooleanStyle"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "tNil"),
        Core.fieldTerm = Core.TermUnit}}))

booleanStyleHashTF :: Phantoms.TTerm Syntax.BooleanStyle
booleanStyleHashTF =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.lisp.syntax.BooleanStyle"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "hashTF"),
        Core.fieldTerm = Core.TermUnit}}))

nilStyleNil :: Phantoms.TTerm Syntax.NilStyle
nilStyleNil =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.lisp.syntax.NilStyle"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "nil"),
        Core.fieldTerm = Core.TermUnit}}))

nilStyleEmptyList :: Phantoms.TTerm Syntax.NilStyle
nilStyleEmptyList =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.lisp.syntax.NilStyle"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "emptyList"),
        Core.fieldTerm = Core.TermUnit}}))

symbol :: Phantoms.TTerm String -> Phantoms.TTerm Syntax.Symbol
symbol x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.lisp.syntax.Symbol"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unSymbol :: Phantoms.TTerm Syntax.Symbol -> Phantoms.TTerm String
unSymbol x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.lisp.syntax.Symbol")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

keyword :: Phantoms.TTerm String -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Syntax.Keyword
keyword name namespace =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.lisp.syntax.Keyword"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "namespace"),
          Core.fieldTerm = (Phantoms.unTTerm namespace)}]}))

keywordName :: Phantoms.TTerm Syntax.Keyword -> Phantoms.TTerm String
keywordName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.Keyword"),
        Core.projectionField = (Core.Name "name")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

keywordNamespace :: Phantoms.TTerm Syntax.Keyword -> Phantoms.TTerm (Maybe String)
keywordNamespace x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.Keyword"),
        Core.projectionField = (Core.Name "namespace")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

keywordWithName :: Phantoms.TTerm Syntax.Keyword -> Phantoms.TTerm String -> Phantoms.TTerm Syntax.Keyword
keywordWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.lisp.syntax.Keyword"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "namespace"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.Keyword"),
              Core.projectionField = (Core.Name "namespace")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

keywordWithNamespace :: Phantoms.TTerm Syntax.Keyword -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Syntax.Keyword
keywordWithNamespace original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.lisp.syntax.Keyword"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.Keyword"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "namespace"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

qualifiedSymbol :: Phantoms.TTerm String -> Phantoms.TTerm String -> Phantoms.TTerm Syntax.QualifiedSymbol
qualifiedSymbol namespace name =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.lisp.syntax.QualifiedSymbol"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "namespace"),
          Core.fieldTerm = (Phantoms.unTTerm namespace)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)}]}))

qualifiedSymbolNamespace :: Phantoms.TTerm Syntax.QualifiedSymbol -> Phantoms.TTerm String
qualifiedSymbolNamespace x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.QualifiedSymbol"),
        Core.projectionField = (Core.Name "namespace")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

qualifiedSymbolName :: Phantoms.TTerm Syntax.QualifiedSymbol -> Phantoms.TTerm String
qualifiedSymbolName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.QualifiedSymbol"),
        Core.projectionField = (Core.Name "name")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

qualifiedSymbolWithNamespace :: Phantoms.TTerm Syntax.QualifiedSymbol -> Phantoms.TTerm String -> Phantoms.TTerm Syntax.QualifiedSymbol
qualifiedSymbolWithNamespace original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.lisp.syntax.QualifiedSymbol"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "namespace"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.QualifiedSymbol"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

qualifiedSymbolWithName :: Phantoms.TTerm Syntax.QualifiedSymbol -> Phantoms.TTerm String -> Phantoms.TTerm Syntax.QualifiedSymbol
qualifiedSymbolWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.lisp.syntax.QualifiedSymbol"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "namespace"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.QualifiedSymbol"),
              Core.projectionField = (Core.Name "namespace")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

namespaceName :: Phantoms.TTerm String -> Phantoms.TTerm Syntax.NamespaceName
namespaceName x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.lisp.syntax.NamespaceName"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unNamespaceName :: Phantoms.TTerm Syntax.NamespaceName -> Phantoms.TTerm String
unNamespaceName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.lisp.syntax.NamespaceName")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

listLiteral :: Phantoms.TTerm [Syntax.Expression] -> Phantoms.TTerm Bool -> Phantoms.TTerm Syntax.ListLiteral
listLiteral elements quoted =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.lisp.syntax.ListLiteral"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "elements"),
          Core.fieldTerm = (Phantoms.unTTerm elements)},
        Core.Field {
          Core.fieldName = (Core.Name "quoted"),
          Core.fieldTerm = (Phantoms.unTTerm quoted)}]}))

listLiteralElements :: Phantoms.TTerm Syntax.ListLiteral -> Phantoms.TTerm [Syntax.Expression]
listLiteralElements x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.ListLiteral"),
        Core.projectionField = (Core.Name "elements")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

listLiteralQuoted :: Phantoms.TTerm Syntax.ListLiteral -> Phantoms.TTerm Bool
listLiteralQuoted x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.ListLiteral"),
        Core.projectionField = (Core.Name "quoted")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

listLiteralWithElements :: Phantoms.TTerm Syntax.ListLiteral -> Phantoms.TTerm [Syntax.Expression] -> Phantoms.TTerm Syntax.ListLiteral
listLiteralWithElements original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.lisp.syntax.ListLiteral"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "elements"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "quoted"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.ListLiteral"),
              Core.projectionField = (Core.Name "quoted")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

listLiteralWithQuoted :: Phantoms.TTerm Syntax.ListLiteral -> Phantoms.TTerm Bool -> Phantoms.TTerm Syntax.ListLiteral
listLiteralWithQuoted original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.lisp.syntax.ListLiteral"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "elements"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.ListLiteral"),
              Core.projectionField = (Core.Name "elements")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "quoted"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

vectorLiteral :: Phantoms.TTerm [Syntax.Expression] -> Phantoms.TTerm Syntax.VectorLiteral
vectorLiteral elements =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.lisp.syntax.VectorLiteral"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "elements"),
          Core.fieldTerm = (Phantoms.unTTerm elements)}]}))

vectorLiteralElements :: Phantoms.TTerm Syntax.VectorLiteral -> Phantoms.TTerm [Syntax.Expression]
vectorLiteralElements x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.VectorLiteral"),
        Core.projectionField = (Core.Name "elements")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

vectorLiteralWithElements :: Phantoms.TTerm Syntax.VectorLiteral -> Phantoms.TTerm [Syntax.Expression] -> Phantoms.TTerm Syntax.VectorLiteral
vectorLiteralWithElements original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.lisp.syntax.VectorLiteral"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "elements"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

mapLiteral :: Phantoms.TTerm [Syntax.MapEntry] -> Phantoms.TTerm Syntax.MapLiteral
mapLiteral entries =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.lisp.syntax.MapLiteral"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "entries"),
          Core.fieldTerm = (Phantoms.unTTerm entries)}]}))

mapLiteralEntries :: Phantoms.TTerm Syntax.MapLiteral -> Phantoms.TTerm [Syntax.MapEntry]
mapLiteralEntries x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.MapLiteral"),
        Core.projectionField = (Core.Name "entries")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

mapLiteralWithEntries :: Phantoms.TTerm Syntax.MapLiteral -> Phantoms.TTerm [Syntax.MapEntry] -> Phantoms.TTerm Syntax.MapLiteral
mapLiteralWithEntries original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.lisp.syntax.MapLiteral"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "entries"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

mapEntry :: Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.MapEntry
mapEntry key value =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.lisp.syntax.MapEntry"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "key"),
          Core.fieldTerm = (Phantoms.unTTerm key)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm value)}]}))

mapEntryKey :: Phantoms.TTerm Syntax.MapEntry -> Phantoms.TTerm Syntax.Expression
mapEntryKey x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.MapEntry"),
        Core.projectionField = (Core.Name "key")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

mapEntryValue :: Phantoms.TTerm Syntax.MapEntry -> Phantoms.TTerm Syntax.Expression
mapEntryValue x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.MapEntry"),
        Core.projectionField = (Core.Name "value")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

mapEntryWithKey :: Phantoms.TTerm Syntax.MapEntry -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.MapEntry
mapEntryWithKey original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.lisp.syntax.MapEntry"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "key"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.MapEntry"),
              Core.projectionField = (Core.Name "value")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

mapEntryWithValue :: Phantoms.TTerm Syntax.MapEntry -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.MapEntry
mapEntryWithValue original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.lisp.syntax.MapEntry"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "key"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.MapEntry"),
              Core.projectionField = (Core.Name "key")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

setLiteral :: Phantoms.TTerm [Syntax.Expression] -> Phantoms.TTerm Syntax.SetLiteral
setLiteral elements =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.lisp.syntax.SetLiteral"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "elements"),
          Core.fieldTerm = (Phantoms.unTTerm elements)}]}))

setLiteralElements :: Phantoms.TTerm Syntax.SetLiteral -> Phantoms.TTerm [Syntax.Expression]
setLiteralElements x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.SetLiteral"),
        Core.projectionField = (Core.Name "elements")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

setLiteralWithElements :: Phantoms.TTerm Syntax.SetLiteral -> Phantoms.TTerm [Syntax.Expression] -> Phantoms.TTerm Syntax.SetLiteral
setLiteralWithElements original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.lisp.syntax.SetLiteral"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "elements"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

consExpression :: Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.ConsExpression
consExpression head tail =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.lisp.syntax.ConsExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "head"),
          Core.fieldTerm = (Phantoms.unTTerm head)},
        Core.Field {
          Core.fieldName = (Core.Name "tail"),
          Core.fieldTerm = (Phantoms.unTTerm tail)}]}))

consExpressionHead :: Phantoms.TTerm Syntax.ConsExpression -> Phantoms.TTerm Syntax.Expression
consExpressionHead x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.ConsExpression"),
        Core.projectionField = (Core.Name "head")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

consExpressionTail :: Phantoms.TTerm Syntax.ConsExpression -> Phantoms.TTerm Syntax.Expression
consExpressionTail x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.ConsExpression"),
        Core.projectionField = (Core.Name "tail")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

consExpressionWithHead :: Phantoms.TTerm Syntax.ConsExpression -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.ConsExpression
consExpressionWithHead original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.lisp.syntax.ConsExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "head"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "tail"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.ConsExpression"),
              Core.projectionField = (Core.Name "tail")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

consExpressionWithTail :: Phantoms.TTerm Syntax.ConsExpression -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.ConsExpression
consExpressionWithTail original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.lisp.syntax.ConsExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "head"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.ConsExpression"),
              Core.projectionField = (Core.Name "head")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tail"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

dottedPair :: Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.DottedPair
dottedPair car cdr =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.lisp.syntax.DottedPair"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "car"),
          Core.fieldTerm = (Phantoms.unTTerm car)},
        Core.Field {
          Core.fieldName = (Core.Name "cdr"),
          Core.fieldTerm = (Phantoms.unTTerm cdr)}]}))

dottedPairCar :: Phantoms.TTerm Syntax.DottedPair -> Phantoms.TTerm Syntax.Expression
dottedPairCar x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.DottedPair"),
        Core.projectionField = (Core.Name "car")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

dottedPairCdr :: Phantoms.TTerm Syntax.DottedPair -> Phantoms.TTerm Syntax.Expression
dottedPairCdr x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.DottedPair"),
        Core.projectionField = (Core.Name "cdr")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

dottedPairWithCar :: Phantoms.TTerm Syntax.DottedPair -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.DottedPair
dottedPairWithCar original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.lisp.syntax.DottedPair"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "car"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "cdr"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.DottedPair"),
              Core.projectionField = (Core.Name "cdr")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

dottedPairWithCdr :: Phantoms.TTerm Syntax.DottedPair -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.DottedPair
dottedPairWithCdr original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.lisp.syntax.DottedPair"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "car"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.DottedPair"),
              Core.projectionField = (Core.Name "car")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "cdr"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

typeHint :: Phantoms.TTerm Syntax.Symbol -> Phantoms.TTerm Syntax.TypeSpecifier -> Phantoms.TTerm Syntax.TypeHint
typeHint name type_ =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.lisp.syntax.TypeHint"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm type_)}]}))

typeHintName :: Phantoms.TTerm Syntax.TypeHint -> Phantoms.TTerm Syntax.Symbol
typeHintName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.TypeHint"),
        Core.projectionField = (Core.Name "name")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

typeHintType :: Phantoms.TTerm Syntax.TypeHint -> Phantoms.TTerm Syntax.TypeSpecifier
typeHintType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.TypeHint"),
        Core.projectionField = (Core.Name "type")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

typeHintWithName :: Phantoms.TTerm Syntax.TypeHint -> Phantoms.TTerm Syntax.Symbol -> Phantoms.TTerm Syntax.TypeHint
typeHintWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.lisp.syntax.TypeHint"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.TypeHint"),
              Core.projectionField = (Core.Name "type")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

typeHintWithType :: Phantoms.TTerm Syntax.TypeHint -> Phantoms.TTerm Syntax.TypeSpecifier -> Phantoms.TTerm Syntax.TypeHint
typeHintWithType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.lisp.syntax.TypeHint"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.TypeHint"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

typeSpecifierNamed :: Phantoms.TTerm Syntax.Symbol -> Phantoms.TTerm Syntax.TypeSpecifier
typeSpecifierNamed x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.lisp.syntax.TypeSpecifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "named"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

typeSpecifierList :: Phantoms.TTerm Syntax.TypeSpecifier -> Phantoms.TTerm Syntax.TypeSpecifier
typeSpecifierList x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.lisp.syntax.TypeSpecifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "list"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

typeSpecifierFunction :: Phantoms.TTerm [Syntax.TypeSpecifier] -> Phantoms.TTerm Syntax.TypeSpecifier
typeSpecifierFunction x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.lisp.syntax.TypeSpecifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "function"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

typeSpecifierMaybe :: Phantoms.TTerm Syntax.TypeSpecifier -> Phantoms.TTerm Syntax.TypeSpecifier
typeSpecifierMaybe x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.lisp.syntax.TypeSpecifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "maybe"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

typeSpecifierMap :: Phantoms.TTerm [Syntax.TypeSpecifier] -> Phantoms.TTerm Syntax.TypeSpecifier
typeSpecifierMap x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.lisp.syntax.TypeSpecifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "map"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

typeSpecifierSet :: Phantoms.TTerm Syntax.TypeSpecifier -> Phantoms.TTerm Syntax.TypeSpecifier
typeSpecifierSet x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.lisp.syntax.TypeSpecifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "set"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

typeSpecifierPair :: Phantoms.TTerm [Syntax.TypeSpecifier] -> Phantoms.TTerm Syntax.TypeSpecifier
typeSpecifierPair x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.lisp.syntax.TypeSpecifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "pair"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

typeSpecifierEither :: Phantoms.TTerm [Syntax.TypeSpecifier] -> Phantoms.TTerm Syntax.TypeSpecifier
typeSpecifierEither x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.lisp.syntax.TypeSpecifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "either"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

typeSpecifierUnit :: Phantoms.TTerm Syntax.TypeSpecifier
typeSpecifierUnit =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.lisp.syntax.TypeSpecifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "unit"),
        Core.fieldTerm = Core.TermUnit}}))

moduleDeclaration :: Phantoms.TTerm Syntax.NamespaceName -> Phantoms.TTerm (Maybe Syntax.Docstring) -> Phantoms.TTerm Syntax.ModuleDeclaration
moduleDeclaration name doc =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.lisp.syntax.ModuleDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Phantoms.unTTerm doc)}]}))

moduleDeclarationName :: Phantoms.TTerm Syntax.ModuleDeclaration -> Phantoms.TTerm Syntax.NamespaceName
moduleDeclarationName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.ModuleDeclaration"),
        Core.projectionField = (Core.Name "name")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

moduleDeclarationDoc :: Phantoms.TTerm Syntax.ModuleDeclaration -> Phantoms.TTerm (Maybe Syntax.Docstring)
moduleDeclarationDoc x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.ModuleDeclaration"),
        Core.projectionField = (Core.Name "doc")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

moduleDeclarationWithName :: Phantoms.TTerm Syntax.ModuleDeclaration -> Phantoms.TTerm Syntax.NamespaceName -> Phantoms.TTerm Syntax.ModuleDeclaration
moduleDeclarationWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.lisp.syntax.ModuleDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.ModuleDeclaration"),
              Core.projectionField = (Core.Name "doc")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

moduleDeclarationWithDoc :: Phantoms.TTerm Syntax.ModuleDeclaration -> Phantoms.TTerm (Maybe Syntax.Docstring) -> Phantoms.TTerm Syntax.ModuleDeclaration
moduleDeclarationWithDoc original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.lisp.syntax.ModuleDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.ModuleDeclaration"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

importDeclaration :: Phantoms.TTerm Syntax.NamespaceName -> Phantoms.TTerm Syntax.ImportSpec -> Phantoms.TTerm Syntax.ImportDeclaration
importDeclaration module_ spec =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.lisp.syntax.ImportDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "module"),
          Core.fieldTerm = (Phantoms.unTTerm module_)},
        Core.Field {
          Core.fieldName = (Core.Name "spec"),
          Core.fieldTerm = (Phantoms.unTTerm spec)}]}))

importDeclarationModule :: Phantoms.TTerm Syntax.ImportDeclaration -> Phantoms.TTerm Syntax.NamespaceName
importDeclarationModule x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.ImportDeclaration"),
        Core.projectionField = (Core.Name "module")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

importDeclarationSpec :: Phantoms.TTerm Syntax.ImportDeclaration -> Phantoms.TTerm Syntax.ImportSpec
importDeclarationSpec x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.ImportDeclaration"),
        Core.projectionField = (Core.Name "spec")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

importDeclarationWithModule :: Phantoms.TTerm Syntax.ImportDeclaration -> Phantoms.TTerm Syntax.NamespaceName -> Phantoms.TTerm Syntax.ImportDeclaration
importDeclarationWithModule original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.lisp.syntax.ImportDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "module"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "spec"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.ImportDeclaration"),
              Core.projectionField = (Core.Name "spec")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

importDeclarationWithSpec :: Phantoms.TTerm Syntax.ImportDeclaration -> Phantoms.TTerm Syntax.ImportSpec -> Phantoms.TTerm Syntax.ImportDeclaration
importDeclarationWithSpec original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.lisp.syntax.ImportDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "module"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.ImportDeclaration"),
              Core.projectionField = (Core.Name "module")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "spec"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

importSpecAll :: Phantoms.TTerm Syntax.ImportSpec
importSpecAll =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.lisp.syntax.ImportSpec"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "all"),
        Core.fieldTerm = Core.TermUnit}}))

importSpecAlias :: Phantoms.TTerm Syntax.Symbol -> Phantoms.TTerm Syntax.ImportSpec
importSpecAlias x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.lisp.syntax.ImportSpec"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "alias"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

importSpecOnly :: Phantoms.TTerm [Syntax.Symbol] -> Phantoms.TTerm Syntax.ImportSpec
importSpecOnly x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.lisp.syntax.ImportSpec"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "only"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

importSpecRename :: Phantoms.TTerm [[Syntax.Symbol]] -> Phantoms.TTerm Syntax.ImportSpec
importSpecRename x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.lisp.syntax.ImportSpec"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "rename"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

exportDeclaration :: Phantoms.TTerm [Syntax.Symbol] -> Phantoms.TTerm Syntax.ExportDeclaration
exportDeclaration symbols =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.lisp.syntax.ExportDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "symbols"),
          Core.fieldTerm = (Phantoms.unTTerm symbols)}]}))

exportDeclarationSymbols :: Phantoms.TTerm Syntax.ExportDeclaration -> Phantoms.TTerm [Syntax.Symbol]
exportDeclarationSymbols x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.ExportDeclaration"),
        Core.projectionField = (Core.Name "symbols")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

exportDeclarationWithSymbols :: Phantoms.TTerm Syntax.ExportDeclaration -> Phantoms.TTerm [Syntax.Symbol] -> Phantoms.TTerm Syntax.ExportDeclaration
exportDeclarationWithSymbols original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.lisp.syntax.ExportDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "symbols"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

comment :: Phantoms.TTerm Syntax.CommentStyle -> Phantoms.TTerm String -> Phantoms.TTerm Syntax.Comment
comment style text =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.lisp.syntax.Comment"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "style"),
          Core.fieldTerm = (Phantoms.unTTerm style)},
        Core.Field {
          Core.fieldName = (Core.Name "text"),
          Core.fieldTerm = (Phantoms.unTTerm text)}]}))

commentStyle :: Phantoms.TTerm Syntax.Comment -> Phantoms.TTerm Syntax.CommentStyle
commentStyle x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.Comment"),
        Core.projectionField = (Core.Name "style")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

commentText :: Phantoms.TTerm Syntax.Comment -> Phantoms.TTerm String
commentText x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.Comment"),
        Core.projectionField = (Core.Name "text")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

commentWithStyle :: Phantoms.TTerm Syntax.Comment -> Phantoms.TTerm Syntax.CommentStyle -> Phantoms.TTerm Syntax.Comment
commentWithStyle original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.lisp.syntax.Comment"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "style"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "text"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.Comment"),
              Core.projectionField = (Core.Name "text")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

commentWithText :: Phantoms.TTerm Syntax.Comment -> Phantoms.TTerm String -> Phantoms.TTerm Syntax.Comment
commentWithText original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.lisp.syntax.Comment"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "style"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.lisp.syntax.Comment"),
              Core.projectionField = (Core.Name "style")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "text"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

commentStyleLine :: Phantoms.TTerm Syntax.CommentStyle
commentStyleLine =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.lisp.syntax.CommentStyle"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "line"),
        Core.fieldTerm = Core.TermUnit}}))

commentStyleBlock :: Phantoms.TTerm Syntax.CommentStyle
commentStyleBlock =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.lisp.syntax.CommentStyle"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "block"),
        Core.fieldTerm = Core.TermUnit}}))

commentStyleDatum :: Phantoms.TTerm Syntax.CommentStyle
commentStyleDatum =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.lisp.syntax.CommentStyle"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "datum"),
        Core.fieldTerm = Core.TermUnit}}))

docstring :: Phantoms.TTerm String -> Phantoms.TTerm Syntax.Docstring
docstring x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.lisp.syntax.Docstring"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unDocstring :: Phantoms.TTerm Syntax.Docstring -> Phantoms.TTerm String
unDocstring x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.lisp.syntax.Docstring")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

dialectClojure :: Phantoms.TTerm Syntax.Dialect
dialectClojure =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.lisp.syntax.Dialect"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "clojure"),
        Core.fieldTerm = Core.TermUnit}}))

dialectEmacsLisp :: Phantoms.TTerm Syntax.Dialect
dialectEmacsLisp =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.lisp.syntax.Dialect"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "emacsLisp"),
        Core.fieldTerm = Core.TermUnit}}))

dialectCommonLisp :: Phantoms.TTerm Syntax.Dialect
dialectCommonLisp =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.lisp.syntax.Dialect"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "commonLisp"),
        Core.fieldTerm = Core.TermUnit}}))

dialectScheme :: Phantoms.TTerm Syntax.Dialect
dialectScheme =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.lisp.syntax.Dialect"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "scheme"),
        Core.fieldTerm = Core.TermUnit}}))

sExpressionAtom :: Phantoms.TTerm String -> Phantoms.TTerm Syntax.SExpression
sExpressionAtom x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.lisp.syntax.SExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "atom"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

sExpressionList :: Phantoms.TTerm [Syntax.SExpression] -> Phantoms.TTerm Syntax.SExpression
sExpressionList x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.lisp.syntax.SExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "list"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
