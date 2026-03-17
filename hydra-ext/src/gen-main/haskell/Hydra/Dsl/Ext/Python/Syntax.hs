-- Note: this is an automatically generated file. Do not edit.

-- | DSL functions for hydra.ext.python.syntax

module Hydra.Dsl.Ext.Python.Syntax where

import qualified Hydra.Core as Core
import qualified Hydra.Ext.Python.Syntax as Syntax
import qualified Hydra.Phantoms as Phantoms
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

annotatedStatement :: (Phantoms.TTerm String -> Phantoms.TTerm Syntax.Statement -> Phantoms.TTerm Syntax.AnnotatedStatement)
annotatedStatement comment statement = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.AnnotatedStatement"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "comment"),
      Core.fieldTerm = (Phantoms.unTTerm comment)},
    Core.Field {
      Core.fieldName = (Core.Name "statement"),
      Core.fieldTerm = (Phantoms.unTTerm statement)}]})))

annotatedStatementComment :: (Phantoms.TTerm Syntax.AnnotatedStatement -> Phantoms.TTerm String)
annotatedStatementComment x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.AnnotatedStatement"),
    Core.projectionField = (Core.Name "comment")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

annotatedStatementStatement :: (Phantoms.TTerm Syntax.AnnotatedStatement -> Phantoms.TTerm Syntax.Statement)
annotatedStatementStatement x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.AnnotatedStatement"),
    Core.projectionField = (Core.Name "statement")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

annotatedStatementWithComment :: (Phantoms.TTerm Syntax.AnnotatedStatement -> Phantoms.TTerm String -> Phantoms.TTerm Syntax.AnnotatedStatement)
annotatedStatementWithComment original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.AnnotatedStatement"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "comment"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "statement"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.AnnotatedStatement"),
          Core.projectionField = (Core.Name "statement")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

annotatedStatementWithStatement :: (Phantoms.TTerm Syntax.AnnotatedStatement -> Phantoms.TTerm Syntax.Statement -> Phantoms.TTerm Syntax.AnnotatedStatement)
annotatedStatementWithStatement original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.AnnotatedStatement"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "comment"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.AnnotatedStatement"),
          Core.projectionField = (Core.Name "comment")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "statement"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

module_ :: (Phantoms.TTerm [[Syntax.Statement]] -> Phantoms.TTerm Syntax.Module)
module_ x = (Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
  Core.wrappedTermTypeName = (Core.Name "hydra.ext.python.syntax.Module"),
  Core.wrappedTermBody = (Phantoms.unTTerm x)})))

unModule :: (Phantoms.TTerm Syntax.Module -> Phantoms.TTerm [[Syntax.Statement]])
unModule x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.python.syntax.Module")))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

quoteStyleSingle :: (Phantoms.TTerm Syntax.QuoteStyle)
quoteStyleSingle = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.QuoteStyle"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "single"),
    Core.fieldTerm = Core.TermUnit}})))

quoteStyleDouble :: (Phantoms.TTerm Syntax.QuoteStyle)
quoteStyleDouble = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.QuoteStyle"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "double"),
    Core.fieldTerm = Core.TermUnit}})))

quoteStyleTriple :: (Phantoms.TTerm Syntax.QuoteStyle)
quoteStyleTriple = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.QuoteStyle"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "triple"),
    Core.fieldTerm = Core.TermUnit}})))

name :: (Phantoms.TTerm String -> Phantoms.TTerm Syntax.Name)
name x = (Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
  Core.wrappedTermTypeName = (Core.Name "hydra.ext.python.syntax.Name"),
  Core.wrappedTermBody = (Phantoms.unTTerm x)})))

unName :: (Phantoms.TTerm Syntax.Name -> Phantoms.TTerm String)
unName x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.python.syntax.Name")))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

numberInteger :: (Phantoms.TTerm Integer -> Phantoms.TTerm Syntax.Number)
numberInteger x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.Number"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "integer"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

numberFloat :: (Phantoms.TTerm Double -> Phantoms.TTerm Syntax.Number)
numberFloat x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.Number"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "float"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

string :: (Phantoms.TTerm String -> Phantoms.TTerm Syntax.QuoteStyle -> Phantoms.TTerm Syntax.String_)
string value quoteStyle = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.String"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "value"),
      Core.fieldTerm = (Phantoms.unTTerm value)},
    Core.Field {
      Core.fieldName = (Core.Name "quoteStyle"),
      Core.fieldTerm = (Phantoms.unTTerm quoteStyle)}]})))

stringValue :: (Phantoms.TTerm Syntax.String_ -> Phantoms.TTerm String)
stringValue x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.String"),
    Core.projectionField = (Core.Name "value")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

stringQuoteStyle :: (Phantoms.TTerm Syntax.String_ -> Phantoms.TTerm Syntax.QuoteStyle)
stringQuoteStyle x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.String"),
    Core.projectionField = (Core.Name "quoteStyle")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

stringWithValue :: (Phantoms.TTerm Syntax.String_ -> Phantoms.TTerm String -> Phantoms.TTerm Syntax.String_)
stringWithValue original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.String"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "value"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "quoteStyle"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.String"),
          Core.projectionField = (Core.Name "quoteStyle")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

stringWithQuoteStyle :: (Phantoms.TTerm Syntax.String_ -> Phantoms.TTerm Syntax.QuoteStyle -> Phantoms.TTerm Syntax.String_)
stringWithQuoteStyle original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.String"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "value"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.String"),
          Core.projectionField = (Core.Name "value")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "quoteStyle"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

typeComment :: (Phantoms.TTerm String -> Phantoms.TTerm Syntax.TypeComment)
typeComment x = (Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
  Core.wrappedTermTypeName = (Core.Name "hydra.ext.python.syntax.TypeComment"),
  Core.wrappedTermBody = (Phantoms.unTTerm x)})))

unTypeComment :: (Phantoms.TTerm Syntax.TypeComment -> Phantoms.TTerm String)
unTypeComment x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.python.syntax.TypeComment")))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

file :: (Phantoms.TTerm [Syntax.Statement] -> Phantoms.TTerm Syntax.File)
file x = (Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
  Core.wrappedTermTypeName = (Core.Name "hydra.ext.python.syntax.File"),
  Core.wrappedTermBody = (Phantoms.unTTerm x)})))

unFile :: (Phantoms.TTerm Syntax.File -> Phantoms.TTerm [Syntax.Statement])
unFile x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.python.syntax.File")))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

interactive :: (Phantoms.TTerm Syntax.Statement -> Phantoms.TTerm Syntax.Interactive)
interactive x = (Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
  Core.wrappedTermTypeName = (Core.Name "hydra.ext.python.syntax.Interactive"),
  Core.wrappedTermBody = (Phantoms.unTTerm x)})))

unInteractive :: (Phantoms.TTerm Syntax.Interactive -> Phantoms.TTerm Syntax.Statement)
unInteractive x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.python.syntax.Interactive")))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

eval :: (Phantoms.TTerm [Syntax.Expression] -> Phantoms.TTerm Syntax.Eval)
eval x = (Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
  Core.wrappedTermTypeName = (Core.Name "hydra.ext.python.syntax.Eval"),
  Core.wrappedTermBody = (Phantoms.unTTerm x)})))

unEval :: (Phantoms.TTerm Syntax.Eval -> Phantoms.TTerm [Syntax.Expression])
unEval x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.python.syntax.Eval")))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

funcType :: (Phantoms.TTerm [Syntax.TypeExpression] -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.FuncType)
funcType type_ body = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.FuncType"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "type"),
      Core.fieldTerm = (Phantoms.unTTerm type_)},
    Core.Field {
      Core.fieldName = (Core.Name "body"),
      Core.fieldTerm = (Phantoms.unTTerm body)}]})))

funcTypeType :: (Phantoms.TTerm Syntax.FuncType -> Phantoms.TTerm [Syntax.TypeExpression])
funcTypeType x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.FuncType"),
    Core.projectionField = (Core.Name "type")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

funcTypeBody :: (Phantoms.TTerm Syntax.FuncType -> Phantoms.TTerm Syntax.Expression)
funcTypeBody x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.FuncType"),
    Core.projectionField = (Core.Name "body")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

funcTypeWithType :: (Phantoms.TTerm Syntax.FuncType -> Phantoms.TTerm [Syntax.TypeExpression] -> Phantoms.TTerm Syntax.FuncType)
funcTypeWithType original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.FuncType"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "type"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "body"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.FuncType"),
          Core.projectionField = (Core.Name "body")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

funcTypeWithBody :: (Phantoms.TTerm Syntax.FuncType -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.FuncType)
funcTypeWithBody original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.FuncType"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "type"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.FuncType"),
          Core.projectionField = (Core.Name "type")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "body"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

statementCompound :: (Phantoms.TTerm Syntax.CompoundStatement -> Phantoms.TTerm Syntax.Statement)
statementCompound x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.Statement"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "compound"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

statementSimple :: (Phantoms.TTerm [Syntax.SimpleStatement] -> Phantoms.TTerm Syntax.Statement)
statementSimple x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.Statement"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "simple"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

statementAnnotated :: (Phantoms.TTerm Syntax.AnnotatedStatement -> Phantoms.TTerm Syntax.Statement)
statementAnnotated x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.Statement"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "annotated"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

simpleStatementAssignment :: (Phantoms.TTerm Syntax.Assignment -> Phantoms.TTerm Syntax.SimpleStatement)
simpleStatementAssignment x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.SimpleStatement"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "assignment"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

simpleStatementTypeAlias :: (Phantoms.TTerm Syntax.TypeAlias -> Phantoms.TTerm Syntax.SimpleStatement)
simpleStatementTypeAlias x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.SimpleStatement"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "typeAlias"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

simpleStatementStarExpressions :: (Phantoms.TTerm [Syntax.StarExpression] -> Phantoms.TTerm Syntax.SimpleStatement)
simpleStatementStarExpressions x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.SimpleStatement"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "starExpressions"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

simpleStatementReturn :: (Phantoms.TTerm Syntax.ReturnStatement -> Phantoms.TTerm Syntax.SimpleStatement)
simpleStatementReturn x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.SimpleStatement"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "return"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

simpleStatementImport :: (Phantoms.TTerm Syntax.ImportStatement -> Phantoms.TTerm Syntax.SimpleStatement)
simpleStatementImport x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.SimpleStatement"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "import"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

simpleStatementRaise :: (Phantoms.TTerm Syntax.RaiseStatement -> Phantoms.TTerm Syntax.SimpleStatement)
simpleStatementRaise x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.SimpleStatement"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "raise"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

simpleStatementPass :: (Phantoms.TTerm Syntax.SimpleStatement)
simpleStatementPass = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.SimpleStatement"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "pass"),
    Core.fieldTerm = Core.TermUnit}})))

simpleStatementDel :: (Phantoms.TTerm Syntax.DelStatement -> Phantoms.TTerm Syntax.SimpleStatement)
simpleStatementDel x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.SimpleStatement"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "del"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

simpleStatementYield :: (Phantoms.TTerm Syntax.YieldStatement -> Phantoms.TTerm Syntax.SimpleStatement)
simpleStatementYield x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.SimpleStatement"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "yield"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

simpleStatementAssert :: (Phantoms.TTerm Syntax.AssertStatement -> Phantoms.TTerm Syntax.SimpleStatement)
simpleStatementAssert x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.SimpleStatement"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "assert"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

simpleStatementBreak :: (Phantoms.TTerm Syntax.SimpleStatement)
simpleStatementBreak = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.SimpleStatement"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "break"),
    Core.fieldTerm = Core.TermUnit}})))

simpleStatementContinue :: (Phantoms.TTerm Syntax.SimpleStatement)
simpleStatementContinue = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.SimpleStatement"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "continue"),
    Core.fieldTerm = Core.TermUnit}})))

simpleStatementGlobal :: (Phantoms.TTerm [Syntax.Name] -> Phantoms.TTerm Syntax.SimpleStatement)
simpleStatementGlobal x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.SimpleStatement"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "global"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

simpleStatementNonlocal :: (Phantoms.TTerm [Syntax.Name] -> Phantoms.TTerm Syntax.SimpleStatement)
simpleStatementNonlocal x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.SimpleStatement"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "nonlocal"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

compoundStatementFunction :: (Phantoms.TTerm Syntax.FunctionDefinition -> Phantoms.TTerm Syntax.CompoundStatement)
compoundStatementFunction x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.CompoundStatement"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "function"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

compoundStatementIf :: (Phantoms.TTerm Syntax.IfStatement -> Phantoms.TTerm Syntax.CompoundStatement)
compoundStatementIf x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.CompoundStatement"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "if"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

compoundStatementClassDef :: (Phantoms.TTerm Syntax.ClassDefinition -> Phantoms.TTerm Syntax.CompoundStatement)
compoundStatementClassDef x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.CompoundStatement"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "classDef"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

compoundStatementWith :: (Phantoms.TTerm Syntax.WithStatement -> Phantoms.TTerm Syntax.CompoundStatement)
compoundStatementWith x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.CompoundStatement"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "with"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

compoundStatementFor :: (Phantoms.TTerm Syntax.ForStatement -> Phantoms.TTerm Syntax.CompoundStatement)
compoundStatementFor x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.CompoundStatement"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "for"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

compoundStatementTry :: (Phantoms.TTerm Syntax.TryStatement -> Phantoms.TTerm Syntax.CompoundStatement)
compoundStatementTry x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.CompoundStatement"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "try"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

compoundStatementWhile :: (Phantoms.TTerm Syntax.WhileStatement -> Phantoms.TTerm Syntax.CompoundStatement)
compoundStatementWhile x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.CompoundStatement"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "while"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

compoundStatementMatch :: (Phantoms.TTerm Syntax.MatchStatement -> Phantoms.TTerm Syntax.CompoundStatement)
compoundStatementMatch x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.CompoundStatement"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "match"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

assignmentTyped :: (Phantoms.TTerm Syntax.TypedAssignment -> Phantoms.TTerm Syntax.Assignment)
assignmentTyped x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.Assignment"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "typed"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

assignmentUntyped :: (Phantoms.TTerm Syntax.UntypedAssignment -> Phantoms.TTerm Syntax.Assignment)
assignmentUntyped x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.Assignment"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "untyped"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

assignmentAug :: (Phantoms.TTerm Syntax.AugAssignment -> Phantoms.TTerm Syntax.Assignment)
assignmentAug x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.Assignment"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "aug"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

typedAssignment :: (Phantoms.TTerm Syntax.SingleTarget -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm (Maybe Syntax.AnnotatedRhs) -> Phantoms.TTerm Syntax.TypedAssignment)
typedAssignment lhs type_ rhs = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.TypedAssignment"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "lhs"),
      Core.fieldTerm = (Phantoms.unTTerm lhs)},
    Core.Field {
      Core.fieldName = (Core.Name "type"),
      Core.fieldTerm = (Phantoms.unTTerm type_)},
    Core.Field {
      Core.fieldName = (Core.Name "rhs"),
      Core.fieldTerm = (Phantoms.unTTerm rhs)}]})))

typedAssignmentLhs :: (Phantoms.TTerm Syntax.TypedAssignment -> Phantoms.TTerm Syntax.SingleTarget)
typedAssignmentLhs x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.TypedAssignment"),
    Core.projectionField = (Core.Name "lhs")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

typedAssignmentType :: (Phantoms.TTerm Syntax.TypedAssignment -> Phantoms.TTerm Syntax.Expression)
typedAssignmentType x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.TypedAssignment"),
    Core.projectionField = (Core.Name "type")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

typedAssignmentRhs :: (Phantoms.TTerm Syntax.TypedAssignment -> Phantoms.TTerm (Maybe Syntax.AnnotatedRhs))
typedAssignmentRhs x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.TypedAssignment"),
    Core.projectionField = (Core.Name "rhs")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

typedAssignmentWithLhs :: (Phantoms.TTerm Syntax.TypedAssignment -> Phantoms.TTerm Syntax.SingleTarget -> Phantoms.TTerm Syntax.TypedAssignment)
typedAssignmentWithLhs original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.TypedAssignment"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "lhs"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "type"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.TypedAssignment"),
          Core.projectionField = (Core.Name "type")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "rhs"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.TypedAssignment"),
          Core.projectionField = (Core.Name "rhs")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

typedAssignmentWithType :: (Phantoms.TTerm Syntax.TypedAssignment -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.TypedAssignment)
typedAssignmentWithType original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.TypedAssignment"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "lhs"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.TypedAssignment"),
          Core.projectionField = (Core.Name "lhs")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "type"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "rhs"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.TypedAssignment"),
          Core.projectionField = (Core.Name "rhs")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

typedAssignmentWithRhs :: (Phantoms.TTerm Syntax.TypedAssignment -> Phantoms.TTerm (Maybe Syntax.AnnotatedRhs) -> Phantoms.TTerm Syntax.TypedAssignment)
typedAssignmentWithRhs original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.TypedAssignment"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "lhs"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.TypedAssignment"),
          Core.projectionField = (Core.Name "lhs")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "type"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.TypedAssignment"),
          Core.projectionField = (Core.Name "type")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "rhs"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

untypedAssignment :: (Phantoms.TTerm [Syntax.StarTarget] -> Phantoms.TTerm Syntax.AnnotatedRhs -> Phantoms.TTerm (Maybe Syntax.TypeComment) -> Phantoms.TTerm Syntax.UntypedAssignment)
untypedAssignment targets rhs typeComment = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.UntypedAssignment"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "targets"),
      Core.fieldTerm = (Phantoms.unTTerm targets)},
    Core.Field {
      Core.fieldName = (Core.Name "rhs"),
      Core.fieldTerm = (Phantoms.unTTerm rhs)},
    Core.Field {
      Core.fieldName = (Core.Name "typeComment"),
      Core.fieldTerm = (Phantoms.unTTerm typeComment)}]})))

untypedAssignmentTargets :: (Phantoms.TTerm Syntax.UntypedAssignment -> Phantoms.TTerm [Syntax.StarTarget])
untypedAssignmentTargets x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.UntypedAssignment"),
    Core.projectionField = (Core.Name "targets")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

untypedAssignmentRhs :: (Phantoms.TTerm Syntax.UntypedAssignment -> Phantoms.TTerm Syntax.AnnotatedRhs)
untypedAssignmentRhs x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.UntypedAssignment"),
    Core.projectionField = (Core.Name "rhs")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

untypedAssignmentTypeComment :: (Phantoms.TTerm Syntax.UntypedAssignment -> Phantoms.TTerm (Maybe Syntax.TypeComment))
untypedAssignmentTypeComment x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.UntypedAssignment"),
    Core.projectionField = (Core.Name "typeComment")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

untypedAssignmentWithTargets :: (Phantoms.TTerm Syntax.UntypedAssignment -> Phantoms.TTerm [Syntax.StarTarget] -> Phantoms.TTerm Syntax.UntypedAssignment)
untypedAssignmentWithTargets original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.UntypedAssignment"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "targets"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "rhs"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.UntypedAssignment"),
          Core.projectionField = (Core.Name "rhs")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "typeComment"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.UntypedAssignment"),
          Core.projectionField = (Core.Name "typeComment")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

untypedAssignmentWithRhs :: (Phantoms.TTerm Syntax.UntypedAssignment -> Phantoms.TTerm Syntax.AnnotatedRhs -> Phantoms.TTerm Syntax.UntypedAssignment)
untypedAssignmentWithRhs original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.UntypedAssignment"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "targets"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.UntypedAssignment"),
          Core.projectionField = (Core.Name "targets")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "rhs"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "typeComment"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.UntypedAssignment"),
          Core.projectionField = (Core.Name "typeComment")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

untypedAssignmentWithTypeComment :: (Phantoms.TTerm Syntax.UntypedAssignment -> Phantoms.TTerm (Maybe Syntax.TypeComment) -> Phantoms.TTerm Syntax.UntypedAssignment)
untypedAssignmentWithTypeComment original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.UntypedAssignment"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "targets"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.UntypedAssignment"),
          Core.projectionField = (Core.Name "targets")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "rhs"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.UntypedAssignment"),
          Core.projectionField = (Core.Name "rhs")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "typeComment"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

augAssignment :: (Phantoms.TTerm Syntax.SingleTarget -> Phantoms.TTerm Syntax.AugAssign -> Phantoms.TTerm Syntax.AnnotatedRhs -> Phantoms.TTerm Syntax.AugAssignment)
augAssignment lhs augassign rhs = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.AugAssignment"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "lhs"),
      Core.fieldTerm = (Phantoms.unTTerm lhs)},
    Core.Field {
      Core.fieldName = (Core.Name "augassign"),
      Core.fieldTerm = (Phantoms.unTTerm augassign)},
    Core.Field {
      Core.fieldName = (Core.Name "rhs"),
      Core.fieldTerm = (Phantoms.unTTerm rhs)}]})))

augAssignmentLhs :: (Phantoms.TTerm Syntax.AugAssignment -> Phantoms.TTerm Syntax.SingleTarget)
augAssignmentLhs x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.AugAssignment"),
    Core.projectionField = (Core.Name "lhs")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

augAssignmentAugassign :: (Phantoms.TTerm Syntax.AugAssignment -> Phantoms.TTerm Syntax.AugAssign)
augAssignmentAugassign x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.AugAssignment"),
    Core.projectionField = (Core.Name "augassign")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

augAssignmentRhs :: (Phantoms.TTerm Syntax.AugAssignment -> Phantoms.TTerm Syntax.AnnotatedRhs)
augAssignmentRhs x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.AugAssignment"),
    Core.projectionField = (Core.Name "rhs")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

augAssignmentWithLhs :: (Phantoms.TTerm Syntax.AugAssignment -> Phantoms.TTerm Syntax.SingleTarget -> Phantoms.TTerm Syntax.AugAssignment)
augAssignmentWithLhs original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.AugAssignment"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "lhs"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "augassign"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.AugAssignment"),
          Core.projectionField = (Core.Name "augassign")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "rhs"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.AugAssignment"),
          Core.projectionField = (Core.Name "rhs")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

augAssignmentWithAugassign :: (Phantoms.TTerm Syntax.AugAssignment -> Phantoms.TTerm Syntax.AugAssign -> Phantoms.TTerm Syntax.AugAssignment)
augAssignmentWithAugassign original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.AugAssignment"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "lhs"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.AugAssignment"),
          Core.projectionField = (Core.Name "lhs")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "augassign"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "rhs"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.AugAssignment"),
          Core.projectionField = (Core.Name "rhs")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

augAssignmentWithRhs :: (Phantoms.TTerm Syntax.AugAssignment -> Phantoms.TTerm Syntax.AnnotatedRhs -> Phantoms.TTerm Syntax.AugAssignment)
augAssignmentWithRhs original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.AugAssignment"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "lhs"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.AugAssignment"),
          Core.projectionField = (Core.Name "lhs")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "augassign"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.AugAssignment"),
          Core.projectionField = (Core.Name "augassign")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "rhs"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

annotatedRhsYield :: (Phantoms.TTerm Syntax.YieldExpression -> Phantoms.TTerm Syntax.AnnotatedRhs)
annotatedRhsYield x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.AnnotatedRhs"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "yield"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

annotatedRhsStar :: (Phantoms.TTerm [Syntax.StarExpression] -> Phantoms.TTerm Syntax.AnnotatedRhs)
annotatedRhsStar x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.AnnotatedRhs"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "star"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

augAssignPlusEqual :: (Phantoms.TTerm Syntax.AugAssign)
augAssignPlusEqual = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.AugAssign"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "plusEqual"),
    Core.fieldTerm = Core.TermUnit}})))

augAssignMinusEqual :: (Phantoms.TTerm Syntax.AugAssign)
augAssignMinusEqual = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.AugAssign"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "minusEqual"),
    Core.fieldTerm = Core.TermUnit}})))

augAssignTimesEqual :: (Phantoms.TTerm Syntax.AugAssign)
augAssignTimesEqual = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.AugAssign"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "timesEqual"),
    Core.fieldTerm = Core.TermUnit}})))

augAssignAtEqual :: (Phantoms.TTerm Syntax.AugAssign)
augAssignAtEqual = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.AugAssign"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "atEqual"),
    Core.fieldTerm = Core.TermUnit}})))

augAssignSlashEqual :: (Phantoms.TTerm Syntax.AugAssign)
augAssignSlashEqual = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.AugAssign"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "slashEqual"),
    Core.fieldTerm = Core.TermUnit}})))

augAssignPercentEqual :: (Phantoms.TTerm Syntax.AugAssign)
augAssignPercentEqual = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.AugAssign"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "percentEqual"),
    Core.fieldTerm = Core.TermUnit}})))

augAssignAmpersandEqual :: (Phantoms.TTerm Syntax.AugAssign)
augAssignAmpersandEqual = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.AugAssign"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "ampersandEqual"),
    Core.fieldTerm = Core.TermUnit}})))

augAssignBarEqual :: (Phantoms.TTerm Syntax.AugAssign)
augAssignBarEqual = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.AugAssign"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "barEqual"),
    Core.fieldTerm = Core.TermUnit}})))

augAssignCaretEqual :: (Phantoms.TTerm Syntax.AugAssign)
augAssignCaretEqual = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.AugAssign"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "caretEqual"),
    Core.fieldTerm = Core.TermUnit}})))

augAssignLeftShiftEqual :: (Phantoms.TTerm Syntax.AugAssign)
augAssignLeftShiftEqual = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.AugAssign"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "leftShiftEqual"),
    Core.fieldTerm = Core.TermUnit}})))

augAssignRightShiftEqual :: (Phantoms.TTerm Syntax.AugAssign)
augAssignRightShiftEqual = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.AugAssign"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "rightShiftEqual"),
    Core.fieldTerm = Core.TermUnit}})))

augAssignStarStarEqual :: (Phantoms.TTerm Syntax.AugAssign)
augAssignStarStarEqual = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.AugAssign"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "starStarEqual"),
    Core.fieldTerm = Core.TermUnit}})))

augAssignDoubleSlashEqual :: (Phantoms.TTerm Syntax.AugAssign)
augAssignDoubleSlashEqual = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.AugAssign"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "doubleSlashEqual"),
    Core.fieldTerm = Core.TermUnit}})))

returnStatement :: (Phantoms.TTerm [Syntax.StarExpression] -> Phantoms.TTerm Syntax.ReturnStatement)
returnStatement x = (Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
  Core.wrappedTermTypeName = (Core.Name "hydra.ext.python.syntax.ReturnStatement"),
  Core.wrappedTermBody = (Phantoms.unTTerm x)})))

unReturnStatement :: (Phantoms.TTerm Syntax.ReturnStatement -> Phantoms.TTerm [Syntax.StarExpression])
unReturnStatement x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.python.syntax.ReturnStatement")))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

raiseStatement :: (Phantoms.TTerm (Maybe Syntax.RaiseExpression) -> Phantoms.TTerm Syntax.RaiseStatement)
raiseStatement x = (Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
  Core.wrappedTermTypeName = (Core.Name "hydra.ext.python.syntax.RaiseStatement"),
  Core.wrappedTermBody = (Phantoms.unTTerm x)})))

unRaiseStatement :: (Phantoms.TTerm Syntax.RaiseStatement -> Phantoms.TTerm (Maybe Syntax.RaiseExpression))
unRaiseStatement x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.python.syntax.RaiseStatement")))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

raiseExpression :: (Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm (Maybe Syntax.Expression) -> Phantoms.TTerm Syntax.RaiseExpression)
raiseExpression expression from = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.RaiseExpression"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "expression"),
      Core.fieldTerm = (Phantoms.unTTerm expression)},
    Core.Field {
      Core.fieldName = (Core.Name "from"),
      Core.fieldTerm = (Phantoms.unTTerm from)}]})))

raiseExpressionExpression :: (Phantoms.TTerm Syntax.RaiseExpression -> Phantoms.TTerm Syntax.Expression)
raiseExpressionExpression x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.RaiseExpression"),
    Core.projectionField = (Core.Name "expression")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

raiseExpressionFrom :: (Phantoms.TTerm Syntax.RaiseExpression -> Phantoms.TTerm (Maybe Syntax.Expression))
raiseExpressionFrom x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.RaiseExpression"),
    Core.projectionField = (Core.Name "from")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

raiseExpressionWithExpression :: (Phantoms.TTerm Syntax.RaiseExpression -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.RaiseExpression)
raiseExpressionWithExpression original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.RaiseExpression"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "expression"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "from"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.RaiseExpression"),
          Core.projectionField = (Core.Name "from")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

raiseExpressionWithFrom :: (Phantoms.TTerm Syntax.RaiseExpression -> Phantoms.TTerm (Maybe Syntax.Expression) -> Phantoms.TTerm Syntax.RaiseExpression)
raiseExpressionWithFrom original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.RaiseExpression"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "expression"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.RaiseExpression"),
          Core.projectionField = (Core.Name "expression")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "from"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

delStatement :: (Phantoms.TTerm Syntax.DelTargets -> Phantoms.TTerm Syntax.DelStatement)
delStatement x = (Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
  Core.wrappedTermTypeName = (Core.Name "hydra.ext.python.syntax.DelStatement"),
  Core.wrappedTermBody = (Phantoms.unTTerm x)})))

unDelStatement :: (Phantoms.TTerm Syntax.DelStatement -> Phantoms.TTerm Syntax.DelTargets)
unDelStatement x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.python.syntax.DelStatement")))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

yieldStatement :: (Phantoms.TTerm Syntax.YieldExpression -> Phantoms.TTerm Syntax.YieldStatement)
yieldStatement x = (Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
  Core.wrappedTermTypeName = (Core.Name "hydra.ext.python.syntax.YieldStatement"),
  Core.wrappedTermBody = (Phantoms.unTTerm x)})))

unYieldStatement :: (Phantoms.TTerm Syntax.YieldStatement -> Phantoms.TTerm Syntax.YieldExpression)
unYieldStatement x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.python.syntax.YieldStatement")))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

assertStatement :: (Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm (Maybe Syntax.Expression) -> Phantoms.TTerm Syntax.AssertStatement)
assertStatement expression1 expression2 = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.AssertStatement"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "expression1"),
      Core.fieldTerm = (Phantoms.unTTerm expression1)},
    Core.Field {
      Core.fieldName = (Core.Name "expression2"),
      Core.fieldTerm = (Phantoms.unTTerm expression2)}]})))

assertStatementExpression1 :: (Phantoms.TTerm Syntax.AssertStatement -> Phantoms.TTerm Syntax.Expression)
assertStatementExpression1 x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.AssertStatement"),
    Core.projectionField = (Core.Name "expression1")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

assertStatementExpression2 :: (Phantoms.TTerm Syntax.AssertStatement -> Phantoms.TTerm (Maybe Syntax.Expression))
assertStatementExpression2 x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.AssertStatement"),
    Core.projectionField = (Core.Name "expression2")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

assertStatementWithExpression1 :: (Phantoms.TTerm Syntax.AssertStatement -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.AssertStatement)
assertStatementWithExpression1 original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.AssertStatement"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "expression1"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "expression2"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.AssertStatement"),
          Core.projectionField = (Core.Name "expression2")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

assertStatementWithExpression2 :: (Phantoms.TTerm Syntax.AssertStatement -> Phantoms.TTerm (Maybe Syntax.Expression) -> Phantoms.TTerm Syntax.AssertStatement)
assertStatementWithExpression2 original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.AssertStatement"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "expression1"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.AssertStatement"),
          Core.projectionField = (Core.Name "expression1")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "expression2"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

importStatementName :: (Phantoms.TTerm Syntax.ImportName -> Phantoms.TTerm Syntax.ImportStatement)
importStatementName x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.ImportStatement"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "name"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

importStatementFrom :: (Phantoms.TTerm Syntax.ImportFrom -> Phantoms.TTerm Syntax.ImportStatement)
importStatementFrom x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.ImportStatement"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "from"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

importName :: (Phantoms.TTerm [Syntax.DottedAsName] -> Phantoms.TTerm Syntax.ImportName)
importName x = (Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
  Core.wrappedTermTypeName = (Core.Name "hydra.ext.python.syntax.ImportName"),
  Core.wrappedTermBody = (Phantoms.unTTerm x)})))

unImportName :: (Phantoms.TTerm Syntax.ImportName -> Phantoms.TTerm [Syntax.DottedAsName])
unImportName x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.python.syntax.ImportName")))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

importFrom :: (Phantoms.TTerm [Syntax.RelativeImportPrefix] -> Phantoms.TTerm (Maybe Syntax.DottedName) -> Phantoms.TTerm Syntax.ImportFromTargets -> Phantoms.TTerm Syntax.ImportFrom)
importFrom prefixes dottedName targets = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.ImportFrom"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "prefixes"),
      Core.fieldTerm = (Phantoms.unTTerm prefixes)},
    Core.Field {
      Core.fieldName = (Core.Name "dottedName"),
      Core.fieldTerm = (Phantoms.unTTerm dottedName)},
    Core.Field {
      Core.fieldName = (Core.Name "targets"),
      Core.fieldTerm = (Phantoms.unTTerm targets)}]})))

importFromPrefixes :: (Phantoms.TTerm Syntax.ImportFrom -> Phantoms.TTerm [Syntax.RelativeImportPrefix])
importFromPrefixes x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.ImportFrom"),
    Core.projectionField = (Core.Name "prefixes")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

importFromDottedName :: (Phantoms.TTerm Syntax.ImportFrom -> Phantoms.TTerm (Maybe Syntax.DottedName))
importFromDottedName x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.ImportFrom"),
    Core.projectionField = (Core.Name "dottedName")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

importFromTargets :: (Phantoms.TTerm Syntax.ImportFrom -> Phantoms.TTerm Syntax.ImportFromTargets)
importFromTargets x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.ImportFrom"),
    Core.projectionField = (Core.Name "targets")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

importFromWithPrefixes :: (Phantoms.TTerm Syntax.ImportFrom -> Phantoms.TTerm [Syntax.RelativeImportPrefix] -> Phantoms.TTerm Syntax.ImportFrom)
importFromWithPrefixes original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.ImportFrom"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "prefixes"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "dottedName"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.ImportFrom"),
          Core.projectionField = (Core.Name "dottedName")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "targets"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.ImportFrom"),
          Core.projectionField = (Core.Name "targets")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

importFromWithDottedName :: (Phantoms.TTerm Syntax.ImportFrom -> Phantoms.TTerm (Maybe Syntax.DottedName) -> Phantoms.TTerm Syntax.ImportFrom)
importFromWithDottedName original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.ImportFrom"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "prefixes"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.ImportFrom"),
          Core.projectionField = (Core.Name "prefixes")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "dottedName"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "targets"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.ImportFrom"),
          Core.projectionField = (Core.Name "targets")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

importFromWithTargets :: (Phantoms.TTerm Syntax.ImportFrom -> Phantoms.TTerm Syntax.ImportFromTargets -> Phantoms.TTerm Syntax.ImportFrom)
importFromWithTargets original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.ImportFrom"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "prefixes"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.ImportFrom"),
          Core.projectionField = (Core.Name "prefixes")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "dottedName"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.ImportFrom"),
          Core.projectionField = (Core.Name "dottedName")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "targets"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

relativeImportPrefixDot :: (Phantoms.TTerm Syntax.RelativeImportPrefix)
relativeImportPrefixDot = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.RelativeImportPrefix"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "dot"),
    Core.fieldTerm = Core.TermUnit}})))

relativeImportPrefixEllipsis :: (Phantoms.TTerm Syntax.RelativeImportPrefix)
relativeImportPrefixEllipsis = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.RelativeImportPrefix"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "ellipsis"),
    Core.fieldTerm = Core.TermUnit}})))

importFromTargetsSimple :: (Phantoms.TTerm [Syntax.ImportFromAsName] -> Phantoms.TTerm Syntax.ImportFromTargets)
importFromTargetsSimple x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.ImportFromTargets"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "simple"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

importFromTargetsParens :: (Phantoms.TTerm [Syntax.ImportFromAsName] -> Phantoms.TTerm Syntax.ImportFromTargets)
importFromTargetsParens x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.ImportFromTargets"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "parens"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

importFromTargetsStar :: (Phantoms.TTerm Syntax.ImportFromTargets)
importFromTargetsStar = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.ImportFromTargets"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "star"),
    Core.fieldTerm = Core.TermUnit}})))

importFromAsName :: (Phantoms.TTerm Syntax.Name -> Phantoms.TTerm (Maybe Syntax.Name) -> Phantoms.TTerm Syntax.ImportFromAsName)
importFromAsName name as = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.ImportFromAsName"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Phantoms.unTTerm name)},
    Core.Field {
      Core.fieldName = (Core.Name "as"),
      Core.fieldTerm = (Phantoms.unTTerm as)}]})))

importFromAsNameName :: (Phantoms.TTerm Syntax.ImportFromAsName -> Phantoms.TTerm Syntax.Name)
importFromAsNameName x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.ImportFromAsName"),
    Core.projectionField = (Core.Name "name")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

importFromAsNameAs :: (Phantoms.TTerm Syntax.ImportFromAsName -> Phantoms.TTerm (Maybe Syntax.Name))
importFromAsNameAs x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.ImportFromAsName"),
    Core.projectionField = (Core.Name "as")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

importFromAsNameWithName :: (Phantoms.TTerm Syntax.ImportFromAsName -> Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.ImportFromAsName)
importFromAsNameWithName original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.ImportFromAsName"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "as"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.ImportFromAsName"),
          Core.projectionField = (Core.Name "as")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

importFromAsNameWithAs :: (Phantoms.TTerm Syntax.ImportFromAsName -> Phantoms.TTerm (Maybe Syntax.Name) -> Phantoms.TTerm Syntax.ImportFromAsName)
importFromAsNameWithAs original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.ImportFromAsName"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.ImportFromAsName"),
          Core.projectionField = (Core.Name "name")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "as"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

dottedAsName :: (Phantoms.TTerm Syntax.DottedName -> Phantoms.TTerm (Maybe Syntax.Name) -> Phantoms.TTerm Syntax.DottedAsName)
dottedAsName name as = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.DottedAsName"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Phantoms.unTTerm name)},
    Core.Field {
      Core.fieldName = (Core.Name "as"),
      Core.fieldTerm = (Phantoms.unTTerm as)}]})))

dottedAsNameName :: (Phantoms.TTerm Syntax.DottedAsName -> Phantoms.TTerm Syntax.DottedName)
dottedAsNameName x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.DottedAsName"),
    Core.projectionField = (Core.Name "name")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

dottedAsNameAs :: (Phantoms.TTerm Syntax.DottedAsName -> Phantoms.TTerm (Maybe Syntax.Name))
dottedAsNameAs x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.DottedAsName"),
    Core.projectionField = (Core.Name "as")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

dottedAsNameWithName :: (Phantoms.TTerm Syntax.DottedAsName -> Phantoms.TTerm Syntax.DottedName -> Phantoms.TTerm Syntax.DottedAsName)
dottedAsNameWithName original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.DottedAsName"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "as"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.DottedAsName"),
          Core.projectionField = (Core.Name "as")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

dottedAsNameWithAs :: (Phantoms.TTerm Syntax.DottedAsName -> Phantoms.TTerm (Maybe Syntax.Name) -> Phantoms.TTerm Syntax.DottedAsName)
dottedAsNameWithAs original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.DottedAsName"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.DottedAsName"),
          Core.projectionField = (Core.Name "name")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "as"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

dottedName :: (Phantoms.TTerm [Syntax.Name] -> Phantoms.TTerm Syntax.DottedName)
dottedName x = (Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
  Core.wrappedTermTypeName = (Core.Name "hydra.ext.python.syntax.DottedName"),
  Core.wrappedTermBody = (Phantoms.unTTerm x)})))

unDottedName :: (Phantoms.TTerm Syntax.DottedName -> Phantoms.TTerm [Syntax.Name])
unDottedName x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.python.syntax.DottedName")))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

blockIndented :: (Phantoms.TTerm [[Syntax.Statement]] -> Phantoms.TTerm Syntax.Block)
blockIndented x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.Block"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "indented"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

blockSimple :: (Phantoms.TTerm [Syntax.SimpleStatement] -> Phantoms.TTerm Syntax.Block)
blockSimple x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.Block"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "simple"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

decorators :: (Phantoms.TTerm [Syntax.NamedExpression] -> Phantoms.TTerm Syntax.Decorators)
decorators x = (Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
  Core.wrappedTermTypeName = (Core.Name "hydra.ext.python.syntax.Decorators"),
  Core.wrappedTermBody = (Phantoms.unTTerm x)})))

unDecorators :: (Phantoms.TTerm Syntax.Decorators -> Phantoms.TTerm [Syntax.NamedExpression])
unDecorators x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.python.syntax.Decorators")))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

classDefinition :: (Phantoms.TTerm (Maybe Syntax.Decorators) -> Phantoms.TTerm Syntax.Name -> Phantoms.TTerm [Syntax.TypeParameter] -> Phantoms.TTerm (Maybe Syntax.Args) -> Phantoms.TTerm Syntax.Block -> Phantoms.TTerm Syntax.ClassDefinition)
classDefinition decorators name typeParams arguments body = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.ClassDefinition"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "decorators"),
      Core.fieldTerm = (Phantoms.unTTerm decorators)},
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Phantoms.unTTerm name)},
    Core.Field {
      Core.fieldName = (Core.Name "typeParams"),
      Core.fieldTerm = (Phantoms.unTTerm typeParams)},
    Core.Field {
      Core.fieldName = (Core.Name "arguments"),
      Core.fieldTerm = (Phantoms.unTTerm arguments)},
    Core.Field {
      Core.fieldName = (Core.Name "body"),
      Core.fieldTerm = (Phantoms.unTTerm body)}]})))

classDefinitionDecorators :: (Phantoms.TTerm Syntax.ClassDefinition -> Phantoms.TTerm (Maybe Syntax.Decorators))
classDefinitionDecorators x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.ClassDefinition"),
    Core.projectionField = (Core.Name "decorators")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

classDefinitionName :: (Phantoms.TTerm Syntax.ClassDefinition -> Phantoms.TTerm Syntax.Name)
classDefinitionName x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.ClassDefinition"),
    Core.projectionField = (Core.Name "name")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

classDefinitionTypeParams :: (Phantoms.TTerm Syntax.ClassDefinition -> Phantoms.TTerm [Syntax.TypeParameter])
classDefinitionTypeParams x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.ClassDefinition"),
    Core.projectionField = (Core.Name "typeParams")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

classDefinitionArguments :: (Phantoms.TTerm Syntax.ClassDefinition -> Phantoms.TTerm (Maybe Syntax.Args))
classDefinitionArguments x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.ClassDefinition"),
    Core.projectionField = (Core.Name "arguments")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

classDefinitionBody :: (Phantoms.TTerm Syntax.ClassDefinition -> Phantoms.TTerm Syntax.Block)
classDefinitionBody x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.ClassDefinition"),
    Core.projectionField = (Core.Name "body")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

classDefinitionWithDecorators :: (Phantoms.TTerm Syntax.ClassDefinition -> Phantoms.TTerm (Maybe Syntax.Decorators) -> Phantoms.TTerm Syntax.ClassDefinition)
classDefinitionWithDecorators original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.ClassDefinition"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "decorators"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.ClassDefinition"),
          Core.projectionField = (Core.Name "name")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "typeParams"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.ClassDefinition"),
          Core.projectionField = (Core.Name "typeParams")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "arguments"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.ClassDefinition"),
          Core.projectionField = (Core.Name "arguments")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "body"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.ClassDefinition"),
          Core.projectionField = (Core.Name "body")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

classDefinitionWithName :: (Phantoms.TTerm Syntax.ClassDefinition -> Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.ClassDefinition)
classDefinitionWithName original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.ClassDefinition"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "decorators"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.ClassDefinition"),
          Core.projectionField = (Core.Name "decorators")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "typeParams"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.ClassDefinition"),
          Core.projectionField = (Core.Name "typeParams")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "arguments"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.ClassDefinition"),
          Core.projectionField = (Core.Name "arguments")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "body"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.ClassDefinition"),
          Core.projectionField = (Core.Name "body")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

classDefinitionWithTypeParams :: (Phantoms.TTerm Syntax.ClassDefinition -> Phantoms.TTerm [Syntax.TypeParameter] -> Phantoms.TTerm Syntax.ClassDefinition)
classDefinitionWithTypeParams original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.ClassDefinition"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "decorators"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.ClassDefinition"),
          Core.projectionField = (Core.Name "decorators")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.ClassDefinition"),
          Core.projectionField = (Core.Name "name")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "typeParams"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "arguments"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.ClassDefinition"),
          Core.projectionField = (Core.Name "arguments")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "body"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.ClassDefinition"),
          Core.projectionField = (Core.Name "body")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

classDefinitionWithArguments :: (Phantoms.TTerm Syntax.ClassDefinition -> Phantoms.TTerm (Maybe Syntax.Args) -> Phantoms.TTerm Syntax.ClassDefinition)
classDefinitionWithArguments original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.ClassDefinition"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "decorators"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.ClassDefinition"),
          Core.projectionField = (Core.Name "decorators")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.ClassDefinition"),
          Core.projectionField = (Core.Name "name")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "typeParams"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.ClassDefinition"),
          Core.projectionField = (Core.Name "typeParams")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "arguments"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "body"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.ClassDefinition"),
          Core.projectionField = (Core.Name "body")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

classDefinitionWithBody :: (Phantoms.TTerm Syntax.ClassDefinition -> Phantoms.TTerm Syntax.Block -> Phantoms.TTerm Syntax.ClassDefinition)
classDefinitionWithBody original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.ClassDefinition"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "decorators"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.ClassDefinition"),
          Core.projectionField = (Core.Name "decorators")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.ClassDefinition"),
          Core.projectionField = (Core.Name "name")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "typeParams"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.ClassDefinition"),
          Core.projectionField = (Core.Name "typeParams")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "arguments"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.ClassDefinition"),
          Core.projectionField = (Core.Name "arguments")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "body"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

functionDefinition :: (Phantoms.TTerm (Maybe Syntax.Decorators) -> Phantoms.TTerm Syntax.FunctionDefRaw -> Phantoms.TTerm Syntax.FunctionDefinition)
functionDefinition decorators raw = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.FunctionDefinition"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "decorators"),
      Core.fieldTerm = (Phantoms.unTTerm decorators)},
    Core.Field {
      Core.fieldName = (Core.Name "raw"),
      Core.fieldTerm = (Phantoms.unTTerm raw)}]})))

functionDefinitionDecorators :: (Phantoms.TTerm Syntax.FunctionDefinition -> Phantoms.TTerm (Maybe Syntax.Decorators))
functionDefinitionDecorators x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.FunctionDefinition"),
    Core.projectionField = (Core.Name "decorators")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

functionDefinitionRaw :: (Phantoms.TTerm Syntax.FunctionDefinition -> Phantoms.TTerm Syntax.FunctionDefRaw)
functionDefinitionRaw x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.FunctionDefinition"),
    Core.projectionField = (Core.Name "raw")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

functionDefinitionWithDecorators :: (Phantoms.TTerm Syntax.FunctionDefinition -> Phantoms.TTerm (Maybe Syntax.Decorators) -> Phantoms.TTerm Syntax.FunctionDefinition)
functionDefinitionWithDecorators original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.FunctionDefinition"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "decorators"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "raw"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.FunctionDefinition"),
          Core.projectionField = (Core.Name "raw")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

functionDefinitionWithRaw :: (Phantoms.TTerm Syntax.FunctionDefinition -> Phantoms.TTerm Syntax.FunctionDefRaw -> Phantoms.TTerm Syntax.FunctionDefinition)
functionDefinitionWithRaw original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.FunctionDefinition"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "decorators"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.FunctionDefinition"),
          Core.projectionField = (Core.Name "decorators")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "raw"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

functionDefRaw :: (Phantoms.TTerm Bool -> Phantoms.TTerm Syntax.Name -> Phantoms.TTerm [Syntax.TypeParameter] -> Phantoms.TTerm (Maybe Syntax.Parameters) -> Phantoms.TTerm (Maybe Syntax.Expression) -> Phantoms.TTerm (Maybe Syntax.FuncTypeComment) -> Phantoms.TTerm Syntax.Block -> Phantoms.TTerm Syntax.FunctionDefRaw)
functionDefRaw async name typeParams params returnType funcTypeComment block = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.FunctionDefRaw"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "async"),
      Core.fieldTerm = (Phantoms.unTTerm async)},
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Phantoms.unTTerm name)},
    Core.Field {
      Core.fieldName = (Core.Name "typeParams"),
      Core.fieldTerm = (Phantoms.unTTerm typeParams)},
    Core.Field {
      Core.fieldName = (Core.Name "params"),
      Core.fieldTerm = (Phantoms.unTTerm params)},
    Core.Field {
      Core.fieldName = (Core.Name "returnType"),
      Core.fieldTerm = (Phantoms.unTTerm returnType)},
    Core.Field {
      Core.fieldName = (Core.Name "funcTypeComment"),
      Core.fieldTerm = (Phantoms.unTTerm funcTypeComment)},
    Core.Field {
      Core.fieldName = (Core.Name "block"),
      Core.fieldTerm = (Phantoms.unTTerm block)}]})))

functionDefRawAsync :: (Phantoms.TTerm Syntax.FunctionDefRaw -> Phantoms.TTerm Bool)
functionDefRawAsync x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.FunctionDefRaw"),
    Core.projectionField = (Core.Name "async")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

functionDefRawName :: (Phantoms.TTerm Syntax.FunctionDefRaw -> Phantoms.TTerm Syntax.Name)
functionDefRawName x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.FunctionDefRaw"),
    Core.projectionField = (Core.Name "name")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

functionDefRawTypeParams :: (Phantoms.TTerm Syntax.FunctionDefRaw -> Phantoms.TTerm [Syntax.TypeParameter])
functionDefRawTypeParams x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.FunctionDefRaw"),
    Core.projectionField = (Core.Name "typeParams")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

functionDefRawParams :: (Phantoms.TTerm Syntax.FunctionDefRaw -> Phantoms.TTerm (Maybe Syntax.Parameters))
functionDefRawParams x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.FunctionDefRaw"),
    Core.projectionField = (Core.Name "params")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

functionDefRawReturnType :: (Phantoms.TTerm Syntax.FunctionDefRaw -> Phantoms.TTerm (Maybe Syntax.Expression))
functionDefRawReturnType x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.FunctionDefRaw"),
    Core.projectionField = (Core.Name "returnType")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

functionDefRawFuncTypeComment :: (Phantoms.TTerm Syntax.FunctionDefRaw -> Phantoms.TTerm (Maybe Syntax.FuncTypeComment))
functionDefRawFuncTypeComment x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.FunctionDefRaw"),
    Core.projectionField = (Core.Name "funcTypeComment")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

functionDefRawBlock :: (Phantoms.TTerm Syntax.FunctionDefRaw -> Phantoms.TTerm Syntax.Block)
functionDefRawBlock x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.FunctionDefRaw"),
    Core.projectionField = (Core.Name "block")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

functionDefRawWithAsync :: (Phantoms.TTerm Syntax.FunctionDefRaw -> Phantoms.TTerm Bool -> Phantoms.TTerm Syntax.FunctionDefRaw)
functionDefRawWithAsync original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.FunctionDefRaw"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "async"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.FunctionDefRaw"),
          Core.projectionField = (Core.Name "name")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "typeParams"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.FunctionDefRaw"),
          Core.projectionField = (Core.Name "typeParams")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "params"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.FunctionDefRaw"),
          Core.projectionField = (Core.Name "params")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "returnType"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.FunctionDefRaw"),
          Core.projectionField = (Core.Name "returnType")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "funcTypeComment"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.FunctionDefRaw"),
          Core.projectionField = (Core.Name "funcTypeComment")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "block"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.FunctionDefRaw"),
          Core.projectionField = (Core.Name "block")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

functionDefRawWithName :: (Phantoms.TTerm Syntax.FunctionDefRaw -> Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.FunctionDefRaw)
functionDefRawWithName original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.FunctionDefRaw"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "async"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.FunctionDefRaw"),
          Core.projectionField = (Core.Name "async")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "typeParams"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.FunctionDefRaw"),
          Core.projectionField = (Core.Name "typeParams")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "params"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.FunctionDefRaw"),
          Core.projectionField = (Core.Name "params")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "returnType"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.FunctionDefRaw"),
          Core.projectionField = (Core.Name "returnType")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "funcTypeComment"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.FunctionDefRaw"),
          Core.projectionField = (Core.Name "funcTypeComment")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "block"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.FunctionDefRaw"),
          Core.projectionField = (Core.Name "block")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

functionDefRawWithTypeParams :: (Phantoms.TTerm Syntax.FunctionDefRaw -> Phantoms.TTerm [Syntax.TypeParameter] -> Phantoms.TTerm Syntax.FunctionDefRaw)
functionDefRawWithTypeParams original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.FunctionDefRaw"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "async"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.FunctionDefRaw"),
          Core.projectionField = (Core.Name "async")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.FunctionDefRaw"),
          Core.projectionField = (Core.Name "name")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "typeParams"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "params"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.FunctionDefRaw"),
          Core.projectionField = (Core.Name "params")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "returnType"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.FunctionDefRaw"),
          Core.projectionField = (Core.Name "returnType")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "funcTypeComment"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.FunctionDefRaw"),
          Core.projectionField = (Core.Name "funcTypeComment")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "block"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.FunctionDefRaw"),
          Core.projectionField = (Core.Name "block")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

functionDefRawWithParams :: (Phantoms.TTerm Syntax.FunctionDefRaw -> Phantoms.TTerm (Maybe Syntax.Parameters) -> Phantoms.TTerm Syntax.FunctionDefRaw)
functionDefRawWithParams original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.FunctionDefRaw"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "async"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.FunctionDefRaw"),
          Core.projectionField = (Core.Name "async")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.FunctionDefRaw"),
          Core.projectionField = (Core.Name "name")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "typeParams"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.FunctionDefRaw"),
          Core.projectionField = (Core.Name "typeParams")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "params"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "returnType"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.FunctionDefRaw"),
          Core.projectionField = (Core.Name "returnType")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "funcTypeComment"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.FunctionDefRaw"),
          Core.projectionField = (Core.Name "funcTypeComment")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "block"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.FunctionDefRaw"),
          Core.projectionField = (Core.Name "block")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

functionDefRawWithReturnType :: (Phantoms.TTerm Syntax.FunctionDefRaw -> Phantoms.TTerm (Maybe Syntax.Expression) -> Phantoms.TTerm Syntax.FunctionDefRaw)
functionDefRawWithReturnType original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.FunctionDefRaw"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "async"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.FunctionDefRaw"),
          Core.projectionField = (Core.Name "async")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.FunctionDefRaw"),
          Core.projectionField = (Core.Name "name")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "typeParams"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.FunctionDefRaw"),
          Core.projectionField = (Core.Name "typeParams")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "params"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.FunctionDefRaw"),
          Core.projectionField = (Core.Name "params")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "returnType"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "funcTypeComment"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.FunctionDefRaw"),
          Core.projectionField = (Core.Name "funcTypeComment")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "block"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.FunctionDefRaw"),
          Core.projectionField = (Core.Name "block")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

functionDefRawWithFuncTypeComment :: (Phantoms.TTerm Syntax.FunctionDefRaw -> Phantoms.TTerm (Maybe Syntax.FuncTypeComment) -> Phantoms.TTerm Syntax.FunctionDefRaw)
functionDefRawWithFuncTypeComment original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.FunctionDefRaw"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "async"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.FunctionDefRaw"),
          Core.projectionField = (Core.Name "async")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.FunctionDefRaw"),
          Core.projectionField = (Core.Name "name")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "typeParams"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.FunctionDefRaw"),
          Core.projectionField = (Core.Name "typeParams")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "params"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.FunctionDefRaw"),
          Core.projectionField = (Core.Name "params")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "returnType"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.FunctionDefRaw"),
          Core.projectionField = (Core.Name "returnType")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "funcTypeComment"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "block"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.FunctionDefRaw"),
          Core.projectionField = (Core.Name "block")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

functionDefRawWithBlock :: (Phantoms.TTerm Syntax.FunctionDefRaw -> Phantoms.TTerm Syntax.Block -> Phantoms.TTerm Syntax.FunctionDefRaw)
functionDefRawWithBlock original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.FunctionDefRaw"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "async"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.FunctionDefRaw"),
          Core.projectionField = (Core.Name "async")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.FunctionDefRaw"),
          Core.projectionField = (Core.Name "name")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "typeParams"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.FunctionDefRaw"),
          Core.projectionField = (Core.Name "typeParams")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "params"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.FunctionDefRaw"),
          Core.projectionField = (Core.Name "params")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "returnType"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.FunctionDefRaw"),
          Core.projectionField = (Core.Name "returnType")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "funcTypeComment"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.FunctionDefRaw"),
          Core.projectionField = (Core.Name "funcTypeComment")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "block"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

parametersSlashNoDefault :: (Phantoms.TTerm Syntax.SlashNoDefaultParameters -> Phantoms.TTerm Syntax.Parameters)
parametersSlashNoDefault x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.Parameters"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "slashNoDefault"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

parametersSlashWithDefault :: (Phantoms.TTerm Syntax.SlashWithDefaultParameters -> Phantoms.TTerm Syntax.Parameters)
parametersSlashWithDefault x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.Parameters"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "slashWithDefault"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

parametersParamNoDefault :: (Phantoms.TTerm Syntax.ParamNoDefaultParameters -> Phantoms.TTerm Syntax.Parameters)
parametersParamNoDefault x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.Parameters"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "paramNoDefault"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

parametersParamWithDefault :: (Phantoms.TTerm Syntax.ParamWithDefaultParameters -> Phantoms.TTerm Syntax.Parameters)
parametersParamWithDefault x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.Parameters"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "paramWithDefault"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

parametersStarEtc :: (Phantoms.TTerm Syntax.StarEtc -> Phantoms.TTerm Syntax.Parameters)
parametersStarEtc x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.Parameters"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "starEtc"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

slashNoDefaultParameters :: (Phantoms.TTerm Syntax.SlashNoDefault -> Phantoms.TTerm [Syntax.ParamNoDefault] -> Phantoms.TTerm [Syntax.ParamWithDefault] -> Phantoms.TTerm (Maybe Syntax.StarEtc) -> Phantoms.TTerm Syntax.SlashNoDefaultParameters)
slashNoDefaultParameters slash paramNoDefault paramWithDefault starEtc = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.SlashNoDefaultParameters"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "slash"),
      Core.fieldTerm = (Phantoms.unTTerm slash)},
    Core.Field {
      Core.fieldName = (Core.Name "paramNoDefault"),
      Core.fieldTerm = (Phantoms.unTTerm paramNoDefault)},
    Core.Field {
      Core.fieldName = (Core.Name "paramWithDefault"),
      Core.fieldTerm = (Phantoms.unTTerm paramWithDefault)},
    Core.Field {
      Core.fieldName = (Core.Name "starEtc"),
      Core.fieldTerm = (Phantoms.unTTerm starEtc)}]})))

slashNoDefaultParametersSlash :: (Phantoms.TTerm Syntax.SlashNoDefaultParameters -> Phantoms.TTerm Syntax.SlashNoDefault)
slashNoDefaultParametersSlash x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.SlashNoDefaultParameters"),
    Core.projectionField = (Core.Name "slash")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

slashNoDefaultParametersParamNoDefault :: (Phantoms.TTerm Syntax.SlashNoDefaultParameters -> Phantoms.TTerm [Syntax.ParamNoDefault])
slashNoDefaultParametersParamNoDefault x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.SlashNoDefaultParameters"),
    Core.projectionField = (Core.Name "paramNoDefault")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

slashNoDefaultParametersParamWithDefault :: (Phantoms.TTerm Syntax.SlashNoDefaultParameters -> Phantoms.TTerm [Syntax.ParamWithDefault])
slashNoDefaultParametersParamWithDefault x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.SlashNoDefaultParameters"),
    Core.projectionField = (Core.Name "paramWithDefault")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

slashNoDefaultParametersStarEtc :: (Phantoms.TTerm Syntax.SlashNoDefaultParameters -> Phantoms.TTerm (Maybe Syntax.StarEtc))
slashNoDefaultParametersStarEtc x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.SlashNoDefaultParameters"),
    Core.projectionField = (Core.Name "starEtc")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

slashNoDefaultParametersWithSlash :: (Phantoms.TTerm Syntax.SlashNoDefaultParameters -> Phantoms.TTerm Syntax.SlashNoDefault -> Phantoms.TTerm Syntax.SlashNoDefaultParameters)
slashNoDefaultParametersWithSlash original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.SlashNoDefaultParameters"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "slash"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "paramNoDefault"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.SlashNoDefaultParameters"),
          Core.projectionField = (Core.Name "paramNoDefault")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "paramWithDefault"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.SlashNoDefaultParameters"),
          Core.projectionField = (Core.Name "paramWithDefault")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "starEtc"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.SlashNoDefaultParameters"),
          Core.projectionField = (Core.Name "starEtc")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

slashNoDefaultParametersWithParamNoDefault :: (Phantoms.TTerm Syntax.SlashNoDefaultParameters -> Phantoms.TTerm [Syntax.ParamNoDefault] -> Phantoms.TTerm Syntax.SlashNoDefaultParameters)
slashNoDefaultParametersWithParamNoDefault original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.SlashNoDefaultParameters"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "slash"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.SlashNoDefaultParameters"),
          Core.projectionField = (Core.Name "slash")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "paramNoDefault"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "paramWithDefault"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.SlashNoDefaultParameters"),
          Core.projectionField = (Core.Name "paramWithDefault")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "starEtc"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.SlashNoDefaultParameters"),
          Core.projectionField = (Core.Name "starEtc")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

slashNoDefaultParametersWithParamWithDefault :: (Phantoms.TTerm Syntax.SlashNoDefaultParameters -> Phantoms.TTerm [Syntax.ParamWithDefault] -> Phantoms.TTerm Syntax.SlashNoDefaultParameters)
slashNoDefaultParametersWithParamWithDefault original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.SlashNoDefaultParameters"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "slash"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.SlashNoDefaultParameters"),
          Core.projectionField = (Core.Name "slash")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "paramNoDefault"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.SlashNoDefaultParameters"),
          Core.projectionField = (Core.Name "paramNoDefault")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "paramWithDefault"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "starEtc"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.SlashNoDefaultParameters"),
          Core.projectionField = (Core.Name "starEtc")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

slashNoDefaultParametersWithStarEtc :: (Phantoms.TTerm Syntax.SlashNoDefaultParameters -> Phantoms.TTerm (Maybe Syntax.StarEtc) -> Phantoms.TTerm Syntax.SlashNoDefaultParameters)
slashNoDefaultParametersWithStarEtc original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.SlashNoDefaultParameters"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "slash"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.SlashNoDefaultParameters"),
          Core.projectionField = (Core.Name "slash")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "paramNoDefault"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.SlashNoDefaultParameters"),
          Core.projectionField = (Core.Name "paramNoDefault")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "paramWithDefault"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.SlashNoDefaultParameters"),
          Core.projectionField = (Core.Name "paramWithDefault")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "starEtc"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

slashWithDefaultParameters :: (Phantoms.TTerm [Syntax.ParamNoDefault] -> Phantoms.TTerm [Syntax.ParamWithDefault] -> Phantoms.TTerm (Maybe Syntax.StarEtc) -> Phantoms.TTerm Syntax.SlashWithDefaultParameters)
slashWithDefaultParameters paramNoDefault paramWithDefault starEtc = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.SlashWithDefaultParameters"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "paramNoDefault"),
      Core.fieldTerm = (Phantoms.unTTerm paramNoDefault)},
    Core.Field {
      Core.fieldName = (Core.Name "paramWithDefault"),
      Core.fieldTerm = (Phantoms.unTTerm paramWithDefault)},
    Core.Field {
      Core.fieldName = (Core.Name "starEtc"),
      Core.fieldTerm = (Phantoms.unTTerm starEtc)}]})))

slashWithDefaultParametersParamNoDefault :: (Phantoms.TTerm Syntax.SlashWithDefaultParameters -> Phantoms.TTerm [Syntax.ParamNoDefault])
slashWithDefaultParametersParamNoDefault x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.SlashWithDefaultParameters"),
    Core.projectionField = (Core.Name "paramNoDefault")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

slashWithDefaultParametersParamWithDefault :: (Phantoms.TTerm Syntax.SlashWithDefaultParameters -> Phantoms.TTerm [Syntax.ParamWithDefault])
slashWithDefaultParametersParamWithDefault x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.SlashWithDefaultParameters"),
    Core.projectionField = (Core.Name "paramWithDefault")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

slashWithDefaultParametersStarEtc :: (Phantoms.TTerm Syntax.SlashWithDefaultParameters -> Phantoms.TTerm (Maybe Syntax.StarEtc))
slashWithDefaultParametersStarEtc x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.SlashWithDefaultParameters"),
    Core.projectionField = (Core.Name "starEtc")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

slashWithDefaultParametersWithParamNoDefault :: (Phantoms.TTerm Syntax.SlashWithDefaultParameters -> Phantoms.TTerm [Syntax.ParamNoDefault] -> Phantoms.TTerm Syntax.SlashWithDefaultParameters)
slashWithDefaultParametersWithParamNoDefault original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.SlashWithDefaultParameters"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "paramNoDefault"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "paramWithDefault"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.SlashWithDefaultParameters"),
          Core.projectionField = (Core.Name "paramWithDefault")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "starEtc"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.SlashWithDefaultParameters"),
          Core.projectionField = (Core.Name "starEtc")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

slashWithDefaultParametersWithParamWithDefault :: (Phantoms.TTerm Syntax.SlashWithDefaultParameters -> Phantoms.TTerm [Syntax.ParamWithDefault] -> Phantoms.TTerm Syntax.SlashWithDefaultParameters)
slashWithDefaultParametersWithParamWithDefault original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.SlashWithDefaultParameters"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "paramNoDefault"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.SlashWithDefaultParameters"),
          Core.projectionField = (Core.Name "paramNoDefault")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "paramWithDefault"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "starEtc"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.SlashWithDefaultParameters"),
          Core.projectionField = (Core.Name "starEtc")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

slashWithDefaultParametersWithStarEtc :: (Phantoms.TTerm Syntax.SlashWithDefaultParameters -> Phantoms.TTerm (Maybe Syntax.StarEtc) -> Phantoms.TTerm Syntax.SlashWithDefaultParameters)
slashWithDefaultParametersWithStarEtc original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.SlashWithDefaultParameters"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "paramNoDefault"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.SlashWithDefaultParameters"),
          Core.projectionField = (Core.Name "paramNoDefault")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "paramWithDefault"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.SlashWithDefaultParameters"),
          Core.projectionField = (Core.Name "paramWithDefault")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "starEtc"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

paramNoDefaultParameters :: (Phantoms.TTerm [Syntax.ParamNoDefault] -> Phantoms.TTerm [Syntax.ParamWithDefault] -> Phantoms.TTerm (Maybe Syntax.StarEtc) -> Phantoms.TTerm Syntax.ParamNoDefaultParameters)
paramNoDefaultParameters paramNoDefault paramWithDefault starEtc = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.ParamNoDefaultParameters"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "paramNoDefault"),
      Core.fieldTerm = (Phantoms.unTTerm paramNoDefault)},
    Core.Field {
      Core.fieldName = (Core.Name "paramWithDefault"),
      Core.fieldTerm = (Phantoms.unTTerm paramWithDefault)},
    Core.Field {
      Core.fieldName = (Core.Name "starEtc"),
      Core.fieldTerm = (Phantoms.unTTerm starEtc)}]})))

paramNoDefaultParametersParamNoDefault :: (Phantoms.TTerm Syntax.ParamNoDefaultParameters -> Phantoms.TTerm [Syntax.ParamNoDefault])
paramNoDefaultParametersParamNoDefault x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.ParamNoDefaultParameters"),
    Core.projectionField = (Core.Name "paramNoDefault")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

paramNoDefaultParametersParamWithDefault :: (Phantoms.TTerm Syntax.ParamNoDefaultParameters -> Phantoms.TTerm [Syntax.ParamWithDefault])
paramNoDefaultParametersParamWithDefault x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.ParamNoDefaultParameters"),
    Core.projectionField = (Core.Name "paramWithDefault")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

paramNoDefaultParametersStarEtc :: (Phantoms.TTerm Syntax.ParamNoDefaultParameters -> Phantoms.TTerm (Maybe Syntax.StarEtc))
paramNoDefaultParametersStarEtc x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.ParamNoDefaultParameters"),
    Core.projectionField = (Core.Name "starEtc")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

paramNoDefaultParametersWithParamNoDefault :: (Phantoms.TTerm Syntax.ParamNoDefaultParameters -> Phantoms.TTerm [Syntax.ParamNoDefault] -> Phantoms.TTerm Syntax.ParamNoDefaultParameters)
paramNoDefaultParametersWithParamNoDefault original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.ParamNoDefaultParameters"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "paramNoDefault"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "paramWithDefault"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.ParamNoDefaultParameters"),
          Core.projectionField = (Core.Name "paramWithDefault")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "starEtc"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.ParamNoDefaultParameters"),
          Core.projectionField = (Core.Name "starEtc")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

paramNoDefaultParametersWithParamWithDefault :: (Phantoms.TTerm Syntax.ParamNoDefaultParameters -> Phantoms.TTerm [Syntax.ParamWithDefault] -> Phantoms.TTerm Syntax.ParamNoDefaultParameters)
paramNoDefaultParametersWithParamWithDefault original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.ParamNoDefaultParameters"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "paramNoDefault"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.ParamNoDefaultParameters"),
          Core.projectionField = (Core.Name "paramNoDefault")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "paramWithDefault"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "starEtc"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.ParamNoDefaultParameters"),
          Core.projectionField = (Core.Name "starEtc")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

paramNoDefaultParametersWithStarEtc :: (Phantoms.TTerm Syntax.ParamNoDefaultParameters -> Phantoms.TTerm (Maybe Syntax.StarEtc) -> Phantoms.TTerm Syntax.ParamNoDefaultParameters)
paramNoDefaultParametersWithStarEtc original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.ParamNoDefaultParameters"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "paramNoDefault"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.ParamNoDefaultParameters"),
          Core.projectionField = (Core.Name "paramNoDefault")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "paramWithDefault"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.ParamNoDefaultParameters"),
          Core.projectionField = (Core.Name "paramWithDefault")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "starEtc"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

paramWithDefaultParameters :: (Phantoms.TTerm [Syntax.ParamWithDefault] -> Phantoms.TTerm (Maybe Syntax.StarEtc) -> Phantoms.TTerm Syntax.ParamWithDefaultParameters)
paramWithDefaultParameters paramWithDefault starEtc = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.ParamWithDefaultParameters"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "paramWithDefault"),
      Core.fieldTerm = (Phantoms.unTTerm paramWithDefault)},
    Core.Field {
      Core.fieldName = (Core.Name "starEtc"),
      Core.fieldTerm = (Phantoms.unTTerm starEtc)}]})))

paramWithDefaultParametersParamWithDefault :: (Phantoms.TTerm Syntax.ParamWithDefaultParameters -> Phantoms.TTerm [Syntax.ParamWithDefault])
paramWithDefaultParametersParamWithDefault x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.ParamWithDefaultParameters"),
    Core.projectionField = (Core.Name "paramWithDefault")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

paramWithDefaultParametersStarEtc :: (Phantoms.TTerm Syntax.ParamWithDefaultParameters -> Phantoms.TTerm (Maybe Syntax.StarEtc))
paramWithDefaultParametersStarEtc x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.ParamWithDefaultParameters"),
    Core.projectionField = (Core.Name "starEtc")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

paramWithDefaultParametersWithParamWithDefault :: (Phantoms.TTerm Syntax.ParamWithDefaultParameters -> Phantoms.TTerm [Syntax.ParamWithDefault] -> Phantoms.TTerm Syntax.ParamWithDefaultParameters)
paramWithDefaultParametersWithParamWithDefault original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.ParamWithDefaultParameters"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "paramWithDefault"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "starEtc"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.ParamWithDefaultParameters"),
          Core.projectionField = (Core.Name "starEtc")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

paramWithDefaultParametersWithStarEtc :: (Phantoms.TTerm Syntax.ParamWithDefaultParameters -> Phantoms.TTerm (Maybe Syntax.StarEtc) -> Phantoms.TTerm Syntax.ParamWithDefaultParameters)
paramWithDefaultParametersWithStarEtc original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.ParamWithDefaultParameters"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "paramWithDefault"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.ParamWithDefaultParameters"),
          Core.projectionField = (Core.Name "paramWithDefault")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "starEtc"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

slashNoDefault :: (Phantoms.TTerm [Syntax.ParamNoDefault] -> Phantoms.TTerm Syntax.SlashNoDefault)
slashNoDefault x = (Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
  Core.wrappedTermTypeName = (Core.Name "hydra.ext.python.syntax.SlashNoDefault"),
  Core.wrappedTermBody = (Phantoms.unTTerm x)})))

unSlashNoDefault :: (Phantoms.TTerm Syntax.SlashNoDefault -> Phantoms.TTerm [Syntax.ParamNoDefault])
unSlashNoDefault x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.python.syntax.SlashNoDefault")))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

slashWithDefault :: (Phantoms.TTerm [Syntax.ParamNoDefault] -> Phantoms.TTerm [Syntax.ParamWithDefault] -> Phantoms.TTerm Syntax.SlashWithDefault)
slashWithDefault paramNoDefault paramWithDefault = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.SlashWithDefault"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "paramNoDefault"),
      Core.fieldTerm = (Phantoms.unTTerm paramNoDefault)},
    Core.Field {
      Core.fieldName = (Core.Name "paramWithDefault"),
      Core.fieldTerm = (Phantoms.unTTerm paramWithDefault)}]})))

slashWithDefaultParamNoDefault :: (Phantoms.TTerm Syntax.SlashWithDefault -> Phantoms.TTerm [Syntax.ParamNoDefault])
slashWithDefaultParamNoDefault x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.SlashWithDefault"),
    Core.projectionField = (Core.Name "paramNoDefault")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

slashWithDefaultParamWithDefault :: (Phantoms.TTerm Syntax.SlashWithDefault -> Phantoms.TTerm [Syntax.ParamWithDefault])
slashWithDefaultParamWithDefault x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.SlashWithDefault"),
    Core.projectionField = (Core.Name "paramWithDefault")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

slashWithDefaultWithParamNoDefault :: (Phantoms.TTerm Syntax.SlashWithDefault -> Phantoms.TTerm [Syntax.ParamNoDefault] -> Phantoms.TTerm Syntax.SlashWithDefault)
slashWithDefaultWithParamNoDefault original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.SlashWithDefault"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "paramNoDefault"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "paramWithDefault"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.SlashWithDefault"),
          Core.projectionField = (Core.Name "paramWithDefault")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

slashWithDefaultWithParamWithDefault :: (Phantoms.TTerm Syntax.SlashWithDefault -> Phantoms.TTerm [Syntax.ParamWithDefault] -> Phantoms.TTerm Syntax.SlashWithDefault)
slashWithDefaultWithParamWithDefault original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.SlashWithDefault"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "paramNoDefault"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.SlashWithDefault"),
          Core.projectionField = (Core.Name "paramNoDefault")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "paramWithDefault"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

starEtcStarNoDefault :: (Phantoms.TTerm Syntax.NoDefaultStarEtc -> Phantoms.TTerm Syntax.StarEtc)
starEtcStarNoDefault x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.StarEtc"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "starNoDefault"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

starEtcStarNoDefaultStarAnnotation :: (Phantoms.TTerm Syntax.NoDefaultStarAnnotationStarEtc -> Phantoms.TTerm Syntax.StarEtc)
starEtcStarNoDefaultStarAnnotation x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.StarEtc"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "starNoDefaultStarAnnotation"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

starEtcStarComma :: (Phantoms.TTerm Syntax.CommaStarEtc -> Phantoms.TTerm Syntax.StarEtc)
starEtcStarComma x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.StarEtc"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "starComma"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

starEtcKeywords :: (Phantoms.TTerm Syntax.Keywords -> Phantoms.TTerm Syntax.StarEtc)
starEtcKeywords x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.StarEtc"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "keywords"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

noDefaultStarEtc :: (Phantoms.TTerm Syntax.ParamNoDefault -> Phantoms.TTerm [Syntax.ParamMaybeDefault] -> Phantoms.TTerm (Maybe Syntax.Keywords) -> Phantoms.TTerm Syntax.NoDefaultStarEtc)
noDefaultStarEtc paramNoDefault paramMaybeDefault keywords = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.NoDefaultStarEtc"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "paramNoDefault"),
      Core.fieldTerm = (Phantoms.unTTerm paramNoDefault)},
    Core.Field {
      Core.fieldName = (Core.Name "paramMaybeDefault"),
      Core.fieldTerm = (Phantoms.unTTerm paramMaybeDefault)},
    Core.Field {
      Core.fieldName = (Core.Name "keywords"),
      Core.fieldTerm = (Phantoms.unTTerm keywords)}]})))

noDefaultStarEtcParamNoDefault :: (Phantoms.TTerm Syntax.NoDefaultStarEtc -> Phantoms.TTerm Syntax.ParamNoDefault)
noDefaultStarEtcParamNoDefault x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.NoDefaultStarEtc"),
    Core.projectionField = (Core.Name "paramNoDefault")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

noDefaultStarEtcParamMaybeDefault :: (Phantoms.TTerm Syntax.NoDefaultStarEtc -> Phantoms.TTerm [Syntax.ParamMaybeDefault])
noDefaultStarEtcParamMaybeDefault x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.NoDefaultStarEtc"),
    Core.projectionField = (Core.Name "paramMaybeDefault")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

noDefaultStarEtcKeywords :: (Phantoms.TTerm Syntax.NoDefaultStarEtc -> Phantoms.TTerm (Maybe Syntax.Keywords))
noDefaultStarEtcKeywords x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.NoDefaultStarEtc"),
    Core.projectionField = (Core.Name "keywords")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

noDefaultStarEtcWithParamNoDefault :: (Phantoms.TTerm Syntax.NoDefaultStarEtc -> Phantoms.TTerm Syntax.ParamNoDefault -> Phantoms.TTerm Syntax.NoDefaultStarEtc)
noDefaultStarEtcWithParamNoDefault original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.NoDefaultStarEtc"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "paramNoDefault"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "paramMaybeDefault"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.NoDefaultStarEtc"),
          Core.projectionField = (Core.Name "paramMaybeDefault")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "keywords"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.NoDefaultStarEtc"),
          Core.projectionField = (Core.Name "keywords")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

noDefaultStarEtcWithParamMaybeDefault :: (Phantoms.TTerm Syntax.NoDefaultStarEtc -> Phantoms.TTerm [Syntax.ParamMaybeDefault] -> Phantoms.TTerm Syntax.NoDefaultStarEtc)
noDefaultStarEtcWithParamMaybeDefault original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.NoDefaultStarEtc"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "paramNoDefault"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.NoDefaultStarEtc"),
          Core.projectionField = (Core.Name "paramNoDefault")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "paramMaybeDefault"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "keywords"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.NoDefaultStarEtc"),
          Core.projectionField = (Core.Name "keywords")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

noDefaultStarEtcWithKeywords :: (Phantoms.TTerm Syntax.NoDefaultStarEtc -> Phantoms.TTerm (Maybe Syntax.Keywords) -> Phantoms.TTerm Syntax.NoDefaultStarEtc)
noDefaultStarEtcWithKeywords original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.NoDefaultStarEtc"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "paramNoDefault"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.NoDefaultStarEtc"),
          Core.projectionField = (Core.Name "paramNoDefault")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "paramMaybeDefault"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.NoDefaultStarEtc"),
          Core.projectionField = (Core.Name "paramMaybeDefault")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "keywords"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

noDefaultStarAnnotationStarEtc :: (Phantoms.TTerm Syntax.ParamNoDefaultStarAnnotation -> Phantoms.TTerm [Syntax.ParamMaybeDefault] -> Phantoms.TTerm (Maybe Syntax.Keywords) -> Phantoms.TTerm Syntax.NoDefaultStarAnnotationStarEtc)
noDefaultStarAnnotationStarEtc paramNoDefaultStarAnnotation paramMaybeDefault keywords = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.NoDefaultStarAnnotationStarEtc"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "paramNoDefaultStarAnnotation"),
      Core.fieldTerm = (Phantoms.unTTerm paramNoDefaultStarAnnotation)},
    Core.Field {
      Core.fieldName = (Core.Name "paramMaybeDefault"),
      Core.fieldTerm = (Phantoms.unTTerm paramMaybeDefault)},
    Core.Field {
      Core.fieldName = (Core.Name "keywords"),
      Core.fieldTerm = (Phantoms.unTTerm keywords)}]})))

noDefaultStarAnnotationStarEtcParamNoDefaultStarAnnotation :: (Phantoms.TTerm Syntax.NoDefaultStarAnnotationStarEtc -> Phantoms.TTerm Syntax.ParamNoDefaultStarAnnotation)
noDefaultStarAnnotationStarEtcParamNoDefaultStarAnnotation x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.NoDefaultStarAnnotationStarEtc"),
    Core.projectionField = (Core.Name "paramNoDefaultStarAnnotation")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

noDefaultStarAnnotationStarEtcParamMaybeDefault :: (Phantoms.TTerm Syntax.NoDefaultStarAnnotationStarEtc -> Phantoms.TTerm [Syntax.ParamMaybeDefault])
noDefaultStarAnnotationStarEtcParamMaybeDefault x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.NoDefaultStarAnnotationStarEtc"),
    Core.projectionField = (Core.Name "paramMaybeDefault")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

noDefaultStarAnnotationStarEtcKeywords :: (Phantoms.TTerm Syntax.NoDefaultStarAnnotationStarEtc -> Phantoms.TTerm (Maybe Syntax.Keywords))
noDefaultStarAnnotationStarEtcKeywords x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.NoDefaultStarAnnotationStarEtc"),
    Core.projectionField = (Core.Name "keywords")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

noDefaultStarAnnotationStarEtcWithParamNoDefaultStarAnnotation :: (Phantoms.TTerm Syntax.NoDefaultStarAnnotationStarEtc -> Phantoms.TTerm Syntax.ParamNoDefaultStarAnnotation -> Phantoms.TTerm Syntax.NoDefaultStarAnnotationStarEtc)
noDefaultStarAnnotationStarEtcWithParamNoDefaultStarAnnotation original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.NoDefaultStarAnnotationStarEtc"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "paramNoDefaultStarAnnotation"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "paramMaybeDefault"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.NoDefaultStarAnnotationStarEtc"),
          Core.projectionField = (Core.Name "paramMaybeDefault")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "keywords"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.NoDefaultStarAnnotationStarEtc"),
          Core.projectionField = (Core.Name "keywords")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

noDefaultStarAnnotationStarEtcWithParamMaybeDefault :: (Phantoms.TTerm Syntax.NoDefaultStarAnnotationStarEtc -> Phantoms.TTerm [Syntax.ParamMaybeDefault] -> Phantoms.TTerm Syntax.NoDefaultStarAnnotationStarEtc)
noDefaultStarAnnotationStarEtcWithParamMaybeDefault original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.NoDefaultStarAnnotationStarEtc"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "paramNoDefaultStarAnnotation"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.NoDefaultStarAnnotationStarEtc"),
          Core.projectionField = (Core.Name "paramNoDefaultStarAnnotation")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "paramMaybeDefault"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "keywords"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.NoDefaultStarAnnotationStarEtc"),
          Core.projectionField = (Core.Name "keywords")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

noDefaultStarAnnotationStarEtcWithKeywords :: (Phantoms.TTerm Syntax.NoDefaultStarAnnotationStarEtc -> Phantoms.TTerm (Maybe Syntax.Keywords) -> Phantoms.TTerm Syntax.NoDefaultStarAnnotationStarEtc)
noDefaultStarAnnotationStarEtcWithKeywords original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.NoDefaultStarAnnotationStarEtc"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "paramNoDefaultStarAnnotation"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.NoDefaultStarAnnotationStarEtc"),
          Core.projectionField = (Core.Name "paramNoDefaultStarAnnotation")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "paramMaybeDefault"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.NoDefaultStarAnnotationStarEtc"),
          Core.projectionField = (Core.Name "paramMaybeDefault")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "keywords"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

commaStarEtc :: (Phantoms.TTerm [Syntax.ParamMaybeDefault] -> Phantoms.TTerm (Maybe Syntax.Keywords) -> Phantoms.TTerm Syntax.CommaStarEtc)
commaStarEtc paramMaybeDefault keywords = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.CommaStarEtc"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "paramMaybeDefault"),
      Core.fieldTerm = (Phantoms.unTTerm paramMaybeDefault)},
    Core.Field {
      Core.fieldName = (Core.Name "keywords"),
      Core.fieldTerm = (Phantoms.unTTerm keywords)}]})))

commaStarEtcParamMaybeDefault :: (Phantoms.TTerm Syntax.CommaStarEtc -> Phantoms.TTerm [Syntax.ParamMaybeDefault])
commaStarEtcParamMaybeDefault x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.CommaStarEtc"),
    Core.projectionField = (Core.Name "paramMaybeDefault")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

commaStarEtcKeywords :: (Phantoms.TTerm Syntax.CommaStarEtc -> Phantoms.TTerm (Maybe Syntax.Keywords))
commaStarEtcKeywords x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.CommaStarEtc"),
    Core.projectionField = (Core.Name "keywords")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

commaStarEtcWithParamMaybeDefault :: (Phantoms.TTerm Syntax.CommaStarEtc -> Phantoms.TTerm [Syntax.ParamMaybeDefault] -> Phantoms.TTerm Syntax.CommaStarEtc)
commaStarEtcWithParamMaybeDefault original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.CommaStarEtc"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "paramMaybeDefault"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "keywords"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.CommaStarEtc"),
          Core.projectionField = (Core.Name "keywords")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

commaStarEtcWithKeywords :: (Phantoms.TTerm Syntax.CommaStarEtc -> Phantoms.TTerm (Maybe Syntax.Keywords) -> Phantoms.TTerm Syntax.CommaStarEtc)
commaStarEtcWithKeywords original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.CommaStarEtc"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "paramMaybeDefault"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.CommaStarEtc"),
          Core.projectionField = (Core.Name "paramMaybeDefault")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "keywords"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

keywords :: (Phantoms.TTerm Syntax.ParamNoDefault -> Phantoms.TTerm Syntax.Keywords)
keywords x = (Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
  Core.wrappedTermTypeName = (Core.Name "hydra.ext.python.syntax.Keywords"),
  Core.wrappedTermBody = (Phantoms.unTTerm x)})))

unKeywords :: (Phantoms.TTerm Syntax.Keywords -> Phantoms.TTerm Syntax.ParamNoDefault)
unKeywords x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.python.syntax.Keywords")))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

paramNoDefault :: (Phantoms.TTerm Syntax.Param -> Phantoms.TTerm (Maybe Syntax.TypeComment) -> Phantoms.TTerm Syntax.ParamNoDefault)
paramNoDefault param typeComment = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.ParamNoDefault"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "param"),
      Core.fieldTerm = (Phantoms.unTTerm param)},
    Core.Field {
      Core.fieldName = (Core.Name "typeComment"),
      Core.fieldTerm = (Phantoms.unTTerm typeComment)}]})))

paramNoDefaultParam :: (Phantoms.TTerm Syntax.ParamNoDefault -> Phantoms.TTerm Syntax.Param)
paramNoDefaultParam x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.ParamNoDefault"),
    Core.projectionField = (Core.Name "param")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

paramNoDefaultTypeComment :: (Phantoms.TTerm Syntax.ParamNoDefault -> Phantoms.TTerm (Maybe Syntax.TypeComment))
paramNoDefaultTypeComment x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.ParamNoDefault"),
    Core.projectionField = (Core.Name "typeComment")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

paramNoDefaultWithParam :: (Phantoms.TTerm Syntax.ParamNoDefault -> Phantoms.TTerm Syntax.Param -> Phantoms.TTerm Syntax.ParamNoDefault)
paramNoDefaultWithParam original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.ParamNoDefault"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "param"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "typeComment"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.ParamNoDefault"),
          Core.projectionField = (Core.Name "typeComment")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

paramNoDefaultWithTypeComment :: (Phantoms.TTerm Syntax.ParamNoDefault -> Phantoms.TTerm (Maybe Syntax.TypeComment) -> Phantoms.TTerm Syntax.ParamNoDefault)
paramNoDefaultWithTypeComment original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.ParamNoDefault"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "param"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.ParamNoDefault"),
          Core.projectionField = (Core.Name "param")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "typeComment"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

paramNoDefaultStarAnnotation :: (Phantoms.TTerm Syntax.ParamStarAnnotation -> Phantoms.TTerm (Maybe Syntax.TypeComment) -> Phantoms.TTerm Syntax.ParamNoDefaultStarAnnotation)
paramNoDefaultStarAnnotation paramStarAnnotation typeComment = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.ParamNoDefaultStarAnnotation"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "paramStarAnnotation"),
      Core.fieldTerm = (Phantoms.unTTerm paramStarAnnotation)},
    Core.Field {
      Core.fieldName = (Core.Name "typeComment"),
      Core.fieldTerm = (Phantoms.unTTerm typeComment)}]})))

paramNoDefaultStarAnnotationParamStarAnnotation :: (Phantoms.TTerm Syntax.ParamNoDefaultStarAnnotation -> Phantoms.TTerm Syntax.ParamStarAnnotation)
paramNoDefaultStarAnnotationParamStarAnnotation x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.ParamNoDefaultStarAnnotation"),
    Core.projectionField = (Core.Name "paramStarAnnotation")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

paramNoDefaultStarAnnotationTypeComment :: (Phantoms.TTerm Syntax.ParamNoDefaultStarAnnotation -> Phantoms.TTerm (Maybe Syntax.TypeComment))
paramNoDefaultStarAnnotationTypeComment x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.ParamNoDefaultStarAnnotation"),
    Core.projectionField = (Core.Name "typeComment")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

paramNoDefaultStarAnnotationWithParamStarAnnotation :: (Phantoms.TTerm Syntax.ParamNoDefaultStarAnnotation -> Phantoms.TTerm Syntax.ParamStarAnnotation -> Phantoms.TTerm Syntax.ParamNoDefaultStarAnnotation)
paramNoDefaultStarAnnotationWithParamStarAnnotation original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.ParamNoDefaultStarAnnotation"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "paramStarAnnotation"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "typeComment"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.ParamNoDefaultStarAnnotation"),
          Core.projectionField = (Core.Name "typeComment")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

paramNoDefaultStarAnnotationWithTypeComment :: (Phantoms.TTerm Syntax.ParamNoDefaultStarAnnotation -> Phantoms.TTerm (Maybe Syntax.TypeComment) -> Phantoms.TTerm Syntax.ParamNoDefaultStarAnnotation)
paramNoDefaultStarAnnotationWithTypeComment original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.ParamNoDefaultStarAnnotation"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "paramStarAnnotation"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.ParamNoDefaultStarAnnotation"),
          Core.projectionField = (Core.Name "paramStarAnnotation")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "typeComment"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

paramWithDefault :: (Phantoms.TTerm Syntax.Param -> Phantoms.TTerm Syntax.Default -> Phantoms.TTerm (Maybe Syntax.TypeComment) -> Phantoms.TTerm Syntax.ParamWithDefault)
paramWithDefault param default_ typeComment = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.ParamWithDefault"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "param"),
      Core.fieldTerm = (Phantoms.unTTerm param)},
    Core.Field {
      Core.fieldName = (Core.Name "default"),
      Core.fieldTerm = (Phantoms.unTTerm default_)},
    Core.Field {
      Core.fieldName = (Core.Name "typeComment"),
      Core.fieldTerm = (Phantoms.unTTerm typeComment)}]})))

paramWithDefaultParam :: (Phantoms.TTerm Syntax.ParamWithDefault -> Phantoms.TTerm Syntax.Param)
paramWithDefaultParam x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.ParamWithDefault"),
    Core.projectionField = (Core.Name "param")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

paramWithDefaultDefault :: (Phantoms.TTerm Syntax.ParamWithDefault -> Phantoms.TTerm Syntax.Default)
paramWithDefaultDefault x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.ParamWithDefault"),
    Core.projectionField = (Core.Name "default")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

paramWithDefaultTypeComment :: (Phantoms.TTerm Syntax.ParamWithDefault -> Phantoms.TTerm (Maybe Syntax.TypeComment))
paramWithDefaultTypeComment x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.ParamWithDefault"),
    Core.projectionField = (Core.Name "typeComment")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

paramWithDefaultWithParam :: (Phantoms.TTerm Syntax.ParamWithDefault -> Phantoms.TTerm Syntax.Param -> Phantoms.TTerm Syntax.ParamWithDefault)
paramWithDefaultWithParam original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.ParamWithDefault"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "param"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "default"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.ParamWithDefault"),
          Core.projectionField = (Core.Name "default")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "typeComment"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.ParamWithDefault"),
          Core.projectionField = (Core.Name "typeComment")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

paramWithDefaultWithDefault :: (Phantoms.TTerm Syntax.ParamWithDefault -> Phantoms.TTerm Syntax.Default -> Phantoms.TTerm Syntax.ParamWithDefault)
paramWithDefaultWithDefault original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.ParamWithDefault"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "param"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.ParamWithDefault"),
          Core.projectionField = (Core.Name "param")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "default"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "typeComment"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.ParamWithDefault"),
          Core.projectionField = (Core.Name "typeComment")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

paramWithDefaultWithTypeComment :: (Phantoms.TTerm Syntax.ParamWithDefault -> Phantoms.TTerm (Maybe Syntax.TypeComment) -> Phantoms.TTerm Syntax.ParamWithDefault)
paramWithDefaultWithTypeComment original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.ParamWithDefault"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "param"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.ParamWithDefault"),
          Core.projectionField = (Core.Name "param")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "default"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.ParamWithDefault"),
          Core.projectionField = (Core.Name "default")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "typeComment"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

paramMaybeDefault :: (Phantoms.TTerm Syntax.Param -> Phantoms.TTerm (Maybe Syntax.Default) -> Phantoms.TTerm (Maybe Syntax.TypeComment) -> Phantoms.TTerm Syntax.ParamMaybeDefault)
paramMaybeDefault param default_ typeComment = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.ParamMaybeDefault"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "param"),
      Core.fieldTerm = (Phantoms.unTTerm param)},
    Core.Field {
      Core.fieldName = (Core.Name "default"),
      Core.fieldTerm = (Phantoms.unTTerm default_)},
    Core.Field {
      Core.fieldName = (Core.Name "typeComment"),
      Core.fieldTerm = (Phantoms.unTTerm typeComment)}]})))

paramMaybeDefaultParam :: (Phantoms.TTerm Syntax.ParamMaybeDefault -> Phantoms.TTerm Syntax.Param)
paramMaybeDefaultParam x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.ParamMaybeDefault"),
    Core.projectionField = (Core.Name "param")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

paramMaybeDefaultDefault :: (Phantoms.TTerm Syntax.ParamMaybeDefault -> Phantoms.TTerm (Maybe Syntax.Default))
paramMaybeDefaultDefault x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.ParamMaybeDefault"),
    Core.projectionField = (Core.Name "default")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

paramMaybeDefaultTypeComment :: (Phantoms.TTerm Syntax.ParamMaybeDefault -> Phantoms.TTerm (Maybe Syntax.TypeComment))
paramMaybeDefaultTypeComment x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.ParamMaybeDefault"),
    Core.projectionField = (Core.Name "typeComment")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

paramMaybeDefaultWithParam :: (Phantoms.TTerm Syntax.ParamMaybeDefault -> Phantoms.TTerm Syntax.Param -> Phantoms.TTerm Syntax.ParamMaybeDefault)
paramMaybeDefaultWithParam original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.ParamMaybeDefault"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "param"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "default"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.ParamMaybeDefault"),
          Core.projectionField = (Core.Name "default")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "typeComment"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.ParamMaybeDefault"),
          Core.projectionField = (Core.Name "typeComment")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

paramMaybeDefaultWithDefault :: (Phantoms.TTerm Syntax.ParamMaybeDefault -> Phantoms.TTerm (Maybe Syntax.Default) -> Phantoms.TTerm Syntax.ParamMaybeDefault)
paramMaybeDefaultWithDefault original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.ParamMaybeDefault"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "param"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.ParamMaybeDefault"),
          Core.projectionField = (Core.Name "param")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "default"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "typeComment"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.ParamMaybeDefault"),
          Core.projectionField = (Core.Name "typeComment")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

paramMaybeDefaultWithTypeComment :: (Phantoms.TTerm Syntax.ParamMaybeDefault -> Phantoms.TTerm (Maybe Syntax.TypeComment) -> Phantoms.TTerm Syntax.ParamMaybeDefault)
paramMaybeDefaultWithTypeComment original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.ParamMaybeDefault"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "param"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.ParamMaybeDefault"),
          Core.projectionField = (Core.Name "param")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "default"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.ParamMaybeDefault"),
          Core.projectionField = (Core.Name "default")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "typeComment"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

param :: (Phantoms.TTerm Syntax.Name -> Phantoms.TTerm (Maybe Syntax.Annotation) -> Phantoms.TTerm Syntax.Param)
param name annotation = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.Param"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Phantoms.unTTerm name)},
    Core.Field {
      Core.fieldName = (Core.Name "annotation"),
      Core.fieldTerm = (Phantoms.unTTerm annotation)}]})))

paramName :: (Phantoms.TTerm Syntax.Param -> Phantoms.TTerm Syntax.Name)
paramName x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.Param"),
    Core.projectionField = (Core.Name "name")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

paramAnnotation :: (Phantoms.TTerm Syntax.Param -> Phantoms.TTerm (Maybe Syntax.Annotation))
paramAnnotation x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.Param"),
    Core.projectionField = (Core.Name "annotation")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

paramWithName :: (Phantoms.TTerm Syntax.Param -> Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.Param)
paramWithName original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.Param"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "annotation"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.Param"),
          Core.projectionField = (Core.Name "annotation")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

paramWithAnnotation :: (Phantoms.TTerm Syntax.Param -> Phantoms.TTerm (Maybe Syntax.Annotation) -> Phantoms.TTerm Syntax.Param)
paramWithAnnotation original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.Param"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.Param"),
          Core.projectionField = (Core.Name "name")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "annotation"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

paramStarAnnotation :: (Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.StarAnnotation -> Phantoms.TTerm Syntax.ParamStarAnnotation)
paramStarAnnotation name annotation = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.ParamStarAnnotation"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Phantoms.unTTerm name)},
    Core.Field {
      Core.fieldName = (Core.Name "annotation"),
      Core.fieldTerm = (Phantoms.unTTerm annotation)}]})))

paramStarAnnotationName :: (Phantoms.TTerm Syntax.ParamStarAnnotation -> Phantoms.TTerm Syntax.Name)
paramStarAnnotationName x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.ParamStarAnnotation"),
    Core.projectionField = (Core.Name "name")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

paramStarAnnotationAnnotation :: (Phantoms.TTerm Syntax.ParamStarAnnotation -> Phantoms.TTerm Syntax.StarAnnotation)
paramStarAnnotationAnnotation x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.ParamStarAnnotation"),
    Core.projectionField = (Core.Name "annotation")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

paramStarAnnotationWithName :: (Phantoms.TTerm Syntax.ParamStarAnnotation -> Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.ParamStarAnnotation)
paramStarAnnotationWithName original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.ParamStarAnnotation"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "annotation"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.ParamStarAnnotation"),
          Core.projectionField = (Core.Name "annotation")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

paramStarAnnotationWithAnnotation :: (Phantoms.TTerm Syntax.ParamStarAnnotation -> Phantoms.TTerm Syntax.StarAnnotation -> Phantoms.TTerm Syntax.ParamStarAnnotation)
paramStarAnnotationWithAnnotation original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.ParamStarAnnotation"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.ParamStarAnnotation"),
          Core.projectionField = (Core.Name "name")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "annotation"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

annotation :: (Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.Annotation)
annotation x = (Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
  Core.wrappedTermTypeName = (Core.Name "hydra.ext.python.syntax.Annotation"),
  Core.wrappedTermBody = (Phantoms.unTTerm x)})))

unAnnotation :: (Phantoms.TTerm Syntax.Annotation -> Phantoms.TTerm Syntax.Expression)
unAnnotation x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.python.syntax.Annotation")))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

starAnnotation :: (Phantoms.TTerm Syntax.StarExpression -> Phantoms.TTerm Syntax.StarAnnotation)
starAnnotation x = (Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
  Core.wrappedTermTypeName = (Core.Name "hydra.ext.python.syntax.StarAnnotation"),
  Core.wrappedTermBody = (Phantoms.unTTerm x)})))

unStarAnnotation :: (Phantoms.TTerm Syntax.StarAnnotation -> Phantoms.TTerm Syntax.StarExpression)
unStarAnnotation x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.python.syntax.StarAnnotation")))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

default_ :: (Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.Default)
default_ x = (Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
  Core.wrappedTermTypeName = (Core.Name "hydra.ext.python.syntax.Default"),
  Core.wrappedTermBody = (Phantoms.unTTerm x)})))

unDefault :: (Phantoms.TTerm Syntax.Default -> Phantoms.TTerm Syntax.Expression)
unDefault x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.python.syntax.Default")))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

ifStatement :: (Phantoms.TTerm Syntax.NamedExpression -> Phantoms.TTerm Syntax.Block -> Phantoms.TTerm (Maybe Syntax.IfTail) -> Phantoms.TTerm Syntax.IfStatement)
ifStatement condition body continuation = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.IfStatement"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "condition"),
      Core.fieldTerm = (Phantoms.unTTerm condition)},
    Core.Field {
      Core.fieldName = (Core.Name "body"),
      Core.fieldTerm = (Phantoms.unTTerm body)},
    Core.Field {
      Core.fieldName = (Core.Name "continuation"),
      Core.fieldTerm = (Phantoms.unTTerm continuation)}]})))

ifStatementCondition :: (Phantoms.TTerm Syntax.IfStatement -> Phantoms.TTerm Syntax.NamedExpression)
ifStatementCondition x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.IfStatement"),
    Core.projectionField = (Core.Name "condition")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

ifStatementBody :: (Phantoms.TTerm Syntax.IfStatement -> Phantoms.TTerm Syntax.Block)
ifStatementBody x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.IfStatement"),
    Core.projectionField = (Core.Name "body")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

ifStatementContinuation :: (Phantoms.TTerm Syntax.IfStatement -> Phantoms.TTerm (Maybe Syntax.IfTail))
ifStatementContinuation x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.IfStatement"),
    Core.projectionField = (Core.Name "continuation")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

ifStatementWithCondition :: (Phantoms.TTerm Syntax.IfStatement -> Phantoms.TTerm Syntax.NamedExpression -> Phantoms.TTerm Syntax.IfStatement)
ifStatementWithCondition original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.IfStatement"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "condition"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "body"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.IfStatement"),
          Core.projectionField = (Core.Name "body")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "continuation"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.IfStatement"),
          Core.projectionField = (Core.Name "continuation")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

ifStatementWithBody :: (Phantoms.TTerm Syntax.IfStatement -> Phantoms.TTerm Syntax.Block -> Phantoms.TTerm Syntax.IfStatement)
ifStatementWithBody original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.IfStatement"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "condition"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.IfStatement"),
          Core.projectionField = (Core.Name "condition")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "body"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "continuation"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.IfStatement"),
          Core.projectionField = (Core.Name "continuation")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

ifStatementWithContinuation :: (Phantoms.TTerm Syntax.IfStatement -> Phantoms.TTerm (Maybe Syntax.IfTail) -> Phantoms.TTerm Syntax.IfStatement)
ifStatementWithContinuation original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.IfStatement"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "condition"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.IfStatement"),
          Core.projectionField = (Core.Name "condition")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "body"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.IfStatement"),
          Core.projectionField = (Core.Name "body")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "continuation"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

ifTailElif :: (Phantoms.TTerm Syntax.IfStatement -> Phantoms.TTerm Syntax.IfTail)
ifTailElif x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.IfTail"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "elif"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

ifTailElse :: (Phantoms.TTerm Syntax.Block -> Phantoms.TTerm Syntax.IfTail)
ifTailElse x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.IfTail"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "else"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

whileStatement :: (Phantoms.TTerm Syntax.NamedExpression -> Phantoms.TTerm Syntax.Block -> Phantoms.TTerm (Maybe Syntax.Block) -> Phantoms.TTerm Syntax.WhileStatement)
whileStatement condition body else_ = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.WhileStatement"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "condition"),
      Core.fieldTerm = (Phantoms.unTTerm condition)},
    Core.Field {
      Core.fieldName = (Core.Name "body"),
      Core.fieldTerm = (Phantoms.unTTerm body)},
    Core.Field {
      Core.fieldName = (Core.Name "else"),
      Core.fieldTerm = (Phantoms.unTTerm else_)}]})))

whileStatementCondition :: (Phantoms.TTerm Syntax.WhileStatement -> Phantoms.TTerm Syntax.NamedExpression)
whileStatementCondition x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.WhileStatement"),
    Core.projectionField = (Core.Name "condition")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

whileStatementBody :: (Phantoms.TTerm Syntax.WhileStatement -> Phantoms.TTerm Syntax.Block)
whileStatementBody x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.WhileStatement"),
    Core.projectionField = (Core.Name "body")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

whileStatementElse :: (Phantoms.TTerm Syntax.WhileStatement -> Phantoms.TTerm (Maybe Syntax.Block))
whileStatementElse x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.WhileStatement"),
    Core.projectionField = (Core.Name "else")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

whileStatementWithCondition :: (Phantoms.TTerm Syntax.WhileStatement -> Phantoms.TTerm Syntax.NamedExpression -> Phantoms.TTerm Syntax.WhileStatement)
whileStatementWithCondition original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.WhileStatement"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "condition"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "body"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.WhileStatement"),
          Core.projectionField = (Core.Name "body")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "else"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.WhileStatement"),
          Core.projectionField = (Core.Name "else")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

whileStatementWithBody :: (Phantoms.TTerm Syntax.WhileStatement -> Phantoms.TTerm Syntax.Block -> Phantoms.TTerm Syntax.WhileStatement)
whileStatementWithBody original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.WhileStatement"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "condition"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.WhileStatement"),
          Core.projectionField = (Core.Name "condition")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "body"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "else"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.WhileStatement"),
          Core.projectionField = (Core.Name "else")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

whileStatementWithElse :: (Phantoms.TTerm Syntax.WhileStatement -> Phantoms.TTerm (Maybe Syntax.Block) -> Phantoms.TTerm Syntax.WhileStatement)
whileStatementWithElse original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.WhileStatement"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "condition"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.WhileStatement"),
          Core.projectionField = (Core.Name "condition")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "body"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.WhileStatement"),
          Core.projectionField = (Core.Name "body")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "else"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

forStatement :: (Phantoms.TTerm Bool -> Phantoms.TTerm [Syntax.StarTarget] -> Phantoms.TTerm [Syntax.StarExpression] -> Phantoms.TTerm (Maybe Syntax.TypeComment) -> Phantoms.TTerm Syntax.Block -> Phantoms.TTerm (Maybe Syntax.Block) -> Phantoms.TTerm Syntax.ForStatement)
forStatement async targets expressions typeComment body else_ = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.ForStatement"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "async"),
      Core.fieldTerm = (Phantoms.unTTerm async)},
    Core.Field {
      Core.fieldName = (Core.Name "targets"),
      Core.fieldTerm = (Phantoms.unTTerm targets)},
    Core.Field {
      Core.fieldName = (Core.Name "expressions"),
      Core.fieldTerm = (Phantoms.unTTerm expressions)},
    Core.Field {
      Core.fieldName = (Core.Name "typeComment"),
      Core.fieldTerm = (Phantoms.unTTerm typeComment)},
    Core.Field {
      Core.fieldName = (Core.Name "body"),
      Core.fieldTerm = (Phantoms.unTTerm body)},
    Core.Field {
      Core.fieldName = (Core.Name "else"),
      Core.fieldTerm = (Phantoms.unTTerm else_)}]})))

forStatementAsync :: (Phantoms.TTerm Syntax.ForStatement -> Phantoms.TTerm Bool)
forStatementAsync x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.ForStatement"),
    Core.projectionField = (Core.Name "async")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

forStatementTargets :: (Phantoms.TTerm Syntax.ForStatement -> Phantoms.TTerm [Syntax.StarTarget])
forStatementTargets x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.ForStatement"),
    Core.projectionField = (Core.Name "targets")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

forStatementExpressions :: (Phantoms.TTerm Syntax.ForStatement -> Phantoms.TTerm [Syntax.StarExpression])
forStatementExpressions x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.ForStatement"),
    Core.projectionField = (Core.Name "expressions")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

forStatementTypeComment :: (Phantoms.TTerm Syntax.ForStatement -> Phantoms.TTerm (Maybe Syntax.TypeComment))
forStatementTypeComment x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.ForStatement"),
    Core.projectionField = (Core.Name "typeComment")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

forStatementBody :: (Phantoms.TTerm Syntax.ForStatement -> Phantoms.TTerm Syntax.Block)
forStatementBody x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.ForStatement"),
    Core.projectionField = (Core.Name "body")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

forStatementElse :: (Phantoms.TTerm Syntax.ForStatement -> Phantoms.TTerm (Maybe Syntax.Block))
forStatementElse x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.ForStatement"),
    Core.projectionField = (Core.Name "else")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

forStatementWithAsync :: (Phantoms.TTerm Syntax.ForStatement -> Phantoms.TTerm Bool -> Phantoms.TTerm Syntax.ForStatement)
forStatementWithAsync original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.ForStatement"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "async"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "targets"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.ForStatement"),
          Core.projectionField = (Core.Name "targets")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "expressions"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.ForStatement"),
          Core.projectionField = (Core.Name "expressions")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "typeComment"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.ForStatement"),
          Core.projectionField = (Core.Name "typeComment")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "body"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.ForStatement"),
          Core.projectionField = (Core.Name "body")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "else"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.ForStatement"),
          Core.projectionField = (Core.Name "else")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

forStatementWithTargets :: (Phantoms.TTerm Syntax.ForStatement -> Phantoms.TTerm [Syntax.StarTarget] -> Phantoms.TTerm Syntax.ForStatement)
forStatementWithTargets original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.ForStatement"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "async"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.ForStatement"),
          Core.projectionField = (Core.Name "async")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "targets"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "expressions"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.ForStatement"),
          Core.projectionField = (Core.Name "expressions")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "typeComment"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.ForStatement"),
          Core.projectionField = (Core.Name "typeComment")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "body"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.ForStatement"),
          Core.projectionField = (Core.Name "body")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "else"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.ForStatement"),
          Core.projectionField = (Core.Name "else")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

forStatementWithExpressions :: (Phantoms.TTerm Syntax.ForStatement -> Phantoms.TTerm [Syntax.StarExpression] -> Phantoms.TTerm Syntax.ForStatement)
forStatementWithExpressions original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.ForStatement"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "async"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.ForStatement"),
          Core.projectionField = (Core.Name "async")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "targets"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.ForStatement"),
          Core.projectionField = (Core.Name "targets")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "expressions"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "typeComment"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.ForStatement"),
          Core.projectionField = (Core.Name "typeComment")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "body"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.ForStatement"),
          Core.projectionField = (Core.Name "body")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "else"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.ForStatement"),
          Core.projectionField = (Core.Name "else")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

forStatementWithTypeComment :: (Phantoms.TTerm Syntax.ForStatement -> Phantoms.TTerm (Maybe Syntax.TypeComment) -> Phantoms.TTerm Syntax.ForStatement)
forStatementWithTypeComment original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.ForStatement"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "async"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.ForStatement"),
          Core.projectionField = (Core.Name "async")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "targets"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.ForStatement"),
          Core.projectionField = (Core.Name "targets")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "expressions"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.ForStatement"),
          Core.projectionField = (Core.Name "expressions")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "typeComment"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "body"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.ForStatement"),
          Core.projectionField = (Core.Name "body")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "else"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.ForStatement"),
          Core.projectionField = (Core.Name "else")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

forStatementWithBody :: (Phantoms.TTerm Syntax.ForStatement -> Phantoms.TTerm Syntax.Block -> Phantoms.TTerm Syntax.ForStatement)
forStatementWithBody original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.ForStatement"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "async"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.ForStatement"),
          Core.projectionField = (Core.Name "async")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "targets"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.ForStatement"),
          Core.projectionField = (Core.Name "targets")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "expressions"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.ForStatement"),
          Core.projectionField = (Core.Name "expressions")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "typeComment"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.ForStatement"),
          Core.projectionField = (Core.Name "typeComment")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "body"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "else"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.ForStatement"),
          Core.projectionField = (Core.Name "else")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

forStatementWithElse :: (Phantoms.TTerm Syntax.ForStatement -> Phantoms.TTerm (Maybe Syntax.Block) -> Phantoms.TTerm Syntax.ForStatement)
forStatementWithElse original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.ForStatement"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "async"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.ForStatement"),
          Core.projectionField = (Core.Name "async")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "targets"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.ForStatement"),
          Core.projectionField = (Core.Name "targets")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "expressions"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.ForStatement"),
          Core.projectionField = (Core.Name "expressions")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "typeComment"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.ForStatement"),
          Core.projectionField = (Core.Name "typeComment")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "body"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.ForStatement"),
          Core.projectionField = (Core.Name "body")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "else"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

withStatement :: (Phantoms.TTerm Bool -> Phantoms.TTerm [Syntax.WithItem] -> Phantoms.TTerm (Maybe Syntax.TypeComment) -> Phantoms.TTerm Syntax.Block -> Phantoms.TTerm Syntax.WithStatement)
withStatement async items typeComment body = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.WithStatement"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "async"),
      Core.fieldTerm = (Phantoms.unTTerm async)},
    Core.Field {
      Core.fieldName = (Core.Name "items"),
      Core.fieldTerm = (Phantoms.unTTerm items)},
    Core.Field {
      Core.fieldName = (Core.Name "typeComment"),
      Core.fieldTerm = (Phantoms.unTTerm typeComment)},
    Core.Field {
      Core.fieldName = (Core.Name "body"),
      Core.fieldTerm = (Phantoms.unTTerm body)}]})))

withStatementAsync :: (Phantoms.TTerm Syntax.WithStatement -> Phantoms.TTerm Bool)
withStatementAsync x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.WithStatement"),
    Core.projectionField = (Core.Name "async")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

withStatementItems :: (Phantoms.TTerm Syntax.WithStatement -> Phantoms.TTerm [Syntax.WithItem])
withStatementItems x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.WithStatement"),
    Core.projectionField = (Core.Name "items")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

withStatementTypeComment :: (Phantoms.TTerm Syntax.WithStatement -> Phantoms.TTerm (Maybe Syntax.TypeComment))
withStatementTypeComment x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.WithStatement"),
    Core.projectionField = (Core.Name "typeComment")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

withStatementBody :: (Phantoms.TTerm Syntax.WithStatement -> Phantoms.TTerm Syntax.Block)
withStatementBody x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.WithStatement"),
    Core.projectionField = (Core.Name "body")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

withStatementWithAsync :: (Phantoms.TTerm Syntax.WithStatement -> Phantoms.TTerm Bool -> Phantoms.TTerm Syntax.WithStatement)
withStatementWithAsync original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.WithStatement"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "async"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "items"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.WithStatement"),
          Core.projectionField = (Core.Name "items")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "typeComment"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.WithStatement"),
          Core.projectionField = (Core.Name "typeComment")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "body"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.WithStatement"),
          Core.projectionField = (Core.Name "body")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

withStatementWithItems :: (Phantoms.TTerm Syntax.WithStatement -> Phantoms.TTerm [Syntax.WithItem] -> Phantoms.TTerm Syntax.WithStatement)
withStatementWithItems original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.WithStatement"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "async"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.WithStatement"),
          Core.projectionField = (Core.Name "async")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "items"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "typeComment"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.WithStatement"),
          Core.projectionField = (Core.Name "typeComment")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "body"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.WithStatement"),
          Core.projectionField = (Core.Name "body")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

withStatementWithTypeComment :: (Phantoms.TTerm Syntax.WithStatement -> Phantoms.TTerm (Maybe Syntax.TypeComment) -> Phantoms.TTerm Syntax.WithStatement)
withStatementWithTypeComment original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.WithStatement"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "async"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.WithStatement"),
          Core.projectionField = (Core.Name "async")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "items"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.WithStatement"),
          Core.projectionField = (Core.Name "items")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "typeComment"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "body"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.WithStatement"),
          Core.projectionField = (Core.Name "body")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

withStatementWithBody :: (Phantoms.TTerm Syntax.WithStatement -> Phantoms.TTerm Syntax.Block -> Phantoms.TTerm Syntax.WithStatement)
withStatementWithBody original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.WithStatement"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "async"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.WithStatement"),
          Core.projectionField = (Core.Name "async")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "items"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.WithStatement"),
          Core.projectionField = (Core.Name "items")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "typeComment"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.WithStatement"),
          Core.projectionField = (Core.Name "typeComment")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "body"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

withItem :: (Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm (Maybe Syntax.StarTarget) -> Phantoms.TTerm Syntax.WithItem)
withItem expression as = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.WithItem"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "expression"),
      Core.fieldTerm = (Phantoms.unTTerm expression)},
    Core.Field {
      Core.fieldName = (Core.Name "as"),
      Core.fieldTerm = (Phantoms.unTTerm as)}]})))

withItemExpression :: (Phantoms.TTerm Syntax.WithItem -> Phantoms.TTerm Syntax.Expression)
withItemExpression x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.WithItem"),
    Core.projectionField = (Core.Name "expression")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

withItemAs :: (Phantoms.TTerm Syntax.WithItem -> Phantoms.TTerm (Maybe Syntax.StarTarget))
withItemAs x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.WithItem"),
    Core.projectionField = (Core.Name "as")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

withItemWithExpression :: (Phantoms.TTerm Syntax.WithItem -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.WithItem)
withItemWithExpression original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.WithItem"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "expression"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "as"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.WithItem"),
          Core.projectionField = (Core.Name "as")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

withItemWithAs :: (Phantoms.TTerm Syntax.WithItem -> Phantoms.TTerm (Maybe Syntax.StarTarget) -> Phantoms.TTerm Syntax.WithItem)
withItemWithAs original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.WithItem"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "expression"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.WithItem"),
          Core.projectionField = (Core.Name "expression")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "as"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

tryStatementFinally :: (Phantoms.TTerm Syntax.TryFinallyStatement -> Phantoms.TTerm Syntax.TryStatement)
tryStatementFinally x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.TryStatement"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "finally"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

tryStatementExcept :: (Phantoms.TTerm Syntax.TryExceptStatement -> Phantoms.TTerm Syntax.TryStatement)
tryStatementExcept x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.TryStatement"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "except"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

tryStatementExceptStar :: (Phantoms.TTerm Syntax.TryExceptStarStatement -> Phantoms.TTerm Syntax.TryStatement)
tryStatementExceptStar x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.TryStatement"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "exceptStar"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

tryFinallyStatement :: (Phantoms.TTerm Syntax.Block -> Phantoms.TTerm Syntax.Block -> Phantoms.TTerm Syntax.TryFinallyStatement)
tryFinallyStatement body finally = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.TryFinallyStatement"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "body"),
      Core.fieldTerm = (Phantoms.unTTerm body)},
    Core.Field {
      Core.fieldName = (Core.Name "finally"),
      Core.fieldTerm = (Phantoms.unTTerm finally)}]})))

tryFinallyStatementBody :: (Phantoms.TTerm Syntax.TryFinallyStatement -> Phantoms.TTerm Syntax.Block)
tryFinallyStatementBody x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.TryFinallyStatement"),
    Core.projectionField = (Core.Name "body")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

tryFinallyStatementFinally :: (Phantoms.TTerm Syntax.TryFinallyStatement -> Phantoms.TTerm Syntax.Block)
tryFinallyStatementFinally x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.TryFinallyStatement"),
    Core.projectionField = (Core.Name "finally")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

tryFinallyStatementWithBody :: (Phantoms.TTerm Syntax.TryFinallyStatement -> Phantoms.TTerm Syntax.Block -> Phantoms.TTerm Syntax.TryFinallyStatement)
tryFinallyStatementWithBody original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.TryFinallyStatement"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "body"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "finally"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.TryFinallyStatement"),
          Core.projectionField = (Core.Name "finally")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

tryFinallyStatementWithFinally :: (Phantoms.TTerm Syntax.TryFinallyStatement -> Phantoms.TTerm Syntax.Block -> Phantoms.TTerm Syntax.TryFinallyStatement)
tryFinallyStatementWithFinally original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.TryFinallyStatement"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "body"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.TryFinallyStatement"),
          Core.projectionField = (Core.Name "body")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "finally"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

tryExceptStatement :: (Phantoms.TTerm Syntax.Block -> Phantoms.TTerm [Syntax.ExceptBlock] -> Phantoms.TTerm (Maybe Syntax.Block) -> Phantoms.TTerm (Maybe Syntax.Block) -> Phantoms.TTerm Syntax.TryExceptStatement)
tryExceptStatement body excepts else_ finally = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.TryExceptStatement"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "body"),
      Core.fieldTerm = (Phantoms.unTTerm body)},
    Core.Field {
      Core.fieldName = (Core.Name "excepts"),
      Core.fieldTerm = (Phantoms.unTTerm excepts)},
    Core.Field {
      Core.fieldName = (Core.Name "else"),
      Core.fieldTerm = (Phantoms.unTTerm else_)},
    Core.Field {
      Core.fieldName = (Core.Name "finally"),
      Core.fieldTerm = (Phantoms.unTTerm finally)}]})))

tryExceptStatementBody :: (Phantoms.TTerm Syntax.TryExceptStatement -> Phantoms.TTerm Syntax.Block)
tryExceptStatementBody x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.TryExceptStatement"),
    Core.projectionField = (Core.Name "body")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

tryExceptStatementExcepts :: (Phantoms.TTerm Syntax.TryExceptStatement -> Phantoms.TTerm [Syntax.ExceptBlock])
tryExceptStatementExcepts x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.TryExceptStatement"),
    Core.projectionField = (Core.Name "excepts")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

tryExceptStatementElse :: (Phantoms.TTerm Syntax.TryExceptStatement -> Phantoms.TTerm (Maybe Syntax.Block))
tryExceptStatementElse x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.TryExceptStatement"),
    Core.projectionField = (Core.Name "else")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

tryExceptStatementFinally :: (Phantoms.TTerm Syntax.TryExceptStatement -> Phantoms.TTerm (Maybe Syntax.Block))
tryExceptStatementFinally x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.TryExceptStatement"),
    Core.projectionField = (Core.Name "finally")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

tryExceptStatementWithBody :: (Phantoms.TTerm Syntax.TryExceptStatement -> Phantoms.TTerm Syntax.Block -> Phantoms.TTerm Syntax.TryExceptStatement)
tryExceptStatementWithBody original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.TryExceptStatement"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "body"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "excepts"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.TryExceptStatement"),
          Core.projectionField = (Core.Name "excepts")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "else"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.TryExceptStatement"),
          Core.projectionField = (Core.Name "else")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "finally"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.TryExceptStatement"),
          Core.projectionField = (Core.Name "finally")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

tryExceptStatementWithExcepts :: (Phantoms.TTerm Syntax.TryExceptStatement -> Phantoms.TTerm [Syntax.ExceptBlock] -> Phantoms.TTerm Syntax.TryExceptStatement)
tryExceptStatementWithExcepts original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.TryExceptStatement"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "body"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.TryExceptStatement"),
          Core.projectionField = (Core.Name "body")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "excepts"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "else"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.TryExceptStatement"),
          Core.projectionField = (Core.Name "else")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "finally"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.TryExceptStatement"),
          Core.projectionField = (Core.Name "finally")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

tryExceptStatementWithElse :: (Phantoms.TTerm Syntax.TryExceptStatement -> Phantoms.TTerm (Maybe Syntax.Block) -> Phantoms.TTerm Syntax.TryExceptStatement)
tryExceptStatementWithElse original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.TryExceptStatement"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "body"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.TryExceptStatement"),
          Core.projectionField = (Core.Name "body")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "excepts"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.TryExceptStatement"),
          Core.projectionField = (Core.Name "excepts")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "else"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "finally"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.TryExceptStatement"),
          Core.projectionField = (Core.Name "finally")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

tryExceptStatementWithFinally :: (Phantoms.TTerm Syntax.TryExceptStatement -> Phantoms.TTerm (Maybe Syntax.Block) -> Phantoms.TTerm Syntax.TryExceptStatement)
tryExceptStatementWithFinally original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.TryExceptStatement"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "body"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.TryExceptStatement"),
          Core.projectionField = (Core.Name "body")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "excepts"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.TryExceptStatement"),
          Core.projectionField = (Core.Name "excepts")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "else"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.TryExceptStatement"),
          Core.projectionField = (Core.Name "else")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "finally"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

tryExceptStarStatement :: (Phantoms.TTerm Syntax.Block -> Phantoms.TTerm [Syntax.ExceptStarBlock] -> Phantoms.TTerm (Maybe Syntax.Block) -> Phantoms.TTerm (Maybe Syntax.Block) -> Phantoms.TTerm Syntax.TryExceptStarStatement)
tryExceptStarStatement body excepts else_ finally = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.TryExceptStarStatement"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "body"),
      Core.fieldTerm = (Phantoms.unTTerm body)},
    Core.Field {
      Core.fieldName = (Core.Name "excepts"),
      Core.fieldTerm = (Phantoms.unTTerm excepts)},
    Core.Field {
      Core.fieldName = (Core.Name "else"),
      Core.fieldTerm = (Phantoms.unTTerm else_)},
    Core.Field {
      Core.fieldName = (Core.Name "finally"),
      Core.fieldTerm = (Phantoms.unTTerm finally)}]})))

tryExceptStarStatementBody :: (Phantoms.TTerm Syntax.TryExceptStarStatement -> Phantoms.TTerm Syntax.Block)
tryExceptStarStatementBody x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.TryExceptStarStatement"),
    Core.projectionField = (Core.Name "body")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

tryExceptStarStatementExcepts :: (Phantoms.TTerm Syntax.TryExceptStarStatement -> Phantoms.TTerm [Syntax.ExceptStarBlock])
tryExceptStarStatementExcepts x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.TryExceptStarStatement"),
    Core.projectionField = (Core.Name "excepts")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

tryExceptStarStatementElse :: (Phantoms.TTerm Syntax.TryExceptStarStatement -> Phantoms.TTerm (Maybe Syntax.Block))
tryExceptStarStatementElse x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.TryExceptStarStatement"),
    Core.projectionField = (Core.Name "else")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

tryExceptStarStatementFinally :: (Phantoms.TTerm Syntax.TryExceptStarStatement -> Phantoms.TTerm (Maybe Syntax.Block))
tryExceptStarStatementFinally x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.TryExceptStarStatement"),
    Core.projectionField = (Core.Name "finally")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

tryExceptStarStatementWithBody :: (Phantoms.TTerm Syntax.TryExceptStarStatement -> Phantoms.TTerm Syntax.Block -> Phantoms.TTerm Syntax.TryExceptStarStatement)
tryExceptStarStatementWithBody original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.TryExceptStarStatement"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "body"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "excepts"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.TryExceptStarStatement"),
          Core.projectionField = (Core.Name "excepts")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "else"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.TryExceptStarStatement"),
          Core.projectionField = (Core.Name "else")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "finally"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.TryExceptStarStatement"),
          Core.projectionField = (Core.Name "finally")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

tryExceptStarStatementWithExcepts :: (Phantoms.TTerm Syntax.TryExceptStarStatement -> Phantoms.TTerm [Syntax.ExceptStarBlock] -> Phantoms.TTerm Syntax.TryExceptStarStatement)
tryExceptStarStatementWithExcepts original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.TryExceptStarStatement"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "body"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.TryExceptStarStatement"),
          Core.projectionField = (Core.Name "body")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "excepts"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "else"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.TryExceptStarStatement"),
          Core.projectionField = (Core.Name "else")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "finally"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.TryExceptStarStatement"),
          Core.projectionField = (Core.Name "finally")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

tryExceptStarStatementWithElse :: (Phantoms.TTerm Syntax.TryExceptStarStatement -> Phantoms.TTerm (Maybe Syntax.Block) -> Phantoms.TTerm Syntax.TryExceptStarStatement)
tryExceptStarStatementWithElse original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.TryExceptStarStatement"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "body"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.TryExceptStarStatement"),
          Core.projectionField = (Core.Name "body")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "excepts"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.TryExceptStarStatement"),
          Core.projectionField = (Core.Name "excepts")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "else"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "finally"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.TryExceptStarStatement"),
          Core.projectionField = (Core.Name "finally")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

tryExceptStarStatementWithFinally :: (Phantoms.TTerm Syntax.TryExceptStarStatement -> Phantoms.TTerm (Maybe Syntax.Block) -> Phantoms.TTerm Syntax.TryExceptStarStatement)
tryExceptStarStatementWithFinally original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.TryExceptStarStatement"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "body"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.TryExceptStarStatement"),
          Core.projectionField = (Core.Name "body")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "excepts"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.TryExceptStarStatement"),
          Core.projectionField = (Core.Name "excepts")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "else"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.TryExceptStarStatement"),
          Core.projectionField = (Core.Name "else")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "finally"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

exceptBlock :: (Phantoms.TTerm (Maybe Syntax.ExceptExpression) -> Phantoms.TTerm Syntax.Block -> Phantoms.TTerm Syntax.ExceptBlock)
exceptBlock expression body = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.ExceptBlock"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "expression"),
      Core.fieldTerm = (Phantoms.unTTerm expression)},
    Core.Field {
      Core.fieldName = (Core.Name "body"),
      Core.fieldTerm = (Phantoms.unTTerm body)}]})))

exceptBlockExpression :: (Phantoms.TTerm Syntax.ExceptBlock -> Phantoms.TTerm (Maybe Syntax.ExceptExpression))
exceptBlockExpression x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.ExceptBlock"),
    Core.projectionField = (Core.Name "expression")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

exceptBlockBody :: (Phantoms.TTerm Syntax.ExceptBlock -> Phantoms.TTerm Syntax.Block)
exceptBlockBody x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.ExceptBlock"),
    Core.projectionField = (Core.Name "body")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

exceptBlockWithExpression :: (Phantoms.TTerm Syntax.ExceptBlock -> Phantoms.TTerm (Maybe Syntax.ExceptExpression) -> Phantoms.TTerm Syntax.ExceptBlock)
exceptBlockWithExpression original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.ExceptBlock"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "expression"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "body"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.ExceptBlock"),
          Core.projectionField = (Core.Name "body")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

exceptBlockWithBody :: (Phantoms.TTerm Syntax.ExceptBlock -> Phantoms.TTerm Syntax.Block -> Phantoms.TTerm Syntax.ExceptBlock)
exceptBlockWithBody original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.ExceptBlock"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "expression"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.ExceptBlock"),
          Core.projectionField = (Core.Name "expression")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "body"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

exceptExpression :: (Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm (Maybe Syntax.Name) -> Phantoms.TTerm Syntax.ExceptExpression)
exceptExpression expression as = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.ExceptExpression"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "expression"),
      Core.fieldTerm = (Phantoms.unTTerm expression)},
    Core.Field {
      Core.fieldName = (Core.Name "as"),
      Core.fieldTerm = (Phantoms.unTTerm as)}]})))

exceptExpressionExpression :: (Phantoms.TTerm Syntax.ExceptExpression -> Phantoms.TTerm Syntax.Expression)
exceptExpressionExpression x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.ExceptExpression"),
    Core.projectionField = (Core.Name "expression")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

exceptExpressionAs :: (Phantoms.TTerm Syntax.ExceptExpression -> Phantoms.TTerm (Maybe Syntax.Name))
exceptExpressionAs x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.ExceptExpression"),
    Core.projectionField = (Core.Name "as")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

exceptExpressionWithExpression :: (Phantoms.TTerm Syntax.ExceptExpression -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.ExceptExpression)
exceptExpressionWithExpression original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.ExceptExpression"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "expression"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "as"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.ExceptExpression"),
          Core.projectionField = (Core.Name "as")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

exceptExpressionWithAs :: (Phantoms.TTerm Syntax.ExceptExpression -> Phantoms.TTerm (Maybe Syntax.Name) -> Phantoms.TTerm Syntax.ExceptExpression)
exceptExpressionWithAs original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.ExceptExpression"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "expression"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.ExceptExpression"),
          Core.projectionField = (Core.Name "expression")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "as"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

exceptStarBlock :: (Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm (Maybe Syntax.Name) -> Phantoms.TTerm Syntax.Block -> Phantoms.TTerm Syntax.ExceptStarBlock)
exceptStarBlock expression as body = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.ExceptStarBlock"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "expression"),
      Core.fieldTerm = (Phantoms.unTTerm expression)},
    Core.Field {
      Core.fieldName = (Core.Name "as"),
      Core.fieldTerm = (Phantoms.unTTerm as)},
    Core.Field {
      Core.fieldName = (Core.Name "body"),
      Core.fieldTerm = (Phantoms.unTTerm body)}]})))

exceptStarBlockExpression :: (Phantoms.TTerm Syntax.ExceptStarBlock -> Phantoms.TTerm Syntax.Expression)
exceptStarBlockExpression x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.ExceptStarBlock"),
    Core.projectionField = (Core.Name "expression")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

exceptStarBlockAs :: (Phantoms.TTerm Syntax.ExceptStarBlock -> Phantoms.TTerm (Maybe Syntax.Name))
exceptStarBlockAs x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.ExceptStarBlock"),
    Core.projectionField = (Core.Name "as")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

exceptStarBlockBody :: (Phantoms.TTerm Syntax.ExceptStarBlock -> Phantoms.TTerm Syntax.Block)
exceptStarBlockBody x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.ExceptStarBlock"),
    Core.projectionField = (Core.Name "body")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

exceptStarBlockWithExpression :: (Phantoms.TTerm Syntax.ExceptStarBlock -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.ExceptStarBlock)
exceptStarBlockWithExpression original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.ExceptStarBlock"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "expression"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "as"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.ExceptStarBlock"),
          Core.projectionField = (Core.Name "as")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "body"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.ExceptStarBlock"),
          Core.projectionField = (Core.Name "body")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

exceptStarBlockWithAs :: (Phantoms.TTerm Syntax.ExceptStarBlock -> Phantoms.TTerm (Maybe Syntax.Name) -> Phantoms.TTerm Syntax.ExceptStarBlock)
exceptStarBlockWithAs original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.ExceptStarBlock"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "expression"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.ExceptStarBlock"),
          Core.projectionField = (Core.Name "expression")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "as"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "body"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.ExceptStarBlock"),
          Core.projectionField = (Core.Name "body")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

exceptStarBlockWithBody :: (Phantoms.TTerm Syntax.ExceptStarBlock -> Phantoms.TTerm Syntax.Block -> Phantoms.TTerm Syntax.ExceptStarBlock)
exceptStarBlockWithBody original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.ExceptStarBlock"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "expression"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.ExceptStarBlock"),
          Core.projectionField = (Core.Name "expression")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "as"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.ExceptStarBlock"),
          Core.projectionField = (Core.Name "as")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "body"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

matchStatement :: (Phantoms.TTerm Syntax.SubjectExpression -> Phantoms.TTerm [Syntax.CaseBlock] -> Phantoms.TTerm Syntax.MatchStatement)
matchStatement subject cases = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.MatchStatement"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "subject"),
      Core.fieldTerm = (Phantoms.unTTerm subject)},
    Core.Field {
      Core.fieldName = (Core.Name "cases"),
      Core.fieldTerm = (Phantoms.unTTerm cases)}]})))

matchStatementSubject :: (Phantoms.TTerm Syntax.MatchStatement -> Phantoms.TTerm Syntax.SubjectExpression)
matchStatementSubject x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.MatchStatement"),
    Core.projectionField = (Core.Name "subject")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

matchStatementCases :: (Phantoms.TTerm Syntax.MatchStatement -> Phantoms.TTerm [Syntax.CaseBlock])
matchStatementCases x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.MatchStatement"),
    Core.projectionField = (Core.Name "cases")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

matchStatementWithSubject :: (Phantoms.TTerm Syntax.MatchStatement -> Phantoms.TTerm Syntax.SubjectExpression -> Phantoms.TTerm Syntax.MatchStatement)
matchStatementWithSubject original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.MatchStatement"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "subject"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "cases"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.MatchStatement"),
          Core.projectionField = (Core.Name "cases")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

matchStatementWithCases :: (Phantoms.TTerm Syntax.MatchStatement -> Phantoms.TTerm [Syntax.CaseBlock] -> Phantoms.TTerm Syntax.MatchStatement)
matchStatementWithCases original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.MatchStatement"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "subject"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.MatchStatement"),
          Core.projectionField = (Core.Name "subject")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "cases"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

subjectExpressionTuple :: (Phantoms.TTerm [Syntax.StarNamedExpression] -> Phantoms.TTerm Syntax.SubjectExpression)
subjectExpressionTuple x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.SubjectExpression"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "tuple"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

subjectExpressionSimple :: (Phantoms.TTerm Syntax.NamedExpression -> Phantoms.TTerm Syntax.SubjectExpression)
subjectExpressionSimple x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.SubjectExpression"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "simple"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

caseBlock :: (Phantoms.TTerm Syntax.Patterns -> Phantoms.TTerm (Maybe Syntax.Guard) -> Phantoms.TTerm Syntax.Block -> Phantoms.TTerm Syntax.CaseBlock)
caseBlock patterns guard body = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.CaseBlock"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "patterns"),
      Core.fieldTerm = (Phantoms.unTTerm patterns)},
    Core.Field {
      Core.fieldName = (Core.Name "guard"),
      Core.fieldTerm = (Phantoms.unTTerm guard)},
    Core.Field {
      Core.fieldName = (Core.Name "body"),
      Core.fieldTerm = (Phantoms.unTTerm body)}]})))

caseBlockPatterns :: (Phantoms.TTerm Syntax.CaseBlock -> Phantoms.TTerm Syntax.Patterns)
caseBlockPatterns x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.CaseBlock"),
    Core.projectionField = (Core.Name "patterns")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

caseBlockGuard :: (Phantoms.TTerm Syntax.CaseBlock -> Phantoms.TTerm (Maybe Syntax.Guard))
caseBlockGuard x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.CaseBlock"),
    Core.projectionField = (Core.Name "guard")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

caseBlockBody :: (Phantoms.TTerm Syntax.CaseBlock -> Phantoms.TTerm Syntax.Block)
caseBlockBody x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.CaseBlock"),
    Core.projectionField = (Core.Name "body")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

caseBlockWithPatterns :: (Phantoms.TTerm Syntax.CaseBlock -> Phantoms.TTerm Syntax.Patterns -> Phantoms.TTerm Syntax.CaseBlock)
caseBlockWithPatterns original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.CaseBlock"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "patterns"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "guard"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.CaseBlock"),
          Core.projectionField = (Core.Name "guard")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "body"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.CaseBlock"),
          Core.projectionField = (Core.Name "body")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

caseBlockWithGuard :: (Phantoms.TTerm Syntax.CaseBlock -> Phantoms.TTerm (Maybe Syntax.Guard) -> Phantoms.TTerm Syntax.CaseBlock)
caseBlockWithGuard original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.CaseBlock"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "patterns"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.CaseBlock"),
          Core.projectionField = (Core.Name "patterns")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "guard"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "body"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.CaseBlock"),
          Core.projectionField = (Core.Name "body")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

caseBlockWithBody :: (Phantoms.TTerm Syntax.CaseBlock -> Phantoms.TTerm Syntax.Block -> Phantoms.TTerm Syntax.CaseBlock)
caseBlockWithBody original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.CaseBlock"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "patterns"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.CaseBlock"),
          Core.projectionField = (Core.Name "patterns")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "guard"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.CaseBlock"),
          Core.projectionField = (Core.Name "guard")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "body"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

guard :: (Phantoms.TTerm Syntax.NamedExpression -> Phantoms.TTerm Syntax.Guard)
guard x = (Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
  Core.wrappedTermTypeName = (Core.Name "hydra.ext.python.syntax.Guard"),
  Core.wrappedTermBody = (Phantoms.unTTerm x)})))

unGuard :: (Phantoms.TTerm Syntax.Guard -> Phantoms.TTerm Syntax.NamedExpression)
unGuard x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.python.syntax.Guard")))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

patternsSequence :: (Phantoms.TTerm Syntax.OpenSequencePattern -> Phantoms.TTerm Syntax.Patterns)
patternsSequence x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.Patterns"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "sequence"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

patternsPattern :: (Phantoms.TTerm Syntax.Pattern -> Phantoms.TTerm Syntax.Patterns)
patternsPattern x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.Patterns"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "pattern"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

patternAs :: (Phantoms.TTerm Syntax.AsPattern -> Phantoms.TTerm Syntax.Pattern)
patternAs x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.Pattern"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "as"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

patternOr :: (Phantoms.TTerm Syntax.OrPattern -> Phantoms.TTerm Syntax.Pattern)
patternOr x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.Pattern"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "or"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

asPattern :: (Phantoms.TTerm Syntax.OrPattern -> Phantoms.TTerm Syntax.PatternCaptureTarget -> Phantoms.TTerm Syntax.AsPattern)
asPattern pattern as = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.AsPattern"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "pattern"),
      Core.fieldTerm = (Phantoms.unTTerm pattern)},
    Core.Field {
      Core.fieldName = (Core.Name "as"),
      Core.fieldTerm = (Phantoms.unTTerm as)}]})))

asPatternPattern :: (Phantoms.TTerm Syntax.AsPattern -> Phantoms.TTerm Syntax.OrPattern)
asPatternPattern x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.AsPattern"),
    Core.projectionField = (Core.Name "pattern")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

asPatternAs :: (Phantoms.TTerm Syntax.AsPattern -> Phantoms.TTerm Syntax.PatternCaptureTarget)
asPatternAs x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.AsPattern"),
    Core.projectionField = (Core.Name "as")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

asPatternWithPattern :: (Phantoms.TTerm Syntax.AsPattern -> Phantoms.TTerm Syntax.OrPattern -> Phantoms.TTerm Syntax.AsPattern)
asPatternWithPattern original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.AsPattern"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "pattern"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "as"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.AsPattern"),
          Core.projectionField = (Core.Name "as")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

asPatternWithAs :: (Phantoms.TTerm Syntax.AsPattern -> Phantoms.TTerm Syntax.PatternCaptureTarget -> Phantoms.TTerm Syntax.AsPattern)
asPatternWithAs original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.AsPattern"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "pattern"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.AsPattern"),
          Core.projectionField = (Core.Name "pattern")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "as"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

orPattern :: (Phantoms.TTerm [Syntax.ClosedPattern] -> Phantoms.TTerm Syntax.OrPattern)
orPattern x = (Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
  Core.wrappedTermTypeName = (Core.Name "hydra.ext.python.syntax.OrPattern"),
  Core.wrappedTermBody = (Phantoms.unTTerm x)})))

unOrPattern :: (Phantoms.TTerm Syntax.OrPattern -> Phantoms.TTerm [Syntax.ClosedPattern])
unOrPattern x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.python.syntax.OrPattern")))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

closedPatternLiteral :: (Phantoms.TTerm Syntax.LiteralExpression -> Phantoms.TTerm Syntax.ClosedPattern)
closedPatternLiteral x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.ClosedPattern"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "literal"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

closedPatternCapture :: (Phantoms.TTerm Syntax.CapturePattern -> Phantoms.TTerm Syntax.ClosedPattern)
closedPatternCapture x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.ClosedPattern"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "capture"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

closedPatternWildcard :: (Phantoms.TTerm Syntax.ClosedPattern)
closedPatternWildcard = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.ClosedPattern"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "wildcard"),
    Core.fieldTerm = Core.TermUnit}})))

closedPatternValue :: (Phantoms.TTerm Syntax.ValuePattern -> Phantoms.TTerm Syntax.ClosedPattern)
closedPatternValue x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.ClosedPattern"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "value"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

closedPatternGroup :: (Phantoms.TTerm Syntax.GroupPattern -> Phantoms.TTerm Syntax.ClosedPattern)
closedPatternGroup x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.ClosedPattern"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "group"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

closedPatternSequence :: (Phantoms.TTerm Syntax.SequencePattern -> Phantoms.TTerm Syntax.ClosedPattern)
closedPatternSequence x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.ClosedPattern"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "sequence"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

closedPatternMapping :: (Phantoms.TTerm Syntax.MappingPattern -> Phantoms.TTerm Syntax.ClosedPattern)
closedPatternMapping x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.ClosedPattern"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "mapping"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

closedPatternClass :: (Phantoms.TTerm Syntax.ClassPattern -> Phantoms.TTerm Syntax.ClosedPattern)
closedPatternClass x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.ClosedPattern"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "class"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

literalExpressionNumber :: (Phantoms.TTerm Syntax.SignedNumber -> Phantoms.TTerm Syntax.LiteralExpression)
literalExpressionNumber x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.LiteralExpression"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "number"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

literalExpressionComplex :: (Phantoms.TTerm Syntax.ComplexNumber -> Phantoms.TTerm Syntax.LiteralExpression)
literalExpressionComplex x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.LiteralExpression"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "complex"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

literalExpressionString :: (Phantoms.TTerm String -> Phantoms.TTerm Syntax.LiteralExpression)
literalExpressionString x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.LiteralExpression"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "string"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

literalExpressionNone :: (Phantoms.TTerm Syntax.LiteralExpression)
literalExpressionNone = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.LiteralExpression"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "none"),
    Core.fieldTerm = Core.TermUnit}})))

literalExpressionTrue :: (Phantoms.TTerm Syntax.LiteralExpression)
literalExpressionTrue = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.LiteralExpression"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "true"),
    Core.fieldTerm = Core.TermUnit}})))

literalExpressionFalse :: (Phantoms.TTerm Syntax.LiteralExpression)
literalExpressionFalse = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.LiteralExpression"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "false"),
    Core.fieldTerm = Core.TermUnit}})))

complexNumber :: (Phantoms.TTerm Syntax.SignedRealNumber -> Phantoms.TTerm Syntax.PlusOrMinus -> Phantoms.TTerm Syntax.ImaginaryNumber -> Phantoms.TTerm Syntax.ComplexNumber)
complexNumber real plusOrMinus imaginary = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.ComplexNumber"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "real"),
      Core.fieldTerm = (Phantoms.unTTerm real)},
    Core.Field {
      Core.fieldName = (Core.Name "plusOrMinus"),
      Core.fieldTerm = (Phantoms.unTTerm plusOrMinus)},
    Core.Field {
      Core.fieldName = (Core.Name "imaginary"),
      Core.fieldTerm = (Phantoms.unTTerm imaginary)}]})))

complexNumberReal :: (Phantoms.TTerm Syntax.ComplexNumber -> Phantoms.TTerm Syntax.SignedRealNumber)
complexNumberReal x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.ComplexNumber"),
    Core.projectionField = (Core.Name "real")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

complexNumberPlusOrMinus :: (Phantoms.TTerm Syntax.ComplexNumber -> Phantoms.TTerm Syntax.PlusOrMinus)
complexNumberPlusOrMinus x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.ComplexNumber"),
    Core.projectionField = (Core.Name "plusOrMinus")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

complexNumberImaginary :: (Phantoms.TTerm Syntax.ComplexNumber -> Phantoms.TTerm Syntax.ImaginaryNumber)
complexNumberImaginary x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.ComplexNumber"),
    Core.projectionField = (Core.Name "imaginary")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

complexNumberWithReal :: (Phantoms.TTerm Syntax.ComplexNumber -> Phantoms.TTerm Syntax.SignedRealNumber -> Phantoms.TTerm Syntax.ComplexNumber)
complexNumberWithReal original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.ComplexNumber"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "real"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "plusOrMinus"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.ComplexNumber"),
          Core.projectionField = (Core.Name "plusOrMinus")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "imaginary"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.ComplexNumber"),
          Core.projectionField = (Core.Name "imaginary")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

complexNumberWithPlusOrMinus :: (Phantoms.TTerm Syntax.ComplexNumber -> Phantoms.TTerm Syntax.PlusOrMinus -> Phantoms.TTerm Syntax.ComplexNumber)
complexNumberWithPlusOrMinus original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.ComplexNumber"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "real"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.ComplexNumber"),
          Core.projectionField = (Core.Name "real")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "plusOrMinus"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "imaginary"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.ComplexNumber"),
          Core.projectionField = (Core.Name "imaginary")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

complexNumberWithImaginary :: (Phantoms.TTerm Syntax.ComplexNumber -> Phantoms.TTerm Syntax.ImaginaryNumber -> Phantoms.TTerm Syntax.ComplexNumber)
complexNumberWithImaginary original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.ComplexNumber"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "real"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.ComplexNumber"),
          Core.projectionField = (Core.Name "real")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "plusOrMinus"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.ComplexNumber"),
          Core.projectionField = (Core.Name "plusOrMinus")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "imaginary"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

plusOrMinusPlus :: (Phantoms.TTerm Syntax.PlusOrMinus)
plusOrMinusPlus = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.PlusOrMinus"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "plus"),
    Core.fieldTerm = Core.TermUnit}})))

plusOrMinusMinus :: (Phantoms.TTerm Syntax.PlusOrMinus)
plusOrMinusMinus = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.PlusOrMinus"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "minus"),
    Core.fieldTerm = Core.TermUnit}})))

signedNumberSign :: (Phantoms.TTerm Syntax.PlusOrMinus -> Phantoms.TTerm Syntax.SignedNumber)
signedNumberSign x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.SignedNumber"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "sign"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

signedNumberNumber :: (Phantoms.TTerm Syntax.Number -> Phantoms.TTerm Syntax.SignedNumber)
signedNumberNumber x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.SignedNumber"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "number"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

signedRealNumberSign :: (Phantoms.TTerm Syntax.PlusOrMinus -> Phantoms.TTerm Syntax.SignedRealNumber)
signedRealNumberSign x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.SignedRealNumber"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "sign"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

signedRealNumberNumber :: (Phantoms.TTerm Syntax.RealNumber -> Phantoms.TTerm Syntax.SignedRealNumber)
signedRealNumberNumber x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.SignedRealNumber"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "number"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

realNumber :: (Phantoms.TTerm Syntax.Number -> Phantoms.TTerm Syntax.RealNumber)
realNumber x = (Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
  Core.wrappedTermTypeName = (Core.Name "hydra.ext.python.syntax.RealNumber"),
  Core.wrappedTermBody = (Phantoms.unTTerm x)})))

unRealNumber :: (Phantoms.TTerm Syntax.RealNumber -> Phantoms.TTerm Syntax.Number)
unRealNumber x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.python.syntax.RealNumber")))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

imaginaryNumber :: (Phantoms.TTerm Syntax.Number -> Phantoms.TTerm Syntax.ImaginaryNumber)
imaginaryNumber x = (Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
  Core.wrappedTermTypeName = (Core.Name "hydra.ext.python.syntax.ImaginaryNumber"),
  Core.wrappedTermBody = (Phantoms.unTTerm x)})))

unImaginaryNumber :: (Phantoms.TTerm Syntax.ImaginaryNumber -> Phantoms.TTerm Syntax.Number)
unImaginaryNumber x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.python.syntax.ImaginaryNumber")))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

capturePattern :: (Phantoms.TTerm Syntax.PatternCaptureTarget -> Phantoms.TTerm Syntax.CapturePattern)
capturePattern x = (Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
  Core.wrappedTermTypeName = (Core.Name "hydra.ext.python.syntax.CapturePattern"),
  Core.wrappedTermBody = (Phantoms.unTTerm x)})))

unCapturePattern :: (Phantoms.TTerm Syntax.CapturePattern -> Phantoms.TTerm Syntax.PatternCaptureTarget)
unCapturePattern x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.python.syntax.CapturePattern")))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

patternCaptureTarget :: (Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.PatternCaptureTarget)
patternCaptureTarget x = (Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
  Core.wrappedTermTypeName = (Core.Name "hydra.ext.python.syntax.PatternCaptureTarget"),
  Core.wrappedTermBody = (Phantoms.unTTerm x)})))

unPatternCaptureTarget :: (Phantoms.TTerm Syntax.PatternCaptureTarget -> Phantoms.TTerm Syntax.Name)
unPatternCaptureTarget x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.python.syntax.PatternCaptureTarget")))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

valuePattern :: (Phantoms.TTerm Syntax.Attribute -> Phantoms.TTerm Syntax.ValuePattern)
valuePattern x = (Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
  Core.wrappedTermTypeName = (Core.Name "hydra.ext.python.syntax.ValuePattern"),
  Core.wrappedTermBody = (Phantoms.unTTerm x)})))

unValuePattern :: (Phantoms.TTerm Syntax.ValuePattern -> Phantoms.TTerm Syntax.Attribute)
unValuePattern x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.python.syntax.ValuePattern")))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

attribute :: (Phantoms.TTerm [Syntax.Name] -> Phantoms.TTerm Syntax.Attribute)
attribute x = (Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
  Core.wrappedTermTypeName = (Core.Name "hydra.ext.python.syntax.Attribute"),
  Core.wrappedTermBody = (Phantoms.unTTerm x)})))

unAttribute :: (Phantoms.TTerm Syntax.Attribute -> Phantoms.TTerm [Syntax.Name])
unAttribute x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.python.syntax.Attribute")))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

nameOrAttribute :: (Phantoms.TTerm [Syntax.Name] -> Phantoms.TTerm Syntax.NameOrAttribute)
nameOrAttribute x = (Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
  Core.wrappedTermTypeName = (Core.Name "hydra.ext.python.syntax.NameOrAttribute"),
  Core.wrappedTermBody = (Phantoms.unTTerm x)})))

unNameOrAttribute :: (Phantoms.TTerm Syntax.NameOrAttribute -> Phantoms.TTerm [Syntax.Name])
unNameOrAttribute x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.python.syntax.NameOrAttribute")))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

groupPattern :: (Phantoms.TTerm Syntax.Pattern -> Phantoms.TTerm Syntax.GroupPattern)
groupPattern x = (Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
  Core.wrappedTermTypeName = (Core.Name "hydra.ext.python.syntax.GroupPattern"),
  Core.wrappedTermBody = (Phantoms.unTTerm x)})))

unGroupPattern :: (Phantoms.TTerm Syntax.GroupPattern -> Phantoms.TTerm Syntax.Pattern)
unGroupPattern x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.python.syntax.GroupPattern")))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

sequencePatternList :: (Phantoms.TTerm (Maybe Syntax.MaybeSequencePattern) -> Phantoms.TTerm Syntax.SequencePattern)
sequencePatternList x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.SequencePattern"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "list"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

sequencePatternTuple :: (Phantoms.TTerm (Maybe Syntax.OpenSequencePattern) -> Phantoms.TTerm Syntax.SequencePattern)
sequencePatternTuple x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.SequencePattern"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "tuple"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

openSequencePattern :: (Phantoms.TTerm Syntax.MaybeStarPattern -> Phantoms.TTerm (Maybe Syntax.MaybeSequencePattern) -> Phantoms.TTerm Syntax.OpenSequencePattern)
openSequencePattern head tail = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.OpenSequencePattern"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "head"),
      Core.fieldTerm = (Phantoms.unTTerm head)},
    Core.Field {
      Core.fieldName = (Core.Name "tail"),
      Core.fieldTerm = (Phantoms.unTTerm tail)}]})))

openSequencePatternHead :: (Phantoms.TTerm Syntax.OpenSequencePattern -> Phantoms.TTerm Syntax.MaybeStarPattern)
openSequencePatternHead x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.OpenSequencePattern"),
    Core.projectionField = (Core.Name "head")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

openSequencePatternTail :: (Phantoms.TTerm Syntax.OpenSequencePattern -> Phantoms.TTerm (Maybe Syntax.MaybeSequencePattern))
openSequencePatternTail x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.OpenSequencePattern"),
    Core.projectionField = (Core.Name "tail")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

openSequencePatternWithHead :: (Phantoms.TTerm Syntax.OpenSequencePattern -> Phantoms.TTerm Syntax.MaybeStarPattern -> Phantoms.TTerm Syntax.OpenSequencePattern)
openSequencePatternWithHead original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.OpenSequencePattern"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "head"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "tail"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.OpenSequencePattern"),
          Core.projectionField = (Core.Name "tail")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

openSequencePatternWithTail :: (Phantoms.TTerm Syntax.OpenSequencePattern -> Phantoms.TTerm (Maybe Syntax.MaybeSequencePattern) -> Phantoms.TTerm Syntax.OpenSequencePattern)
openSequencePatternWithTail original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.OpenSequencePattern"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "head"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.OpenSequencePattern"),
          Core.projectionField = (Core.Name "head")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "tail"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

maybeSequencePattern :: (Phantoms.TTerm [Syntax.MaybeStarPattern] -> Phantoms.TTerm Syntax.MaybeSequencePattern)
maybeSequencePattern x = (Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
  Core.wrappedTermTypeName = (Core.Name "hydra.ext.python.syntax.MaybeSequencePattern"),
  Core.wrappedTermBody = (Phantoms.unTTerm x)})))

unMaybeSequencePattern :: (Phantoms.TTerm Syntax.MaybeSequencePattern -> Phantoms.TTerm [Syntax.MaybeStarPattern])
unMaybeSequencePattern x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.python.syntax.MaybeSequencePattern")))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

maybeStarPatternStar :: (Phantoms.TTerm Syntax.StarPattern -> Phantoms.TTerm Syntax.MaybeStarPattern)
maybeStarPatternStar x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.MaybeStarPattern"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "star"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

maybeStarPatternPattern :: (Phantoms.TTerm Syntax.Pattern -> Phantoms.TTerm Syntax.MaybeStarPattern)
maybeStarPatternPattern x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.MaybeStarPattern"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "pattern"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

starPatternCapture :: (Phantoms.TTerm Syntax.PatternCaptureTarget -> Phantoms.TTerm Syntax.StarPattern)
starPatternCapture x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.StarPattern"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "capture"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

starPatternWildcard :: (Phantoms.TTerm Syntax.StarPattern)
starPatternWildcard = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.StarPattern"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "wildcard"),
    Core.fieldTerm = Core.TermUnit}})))

mappingPattern :: (Phantoms.TTerm (Maybe Syntax.ItemsPattern) -> Phantoms.TTerm (Maybe Syntax.DoubleStarPattern) -> Phantoms.TTerm Syntax.MappingPattern)
mappingPattern items doubleStar = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.MappingPattern"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "items"),
      Core.fieldTerm = (Phantoms.unTTerm items)},
    Core.Field {
      Core.fieldName = (Core.Name "doubleStar"),
      Core.fieldTerm = (Phantoms.unTTerm doubleStar)}]})))

mappingPatternItems :: (Phantoms.TTerm Syntax.MappingPattern -> Phantoms.TTerm (Maybe Syntax.ItemsPattern))
mappingPatternItems x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.MappingPattern"),
    Core.projectionField = (Core.Name "items")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

mappingPatternDoubleStar :: (Phantoms.TTerm Syntax.MappingPattern -> Phantoms.TTerm (Maybe Syntax.DoubleStarPattern))
mappingPatternDoubleStar x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.MappingPattern"),
    Core.projectionField = (Core.Name "doubleStar")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

mappingPatternWithItems :: (Phantoms.TTerm Syntax.MappingPattern -> Phantoms.TTerm (Maybe Syntax.ItemsPattern) -> Phantoms.TTerm Syntax.MappingPattern)
mappingPatternWithItems original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.MappingPattern"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "items"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "doubleStar"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.MappingPattern"),
          Core.projectionField = (Core.Name "doubleStar")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

mappingPatternWithDoubleStar :: (Phantoms.TTerm Syntax.MappingPattern -> Phantoms.TTerm (Maybe Syntax.DoubleStarPattern) -> Phantoms.TTerm Syntax.MappingPattern)
mappingPatternWithDoubleStar original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.MappingPattern"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "items"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.MappingPattern"),
          Core.projectionField = (Core.Name "items")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "doubleStar"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

itemsPattern :: (Phantoms.TTerm [Syntax.KeyValuePattern] -> Phantoms.TTerm Syntax.ItemsPattern)
itemsPattern x = (Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
  Core.wrappedTermTypeName = (Core.Name "hydra.ext.python.syntax.ItemsPattern"),
  Core.wrappedTermBody = (Phantoms.unTTerm x)})))

unItemsPattern :: (Phantoms.TTerm Syntax.ItemsPattern -> Phantoms.TTerm [Syntax.KeyValuePattern])
unItemsPattern x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.python.syntax.ItemsPattern")))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

keyValuePattern :: (Phantoms.TTerm Syntax.LiteralExpressionOrAttribute -> Phantoms.TTerm Syntax.Pattern -> Phantoms.TTerm Syntax.KeyValuePattern)
keyValuePattern key value = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.KeyValuePattern"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "key"),
      Core.fieldTerm = (Phantoms.unTTerm key)},
    Core.Field {
      Core.fieldName = (Core.Name "value"),
      Core.fieldTerm = (Phantoms.unTTerm value)}]})))

keyValuePatternKey :: (Phantoms.TTerm Syntax.KeyValuePattern -> Phantoms.TTerm Syntax.LiteralExpressionOrAttribute)
keyValuePatternKey x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.KeyValuePattern"),
    Core.projectionField = (Core.Name "key")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

keyValuePatternValue :: (Phantoms.TTerm Syntax.KeyValuePattern -> Phantoms.TTerm Syntax.Pattern)
keyValuePatternValue x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.KeyValuePattern"),
    Core.projectionField = (Core.Name "value")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

keyValuePatternWithKey :: (Phantoms.TTerm Syntax.KeyValuePattern -> Phantoms.TTerm Syntax.LiteralExpressionOrAttribute -> Phantoms.TTerm Syntax.KeyValuePattern)
keyValuePatternWithKey original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.KeyValuePattern"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "key"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "value"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.KeyValuePattern"),
          Core.projectionField = (Core.Name "value")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

keyValuePatternWithValue :: (Phantoms.TTerm Syntax.KeyValuePattern -> Phantoms.TTerm Syntax.Pattern -> Phantoms.TTerm Syntax.KeyValuePattern)
keyValuePatternWithValue original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.KeyValuePattern"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "key"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.KeyValuePattern"),
          Core.projectionField = (Core.Name "key")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "value"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

literalExpressionOrAttributeLiteral :: (Phantoms.TTerm Syntax.LiteralExpression -> Phantoms.TTerm Syntax.LiteralExpressionOrAttribute)
literalExpressionOrAttributeLiteral x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.LiteralExpressionOrAttribute"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "literal"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

literalExpressionOrAttributeAttribute :: (Phantoms.TTerm Syntax.Attribute -> Phantoms.TTerm Syntax.LiteralExpressionOrAttribute)
literalExpressionOrAttributeAttribute x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.LiteralExpressionOrAttribute"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "attribute"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

doubleStarPattern :: (Phantoms.TTerm Syntax.PatternCaptureTarget -> Phantoms.TTerm Syntax.DoubleStarPattern)
doubleStarPattern x = (Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
  Core.wrappedTermTypeName = (Core.Name "hydra.ext.python.syntax.DoubleStarPattern"),
  Core.wrappedTermBody = (Phantoms.unTTerm x)})))

unDoubleStarPattern :: (Phantoms.TTerm Syntax.DoubleStarPattern -> Phantoms.TTerm Syntax.PatternCaptureTarget)
unDoubleStarPattern x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.python.syntax.DoubleStarPattern")))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

classPattern :: (Phantoms.TTerm Syntax.NameOrAttribute -> Phantoms.TTerm (Maybe Syntax.PositionalPatterns) -> Phantoms.TTerm (Maybe Syntax.KeywordPatterns) -> Phantoms.TTerm Syntax.ClassPattern)
classPattern nameOrAttribute positionalPatterns keywordPatterns = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.ClassPattern"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "nameOrAttribute"),
      Core.fieldTerm = (Phantoms.unTTerm nameOrAttribute)},
    Core.Field {
      Core.fieldName = (Core.Name "positionalPatterns"),
      Core.fieldTerm = (Phantoms.unTTerm positionalPatterns)},
    Core.Field {
      Core.fieldName = (Core.Name "keywordPatterns"),
      Core.fieldTerm = (Phantoms.unTTerm keywordPatterns)}]})))

classPatternNameOrAttribute :: (Phantoms.TTerm Syntax.ClassPattern -> Phantoms.TTerm Syntax.NameOrAttribute)
classPatternNameOrAttribute x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.ClassPattern"),
    Core.projectionField = (Core.Name "nameOrAttribute")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

classPatternPositionalPatterns :: (Phantoms.TTerm Syntax.ClassPattern -> Phantoms.TTerm (Maybe Syntax.PositionalPatterns))
classPatternPositionalPatterns x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.ClassPattern"),
    Core.projectionField = (Core.Name "positionalPatterns")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

classPatternKeywordPatterns :: (Phantoms.TTerm Syntax.ClassPattern -> Phantoms.TTerm (Maybe Syntax.KeywordPatterns))
classPatternKeywordPatterns x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.ClassPattern"),
    Core.projectionField = (Core.Name "keywordPatterns")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

classPatternWithNameOrAttribute :: (Phantoms.TTerm Syntax.ClassPattern -> Phantoms.TTerm Syntax.NameOrAttribute -> Phantoms.TTerm Syntax.ClassPattern)
classPatternWithNameOrAttribute original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.ClassPattern"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "nameOrAttribute"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "positionalPatterns"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.ClassPattern"),
          Core.projectionField = (Core.Name "positionalPatterns")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "keywordPatterns"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.ClassPattern"),
          Core.projectionField = (Core.Name "keywordPatterns")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

classPatternWithPositionalPatterns :: (Phantoms.TTerm Syntax.ClassPattern -> Phantoms.TTerm (Maybe Syntax.PositionalPatterns) -> Phantoms.TTerm Syntax.ClassPattern)
classPatternWithPositionalPatterns original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.ClassPattern"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "nameOrAttribute"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.ClassPattern"),
          Core.projectionField = (Core.Name "nameOrAttribute")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "positionalPatterns"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "keywordPatterns"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.ClassPattern"),
          Core.projectionField = (Core.Name "keywordPatterns")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

classPatternWithKeywordPatterns :: (Phantoms.TTerm Syntax.ClassPattern -> Phantoms.TTerm (Maybe Syntax.KeywordPatterns) -> Phantoms.TTerm Syntax.ClassPattern)
classPatternWithKeywordPatterns original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.ClassPattern"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "nameOrAttribute"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.ClassPattern"),
          Core.projectionField = (Core.Name "nameOrAttribute")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "positionalPatterns"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.ClassPattern"),
          Core.projectionField = (Core.Name "positionalPatterns")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "keywordPatterns"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

positionalPatterns :: (Phantoms.TTerm [Syntax.Pattern] -> Phantoms.TTerm Syntax.PositionalPatterns)
positionalPatterns x = (Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
  Core.wrappedTermTypeName = (Core.Name "hydra.ext.python.syntax.PositionalPatterns"),
  Core.wrappedTermBody = (Phantoms.unTTerm x)})))

unPositionalPatterns :: (Phantoms.TTerm Syntax.PositionalPatterns -> Phantoms.TTerm [Syntax.Pattern])
unPositionalPatterns x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.python.syntax.PositionalPatterns")))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

keywordPatterns :: (Phantoms.TTerm [Syntax.KeywordPattern] -> Phantoms.TTerm Syntax.KeywordPatterns)
keywordPatterns x = (Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
  Core.wrappedTermTypeName = (Core.Name "hydra.ext.python.syntax.KeywordPatterns"),
  Core.wrappedTermBody = (Phantoms.unTTerm x)})))

unKeywordPatterns :: (Phantoms.TTerm Syntax.KeywordPatterns -> Phantoms.TTerm [Syntax.KeywordPattern])
unKeywordPatterns x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.python.syntax.KeywordPatterns")))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

keywordPattern :: (Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.Pattern -> Phantoms.TTerm Syntax.KeywordPattern)
keywordPattern name pattern = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.KeywordPattern"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Phantoms.unTTerm name)},
    Core.Field {
      Core.fieldName = (Core.Name "pattern"),
      Core.fieldTerm = (Phantoms.unTTerm pattern)}]})))

keywordPatternName :: (Phantoms.TTerm Syntax.KeywordPattern -> Phantoms.TTerm Syntax.Name)
keywordPatternName x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.KeywordPattern"),
    Core.projectionField = (Core.Name "name")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

keywordPatternPattern :: (Phantoms.TTerm Syntax.KeywordPattern -> Phantoms.TTerm Syntax.Pattern)
keywordPatternPattern x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.KeywordPattern"),
    Core.projectionField = (Core.Name "pattern")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

keywordPatternWithName :: (Phantoms.TTerm Syntax.KeywordPattern -> Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.KeywordPattern)
keywordPatternWithName original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.KeywordPattern"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "pattern"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.KeywordPattern"),
          Core.projectionField = (Core.Name "pattern")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

keywordPatternWithPattern :: (Phantoms.TTerm Syntax.KeywordPattern -> Phantoms.TTerm Syntax.Pattern -> Phantoms.TTerm Syntax.KeywordPattern)
keywordPatternWithPattern original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.KeywordPattern"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.KeywordPattern"),
          Core.projectionField = (Core.Name "name")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "pattern"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

typeAlias :: (Phantoms.TTerm Syntax.Name -> Phantoms.TTerm [Syntax.TypeParameter] -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.TypeAlias)
typeAlias name typeParams expression = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.TypeAlias"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Phantoms.unTTerm name)},
    Core.Field {
      Core.fieldName = (Core.Name "typeParams"),
      Core.fieldTerm = (Phantoms.unTTerm typeParams)},
    Core.Field {
      Core.fieldName = (Core.Name "expression"),
      Core.fieldTerm = (Phantoms.unTTerm expression)}]})))

typeAliasName :: (Phantoms.TTerm Syntax.TypeAlias -> Phantoms.TTerm Syntax.Name)
typeAliasName x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.TypeAlias"),
    Core.projectionField = (Core.Name "name")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

typeAliasTypeParams :: (Phantoms.TTerm Syntax.TypeAlias -> Phantoms.TTerm [Syntax.TypeParameter])
typeAliasTypeParams x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.TypeAlias"),
    Core.projectionField = (Core.Name "typeParams")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

typeAliasExpression :: (Phantoms.TTerm Syntax.TypeAlias -> Phantoms.TTerm Syntax.Expression)
typeAliasExpression x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.TypeAlias"),
    Core.projectionField = (Core.Name "expression")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

typeAliasWithName :: (Phantoms.TTerm Syntax.TypeAlias -> Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.TypeAlias)
typeAliasWithName original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.TypeAlias"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "typeParams"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.TypeAlias"),
          Core.projectionField = (Core.Name "typeParams")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "expression"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.TypeAlias"),
          Core.projectionField = (Core.Name "expression")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

typeAliasWithTypeParams :: (Phantoms.TTerm Syntax.TypeAlias -> Phantoms.TTerm [Syntax.TypeParameter] -> Phantoms.TTerm Syntax.TypeAlias)
typeAliasWithTypeParams original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.TypeAlias"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.TypeAlias"),
          Core.projectionField = (Core.Name "name")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "typeParams"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "expression"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.TypeAlias"),
          Core.projectionField = (Core.Name "expression")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

typeAliasWithExpression :: (Phantoms.TTerm Syntax.TypeAlias -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.TypeAlias)
typeAliasWithExpression original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.TypeAlias"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.TypeAlias"),
          Core.projectionField = (Core.Name "name")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "typeParams"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.TypeAlias"),
          Core.projectionField = (Core.Name "typeParams")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "expression"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

typeParameterSimple :: (Phantoms.TTerm Syntax.SimpleTypeParameter -> Phantoms.TTerm Syntax.TypeParameter)
typeParameterSimple x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.TypeParameter"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "simple"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

typeParameterStar :: (Phantoms.TTerm Syntax.StarTypeParameter -> Phantoms.TTerm Syntax.TypeParameter)
typeParameterStar x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.TypeParameter"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "star"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

typeParameterDoubleStar :: (Phantoms.TTerm Syntax.DoubleStarTypeParameter -> Phantoms.TTerm Syntax.TypeParameter)
typeParameterDoubleStar x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.TypeParameter"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "doubleStar"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

simpleTypeParameter :: (Phantoms.TTerm Syntax.Name -> Phantoms.TTerm (Maybe Syntax.Expression) -> Phantoms.TTerm (Maybe Syntax.Expression) -> Phantoms.TTerm Syntax.SimpleTypeParameter)
simpleTypeParameter name bound default_ = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.SimpleTypeParameter"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Phantoms.unTTerm name)},
    Core.Field {
      Core.fieldName = (Core.Name "bound"),
      Core.fieldTerm = (Phantoms.unTTerm bound)},
    Core.Field {
      Core.fieldName = (Core.Name "default"),
      Core.fieldTerm = (Phantoms.unTTerm default_)}]})))

simpleTypeParameterName :: (Phantoms.TTerm Syntax.SimpleTypeParameter -> Phantoms.TTerm Syntax.Name)
simpleTypeParameterName x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.SimpleTypeParameter"),
    Core.projectionField = (Core.Name "name")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

simpleTypeParameterBound :: (Phantoms.TTerm Syntax.SimpleTypeParameter -> Phantoms.TTerm (Maybe Syntax.Expression))
simpleTypeParameterBound x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.SimpleTypeParameter"),
    Core.projectionField = (Core.Name "bound")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

simpleTypeParameterDefault :: (Phantoms.TTerm Syntax.SimpleTypeParameter -> Phantoms.TTerm (Maybe Syntax.Expression))
simpleTypeParameterDefault x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.SimpleTypeParameter"),
    Core.projectionField = (Core.Name "default")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

simpleTypeParameterWithName :: (Phantoms.TTerm Syntax.SimpleTypeParameter -> Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.SimpleTypeParameter)
simpleTypeParameterWithName original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.SimpleTypeParameter"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "bound"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.SimpleTypeParameter"),
          Core.projectionField = (Core.Name "bound")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "default"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.SimpleTypeParameter"),
          Core.projectionField = (Core.Name "default")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

simpleTypeParameterWithBound :: (Phantoms.TTerm Syntax.SimpleTypeParameter -> Phantoms.TTerm (Maybe Syntax.Expression) -> Phantoms.TTerm Syntax.SimpleTypeParameter)
simpleTypeParameterWithBound original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.SimpleTypeParameter"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.SimpleTypeParameter"),
          Core.projectionField = (Core.Name "name")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "bound"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "default"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.SimpleTypeParameter"),
          Core.projectionField = (Core.Name "default")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

simpleTypeParameterWithDefault :: (Phantoms.TTerm Syntax.SimpleTypeParameter -> Phantoms.TTerm (Maybe Syntax.Expression) -> Phantoms.TTerm Syntax.SimpleTypeParameter)
simpleTypeParameterWithDefault original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.SimpleTypeParameter"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.SimpleTypeParameter"),
          Core.projectionField = (Core.Name "name")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "bound"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.SimpleTypeParameter"),
          Core.projectionField = (Core.Name "bound")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "default"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

starTypeParameter :: (Phantoms.TTerm Syntax.Name -> Phantoms.TTerm (Maybe Syntax.StarExpression) -> Phantoms.TTerm Syntax.StarTypeParameter)
starTypeParameter name default_ = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.StarTypeParameter"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Phantoms.unTTerm name)},
    Core.Field {
      Core.fieldName = (Core.Name "default"),
      Core.fieldTerm = (Phantoms.unTTerm default_)}]})))

starTypeParameterName :: (Phantoms.TTerm Syntax.StarTypeParameter -> Phantoms.TTerm Syntax.Name)
starTypeParameterName x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.StarTypeParameter"),
    Core.projectionField = (Core.Name "name")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

starTypeParameterDefault :: (Phantoms.TTerm Syntax.StarTypeParameter -> Phantoms.TTerm (Maybe Syntax.StarExpression))
starTypeParameterDefault x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.StarTypeParameter"),
    Core.projectionField = (Core.Name "default")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

starTypeParameterWithName :: (Phantoms.TTerm Syntax.StarTypeParameter -> Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.StarTypeParameter)
starTypeParameterWithName original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.StarTypeParameter"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "default"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.StarTypeParameter"),
          Core.projectionField = (Core.Name "default")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

starTypeParameterWithDefault :: (Phantoms.TTerm Syntax.StarTypeParameter -> Phantoms.TTerm (Maybe Syntax.StarExpression) -> Phantoms.TTerm Syntax.StarTypeParameter)
starTypeParameterWithDefault original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.StarTypeParameter"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.StarTypeParameter"),
          Core.projectionField = (Core.Name "name")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "default"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

doubleStarTypeParameter :: (Phantoms.TTerm Syntax.Name -> Phantoms.TTerm (Maybe Syntax.Expression) -> Phantoms.TTerm Syntax.DoubleStarTypeParameter)
doubleStarTypeParameter name default_ = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.DoubleStarTypeParameter"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Phantoms.unTTerm name)},
    Core.Field {
      Core.fieldName = (Core.Name "default"),
      Core.fieldTerm = (Phantoms.unTTerm default_)}]})))

doubleStarTypeParameterName :: (Phantoms.TTerm Syntax.DoubleStarTypeParameter -> Phantoms.TTerm Syntax.Name)
doubleStarTypeParameterName x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.DoubleStarTypeParameter"),
    Core.projectionField = (Core.Name "name")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

doubleStarTypeParameterDefault :: (Phantoms.TTerm Syntax.DoubleStarTypeParameter -> Phantoms.TTerm (Maybe Syntax.Expression))
doubleStarTypeParameterDefault x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.DoubleStarTypeParameter"),
    Core.projectionField = (Core.Name "default")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

doubleStarTypeParameterWithName :: (Phantoms.TTerm Syntax.DoubleStarTypeParameter -> Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.DoubleStarTypeParameter)
doubleStarTypeParameterWithName original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.DoubleStarTypeParameter"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "default"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.DoubleStarTypeParameter"),
          Core.projectionField = (Core.Name "default")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

doubleStarTypeParameterWithDefault :: (Phantoms.TTerm Syntax.DoubleStarTypeParameter -> Phantoms.TTerm (Maybe Syntax.Expression) -> Phantoms.TTerm Syntax.DoubleStarTypeParameter)
doubleStarTypeParameterWithDefault original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.DoubleStarTypeParameter"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.DoubleStarTypeParameter"),
          Core.projectionField = (Core.Name "name")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "default"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

expressionConditional :: (Phantoms.TTerm Syntax.Conditional -> Phantoms.TTerm Syntax.Expression)
expressionConditional x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.Expression"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "conditional"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

expressionSimple :: (Phantoms.TTerm Syntax.Disjunction -> Phantoms.TTerm Syntax.Expression)
expressionSimple x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.Expression"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "simple"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

expressionLambda :: (Phantoms.TTerm Syntax.Lambda -> Phantoms.TTerm Syntax.Expression)
expressionLambda x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.Expression"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "lambda"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

conditional :: (Phantoms.TTerm Syntax.Disjunction -> Phantoms.TTerm Syntax.Disjunction -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.Conditional)
conditional body if_ else_ = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.Conditional"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "body"),
      Core.fieldTerm = (Phantoms.unTTerm body)},
    Core.Field {
      Core.fieldName = (Core.Name "if"),
      Core.fieldTerm = (Phantoms.unTTerm if_)},
    Core.Field {
      Core.fieldName = (Core.Name "else"),
      Core.fieldTerm = (Phantoms.unTTerm else_)}]})))

conditionalBody :: (Phantoms.TTerm Syntax.Conditional -> Phantoms.TTerm Syntax.Disjunction)
conditionalBody x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.Conditional"),
    Core.projectionField = (Core.Name "body")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

conditionalIf :: (Phantoms.TTerm Syntax.Conditional -> Phantoms.TTerm Syntax.Disjunction)
conditionalIf x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.Conditional"),
    Core.projectionField = (Core.Name "if")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

conditionalElse :: (Phantoms.TTerm Syntax.Conditional -> Phantoms.TTerm Syntax.Expression)
conditionalElse x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.Conditional"),
    Core.projectionField = (Core.Name "else")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

conditionalWithBody :: (Phantoms.TTerm Syntax.Conditional -> Phantoms.TTerm Syntax.Disjunction -> Phantoms.TTerm Syntax.Conditional)
conditionalWithBody original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.Conditional"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "body"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "if"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.Conditional"),
          Core.projectionField = (Core.Name "if")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "else"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.Conditional"),
          Core.projectionField = (Core.Name "else")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

conditionalWithIf :: (Phantoms.TTerm Syntax.Conditional -> Phantoms.TTerm Syntax.Disjunction -> Phantoms.TTerm Syntax.Conditional)
conditionalWithIf original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.Conditional"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "body"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.Conditional"),
          Core.projectionField = (Core.Name "body")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "if"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "else"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.Conditional"),
          Core.projectionField = (Core.Name "else")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

conditionalWithElse :: (Phantoms.TTerm Syntax.Conditional -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.Conditional)
conditionalWithElse original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.Conditional"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "body"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.Conditional"),
          Core.projectionField = (Core.Name "body")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "if"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.Conditional"),
          Core.projectionField = (Core.Name "if")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "else"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

yieldExpressionFrom :: (Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.YieldExpression)
yieldExpressionFrom x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.YieldExpression"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "from"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

yieldExpressionSimple :: (Phantoms.TTerm [Syntax.StarExpression] -> Phantoms.TTerm Syntax.YieldExpression)
yieldExpressionSimple x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.YieldExpression"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "simple"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

starExpressionStar :: (Phantoms.TTerm Syntax.BitwiseOr -> Phantoms.TTerm Syntax.StarExpression)
starExpressionStar x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.StarExpression"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "star"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

starExpressionSimple :: (Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.StarExpression)
starExpressionSimple x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.StarExpression"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "simple"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

starNamedExpressions :: (Phantoms.TTerm [Syntax.StarNamedExpression] -> Phantoms.TTerm Syntax.StarNamedExpressions)
starNamedExpressions x = (Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
  Core.wrappedTermTypeName = (Core.Name "hydra.ext.python.syntax.StarNamedExpressions"),
  Core.wrappedTermBody = (Phantoms.unTTerm x)})))

unStarNamedExpressions :: (Phantoms.TTerm Syntax.StarNamedExpressions -> Phantoms.TTerm [Syntax.StarNamedExpression])
unStarNamedExpressions x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.python.syntax.StarNamedExpressions")))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

starNamedExpressionStar :: (Phantoms.TTerm Syntax.BitwiseOr -> Phantoms.TTerm Syntax.StarNamedExpression)
starNamedExpressionStar x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.StarNamedExpression"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "star"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

starNamedExpressionSimple :: (Phantoms.TTerm Syntax.NamedExpression -> Phantoms.TTerm Syntax.StarNamedExpression)
starNamedExpressionSimple x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.StarNamedExpression"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "simple"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

assignmentExpression :: (Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.AssignmentExpression)
assignmentExpression name expression = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.AssignmentExpression"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Phantoms.unTTerm name)},
    Core.Field {
      Core.fieldName = (Core.Name "expression"),
      Core.fieldTerm = (Phantoms.unTTerm expression)}]})))

assignmentExpressionName :: (Phantoms.TTerm Syntax.AssignmentExpression -> Phantoms.TTerm Syntax.Name)
assignmentExpressionName x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.AssignmentExpression"),
    Core.projectionField = (Core.Name "name")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

assignmentExpressionExpression :: (Phantoms.TTerm Syntax.AssignmentExpression -> Phantoms.TTerm Syntax.Expression)
assignmentExpressionExpression x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.AssignmentExpression"),
    Core.projectionField = (Core.Name "expression")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

assignmentExpressionWithName :: (Phantoms.TTerm Syntax.AssignmentExpression -> Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.AssignmentExpression)
assignmentExpressionWithName original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.AssignmentExpression"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "expression"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.AssignmentExpression"),
          Core.projectionField = (Core.Name "expression")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

assignmentExpressionWithExpression :: (Phantoms.TTerm Syntax.AssignmentExpression -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.AssignmentExpression)
assignmentExpressionWithExpression original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.AssignmentExpression"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.AssignmentExpression"),
          Core.projectionField = (Core.Name "name")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "expression"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

namedExpressionAssignment :: (Phantoms.TTerm Syntax.AssignmentExpression -> Phantoms.TTerm Syntax.NamedExpression)
namedExpressionAssignment x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.NamedExpression"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "assignment"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

namedExpressionSimple :: (Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.NamedExpression)
namedExpressionSimple x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.NamedExpression"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "simple"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

disjunction :: (Phantoms.TTerm [Syntax.Conjunction] -> Phantoms.TTerm Syntax.Disjunction)
disjunction x = (Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
  Core.wrappedTermTypeName = (Core.Name "hydra.ext.python.syntax.Disjunction"),
  Core.wrappedTermBody = (Phantoms.unTTerm x)})))

unDisjunction :: (Phantoms.TTerm Syntax.Disjunction -> Phantoms.TTerm [Syntax.Conjunction])
unDisjunction x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.python.syntax.Disjunction")))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

conjunction :: (Phantoms.TTerm [Syntax.Inversion] -> Phantoms.TTerm Syntax.Conjunction)
conjunction x = (Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
  Core.wrappedTermTypeName = (Core.Name "hydra.ext.python.syntax.Conjunction"),
  Core.wrappedTermBody = (Phantoms.unTTerm x)})))

unConjunction :: (Phantoms.TTerm Syntax.Conjunction -> Phantoms.TTerm [Syntax.Inversion])
unConjunction x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.python.syntax.Conjunction")))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

inversionNot :: (Phantoms.TTerm Syntax.Inversion -> Phantoms.TTerm Syntax.Inversion)
inversionNot x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.Inversion"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "not"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

inversionSimple :: (Phantoms.TTerm Syntax.Comparison -> Phantoms.TTerm Syntax.Inversion)
inversionSimple x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.Inversion"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "simple"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

comparison :: (Phantoms.TTerm Syntax.BitwiseOr -> Phantoms.TTerm [Syntax.CompareOpBitwiseOrPair] -> Phantoms.TTerm Syntax.Comparison)
comparison lhs rhs = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.Comparison"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "lhs"),
      Core.fieldTerm = (Phantoms.unTTerm lhs)},
    Core.Field {
      Core.fieldName = (Core.Name "rhs"),
      Core.fieldTerm = (Phantoms.unTTerm rhs)}]})))

comparisonLhs :: (Phantoms.TTerm Syntax.Comparison -> Phantoms.TTerm Syntax.BitwiseOr)
comparisonLhs x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.Comparison"),
    Core.projectionField = (Core.Name "lhs")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

comparisonRhs :: (Phantoms.TTerm Syntax.Comparison -> Phantoms.TTerm [Syntax.CompareOpBitwiseOrPair])
comparisonRhs x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.Comparison"),
    Core.projectionField = (Core.Name "rhs")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

comparisonWithLhs :: (Phantoms.TTerm Syntax.Comparison -> Phantoms.TTerm Syntax.BitwiseOr -> Phantoms.TTerm Syntax.Comparison)
comparisonWithLhs original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.Comparison"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "lhs"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "rhs"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.Comparison"),
          Core.projectionField = (Core.Name "rhs")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

comparisonWithRhs :: (Phantoms.TTerm Syntax.Comparison -> Phantoms.TTerm [Syntax.CompareOpBitwiseOrPair] -> Phantoms.TTerm Syntax.Comparison)
comparisonWithRhs original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.Comparison"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "lhs"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.Comparison"),
          Core.projectionField = (Core.Name "lhs")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "rhs"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

compareOpBitwiseOrPair :: (Phantoms.TTerm Syntax.CompareOp -> Phantoms.TTerm Syntax.BitwiseOr -> Phantoms.TTerm Syntax.CompareOpBitwiseOrPair)
compareOpBitwiseOrPair operator rhs = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.CompareOpBitwiseOrPair"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "operator"),
      Core.fieldTerm = (Phantoms.unTTerm operator)},
    Core.Field {
      Core.fieldName = (Core.Name "rhs"),
      Core.fieldTerm = (Phantoms.unTTerm rhs)}]})))

compareOpBitwiseOrPairOperator :: (Phantoms.TTerm Syntax.CompareOpBitwiseOrPair -> Phantoms.TTerm Syntax.CompareOp)
compareOpBitwiseOrPairOperator x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.CompareOpBitwiseOrPair"),
    Core.projectionField = (Core.Name "operator")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

compareOpBitwiseOrPairRhs :: (Phantoms.TTerm Syntax.CompareOpBitwiseOrPair -> Phantoms.TTerm Syntax.BitwiseOr)
compareOpBitwiseOrPairRhs x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.CompareOpBitwiseOrPair"),
    Core.projectionField = (Core.Name "rhs")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

compareOpBitwiseOrPairWithOperator :: (Phantoms.TTerm Syntax.CompareOpBitwiseOrPair -> Phantoms.TTerm Syntax.CompareOp -> Phantoms.TTerm Syntax.CompareOpBitwiseOrPair)
compareOpBitwiseOrPairWithOperator original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.CompareOpBitwiseOrPair"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "operator"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "rhs"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.CompareOpBitwiseOrPair"),
          Core.projectionField = (Core.Name "rhs")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

compareOpBitwiseOrPairWithRhs :: (Phantoms.TTerm Syntax.CompareOpBitwiseOrPair -> Phantoms.TTerm Syntax.BitwiseOr -> Phantoms.TTerm Syntax.CompareOpBitwiseOrPair)
compareOpBitwiseOrPairWithRhs original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.CompareOpBitwiseOrPair"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "operator"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.CompareOpBitwiseOrPair"),
          Core.projectionField = (Core.Name "operator")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "rhs"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

compareOpEq :: (Phantoms.TTerm Syntax.CompareOp)
compareOpEq = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.CompareOp"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "eq"),
    Core.fieldTerm = Core.TermUnit}})))

compareOpNoteq :: (Phantoms.TTerm Syntax.CompareOp)
compareOpNoteq = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.CompareOp"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "noteq"),
    Core.fieldTerm = Core.TermUnit}})))

compareOpLte :: (Phantoms.TTerm Syntax.CompareOp)
compareOpLte = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.CompareOp"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "lte"),
    Core.fieldTerm = Core.TermUnit}})))

compareOpLt :: (Phantoms.TTerm Syntax.CompareOp)
compareOpLt = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.CompareOp"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "lt"),
    Core.fieldTerm = Core.TermUnit}})))

compareOpGte :: (Phantoms.TTerm Syntax.CompareOp)
compareOpGte = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.CompareOp"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "gte"),
    Core.fieldTerm = Core.TermUnit}})))

compareOpGt :: (Phantoms.TTerm Syntax.CompareOp)
compareOpGt = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.CompareOp"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "gt"),
    Core.fieldTerm = Core.TermUnit}})))

compareOpNotin :: (Phantoms.TTerm Syntax.CompareOp)
compareOpNotin = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.CompareOp"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "notin"),
    Core.fieldTerm = Core.TermUnit}})))

compareOpIn :: (Phantoms.TTerm Syntax.CompareOp)
compareOpIn = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.CompareOp"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "in"),
    Core.fieldTerm = Core.TermUnit}})))

compareOpIsnot :: (Phantoms.TTerm Syntax.CompareOp)
compareOpIsnot = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.CompareOp"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "isnot"),
    Core.fieldTerm = Core.TermUnit}})))

compareOpIs :: (Phantoms.TTerm Syntax.CompareOp)
compareOpIs = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.CompareOp"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "is"),
    Core.fieldTerm = Core.TermUnit}})))

bitwiseOr :: (Phantoms.TTerm (Maybe Syntax.BitwiseOr) -> Phantoms.TTerm Syntax.BitwiseXor -> Phantoms.TTerm Syntax.BitwiseOr)
bitwiseOr lhs rhs = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.BitwiseOr"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "lhs"),
      Core.fieldTerm = (Phantoms.unTTerm lhs)},
    Core.Field {
      Core.fieldName = (Core.Name "rhs"),
      Core.fieldTerm = (Phantoms.unTTerm rhs)}]})))

bitwiseOrLhs :: (Phantoms.TTerm Syntax.BitwiseOr -> Phantoms.TTerm (Maybe Syntax.BitwiseOr))
bitwiseOrLhs x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.BitwiseOr"),
    Core.projectionField = (Core.Name "lhs")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

bitwiseOrRhs :: (Phantoms.TTerm Syntax.BitwiseOr -> Phantoms.TTerm Syntax.BitwiseXor)
bitwiseOrRhs x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.BitwiseOr"),
    Core.projectionField = (Core.Name "rhs")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

bitwiseOrWithLhs :: (Phantoms.TTerm Syntax.BitwiseOr -> Phantoms.TTerm (Maybe Syntax.BitwiseOr) -> Phantoms.TTerm Syntax.BitwiseOr)
bitwiseOrWithLhs original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.BitwiseOr"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "lhs"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "rhs"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.BitwiseOr"),
          Core.projectionField = (Core.Name "rhs")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

bitwiseOrWithRhs :: (Phantoms.TTerm Syntax.BitwiseOr -> Phantoms.TTerm Syntax.BitwiseXor -> Phantoms.TTerm Syntax.BitwiseOr)
bitwiseOrWithRhs original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.BitwiseOr"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "lhs"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.BitwiseOr"),
          Core.projectionField = (Core.Name "lhs")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "rhs"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

bitwiseXor :: (Phantoms.TTerm (Maybe Syntax.BitwiseXor) -> Phantoms.TTerm Syntax.BitwiseAnd -> Phantoms.TTerm Syntax.BitwiseXor)
bitwiseXor lhs rhs = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.BitwiseXor"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "lhs"),
      Core.fieldTerm = (Phantoms.unTTerm lhs)},
    Core.Field {
      Core.fieldName = (Core.Name "rhs"),
      Core.fieldTerm = (Phantoms.unTTerm rhs)}]})))

bitwiseXorLhs :: (Phantoms.TTerm Syntax.BitwiseXor -> Phantoms.TTerm (Maybe Syntax.BitwiseXor))
bitwiseXorLhs x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.BitwiseXor"),
    Core.projectionField = (Core.Name "lhs")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

bitwiseXorRhs :: (Phantoms.TTerm Syntax.BitwiseXor -> Phantoms.TTerm Syntax.BitwiseAnd)
bitwiseXorRhs x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.BitwiseXor"),
    Core.projectionField = (Core.Name "rhs")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

bitwiseXorWithLhs :: (Phantoms.TTerm Syntax.BitwiseXor -> Phantoms.TTerm (Maybe Syntax.BitwiseXor) -> Phantoms.TTerm Syntax.BitwiseXor)
bitwiseXorWithLhs original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.BitwiseXor"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "lhs"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "rhs"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.BitwiseXor"),
          Core.projectionField = (Core.Name "rhs")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

bitwiseXorWithRhs :: (Phantoms.TTerm Syntax.BitwiseXor -> Phantoms.TTerm Syntax.BitwiseAnd -> Phantoms.TTerm Syntax.BitwiseXor)
bitwiseXorWithRhs original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.BitwiseXor"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "lhs"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.BitwiseXor"),
          Core.projectionField = (Core.Name "lhs")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "rhs"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

bitwiseAnd :: (Phantoms.TTerm (Maybe Syntax.BitwiseAnd) -> Phantoms.TTerm Syntax.ShiftExpression -> Phantoms.TTerm Syntax.BitwiseAnd)
bitwiseAnd lhs rhs = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.BitwiseAnd"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "lhs"),
      Core.fieldTerm = (Phantoms.unTTerm lhs)},
    Core.Field {
      Core.fieldName = (Core.Name "rhs"),
      Core.fieldTerm = (Phantoms.unTTerm rhs)}]})))

bitwiseAndLhs :: (Phantoms.TTerm Syntax.BitwiseAnd -> Phantoms.TTerm (Maybe Syntax.BitwiseAnd))
bitwiseAndLhs x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.BitwiseAnd"),
    Core.projectionField = (Core.Name "lhs")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

bitwiseAndRhs :: (Phantoms.TTerm Syntax.BitwiseAnd -> Phantoms.TTerm Syntax.ShiftExpression)
bitwiseAndRhs x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.BitwiseAnd"),
    Core.projectionField = (Core.Name "rhs")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

bitwiseAndWithLhs :: (Phantoms.TTerm Syntax.BitwiseAnd -> Phantoms.TTerm (Maybe Syntax.BitwiseAnd) -> Phantoms.TTerm Syntax.BitwiseAnd)
bitwiseAndWithLhs original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.BitwiseAnd"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "lhs"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "rhs"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.BitwiseAnd"),
          Core.projectionField = (Core.Name "rhs")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

bitwiseAndWithRhs :: (Phantoms.TTerm Syntax.BitwiseAnd -> Phantoms.TTerm Syntax.ShiftExpression -> Phantoms.TTerm Syntax.BitwiseAnd)
bitwiseAndWithRhs original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.BitwiseAnd"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "lhs"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.BitwiseAnd"),
          Core.projectionField = (Core.Name "lhs")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "rhs"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

shiftExpression :: (Phantoms.TTerm (Maybe Syntax.ShiftLhs) -> Phantoms.TTerm Syntax.Sum -> Phantoms.TTerm Syntax.ShiftExpression)
shiftExpression lhs rhs = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.ShiftExpression"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "lhs"),
      Core.fieldTerm = (Phantoms.unTTerm lhs)},
    Core.Field {
      Core.fieldName = (Core.Name "rhs"),
      Core.fieldTerm = (Phantoms.unTTerm rhs)}]})))

shiftExpressionLhs :: (Phantoms.TTerm Syntax.ShiftExpression -> Phantoms.TTerm (Maybe Syntax.ShiftLhs))
shiftExpressionLhs x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.ShiftExpression"),
    Core.projectionField = (Core.Name "lhs")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

shiftExpressionRhs :: (Phantoms.TTerm Syntax.ShiftExpression -> Phantoms.TTerm Syntax.Sum)
shiftExpressionRhs x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.ShiftExpression"),
    Core.projectionField = (Core.Name "rhs")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

shiftExpressionWithLhs :: (Phantoms.TTerm Syntax.ShiftExpression -> Phantoms.TTerm (Maybe Syntax.ShiftLhs) -> Phantoms.TTerm Syntax.ShiftExpression)
shiftExpressionWithLhs original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.ShiftExpression"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "lhs"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "rhs"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.ShiftExpression"),
          Core.projectionField = (Core.Name "rhs")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

shiftExpressionWithRhs :: (Phantoms.TTerm Syntax.ShiftExpression -> Phantoms.TTerm Syntax.Sum -> Phantoms.TTerm Syntax.ShiftExpression)
shiftExpressionWithRhs original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.ShiftExpression"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "lhs"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.ShiftExpression"),
          Core.projectionField = (Core.Name "lhs")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "rhs"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

shiftLhs :: (Phantoms.TTerm Syntax.ShiftExpression -> Phantoms.TTerm Syntax.ShiftOp -> Phantoms.TTerm Syntax.ShiftLhs)
shiftLhs operand operator = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.ShiftLhs"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "operand"),
      Core.fieldTerm = (Phantoms.unTTerm operand)},
    Core.Field {
      Core.fieldName = (Core.Name "operator"),
      Core.fieldTerm = (Phantoms.unTTerm operator)}]})))

shiftLhsOperand :: (Phantoms.TTerm Syntax.ShiftLhs -> Phantoms.TTerm Syntax.ShiftExpression)
shiftLhsOperand x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.ShiftLhs"),
    Core.projectionField = (Core.Name "operand")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

shiftLhsOperator :: (Phantoms.TTerm Syntax.ShiftLhs -> Phantoms.TTerm Syntax.ShiftOp)
shiftLhsOperator x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.ShiftLhs"),
    Core.projectionField = (Core.Name "operator")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

shiftLhsWithOperand :: (Phantoms.TTerm Syntax.ShiftLhs -> Phantoms.TTerm Syntax.ShiftExpression -> Phantoms.TTerm Syntax.ShiftLhs)
shiftLhsWithOperand original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.ShiftLhs"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "operand"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "operator"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.ShiftLhs"),
          Core.projectionField = (Core.Name "operator")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

shiftLhsWithOperator :: (Phantoms.TTerm Syntax.ShiftLhs -> Phantoms.TTerm Syntax.ShiftOp -> Phantoms.TTerm Syntax.ShiftLhs)
shiftLhsWithOperator original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.ShiftLhs"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "operand"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.ShiftLhs"),
          Core.projectionField = (Core.Name "operand")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "operator"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

shiftOpLeft :: (Phantoms.TTerm Syntax.ShiftOp)
shiftOpLeft = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.ShiftOp"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "left"),
    Core.fieldTerm = Core.TermUnit}})))

shiftOpRight :: (Phantoms.TTerm Syntax.ShiftOp)
shiftOpRight = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.ShiftOp"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "right"),
    Core.fieldTerm = Core.TermUnit}})))

sum :: (Phantoms.TTerm (Maybe Syntax.SumLhs) -> Phantoms.TTerm Syntax.Term -> Phantoms.TTerm Syntax.Sum)
sum lhs rhs = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.Sum"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "lhs"),
      Core.fieldTerm = (Phantoms.unTTerm lhs)},
    Core.Field {
      Core.fieldName = (Core.Name "rhs"),
      Core.fieldTerm = (Phantoms.unTTerm rhs)}]})))

sumLhs :: (Phantoms.TTerm Syntax.Sum -> Phantoms.TTerm (Maybe Syntax.SumLhs))
sumLhs x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.Sum"),
    Core.projectionField = (Core.Name "lhs")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

sumRhs :: (Phantoms.TTerm Syntax.Sum -> Phantoms.TTerm Syntax.Term)
sumRhs x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.Sum"),
    Core.projectionField = (Core.Name "rhs")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

sumWithLhs :: (Phantoms.TTerm Syntax.Sum -> Phantoms.TTerm (Maybe Syntax.SumLhs) -> Phantoms.TTerm Syntax.Sum)
sumWithLhs original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.Sum"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "lhs"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "rhs"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.Sum"),
          Core.projectionField = (Core.Name "rhs")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

sumWithRhs :: (Phantoms.TTerm Syntax.Sum -> Phantoms.TTerm Syntax.Term -> Phantoms.TTerm Syntax.Sum)
sumWithRhs original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.Sum"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "lhs"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.Sum"),
          Core.projectionField = (Core.Name "lhs")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "rhs"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

sumLhs_ :: (Phantoms.TTerm Syntax.Sum -> Phantoms.TTerm Syntax.SumOp -> Phantoms.TTerm Syntax.SumLhs)
sumLhs_ operand operator = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.SumLhs"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "operand"),
      Core.fieldTerm = (Phantoms.unTTerm operand)},
    Core.Field {
      Core.fieldName = (Core.Name "operator"),
      Core.fieldTerm = (Phantoms.unTTerm operator)}]})))

sumLhsOperand :: (Phantoms.TTerm Syntax.SumLhs -> Phantoms.TTerm Syntax.Sum)
sumLhsOperand x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.SumLhs"),
    Core.projectionField = (Core.Name "operand")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

sumLhsOperator :: (Phantoms.TTerm Syntax.SumLhs -> Phantoms.TTerm Syntax.SumOp)
sumLhsOperator x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.SumLhs"),
    Core.projectionField = (Core.Name "operator")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

sumLhsWithOperand :: (Phantoms.TTerm Syntax.SumLhs -> Phantoms.TTerm Syntax.Sum -> Phantoms.TTerm Syntax.SumLhs)
sumLhsWithOperand original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.SumLhs"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "operand"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "operator"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.SumLhs"),
          Core.projectionField = (Core.Name "operator")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

sumLhsWithOperator :: (Phantoms.TTerm Syntax.SumLhs -> Phantoms.TTerm Syntax.SumOp -> Phantoms.TTerm Syntax.SumLhs)
sumLhsWithOperator original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.SumLhs"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "operand"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.SumLhs"),
          Core.projectionField = (Core.Name "operand")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "operator"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

sumOpAdd :: (Phantoms.TTerm Syntax.SumOp)
sumOpAdd = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.SumOp"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "add"),
    Core.fieldTerm = Core.TermUnit}})))

sumOpSub :: (Phantoms.TTerm Syntax.SumOp)
sumOpSub = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.SumOp"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "sub"),
    Core.fieldTerm = Core.TermUnit}})))

term :: (Phantoms.TTerm (Maybe Syntax.TermLhs) -> Phantoms.TTerm Syntax.Factor -> Phantoms.TTerm Syntax.Term)
term lhs rhs = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.Term"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "lhs"),
      Core.fieldTerm = (Phantoms.unTTerm lhs)},
    Core.Field {
      Core.fieldName = (Core.Name "rhs"),
      Core.fieldTerm = (Phantoms.unTTerm rhs)}]})))

termLhs :: (Phantoms.TTerm Syntax.Term -> Phantoms.TTerm (Maybe Syntax.TermLhs))
termLhs x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.Term"),
    Core.projectionField = (Core.Name "lhs")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

termRhs :: (Phantoms.TTerm Syntax.Term -> Phantoms.TTerm Syntax.Factor)
termRhs x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.Term"),
    Core.projectionField = (Core.Name "rhs")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

termWithLhs :: (Phantoms.TTerm Syntax.Term -> Phantoms.TTerm (Maybe Syntax.TermLhs) -> Phantoms.TTerm Syntax.Term)
termWithLhs original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.Term"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "lhs"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "rhs"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.Term"),
          Core.projectionField = (Core.Name "rhs")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

termWithRhs :: (Phantoms.TTerm Syntax.Term -> Phantoms.TTerm Syntax.Factor -> Phantoms.TTerm Syntax.Term)
termWithRhs original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.Term"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "lhs"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.Term"),
          Core.projectionField = (Core.Name "lhs")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "rhs"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

termLhs_ :: (Phantoms.TTerm Syntax.Term -> Phantoms.TTerm Syntax.TermOp -> Phantoms.TTerm Syntax.TermLhs)
termLhs_ operand operator = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.TermLhs"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "operand"),
      Core.fieldTerm = (Phantoms.unTTerm operand)},
    Core.Field {
      Core.fieldName = (Core.Name "operator"),
      Core.fieldTerm = (Phantoms.unTTerm operator)}]})))

termLhsOperand :: (Phantoms.TTerm Syntax.TermLhs -> Phantoms.TTerm Syntax.Term)
termLhsOperand x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.TermLhs"),
    Core.projectionField = (Core.Name "operand")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

termLhsOperator :: (Phantoms.TTerm Syntax.TermLhs -> Phantoms.TTerm Syntax.TermOp)
termLhsOperator x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.TermLhs"),
    Core.projectionField = (Core.Name "operator")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

termLhsWithOperand :: (Phantoms.TTerm Syntax.TermLhs -> Phantoms.TTerm Syntax.Term -> Phantoms.TTerm Syntax.TermLhs)
termLhsWithOperand original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.TermLhs"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "operand"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "operator"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.TermLhs"),
          Core.projectionField = (Core.Name "operator")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

termLhsWithOperator :: (Phantoms.TTerm Syntax.TermLhs -> Phantoms.TTerm Syntax.TermOp -> Phantoms.TTerm Syntax.TermLhs)
termLhsWithOperator original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.TermLhs"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "operand"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.TermLhs"),
          Core.projectionField = (Core.Name "operand")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "operator"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

termOpMul :: (Phantoms.TTerm Syntax.TermOp)
termOpMul = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.TermOp"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "mul"),
    Core.fieldTerm = Core.TermUnit}})))

termOpDiv :: (Phantoms.TTerm Syntax.TermOp)
termOpDiv = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.TermOp"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "div"),
    Core.fieldTerm = Core.TermUnit}})))

termOpFloordiv :: (Phantoms.TTerm Syntax.TermOp)
termOpFloordiv = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.TermOp"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "floordiv"),
    Core.fieldTerm = Core.TermUnit}})))

termOpMod :: (Phantoms.TTerm Syntax.TermOp)
termOpMod = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.TermOp"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "mod"),
    Core.fieldTerm = Core.TermUnit}})))

termOpMatmul :: (Phantoms.TTerm Syntax.TermOp)
termOpMatmul = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.TermOp"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "matmul"),
    Core.fieldTerm = Core.TermUnit}})))

factorPositive :: (Phantoms.TTerm Syntax.Factor -> Phantoms.TTerm Syntax.Factor)
factorPositive x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.Factor"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "positive"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

factorNegative :: (Phantoms.TTerm Syntax.Factor -> Phantoms.TTerm Syntax.Factor)
factorNegative x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.Factor"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "negative"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

factorComplement :: (Phantoms.TTerm Syntax.Factor -> Phantoms.TTerm Syntax.Factor)
factorComplement x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.Factor"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "complement"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

factorSimple :: (Phantoms.TTerm Syntax.Power -> Phantoms.TTerm Syntax.Factor)
factorSimple x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.Factor"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "simple"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

power :: (Phantoms.TTerm Syntax.AwaitPrimary -> Phantoms.TTerm (Maybe Syntax.Factor) -> Phantoms.TTerm Syntax.Power)
power lhs rhs = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.Power"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "lhs"),
      Core.fieldTerm = (Phantoms.unTTerm lhs)},
    Core.Field {
      Core.fieldName = (Core.Name "rhs"),
      Core.fieldTerm = (Phantoms.unTTerm rhs)}]})))

powerLhs :: (Phantoms.TTerm Syntax.Power -> Phantoms.TTerm Syntax.AwaitPrimary)
powerLhs x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.Power"),
    Core.projectionField = (Core.Name "lhs")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

powerRhs :: (Phantoms.TTerm Syntax.Power -> Phantoms.TTerm (Maybe Syntax.Factor))
powerRhs x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.Power"),
    Core.projectionField = (Core.Name "rhs")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

powerWithLhs :: (Phantoms.TTerm Syntax.Power -> Phantoms.TTerm Syntax.AwaitPrimary -> Phantoms.TTerm Syntax.Power)
powerWithLhs original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.Power"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "lhs"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "rhs"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.Power"),
          Core.projectionField = (Core.Name "rhs")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

powerWithRhs :: (Phantoms.TTerm Syntax.Power -> Phantoms.TTerm (Maybe Syntax.Factor) -> Phantoms.TTerm Syntax.Power)
powerWithRhs original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.Power"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "lhs"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.Power"),
          Core.projectionField = (Core.Name "lhs")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "rhs"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

awaitPrimary :: (Phantoms.TTerm Bool -> Phantoms.TTerm Syntax.Primary -> Phantoms.TTerm Syntax.AwaitPrimary)
awaitPrimary await primary = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.AwaitPrimary"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "await"),
      Core.fieldTerm = (Phantoms.unTTerm await)},
    Core.Field {
      Core.fieldName = (Core.Name "primary"),
      Core.fieldTerm = (Phantoms.unTTerm primary)}]})))

awaitPrimaryAwait :: (Phantoms.TTerm Syntax.AwaitPrimary -> Phantoms.TTerm Bool)
awaitPrimaryAwait x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.AwaitPrimary"),
    Core.projectionField = (Core.Name "await")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

awaitPrimaryPrimary :: (Phantoms.TTerm Syntax.AwaitPrimary -> Phantoms.TTerm Syntax.Primary)
awaitPrimaryPrimary x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.AwaitPrimary"),
    Core.projectionField = (Core.Name "primary")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

awaitPrimaryWithAwait :: (Phantoms.TTerm Syntax.AwaitPrimary -> Phantoms.TTerm Bool -> Phantoms.TTerm Syntax.AwaitPrimary)
awaitPrimaryWithAwait original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.AwaitPrimary"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "await"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "primary"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.AwaitPrimary"),
          Core.projectionField = (Core.Name "primary")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

awaitPrimaryWithPrimary :: (Phantoms.TTerm Syntax.AwaitPrimary -> Phantoms.TTerm Syntax.Primary -> Phantoms.TTerm Syntax.AwaitPrimary)
awaitPrimaryWithPrimary original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.AwaitPrimary"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "await"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.AwaitPrimary"),
          Core.projectionField = (Core.Name "await")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "primary"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

primarySimple :: (Phantoms.TTerm Syntax.Atom -> Phantoms.TTerm Syntax.Primary)
primarySimple x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.Primary"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "simple"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

primaryCompound :: (Phantoms.TTerm Syntax.PrimaryWithRhs -> Phantoms.TTerm Syntax.Primary)
primaryCompound x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.Primary"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "compound"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

primaryWithRhs :: (Phantoms.TTerm Syntax.Primary -> Phantoms.TTerm Syntax.PrimaryRhs -> Phantoms.TTerm Syntax.PrimaryWithRhs)
primaryWithRhs primary rhs = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.PrimaryWithRhs"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "primary"),
      Core.fieldTerm = (Phantoms.unTTerm primary)},
    Core.Field {
      Core.fieldName = (Core.Name "rhs"),
      Core.fieldTerm = (Phantoms.unTTerm rhs)}]})))

primaryWithRhsPrimary :: (Phantoms.TTerm Syntax.PrimaryWithRhs -> Phantoms.TTerm Syntax.Primary)
primaryWithRhsPrimary x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.PrimaryWithRhs"),
    Core.projectionField = (Core.Name "primary")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

primaryWithRhsRhs :: (Phantoms.TTerm Syntax.PrimaryWithRhs -> Phantoms.TTerm Syntax.PrimaryRhs)
primaryWithRhsRhs x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.PrimaryWithRhs"),
    Core.projectionField = (Core.Name "rhs")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

primaryWithRhsWithPrimary :: (Phantoms.TTerm Syntax.PrimaryWithRhs -> Phantoms.TTerm Syntax.Primary -> Phantoms.TTerm Syntax.PrimaryWithRhs)
primaryWithRhsWithPrimary original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.PrimaryWithRhs"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "primary"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "rhs"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.PrimaryWithRhs"),
          Core.projectionField = (Core.Name "rhs")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

primaryWithRhsWithRhs :: (Phantoms.TTerm Syntax.PrimaryWithRhs -> Phantoms.TTerm Syntax.PrimaryRhs -> Phantoms.TTerm Syntax.PrimaryWithRhs)
primaryWithRhsWithRhs original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.PrimaryWithRhs"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "primary"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.PrimaryWithRhs"),
          Core.projectionField = (Core.Name "primary")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "rhs"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

primaryRhsProject :: (Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.PrimaryRhs)
primaryRhsProject x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.PrimaryRhs"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "project"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

primaryRhsGenexp :: (Phantoms.TTerm Syntax.Genexp -> Phantoms.TTerm Syntax.PrimaryRhs)
primaryRhsGenexp x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.PrimaryRhs"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "genexp"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

primaryRhsCall :: (Phantoms.TTerm Syntax.Args -> Phantoms.TTerm Syntax.PrimaryRhs)
primaryRhsCall x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.PrimaryRhs"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "call"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

primaryRhsSlices :: (Phantoms.TTerm Syntax.Slices -> Phantoms.TTerm Syntax.PrimaryRhs)
primaryRhsSlices x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.PrimaryRhs"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "slices"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

slices :: (Phantoms.TTerm Syntax.Slice -> Phantoms.TTerm [Syntax.SliceOrStarredExpression] -> Phantoms.TTerm Syntax.Slices)
slices head tail = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.Slices"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "head"),
      Core.fieldTerm = (Phantoms.unTTerm head)},
    Core.Field {
      Core.fieldName = (Core.Name "tail"),
      Core.fieldTerm = (Phantoms.unTTerm tail)}]})))

slicesHead :: (Phantoms.TTerm Syntax.Slices -> Phantoms.TTerm Syntax.Slice)
slicesHead x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.Slices"),
    Core.projectionField = (Core.Name "head")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

slicesTail :: (Phantoms.TTerm Syntax.Slices -> Phantoms.TTerm [Syntax.SliceOrStarredExpression])
slicesTail x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.Slices"),
    Core.projectionField = (Core.Name "tail")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

slicesWithHead :: (Phantoms.TTerm Syntax.Slices -> Phantoms.TTerm Syntax.Slice -> Phantoms.TTerm Syntax.Slices)
slicesWithHead original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.Slices"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "head"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "tail"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.Slices"),
          Core.projectionField = (Core.Name "tail")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

slicesWithTail :: (Phantoms.TTerm Syntax.Slices -> Phantoms.TTerm [Syntax.SliceOrStarredExpression] -> Phantoms.TTerm Syntax.Slices)
slicesWithTail original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.Slices"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "head"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.Slices"),
          Core.projectionField = (Core.Name "head")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "tail"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

sliceOrStarredExpressionSlice :: (Phantoms.TTerm Syntax.Slice -> Phantoms.TTerm Syntax.SliceOrStarredExpression)
sliceOrStarredExpressionSlice x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.SliceOrStarredExpression"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "slice"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

sliceOrStarredExpressionStarred :: (Phantoms.TTerm Syntax.StarredExpression -> Phantoms.TTerm Syntax.SliceOrStarredExpression)
sliceOrStarredExpressionStarred x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.SliceOrStarredExpression"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "starred"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

sliceNamed :: (Phantoms.TTerm Syntax.NamedExpression -> Phantoms.TTerm Syntax.Slice)
sliceNamed x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.Slice"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "named"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

sliceSlice_ :: (Phantoms.TTerm Syntax.SliceExpression -> Phantoms.TTerm Syntax.Slice)
sliceSlice_ x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.Slice"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "slice_"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

sliceExpression :: (Phantoms.TTerm (Maybe Syntax.Expression) -> Phantoms.TTerm (Maybe Syntax.Expression) -> Phantoms.TTerm (Maybe Syntax.Expression) -> Phantoms.TTerm Syntax.SliceExpression)
sliceExpression start stop step = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.SliceExpression"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "start"),
      Core.fieldTerm = (Phantoms.unTTerm start)},
    Core.Field {
      Core.fieldName = (Core.Name "stop"),
      Core.fieldTerm = (Phantoms.unTTerm stop)},
    Core.Field {
      Core.fieldName = (Core.Name "step"),
      Core.fieldTerm = (Phantoms.unTTerm step)}]})))

sliceExpressionStart :: (Phantoms.TTerm Syntax.SliceExpression -> Phantoms.TTerm (Maybe Syntax.Expression))
sliceExpressionStart x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.SliceExpression"),
    Core.projectionField = (Core.Name "start")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

sliceExpressionStop :: (Phantoms.TTerm Syntax.SliceExpression -> Phantoms.TTerm (Maybe Syntax.Expression))
sliceExpressionStop x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.SliceExpression"),
    Core.projectionField = (Core.Name "stop")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

sliceExpressionStep :: (Phantoms.TTerm Syntax.SliceExpression -> Phantoms.TTerm (Maybe Syntax.Expression))
sliceExpressionStep x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.SliceExpression"),
    Core.projectionField = (Core.Name "step")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

sliceExpressionWithStart :: (Phantoms.TTerm Syntax.SliceExpression -> Phantoms.TTerm (Maybe Syntax.Expression) -> Phantoms.TTerm Syntax.SliceExpression)
sliceExpressionWithStart original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.SliceExpression"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "start"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "stop"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.SliceExpression"),
          Core.projectionField = (Core.Name "stop")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "step"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.SliceExpression"),
          Core.projectionField = (Core.Name "step")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

sliceExpressionWithStop :: (Phantoms.TTerm Syntax.SliceExpression -> Phantoms.TTerm (Maybe Syntax.Expression) -> Phantoms.TTerm Syntax.SliceExpression)
sliceExpressionWithStop original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.SliceExpression"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "start"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.SliceExpression"),
          Core.projectionField = (Core.Name "start")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "stop"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "step"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.SliceExpression"),
          Core.projectionField = (Core.Name "step")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

sliceExpressionWithStep :: (Phantoms.TTerm Syntax.SliceExpression -> Phantoms.TTerm (Maybe Syntax.Expression) -> Phantoms.TTerm Syntax.SliceExpression)
sliceExpressionWithStep original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.SliceExpression"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "start"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.SliceExpression"),
          Core.projectionField = (Core.Name "start")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "stop"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.SliceExpression"),
          Core.projectionField = (Core.Name "stop")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "step"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

atomName :: (Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.Atom)
atomName x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.Atom"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "name"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

atomTrue :: (Phantoms.TTerm Syntax.Atom)
atomTrue = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.Atom"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "true"),
    Core.fieldTerm = Core.TermUnit}})))

atomFalse :: (Phantoms.TTerm Syntax.Atom)
atomFalse = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.Atom"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "false"),
    Core.fieldTerm = Core.TermUnit}})))

atomNone :: (Phantoms.TTerm Syntax.Atom)
atomNone = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.Atom"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "none"),
    Core.fieldTerm = Core.TermUnit}})))

atomString :: (Phantoms.TTerm Syntax.String_ -> Phantoms.TTerm Syntax.Atom)
atomString x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.Atom"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "string"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

atomNumber :: (Phantoms.TTerm Syntax.Number -> Phantoms.TTerm Syntax.Atom)
atomNumber x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.Atom"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "number"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

atomTuple :: (Phantoms.TTerm Syntax.Tuple -> Phantoms.TTerm Syntax.Atom)
atomTuple x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.Atom"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "tuple"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

atomGroup :: (Phantoms.TTerm Syntax.Group -> Phantoms.TTerm Syntax.Atom)
atomGroup x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.Atom"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "group"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

atomGenexp :: (Phantoms.TTerm Syntax.Genexp -> Phantoms.TTerm Syntax.Atom)
atomGenexp x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.Atom"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "genexp"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

atomList :: (Phantoms.TTerm Syntax.List -> Phantoms.TTerm Syntax.Atom)
atomList x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.Atom"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "list"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

atomListcomp :: (Phantoms.TTerm Syntax.Listcomp -> Phantoms.TTerm Syntax.Atom)
atomListcomp x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.Atom"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "listcomp"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

atomDict :: (Phantoms.TTerm Syntax.Dict -> Phantoms.TTerm Syntax.Atom)
atomDict x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.Atom"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "dict"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

atomSet :: (Phantoms.TTerm Syntax.Set -> Phantoms.TTerm Syntax.Atom)
atomSet x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.Atom"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "set"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

atomDictcomp :: (Phantoms.TTerm Syntax.Dictcomp -> Phantoms.TTerm Syntax.Atom)
atomDictcomp x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.Atom"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "dictcomp"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

atomSetcomp :: (Phantoms.TTerm Syntax.Setcomp -> Phantoms.TTerm Syntax.Atom)
atomSetcomp x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.Atom"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "setcomp"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

atomEllipsis :: (Phantoms.TTerm Syntax.Atom)
atomEllipsis = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.Atom"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "ellipsis"),
    Core.fieldTerm = Core.TermUnit}})))

groupYield :: (Phantoms.TTerm Syntax.YieldExpression -> Phantoms.TTerm Syntax.Group)
groupYield x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.Group"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "yield"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

groupExpression :: (Phantoms.TTerm Syntax.NamedExpression -> Phantoms.TTerm Syntax.Group)
groupExpression x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.Group"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "expression"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

lambda :: (Phantoms.TTerm Syntax.LambdaParameters -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.Lambda)
lambda params body = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.Lambda"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "params"),
      Core.fieldTerm = (Phantoms.unTTerm params)},
    Core.Field {
      Core.fieldName = (Core.Name "body"),
      Core.fieldTerm = (Phantoms.unTTerm body)}]})))

lambdaParams :: (Phantoms.TTerm Syntax.Lambda -> Phantoms.TTerm Syntax.LambdaParameters)
lambdaParams x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.Lambda"),
    Core.projectionField = (Core.Name "params")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

lambdaBody :: (Phantoms.TTerm Syntax.Lambda -> Phantoms.TTerm Syntax.Expression)
lambdaBody x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.Lambda"),
    Core.projectionField = (Core.Name "body")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

lambdaWithParams :: (Phantoms.TTerm Syntax.Lambda -> Phantoms.TTerm Syntax.LambdaParameters -> Phantoms.TTerm Syntax.Lambda)
lambdaWithParams original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.Lambda"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "params"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "body"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.Lambda"),
          Core.projectionField = (Core.Name "body")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

lambdaWithBody :: (Phantoms.TTerm Syntax.Lambda -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.Lambda)
lambdaWithBody original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.Lambda"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "params"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.Lambda"),
          Core.projectionField = (Core.Name "params")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "body"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

lambdaParameters :: (Phantoms.TTerm (Maybe Syntax.LambdaSlashNoDefault) -> Phantoms.TTerm [Syntax.LambdaParamNoDefault] -> Phantoms.TTerm [Syntax.LambdaParamWithDefault] -> Phantoms.TTerm (Maybe Syntax.LambdaStarEtc) -> Phantoms.TTerm Syntax.LambdaParameters)
lambdaParameters slashNoDefault paramNoDefault paramWithDefault starEtc = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.LambdaParameters"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "slashNoDefault"),
      Core.fieldTerm = (Phantoms.unTTerm slashNoDefault)},
    Core.Field {
      Core.fieldName = (Core.Name "paramNoDefault"),
      Core.fieldTerm = (Phantoms.unTTerm paramNoDefault)},
    Core.Field {
      Core.fieldName = (Core.Name "paramWithDefault"),
      Core.fieldTerm = (Phantoms.unTTerm paramWithDefault)},
    Core.Field {
      Core.fieldName = (Core.Name "starEtc"),
      Core.fieldTerm = (Phantoms.unTTerm starEtc)}]})))

lambdaParametersSlashNoDefault :: (Phantoms.TTerm Syntax.LambdaParameters -> Phantoms.TTerm (Maybe Syntax.LambdaSlashNoDefault))
lambdaParametersSlashNoDefault x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.LambdaParameters"),
    Core.projectionField = (Core.Name "slashNoDefault")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

lambdaParametersParamNoDefault :: (Phantoms.TTerm Syntax.LambdaParameters -> Phantoms.TTerm [Syntax.LambdaParamNoDefault])
lambdaParametersParamNoDefault x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.LambdaParameters"),
    Core.projectionField = (Core.Name "paramNoDefault")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

lambdaParametersParamWithDefault :: (Phantoms.TTerm Syntax.LambdaParameters -> Phantoms.TTerm [Syntax.LambdaParamWithDefault])
lambdaParametersParamWithDefault x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.LambdaParameters"),
    Core.projectionField = (Core.Name "paramWithDefault")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

lambdaParametersStarEtc :: (Phantoms.TTerm Syntax.LambdaParameters -> Phantoms.TTerm (Maybe Syntax.LambdaStarEtc))
lambdaParametersStarEtc x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.LambdaParameters"),
    Core.projectionField = (Core.Name "starEtc")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

lambdaParametersWithSlashNoDefault :: (Phantoms.TTerm Syntax.LambdaParameters -> Phantoms.TTerm (Maybe Syntax.LambdaSlashNoDefault) -> Phantoms.TTerm Syntax.LambdaParameters)
lambdaParametersWithSlashNoDefault original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.LambdaParameters"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "slashNoDefault"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "paramNoDefault"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.LambdaParameters"),
          Core.projectionField = (Core.Name "paramNoDefault")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "paramWithDefault"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.LambdaParameters"),
          Core.projectionField = (Core.Name "paramWithDefault")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "starEtc"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.LambdaParameters"),
          Core.projectionField = (Core.Name "starEtc")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

lambdaParametersWithParamNoDefault :: (Phantoms.TTerm Syntax.LambdaParameters -> Phantoms.TTerm [Syntax.LambdaParamNoDefault] -> Phantoms.TTerm Syntax.LambdaParameters)
lambdaParametersWithParamNoDefault original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.LambdaParameters"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "slashNoDefault"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.LambdaParameters"),
          Core.projectionField = (Core.Name "slashNoDefault")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "paramNoDefault"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "paramWithDefault"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.LambdaParameters"),
          Core.projectionField = (Core.Name "paramWithDefault")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "starEtc"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.LambdaParameters"),
          Core.projectionField = (Core.Name "starEtc")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

lambdaParametersWithParamWithDefault :: (Phantoms.TTerm Syntax.LambdaParameters -> Phantoms.TTerm [Syntax.LambdaParamWithDefault] -> Phantoms.TTerm Syntax.LambdaParameters)
lambdaParametersWithParamWithDefault original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.LambdaParameters"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "slashNoDefault"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.LambdaParameters"),
          Core.projectionField = (Core.Name "slashNoDefault")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "paramNoDefault"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.LambdaParameters"),
          Core.projectionField = (Core.Name "paramNoDefault")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "paramWithDefault"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "starEtc"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.LambdaParameters"),
          Core.projectionField = (Core.Name "starEtc")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

lambdaParametersWithStarEtc :: (Phantoms.TTerm Syntax.LambdaParameters -> Phantoms.TTerm (Maybe Syntax.LambdaStarEtc) -> Phantoms.TTerm Syntax.LambdaParameters)
lambdaParametersWithStarEtc original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.LambdaParameters"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "slashNoDefault"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.LambdaParameters"),
          Core.projectionField = (Core.Name "slashNoDefault")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "paramNoDefault"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.LambdaParameters"),
          Core.projectionField = (Core.Name "paramNoDefault")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "paramWithDefault"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.LambdaParameters"),
          Core.projectionField = (Core.Name "paramWithDefault")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "starEtc"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

lambdaSlashNoDefault :: (Phantoms.TTerm [Syntax.LambdaParamNoDefault] -> Phantoms.TTerm Syntax.LambdaSlashNoDefault)
lambdaSlashNoDefault parameters = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.LambdaSlashNoDefault"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "parameters"),
      Core.fieldTerm = (Phantoms.unTTerm parameters)}]})))

lambdaSlashNoDefaultParameters :: (Phantoms.TTerm Syntax.LambdaSlashNoDefault -> Phantoms.TTerm [Syntax.LambdaParamNoDefault])
lambdaSlashNoDefaultParameters x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.LambdaSlashNoDefault"),
    Core.projectionField = (Core.Name "parameters")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

lambdaSlashNoDefaultWithParameters :: (Phantoms.TTerm Syntax.LambdaSlashNoDefault -> Phantoms.TTerm [Syntax.LambdaParamNoDefault] -> Phantoms.TTerm Syntax.LambdaSlashNoDefault)
lambdaSlashNoDefaultWithParameters original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.LambdaSlashNoDefault"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "parameters"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

lambdaSlashWithDefault :: (Phantoms.TTerm [Syntax.LambdaParamNoDefault] -> Phantoms.TTerm [Syntax.LambdaParamWithDefault] -> Phantoms.TTerm Syntax.LambdaSlashWithDefault)
lambdaSlashWithDefault paramNoDefault paramWithDefault = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.LambdaSlashWithDefault"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "paramNoDefault"),
      Core.fieldTerm = (Phantoms.unTTerm paramNoDefault)},
    Core.Field {
      Core.fieldName = (Core.Name "paramWithDefault"),
      Core.fieldTerm = (Phantoms.unTTerm paramWithDefault)}]})))

lambdaSlashWithDefaultParamNoDefault :: (Phantoms.TTerm Syntax.LambdaSlashWithDefault -> Phantoms.TTerm [Syntax.LambdaParamNoDefault])
lambdaSlashWithDefaultParamNoDefault x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.LambdaSlashWithDefault"),
    Core.projectionField = (Core.Name "paramNoDefault")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

lambdaSlashWithDefaultParamWithDefault :: (Phantoms.TTerm Syntax.LambdaSlashWithDefault -> Phantoms.TTerm [Syntax.LambdaParamWithDefault])
lambdaSlashWithDefaultParamWithDefault x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.LambdaSlashWithDefault"),
    Core.projectionField = (Core.Name "paramWithDefault")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

lambdaSlashWithDefaultWithParamNoDefault :: (Phantoms.TTerm Syntax.LambdaSlashWithDefault -> Phantoms.TTerm [Syntax.LambdaParamNoDefault] -> Phantoms.TTerm Syntax.LambdaSlashWithDefault)
lambdaSlashWithDefaultWithParamNoDefault original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.LambdaSlashWithDefault"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "paramNoDefault"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "paramWithDefault"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.LambdaSlashWithDefault"),
          Core.projectionField = (Core.Name "paramWithDefault")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

lambdaSlashWithDefaultWithParamWithDefault :: (Phantoms.TTerm Syntax.LambdaSlashWithDefault -> Phantoms.TTerm [Syntax.LambdaParamWithDefault] -> Phantoms.TTerm Syntax.LambdaSlashWithDefault)
lambdaSlashWithDefaultWithParamWithDefault original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.LambdaSlashWithDefault"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "paramNoDefault"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.LambdaSlashWithDefault"),
          Core.projectionField = (Core.Name "paramNoDefault")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "paramWithDefault"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

lambdaStarEtcStar :: (Phantoms.TTerm Bool -> Phantoms.TTerm Syntax.LambdaStarEtc)
lambdaStarEtcStar x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.LambdaStarEtc"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "star"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

lambdaStarEtcParamNoDefault :: (Phantoms.TTerm Syntax.LambdaParamNoDefault -> Phantoms.TTerm Syntax.LambdaStarEtc)
lambdaStarEtcParamNoDefault x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.LambdaStarEtc"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "paramNoDefault"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

lambdaStarEtcParamMaybeDefault :: (Phantoms.TTerm [Syntax.LambdaParamMaybeDefault] -> Phantoms.TTerm Syntax.LambdaStarEtc)
lambdaStarEtcParamMaybeDefault x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.LambdaStarEtc"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "paramMaybeDefault"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

lambdaStarEtcKwds :: (Phantoms.TTerm Syntax.LambdaKwds -> Phantoms.TTerm Syntax.LambdaStarEtc)
lambdaStarEtcKwds x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.LambdaStarEtc"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "kwds"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

lambdaKwds :: (Phantoms.TTerm Syntax.LambdaParamNoDefault -> Phantoms.TTerm Syntax.LambdaKwds)
lambdaKwds x = (Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
  Core.wrappedTermTypeName = (Core.Name "hydra.ext.python.syntax.LambdaKwds"),
  Core.wrappedTermBody = (Phantoms.unTTerm x)})))

unLambdaKwds :: (Phantoms.TTerm Syntax.LambdaKwds -> Phantoms.TTerm Syntax.LambdaParamNoDefault)
unLambdaKwds x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.python.syntax.LambdaKwds")))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

lambdaParamNoDefault :: (Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.LambdaParamNoDefault)
lambdaParamNoDefault x = (Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
  Core.wrappedTermTypeName = (Core.Name "hydra.ext.python.syntax.LambdaParamNoDefault"),
  Core.wrappedTermBody = (Phantoms.unTTerm x)})))

unLambdaParamNoDefault :: (Phantoms.TTerm Syntax.LambdaParamNoDefault -> Phantoms.TTerm Syntax.Name)
unLambdaParamNoDefault x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.python.syntax.LambdaParamNoDefault")))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

lambdaParamWithDefault :: (Phantoms.TTerm Syntax.Name -> Phantoms.TTerm (Maybe Syntax.Default) -> Phantoms.TTerm Syntax.LambdaParamWithDefault)
lambdaParamWithDefault param default_ = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.LambdaParamWithDefault"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "param"),
      Core.fieldTerm = (Phantoms.unTTerm param)},
    Core.Field {
      Core.fieldName = (Core.Name "default"),
      Core.fieldTerm = (Phantoms.unTTerm default_)}]})))

lambdaParamWithDefaultParam :: (Phantoms.TTerm Syntax.LambdaParamWithDefault -> Phantoms.TTerm Syntax.Name)
lambdaParamWithDefaultParam x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.LambdaParamWithDefault"),
    Core.projectionField = (Core.Name "param")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

lambdaParamWithDefaultDefault :: (Phantoms.TTerm Syntax.LambdaParamWithDefault -> Phantoms.TTerm (Maybe Syntax.Default))
lambdaParamWithDefaultDefault x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.LambdaParamWithDefault"),
    Core.projectionField = (Core.Name "default")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

lambdaParamWithDefaultWithParam :: (Phantoms.TTerm Syntax.LambdaParamWithDefault -> Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.LambdaParamWithDefault)
lambdaParamWithDefaultWithParam original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.LambdaParamWithDefault"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "param"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "default"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.LambdaParamWithDefault"),
          Core.projectionField = (Core.Name "default")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

lambdaParamWithDefaultWithDefault :: (Phantoms.TTerm Syntax.LambdaParamWithDefault -> Phantoms.TTerm (Maybe Syntax.Default) -> Phantoms.TTerm Syntax.LambdaParamWithDefault)
lambdaParamWithDefaultWithDefault original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.LambdaParamWithDefault"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "param"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.LambdaParamWithDefault"),
          Core.projectionField = (Core.Name "param")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "default"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

lambdaParamMaybeDefault :: (Phantoms.TTerm Syntax.Name -> Phantoms.TTerm (Maybe Syntax.Default) -> Phantoms.TTerm Syntax.LambdaParamMaybeDefault)
lambdaParamMaybeDefault param default_ = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.LambdaParamMaybeDefault"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "param"),
      Core.fieldTerm = (Phantoms.unTTerm param)},
    Core.Field {
      Core.fieldName = (Core.Name "default"),
      Core.fieldTerm = (Phantoms.unTTerm default_)}]})))

lambdaParamMaybeDefaultParam :: (Phantoms.TTerm Syntax.LambdaParamMaybeDefault -> Phantoms.TTerm Syntax.Name)
lambdaParamMaybeDefaultParam x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.LambdaParamMaybeDefault"),
    Core.projectionField = (Core.Name "param")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

lambdaParamMaybeDefaultDefault :: (Phantoms.TTerm Syntax.LambdaParamMaybeDefault -> Phantoms.TTerm (Maybe Syntax.Default))
lambdaParamMaybeDefaultDefault x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.LambdaParamMaybeDefault"),
    Core.projectionField = (Core.Name "default")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

lambdaParamMaybeDefaultWithParam :: (Phantoms.TTerm Syntax.LambdaParamMaybeDefault -> Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.LambdaParamMaybeDefault)
lambdaParamMaybeDefaultWithParam original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.LambdaParamMaybeDefault"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "param"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "default"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.LambdaParamMaybeDefault"),
          Core.projectionField = (Core.Name "default")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

lambdaParamMaybeDefaultWithDefault :: (Phantoms.TTerm Syntax.LambdaParamMaybeDefault -> Phantoms.TTerm (Maybe Syntax.Default) -> Phantoms.TTerm Syntax.LambdaParamMaybeDefault)
lambdaParamMaybeDefaultWithDefault original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.LambdaParamMaybeDefault"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "param"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.LambdaParamMaybeDefault"),
          Core.projectionField = (Core.Name "param")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "default"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

list :: (Phantoms.TTerm [Syntax.StarNamedExpression] -> Phantoms.TTerm Syntax.List)
list x = (Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
  Core.wrappedTermTypeName = (Core.Name "hydra.ext.python.syntax.List"),
  Core.wrappedTermBody = (Phantoms.unTTerm x)})))

unList :: (Phantoms.TTerm Syntax.List -> Phantoms.TTerm [Syntax.StarNamedExpression])
unList x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.python.syntax.List")))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

tuple :: (Phantoms.TTerm [Syntax.StarNamedExpression] -> Phantoms.TTerm Syntax.Tuple)
tuple x = (Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
  Core.wrappedTermTypeName = (Core.Name "hydra.ext.python.syntax.Tuple"),
  Core.wrappedTermBody = (Phantoms.unTTerm x)})))

unTuple :: (Phantoms.TTerm Syntax.Tuple -> Phantoms.TTerm [Syntax.StarNamedExpression])
unTuple x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.python.syntax.Tuple")))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

set :: (Phantoms.TTerm [Syntax.StarNamedExpression] -> Phantoms.TTerm Syntax.Set)
set x = (Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
  Core.wrappedTermTypeName = (Core.Name "hydra.ext.python.syntax.Set"),
  Core.wrappedTermBody = (Phantoms.unTTerm x)})))

unSet :: (Phantoms.TTerm Syntax.Set -> Phantoms.TTerm [Syntax.StarNamedExpression])
unSet x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.python.syntax.Set")))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

dict :: (Phantoms.TTerm [Syntax.DoubleStarredKvpair] -> Phantoms.TTerm Syntax.Dict)
dict x = (Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
  Core.wrappedTermTypeName = (Core.Name "hydra.ext.python.syntax.Dict"),
  Core.wrappedTermBody = (Phantoms.unTTerm x)})))

unDict :: (Phantoms.TTerm Syntax.Dict -> Phantoms.TTerm [Syntax.DoubleStarredKvpair])
unDict x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.python.syntax.Dict")))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

doubleStarredKvpairStarred :: (Phantoms.TTerm Syntax.BitwiseOr -> Phantoms.TTerm Syntax.DoubleStarredKvpair)
doubleStarredKvpairStarred x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.DoubleStarredKvpair"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "starred"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

doubleStarredKvpairPair :: (Phantoms.TTerm Syntax.Kvpair -> Phantoms.TTerm Syntax.DoubleStarredKvpair)
doubleStarredKvpairPair x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.DoubleStarredKvpair"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "pair"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

kvpair :: (Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.Kvpair)
kvpair key value = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.Kvpair"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "key"),
      Core.fieldTerm = (Phantoms.unTTerm key)},
    Core.Field {
      Core.fieldName = (Core.Name "value"),
      Core.fieldTerm = (Phantoms.unTTerm value)}]})))

kvpairKey :: (Phantoms.TTerm Syntax.Kvpair -> Phantoms.TTerm Syntax.Expression)
kvpairKey x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.Kvpair"),
    Core.projectionField = (Core.Name "key")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

kvpairValue :: (Phantoms.TTerm Syntax.Kvpair -> Phantoms.TTerm Syntax.Expression)
kvpairValue x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.Kvpair"),
    Core.projectionField = (Core.Name "value")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

kvpairWithKey :: (Phantoms.TTerm Syntax.Kvpair -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.Kvpair)
kvpairWithKey original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.Kvpair"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "key"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "value"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.Kvpair"),
          Core.projectionField = (Core.Name "value")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

kvpairWithValue :: (Phantoms.TTerm Syntax.Kvpair -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.Kvpair)
kvpairWithValue original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.Kvpair"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "key"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.Kvpair"),
          Core.projectionField = (Core.Name "key")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "value"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

forIfClauses :: (Phantoms.TTerm [Syntax.ForIfClause] -> Phantoms.TTerm Syntax.ForIfClauses)
forIfClauses x = (Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
  Core.wrappedTermTypeName = (Core.Name "hydra.ext.python.syntax.ForIfClauses"),
  Core.wrappedTermBody = (Phantoms.unTTerm x)})))

unForIfClauses :: (Phantoms.TTerm Syntax.ForIfClauses -> Phantoms.TTerm [Syntax.ForIfClause])
unForIfClauses x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.python.syntax.ForIfClauses")))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

forIfClause :: (Phantoms.TTerm Bool -> Phantoms.TTerm [Syntax.StarTarget] -> Phantoms.TTerm Syntax.Disjunction -> Phantoms.TTerm [Syntax.Disjunction] -> Phantoms.TTerm Syntax.ForIfClause)
forIfClause async targets in_ ifs = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.ForIfClause"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "async"),
      Core.fieldTerm = (Phantoms.unTTerm async)},
    Core.Field {
      Core.fieldName = (Core.Name "targets"),
      Core.fieldTerm = (Phantoms.unTTerm targets)},
    Core.Field {
      Core.fieldName = (Core.Name "in"),
      Core.fieldTerm = (Phantoms.unTTerm in_)},
    Core.Field {
      Core.fieldName = (Core.Name "ifs"),
      Core.fieldTerm = (Phantoms.unTTerm ifs)}]})))

forIfClauseAsync :: (Phantoms.TTerm Syntax.ForIfClause -> Phantoms.TTerm Bool)
forIfClauseAsync x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.ForIfClause"),
    Core.projectionField = (Core.Name "async")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

forIfClauseTargets :: (Phantoms.TTerm Syntax.ForIfClause -> Phantoms.TTerm [Syntax.StarTarget])
forIfClauseTargets x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.ForIfClause"),
    Core.projectionField = (Core.Name "targets")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

forIfClauseIn :: (Phantoms.TTerm Syntax.ForIfClause -> Phantoms.TTerm Syntax.Disjunction)
forIfClauseIn x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.ForIfClause"),
    Core.projectionField = (Core.Name "in")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

forIfClauseIfs :: (Phantoms.TTerm Syntax.ForIfClause -> Phantoms.TTerm [Syntax.Disjunction])
forIfClauseIfs x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.ForIfClause"),
    Core.projectionField = (Core.Name "ifs")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

forIfClauseWithAsync :: (Phantoms.TTerm Syntax.ForIfClause -> Phantoms.TTerm Bool -> Phantoms.TTerm Syntax.ForIfClause)
forIfClauseWithAsync original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.ForIfClause"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "async"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "targets"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.ForIfClause"),
          Core.projectionField = (Core.Name "targets")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "in"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.ForIfClause"),
          Core.projectionField = (Core.Name "in")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "ifs"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.ForIfClause"),
          Core.projectionField = (Core.Name "ifs")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

forIfClauseWithTargets :: (Phantoms.TTerm Syntax.ForIfClause -> Phantoms.TTerm [Syntax.StarTarget] -> Phantoms.TTerm Syntax.ForIfClause)
forIfClauseWithTargets original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.ForIfClause"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "async"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.ForIfClause"),
          Core.projectionField = (Core.Name "async")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "targets"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "in"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.ForIfClause"),
          Core.projectionField = (Core.Name "in")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "ifs"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.ForIfClause"),
          Core.projectionField = (Core.Name "ifs")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

forIfClauseWithIn :: (Phantoms.TTerm Syntax.ForIfClause -> Phantoms.TTerm Syntax.Disjunction -> Phantoms.TTerm Syntax.ForIfClause)
forIfClauseWithIn original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.ForIfClause"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "async"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.ForIfClause"),
          Core.projectionField = (Core.Name "async")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "targets"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.ForIfClause"),
          Core.projectionField = (Core.Name "targets")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "in"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "ifs"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.ForIfClause"),
          Core.projectionField = (Core.Name "ifs")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

forIfClauseWithIfs :: (Phantoms.TTerm Syntax.ForIfClause -> Phantoms.TTerm [Syntax.Disjunction] -> Phantoms.TTerm Syntax.ForIfClause)
forIfClauseWithIfs original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.ForIfClause"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "async"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.ForIfClause"),
          Core.projectionField = (Core.Name "async")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "targets"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.ForIfClause"),
          Core.projectionField = (Core.Name "targets")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "in"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.ForIfClause"),
          Core.projectionField = (Core.Name "in")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "ifs"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

listcomp :: (Phantoms.TTerm Syntax.NamedExpression -> Phantoms.TTerm Syntax.ForIfClauses -> Phantoms.TTerm Syntax.Listcomp)
listcomp expression forIfClauses = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.Listcomp"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "expression"),
      Core.fieldTerm = (Phantoms.unTTerm expression)},
    Core.Field {
      Core.fieldName = (Core.Name "forIfClauses"),
      Core.fieldTerm = (Phantoms.unTTerm forIfClauses)}]})))

listcompExpression :: (Phantoms.TTerm Syntax.Listcomp -> Phantoms.TTerm Syntax.NamedExpression)
listcompExpression x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.Listcomp"),
    Core.projectionField = (Core.Name "expression")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

listcompForIfClauses :: (Phantoms.TTerm Syntax.Listcomp -> Phantoms.TTerm Syntax.ForIfClauses)
listcompForIfClauses x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.Listcomp"),
    Core.projectionField = (Core.Name "forIfClauses")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

listcompWithExpression :: (Phantoms.TTerm Syntax.Listcomp -> Phantoms.TTerm Syntax.NamedExpression -> Phantoms.TTerm Syntax.Listcomp)
listcompWithExpression original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.Listcomp"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "expression"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "forIfClauses"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.Listcomp"),
          Core.projectionField = (Core.Name "forIfClauses")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

listcompWithForIfClauses :: (Phantoms.TTerm Syntax.Listcomp -> Phantoms.TTerm Syntax.ForIfClauses -> Phantoms.TTerm Syntax.Listcomp)
listcompWithForIfClauses original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.Listcomp"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "expression"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.Listcomp"),
          Core.projectionField = (Core.Name "expression")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "forIfClauses"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

setcomp :: (Phantoms.TTerm Syntax.NamedExpression -> Phantoms.TTerm Syntax.ForIfClauses -> Phantoms.TTerm Syntax.Setcomp)
setcomp expression forIfClauses = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.Setcomp"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "expression"),
      Core.fieldTerm = (Phantoms.unTTerm expression)},
    Core.Field {
      Core.fieldName = (Core.Name "forIfClauses"),
      Core.fieldTerm = (Phantoms.unTTerm forIfClauses)}]})))

setcompExpression :: (Phantoms.TTerm Syntax.Setcomp -> Phantoms.TTerm Syntax.NamedExpression)
setcompExpression x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.Setcomp"),
    Core.projectionField = (Core.Name "expression")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

setcompForIfClauses :: (Phantoms.TTerm Syntax.Setcomp -> Phantoms.TTerm Syntax.ForIfClauses)
setcompForIfClauses x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.Setcomp"),
    Core.projectionField = (Core.Name "forIfClauses")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

setcompWithExpression :: (Phantoms.TTerm Syntax.Setcomp -> Phantoms.TTerm Syntax.NamedExpression -> Phantoms.TTerm Syntax.Setcomp)
setcompWithExpression original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.Setcomp"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "expression"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "forIfClauses"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.Setcomp"),
          Core.projectionField = (Core.Name "forIfClauses")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

setcompWithForIfClauses :: (Phantoms.TTerm Syntax.Setcomp -> Phantoms.TTerm Syntax.ForIfClauses -> Phantoms.TTerm Syntax.Setcomp)
setcompWithForIfClauses original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.Setcomp"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "expression"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.Setcomp"),
          Core.projectionField = (Core.Name "expression")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "forIfClauses"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

genexp :: (Phantoms.TTerm Syntax.GenexpHead -> Phantoms.TTerm Syntax.ForIfClauses -> Phantoms.TTerm Syntax.Genexp)
genexp head tail = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.Genexp"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "head"),
      Core.fieldTerm = (Phantoms.unTTerm head)},
    Core.Field {
      Core.fieldName = (Core.Name "tail"),
      Core.fieldTerm = (Phantoms.unTTerm tail)}]})))

genexpHead :: (Phantoms.TTerm Syntax.Genexp -> Phantoms.TTerm Syntax.GenexpHead)
genexpHead x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.Genexp"),
    Core.projectionField = (Core.Name "head")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

genexpTail :: (Phantoms.TTerm Syntax.Genexp -> Phantoms.TTerm Syntax.ForIfClauses)
genexpTail x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.Genexp"),
    Core.projectionField = (Core.Name "tail")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

genexpWithHead :: (Phantoms.TTerm Syntax.Genexp -> Phantoms.TTerm Syntax.GenexpHead -> Phantoms.TTerm Syntax.Genexp)
genexpWithHead original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.Genexp"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "head"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "tail"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.Genexp"),
          Core.projectionField = (Core.Name "tail")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

genexpWithTail :: (Phantoms.TTerm Syntax.Genexp -> Phantoms.TTerm Syntax.ForIfClauses -> Phantoms.TTerm Syntax.Genexp)
genexpWithTail original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.Genexp"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "head"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.Genexp"),
          Core.projectionField = (Core.Name "head")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "tail"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

genexpHeadAssignment :: (Phantoms.TTerm Syntax.AssignmentExpression -> Phantoms.TTerm Syntax.GenexpHead)
genexpHeadAssignment x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.GenexpHead"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "assignment"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

genexpHeadExpression :: (Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.GenexpHead)
genexpHeadExpression x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.GenexpHead"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "expression"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

dictcomp :: (Phantoms.TTerm Syntax.Kvpair -> Phantoms.TTerm Syntax.ForIfClauses -> Phantoms.TTerm Syntax.Dictcomp)
dictcomp kvpair forIfClauses = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.Dictcomp"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "kvpair"),
      Core.fieldTerm = (Phantoms.unTTerm kvpair)},
    Core.Field {
      Core.fieldName = (Core.Name "forIfClauses"),
      Core.fieldTerm = (Phantoms.unTTerm forIfClauses)}]})))

dictcompKvpair :: (Phantoms.TTerm Syntax.Dictcomp -> Phantoms.TTerm Syntax.Kvpair)
dictcompKvpair x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.Dictcomp"),
    Core.projectionField = (Core.Name "kvpair")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

dictcompForIfClauses :: (Phantoms.TTerm Syntax.Dictcomp -> Phantoms.TTerm Syntax.ForIfClauses)
dictcompForIfClauses x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.Dictcomp"),
    Core.projectionField = (Core.Name "forIfClauses")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

dictcompWithKvpair :: (Phantoms.TTerm Syntax.Dictcomp -> Phantoms.TTerm Syntax.Kvpair -> Phantoms.TTerm Syntax.Dictcomp)
dictcompWithKvpair original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.Dictcomp"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "kvpair"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "forIfClauses"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.Dictcomp"),
          Core.projectionField = (Core.Name "forIfClauses")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

dictcompWithForIfClauses :: (Phantoms.TTerm Syntax.Dictcomp -> Phantoms.TTerm Syntax.ForIfClauses -> Phantoms.TTerm Syntax.Dictcomp)
dictcompWithForIfClauses original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.Dictcomp"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "kvpair"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.Dictcomp"),
          Core.projectionField = (Core.Name "kvpair")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "forIfClauses"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

args :: (Phantoms.TTerm [Syntax.PosArg] -> Phantoms.TTerm [Syntax.KwargOrStarred] -> Phantoms.TTerm [Syntax.KwargOrDoubleStarred] -> Phantoms.TTerm Syntax.Args)
args positional kwargOrStarred kwargOrDoubleStarred = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.Args"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "positional"),
      Core.fieldTerm = (Phantoms.unTTerm positional)},
    Core.Field {
      Core.fieldName = (Core.Name "kwargOrStarred"),
      Core.fieldTerm = (Phantoms.unTTerm kwargOrStarred)},
    Core.Field {
      Core.fieldName = (Core.Name "kwargOrDoubleStarred"),
      Core.fieldTerm = (Phantoms.unTTerm kwargOrDoubleStarred)}]})))

argsPositional :: (Phantoms.TTerm Syntax.Args -> Phantoms.TTerm [Syntax.PosArg])
argsPositional x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.Args"),
    Core.projectionField = (Core.Name "positional")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

argsKwargOrStarred :: (Phantoms.TTerm Syntax.Args -> Phantoms.TTerm [Syntax.KwargOrStarred])
argsKwargOrStarred x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.Args"),
    Core.projectionField = (Core.Name "kwargOrStarred")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

argsKwargOrDoubleStarred :: (Phantoms.TTerm Syntax.Args -> Phantoms.TTerm [Syntax.KwargOrDoubleStarred])
argsKwargOrDoubleStarred x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.Args"),
    Core.projectionField = (Core.Name "kwargOrDoubleStarred")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

argsWithPositional :: (Phantoms.TTerm Syntax.Args -> Phantoms.TTerm [Syntax.PosArg] -> Phantoms.TTerm Syntax.Args)
argsWithPositional original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.Args"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "positional"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "kwargOrStarred"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.Args"),
          Core.projectionField = (Core.Name "kwargOrStarred")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "kwargOrDoubleStarred"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.Args"),
          Core.projectionField = (Core.Name "kwargOrDoubleStarred")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

argsWithKwargOrStarred :: (Phantoms.TTerm Syntax.Args -> Phantoms.TTerm [Syntax.KwargOrStarred] -> Phantoms.TTerm Syntax.Args)
argsWithKwargOrStarred original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.Args"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "positional"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.Args"),
          Core.projectionField = (Core.Name "positional")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "kwargOrStarred"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "kwargOrDoubleStarred"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.Args"),
          Core.projectionField = (Core.Name "kwargOrDoubleStarred")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

argsWithKwargOrDoubleStarred :: (Phantoms.TTerm Syntax.Args -> Phantoms.TTerm [Syntax.KwargOrDoubleStarred] -> Phantoms.TTerm Syntax.Args)
argsWithKwargOrDoubleStarred original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.Args"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "positional"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.Args"),
          Core.projectionField = (Core.Name "positional")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "kwargOrStarred"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.Args"),
          Core.projectionField = (Core.Name "kwargOrStarred")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "kwargOrDoubleStarred"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

posArgStarred :: (Phantoms.TTerm Syntax.StarredExpression -> Phantoms.TTerm Syntax.PosArg)
posArgStarred x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.PosArg"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "starred"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

posArgAssignment :: (Phantoms.TTerm Syntax.AssignmentExpression -> Phantoms.TTerm Syntax.PosArg)
posArgAssignment x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.PosArg"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "assignment"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

posArgExpression :: (Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.PosArg)
posArgExpression x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.PosArg"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "expression"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

starredExpression :: (Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.StarredExpression)
starredExpression x = (Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
  Core.wrappedTermTypeName = (Core.Name "hydra.ext.python.syntax.StarredExpression"),
  Core.wrappedTermBody = (Phantoms.unTTerm x)})))

unStarredExpression :: (Phantoms.TTerm Syntax.StarredExpression -> Phantoms.TTerm Syntax.Expression)
unStarredExpression x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.python.syntax.StarredExpression")))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

kwargOrStarredKwarg :: (Phantoms.TTerm Syntax.Kwarg -> Phantoms.TTerm Syntax.KwargOrStarred)
kwargOrStarredKwarg x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.KwargOrStarred"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "kwarg"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

kwargOrStarredStarred :: (Phantoms.TTerm Syntax.StarredExpression -> Phantoms.TTerm Syntax.KwargOrStarred)
kwargOrStarredStarred x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.KwargOrStarred"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "starred"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

kwarg :: (Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.Kwarg)
kwarg name value = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.Kwarg"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Phantoms.unTTerm name)},
    Core.Field {
      Core.fieldName = (Core.Name "value"),
      Core.fieldTerm = (Phantoms.unTTerm value)}]})))

kwargName :: (Phantoms.TTerm Syntax.Kwarg -> Phantoms.TTerm Syntax.Name)
kwargName x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.Kwarg"),
    Core.projectionField = (Core.Name "name")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

kwargValue :: (Phantoms.TTerm Syntax.Kwarg -> Phantoms.TTerm Syntax.Expression)
kwargValue x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.Kwarg"),
    Core.projectionField = (Core.Name "value")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

kwargWithName :: (Phantoms.TTerm Syntax.Kwarg -> Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.Kwarg)
kwargWithName original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.Kwarg"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "value"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.Kwarg"),
          Core.projectionField = (Core.Name "value")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

kwargWithValue :: (Phantoms.TTerm Syntax.Kwarg -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.Kwarg)
kwargWithValue original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.Kwarg"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.Kwarg"),
          Core.projectionField = (Core.Name "name")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "value"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

kwargOrDoubleStarredKwarg :: (Phantoms.TTerm Syntax.Kwarg -> Phantoms.TTerm Syntax.KwargOrDoubleStarred)
kwargOrDoubleStarredKwarg x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.KwargOrDoubleStarred"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "kwarg"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

kwargOrDoubleStarredDoubleStarred :: (Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.KwargOrDoubleStarred)
kwargOrDoubleStarredDoubleStarred x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.KwargOrDoubleStarred"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "doubleStarred"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

starTargetsListSeq :: (Phantoms.TTerm [Syntax.StarTarget] -> Phantoms.TTerm Syntax.StarTargetsListSeq)
starTargetsListSeq x = (Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
  Core.wrappedTermTypeName = (Core.Name "hydra.ext.python.syntax.StarTargetsListSeq"),
  Core.wrappedTermBody = (Phantoms.unTTerm x)})))

unStarTargetsListSeq :: (Phantoms.TTerm Syntax.StarTargetsListSeq -> Phantoms.TTerm [Syntax.StarTarget])
unStarTargetsListSeq x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.python.syntax.StarTargetsListSeq")))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

starTargetsTupleSeq :: (Phantoms.TTerm [Syntax.StarTarget] -> Phantoms.TTerm Syntax.StarTargetsTupleSeq)
starTargetsTupleSeq x = (Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
  Core.wrappedTermTypeName = (Core.Name "hydra.ext.python.syntax.StarTargetsTupleSeq"),
  Core.wrappedTermBody = (Phantoms.unTTerm x)})))

unStarTargetsTupleSeq :: (Phantoms.TTerm Syntax.StarTargetsTupleSeq -> Phantoms.TTerm [Syntax.StarTarget])
unStarTargetsTupleSeq x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.python.syntax.StarTargetsTupleSeq")))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

starTargetStarred :: (Phantoms.TTerm Syntax.StarTarget -> Phantoms.TTerm Syntax.StarTarget)
starTargetStarred x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.StarTarget"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "starred"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

starTargetUnstarred :: (Phantoms.TTerm Syntax.TargetWithStarAtom -> Phantoms.TTerm Syntax.StarTarget)
starTargetUnstarred x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.StarTarget"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "unstarred"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

targetWithStarAtomProject :: (Phantoms.TTerm Syntax.TPrimaryAndName -> Phantoms.TTerm Syntax.TargetWithStarAtom)
targetWithStarAtomProject x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.TargetWithStarAtom"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "project"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

targetWithStarAtomSlices :: (Phantoms.TTerm Syntax.TPrimaryAndSlices -> Phantoms.TTerm Syntax.TargetWithStarAtom)
targetWithStarAtomSlices x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.TargetWithStarAtom"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "slices"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

targetWithStarAtomAtom :: (Phantoms.TTerm Syntax.StarAtom -> Phantoms.TTerm Syntax.TargetWithStarAtom)
targetWithStarAtomAtom x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.TargetWithStarAtom"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "atom"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

tPrimaryAndName :: (Phantoms.TTerm Syntax.TPrimary -> Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.TPrimaryAndName)
tPrimaryAndName primary name = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.TPrimaryAndName"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "primary"),
      Core.fieldTerm = (Phantoms.unTTerm primary)},
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Phantoms.unTTerm name)}]})))

tPrimaryAndNamePrimary :: (Phantoms.TTerm Syntax.TPrimaryAndName -> Phantoms.TTerm Syntax.TPrimary)
tPrimaryAndNamePrimary x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.TPrimaryAndName"),
    Core.projectionField = (Core.Name "primary")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

tPrimaryAndNameName :: (Phantoms.TTerm Syntax.TPrimaryAndName -> Phantoms.TTerm Syntax.Name)
tPrimaryAndNameName x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.TPrimaryAndName"),
    Core.projectionField = (Core.Name "name")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

tPrimaryAndNameWithPrimary :: (Phantoms.TTerm Syntax.TPrimaryAndName -> Phantoms.TTerm Syntax.TPrimary -> Phantoms.TTerm Syntax.TPrimaryAndName)
tPrimaryAndNameWithPrimary original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.TPrimaryAndName"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "primary"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.TPrimaryAndName"),
          Core.projectionField = (Core.Name "name")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

tPrimaryAndNameWithName :: (Phantoms.TTerm Syntax.TPrimaryAndName -> Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.TPrimaryAndName)
tPrimaryAndNameWithName original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.TPrimaryAndName"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "primary"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.TPrimaryAndName"),
          Core.projectionField = (Core.Name "primary")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

tPrimaryAndSlices :: (Phantoms.TTerm Syntax.TPrimary -> Phantoms.TTerm Syntax.Slices -> Phantoms.TTerm Syntax.TPrimaryAndSlices)
tPrimaryAndSlices primary slices = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.TPrimaryAndSlices"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "primary"),
      Core.fieldTerm = (Phantoms.unTTerm primary)},
    Core.Field {
      Core.fieldName = (Core.Name "slices"),
      Core.fieldTerm = (Phantoms.unTTerm slices)}]})))

tPrimaryAndSlicesPrimary :: (Phantoms.TTerm Syntax.TPrimaryAndSlices -> Phantoms.TTerm Syntax.TPrimary)
tPrimaryAndSlicesPrimary x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.TPrimaryAndSlices"),
    Core.projectionField = (Core.Name "primary")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

tPrimaryAndSlicesSlices :: (Phantoms.TTerm Syntax.TPrimaryAndSlices -> Phantoms.TTerm Syntax.Slices)
tPrimaryAndSlicesSlices x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.TPrimaryAndSlices"),
    Core.projectionField = (Core.Name "slices")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

tPrimaryAndSlicesWithPrimary :: (Phantoms.TTerm Syntax.TPrimaryAndSlices -> Phantoms.TTerm Syntax.TPrimary -> Phantoms.TTerm Syntax.TPrimaryAndSlices)
tPrimaryAndSlicesWithPrimary original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.TPrimaryAndSlices"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "primary"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "slices"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.TPrimaryAndSlices"),
          Core.projectionField = (Core.Name "slices")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

tPrimaryAndSlicesWithSlices :: (Phantoms.TTerm Syntax.TPrimaryAndSlices -> Phantoms.TTerm Syntax.Slices -> Phantoms.TTerm Syntax.TPrimaryAndSlices)
tPrimaryAndSlicesWithSlices original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.TPrimaryAndSlices"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "primary"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.TPrimaryAndSlices"),
          Core.projectionField = (Core.Name "primary")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "slices"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

starAtomName :: (Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.StarAtom)
starAtomName x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.StarAtom"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "name"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

starAtomTargetWithStarAtom :: (Phantoms.TTerm Syntax.TargetWithStarAtom -> Phantoms.TTerm Syntax.StarAtom)
starAtomTargetWithStarAtom x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.StarAtom"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "targetWithStarAtom"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

starAtomStarTargetsTupleSeq :: (Phantoms.TTerm (Maybe Syntax.StarTargetsTupleSeq) -> Phantoms.TTerm Syntax.StarAtom)
starAtomStarTargetsTupleSeq x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.StarAtom"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "starTargetsTupleSeq"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

starAtomStarTargetsListSeq :: (Phantoms.TTerm (Maybe Syntax.StarTargetsListSeq) -> Phantoms.TTerm Syntax.StarAtom)
starAtomStarTargetsListSeq x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.StarAtom"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "starTargetsListSeq"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

singleTargetSubscriptAttributeTarget :: (Phantoms.TTerm Syntax.SingleSubscriptAttributeTarget -> Phantoms.TTerm Syntax.SingleTarget)
singleTargetSubscriptAttributeTarget x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.SingleTarget"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "subscriptAttributeTarget"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

singleTargetName :: (Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.SingleTarget)
singleTargetName x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.SingleTarget"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "name"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

singleTargetParens :: (Phantoms.TTerm Syntax.SingleTarget -> Phantoms.TTerm Syntax.SingleTarget)
singleTargetParens x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.SingleTarget"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "parens"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

singleSubscriptAttributeTargetPrimaryAndName :: (Phantoms.TTerm Syntax.TPrimaryAndName -> Phantoms.TTerm Syntax.SingleSubscriptAttributeTarget)
singleSubscriptAttributeTargetPrimaryAndName x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.SingleSubscriptAttributeTarget"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "primaryAndName"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

singleSubscriptAttributeTargetPrimaryAndSlices :: (Phantoms.TTerm Syntax.TPrimaryAndSlices -> Phantoms.TTerm Syntax.SingleSubscriptAttributeTarget)
singleSubscriptAttributeTargetPrimaryAndSlices x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.SingleSubscriptAttributeTarget"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "primaryAndSlices"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

tPrimaryPrimaryAndName :: (Phantoms.TTerm Syntax.TPrimaryAndName -> Phantoms.TTerm Syntax.TPrimary)
tPrimaryPrimaryAndName x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.TPrimary"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "primaryAndName"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

tPrimaryPrimaryAndSlices :: (Phantoms.TTerm Syntax.TPrimaryAndSlices -> Phantoms.TTerm Syntax.TPrimary)
tPrimaryPrimaryAndSlices x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.TPrimary"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "primaryAndSlices"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

tPrimaryPrimaryAndGenexp :: (Phantoms.TTerm Syntax.TPrimaryAndGenexp -> Phantoms.TTerm Syntax.TPrimary)
tPrimaryPrimaryAndGenexp x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.TPrimary"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "primaryAndGenexp"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

tPrimaryPrimaryAndArguments :: (Phantoms.TTerm Syntax.TPrimaryAndArguments -> Phantoms.TTerm Syntax.TPrimary)
tPrimaryPrimaryAndArguments x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.TPrimary"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "primaryAndArguments"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

tPrimaryAtom :: (Phantoms.TTerm Syntax.Atom -> Phantoms.TTerm Syntax.TPrimary)
tPrimaryAtom x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.TPrimary"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "atom"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

tPrimaryAndGenexp :: (Phantoms.TTerm Syntax.TPrimary -> Phantoms.TTerm Syntax.Genexp -> Phantoms.TTerm Syntax.TPrimaryAndGenexp)
tPrimaryAndGenexp primary genexp = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.TPrimaryAndGenexp"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "primary"),
      Core.fieldTerm = (Phantoms.unTTerm primary)},
    Core.Field {
      Core.fieldName = (Core.Name "genexp"),
      Core.fieldTerm = (Phantoms.unTTerm genexp)}]})))

tPrimaryAndGenexpPrimary :: (Phantoms.TTerm Syntax.TPrimaryAndGenexp -> Phantoms.TTerm Syntax.TPrimary)
tPrimaryAndGenexpPrimary x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.TPrimaryAndGenexp"),
    Core.projectionField = (Core.Name "primary")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

tPrimaryAndGenexpGenexp :: (Phantoms.TTerm Syntax.TPrimaryAndGenexp -> Phantoms.TTerm Syntax.Genexp)
tPrimaryAndGenexpGenexp x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.TPrimaryAndGenexp"),
    Core.projectionField = (Core.Name "genexp")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

tPrimaryAndGenexpWithPrimary :: (Phantoms.TTerm Syntax.TPrimaryAndGenexp -> Phantoms.TTerm Syntax.TPrimary -> Phantoms.TTerm Syntax.TPrimaryAndGenexp)
tPrimaryAndGenexpWithPrimary original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.TPrimaryAndGenexp"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "primary"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "genexp"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.TPrimaryAndGenexp"),
          Core.projectionField = (Core.Name "genexp")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

tPrimaryAndGenexpWithGenexp :: (Phantoms.TTerm Syntax.TPrimaryAndGenexp -> Phantoms.TTerm Syntax.Genexp -> Phantoms.TTerm Syntax.TPrimaryAndGenexp)
tPrimaryAndGenexpWithGenexp original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.TPrimaryAndGenexp"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "primary"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.TPrimaryAndGenexp"),
          Core.projectionField = (Core.Name "primary")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "genexp"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

tPrimaryAndArguments :: (Phantoms.TTerm Syntax.TPrimary -> Phantoms.TTerm (Maybe Syntax.Args) -> Phantoms.TTerm Syntax.TPrimaryAndArguments)
tPrimaryAndArguments primary arguments = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.TPrimaryAndArguments"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "primary"),
      Core.fieldTerm = (Phantoms.unTTerm primary)},
    Core.Field {
      Core.fieldName = (Core.Name "arguments"),
      Core.fieldTerm = (Phantoms.unTTerm arguments)}]})))

tPrimaryAndArgumentsPrimary :: (Phantoms.TTerm Syntax.TPrimaryAndArguments -> Phantoms.TTerm Syntax.TPrimary)
tPrimaryAndArgumentsPrimary x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.TPrimaryAndArguments"),
    Core.projectionField = (Core.Name "primary")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

tPrimaryAndArgumentsArguments :: (Phantoms.TTerm Syntax.TPrimaryAndArguments -> Phantoms.TTerm (Maybe Syntax.Args))
tPrimaryAndArgumentsArguments x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.TPrimaryAndArguments"),
    Core.projectionField = (Core.Name "arguments")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

tPrimaryAndArgumentsWithPrimary :: (Phantoms.TTerm Syntax.TPrimaryAndArguments -> Phantoms.TTerm Syntax.TPrimary -> Phantoms.TTerm Syntax.TPrimaryAndArguments)
tPrimaryAndArgumentsWithPrimary original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.TPrimaryAndArguments"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "primary"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "arguments"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.TPrimaryAndArguments"),
          Core.projectionField = (Core.Name "arguments")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

tPrimaryAndArgumentsWithArguments :: (Phantoms.TTerm Syntax.TPrimaryAndArguments -> Phantoms.TTerm (Maybe Syntax.Args) -> Phantoms.TTerm Syntax.TPrimaryAndArguments)
tPrimaryAndArgumentsWithArguments original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.python.syntax.TPrimaryAndArguments"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "primary"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.python.syntax.TPrimaryAndArguments"),
          Core.projectionField = (Core.Name "primary")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "arguments"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

delTargets :: (Phantoms.TTerm [Syntax.DelTarget] -> Phantoms.TTerm Syntax.DelTargets)
delTargets x = (Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
  Core.wrappedTermTypeName = (Core.Name "hydra.ext.python.syntax.DelTargets"),
  Core.wrappedTermBody = (Phantoms.unTTerm x)})))

unDelTargets :: (Phantoms.TTerm Syntax.DelTargets -> Phantoms.TTerm [Syntax.DelTarget])
unDelTargets x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.python.syntax.DelTargets")))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

delTargetPrimaryAndName :: (Phantoms.TTerm Syntax.TPrimaryAndName -> Phantoms.TTerm Syntax.DelTarget)
delTargetPrimaryAndName x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.DelTarget"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "primaryAndName"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

delTargetPrimaryAndSlices :: (Phantoms.TTerm Syntax.TPrimaryAndSlices -> Phantoms.TTerm Syntax.DelTarget)
delTargetPrimaryAndSlices x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.DelTarget"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "primaryAndSlices"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

delTargetDelTAtom :: (Phantoms.TTerm Syntax.DelTAtom -> Phantoms.TTerm Syntax.DelTarget)
delTargetDelTAtom x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.DelTarget"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "delTAtom"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

delTAtomName :: (Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.DelTAtom)
delTAtomName x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.DelTAtom"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "name"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

delTAtomTarget :: (Phantoms.TTerm Syntax.DelTarget -> Phantoms.TTerm Syntax.DelTAtom)
delTAtomTarget x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.DelTAtom"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "target"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

delTAtomTargets :: (Phantoms.TTerm Syntax.DelTargets -> Phantoms.TTerm Syntax.DelTAtom)
delTAtomTargets x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.DelTAtom"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "targets"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

typeExpressionExpression :: (Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.TypeExpression)
typeExpressionExpression x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.TypeExpression"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "expression"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

typeExpressionStarredExpression :: (Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.TypeExpression)
typeExpressionStarredExpression x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.TypeExpression"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "starredExpression"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

typeExpressionDoubleStarredExpression :: (Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.TypeExpression)
typeExpressionDoubleStarredExpression x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.python.syntax.TypeExpression"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "doubleStarredExpression"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

funcTypeComment :: (Phantoms.TTerm Syntax.TypeComment -> Phantoms.TTerm Syntax.FuncTypeComment)
funcTypeComment x = (Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
  Core.wrappedTermTypeName = (Core.Name "hydra.ext.python.syntax.FuncTypeComment"),
  Core.wrappedTermBody = (Phantoms.unTTerm x)})))

unFuncTypeComment :: (Phantoms.TTerm Syntax.FuncTypeComment -> Phantoms.TTerm Syntax.TypeComment)
unFuncTypeComment x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.python.syntax.FuncTypeComment")))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))
