-- Note: this is an automatically generated file. Do not edit.

-- | DSL functions for hydra.ext.scala.meta

module Hydra.Dsl.Ext.Scala.Meta where

import qualified Hydra.Core as Core
import qualified Hydra.Ext.Scala.Meta as Meta
import qualified Hydra.Phantoms as Phantoms
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

predefString :: (Phantoms.TTerm String -> Phantoms.TTerm Meta.PredefString)
predefString x = (Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
  Core.wrappedTermTypeName = (Core.Name "hydra.ext.scala.meta.PredefString"),
  Core.wrappedTermBody = (Phantoms.unTTerm x)})))

unPredefString :: (Phantoms.TTerm Meta.PredefString -> Phantoms.TTerm String)
unPredefString x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.scala.meta.PredefString")))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

scalaSymbol :: (Phantoms.TTerm String -> Phantoms.TTerm Meta.ScalaSymbol)
scalaSymbol name = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.ScalaSymbol"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Phantoms.unTTerm name)}]})))

scalaSymbolName :: (Phantoms.TTerm Meta.ScalaSymbol -> Phantoms.TTerm String)
scalaSymbolName x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.ScalaSymbol"),
    Core.projectionField = (Core.Name "name")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

scalaSymbolWithName :: (Phantoms.TTerm Meta.ScalaSymbol -> Phantoms.TTerm String -> Phantoms.TTerm Meta.ScalaSymbol)
scalaSymbolWithName original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.ScalaSymbol"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

treeRef :: (Phantoms.TTerm Meta.Ref -> Phantoms.TTerm Meta.Tree)
treeRef x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Tree"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "ref"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

treeStat :: (Phantoms.TTerm Meta.Stat -> Phantoms.TTerm Meta.Tree)
treeStat x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Tree"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "stat"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

treeType :: (Phantoms.TTerm Meta.Type -> Phantoms.TTerm Meta.Tree)
treeType x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Tree"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "type"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

treeBounds :: (Phantoms.TTerm Meta.TypeBounds -> Phantoms.TTerm Meta.Tree)
treeBounds x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Tree"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "bounds"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

treePat :: (Phantoms.TTerm Meta.Pat -> Phantoms.TTerm Meta.Tree)
treePat x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Tree"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "pat"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

treeMember :: (Phantoms.TTerm Meta.Member -> Phantoms.TTerm Meta.Tree)
treeMember x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Tree"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "member"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

treeCtor :: (Phantoms.TTerm Meta.Ctor -> Phantoms.TTerm Meta.Tree)
treeCtor x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Tree"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "ctor"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

treeTemplate :: (Phantoms.TTerm Meta.Template -> Phantoms.TTerm Meta.Tree)
treeTemplate x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Tree"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "template"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

treeMod :: (Phantoms.TTerm Meta.Mod -> Phantoms.TTerm Meta.Tree)
treeMod x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Tree"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "mod"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

treeEnumerator :: (Phantoms.TTerm Meta.Enumerator -> Phantoms.TTerm Meta.Tree)
treeEnumerator x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Tree"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "enumerator"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

treeImporter :: (Phantoms.TTerm Meta.Importer -> Phantoms.TTerm Meta.Tree)
treeImporter x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Tree"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "importer"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

treeImportee :: (Phantoms.TTerm Meta.Importee -> Phantoms.TTerm Meta.Tree)
treeImportee x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Tree"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "importee"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

treeCaseTree :: (Phantoms.TTerm Meta.CaseTree -> Phantoms.TTerm Meta.Tree)
treeCaseTree x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Tree"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "caseTree"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

treeSource :: (Phantoms.TTerm Meta.Source -> Phantoms.TTerm Meta.Tree)
treeSource x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Tree"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "source"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

treeQuasi :: (Phantoms.TTerm Meta.Quasi -> Phantoms.TTerm Meta.Tree)
treeQuasi x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Tree"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "quasi"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

refName :: (Phantoms.TTerm Meta.Name -> Phantoms.TTerm Meta.Ref)
refName x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Ref"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "name"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

refInit :: (Phantoms.TTerm Meta.Init -> Phantoms.TTerm Meta.Ref)
refInit x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Ref"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "init"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

statTerm :: (Phantoms.TTerm Meta.Data -> Phantoms.TTerm Meta.Stat)
statTerm x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Stat"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "term"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

statDecl :: (Phantoms.TTerm Meta.Decl -> Phantoms.TTerm Meta.Stat)
statDecl x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Stat"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "decl"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

statDefn :: (Phantoms.TTerm Meta.Defn -> Phantoms.TTerm Meta.Stat)
statDefn x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Stat"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "defn"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

statImportExport :: (Phantoms.TTerm Meta.ImportExportStat -> Phantoms.TTerm Meta.Stat)
statImportExport x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Stat"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "importExport"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

nameValue :: (Phantoms.TTerm String -> Phantoms.TTerm Meta.Name)
nameValue x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Name"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "value"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

nameAnonymous :: (Phantoms.TTerm Meta.Name)
nameAnonymous = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Name"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "anonymous"),
    Core.fieldTerm = Core.TermUnit}})))

nameIndeterminate :: (Phantoms.TTerm Meta.PredefString -> Phantoms.TTerm Meta.Name)
nameIndeterminate x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Name"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "indeterminate"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

litNull :: (Phantoms.TTerm Meta.Lit)
litNull = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Lit"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "null"),
    Core.fieldTerm = Core.TermUnit}})))

litInt :: (Phantoms.TTerm Int -> Phantoms.TTerm Meta.Lit)
litInt x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Lit"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "int"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

litDouble :: (Phantoms.TTerm Double -> Phantoms.TTerm Meta.Lit)
litDouble x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Lit"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "double"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

litFloat :: (Phantoms.TTerm Float -> Phantoms.TTerm Meta.Lit)
litFloat x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Lit"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "float"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

litByte :: (Phantoms.TTerm I.Int8 -> Phantoms.TTerm Meta.Lit)
litByte x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Lit"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "byte"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

litShort :: (Phantoms.TTerm I.Int16 -> Phantoms.TTerm Meta.Lit)
litShort x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Lit"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "short"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

litChar :: (Phantoms.TTerm Int -> Phantoms.TTerm Meta.Lit)
litChar x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Lit"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "char"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

litLong :: (Phantoms.TTerm I.Int64 -> Phantoms.TTerm Meta.Lit)
litLong x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Lit"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "long"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

litBoolean :: (Phantoms.TTerm Bool -> Phantoms.TTerm Meta.Lit)
litBoolean x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Lit"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "boolean"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

litUnit :: (Phantoms.TTerm Meta.Lit)
litUnit = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Lit"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "unit"),
    Core.fieldTerm = Core.TermUnit}})))

litString :: (Phantoms.TTerm String -> Phantoms.TTerm Meta.Lit)
litString x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Lit"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "string"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

litSymbol :: (Phantoms.TTerm Meta.ScalaSymbol -> Phantoms.TTerm Meta.Lit)
litSymbol x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Lit"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "symbol"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

dataLit :: (Phantoms.TTerm Meta.Lit -> Phantoms.TTerm Meta.Data)
dataLit x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Data"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "lit"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

dataRef :: (Phantoms.TTerm Meta.Data_Ref -> Phantoms.TTerm Meta.Data)
dataRef x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Data"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "ref"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

dataInterpolate :: (Phantoms.TTerm Meta.Data_Interpolate -> Phantoms.TTerm Meta.Data)
dataInterpolate x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Data"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "interpolate"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

dataXml :: (Phantoms.TTerm Meta.Data_Xml -> Phantoms.TTerm Meta.Data)
dataXml x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Data"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "xml"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

dataApply :: (Phantoms.TTerm Meta.Data_Apply -> Phantoms.TTerm Meta.Data)
dataApply x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Data"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "apply"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

dataApplyUsing :: (Phantoms.TTerm Meta.Data_ApplyUsing -> Phantoms.TTerm Meta.Data)
dataApplyUsing x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Data"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "applyUsing"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

dataApplyType :: (Phantoms.TTerm Meta.Data_ApplyType -> Phantoms.TTerm Meta.Data)
dataApplyType x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Data"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "applyType"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

dataAssign :: (Phantoms.TTerm Meta.Data_Assign -> Phantoms.TTerm Meta.Data)
dataAssign x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Data"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "assign"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

dataReturn :: (Phantoms.TTerm Meta.Data_Return -> Phantoms.TTerm Meta.Data)
dataReturn x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Data"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "return"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

dataThrow :: (Phantoms.TTerm Meta.Data_Throw -> Phantoms.TTerm Meta.Data)
dataThrow x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Data"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "throw"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

dataAscribe :: (Phantoms.TTerm Meta.Data_Ascribe -> Phantoms.TTerm Meta.Data)
dataAscribe x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Data"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "ascribe"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

dataAnnotate :: (Phantoms.TTerm Meta.Data_Annotate -> Phantoms.TTerm Meta.Data)
dataAnnotate x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Data"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "annotate"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

dataTuple :: (Phantoms.TTerm Meta.Data_Tuple -> Phantoms.TTerm Meta.Data)
dataTuple x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Data"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "tuple"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

dataBlock :: (Phantoms.TTerm Meta.Data_Block -> Phantoms.TTerm Meta.Data)
dataBlock x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Data"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "block"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

dataEndMarker :: (Phantoms.TTerm Meta.Data_EndMarker -> Phantoms.TTerm Meta.Data)
dataEndMarker x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Data"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "endMarker"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

dataIf :: (Phantoms.TTerm Meta.Data_If -> Phantoms.TTerm Meta.Data)
dataIf x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Data"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "if"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

dataQuotedMacroExpr :: (Phantoms.TTerm Meta.Data_QuotedMacroExpr -> Phantoms.TTerm Meta.Data)
dataQuotedMacroExpr x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Data"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "quotedMacroExpr"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

dataQuotedMacroType :: (Phantoms.TTerm Meta.Data_QuotedMacroType -> Phantoms.TTerm Meta.Data)
dataQuotedMacroType x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Data"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "quotedMacroType"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

dataSplicedMacroExpr :: (Phantoms.TTerm Meta.Data_SplicedMacroExpr -> Phantoms.TTerm Meta.Data)
dataSplicedMacroExpr x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Data"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "splicedMacroExpr"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

dataMatch :: (Phantoms.TTerm Meta.Data_Match -> Phantoms.TTerm Meta.Data)
dataMatch x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Data"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "match"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

dataTry :: (Phantoms.TTerm Meta.Data_Try -> Phantoms.TTerm Meta.Data)
dataTry x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Data"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "try"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

dataTryWithHandler :: (Phantoms.TTerm Meta.Data_TryWithHandler -> Phantoms.TTerm Meta.Data)
dataTryWithHandler x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Data"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "tryWithHandler"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

dataFunctionData :: (Phantoms.TTerm Meta.Data_FunctionData -> Phantoms.TTerm Meta.Data)
dataFunctionData x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Data"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "functionData"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

dataPolyFunction :: (Phantoms.TTerm Meta.Data_PolyFunction -> Phantoms.TTerm Meta.Data)
dataPolyFunction x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Data"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "polyFunction"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

dataPartialFunction :: (Phantoms.TTerm Meta.Data_PartialFunction -> Phantoms.TTerm Meta.Data)
dataPartialFunction x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Data"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "partialFunction"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

dataWhile :: (Phantoms.TTerm Meta.Data_While -> Phantoms.TTerm Meta.Data)
dataWhile x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Data"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "while"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

dataDo :: (Phantoms.TTerm Meta.Data_Do -> Phantoms.TTerm Meta.Data)
dataDo x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Data"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "do"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

dataFor :: (Phantoms.TTerm Meta.Data_For -> Phantoms.TTerm Meta.Data)
dataFor x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Data"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "for"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

dataForYield :: (Phantoms.TTerm Meta.Data_ForYield -> Phantoms.TTerm Meta.Data)
dataForYield x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Data"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "forYield"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

dataNew :: (Phantoms.TTerm Meta.Data_New -> Phantoms.TTerm Meta.Data)
dataNew x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Data"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "new"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

dataNewAnonymous :: (Phantoms.TTerm Meta.Data_NewAnonymous -> Phantoms.TTerm Meta.Data)
dataNewAnonymous x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Data"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "newAnonymous"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

dataPlaceholder :: (Phantoms.TTerm Meta.Data_Placeholder -> Phantoms.TTerm Meta.Data)
dataPlaceholder x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Data"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "placeholder"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

dataEta :: (Phantoms.TTerm Meta.Data_Eta -> Phantoms.TTerm Meta.Data)
dataEta x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Data"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "eta"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

dataRepeated :: (Phantoms.TTerm Meta.Data_Repeated -> Phantoms.TTerm Meta.Data)
dataRepeated x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Data"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "repeated"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

dataParam :: (Phantoms.TTerm Meta.Data_Param -> Phantoms.TTerm Meta.Data)
dataParam x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Data"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "param"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

data_RefThis :: (Phantoms.TTerm Meta.Data_This -> Phantoms.TTerm Meta.Data_Ref)
data_RefThis x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Data_Ref"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "this"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

data_RefSuper :: (Phantoms.TTerm Meta.Data_Super -> Phantoms.TTerm Meta.Data_Ref)
data_RefSuper x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Data_Ref"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "super"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

data_RefName :: (Phantoms.TTerm Meta.Data_Name -> Phantoms.TTerm Meta.Data_Ref)
data_RefName x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Data_Ref"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "name"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

data_RefAnonymous :: (Phantoms.TTerm Meta.Data_Anonymous -> Phantoms.TTerm Meta.Data_Ref)
data_RefAnonymous x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Data_Ref"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "anonymous"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

data_RefSelect :: (Phantoms.TTerm Meta.Data_Select -> Phantoms.TTerm Meta.Data_Ref)
data_RefSelect x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Data_Ref"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "select"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

data_RefApplyUnary :: (Phantoms.TTerm Meta.Data_ApplyUnary -> Phantoms.TTerm Meta.Data_Ref)
data_RefApplyUnary x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Data_Ref"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "applyUnary"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

data_This :: (Phantoms.TTerm () -> Phantoms.TTerm Meta.Data_This)
data_This x = (Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
  Core.wrappedTermTypeName = (Core.Name "hydra.ext.scala.meta.Data_This"),
  Core.wrappedTermBody = (Phantoms.unTTerm x)})))

unData_This :: (Phantoms.TTerm Meta.Data_This -> Phantoms.TTerm ())
unData_This x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.scala.meta.Data_This")))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

data_Super :: (Phantoms.TTerm Meta.Name -> Phantoms.TTerm Meta.Name -> Phantoms.TTerm Meta.Data_Super)
data_Super thisp superp = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Data_Super"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "thisp"),
      Core.fieldTerm = (Phantoms.unTTerm thisp)},
    Core.Field {
      Core.fieldName = (Core.Name "superp"),
      Core.fieldTerm = (Phantoms.unTTerm superp)}]})))

data_SuperThisp :: (Phantoms.TTerm Meta.Data_Super -> Phantoms.TTerm Meta.Name)
data_SuperThisp x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Data_Super"),
    Core.projectionField = (Core.Name "thisp")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

data_SuperSuperp :: (Phantoms.TTerm Meta.Data_Super -> Phantoms.TTerm Meta.Name)
data_SuperSuperp x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Data_Super"),
    Core.projectionField = (Core.Name "superp")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

data_SuperWithThisp :: (Phantoms.TTerm Meta.Data_Super -> Phantoms.TTerm Meta.Name -> Phantoms.TTerm Meta.Data_Super)
data_SuperWithThisp original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Data_Super"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "thisp"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "superp"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Data_Super"),
          Core.projectionField = (Core.Name "superp")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

data_SuperWithSuperp :: (Phantoms.TTerm Meta.Data_Super -> Phantoms.TTerm Meta.Name -> Phantoms.TTerm Meta.Data_Super)
data_SuperWithSuperp original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Data_Super"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "thisp"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Data_Super"),
          Core.projectionField = (Core.Name "thisp")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "superp"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

data_Name :: (Phantoms.TTerm Meta.PredefString -> Phantoms.TTerm Meta.Data_Name)
data_Name value = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Data_Name"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "value"),
      Core.fieldTerm = (Phantoms.unTTerm value)}]})))

data_NameValue :: (Phantoms.TTerm Meta.Data_Name -> Phantoms.TTerm Meta.PredefString)
data_NameValue x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Data_Name"),
    Core.projectionField = (Core.Name "value")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

data_NameWithValue :: (Phantoms.TTerm Meta.Data_Name -> Phantoms.TTerm Meta.PredefString -> Phantoms.TTerm Meta.Data_Name)
data_NameWithValue original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Data_Name"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "value"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

data_Anonymous :: (Phantoms.TTerm () -> Phantoms.TTerm Meta.Data_Anonymous)
data_Anonymous x = (Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
  Core.wrappedTermTypeName = (Core.Name "hydra.ext.scala.meta.Data_Anonymous"),
  Core.wrappedTermBody = (Phantoms.unTTerm x)})))

unData_Anonymous :: (Phantoms.TTerm Meta.Data_Anonymous -> Phantoms.TTerm ())
unData_Anonymous x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.scala.meta.Data_Anonymous")))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

data_Select :: (Phantoms.TTerm Meta.Data -> Phantoms.TTerm Meta.Data_Name -> Phantoms.TTerm Meta.Data_Select)
data_Select qual name = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Data_Select"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "qual"),
      Core.fieldTerm = (Phantoms.unTTerm qual)},
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Phantoms.unTTerm name)}]})))

data_SelectQual :: (Phantoms.TTerm Meta.Data_Select -> Phantoms.TTerm Meta.Data)
data_SelectQual x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Data_Select"),
    Core.projectionField = (Core.Name "qual")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

data_SelectName :: (Phantoms.TTerm Meta.Data_Select -> Phantoms.TTerm Meta.Data_Name)
data_SelectName x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Data_Select"),
    Core.projectionField = (Core.Name "name")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

data_SelectWithQual :: (Phantoms.TTerm Meta.Data_Select -> Phantoms.TTerm Meta.Data -> Phantoms.TTerm Meta.Data_Select)
data_SelectWithQual original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Data_Select"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "qual"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Data_Select"),
          Core.projectionField = (Core.Name "name")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

data_SelectWithName :: (Phantoms.TTerm Meta.Data_Select -> Phantoms.TTerm Meta.Data_Name -> Phantoms.TTerm Meta.Data_Select)
data_SelectWithName original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Data_Select"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "qual"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Data_Select"),
          Core.projectionField = (Core.Name "qual")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

data_Interpolate :: (Phantoms.TTerm Meta.Data_Name -> Phantoms.TTerm [Meta.Lit] -> Phantoms.TTerm [Meta.Data] -> Phantoms.TTerm Meta.Data_Interpolate)
data_Interpolate prefix parts args = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Data_Interpolate"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "prefix"),
      Core.fieldTerm = (Phantoms.unTTerm prefix)},
    Core.Field {
      Core.fieldName = (Core.Name "parts"),
      Core.fieldTerm = (Phantoms.unTTerm parts)},
    Core.Field {
      Core.fieldName = (Core.Name "args"),
      Core.fieldTerm = (Phantoms.unTTerm args)}]})))

data_InterpolatePrefix :: (Phantoms.TTerm Meta.Data_Interpolate -> Phantoms.TTerm Meta.Data_Name)
data_InterpolatePrefix x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Data_Interpolate"),
    Core.projectionField = (Core.Name "prefix")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

data_InterpolateParts :: (Phantoms.TTerm Meta.Data_Interpolate -> Phantoms.TTerm [Meta.Lit])
data_InterpolateParts x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Data_Interpolate"),
    Core.projectionField = (Core.Name "parts")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

data_InterpolateArgs :: (Phantoms.TTerm Meta.Data_Interpolate -> Phantoms.TTerm [Meta.Data])
data_InterpolateArgs x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Data_Interpolate"),
    Core.projectionField = (Core.Name "args")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

data_InterpolateWithPrefix :: (Phantoms.TTerm Meta.Data_Interpolate -> Phantoms.TTerm Meta.Data_Name -> Phantoms.TTerm Meta.Data_Interpolate)
data_InterpolateWithPrefix original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Data_Interpolate"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "prefix"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "parts"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Data_Interpolate"),
          Core.projectionField = (Core.Name "parts")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "args"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Data_Interpolate"),
          Core.projectionField = (Core.Name "args")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

data_InterpolateWithParts :: (Phantoms.TTerm Meta.Data_Interpolate -> Phantoms.TTerm [Meta.Lit] -> Phantoms.TTerm Meta.Data_Interpolate)
data_InterpolateWithParts original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Data_Interpolate"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "prefix"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Data_Interpolate"),
          Core.projectionField = (Core.Name "prefix")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "parts"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "args"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Data_Interpolate"),
          Core.projectionField = (Core.Name "args")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

data_InterpolateWithArgs :: (Phantoms.TTerm Meta.Data_Interpolate -> Phantoms.TTerm [Meta.Data] -> Phantoms.TTerm Meta.Data_Interpolate)
data_InterpolateWithArgs original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Data_Interpolate"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "prefix"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Data_Interpolate"),
          Core.projectionField = (Core.Name "prefix")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "parts"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Data_Interpolate"),
          Core.projectionField = (Core.Name "parts")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "args"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

data_Xml :: (Phantoms.TTerm [Meta.Lit] -> Phantoms.TTerm [Meta.Data] -> Phantoms.TTerm Meta.Data_Xml)
data_Xml parts args = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Data_Xml"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "parts"),
      Core.fieldTerm = (Phantoms.unTTerm parts)},
    Core.Field {
      Core.fieldName = (Core.Name "args"),
      Core.fieldTerm = (Phantoms.unTTerm args)}]})))

data_XmlParts :: (Phantoms.TTerm Meta.Data_Xml -> Phantoms.TTerm [Meta.Lit])
data_XmlParts x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Data_Xml"),
    Core.projectionField = (Core.Name "parts")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

data_XmlArgs :: (Phantoms.TTerm Meta.Data_Xml -> Phantoms.TTerm [Meta.Data])
data_XmlArgs x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Data_Xml"),
    Core.projectionField = (Core.Name "args")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

data_XmlWithParts :: (Phantoms.TTerm Meta.Data_Xml -> Phantoms.TTerm [Meta.Lit] -> Phantoms.TTerm Meta.Data_Xml)
data_XmlWithParts original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Data_Xml"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "parts"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "args"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Data_Xml"),
          Core.projectionField = (Core.Name "args")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

data_XmlWithArgs :: (Phantoms.TTerm Meta.Data_Xml -> Phantoms.TTerm [Meta.Data] -> Phantoms.TTerm Meta.Data_Xml)
data_XmlWithArgs original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Data_Xml"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "parts"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Data_Xml"),
          Core.projectionField = (Core.Name "parts")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "args"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

data_Apply :: (Phantoms.TTerm Meta.Data -> Phantoms.TTerm [Meta.Data] -> Phantoms.TTerm Meta.Data_Apply)
data_Apply fun args = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Data_Apply"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "fun"),
      Core.fieldTerm = (Phantoms.unTTerm fun)},
    Core.Field {
      Core.fieldName = (Core.Name "args"),
      Core.fieldTerm = (Phantoms.unTTerm args)}]})))

data_ApplyFun :: (Phantoms.TTerm Meta.Data_Apply -> Phantoms.TTerm Meta.Data)
data_ApplyFun x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Data_Apply"),
    Core.projectionField = (Core.Name "fun")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

data_ApplyArgs :: (Phantoms.TTerm Meta.Data_Apply -> Phantoms.TTerm [Meta.Data])
data_ApplyArgs x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Data_Apply"),
    Core.projectionField = (Core.Name "args")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

data_ApplyWithFun :: (Phantoms.TTerm Meta.Data_Apply -> Phantoms.TTerm Meta.Data -> Phantoms.TTerm Meta.Data_Apply)
data_ApplyWithFun original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Data_Apply"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "fun"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "args"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Data_Apply"),
          Core.projectionField = (Core.Name "args")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

data_ApplyWithArgs :: (Phantoms.TTerm Meta.Data_Apply -> Phantoms.TTerm [Meta.Data] -> Phantoms.TTerm Meta.Data_Apply)
data_ApplyWithArgs original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Data_Apply"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "fun"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Data_Apply"),
          Core.projectionField = (Core.Name "fun")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "args"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

data_ApplyUsing :: (Phantoms.TTerm Meta.Data -> Phantoms.TTerm [Meta.Data] -> Phantoms.TTerm Meta.Data_ApplyUsing)
data_ApplyUsing fun targs = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Data_ApplyUsing"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "fun"),
      Core.fieldTerm = (Phantoms.unTTerm fun)},
    Core.Field {
      Core.fieldName = (Core.Name "targs"),
      Core.fieldTerm = (Phantoms.unTTerm targs)}]})))

data_ApplyUsingFun :: (Phantoms.TTerm Meta.Data_ApplyUsing -> Phantoms.TTerm Meta.Data)
data_ApplyUsingFun x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Data_ApplyUsing"),
    Core.projectionField = (Core.Name "fun")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

data_ApplyUsingTargs :: (Phantoms.TTerm Meta.Data_ApplyUsing -> Phantoms.TTerm [Meta.Data])
data_ApplyUsingTargs x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Data_ApplyUsing"),
    Core.projectionField = (Core.Name "targs")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

data_ApplyUsingWithFun :: (Phantoms.TTerm Meta.Data_ApplyUsing -> Phantoms.TTerm Meta.Data -> Phantoms.TTerm Meta.Data_ApplyUsing)
data_ApplyUsingWithFun original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Data_ApplyUsing"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "fun"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "targs"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Data_ApplyUsing"),
          Core.projectionField = (Core.Name "targs")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

data_ApplyUsingWithTargs :: (Phantoms.TTerm Meta.Data_ApplyUsing -> Phantoms.TTerm [Meta.Data] -> Phantoms.TTerm Meta.Data_ApplyUsing)
data_ApplyUsingWithTargs original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Data_ApplyUsing"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "fun"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Data_ApplyUsing"),
          Core.projectionField = (Core.Name "fun")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "targs"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

data_ApplyType :: (Phantoms.TTerm Meta.Data -> Phantoms.TTerm Meta.Data_Name -> Phantoms.TTerm [Meta.Type] -> Phantoms.TTerm [Meta.Data] -> Phantoms.TTerm Meta.Data_ApplyType)
data_ApplyType lhs op targs args = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Data_ApplyType"),
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
      Core.fieldTerm = (Phantoms.unTTerm args)}]})))

data_ApplyTypeLhs :: (Phantoms.TTerm Meta.Data_ApplyType -> Phantoms.TTerm Meta.Data)
data_ApplyTypeLhs x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Data_ApplyType"),
    Core.projectionField = (Core.Name "lhs")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

data_ApplyTypeOp :: (Phantoms.TTerm Meta.Data_ApplyType -> Phantoms.TTerm Meta.Data_Name)
data_ApplyTypeOp x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Data_ApplyType"),
    Core.projectionField = (Core.Name "op")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

data_ApplyTypeTargs :: (Phantoms.TTerm Meta.Data_ApplyType -> Phantoms.TTerm [Meta.Type])
data_ApplyTypeTargs x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Data_ApplyType"),
    Core.projectionField = (Core.Name "targs")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

data_ApplyTypeArgs :: (Phantoms.TTerm Meta.Data_ApplyType -> Phantoms.TTerm [Meta.Data])
data_ApplyTypeArgs x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Data_ApplyType"),
    Core.projectionField = (Core.Name "args")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

data_ApplyTypeWithLhs :: (Phantoms.TTerm Meta.Data_ApplyType -> Phantoms.TTerm Meta.Data -> Phantoms.TTerm Meta.Data_ApplyType)
data_ApplyTypeWithLhs original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Data_ApplyType"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "lhs"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "op"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Data_ApplyType"),
          Core.projectionField = (Core.Name "op")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "targs"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Data_ApplyType"),
          Core.projectionField = (Core.Name "targs")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "args"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Data_ApplyType"),
          Core.projectionField = (Core.Name "args")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

data_ApplyTypeWithOp :: (Phantoms.TTerm Meta.Data_ApplyType -> Phantoms.TTerm Meta.Data_Name -> Phantoms.TTerm Meta.Data_ApplyType)
data_ApplyTypeWithOp original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Data_ApplyType"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "lhs"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Data_ApplyType"),
          Core.projectionField = (Core.Name "lhs")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "op"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "targs"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Data_ApplyType"),
          Core.projectionField = (Core.Name "targs")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "args"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Data_ApplyType"),
          Core.projectionField = (Core.Name "args")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

data_ApplyTypeWithTargs :: (Phantoms.TTerm Meta.Data_ApplyType -> Phantoms.TTerm [Meta.Type] -> Phantoms.TTerm Meta.Data_ApplyType)
data_ApplyTypeWithTargs original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Data_ApplyType"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "lhs"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Data_ApplyType"),
          Core.projectionField = (Core.Name "lhs")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "op"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Data_ApplyType"),
          Core.projectionField = (Core.Name "op")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "targs"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "args"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Data_ApplyType"),
          Core.projectionField = (Core.Name "args")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

data_ApplyTypeWithArgs :: (Phantoms.TTerm Meta.Data_ApplyType -> Phantoms.TTerm [Meta.Data] -> Phantoms.TTerm Meta.Data_ApplyType)
data_ApplyTypeWithArgs original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Data_ApplyType"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "lhs"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Data_ApplyType"),
          Core.projectionField = (Core.Name "lhs")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "op"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Data_ApplyType"),
          Core.projectionField = (Core.Name "op")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "targs"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Data_ApplyType"),
          Core.projectionField = (Core.Name "targs")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "args"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

data_ApplyInfix :: (Phantoms.TTerm Meta.Data -> Phantoms.TTerm Meta.Data_Name -> Phantoms.TTerm [Meta.Type] -> Phantoms.TTerm [Meta.Data] -> Phantoms.TTerm Meta.Data_ApplyInfix)
data_ApplyInfix lhs op targs args = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Data_ApplyInfix"),
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
      Core.fieldTerm = (Phantoms.unTTerm args)}]})))

data_ApplyInfixLhs :: (Phantoms.TTerm Meta.Data_ApplyInfix -> Phantoms.TTerm Meta.Data)
data_ApplyInfixLhs x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Data_ApplyInfix"),
    Core.projectionField = (Core.Name "lhs")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

data_ApplyInfixOp :: (Phantoms.TTerm Meta.Data_ApplyInfix -> Phantoms.TTerm Meta.Data_Name)
data_ApplyInfixOp x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Data_ApplyInfix"),
    Core.projectionField = (Core.Name "op")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

data_ApplyInfixTargs :: (Phantoms.TTerm Meta.Data_ApplyInfix -> Phantoms.TTerm [Meta.Type])
data_ApplyInfixTargs x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Data_ApplyInfix"),
    Core.projectionField = (Core.Name "targs")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

data_ApplyInfixArgs :: (Phantoms.TTerm Meta.Data_ApplyInfix -> Phantoms.TTerm [Meta.Data])
data_ApplyInfixArgs x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Data_ApplyInfix"),
    Core.projectionField = (Core.Name "args")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

data_ApplyInfixWithLhs :: (Phantoms.TTerm Meta.Data_ApplyInfix -> Phantoms.TTerm Meta.Data -> Phantoms.TTerm Meta.Data_ApplyInfix)
data_ApplyInfixWithLhs original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Data_ApplyInfix"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "lhs"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "op"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Data_ApplyInfix"),
          Core.projectionField = (Core.Name "op")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "targs"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Data_ApplyInfix"),
          Core.projectionField = (Core.Name "targs")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "args"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Data_ApplyInfix"),
          Core.projectionField = (Core.Name "args")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

data_ApplyInfixWithOp :: (Phantoms.TTerm Meta.Data_ApplyInfix -> Phantoms.TTerm Meta.Data_Name -> Phantoms.TTerm Meta.Data_ApplyInfix)
data_ApplyInfixWithOp original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Data_ApplyInfix"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "lhs"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Data_ApplyInfix"),
          Core.projectionField = (Core.Name "lhs")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "op"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "targs"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Data_ApplyInfix"),
          Core.projectionField = (Core.Name "targs")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "args"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Data_ApplyInfix"),
          Core.projectionField = (Core.Name "args")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

data_ApplyInfixWithTargs :: (Phantoms.TTerm Meta.Data_ApplyInfix -> Phantoms.TTerm [Meta.Type] -> Phantoms.TTerm Meta.Data_ApplyInfix)
data_ApplyInfixWithTargs original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Data_ApplyInfix"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "lhs"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Data_ApplyInfix"),
          Core.projectionField = (Core.Name "lhs")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "op"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Data_ApplyInfix"),
          Core.projectionField = (Core.Name "op")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "targs"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "args"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Data_ApplyInfix"),
          Core.projectionField = (Core.Name "args")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

data_ApplyInfixWithArgs :: (Phantoms.TTerm Meta.Data_ApplyInfix -> Phantoms.TTerm [Meta.Data] -> Phantoms.TTerm Meta.Data_ApplyInfix)
data_ApplyInfixWithArgs original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Data_ApplyInfix"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "lhs"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Data_ApplyInfix"),
          Core.projectionField = (Core.Name "lhs")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "op"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Data_ApplyInfix"),
          Core.projectionField = (Core.Name "op")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "targs"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Data_ApplyInfix"),
          Core.projectionField = (Core.Name "targs")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "args"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

data_ApplyUnary :: (Phantoms.TTerm Meta.Data_Name -> Phantoms.TTerm Meta.Data -> Phantoms.TTerm Meta.Data_ApplyUnary)
data_ApplyUnary op arg = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Data_ApplyUnary"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "op"),
      Core.fieldTerm = (Phantoms.unTTerm op)},
    Core.Field {
      Core.fieldName = (Core.Name "arg"),
      Core.fieldTerm = (Phantoms.unTTerm arg)}]})))

data_ApplyUnaryOp :: (Phantoms.TTerm Meta.Data_ApplyUnary -> Phantoms.TTerm Meta.Data_Name)
data_ApplyUnaryOp x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Data_ApplyUnary"),
    Core.projectionField = (Core.Name "op")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

data_ApplyUnaryArg :: (Phantoms.TTerm Meta.Data_ApplyUnary -> Phantoms.TTerm Meta.Data)
data_ApplyUnaryArg x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Data_ApplyUnary"),
    Core.projectionField = (Core.Name "arg")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

data_ApplyUnaryWithOp :: (Phantoms.TTerm Meta.Data_ApplyUnary -> Phantoms.TTerm Meta.Data_Name -> Phantoms.TTerm Meta.Data_ApplyUnary)
data_ApplyUnaryWithOp original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Data_ApplyUnary"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "op"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "arg"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Data_ApplyUnary"),
          Core.projectionField = (Core.Name "arg")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

data_ApplyUnaryWithArg :: (Phantoms.TTerm Meta.Data_ApplyUnary -> Phantoms.TTerm Meta.Data -> Phantoms.TTerm Meta.Data_ApplyUnary)
data_ApplyUnaryWithArg original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Data_ApplyUnary"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "op"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Data_ApplyUnary"),
          Core.projectionField = (Core.Name "op")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "arg"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

data_Assign :: (Phantoms.TTerm Meta.Data -> Phantoms.TTerm Meta.Data -> Phantoms.TTerm Meta.Data_Assign)
data_Assign lhs rhs = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Data_Assign"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "lhs"),
      Core.fieldTerm = (Phantoms.unTTerm lhs)},
    Core.Field {
      Core.fieldName = (Core.Name "rhs"),
      Core.fieldTerm = (Phantoms.unTTerm rhs)}]})))

data_AssignLhs :: (Phantoms.TTerm Meta.Data_Assign -> Phantoms.TTerm Meta.Data)
data_AssignLhs x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Data_Assign"),
    Core.projectionField = (Core.Name "lhs")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

data_AssignRhs :: (Phantoms.TTerm Meta.Data_Assign -> Phantoms.TTerm Meta.Data)
data_AssignRhs x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Data_Assign"),
    Core.projectionField = (Core.Name "rhs")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

data_AssignWithLhs :: (Phantoms.TTerm Meta.Data_Assign -> Phantoms.TTerm Meta.Data -> Phantoms.TTerm Meta.Data_Assign)
data_AssignWithLhs original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Data_Assign"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "lhs"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "rhs"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Data_Assign"),
          Core.projectionField = (Core.Name "rhs")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

data_AssignWithRhs :: (Phantoms.TTerm Meta.Data_Assign -> Phantoms.TTerm Meta.Data -> Phantoms.TTerm Meta.Data_Assign)
data_AssignWithRhs original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Data_Assign"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "lhs"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Data_Assign"),
          Core.projectionField = (Core.Name "lhs")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "rhs"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

data_Return :: (Phantoms.TTerm Meta.Data -> Phantoms.TTerm Meta.Data_Return)
data_Return expr = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Data_Return"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "expr"),
      Core.fieldTerm = (Phantoms.unTTerm expr)}]})))

data_ReturnExpr :: (Phantoms.TTerm Meta.Data_Return -> Phantoms.TTerm Meta.Data)
data_ReturnExpr x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Data_Return"),
    Core.projectionField = (Core.Name "expr")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

data_ReturnWithExpr :: (Phantoms.TTerm Meta.Data_Return -> Phantoms.TTerm Meta.Data -> Phantoms.TTerm Meta.Data_Return)
data_ReturnWithExpr original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Data_Return"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "expr"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

data_Throw :: (Phantoms.TTerm Meta.Data -> Phantoms.TTerm Meta.Data_Throw)
data_Throw expr = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Data_Throw"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "expr"),
      Core.fieldTerm = (Phantoms.unTTerm expr)}]})))

data_ThrowExpr :: (Phantoms.TTerm Meta.Data_Throw -> Phantoms.TTerm Meta.Data)
data_ThrowExpr x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Data_Throw"),
    Core.projectionField = (Core.Name "expr")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

data_ThrowWithExpr :: (Phantoms.TTerm Meta.Data_Throw -> Phantoms.TTerm Meta.Data -> Phantoms.TTerm Meta.Data_Throw)
data_ThrowWithExpr original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Data_Throw"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "expr"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

data_Ascribe :: (Phantoms.TTerm Meta.Data -> Phantoms.TTerm Meta.Type -> Phantoms.TTerm Meta.Data_Ascribe)
data_Ascribe expr tpe = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Data_Ascribe"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "expr"),
      Core.fieldTerm = (Phantoms.unTTerm expr)},
    Core.Field {
      Core.fieldName = (Core.Name "tpe"),
      Core.fieldTerm = (Phantoms.unTTerm tpe)}]})))

data_AscribeExpr :: (Phantoms.TTerm Meta.Data_Ascribe -> Phantoms.TTerm Meta.Data)
data_AscribeExpr x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Data_Ascribe"),
    Core.projectionField = (Core.Name "expr")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

data_AscribeTpe :: (Phantoms.TTerm Meta.Data_Ascribe -> Phantoms.TTerm Meta.Type)
data_AscribeTpe x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Data_Ascribe"),
    Core.projectionField = (Core.Name "tpe")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

data_AscribeWithExpr :: (Phantoms.TTerm Meta.Data_Ascribe -> Phantoms.TTerm Meta.Data -> Phantoms.TTerm Meta.Data_Ascribe)
data_AscribeWithExpr original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Data_Ascribe"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "expr"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "tpe"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Data_Ascribe"),
          Core.projectionField = (Core.Name "tpe")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

data_AscribeWithTpe :: (Phantoms.TTerm Meta.Data_Ascribe -> Phantoms.TTerm Meta.Type -> Phantoms.TTerm Meta.Data_Ascribe)
data_AscribeWithTpe original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Data_Ascribe"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "expr"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Data_Ascribe"),
          Core.projectionField = (Core.Name "expr")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "tpe"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

data_Annotate :: (Phantoms.TTerm Meta.Data -> Phantoms.TTerm [Meta.Mod_Annot] -> Phantoms.TTerm Meta.Data_Annotate)
data_Annotate expr annots = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Data_Annotate"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "expr"),
      Core.fieldTerm = (Phantoms.unTTerm expr)},
    Core.Field {
      Core.fieldName = (Core.Name "annots"),
      Core.fieldTerm = (Phantoms.unTTerm annots)}]})))

data_AnnotateExpr :: (Phantoms.TTerm Meta.Data_Annotate -> Phantoms.TTerm Meta.Data)
data_AnnotateExpr x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Data_Annotate"),
    Core.projectionField = (Core.Name "expr")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

data_AnnotateAnnots :: (Phantoms.TTerm Meta.Data_Annotate -> Phantoms.TTerm [Meta.Mod_Annot])
data_AnnotateAnnots x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Data_Annotate"),
    Core.projectionField = (Core.Name "annots")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

data_AnnotateWithExpr :: (Phantoms.TTerm Meta.Data_Annotate -> Phantoms.TTerm Meta.Data -> Phantoms.TTerm Meta.Data_Annotate)
data_AnnotateWithExpr original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Data_Annotate"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "expr"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "annots"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Data_Annotate"),
          Core.projectionField = (Core.Name "annots")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

data_AnnotateWithAnnots :: (Phantoms.TTerm Meta.Data_Annotate -> Phantoms.TTerm [Meta.Mod_Annot] -> Phantoms.TTerm Meta.Data_Annotate)
data_AnnotateWithAnnots original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Data_Annotate"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "expr"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Data_Annotate"),
          Core.projectionField = (Core.Name "expr")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "annots"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

data_Tuple :: (Phantoms.TTerm [Meta.Data] -> Phantoms.TTerm Meta.Data_Tuple)
data_Tuple args = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Data_Tuple"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "args"),
      Core.fieldTerm = (Phantoms.unTTerm args)}]})))

data_TupleArgs :: (Phantoms.TTerm Meta.Data_Tuple -> Phantoms.TTerm [Meta.Data])
data_TupleArgs x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Data_Tuple"),
    Core.projectionField = (Core.Name "args")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

data_TupleWithArgs :: (Phantoms.TTerm Meta.Data_Tuple -> Phantoms.TTerm [Meta.Data] -> Phantoms.TTerm Meta.Data_Tuple)
data_TupleWithArgs original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Data_Tuple"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "args"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

data_Block :: (Phantoms.TTerm [Meta.Stat] -> Phantoms.TTerm Meta.Data_Block)
data_Block stats = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Data_Block"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "stats"),
      Core.fieldTerm = (Phantoms.unTTerm stats)}]})))

data_BlockStats :: (Phantoms.TTerm Meta.Data_Block -> Phantoms.TTerm [Meta.Stat])
data_BlockStats x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Data_Block"),
    Core.projectionField = (Core.Name "stats")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

data_BlockWithStats :: (Phantoms.TTerm Meta.Data_Block -> Phantoms.TTerm [Meta.Stat] -> Phantoms.TTerm Meta.Data_Block)
data_BlockWithStats original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Data_Block"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "stats"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

data_EndMarker :: (Phantoms.TTerm Meta.Data_Name -> Phantoms.TTerm Meta.Data_EndMarker)
data_EndMarker name = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Data_EndMarker"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Phantoms.unTTerm name)}]})))

data_EndMarkerName :: (Phantoms.TTerm Meta.Data_EndMarker -> Phantoms.TTerm Meta.Data_Name)
data_EndMarkerName x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Data_EndMarker"),
    Core.projectionField = (Core.Name "name")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

data_EndMarkerWithName :: (Phantoms.TTerm Meta.Data_EndMarker -> Phantoms.TTerm Meta.Data_Name -> Phantoms.TTerm Meta.Data_EndMarker)
data_EndMarkerWithName original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Data_EndMarker"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

data_If :: (Phantoms.TTerm Meta.Data -> Phantoms.TTerm Meta.Data -> Phantoms.TTerm Meta.Data -> Phantoms.TTerm Meta.Data_If)
data_If cond thenp elsep = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Data_If"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "cond"),
      Core.fieldTerm = (Phantoms.unTTerm cond)},
    Core.Field {
      Core.fieldName = (Core.Name "thenp"),
      Core.fieldTerm = (Phantoms.unTTerm thenp)},
    Core.Field {
      Core.fieldName = (Core.Name "elsep"),
      Core.fieldTerm = (Phantoms.unTTerm elsep)}]})))

data_IfCond :: (Phantoms.TTerm Meta.Data_If -> Phantoms.TTerm Meta.Data)
data_IfCond x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Data_If"),
    Core.projectionField = (Core.Name "cond")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

data_IfThenp :: (Phantoms.TTerm Meta.Data_If -> Phantoms.TTerm Meta.Data)
data_IfThenp x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Data_If"),
    Core.projectionField = (Core.Name "thenp")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

data_IfElsep :: (Phantoms.TTerm Meta.Data_If -> Phantoms.TTerm Meta.Data)
data_IfElsep x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Data_If"),
    Core.projectionField = (Core.Name "elsep")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

data_IfWithCond :: (Phantoms.TTerm Meta.Data_If -> Phantoms.TTerm Meta.Data -> Phantoms.TTerm Meta.Data_If)
data_IfWithCond original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Data_If"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "cond"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "thenp"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Data_If"),
          Core.projectionField = (Core.Name "thenp")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "elsep"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Data_If"),
          Core.projectionField = (Core.Name "elsep")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

data_IfWithThenp :: (Phantoms.TTerm Meta.Data_If -> Phantoms.TTerm Meta.Data -> Phantoms.TTerm Meta.Data_If)
data_IfWithThenp original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Data_If"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "cond"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Data_If"),
          Core.projectionField = (Core.Name "cond")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "thenp"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "elsep"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Data_If"),
          Core.projectionField = (Core.Name "elsep")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

data_IfWithElsep :: (Phantoms.TTerm Meta.Data_If -> Phantoms.TTerm Meta.Data -> Phantoms.TTerm Meta.Data_If)
data_IfWithElsep original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Data_If"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "cond"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Data_If"),
          Core.projectionField = (Core.Name "cond")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "thenp"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Data_If"),
          Core.projectionField = (Core.Name "thenp")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "elsep"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

data_QuotedMacroExpr :: (Phantoms.TTerm Meta.Data -> Phantoms.TTerm Meta.Data_QuotedMacroExpr)
data_QuotedMacroExpr body = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Data_QuotedMacroExpr"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "body"),
      Core.fieldTerm = (Phantoms.unTTerm body)}]})))

data_QuotedMacroExprBody :: (Phantoms.TTerm Meta.Data_QuotedMacroExpr -> Phantoms.TTerm Meta.Data)
data_QuotedMacroExprBody x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Data_QuotedMacroExpr"),
    Core.projectionField = (Core.Name "body")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

data_QuotedMacroExprWithBody :: (Phantoms.TTerm Meta.Data_QuotedMacroExpr -> Phantoms.TTerm Meta.Data -> Phantoms.TTerm Meta.Data_QuotedMacroExpr)
data_QuotedMacroExprWithBody original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Data_QuotedMacroExpr"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "body"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

data_QuotedMacroType :: (Phantoms.TTerm Meta.Type -> Phantoms.TTerm Meta.Data_QuotedMacroType)
data_QuotedMacroType tpe = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Data_QuotedMacroType"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "tpe"),
      Core.fieldTerm = (Phantoms.unTTerm tpe)}]})))

data_QuotedMacroTypeTpe :: (Phantoms.TTerm Meta.Data_QuotedMacroType -> Phantoms.TTerm Meta.Type)
data_QuotedMacroTypeTpe x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Data_QuotedMacroType"),
    Core.projectionField = (Core.Name "tpe")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

data_QuotedMacroTypeWithTpe :: (Phantoms.TTerm Meta.Data_QuotedMacroType -> Phantoms.TTerm Meta.Type -> Phantoms.TTerm Meta.Data_QuotedMacroType)
data_QuotedMacroTypeWithTpe original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Data_QuotedMacroType"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "tpe"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

data_SplicedMacroExpr :: (Phantoms.TTerm Meta.Data -> Phantoms.TTerm Meta.Data_SplicedMacroExpr)
data_SplicedMacroExpr body = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Data_SplicedMacroExpr"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "body"),
      Core.fieldTerm = (Phantoms.unTTerm body)}]})))

data_SplicedMacroExprBody :: (Phantoms.TTerm Meta.Data_SplicedMacroExpr -> Phantoms.TTerm Meta.Data)
data_SplicedMacroExprBody x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Data_SplicedMacroExpr"),
    Core.projectionField = (Core.Name "body")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

data_SplicedMacroExprWithBody :: (Phantoms.TTerm Meta.Data_SplicedMacroExpr -> Phantoms.TTerm Meta.Data -> Phantoms.TTerm Meta.Data_SplicedMacroExpr)
data_SplicedMacroExprWithBody original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Data_SplicedMacroExpr"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "body"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

data_Match :: (Phantoms.TTerm Meta.Data -> Phantoms.TTerm [Meta.Case] -> Phantoms.TTerm Meta.Data_Match)
data_Match expr cases = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Data_Match"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "expr"),
      Core.fieldTerm = (Phantoms.unTTerm expr)},
    Core.Field {
      Core.fieldName = (Core.Name "cases"),
      Core.fieldTerm = (Phantoms.unTTerm cases)}]})))

data_MatchExpr :: (Phantoms.TTerm Meta.Data_Match -> Phantoms.TTerm Meta.Data)
data_MatchExpr x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Data_Match"),
    Core.projectionField = (Core.Name "expr")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

data_MatchCases :: (Phantoms.TTerm Meta.Data_Match -> Phantoms.TTerm [Meta.Case])
data_MatchCases x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Data_Match"),
    Core.projectionField = (Core.Name "cases")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

data_MatchWithExpr :: (Phantoms.TTerm Meta.Data_Match -> Phantoms.TTerm Meta.Data -> Phantoms.TTerm Meta.Data_Match)
data_MatchWithExpr original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Data_Match"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "expr"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "cases"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Data_Match"),
          Core.projectionField = (Core.Name "cases")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

data_MatchWithCases :: (Phantoms.TTerm Meta.Data_Match -> Phantoms.TTerm [Meta.Case] -> Phantoms.TTerm Meta.Data_Match)
data_MatchWithCases original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Data_Match"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "expr"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Data_Match"),
          Core.projectionField = (Core.Name "expr")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "cases"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

data_Try :: (Phantoms.TTerm Meta.Data -> Phantoms.TTerm [Meta.Case] -> Phantoms.TTerm (Maybe Meta.Data) -> Phantoms.TTerm Meta.Data_Try)
data_Try expr catchp finallyp = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Data_Try"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "expr"),
      Core.fieldTerm = (Phantoms.unTTerm expr)},
    Core.Field {
      Core.fieldName = (Core.Name "catchp"),
      Core.fieldTerm = (Phantoms.unTTerm catchp)},
    Core.Field {
      Core.fieldName = (Core.Name "finallyp"),
      Core.fieldTerm = (Phantoms.unTTerm finallyp)}]})))

data_TryExpr :: (Phantoms.TTerm Meta.Data_Try -> Phantoms.TTerm Meta.Data)
data_TryExpr x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Data_Try"),
    Core.projectionField = (Core.Name "expr")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

data_TryCatchp :: (Phantoms.TTerm Meta.Data_Try -> Phantoms.TTerm [Meta.Case])
data_TryCatchp x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Data_Try"),
    Core.projectionField = (Core.Name "catchp")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

data_TryFinallyp :: (Phantoms.TTerm Meta.Data_Try -> Phantoms.TTerm (Maybe Meta.Data))
data_TryFinallyp x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Data_Try"),
    Core.projectionField = (Core.Name "finallyp")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

data_TryWithExpr :: (Phantoms.TTerm Meta.Data_Try -> Phantoms.TTerm Meta.Data -> Phantoms.TTerm Meta.Data_Try)
data_TryWithExpr original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Data_Try"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "expr"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "catchp"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Data_Try"),
          Core.projectionField = (Core.Name "catchp")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "finallyp"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Data_Try"),
          Core.projectionField = (Core.Name "finallyp")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

data_TryWithCatchp :: (Phantoms.TTerm Meta.Data_Try -> Phantoms.TTerm [Meta.Case] -> Phantoms.TTerm Meta.Data_Try)
data_TryWithCatchp original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Data_Try"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "expr"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Data_Try"),
          Core.projectionField = (Core.Name "expr")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "catchp"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "finallyp"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Data_Try"),
          Core.projectionField = (Core.Name "finallyp")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

data_TryWithFinallyp :: (Phantoms.TTerm Meta.Data_Try -> Phantoms.TTerm (Maybe Meta.Data) -> Phantoms.TTerm Meta.Data_Try)
data_TryWithFinallyp original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Data_Try"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "expr"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Data_Try"),
          Core.projectionField = (Core.Name "expr")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "catchp"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Data_Try"),
          Core.projectionField = (Core.Name "catchp")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "finallyp"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

data_TryWithHandler :: (Phantoms.TTerm Meta.Data -> Phantoms.TTerm Meta.Data -> Phantoms.TTerm (Maybe Meta.Data) -> Phantoms.TTerm Meta.Data_TryWithHandler)
data_TryWithHandler expr catchp finallyp = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Data_TryWithHandler"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "expr"),
      Core.fieldTerm = (Phantoms.unTTerm expr)},
    Core.Field {
      Core.fieldName = (Core.Name "catchp"),
      Core.fieldTerm = (Phantoms.unTTerm catchp)},
    Core.Field {
      Core.fieldName = (Core.Name "finallyp"),
      Core.fieldTerm = (Phantoms.unTTerm finallyp)}]})))

data_TryWithHandlerExpr :: (Phantoms.TTerm Meta.Data_TryWithHandler -> Phantoms.TTerm Meta.Data)
data_TryWithHandlerExpr x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Data_TryWithHandler"),
    Core.projectionField = (Core.Name "expr")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

data_TryWithHandlerCatchp :: (Phantoms.TTerm Meta.Data_TryWithHandler -> Phantoms.TTerm Meta.Data)
data_TryWithHandlerCatchp x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Data_TryWithHandler"),
    Core.projectionField = (Core.Name "catchp")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

data_TryWithHandlerFinallyp :: (Phantoms.TTerm Meta.Data_TryWithHandler -> Phantoms.TTerm (Maybe Meta.Data))
data_TryWithHandlerFinallyp x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Data_TryWithHandler"),
    Core.projectionField = (Core.Name "finallyp")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

data_TryWithHandlerWithExpr :: (Phantoms.TTerm Meta.Data_TryWithHandler -> Phantoms.TTerm Meta.Data -> Phantoms.TTerm Meta.Data_TryWithHandler)
data_TryWithHandlerWithExpr original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Data_TryWithHandler"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "expr"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "catchp"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Data_TryWithHandler"),
          Core.projectionField = (Core.Name "catchp")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "finallyp"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Data_TryWithHandler"),
          Core.projectionField = (Core.Name "finallyp")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

data_TryWithHandlerWithCatchp :: (Phantoms.TTerm Meta.Data_TryWithHandler -> Phantoms.TTerm Meta.Data -> Phantoms.TTerm Meta.Data_TryWithHandler)
data_TryWithHandlerWithCatchp original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Data_TryWithHandler"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "expr"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Data_TryWithHandler"),
          Core.projectionField = (Core.Name "expr")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "catchp"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "finallyp"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Data_TryWithHandler"),
          Core.projectionField = (Core.Name "finallyp")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

data_TryWithHandlerWithFinallyp :: (Phantoms.TTerm Meta.Data_TryWithHandler -> Phantoms.TTerm (Maybe Meta.Data) -> Phantoms.TTerm Meta.Data_TryWithHandler)
data_TryWithHandlerWithFinallyp original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Data_TryWithHandler"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "expr"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Data_TryWithHandler"),
          Core.projectionField = (Core.Name "expr")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "catchp"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Data_TryWithHandler"),
          Core.projectionField = (Core.Name "catchp")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "finallyp"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

data_FunctionDataContextFunction :: (Phantoms.TTerm Meta.Data_ContextFunction -> Phantoms.TTerm Meta.Data_FunctionData)
data_FunctionDataContextFunction x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Data_FunctionData"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "contextFunction"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

data_FunctionDataFunction :: (Phantoms.TTerm Meta.Data_Function -> Phantoms.TTerm Meta.Data_FunctionData)
data_FunctionDataFunction x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Data_FunctionData"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "function"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

data_ContextFunction :: (Phantoms.TTerm [Meta.Data_Param] -> Phantoms.TTerm Meta.Data -> Phantoms.TTerm Meta.Data_ContextFunction)
data_ContextFunction params body = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Data_ContextFunction"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "params"),
      Core.fieldTerm = (Phantoms.unTTerm params)},
    Core.Field {
      Core.fieldName = (Core.Name "body"),
      Core.fieldTerm = (Phantoms.unTTerm body)}]})))

data_ContextFunctionParams :: (Phantoms.TTerm Meta.Data_ContextFunction -> Phantoms.TTerm [Meta.Data_Param])
data_ContextFunctionParams x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Data_ContextFunction"),
    Core.projectionField = (Core.Name "params")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

data_ContextFunctionBody :: (Phantoms.TTerm Meta.Data_ContextFunction -> Phantoms.TTerm Meta.Data)
data_ContextFunctionBody x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Data_ContextFunction"),
    Core.projectionField = (Core.Name "body")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

data_ContextFunctionWithParams :: (Phantoms.TTerm Meta.Data_ContextFunction -> Phantoms.TTerm [Meta.Data_Param] -> Phantoms.TTerm Meta.Data_ContextFunction)
data_ContextFunctionWithParams original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Data_ContextFunction"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "params"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "body"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Data_ContextFunction"),
          Core.projectionField = (Core.Name "body")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

data_ContextFunctionWithBody :: (Phantoms.TTerm Meta.Data_ContextFunction -> Phantoms.TTerm Meta.Data -> Phantoms.TTerm Meta.Data_ContextFunction)
data_ContextFunctionWithBody original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Data_ContextFunction"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "params"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Data_ContextFunction"),
          Core.projectionField = (Core.Name "params")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "body"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

data_Function :: (Phantoms.TTerm [Meta.Data_Param] -> Phantoms.TTerm Meta.Data -> Phantoms.TTerm Meta.Data_Function)
data_Function params body = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Data_Function"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "params"),
      Core.fieldTerm = (Phantoms.unTTerm params)},
    Core.Field {
      Core.fieldName = (Core.Name "body"),
      Core.fieldTerm = (Phantoms.unTTerm body)}]})))

data_FunctionParams :: (Phantoms.TTerm Meta.Data_Function -> Phantoms.TTerm [Meta.Data_Param])
data_FunctionParams x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Data_Function"),
    Core.projectionField = (Core.Name "params")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

data_FunctionBody :: (Phantoms.TTerm Meta.Data_Function -> Phantoms.TTerm Meta.Data)
data_FunctionBody x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Data_Function"),
    Core.projectionField = (Core.Name "body")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

data_FunctionWithParams :: (Phantoms.TTerm Meta.Data_Function -> Phantoms.TTerm [Meta.Data_Param] -> Phantoms.TTerm Meta.Data_Function)
data_FunctionWithParams original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Data_Function"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "params"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "body"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Data_Function"),
          Core.projectionField = (Core.Name "body")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

data_FunctionWithBody :: (Phantoms.TTerm Meta.Data_Function -> Phantoms.TTerm Meta.Data -> Phantoms.TTerm Meta.Data_Function)
data_FunctionWithBody original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Data_Function"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "params"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Data_Function"),
          Core.projectionField = (Core.Name "params")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "body"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

data_PolyFunction :: (Phantoms.TTerm [Meta.Type_Param] -> Phantoms.TTerm Meta.Data -> Phantoms.TTerm Meta.Data_PolyFunction)
data_PolyFunction tparams body = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Data_PolyFunction"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "tparams"),
      Core.fieldTerm = (Phantoms.unTTerm tparams)},
    Core.Field {
      Core.fieldName = (Core.Name "body"),
      Core.fieldTerm = (Phantoms.unTTerm body)}]})))

data_PolyFunctionTparams :: (Phantoms.TTerm Meta.Data_PolyFunction -> Phantoms.TTerm [Meta.Type_Param])
data_PolyFunctionTparams x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Data_PolyFunction"),
    Core.projectionField = (Core.Name "tparams")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

data_PolyFunctionBody :: (Phantoms.TTerm Meta.Data_PolyFunction -> Phantoms.TTerm Meta.Data)
data_PolyFunctionBody x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Data_PolyFunction"),
    Core.projectionField = (Core.Name "body")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

data_PolyFunctionWithTparams :: (Phantoms.TTerm Meta.Data_PolyFunction -> Phantoms.TTerm [Meta.Type_Param] -> Phantoms.TTerm Meta.Data_PolyFunction)
data_PolyFunctionWithTparams original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Data_PolyFunction"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "tparams"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "body"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Data_PolyFunction"),
          Core.projectionField = (Core.Name "body")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

data_PolyFunctionWithBody :: (Phantoms.TTerm Meta.Data_PolyFunction -> Phantoms.TTerm Meta.Data -> Phantoms.TTerm Meta.Data_PolyFunction)
data_PolyFunctionWithBody original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Data_PolyFunction"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "tparams"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Data_PolyFunction"),
          Core.projectionField = (Core.Name "tparams")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "body"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

data_PartialFunction :: (Phantoms.TTerm [Meta.Case] -> Phantoms.TTerm Meta.Data_PartialFunction)
data_PartialFunction cases = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Data_PartialFunction"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "cases"),
      Core.fieldTerm = (Phantoms.unTTerm cases)}]})))

data_PartialFunctionCases :: (Phantoms.TTerm Meta.Data_PartialFunction -> Phantoms.TTerm [Meta.Case])
data_PartialFunctionCases x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Data_PartialFunction"),
    Core.projectionField = (Core.Name "cases")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

data_PartialFunctionWithCases :: (Phantoms.TTerm Meta.Data_PartialFunction -> Phantoms.TTerm [Meta.Case] -> Phantoms.TTerm Meta.Data_PartialFunction)
data_PartialFunctionWithCases original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Data_PartialFunction"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "cases"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

data_While :: (Phantoms.TTerm Meta.Data -> Phantoms.TTerm Meta.Data -> Phantoms.TTerm Meta.Data_While)
data_While expr body = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Data_While"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "expr"),
      Core.fieldTerm = (Phantoms.unTTerm expr)},
    Core.Field {
      Core.fieldName = (Core.Name "body"),
      Core.fieldTerm = (Phantoms.unTTerm body)}]})))

data_WhileExpr :: (Phantoms.TTerm Meta.Data_While -> Phantoms.TTerm Meta.Data)
data_WhileExpr x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Data_While"),
    Core.projectionField = (Core.Name "expr")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

data_WhileBody :: (Phantoms.TTerm Meta.Data_While -> Phantoms.TTerm Meta.Data)
data_WhileBody x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Data_While"),
    Core.projectionField = (Core.Name "body")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

data_WhileWithExpr :: (Phantoms.TTerm Meta.Data_While -> Phantoms.TTerm Meta.Data -> Phantoms.TTerm Meta.Data_While)
data_WhileWithExpr original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Data_While"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "expr"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "body"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Data_While"),
          Core.projectionField = (Core.Name "body")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

data_WhileWithBody :: (Phantoms.TTerm Meta.Data_While -> Phantoms.TTerm Meta.Data -> Phantoms.TTerm Meta.Data_While)
data_WhileWithBody original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Data_While"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "expr"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Data_While"),
          Core.projectionField = (Core.Name "expr")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "body"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

data_Do :: (Phantoms.TTerm Meta.Data -> Phantoms.TTerm Meta.Data -> Phantoms.TTerm Meta.Data_Do)
data_Do body expr = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Data_Do"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "body"),
      Core.fieldTerm = (Phantoms.unTTerm body)},
    Core.Field {
      Core.fieldName = (Core.Name "expr"),
      Core.fieldTerm = (Phantoms.unTTerm expr)}]})))

data_DoBody :: (Phantoms.TTerm Meta.Data_Do -> Phantoms.TTerm Meta.Data)
data_DoBody x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Data_Do"),
    Core.projectionField = (Core.Name "body")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

data_DoExpr :: (Phantoms.TTerm Meta.Data_Do -> Phantoms.TTerm Meta.Data)
data_DoExpr x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Data_Do"),
    Core.projectionField = (Core.Name "expr")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

data_DoWithBody :: (Phantoms.TTerm Meta.Data_Do -> Phantoms.TTerm Meta.Data -> Phantoms.TTerm Meta.Data_Do)
data_DoWithBody original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Data_Do"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "body"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "expr"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Data_Do"),
          Core.projectionField = (Core.Name "expr")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

data_DoWithExpr :: (Phantoms.TTerm Meta.Data_Do -> Phantoms.TTerm Meta.Data -> Phantoms.TTerm Meta.Data_Do)
data_DoWithExpr original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Data_Do"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "body"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Data_Do"),
          Core.projectionField = (Core.Name "body")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "expr"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

data_For :: (Phantoms.TTerm [Meta.Enumerator] -> Phantoms.TTerm Meta.Data_For)
data_For enums = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Data_For"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "enums"),
      Core.fieldTerm = (Phantoms.unTTerm enums)}]})))

data_ForEnums :: (Phantoms.TTerm Meta.Data_For -> Phantoms.TTerm [Meta.Enumerator])
data_ForEnums x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Data_For"),
    Core.projectionField = (Core.Name "enums")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

data_ForWithEnums :: (Phantoms.TTerm Meta.Data_For -> Phantoms.TTerm [Meta.Enumerator] -> Phantoms.TTerm Meta.Data_For)
data_ForWithEnums original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Data_For"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "enums"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

data_ForYield :: (Phantoms.TTerm [Meta.Enumerator] -> Phantoms.TTerm Meta.Data_ForYield)
data_ForYield enums = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Data_ForYield"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "enums"),
      Core.fieldTerm = (Phantoms.unTTerm enums)}]})))

data_ForYieldEnums :: (Phantoms.TTerm Meta.Data_ForYield -> Phantoms.TTerm [Meta.Enumerator])
data_ForYieldEnums x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Data_ForYield"),
    Core.projectionField = (Core.Name "enums")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

data_ForYieldWithEnums :: (Phantoms.TTerm Meta.Data_ForYield -> Phantoms.TTerm [Meta.Enumerator] -> Phantoms.TTerm Meta.Data_ForYield)
data_ForYieldWithEnums original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Data_ForYield"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "enums"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

data_New :: (Phantoms.TTerm Meta.Init -> Phantoms.TTerm Meta.Data_New)
data_New init = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Data_New"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "init"),
      Core.fieldTerm = (Phantoms.unTTerm init)}]})))

data_NewInit :: (Phantoms.TTerm Meta.Data_New -> Phantoms.TTerm Meta.Init)
data_NewInit x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Data_New"),
    Core.projectionField = (Core.Name "init")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

data_NewWithInit :: (Phantoms.TTerm Meta.Data_New -> Phantoms.TTerm Meta.Init -> Phantoms.TTerm Meta.Data_New)
data_NewWithInit original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Data_New"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "init"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

data_NewAnonymous :: (Phantoms.TTerm Meta.Template -> Phantoms.TTerm Meta.Data_NewAnonymous)
data_NewAnonymous templ = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Data_NewAnonymous"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "templ"),
      Core.fieldTerm = (Phantoms.unTTerm templ)}]})))

data_NewAnonymousTempl :: (Phantoms.TTerm Meta.Data_NewAnonymous -> Phantoms.TTerm Meta.Template)
data_NewAnonymousTempl x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Data_NewAnonymous"),
    Core.projectionField = (Core.Name "templ")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

data_NewAnonymousWithTempl :: (Phantoms.TTerm Meta.Data_NewAnonymous -> Phantoms.TTerm Meta.Template -> Phantoms.TTerm Meta.Data_NewAnonymous)
data_NewAnonymousWithTempl original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Data_NewAnonymous"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "templ"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

data_Eta :: (Phantoms.TTerm Meta.Data -> Phantoms.TTerm Meta.Data_Eta)
data_Eta expr = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Data_Eta"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "expr"),
      Core.fieldTerm = (Phantoms.unTTerm expr)}]})))

data_EtaExpr :: (Phantoms.TTerm Meta.Data_Eta -> Phantoms.TTerm Meta.Data)
data_EtaExpr x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Data_Eta"),
    Core.projectionField = (Core.Name "expr")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

data_EtaWithExpr :: (Phantoms.TTerm Meta.Data_Eta -> Phantoms.TTerm Meta.Data -> Phantoms.TTerm Meta.Data_Eta)
data_EtaWithExpr original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Data_Eta"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "expr"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

data_Repeated :: (Phantoms.TTerm Meta.Data -> Phantoms.TTerm Meta.Data_Repeated)
data_Repeated expr = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Data_Repeated"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "expr"),
      Core.fieldTerm = (Phantoms.unTTerm expr)}]})))

data_RepeatedExpr :: (Phantoms.TTerm Meta.Data_Repeated -> Phantoms.TTerm Meta.Data)
data_RepeatedExpr x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Data_Repeated"),
    Core.projectionField = (Core.Name "expr")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

data_RepeatedWithExpr :: (Phantoms.TTerm Meta.Data_Repeated -> Phantoms.TTerm Meta.Data -> Phantoms.TTerm Meta.Data_Repeated)
data_RepeatedWithExpr original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Data_Repeated"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "expr"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

data_Param :: (Phantoms.TTerm [Meta.Mod] -> Phantoms.TTerm Meta.Name -> Phantoms.TTerm (Maybe Meta.Type) -> Phantoms.TTerm (Maybe Meta.Data) -> Phantoms.TTerm Meta.Data_Param)
data_Param mods name decltpe default_ = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Data_Param"),
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
      Core.fieldTerm = (Phantoms.unTTerm default_)}]})))

data_ParamMods :: (Phantoms.TTerm Meta.Data_Param -> Phantoms.TTerm [Meta.Mod])
data_ParamMods x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Data_Param"),
    Core.projectionField = (Core.Name "mods")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

data_ParamName :: (Phantoms.TTerm Meta.Data_Param -> Phantoms.TTerm Meta.Name)
data_ParamName x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Data_Param"),
    Core.projectionField = (Core.Name "name")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

data_ParamDecltpe :: (Phantoms.TTerm Meta.Data_Param -> Phantoms.TTerm (Maybe Meta.Type))
data_ParamDecltpe x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Data_Param"),
    Core.projectionField = (Core.Name "decltpe")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

data_ParamDefault :: (Phantoms.TTerm Meta.Data_Param -> Phantoms.TTerm (Maybe Meta.Data))
data_ParamDefault x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Data_Param"),
    Core.projectionField = (Core.Name "default")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

data_ParamWithMods :: (Phantoms.TTerm Meta.Data_Param -> Phantoms.TTerm [Meta.Mod] -> Phantoms.TTerm Meta.Data_Param)
data_ParamWithMods original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Data_Param"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "mods"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Data_Param"),
          Core.projectionField = (Core.Name "name")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "decltpe"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Data_Param"),
          Core.projectionField = (Core.Name "decltpe")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "default"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Data_Param"),
          Core.projectionField = (Core.Name "default")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

data_ParamWithName :: (Phantoms.TTerm Meta.Data_Param -> Phantoms.TTerm Meta.Name -> Phantoms.TTerm Meta.Data_Param)
data_ParamWithName original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Data_Param"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "mods"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Data_Param"),
          Core.projectionField = (Core.Name "mods")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "decltpe"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Data_Param"),
          Core.projectionField = (Core.Name "decltpe")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "default"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Data_Param"),
          Core.projectionField = (Core.Name "default")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

data_ParamWithDecltpe :: (Phantoms.TTerm Meta.Data_Param -> Phantoms.TTerm (Maybe Meta.Type) -> Phantoms.TTerm Meta.Data_Param)
data_ParamWithDecltpe original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Data_Param"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "mods"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Data_Param"),
          Core.projectionField = (Core.Name "mods")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Data_Param"),
          Core.projectionField = (Core.Name "name")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "decltpe"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "default"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Data_Param"),
          Core.projectionField = (Core.Name "default")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

data_ParamWithDefault :: (Phantoms.TTerm Meta.Data_Param -> Phantoms.TTerm (Maybe Meta.Data) -> Phantoms.TTerm Meta.Data_Param)
data_ParamWithDefault original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Data_Param"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "mods"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Data_Param"),
          Core.projectionField = (Core.Name "mods")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Data_Param"),
          Core.projectionField = (Core.Name "name")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "decltpe"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Data_Param"),
          Core.projectionField = (Core.Name "decltpe")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "default"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

typeRef :: (Phantoms.TTerm Meta.Type_Ref -> Phantoms.TTerm Meta.Type)
typeRef x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Type"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "ref"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

typeAnonymousName :: (Phantoms.TTerm Meta.Type_AnonymousName -> Phantoms.TTerm Meta.Type)
typeAnonymousName x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Type"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "anonymousName"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

typeApply :: (Phantoms.TTerm Meta.Type_Apply -> Phantoms.TTerm Meta.Type)
typeApply x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Type"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "apply"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

typeApplyInfix :: (Phantoms.TTerm Meta.Type_ApplyInfix -> Phantoms.TTerm Meta.Type)
typeApplyInfix x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Type"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "applyInfix"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

typeFunctionType :: (Phantoms.TTerm Meta.Type_FunctionType -> Phantoms.TTerm Meta.Type)
typeFunctionType x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Type"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "functionType"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

typePolyFunction :: (Phantoms.TTerm Meta.Type_PolyFunction -> Phantoms.TTerm Meta.Type)
typePolyFunction x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Type"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "polyFunction"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

typeImplicitFunction :: (Phantoms.TTerm Meta.Type_ImplicitFunction -> Phantoms.TTerm Meta.Type)
typeImplicitFunction x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Type"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "implicitFunction"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

typeTuple :: (Phantoms.TTerm Meta.Type_Tuple -> Phantoms.TTerm Meta.Type)
typeTuple x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Type"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "tuple"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

typeWith :: (Phantoms.TTerm Meta.Type_With -> Phantoms.TTerm Meta.Type)
typeWith x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Type"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "with"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

typeAnd :: (Phantoms.TTerm Meta.Type_And -> Phantoms.TTerm Meta.Type)
typeAnd x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Type"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "and"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

typeOr :: (Phantoms.TTerm Meta.Type_Or -> Phantoms.TTerm Meta.Type)
typeOr x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Type"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "or"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

typeRefine :: (Phantoms.TTerm Meta.Type_Refine -> Phantoms.TTerm Meta.Type)
typeRefine x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Type"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "refine"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

typeExistential :: (Phantoms.TTerm Meta.Type_Existential -> Phantoms.TTerm Meta.Type)
typeExistential x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Type"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "existential"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

typeAnnotate :: (Phantoms.TTerm Meta.Type_Annotate -> Phantoms.TTerm Meta.Type)
typeAnnotate x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Type"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "annotate"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

typeLambda :: (Phantoms.TTerm Meta.Type_Lambda -> Phantoms.TTerm Meta.Type)
typeLambda x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Type"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "lambda"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

typeMacro :: (Phantoms.TTerm Meta.Type_Macro -> Phantoms.TTerm Meta.Type)
typeMacro x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Type"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "macro"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

typeMethod :: (Phantoms.TTerm Meta.Type_Method -> Phantoms.TTerm Meta.Type)
typeMethod x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Type"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "method"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

typePlaceholder :: (Phantoms.TTerm Meta.Type_Placeholder -> Phantoms.TTerm Meta.Type)
typePlaceholder x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Type"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "placeholder"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

typeByName :: (Phantoms.TTerm Meta.Type_ByName -> Phantoms.TTerm Meta.Type)
typeByName x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Type"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "byName"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

typeRepeated :: (Phantoms.TTerm Meta.Type_Repeated -> Phantoms.TTerm Meta.Type)
typeRepeated x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Type"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "repeated"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

typeVar :: (Phantoms.TTerm Meta.Type_Var -> Phantoms.TTerm Meta.Type)
typeVar x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Type"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "var"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

typeTypedParam :: (Phantoms.TTerm Meta.Type_TypedParam -> Phantoms.TTerm Meta.Type)
typeTypedParam x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Type"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "typedParam"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

typeMatch :: (Phantoms.TTerm Meta.Type_Match -> Phantoms.TTerm Meta.Type)
typeMatch x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Type"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "match"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

type_RefName :: (Phantoms.TTerm Meta.Type_Name -> Phantoms.TTerm Meta.Type_Ref)
type_RefName x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Type_Ref"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "name"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

type_RefSelect :: (Phantoms.TTerm Meta.Type_Select -> Phantoms.TTerm Meta.Type_Ref)
type_RefSelect x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Type_Ref"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "select"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

type_RefProject :: (Phantoms.TTerm Meta.Type_Project -> Phantoms.TTerm Meta.Type_Ref)
type_RefProject x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Type_Ref"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "project"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

type_RefSingleton :: (Phantoms.TTerm Meta.Type_Singleton -> Phantoms.TTerm Meta.Type_Ref)
type_RefSingleton x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Type_Ref"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "singleton"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

type_Name :: (Phantoms.TTerm String -> Phantoms.TTerm Meta.Type_Name)
type_Name value = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Type_Name"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "value"),
      Core.fieldTerm = (Phantoms.unTTerm value)}]})))

type_NameValue :: (Phantoms.TTerm Meta.Type_Name -> Phantoms.TTerm String)
type_NameValue x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Type_Name"),
    Core.projectionField = (Core.Name "value")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

type_NameWithValue :: (Phantoms.TTerm Meta.Type_Name -> Phantoms.TTerm String -> Phantoms.TTerm Meta.Type_Name)
type_NameWithValue original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Type_Name"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "value"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

type_AnonymousName :: (Phantoms.TTerm () -> Phantoms.TTerm Meta.Type_AnonymousName)
type_AnonymousName x = (Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
  Core.wrappedTermTypeName = (Core.Name "hydra.ext.scala.meta.Type_AnonymousName"),
  Core.wrappedTermBody = (Phantoms.unTTerm x)})))

unType_AnonymousName :: (Phantoms.TTerm Meta.Type_AnonymousName -> Phantoms.TTerm ())
unType_AnonymousName x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.scala.meta.Type_AnonymousName")))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

type_Select :: (Phantoms.TTerm Meta.Data_Ref -> Phantoms.TTerm Meta.Type_Name -> Phantoms.TTerm Meta.Type_Select)
type_Select qual name = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Type_Select"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "qual"),
      Core.fieldTerm = (Phantoms.unTTerm qual)},
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Phantoms.unTTerm name)}]})))

type_SelectQual :: (Phantoms.TTerm Meta.Type_Select -> Phantoms.TTerm Meta.Data_Ref)
type_SelectQual x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Type_Select"),
    Core.projectionField = (Core.Name "qual")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

type_SelectName :: (Phantoms.TTerm Meta.Type_Select -> Phantoms.TTerm Meta.Type_Name)
type_SelectName x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Type_Select"),
    Core.projectionField = (Core.Name "name")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

type_SelectWithQual :: (Phantoms.TTerm Meta.Type_Select -> Phantoms.TTerm Meta.Data_Ref -> Phantoms.TTerm Meta.Type_Select)
type_SelectWithQual original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Type_Select"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "qual"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Type_Select"),
          Core.projectionField = (Core.Name "name")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

type_SelectWithName :: (Phantoms.TTerm Meta.Type_Select -> Phantoms.TTerm Meta.Type_Name -> Phantoms.TTerm Meta.Type_Select)
type_SelectWithName original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Type_Select"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "qual"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Type_Select"),
          Core.projectionField = (Core.Name "qual")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

type_Project :: (Phantoms.TTerm Meta.Type -> Phantoms.TTerm Meta.Type_Name -> Phantoms.TTerm Meta.Type_Project)
type_Project qual name = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Type_Project"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "qual"),
      Core.fieldTerm = (Phantoms.unTTerm qual)},
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Phantoms.unTTerm name)}]})))

type_ProjectQual :: (Phantoms.TTerm Meta.Type_Project -> Phantoms.TTerm Meta.Type)
type_ProjectQual x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Type_Project"),
    Core.projectionField = (Core.Name "qual")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

type_ProjectName :: (Phantoms.TTerm Meta.Type_Project -> Phantoms.TTerm Meta.Type_Name)
type_ProjectName x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Type_Project"),
    Core.projectionField = (Core.Name "name")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

type_ProjectWithQual :: (Phantoms.TTerm Meta.Type_Project -> Phantoms.TTerm Meta.Type -> Phantoms.TTerm Meta.Type_Project)
type_ProjectWithQual original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Type_Project"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "qual"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Type_Project"),
          Core.projectionField = (Core.Name "name")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

type_ProjectWithName :: (Phantoms.TTerm Meta.Type_Project -> Phantoms.TTerm Meta.Type_Name -> Phantoms.TTerm Meta.Type_Project)
type_ProjectWithName original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Type_Project"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "qual"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Type_Project"),
          Core.projectionField = (Core.Name "qual")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

type_Singleton :: (Phantoms.TTerm Meta.Data_Ref -> Phantoms.TTerm Meta.Type_Singleton)
type_Singleton ref = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Type_Singleton"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "ref"),
      Core.fieldTerm = (Phantoms.unTTerm ref)}]})))

type_SingletonRef :: (Phantoms.TTerm Meta.Type_Singleton -> Phantoms.TTerm Meta.Data_Ref)
type_SingletonRef x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Type_Singleton"),
    Core.projectionField = (Core.Name "ref")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

type_SingletonWithRef :: (Phantoms.TTerm Meta.Type_Singleton -> Phantoms.TTerm Meta.Data_Ref -> Phantoms.TTerm Meta.Type_Singleton)
type_SingletonWithRef original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Type_Singleton"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "ref"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

type_Apply :: (Phantoms.TTerm Meta.Type -> Phantoms.TTerm [Meta.Type] -> Phantoms.TTerm Meta.Type_Apply)
type_Apply tpe args = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Type_Apply"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "tpe"),
      Core.fieldTerm = (Phantoms.unTTerm tpe)},
    Core.Field {
      Core.fieldName = (Core.Name "args"),
      Core.fieldTerm = (Phantoms.unTTerm args)}]})))

type_ApplyTpe :: (Phantoms.TTerm Meta.Type_Apply -> Phantoms.TTerm Meta.Type)
type_ApplyTpe x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Type_Apply"),
    Core.projectionField = (Core.Name "tpe")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

type_ApplyArgs :: (Phantoms.TTerm Meta.Type_Apply -> Phantoms.TTerm [Meta.Type])
type_ApplyArgs x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Type_Apply"),
    Core.projectionField = (Core.Name "args")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

type_ApplyWithTpe :: (Phantoms.TTerm Meta.Type_Apply -> Phantoms.TTerm Meta.Type -> Phantoms.TTerm Meta.Type_Apply)
type_ApplyWithTpe original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Type_Apply"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "tpe"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "args"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Type_Apply"),
          Core.projectionField = (Core.Name "args")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

type_ApplyWithArgs :: (Phantoms.TTerm Meta.Type_Apply -> Phantoms.TTerm [Meta.Type] -> Phantoms.TTerm Meta.Type_Apply)
type_ApplyWithArgs original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Type_Apply"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "tpe"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Type_Apply"),
          Core.projectionField = (Core.Name "tpe")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "args"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

type_ApplyInfix :: (Phantoms.TTerm Meta.Type -> Phantoms.TTerm Meta.Type_Name -> Phantoms.TTerm Meta.Type -> Phantoms.TTerm Meta.Type_ApplyInfix)
type_ApplyInfix lhs op rhs = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Type_ApplyInfix"),
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

type_ApplyInfixLhs :: (Phantoms.TTerm Meta.Type_ApplyInfix -> Phantoms.TTerm Meta.Type)
type_ApplyInfixLhs x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Type_ApplyInfix"),
    Core.projectionField = (Core.Name "lhs")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

type_ApplyInfixOp :: (Phantoms.TTerm Meta.Type_ApplyInfix -> Phantoms.TTerm Meta.Type_Name)
type_ApplyInfixOp x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Type_ApplyInfix"),
    Core.projectionField = (Core.Name "op")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

type_ApplyInfixRhs :: (Phantoms.TTerm Meta.Type_ApplyInfix -> Phantoms.TTerm Meta.Type)
type_ApplyInfixRhs x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Type_ApplyInfix"),
    Core.projectionField = (Core.Name "rhs")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

type_ApplyInfixWithLhs :: (Phantoms.TTerm Meta.Type_ApplyInfix -> Phantoms.TTerm Meta.Type -> Phantoms.TTerm Meta.Type_ApplyInfix)
type_ApplyInfixWithLhs original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Type_ApplyInfix"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "lhs"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "op"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Type_ApplyInfix"),
          Core.projectionField = (Core.Name "op")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "rhs"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Type_ApplyInfix"),
          Core.projectionField = (Core.Name "rhs")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

type_ApplyInfixWithOp :: (Phantoms.TTerm Meta.Type_ApplyInfix -> Phantoms.TTerm Meta.Type_Name -> Phantoms.TTerm Meta.Type_ApplyInfix)
type_ApplyInfixWithOp original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Type_ApplyInfix"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "lhs"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Type_ApplyInfix"),
          Core.projectionField = (Core.Name "lhs")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "op"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "rhs"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Type_ApplyInfix"),
          Core.projectionField = (Core.Name "rhs")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

type_ApplyInfixWithRhs :: (Phantoms.TTerm Meta.Type_ApplyInfix -> Phantoms.TTerm Meta.Type -> Phantoms.TTerm Meta.Type_ApplyInfix)
type_ApplyInfixWithRhs original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Type_ApplyInfix"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "lhs"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Type_ApplyInfix"),
          Core.projectionField = (Core.Name "lhs")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "op"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Type_ApplyInfix"),
          Core.projectionField = (Core.Name "op")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "rhs"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

type_FunctionTypeFunction :: (Phantoms.TTerm Meta.Type_Function -> Phantoms.TTerm Meta.Type_FunctionType)
type_FunctionTypeFunction x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Type_FunctionType"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "function"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

type_FunctionTypeContextFunction :: (Phantoms.TTerm Meta.Type_ContextFunction -> Phantoms.TTerm Meta.Type_FunctionType)
type_FunctionTypeContextFunction x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Type_FunctionType"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "contextFunction"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

type_Function :: (Phantoms.TTerm [Meta.Type] -> Phantoms.TTerm Meta.Type -> Phantoms.TTerm Meta.Type_Function)
type_Function params res = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Type_Function"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "params"),
      Core.fieldTerm = (Phantoms.unTTerm params)},
    Core.Field {
      Core.fieldName = (Core.Name "res"),
      Core.fieldTerm = (Phantoms.unTTerm res)}]})))

type_FunctionParams :: (Phantoms.TTerm Meta.Type_Function -> Phantoms.TTerm [Meta.Type])
type_FunctionParams x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Type_Function"),
    Core.projectionField = (Core.Name "params")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

type_FunctionRes :: (Phantoms.TTerm Meta.Type_Function -> Phantoms.TTerm Meta.Type)
type_FunctionRes x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Type_Function"),
    Core.projectionField = (Core.Name "res")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

type_FunctionWithParams :: (Phantoms.TTerm Meta.Type_Function -> Phantoms.TTerm [Meta.Type] -> Phantoms.TTerm Meta.Type_Function)
type_FunctionWithParams original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Type_Function"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "params"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "res"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Type_Function"),
          Core.projectionField = (Core.Name "res")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

type_FunctionWithRes :: (Phantoms.TTerm Meta.Type_Function -> Phantoms.TTerm Meta.Type -> Phantoms.TTerm Meta.Type_Function)
type_FunctionWithRes original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Type_Function"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "params"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Type_Function"),
          Core.projectionField = (Core.Name "params")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "res"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

type_PolyFunction :: (Phantoms.TTerm [Meta.Type_Param] -> Phantoms.TTerm Meta.Type -> Phantoms.TTerm Meta.Type_PolyFunction)
type_PolyFunction tparams tpe = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Type_PolyFunction"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "tparams"),
      Core.fieldTerm = (Phantoms.unTTerm tparams)},
    Core.Field {
      Core.fieldName = (Core.Name "tpe"),
      Core.fieldTerm = (Phantoms.unTTerm tpe)}]})))

type_PolyFunctionTparams :: (Phantoms.TTerm Meta.Type_PolyFunction -> Phantoms.TTerm [Meta.Type_Param])
type_PolyFunctionTparams x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Type_PolyFunction"),
    Core.projectionField = (Core.Name "tparams")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

type_PolyFunctionTpe :: (Phantoms.TTerm Meta.Type_PolyFunction -> Phantoms.TTerm Meta.Type)
type_PolyFunctionTpe x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Type_PolyFunction"),
    Core.projectionField = (Core.Name "tpe")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

type_PolyFunctionWithTparams :: (Phantoms.TTerm Meta.Type_PolyFunction -> Phantoms.TTerm [Meta.Type_Param] -> Phantoms.TTerm Meta.Type_PolyFunction)
type_PolyFunctionWithTparams original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Type_PolyFunction"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "tparams"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "tpe"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Type_PolyFunction"),
          Core.projectionField = (Core.Name "tpe")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

type_PolyFunctionWithTpe :: (Phantoms.TTerm Meta.Type_PolyFunction -> Phantoms.TTerm Meta.Type -> Phantoms.TTerm Meta.Type_PolyFunction)
type_PolyFunctionWithTpe original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Type_PolyFunction"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "tparams"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Type_PolyFunction"),
          Core.projectionField = (Core.Name "tparams")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "tpe"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

type_ContextFunction :: (Phantoms.TTerm [Meta.Type] -> Phantoms.TTerm Meta.Type -> Phantoms.TTerm Meta.Type_ContextFunction)
type_ContextFunction params res = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Type_ContextFunction"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "params"),
      Core.fieldTerm = (Phantoms.unTTerm params)},
    Core.Field {
      Core.fieldName = (Core.Name "res"),
      Core.fieldTerm = (Phantoms.unTTerm res)}]})))

type_ContextFunctionParams :: (Phantoms.TTerm Meta.Type_ContextFunction -> Phantoms.TTerm [Meta.Type])
type_ContextFunctionParams x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Type_ContextFunction"),
    Core.projectionField = (Core.Name "params")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

type_ContextFunctionRes :: (Phantoms.TTerm Meta.Type_ContextFunction -> Phantoms.TTerm Meta.Type)
type_ContextFunctionRes x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Type_ContextFunction"),
    Core.projectionField = (Core.Name "res")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

type_ContextFunctionWithParams :: (Phantoms.TTerm Meta.Type_ContextFunction -> Phantoms.TTerm [Meta.Type] -> Phantoms.TTerm Meta.Type_ContextFunction)
type_ContextFunctionWithParams original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Type_ContextFunction"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "params"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "res"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Type_ContextFunction"),
          Core.projectionField = (Core.Name "res")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

type_ContextFunctionWithRes :: (Phantoms.TTerm Meta.Type_ContextFunction -> Phantoms.TTerm Meta.Type -> Phantoms.TTerm Meta.Type_ContextFunction)
type_ContextFunctionWithRes original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Type_ContextFunction"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "params"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Type_ContextFunction"),
          Core.projectionField = (Core.Name "params")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "res"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

type_ImplicitFunction :: (Phantoms.TTerm [Meta.Type] -> Phantoms.TTerm Meta.Type -> Phantoms.TTerm Meta.Type_ImplicitFunction)
type_ImplicitFunction params res = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Type_ImplicitFunction"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "params"),
      Core.fieldTerm = (Phantoms.unTTerm params)},
    Core.Field {
      Core.fieldName = (Core.Name "res"),
      Core.fieldTerm = (Phantoms.unTTerm res)}]})))

type_ImplicitFunctionParams :: (Phantoms.TTerm Meta.Type_ImplicitFunction -> Phantoms.TTerm [Meta.Type])
type_ImplicitFunctionParams x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Type_ImplicitFunction"),
    Core.projectionField = (Core.Name "params")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

type_ImplicitFunctionRes :: (Phantoms.TTerm Meta.Type_ImplicitFunction -> Phantoms.TTerm Meta.Type)
type_ImplicitFunctionRes x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Type_ImplicitFunction"),
    Core.projectionField = (Core.Name "res")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

type_ImplicitFunctionWithParams :: (Phantoms.TTerm Meta.Type_ImplicitFunction -> Phantoms.TTerm [Meta.Type] -> Phantoms.TTerm Meta.Type_ImplicitFunction)
type_ImplicitFunctionWithParams original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Type_ImplicitFunction"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "params"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "res"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Type_ImplicitFunction"),
          Core.projectionField = (Core.Name "res")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

type_ImplicitFunctionWithRes :: (Phantoms.TTerm Meta.Type_ImplicitFunction -> Phantoms.TTerm Meta.Type -> Phantoms.TTerm Meta.Type_ImplicitFunction)
type_ImplicitFunctionWithRes original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Type_ImplicitFunction"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "params"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Type_ImplicitFunction"),
          Core.projectionField = (Core.Name "params")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "res"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

type_Tuple :: (Phantoms.TTerm [Meta.Type] -> Phantoms.TTerm Meta.Type_Tuple)
type_Tuple args = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Type_Tuple"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "args"),
      Core.fieldTerm = (Phantoms.unTTerm args)}]})))

type_TupleArgs :: (Phantoms.TTerm Meta.Type_Tuple -> Phantoms.TTerm [Meta.Type])
type_TupleArgs x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Type_Tuple"),
    Core.projectionField = (Core.Name "args")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

type_TupleWithArgs :: (Phantoms.TTerm Meta.Type_Tuple -> Phantoms.TTerm [Meta.Type] -> Phantoms.TTerm Meta.Type_Tuple)
type_TupleWithArgs original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Type_Tuple"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "args"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

type_With :: (Phantoms.TTerm Meta.Type -> Phantoms.TTerm Meta.Type -> Phantoms.TTerm Meta.Type_With)
type_With lhs rhs = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Type_With"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "lhs"),
      Core.fieldTerm = (Phantoms.unTTerm lhs)},
    Core.Field {
      Core.fieldName = (Core.Name "rhs"),
      Core.fieldTerm = (Phantoms.unTTerm rhs)}]})))

type_WithLhs :: (Phantoms.TTerm Meta.Type_With -> Phantoms.TTerm Meta.Type)
type_WithLhs x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Type_With"),
    Core.projectionField = (Core.Name "lhs")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

type_WithRhs :: (Phantoms.TTerm Meta.Type_With -> Phantoms.TTerm Meta.Type)
type_WithRhs x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Type_With"),
    Core.projectionField = (Core.Name "rhs")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

type_WithWithLhs :: (Phantoms.TTerm Meta.Type_With -> Phantoms.TTerm Meta.Type -> Phantoms.TTerm Meta.Type_With)
type_WithWithLhs original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Type_With"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "lhs"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "rhs"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Type_With"),
          Core.projectionField = (Core.Name "rhs")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

type_WithWithRhs :: (Phantoms.TTerm Meta.Type_With -> Phantoms.TTerm Meta.Type -> Phantoms.TTerm Meta.Type_With)
type_WithWithRhs original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Type_With"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "lhs"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Type_With"),
          Core.projectionField = (Core.Name "lhs")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "rhs"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

type_And :: (Phantoms.TTerm Meta.Type -> Phantoms.TTerm Meta.Type -> Phantoms.TTerm Meta.Type_And)
type_And lhs rhs = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Type_And"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "lhs"),
      Core.fieldTerm = (Phantoms.unTTerm lhs)},
    Core.Field {
      Core.fieldName = (Core.Name "rhs"),
      Core.fieldTerm = (Phantoms.unTTerm rhs)}]})))

type_AndLhs :: (Phantoms.TTerm Meta.Type_And -> Phantoms.TTerm Meta.Type)
type_AndLhs x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Type_And"),
    Core.projectionField = (Core.Name "lhs")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

type_AndRhs :: (Phantoms.TTerm Meta.Type_And -> Phantoms.TTerm Meta.Type)
type_AndRhs x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Type_And"),
    Core.projectionField = (Core.Name "rhs")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

type_AndWithLhs :: (Phantoms.TTerm Meta.Type_And -> Phantoms.TTerm Meta.Type -> Phantoms.TTerm Meta.Type_And)
type_AndWithLhs original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Type_And"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "lhs"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "rhs"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Type_And"),
          Core.projectionField = (Core.Name "rhs")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

type_AndWithRhs :: (Phantoms.TTerm Meta.Type_And -> Phantoms.TTerm Meta.Type -> Phantoms.TTerm Meta.Type_And)
type_AndWithRhs original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Type_And"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "lhs"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Type_And"),
          Core.projectionField = (Core.Name "lhs")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "rhs"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

type_Or :: (Phantoms.TTerm Meta.Type -> Phantoms.TTerm Meta.Type -> Phantoms.TTerm Meta.Type_Or)
type_Or lhs rhs = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Type_Or"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "lhs"),
      Core.fieldTerm = (Phantoms.unTTerm lhs)},
    Core.Field {
      Core.fieldName = (Core.Name "rhs"),
      Core.fieldTerm = (Phantoms.unTTerm rhs)}]})))

type_OrLhs :: (Phantoms.TTerm Meta.Type_Or -> Phantoms.TTerm Meta.Type)
type_OrLhs x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Type_Or"),
    Core.projectionField = (Core.Name "lhs")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

type_OrRhs :: (Phantoms.TTerm Meta.Type_Or -> Phantoms.TTerm Meta.Type)
type_OrRhs x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Type_Or"),
    Core.projectionField = (Core.Name "rhs")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

type_OrWithLhs :: (Phantoms.TTerm Meta.Type_Or -> Phantoms.TTerm Meta.Type -> Phantoms.TTerm Meta.Type_Or)
type_OrWithLhs original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Type_Or"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "lhs"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "rhs"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Type_Or"),
          Core.projectionField = (Core.Name "rhs")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

type_OrWithRhs :: (Phantoms.TTerm Meta.Type_Or -> Phantoms.TTerm Meta.Type -> Phantoms.TTerm Meta.Type_Or)
type_OrWithRhs original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Type_Or"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "lhs"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Type_Or"),
          Core.projectionField = (Core.Name "lhs")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "rhs"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

type_Refine :: (Phantoms.TTerm (Maybe Meta.Type) -> Phantoms.TTerm [Meta.Stat] -> Phantoms.TTerm Meta.Type_Refine)
type_Refine tpe stats = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Type_Refine"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "tpe"),
      Core.fieldTerm = (Phantoms.unTTerm tpe)},
    Core.Field {
      Core.fieldName = (Core.Name "stats"),
      Core.fieldTerm = (Phantoms.unTTerm stats)}]})))

type_RefineTpe :: (Phantoms.TTerm Meta.Type_Refine -> Phantoms.TTerm (Maybe Meta.Type))
type_RefineTpe x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Type_Refine"),
    Core.projectionField = (Core.Name "tpe")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

type_RefineStats :: (Phantoms.TTerm Meta.Type_Refine -> Phantoms.TTerm [Meta.Stat])
type_RefineStats x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Type_Refine"),
    Core.projectionField = (Core.Name "stats")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

type_RefineWithTpe :: (Phantoms.TTerm Meta.Type_Refine -> Phantoms.TTerm (Maybe Meta.Type) -> Phantoms.TTerm Meta.Type_Refine)
type_RefineWithTpe original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Type_Refine"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "tpe"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "stats"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Type_Refine"),
          Core.projectionField = (Core.Name "stats")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

type_RefineWithStats :: (Phantoms.TTerm Meta.Type_Refine -> Phantoms.TTerm [Meta.Stat] -> Phantoms.TTerm Meta.Type_Refine)
type_RefineWithStats original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Type_Refine"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "tpe"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Type_Refine"),
          Core.projectionField = (Core.Name "tpe")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "stats"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

type_Existential :: (Phantoms.TTerm Meta.Type -> Phantoms.TTerm [Meta.Stat] -> Phantoms.TTerm Meta.Type_Existential)
type_Existential tpe stats = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Type_Existential"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "tpe"),
      Core.fieldTerm = (Phantoms.unTTerm tpe)},
    Core.Field {
      Core.fieldName = (Core.Name "stats"),
      Core.fieldTerm = (Phantoms.unTTerm stats)}]})))

type_ExistentialTpe :: (Phantoms.TTerm Meta.Type_Existential -> Phantoms.TTerm Meta.Type)
type_ExistentialTpe x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Type_Existential"),
    Core.projectionField = (Core.Name "tpe")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

type_ExistentialStats :: (Phantoms.TTerm Meta.Type_Existential -> Phantoms.TTerm [Meta.Stat])
type_ExistentialStats x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Type_Existential"),
    Core.projectionField = (Core.Name "stats")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

type_ExistentialWithTpe :: (Phantoms.TTerm Meta.Type_Existential -> Phantoms.TTerm Meta.Type -> Phantoms.TTerm Meta.Type_Existential)
type_ExistentialWithTpe original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Type_Existential"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "tpe"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "stats"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Type_Existential"),
          Core.projectionField = (Core.Name "stats")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

type_ExistentialWithStats :: (Phantoms.TTerm Meta.Type_Existential -> Phantoms.TTerm [Meta.Stat] -> Phantoms.TTerm Meta.Type_Existential)
type_ExistentialWithStats original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Type_Existential"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "tpe"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Type_Existential"),
          Core.projectionField = (Core.Name "tpe")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "stats"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

type_Annotate :: (Phantoms.TTerm Meta.Type -> Phantoms.TTerm [Meta.Mod_Annot] -> Phantoms.TTerm Meta.Type_Annotate)
type_Annotate tpe annots = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Type_Annotate"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "tpe"),
      Core.fieldTerm = (Phantoms.unTTerm tpe)},
    Core.Field {
      Core.fieldName = (Core.Name "annots"),
      Core.fieldTerm = (Phantoms.unTTerm annots)}]})))

type_AnnotateTpe :: (Phantoms.TTerm Meta.Type_Annotate -> Phantoms.TTerm Meta.Type)
type_AnnotateTpe x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Type_Annotate"),
    Core.projectionField = (Core.Name "tpe")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

type_AnnotateAnnots :: (Phantoms.TTerm Meta.Type_Annotate -> Phantoms.TTerm [Meta.Mod_Annot])
type_AnnotateAnnots x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Type_Annotate"),
    Core.projectionField = (Core.Name "annots")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

type_AnnotateWithTpe :: (Phantoms.TTerm Meta.Type_Annotate -> Phantoms.TTerm Meta.Type -> Phantoms.TTerm Meta.Type_Annotate)
type_AnnotateWithTpe original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Type_Annotate"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "tpe"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "annots"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Type_Annotate"),
          Core.projectionField = (Core.Name "annots")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

type_AnnotateWithAnnots :: (Phantoms.TTerm Meta.Type_Annotate -> Phantoms.TTerm [Meta.Mod_Annot] -> Phantoms.TTerm Meta.Type_Annotate)
type_AnnotateWithAnnots original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Type_Annotate"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "tpe"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Type_Annotate"),
          Core.projectionField = (Core.Name "tpe")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "annots"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

type_Lambda :: (Phantoms.TTerm [Meta.Type_Param] -> Phantoms.TTerm Meta.Type -> Phantoms.TTerm Meta.Type_Lambda)
type_Lambda tparams tpe = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Type_Lambda"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "tparams"),
      Core.fieldTerm = (Phantoms.unTTerm tparams)},
    Core.Field {
      Core.fieldName = (Core.Name "tpe"),
      Core.fieldTerm = (Phantoms.unTTerm tpe)}]})))

type_LambdaTparams :: (Phantoms.TTerm Meta.Type_Lambda -> Phantoms.TTerm [Meta.Type_Param])
type_LambdaTparams x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Type_Lambda"),
    Core.projectionField = (Core.Name "tparams")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

type_LambdaTpe :: (Phantoms.TTerm Meta.Type_Lambda -> Phantoms.TTerm Meta.Type)
type_LambdaTpe x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Type_Lambda"),
    Core.projectionField = (Core.Name "tpe")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

type_LambdaWithTparams :: (Phantoms.TTerm Meta.Type_Lambda -> Phantoms.TTerm [Meta.Type_Param] -> Phantoms.TTerm Meta.Type_Lambda)
type_LambdaWithTparams original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Type_Lambda"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "tparams"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "tpe"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Type_Lambda"),
          Core.projectionField = (Core.Name "tpe")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

type_LambdaWithTpe :: (Phantoms.TTerm Meta.Type_Lambda -> Phantoms.TTerm Meta.Type -> Phantoms.TTerm Meta.Type_Lambda)
type_LambdaWithTpe original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Type_Lambda"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "tparams"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Type_Lambda"),
          Core.projectionField = (Core.Name "tparams")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "tpe"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

type_Macro :: (Phantoms.TTerm Meta.Data -> Phantoms.TTerm Meta.Type_Macro)
type_Macro body = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Type_Macro"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "body"),
      Core.fieldTerm = (Phantoms.unTTerm body)}]})))

type_MacroBody :: (Phantoms.TTerm Meta.Type_Macro -> Phantoms.TTerm Meta.Data)
type_MacroBody x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Type_Macro"),
    Core.projectionField = (Core.Name "body")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

type_MacroWithBody :: (Phantoms.TTerm Meta.Type_Macro -> Phantoms.TTerm Meta.Data -> Phantoms.TTerm Meta.Type_Macro)
type_MacroWithBody original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Type_Macro"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "body"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

type_Method :: (Phantoms.TTerm [[Meta.Data_Param]] -> Phantoms.TTerm Meta.Type -> Phantoms.TTerm Meta.Type_Method)
type_Method paramss tpe = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Type_Method"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "paramss"),
      Core.fieldTerm = (Phantoms.unTTerm paramss)},
    Core.Field {
      Core.fieldName = (Core.Name "tpe"),
      Core.fieldTerm = (Phantoms.unTTerm tpe)}]})))

type_MethodParamss :: (Phantoms.TTerm Meta.Type_Method -> Phantoms.TTerm [[Meta.Data_Param]])
type_MethodParamss x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Type_Method"),
    Core.projectionField = (Core.Name "paramss")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

type_MethodTpe :: (Phantoms.TTerm Meta.Type_Method -> Phantoms.TTerm Meta.Type)
type_MethodTpe x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Type_Method"),
    Core.projectionField = (Core.Name "tpe")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

type_MethodWithParamss :: (Phantoms.TTerm Meta.Type_Method -> Phantoms.TTerm [[Meta.Data_Param]] -> Phantoms.TTerm Meta.Type_Method)
type_MethodWithParamss original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Type_Method"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "paramss"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "tpe"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Type_Method"),
          Core.projectionField = (Core.Name "tpe")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

type_MethodWithTpe :: (Phantoms.TTerm Meta.Type_Method -> Phantoms.TTerm Meta.Type -> Phantoms.TTerm Meta.Type_Method)
type_MethodWithTpe original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Type_Method"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "paramss"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Type_Method"),
          Core.projectionField = (Core.Name "paramss")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "tpe"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

type_Placeholder :: (Phantoms.TTerm Meta.TypeBounds -> Phantoms.TTerm Meta.Type_Placeholder)
type_Placeholder bounds = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Type_Placeholder"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "bounds"),
      Core.fieldTerm = (Phantoms.unTTerm bounds)}]})))

type_PlaceholderBounds :: (Phantoms.TTerm Meta.Type_Placeholder -> Phantoms.TTerm Meta.TypeBounds)
type_PlaceholderBounds x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Type_Placeholder"),
    Core.projectionField = (Core.Name "bounds")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

type_PlaceholderWithBounds :: (Phantoms.TTerm Meta.Type_Placeholder -> Phantoms.TTerm Meta.TypeBounds -> Phantoms.TTerm Meta.Type_Placeholder)
type_PlaceholderWithBounds original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Type_Placeholder"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "bounds"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

typeBounds :: (Phantoms.TTerm (Maybe Meta.Type) -> Phantoms.TTerm (Maybe Meta.Type) -> Phantoms.TTerm Meta.TypeBounds)
typeBounds lo hi = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.TypeBounds"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "lo"),
      Core.fieldTerm = (Phantoms.unTTerm lo)},
    Core.Field {
      Core.fieldName = (Core.Name "hi"),
      Core.fieldTerm = (Phantoms.unTTerm hi)}]})))

typeBoundsLo :: (Phantoms.TTerm Meta.TypeBounds -> Phantoms.TTerm (Maybe Meta.Type))
typeBoundsLo x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.TypeBounds"),
    Core.projectionField = (Core.Name "lo")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

typeBoundsHi :: (Phantoms.TTerm Meta.TypeBounds -> Phantoms.TTerm (Maybe Meta.Type))
typeBoundsHi x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.TypeBounds"),
    Core.projectionField = (Core.Name "hi")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

typeBoundsWithLo :: (Phantoms.TTerm Meta.TypeBounds -> Phantoms.TTerm (Maybe Meta.Type) -> Phantoms.TTerm Meta.TypeBounds)
typeBoundsWithLo original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.TypeBounds"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "lo"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "hi"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.TypeBounds"),
          Core.projectionField = (Core.Name "hi")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

typeBoundsWithHi :: (Phantoms.TTerm Meta.TypeBounds -> Phantoms.TTerm (Maybe Meta.Type) -> Phantoms.TTerm Meta.TypeBounds)
typeBoundsWithHi original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.TypeBounds"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "lo"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.TypeBounds"),
          Core.projectionField = (Core.Name "lo")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "hi"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

type_ByName :: (Phantoms.TTerm Meta.Type -> Phantoms.TTerm Meta.Type_ByName)
type_ByName tpe = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Type_ByName"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "tpe"),
      Core.fieldTerm = (Phantoms.unTTerm tpe)}]})))

type_ByNameTpe :: (Phantoms.TTerm Meta.Type_ByName -> Phantoms.TTerm Meta.Type)
type_ByNameTpe x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Type_ByName"),
    Core.projectionField = (Core.Name "tpe")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

type_ByNameWithTpe :: (Phantoms.TTerm Meta.Type_ByName -> Phantoms.TTerm Meta.Type -> Phantoms.TTerm Meta.Type_ByName)
type_ByNameWithTpe original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Type_ByName"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "tpe"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

type_Repeated :: (Phantoms.TTerm Meta.Type -> Phantoms.TTerm Meta.Type_Repeated)
type_Repeated tpe = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Type_Repeated"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "tpe"),
      Core.fieldTerm = (Phantoms.unTTerm tpe)}]})))

type_RepeatedTpe :: (Phantoms.TTerm Meta.Type_Repeated -> Phantoms.TTerm Meta.Type)
type_RepeatedTpe x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Type_Repeated"),
    Core.projectionField = (Core.Name "tpe")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

type_RepeatedWithTpe :: (Phantoms.TTerm Meta.Type_Repeated -> Phantoms.TTerm Meta.Type -> Phantoms.TTerm Meta.Type_Repeated)
type_RepeatedWithTpe original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Type_Repeated"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "tpe"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

type_Var :: (Phantoms.TTerm Meta.Type_Name -> Phantoms.TTerm Meta.Type_Var)
type_Var name = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Type_Var"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Phantoms.unTTerm name)}]})))

type_VarName :: (Phantoms.TTerm Meta.Type_Var -> Phantoms.TTerm Meta.Type_Name)
type_VarName x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Type_Var"),
    Core.projectionField = (Core.Name "name")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

type_VarWithName :: (Phantoms.TTerm Meta.Type_Var -> Phantoms.TTerm Meta.Type_Name -> Phantoms.TTerm Meta.Type_Var)
type_VarWithName original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Type_Var"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

type_TypedParam :: (Phantoms.TTerm Meta.Name -> Phantoms.TTerm Meta.Type -> Phantoms.TTerm Meta.Type_TypedParam)
type_TypedParam name typ = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Type_TypedParam"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Phantoms.unTTerm name)},
    Core.Field {
      Core.fieldName = (Core.Name "typ"),
      Core.fieldTerm = (Phantoms.unTTerm typ)}]})))

type_TypedParamName :: (Phantoms.TTerm Meta.Type_TypedParam -> Phantoms.TTerm Meta.Name)
type_TypedParamName x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Type_TypedParam"),
    Core.projectionField = (Core.Name "name")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

type_TypedParamTyp :: (Phantoms.TTerm Meta.Type_TypedParam -> Phantoms.TTerm Meta.Type)
type_TypedParamTyp x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Type_TypedParam"),
    Core.projectionField = (Core.Name "typ")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

type_TypedParamWithName :: (Phantoms.TTerm Meta.Type_TypedParam -> Phantoms.TTerm Meta.Name -> Phantoms.TTerm Meta.Type_TypedParam)
type_TypedParamWithName original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Type_TypedParam"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "typ"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Type_TypedParam"),
          Core.projectionField = (Core.Name "typ")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

type_TypedParamWithTyp :: (Phantoms.TTerm Meta.Type_TypedParam -> Phantoms.TTerm Meta.Type -> Phantoms.TTerm Meta.Type_TypedParam)
type_TypedParamWithTyp original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Type_TypedParam"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Type_TypedParam"),
          Core.projectionField = (Core.Name "name")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "typ"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

type_Param :: (Phantoms.TTerm [Meta.Mod] -> Phantoms.TTerm Meta.Name -> Phantoms.TTerm [Meta.Type_Param] -> Phantoms.TTerm [Meta.TypeBounds] -> Phantoms.TTerm [Meta.Type] -> Phantoms.TTerm [Meta.Type] -> Phantoms.TTerm Meta.Type_Param)
type_Param mods name tparams tbounds vbounds cbounds = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Type_Param"),
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
      Core.fieldTerm = (Phantoms.unTTerm cbounds)}]})))

type_ParamMods :: (Phantoms.TTerm Meta.Type_Param -> Phantoms.TTerm [Meta.Mod])
type_ParamMods x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Type_Param"),
    Core.projectionField = (Core.Name "mods")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

type_ParamName :: (Phantoms.TTerm Meta.Type_Param -> Phantoms.TTerm Meta.Name)
type_ParamName x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Type_Param"),
    Core.projectionField = (Core.Name "name")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

type_ParamTparams :: (Phantoms.TTerm Meta.Type_Param -> Phantoms.TTerm [Meta.Type_Param])
type_ParamTparams x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Type_Param"),
    Core.projectionField = (Core.Name "tparams")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

type_ParamTbounds :: (Phantoms.TTerm Meta.Type_Param -> Phantoms.TTerm [Meta.TypeBounds])
type_ParamTbounds x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Type_Param"),
    Core.projectionField = (Core.Name "tbounds")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

type_ParamVbounds :: (Phantoms.TTerm Meta.Type_Param -> Phantoms.TTerm [Meta.Type])
type_ParamVbounds x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Type_Param"),
    Core.projectionField = (Core.Name "vbounds")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

type_ParamCbounds :: (Phantoms.TTerm Meta.Type_Param -> Phantoms.TTerm [Meta.Type])
type_ParamCbounds x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Type_Param"),
    Core.projectionField = (Core.Name "cbounds")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

type_ParamWithMods :: (Phantoms.TTerm Meta.Type_Param -> Phantoms.TTerm [Meta.Mod] -> Phantoms.TTerm Meta.Type_Param)
type_ParamWithMods original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Type_Param"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "mods"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Type_Param"),
          Core.projectionField = (Core.Name "name")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "tparams"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Type_Param"),
          Core.projectionField = (Core.Name "tparams")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "tbounds"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Type_Param"),
          Core.projectionField = (Core.Name "tbounds")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "vbounds"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Type_Param"),
          Core.projectionField = (Core.Name "vbounds")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "cbounds"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Type_Param"),
          Core.projectionField = (Core.Name "cbounds")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

type_ParamWithName :: (Phantoms.TTerm Meta.Type_Param -> Phantoms.TTerm Meta.Name -> Phantoms.TTerm Meta.Type_Param)
type_ParamWithName original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Type_Param"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "mods"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Type_Param"),
          Core.projectionField = (Core.Name "mods")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "tparams"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Type_Param"),
          Core.projectionField = (Core.Name "tparams")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "tbounds"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Type_Param"),
          Core.projectionField = (Core.Name "tbounds")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "vbounds"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Type_Param"),
          Core.projectionField = (Core.Name "vbounds")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "cbounds"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Type_Param"),
          Core.projectionField = (Core.Name "cbounds")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

type_ParamWithTparams :: (Phantoms.TTerm Meta.Type_Param -> Phantoms.TTerm [Meta.Type_Param] -> Phantoms.TTerm Meta.Type_Param)
type_ParamWithTparams original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Type_Param"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "mods"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Type_Param"),
          Core.projectionField = (Core.Name "mods")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Type_Param"),
          Core.projectionField = (Core.Name "name")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "tparams"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "tbounds"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Type_Param"),
          Core.projectionField = (Core.Name "tbounds")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "vbounds"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Type_Param"),
          Core.projectionField = (Core.Name "vbounds")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "cbounds"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Type_Param"),
          Core.projectionField = (Core.Name "cbounds")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

type_ParamWithTbounds :: (Phantoms.TTerm Meta.Type_Param -> Phantoms.TTerm [Meta.TypeBounds] -> Phantoms.TTerm Meta.Type_Param)
type_ParamWithTbounds original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Type_Param"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "mods"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Type_Param"),
          Core.projectionField = (Core.Name "mods")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Type_Param"),
          Core.projectionField = (Core.Name "name")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "tparams"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Type_Param"),
          Core.projectionField = (Core.Name "tparams")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "tbounds"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "vbounds"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Type_Param"),
          Core.projectionField = (Core.Name "vbounds")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "cbounds"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Type_Param"),
          Core.projectionField = (Core.Name "cbounds")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

type_ParamWithVbounds :: (Phantoms.TTerm Meta.Type_Param -> Phantoms.TTerm [Meta.Type] -> Phantoms.TTerm Meta.Type_Param)
type_ParamWithVbounds original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Type_Param"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "mods"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Type_Param"),
          Core.projectionField = (Core.Name "mods")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Type_Param"),
          Core.projectionField = (Core.Name "name")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "tparams"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Type_Param"),
          Core.projectionField = (Core.Name "tparams")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "tbounds"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Type_Param"),
          Core.projectionField = (Core.Name "tbounds")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "vbounds"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "cbounds"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Type_Param"),
          Core.projectionField = (Core.Name "cbounds")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

type_ParamWithCbounds :: (Phantoms.TTerm Meta.Type_Param -> Phantoms.TTerm [Meta.Type] -> Phantoms.TTerm Meta.Type_Param)
type_ParamWithCbounds original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Type_Param"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "mods"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Type_Param"),
          Core.projectionField = (Core.Name "mods")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Type_Param"),
          Core.projectionField = (Core.Name "name")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "tparams"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Type_Param"),
          Core.projectionField = (Core.Name "tparams")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "tbounds"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Type_Param"),
          Core.projectionField = (Core.Name "tbounds")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "vbounds"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Type_Param"),
          Core.projectionField = (Core.Name "vbounds")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "cbounds"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

type_Match :: (Phantoms.TTerm Meta.Type -> Phantoms.TTerm [Meta.TypeCase] -> Phantoms.TTerm Meta.Type_Match)
type_Match tpe cases = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Type_Match"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "tpe"),
      Core.fieldTerm = (Phantoms.unTTerm tpe)},
    Core.Field {
      Core.fieldName = (Core.Name "cases"),
      Core.fieldTerm = (Phantoms.unTTerm cases)}]})))

type_MatchTpe :: (Phantoms.TTerm Meta.Type_Match -> Phantoms.TTerm Meta.Type)
type_MatchTpe x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Type_Match"),
    Core.projectionField = (Core.Name "tpe")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

type_MatchCases :: (Phantoms.TTerm Meta.Type_Match -> Phantoms.TTerm [Meta.TypeCase])
type_MatchCases x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Type_Match"),
    Core.projectionField = (Core.Name "cases")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

type_MatchWithTpe :: (Phantoms.TTerm Meta.Type_Match -> Phantoms.TTerm Meta.Type -> Phantoms.TTerm Meta.Type_Match)
type_MatchWithTpe original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Type_Match"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "tpe"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "cases"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Type_Match"),
          Core.projectionField = (Core.Name "cases")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

type_MatchWithCases :: (Phantoms.TTerm Meta.Type_Match -> Phantoms.TTerm [Meta.TypeCase] -> Phantoms.TTerm Meta.Type_Match)
type_MatchWithCases original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Type_Match"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "tpe"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Type_Match"),
          Core.projectionField = (Core.Name "tpe")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "cases"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

patVar :: (Phantoms.TTerm Meta.Pat_Var -> Phantoms.TTerm Meta.Pat)
patVar x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Pat"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "var"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

patWildcard :: (Phantoms.TTerm Meta.Pat)
patWildcard = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Pat"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "wildcard"),
    Core.fieldTerm = Core.TermUnit}})))

patSeqWildcard :: (Phantoms.TTerm Meta.Pat)
patSeqWildcard = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Pat"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "seqWildcard"),
    Core.fieldTerm = Core.TermUnit}})))

patBind :: (Phantoms.TTerm Meta.Pat_Bind -> Phantoms.TTerm Meta.Pat)
patBind x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Pat"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "bind"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

patAlternative :: (Phantoms.TTerm Meta.Pat_Alternative -> Phantoms.TTerm Meta.Pat)
patAlternative x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Pat"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "alternative"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

patTuple :: (Phantoms.TTerm Meta.Pat_Tuple -> Phantoms.TTerm Meta.Pat)
patTuple x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Pat"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "tuple"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

patRepeated :: (Phantoms.TTerm Meta.Pat_Repeated -> Phantoms.TTerm Meta.Pat)
patRepeated x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Pat"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "repeated"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

patExtract :: (Phantoms.TTerm Meta.Pat_Extract -> Phantoms.TTerm Meta.Pat)
patExtract x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Pat"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "extract"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

patExtractInfix :: (Phantoms.TTerm Meta.Pat_ExtractInfix -> Phantoms.TTerm Meta.Pat)
patExtractInfix x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Pat"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "extractInfix"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

patInterpolate :: (Phantoms.TTerm Meta.Pat_Interpolate -> Phantoms.TTerm Meta.Pat)
patInterpolate x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Pat"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "interpolate"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

patXml :: (Phantoms.TTerm Meta.Pat_Xml -> Phantoms.TTerm Meta.Pat)
patXml x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Pat"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "xml"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

patTyped :: (Phantoms.TTerm Meta.Pat_Typed -> Phantoms.TTerm Meta.Pat)
patTyped x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Pat"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "typed"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

patMacro :: (Phantoms.TTerm Meta.Pat_Macro -> Phantoms.TTerm Meta.Pat)
patMacro x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Pat"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "macro"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

patGiven :: (Phantoms.TTerm Meta.Pat_Given -> Phantoms.TTerm Meta.Pat)
patGiven x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Pat"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "given"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

pat_Var :: (Phantoms.TTerm Meta.Data_Name -> Phantoms.TTerm Meta.Pat_Var)
pat_Var name = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Pat_Var"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Phantoms.unTTerm name)}]})))

pat_VarName :: (Phantoms.TTerm Meta.Pat_Var -> Phantoms.TTerm Meta.Data_Name)
pat_VarName x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Pat_Var"),
    Core.projectionField = (Core.Name "name")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

pat_VarWithName :: (Phantoms.TTerm Meta.Pat_Var -> Phantoms.TTerm Meta.Data_Name -> Phantoms.TTerm Meta.Pat_Var)
pat_VarWithName original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Pat_Var"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

pat_Bind :: (Phantoms.TTerm Meta.Pat -> Phantoms.TTerm Meta.Pat -> Phantoms.TTerm Meta.Pat_Bind)
pat_Bind lhs rhs = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Pat_Bind"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "lhs"),
      Core.fieldTerm = (Phantoms.unTTerm lhs)},
    Core.Field {
      Core.fieldName = (Core.Name "rhs"),
      Core.fieldTerm = (Phantoms.unTTerm rhs)}]})))

pat_BindLhs :: (Phantoms.TTerm Meta.Pat_Bind -> Phantoms.TTerm Meta.Pat)
pat_BindLhs x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Pat_Bind"),
    Core.projectionField = (Core.Name "lhs")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

pat_BindRhs :: (Phantoms.TTerm Meta.Pat_Bind -> Phantoms.TTerm Meta.Pat)
pat_BindRhs x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Pat_Bind"),
    Core.projectionField = (Core.Name "rhs")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

pat_BindWithLhs :: (Phantoms.TTerm Meta.Pat_Bind -> Phantoms.TTerm Meta.Pat -> Phantoms.TTerm Meta.Pat_Bind)
pat_BindWithLhs original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Pat_Bind"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "lhs"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "rhs"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Pat_Bind"),
          Core.projectionField = (Core.Name "rhs")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

pat_BindWithRhs :: (Phantoms.TTerm Meta.Pat_Bind -> Phantoms.TTerm Meta.Pat -> Phantoms.TTerm Meta.Pat_Bind)
pat_BindWithRhs original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Pat_Bind"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "lhs"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Pat_Bind"),
          Core.projectionField = (Core.Name "lhs")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "rhs"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

pat_Alternative :: (Phantoms.TTerm Meta.Pat -> Phantoms.TTerm Meta.Pat -> Phantoms.TTerm Meta.Pat_Alternative)
pat_Alternative lhs rhs = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Pat_Alternative"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "lhs"),
      Core.fieldTerm = (Phantoms.unTTerm lhs)},
    Core.Field {
      Core.fieldName = (Core.Name "rhs"),
      Core.fieldTerm = (Phantoms.unTTerm rhs)}]})))

pat_AlternativeLhs :: (Phantoms.TTerm Meta.Pat_Alternative -> Phantoms.TTerm Meta.Pat)
pat_AlternativeLhs x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Pat_Alternative"),
    Core.projectionField = (Core.Name "lhs")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

pat_AlternativeRhs :: (Phantoms.TTerm Meta.Pat_Alternative -> Phantoms.TTerm Meta.Pat)
pat_AlternativeRhs x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Pat_Alternative"),
    Core.projectionField = (Core.Name "rhs")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

pat_AlternativeWithLhs :: (Phantoms.TTerm Meta.Pat_Alternative -> Phantoms.TTerm Meta.Pat -> Phantoms.TTerm Meta.Pat_Alternative)
pat_AlternativeWithLhs original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Pat_Alternative"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "lhs"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "rhs"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Pat_Alternative"),
          Core.projectionField = (Core.Name "rhs")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

pat_AlternativeWithRhs :: (Phantoms.TTerm Meta.Pat_Alternative -> Phantoms.TTerm Meta.Pat -> Phantoms.TTerm Meta.Pat_Alternative)
pat_AlternativeWithRhs original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Pat_Alternative"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "lhs"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Pat_Alternative"),
          Core.projectionField = (Core.Name "lhs")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "rhs"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

pat_Tuple :: (Phantoms.TTerm [Meta.Pat] -> Phantoms.TTerm Meta.Pat_Tuple)
pat_Tuple args = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Pat_Tuple"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "args"),
      Core.fieldTerm = (Phantoms.unTTerm args)}]})))

pat_TupleArgs :: (Phantoms.TTerm Meta.Pat_Tuple -> Phantoms.TTerm [Meta.Pat])
pat_TupleArgs x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Pat_Tuple"),
    Core.projectionField = (Core.Name "args")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

pat_TupleWithArgs :: (Phantoms.TTerm Meta.Pat_Tuple -> Phantoms.TTerm [Meta.Pat] -> Phantoms.TTerm Meta.Pat_Tuple)
pat_TupleWithArgs original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Pat_Tuple"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "args"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

pat_Repeated :: (Phantoms.TTerm Meta.Data_Name -> Phantoms.TTerm Meta.Pat_Repeated)
pat_Repeated name = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Pat_Repeated"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Phantoms.unTTerm name)}]})))

pat_RepeatedName :: (Phantoms.TTerm Meta.Pat_Repeated -> Phantoms.TTerm Meta.Data_Name)
pat_RepeatedName x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Pat_Repeated"),
    Core.projectionField = (Core.Name "name")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

pat_RepeatedWithName :: (Phantoms.TTerm Meta.Pat_Repeated -> Phantoms.TTerm Meta.Data_Name -> Phantoms.TTerm Meta.Pat_Repeated)
pat_RepeatedWithName original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Pat_Repeated"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

pat_Extract :: (Phantoms.TTerm Meta.Data -> Phantoms.TTerm [Meta.Pat] -> Phantoms.TTerm Meta.Pat_Extract)
pat_Extract fun args = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Pat_Extract"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "fun"),
      Core.fieldTerm = (Phantoms.unTTerm fun)},
    Core.Field {
      Core.fieldName = (Core.Name "args"),
      Core.fieldTerm = (Phantoms.unTTerm args)}]})))

pat_ExtractFun :: (Phantoms.TTerm Meta.Pat_Extract -> Phantoms.TTerm Meta.Data)
pat_ExtractFun x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Pat_Extract"),
    Core.projectionField = (Core.Name "fun")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

pat_ExtractArgs :: (Phantoms.TTerm Meta.Pat_Extract -> Phantoms.TTerm [Meta.Pat])
pat_ExtractArgs x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Pat_Extract"),
    Core.projectionField = (Core.Name "args")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

pat_ExtractWithFun :: (Phantoms.TTerm Meta.Pat_Extract -> Phantoms.TTerm Meta.Data -> Phantoms.TTerm Meta.Pat_Extract)
pat_ExtractWithFun original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Pat_Extract"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "fun"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "args"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Pat_Extract"),
          Core.projectionField = (Core.Name "args")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

pat_ExtractWithArgs :: (Phantoms.TTerm Meta.Pat_Extract -> Phantoms.TTerm [Meta.Pat] -> Phantoms.TTerm Meta.Pat_Extract)
pat_ExtractWithArgs original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Pat_Extract"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "fun"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Pat_Extract"),
          Core.projectionField = (Core.Name "fun")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "args"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

pat_ExtractInfix :: (Phantoms.TTerm Meta.Pat -> Phantoms.TTerm Meta.Data_Name -> Phantoms.TTerm [Meta.Pat] -> Phantoms.TTerm Meta.Pat_ExtractInfix)
pat_ExtractInfix lhs op rhs = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Pat_ExtractInfix"),
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

pat_ExtractInfixLhs :: (Phantoms.TTerm Meta.Pat_ExtractInfix -> Phantoms.TTerm Meta.Pat)
pat_ExtractInfixLhs x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Pat_ExtractInfix"),
    Core.projectionField = (Core.Name "lhs")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

pat_ExtractInfixOp :: (Phantoms.TTerm Meta.Pat_ExtractInfix -> Phantoms.TTerm Meta.Data_Name)
pat_ExtractInfixOp x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Pat_ExtractInfix"),
    Core.projectionField = (Core.Name "op")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

pat_ExtractInfixRhs :: (Phantoms.TTerm Meta.Pat_ExtractInfix -> Phantoms.TTerm [Meta.Pat])
pat_ExtractInfixRhs x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Pat_ExtractInfix"),
    Core.projectionField = (Core.Name "rhs")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

pat_ExtractInfixWithLhs :: (Phantoms.TTerm Meta.Pat_ExtractInfix -> Phantoms.TTerm Meta.Pat -> Phantoms.TTerm Meta.Pat_ExtractInfix)
pat_ExtractInfixWithLhs original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Pat_ExtractInfix"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "lhs"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "op"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Pat_ExtractInfix"),
          Core.projectionField = (Core.Name "op")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "rhs"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Pat_ExtractInfix"),
          Core.projectionField = (Core.Name "rhs")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

pat_ExtractInfixWithOp :: (Phantoms.TTerm Meta.Pat_ExtractInfix -> Phantoms.TTerm Meta.Data_Name -> Phantoms.TTerm Meta.Pat_ExtractInfix)
pat_ExtractInfixWithOp original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Pat_ExtractInfix"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "lhs"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Pat_ExtractInfix"),
          Core.projectionField = (Core.Name "lhs")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "op"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "rhs"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Pat_ExtractInfix"),
          Core.projectionField = (Core.Name "rhs")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

pat_ExtractInfixWithRhs :: (Phantoms.TTerm Meta.Pat_ExtractInfix -> Phantoms.TTerm [Meta.Pat] -> Phantoms.TTerm Meta.Pat_ExtractInfix)
pat_ExtractInfixWithRhs original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Pat_ExtractInfix"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "lhs"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Pat_ExtractInfix"),
          Core.projectionField = (Core.Name "lhs")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "op"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Pat_ExtractInfix"),
          Core.projectionField = (Core.Name "op")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "rhs"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

pat_Interpolate :: (Phantoms.TTerm Meta.Data_Name -> Phantoms.TTerm [Meta.Lit] -> Phantoms.TTerm Meta.Pat_Interpolate)
pat_Interpolate prefix parts = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Pat_Interpolate"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "prefix"),
      Core.fieldTerm = (Phantoms.unTTerm prefix)},
    Core.Field {
      Core.fieldName = (Core.Name "parts"),
      Core.fieldTerm = (Phantoms.unTTerm parts)}]})))

pat_InterpolatePrefix :: (Phantoms.TTerm Meta.Pat_Interpolate -> Phantoms.TTerm Meta.Data_Name)
pat_InterpolatePrefix x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Pat_Interpolate"),
    Core.projectionField = (Core.Name "prefix")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

pat_InterpolateParts :: (Phantoms.TTerm Meta.Pat_Interpolate -> Phantoms.TTerm [Meta.Lit])
pat_InterpolateParts x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Pat_Interpolate"),
    Core.projectionField = (Core.Name "parts")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

pat_InterpolateWithPrefix :: (Phantoms.TTerm Meta.Pat_Interpolate -> Phantoms.TTerm Meta.Data_Name -> Phantoms.TTerm Meta.Pat_Interpolate)
pat_InterpolateWithPrefix original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Pat_Interpolate"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "prefix"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "parts"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Pat_Interpolate"),
          Core.projectionField = (Core.Name "parts")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

pat_InterpolateWithParts :: (Phantoms.TTerm Meta.Pat_Interpolate -> Phantoms.TTerm [Meta.Lit] -> Phantoms.TTerm Meta.Pat_Interpolate)
pat_InterpolateWithParts original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Pat_Interpolate"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "prefix"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Pat_Interpolate"),
          Core.projectionField = (Core.Name "prefix")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "parts"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

pat_Xml :: (Phantoms.TTerm [Meta.Lit] -> Phantoms.TTerm [Meta.Pat] -> Phantoms.TTerm Meta.Pat_Xml)
pat_Xml parts args = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Pat_Xml"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "parts"),
      Core.fieldTerm = (Phantoms.unTTerm parts)},
    Core.Field {
      Core.fieldName = (Core.Name "args"),
      Core.fieldTerm = (Phantoms.unTTerm args)}]})))

pat_XmlParts :: (Phantoms.TTerm Meta.Pat_Xml -> Phantoms.TTerm [Meta.Lit])
pat_XmlParts x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Pat_Xml"),
    Core.projectionField = (Core.Name "parts")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

pat_XmlArgs :: (Phantoms.TTerm Meta.Pat_Xml -> Phantoms.TTerm [Meta.Pat])
pat_XmlArgs x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Pat_Xml"),
    Core.projectionField = (Core.Name "args")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

pat_XmlWithParts :: (Phantoms.TTerm Meta.Pat_Xml -> Phantoms.TTerm [Meta.Lit] -> Phantoms.TTerm Meta.Pat_Xml)
pat_XmlWithParts original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Pat_Xml"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "parts"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "args"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Pat_Xml"),
          Core.projectionField = (Core.Name "args")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

pat_XmlWithArgs :: (Phantoms.TTerm Meta.Pat_Xml -> Phantoms.TTerm [Meta.Pat] -> Phantoms.TTerm Meta.Pat_Xml)
pat_XmlWithArgs original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Pat_Xml"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "parts"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Pat_Xml"),
          Core.projectionField = (Core.Name "parts")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "args"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

pat_Typed :: (Phantoms.TTerm Meta.Pat -> Phantoms.TTerm Meta.Type -> Phantoms.TTerm Meta.Pat_Typed)
pat_Typed lhs rhs = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Pat_Typed"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "lhs"),
      Core.fieldTerm = (Phantoms.unTTerm lhs)},
    Core.Field {
      Core.fieldName = (Core.Name "rhs"),
      Core.fieldTerm = (Phantoms.unTTerm rhs)}]})))

pat_TypedLhs :: (Phantoms.TTerm Meta.Pat_Typed -> Phantoms.TTerm Meta.Pat)
pat_TypedLhs x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Pat_Typed"),
    Core.projectionField = (Core.Name "lhs")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

pat_TypedRhs :: (Phantoms.TTerm Meta.Pat_Typed -> Phantoms.TTerm Meta.Type)
pat_TypedRhs x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Pat_Typed"),
    Core.projectionField = (Core.Name "rhs")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

pat_TypedWithLhs :: (Phantoms.TTerm Meta.Pat_Typed -> Phantoms.TTerm Meta.Pat -> Phantoms.TTerm Meta.Pat_Typed)
pat_TypedWithLhs original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Pat_Typed"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "lhs"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "rhs"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Pat_Typed"),
          Core.projectionField = (Core.Name "rhs")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

pat_TypedWithRhs :: (Phantoms.TTerm Meta.Pat_Typed -> Phantoms.TTerm Meta.Type -> Phantoms.TTerm Meta.Pat_Typed)
pat_TypedWithRhs original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Pat_Typed"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "lhs"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Pat_Typed"),
          Core.projectionField = (Core.Name "lhs")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "rhs"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

pat_Macro :: (Phantoms.TTerm Meta.Data -> Phantoms.TTerm Meta.Pat_Macro)
pat_Macro body = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Pat_Macro"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "body"),
      Core.fieldTerm = (Phantoms.unTTerm body)}]})))

pat_MacroBody :: (Phantoms.TTerm Meta.Pat_Macro -> Phantoms.TTerm Meta.Data)
pat_MacroBody x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Pat_Macro"),
    Core.projectionField = (Core.Name "body")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

pat_MacroWithBody :: (Phantoms.TTerm Meta.Pat_Macro -> Phantoms.TTerm Meta.Data -> Phantoms.TTerm Meta.Pat_Macro)
pat_MacroWithBody original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Pat_Macro"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "body"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

pat_Given :: (Phantoms.TTerm Meta.Type -> Phantoms.TTerm Meta.Pat_Given)
pat_Given tpe = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Pat_Given"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "tpe"),
      Core.fieldTerm = (Phantoms.unTTerm tpe)}]})))

pat_GivenTpe :: (Phantoms.TTerm Meta.Pat_Given -> Phantoms.TTerm Meta.Type)
pat_GivenTpe x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Pat_Given"),
    Core.projectionField = (Core.Name "tpe")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

pat_GivenWithTpe :: (Phantoms.TTerm Meta.Pat_Given -> Phantoms.TTerm Meta.Type -> Phantoms.TTerm Meta.Pat_Given)
pat_GivenWithTpe original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Pat_Given"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "tpe"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

memberTerm :: (Phantoms.TTerm Meta.Member_Data -> Phantoms.TTerm Meta.Member)
memberTerm x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Member"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "term"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

memberType :: (Phantoms.TTerm Meta.Member_Type -> Phantoms.TTerm Meta.Member)
memberType x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Member"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "type"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

memberTermParam :: (Phantoms.TTerm Meta.Data_Param -> Phantoms.TTerm Meta.Member)
memberTermParam x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Member"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "termParam"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

memberTypeParam :: (Phantoms.TTerm Meta.Type_Param -> Phantoms.TTerm Meta.Member)
memberTypeParam x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Member"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "typeParam"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

memberSelf :: (Phantoms.TTerm Meta.Self -> Phantoms.TTerm Meta.Member)
memberSelf x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Member"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "self"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

member_DataPkg :: (Phantoms.TTerm Meta.Pkg -> Phantoms.TTerm Meta.Member_Data)
member_DataPkg x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Member_Data"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "pkg"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

member_DataObject :: (Phantoms.TTerm Meta.Pkg_Object -> Phantoms.TTerm Meta.Member_Data)
member_DataObject x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Member_Data"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "object"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

member_Type :: (Phantoms.TTerm Meta.Type_Name -> Phantoms.TTerm Meta.Member_Type)
member_Type name = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Member_Type"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Phantoms.unTTerm name)}]})))

member_TypeName :: (Phantoms.TTerm Meta.Member_Type -> Phantoms.TTerm Meta.Type_Name)
member_TypeName x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Member_Type"),
    Core.projectionField = (Core.Name "name")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

member_TypeWithName :: (Phantoms.TTerm Meta.Member_Type -> Phantoms.TTerm Meta.Type_Name -> Phantoms.TTerm Meta.Member_Type)
member_TypeWithName original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Member_Type"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

declVal :: (Phantoms.TTerm Meta.Decl_Val -> Phantoms.TTerm Meta.Decl)
declVal x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Decl"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "val"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

declVar :: (Phantoms.TTerm Meta.Decl_Var -> Phantoms.TTerm Meta.Decl)
declVar x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Decl"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "var"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

declDef :: (Phantoms.TTerm Meta.Decl_Def -> Phantoms.TTerm Meta.Decl)
declDef x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Decl"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "def"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

declType :: (Phantoms.TTerm Meta.Decl_Type -> Phantoms.TTerm Meta.Decl)
declType x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Decl"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "type"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

declGiven :: (Phantoms.TTerm Meta.Decl_Given -> Phantoms.TTerm Meta.Decl)
declGiven x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Decl"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "given"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

decl_Val :: (Phantoms.TTerm [Meta.Mod] -> Phantoms.TTerm [Meta.Pat] -> Phantoms.TTerm Meta.Type -> Phantoms.TTerm Meta.Decl_Val)
decl_Val mods pats decltpe = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Decl_Val"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "mods"),
      Core.fieldTerm = (Phantoms.unTTerm mods)},
    Core.Field {
      Core.fieldName = (Core.Name "pats"),
      Core.fieldTerm = (Phantoms.unTTerm pats)},
    Core.Field {
      Core.fieldName = (Core.Name "decltpe"),
      Core.fieldTerm = (Phantoms.unTTerm decltpe)}]})))

decl_ValMods :: (Phantoms.TTerm Meta.Decl_Val -> Phantoms.TTerm [Meta.Mod])
decl_ValMods x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Decl_Val"),
    Core.projectionField = (Core.Name "mods")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

decl_ValPats :: (Phantoms.TTerm Meta.Decl_Val -> Phantoms.TTerm [Meta.Pat])
decl_ValPats x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Decl_Val"),
    Core.projectionField = (Core.Name "pats")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

decl_ValDecltpe :: (Phantoms.TTerm Meta.Decl_Val -> Phantoms.TTerm Meta.Type)
decl_ValDecltpe x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Decl_Val"),
    Core.projectionField = (Core.Name "decltpe")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

decl_ValWithMods :: (Phantoms.TTerm Meta.Decl_Val -> Phantoms.TTerm [Meta.Mod] -> Phantoms.TTerm Meta.Decl_Val)
decl_ValWithMods original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Decl_Val"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "mods"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "pats"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Decl_Val"),
          Core.projectionField = (Core.Name "pats")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "decltpe"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Decl_Val"),
          Core.projectionField = (Core.Name "decltpe")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

decl_ValWithPats :: (Phantoms.TTerm Meta.Decl_Val -> Phantoms.TTerm [Meta.Pat] -> Phantoms.TTerm Meta.Decl_Val)
decl_ValWithPats original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Decl_Val"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "mods"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Decl_Val"),
          Core.projectionField = (Core.Name "mods")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "pats"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "decltpe"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Decl_Val"),
          Core.projectionField = (Core.Name "decltpe")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

decl_ValWithDecltpe :: (Phantoms.TTerm Meta.Decl_Val -> Phantoms.TTerm Meta.Type -> Phantoms.TTerm Meta.Decl_Val)
decl_ValWithDecltpe original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Decl_Val"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "mods"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Decl_Val"),
          Core.projectionField = (Core.Name "mods")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "pats"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Decl_Val"),
          Core.projectionField = (Core.Name "pats")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "decltpe"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

decl_Var :: (Phantoms.TTerm [Meta.Mod] -> Phantoms.TTerm [Meta.Pat] -> Phantoms.TTerm Meta.Type -> Phantoms.TTerm Meta.Decl_Var)
decl_Var mods pats decltpe = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Decl_Var"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "mods"),
      Core.fieldTerm = (Phantoms.unTTerm mods)},
    Core.Field {
      Core.fieldName = (Core.Name "pats"),
      Core.fieldTerm = (Phantoms.unTTerm pats)},
    Core.Field {
      Core.fieldName = (Core.Name "decltpe"),
      Core.fieldTerm = (Phantoms.unTTerm decltpe)}]})))

decl_VarMods :: (Phantoms.TTerm Meta.Decl_Var -> Phantoms.TTerm [Meta.Mod])
decl_VarMods x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Decl_Var"),
    Core.projectionField = (Core.Name "mods")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

decl_VarPats :: (Phantoms.TTerm Meta.Decl_Var -> Phantoms.TTerm [Meta.Pat])
decl_VarPats x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Decl_Var"),
    Core.projectionField = (Core.Name "pats")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

decl_VarDecltpe :: (Phantoms.TTerm Meta.Decl_Var -> Phantoms.TTerm Meta.Type)
decl_VarDecltpe x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Decl_Var"),
    Core.projectionField = (Core.Name "decltpe")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

decl_VarWithMods :: (Phantoms.TTerm Meta.Decl_Var -> Phantoms.TTerm [Meta.Mod] -> Phantoms.TTerm Meta.Decl_Var)
decl_VarWithMods original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Decl_Var"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "mods"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "pats"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Decl_Var"),
          Core.projectionField = (Core.Name "pats")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "decltpe"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Decl_Var"),
          Core.projectionField = (Core.Name "decltpe")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

decl_VarWithPats :: (Phantoms.TTerm Meta.Decl_Var -> Phantoms.TTerm [Meta.Pat] -> Phantoms.TTerm Meta.Decl_Var)
decl_VarWithPats original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Decl_Var"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "mods"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Decl_Var"),
          Core.projectionField = (Core.Name "mods")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "pats"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "decltpe"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Decl_Var"),
          Core.projectionField = (Core.Name "decltpe")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

decl_VarWithDecltpe :: (Phantoms.TTerm Meta.Decl_Var -> Phantoms.TTerm Meta.Type -> Phantoms.TTerm Meta.Decl_Var)
decl_VarWithDecltpe original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Decl_Var"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "mods"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Decl_Var"),
          Core.projectionField = (Core.Name "mods")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "pats"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Decl_Var"),
          Core.projectionField = (Core.Name "pats")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "decltpe"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

decl_Def :: (Phantoms.TTerm [Meta.Mod] -> Phantoms.TTerm Meta.Data_Name -> Phantoms.TTerm [Meta.Type_Param] -> Phantoms.TTerm [[Meta.Data_Param]] -> Phantoms.TTerm Meta.Type -> Phantoms.TTerm Meta.Decl_Def)
decl_Def mods name tparams paramss decltpe = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Decl_Def"),
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
      Core.fieldTerm = (Phantoms.unTTerm decltpe)}]})))

decl_DefMods :: (Phantoms.TTerm Meta.Decl_Def -> Phantoms.TTerm [Meta.Mod])
decl_DefMods x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Decl_Def"),
    Core.projectionField = (Core.Name "mods")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

decl_DefName :: (Phantoms.TTerm Meta.Decl_Def -> Phantoms.TTerm Meta.Data_Name)
decl_DefName x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Decl_Def"),
    Core.projectionField = (Core.Name "name")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

decl_DefTparams :: (Phantoms.TTerm Meta.Decl_Def -> Phantoms.TTerm [Meta.Type_Param])
decl_DefTparams x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Decl_Def"),
    Core.projectionField = (Core.Name "tparams")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

decl_DefParamss :: (Phantoms.TTerm Meta.Decl_Def -> Phantoms.TTerm [[Meta.Data_Param]])
decl_DefParamss x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Decl_Def"),
    Core.projectionField = (Core.Name "paramss")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

decl_DefDecltpe :: (Phantoms.TTerm Meta.Decl_Def -> Phantoms.TTerm Meta.Type)
decl_DefDecltpe x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Decl_Def"),
    Core.projectionField = (Core.Name "decltpe")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

decl_DefWithMods :: (Phantoms.TTerm Meta.Decl_Def -> Phantoms.TTerm [Meta.Mod] -> Phantoms.TTerm Meta.Decl_Def)
decl_DefWithMods original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Decl_Def"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "mods"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Decl_Def"),
          Core.projectionField = (Core.Name "name")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "tparams"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Decl_Def"),
          Core.projectionField = (Core.Name "tparams")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "paramss"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Decl_Def"),
          Core.projectionField = (Core.Name "paramss")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "decltpe"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Decl_Def"),
          Core.projectionField = (Core.Name "decltpe")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

decl_DefWithName :: (Phantoms.TTerm Meta.Decl_Def -> Phantoms.TTerm Meta.Data_Name -> Phantoms.TTerm Meta.Decl_Def)
decl_DefWithName original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Decl_Def"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "mods"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Decl_Def"),
          Core.projectionField = (Core.Name "mods")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "tparams"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Decl_Def"),
          Core.projectionField = (Core.Name "tparams")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "paramss"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Decl_Def"),
          Core.projectionField = (Core.Name "paramss")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "decltpe"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Decl_Def"),
          Core.projectionField = (Core.Name "decltpe")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

decl_DefWithTparams :: (Phantoms.TTerm Meta.Decl_Def -> Phantoms.TTerm [Meta.Type_Param] -> Phantoms.TTerm Meta.Decl_Def)
decl_DefWithTparams original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Decl_Def"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "mods"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Decl_Def"),
          Core.projectionField = (Core.Name "mods")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Decl_Def"),
          Core.projectionField = (Core.Name "name")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "tparams"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "paramss"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Decl_Def"),
          Core.projectionField = (Core.Name "paramss")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "decltpe"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Decl_Def"),
          Core.projectionField = (Core.Name "decltpe")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

decl_DefWithParamss :: (Phantoms.TTerm Meta.Decl_Def -> Phantoms.TTerm [[Meta.Data_Param]] -> Phantoms.TTerm Meta.Decl_Def)
decl_DefWithParamss original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Decl_Def"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "mods"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Decl_Def"),
          Core.projectionField = (Core.Name "mods")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Decl_Def"),
          Core.projectionField = (Core.Name "name")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "tparams"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Decl_Def"),
          Core.projectionField = (Core.Name "tparams")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "paramss"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "decltpe"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Decl_Def"),
          Core.projectionField = (Core.Name "decltpe")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

decl_DefWithDecltpe :: (Phantoms.TTerm Meta.Decl_Def -> Phantoms.TTerm Meta.Type -> Phantoms.TTerm Meta.Decl_Def)
decl_DefWithDecltpe original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Decl_Def"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "mods"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Decl_Def"),
          Core.projectionField = (Core.Name "mods")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Decl_Def"),
          Core.projectionField = (Core.Name "name")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "tparams"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Decl_Def"),
          Core.projectionField = (Core.Name "tparams")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "paramss"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Decl_Def"),
          Core.projectionField = (Core.Name "paramss")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "decltpe"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

decl_Type :: (Phantoms.TTerm [Meta.Mod] -> Phantoms.TTerm Meta.Type_Name -> Phantoms.TTerm [Meta.Type_Param] -> Phantoms.TTerm Meta.TypeBounds -> Phantoms.TTerm Meta.Decl_Type)
decl_Type mods name tparams bounds = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Decl_Type"),
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
      Core.fieldTerm = (Phantoms.unTTerm bounds)}]})))

decl_TypeMods :: (Phantoms.TTerm Meta.Decl_Type -> Phantoms.TTerm [Meta.Mod])
decl_TypeMods x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Decl_Type"),
    Core.projectionField = (Core.Name "mods")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

decl_TypeName :: (Phantoms.TTerm Meta.Decl_Type -> Phantoms.TTerm Meta.Type_Name)
decl_TypeName x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Decl_Type"),
    Core.projectionField = (Core.Name "name")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

decl_TypeTparams :: (Phantoms.TTerm Meta.Decl_Type -> Phantoms.TTerm [Meta.Type_Param])
decl_TypeTparams x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Decl_Type"),
    Core.projectionField = (Core.Name "tparams")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

decl_TypeBounds :: (Phantoms.TTerm Meta.Decl_Type -> Phantoms.TTerm Meta.TypeBounds)
decl_TypeBounds x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Decl_Type"),
    Core.projectionField = (Core.Name "bounds")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

decl_TypeWithMods :: (Phantoms.TTerm Meta.Decl_Type -> Phantoms.TTerm [Meta.Mod] -> Phantoms.TTerm Meta.Decl_Type)
decl_TypeWithMods original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Decl_Type"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "mods"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Decl_Type"),
          Core.projectionField = (Core.Name "name")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "tparams"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Decl_Type"),
          Core.projectionField = (Core.Name "tparams")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "bounds"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Decl_Type"),
          Core.projectionField = (Core.Name "bounds")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

decl_TypeWithName :: (Phantoms.TTerm Meta.Decl_Type -> Phantoms.TTerm Meta.Type_Name -> Phantoms.TTerm Meta.Decl_Type)
decl_TypeWithName original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Decl_Type"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "mods"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Decl_Type"),
          Core.projectionField = (Core.Name "mods")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "tparams"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Decl_Type"),
          Core.projectionField = (Core.Name "tparams")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "bounds"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Decl_Type"),
          Core.projectionField = (Core.Name "bounds")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

decl_TypeWithTparams :: (Phantoms.TTerm Meta.Decl_Type -> Phantoms.TTerm [Meta.Type_Param] -> Phantoms.TTerm Meta.Decl_Type)
decl_TypeWithTparams original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Decl_Type"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "mods"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Decl_Type"),
          Core.projectionField = (Core.Name "mods")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Decl_Type"),
          Core.projectionField = (Core.Name "name")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "tparams"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "bounds"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Decl_Type"),
          Core.projectionField = (Core.Name "bounds")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

decl_TypeWithBounds :: (Phantoms.TTerm Meta.Decl_Type -> Phantoms.TTerm Meta.TypeBounds -> Phantoms.TTerm Meta.Decl_Type)
decl_TypeWithBounds original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Decl_Type"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "mods"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Decl_Type"),
          Core.projectionField = (Core.Name "mods")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Decl_Type"),
          Core.projectionField = (Core.Name "name")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "tparams"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Decl_Type"),
          Core.projectionField = (Core.Name "tparams")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "bounds"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

decl_Given :: (Phantoms.TTerm [Meta.Mod] -> Phantoms.TTerm Meta.Data_Name -> Phantoms.TTerm [Meta.Type_Param] -> Phantoms.TTerm [[Meta.Data_Param]] -> Phantoms.TTerm Meta.Type -> Phantoms.TTerm Meta.Decl_Given)
decl_Given mods name tparams sparams decltpe = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Decl_Given"),
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
      Core.fieldTerm = (Phantoms.unTTerm decltpe)}]})))

decl_GivenMods :: (Phantoms.TTerm Meta.Decl_Given -> Phantoms.TTerm [Meta.Mod])
decl_GivenMods x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Decl_Given"),
    Core.projectionField = (Core.Name "mods")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

decl_GivenName :: (Phantoms.TTerm Meta.Decl_Given -> Phantoms.TTerm Meta.Data_Name)
decl_GivenName x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Decl_Given"),
    Core.projectionField = (Core.Name "name")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

decl_GivenTparams :: (Phantoms.TTerm Meta.Decl_Given -> Phantoms.TTerm [Meta.Type_Param])
decl_GivenTparams x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Decl_Given"),
    Core.projectionField = (Core.Name "tparams")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

decl_GivenSparams :: (Phantoms.TTerm Meta.Decl_Given -> Phantoms.TTerm [[Meta.Data_Param]])
decl_GivenSparams x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Decl_Given"),
    Core.projectionField = (Core.Name "sparams")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

decl_GivenDecltpe :: (Phantoms.TTerm Meta.Decl_Given -> Phantoms.TTerm Meta.Type)
decl_GivenDecltpe x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Decl_Given"),
    Core.projectionField = (Core.Name "decltpe")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

decl_GivenWithMods :: (Phantoms.TTerm Meta.Decl_Given -> Phantoms.TTerm [Meta.Mod] -> Phantoms.TTerm Meta.Decl_Given)
decl_GivenWithMods original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Decl_Given"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "mods"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Decl_Given"),
          Core.projectionField = (Core.Name "name")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "tparams"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Decl_Given"),
          Core.projectionField = (Core.Name "tparams")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "sparams"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Decl_Given"),
          Core.projectionField = (Core.Name "sparams")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "decltpe"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Decl_Given"),
          Core.projectionField = (Core.Name "decltpe")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

decl_GivenWithName :: (Phantoms.TTerm Meta.Decl_Given -> Phantoms.TTerm Meta.Data_Name -> Phantoms.TTerm Meta.Decl_Given)
decl_GivenWithName original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Decl_Given"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "mods"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Decl_Given"),
          Core.projectionField = (Core.Name "mods")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "tparams"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Decl_Given"),
          Core.projectionField = (Core.Name "tparams")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "sparams"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Decl_Given"),
          Core.projectionField = (Core.Name "sparams")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "decltpe"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Decl_Given"),
          Core.projectionField = (Core.Name "decltpe")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

decl_GivenWithTparams :: (Phantoms.TTerm Meta.Decl_Given -> Phantoms.TTerm [Meta.Type_Param] -> Phantoms.TTerm Meta.Decl_Given)
decl_GivenWithTparams original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Decl_Given"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "mods"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Decl_Given"),
          Core.projectionField = (Core.Name "mods")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Decl_Given"),
          Core.projectionField = (Core.Name "name")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "tparams"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "sparams"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Decl_Given"),
          Core.projectionField = (Core.Name "sparams")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "decltpe"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Decl_Given"),
          Core.projectionField = (Core.Name "decltpe")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

decl_GivenWithSparams :: (Phantoms.TTerm Meta.Decl_Given -> Phantoms.TTerm [[Meta.Data_Param]] -> Phantoms.TTerm Meta.Decl_Given)
decl_GivenWithSparams original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Decl_Given"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "mods"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Decl_Given"),
          Core.projectionField = (Core.Name "mods")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Decl_Given"),
          Core.projectionField = (Core.Name "name")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "tparams"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Decl_Given"),
          Core.projectionField = (Core.Name "tparams")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "sparams"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "decltpe"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Decl_Given"),
          Core.projectionField = (Core.Name "decltpe")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

decl_GivenWithDecltpe :: (Phantoms.TTerm Meta.Decl_Given -> Phantoms.TTerm Meta.Type -> Phantoms.TTerm Meta.Decl_Given)
decl_GivenWithDecltpe original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Decl_Given"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "mods"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Decl_Given"),
          Core.projectionField = (Core.Name "mods")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Decl_Given"),
          Core.projectionField = (Core.Name "name")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "tparams"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Decl_Given"),
          Core.projectionField = (Core.Name "tparams")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "sparams"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Decl_Given"),
          Core.projectionField = (Core.Name "sparams")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "decltpe"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

defnVal :: (Phantoms.TTerm Meta.Defn_Val -> Phantoms.TTerm Meta.Defn)
defnVal x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "val"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

defnVar :: (Phantoms.TTerm Meta.Defn_Var -> Phantoms.TTerm Meta.Defn)
defnVar x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "var"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

defnGiven :: (Phantoms.TTerm Meta.Defn_Given -> Phantoms.TTerm Meta.Defn)
defnGiven x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "given"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

defnEnum :: (Phantoms.TTerm Meta.Defn_Enum -> Phantoms.TTerm Meta.Defn)
defnEnum x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "enum"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

defnEnumCase :: (Phantoms.TTerm Meta.Defn_EnumCase -> Phantoms.TTerm Meta.Defn)
defnEnumCase x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "enumCase"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

defnRepeatedEnumCase :: (Phantoms.TTerm Meta.Defn_RepeatedEnumCase -> Phantoms.TTerm Meta.Defn)
defnRepeatedEnumCase x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "repeatedEnumCase"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

defnGivenAlias :: (Phantoms.TTerm Meta.Defn_GivenAlias -> Phantoms.TTerm Meta.Defn)
defnGivenAlias x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "givenAlias"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

defnExtensionGroup :: (Phantoms.TTerm Meta.Defn_ExtensionGroup -> Phantoms.TTerm Meta.Defn)
defnExtensionGroup x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "extensionGroup"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

defnDef :: (Phantoms.TTerm Meta.Defn_Def -> Phantoms.TTerm Meta.Defn)
defnDef x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "def"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

defnMacro :: (Phantoms.TTerm Meta.Defn_Macro -> Phantoms.TTerm Meta.Defn)
defnMacro x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "macro"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

defnType :: (Phantoms.TTerm Meta.Defn_Type -> Phantoms.TTerm Meta.Defn)
defnType x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "type"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

defnClass :: (Phantoms.TTerm Meta.Defn_Class -> Phantoms.TTerm Meta.Defn)
defnClass x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "class"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

defnTrait :: (Phantoms.TTerm Meta.Defn_Trait -> Phantoms.TTerm Meta.Defn)
defnTrait x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "trait"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

defnObject :: (Phantoms.TTerm Meta.Defn_Object -> Phantoms.TTerm Meta.Defn)
defnObject x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "object"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

defn_Val :: (Phantoms.TTerm [Meta.Mod] -> Phantoms.TTerm [Meta.Pat] -> Phantoms.TTerm (Maybe Meta.Type) -> Phantoms.TTerm Meta.Data -> Phantoms.TTerm Meta.Defn_Val)
defn_Val mods pats decltpe rhs = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Val"),
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
      Core.fieldTerm = (Phantoms.unTTerm rhs)}]})))

defn_ValMods :: (Phantoms.TTerm Meta.Defn_Val -> Phantoms.TTerm [Meta.Mod])
defn_ValMods x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Val"),
    Core.projectionField = (Core.Name "mods")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

defn_ValPats :: (Phantoms.TTerm Meta.Defn_Val -> Phantoms.TTerm [Meta.Pat])
defn_ValPats x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Val"),
    Core.projectionField = (Core.Name "pats")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

defn_ValDecltpe :: (Phantoms.TTerm Meta.Defn_Val -> Phantoms.TTerm (Maybe Meta.Type))
defn_ValDecltpe x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Val"),
    Core.projectionField = (Core.Name "decltpe")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

defn_ValRhs :: (Phantoms.TTerm Meta.Defn_Val -> Phantoms.TTerm Meta.Data)
defn_ValRhs x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Val"),
    Core.projectionField = (Core.Name "rhs")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

defn_ValWithMods :: (Phantoms.TTerm Meta.Defn_Val -> Phantoms.TTerm [Meta.Mod] -> Phantoms.TTerm Meta.Defn_Val)
defn_ValWithMods original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Val"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "mods"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "pats"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Val"),
          Core.projectionField = (Core.Name "pats")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "decltpe"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Val"),
          Core.projectionField = (Core.Name "decltpe")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "rhs"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Val"),
          Core.projectionField = (Core.Name "rhs")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

defn_ValWithPats :: (Phantoms.TTerm Meta.Defn_Val -> Phantoms.TTerm [Meta.Pat] -> Phantoms.TTerm Meta.Defn_Val)
defn_ValWithPats original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Val"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "mods"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Val"),
          Core.projectionField = (Core.Name "mods")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "pats"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "decltpe"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Val"),
          Core.projectionField = (Core.Name "decltpe")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "rhs"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Val"),
          Core.projectionField = (Core.Name "rhs")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

defn_ValWithDecltpe :: (Phantoms.TTerm Meta.Defn_Val -> Phantoms.TTerm (Maybe Meta.Type) -> Phantoms.TTerm Meta.Defn_Val)
defn_ValWithDecltpe original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Val"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "mods"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Val"),
          Core.projectionField = (Core.Name "mods")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "pats"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Val"),
          Core.projectionField = (Core.Name "pats")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "decltpe"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "rhs"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Val"),
          Core.projectionField = (Core.Name "rhs")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

defn_ValWithRhs :: (Phantoms.TTerm Meta.Defn_Val -> Phantoms.TTerm Meta.Data -> Phantoms.TTerm Meta.Defn_Val)
defn_ValWithRhs original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Val"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "mods"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Val"),
          Core.projectionField = (Core.Name "mods")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "pats"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Val"),
          Core.projectionField = (Core.Name "pats")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "decltpe"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Val"),
          Core.projectionField = (Core.Name "decltpe")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "rhs"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

defn_Var :: (Phantoms.TTerm [Meta.Mod] -> Phantoms.TTerm [Meta.Pat] -> Phantoms.TTerm Meta.Type -> Phantoms.TTerm (Maybe Meta.Data) -> Phantoms.TTerm Meta.Defn_Var)
defn_Var mods pats decltpe rhs = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Var"),
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
      Core.fieldTerm = (Phantoms.unTTerm rhs)}]})))

defn_VarMods :: (Phantoms.TTerm Meta.Defn_Var -> Phantoms.TTerm [Meta.Mod])
defn_VarMods x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Var"),
    Core.projectionField = (Core.Name "mods")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

defn_VarPats :: (Phantoms.TTerm Meta.Defn_Var -> Phantoms.TTerm [Meta.Pat])
defn_VarPats x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Var"),
    Core.projectionField = (Core.Name "pats")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

defn_VarDecltpe :: (Phantoms.TTerm Meta.Defn_Var -> Phantoms.TTerm Meta.Type)
defn_VarDecltpe x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Var"),
    Core.projectionField = (Core.Name "decltpe")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

defn_VarRhs :: (Phantoms.TTerm Meta.Defn_Var -> Phantoms.TTerm (Maybe Meta.Data))
defn_VarRhs x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Var"),
    Core.projectionField = (Core.Name "rhs")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

defn_VarWithMods :: (Phantoms.TTerm Meta.Defn_Var -> Phantoms.TTerm [Meta.Mod] -> Phantoms.TTerm Meta.Defn_Var)
defn_VarWithMods original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Var"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "mods"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "pats"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Var"),
          Core.projectionField = (Core.Name "pats")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "decltpe"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Var"),
          Core.projectionField = (Core.Name "decltpe")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "rhs"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Var"),
          Core.projectionField = (Core.Name "rhs")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

defn_VarWithPats :: (Phantoms.TTerm Meta.Defn_Var -> Phantoms.TTerm [Meta.Pat] -> Phantoms.TTerm Meta.Defn_Var)
defn_VarWithPats original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Var"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "mods"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Var"),
          Core.projectionField = (Core.Name "mods")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "pats"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "decltpe"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Var"),
          Core.projectionField = (Core.Name "decltpe")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "rhs"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Var"),
          Core.projectionField = (Core.Name "rhs")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

defn_VarWithDecltpe :: (Phantoms.TTerm Meta.Defn_Var -> Phantoms.TTerm Meta.Type -> Phantoms.TTerm Meta.Defn_Var)
defn_VarWithDecltpe original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Var"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "mods"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Var"),
          Core.projectionField = (Core.Name "mods")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "pats"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Var"),
          Core.projectionField = (Core.Name "pats")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "decltpe"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "rhs"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Var"),
          Core.projectionField = (Core.Name "rhs")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

defn_VarWithRhs :: (Phantoms.TTerm Meta.Defn_Var -> Phantoms.TTerm (Maybe Meta.Data) -> Phantoms.TTerm Meta.Defn_Var)
defn_VarWithRhs original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Var"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "mods"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Var"),
          Core.projectionField = (Core.Name "mods")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "pats"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Var"),
          Core.projectionField = (Core.Name "pats")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "decltpe"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Var"),
          Core.projectionField = (Core.Name "decltpe")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "rhs"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

defn_Given :: (Phantoms.TTerm [Meta.Mod] -> Phantoms.TTerm Meta.Name -> Phantoms.TTerm [[Meta.Type_Param]] -> Phantoms.TTerm [[Meta.Data_Param]] -> Phantoms.TTerm Meta.Template -> Phantoms.TTerm Meta.Defn_Given)
defn_Given mods name tparams sparams templ = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Given"),
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
      Core.fieldTerm = (Phantoms.unTTerm templ)}]})))

defn_GivenMods :: (Phantoms.TTerm Meta.Defn_Given -> Phantoms.TTerm [Meta.Mod])
defn_GivenMods x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Given"),
    Core.projectionField = (Core.Name "mods")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

defn_GivenName :: (Phantoms.TTerm Meta.Defn_Given -> Phantoms.TTerm Meta.Name)
defn_GivenName x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Given"),
    Core.projectionField = (Core.Name "name")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

defn_GivenTparams :: (Phantoms.TTerm Meta.Defn_Given -> Phantoms.TTerm [[Meta.Type_Param]])
defn_GivenTparams x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Given"),
    Core.projectionField = (Core.Name "tparams")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

defn_GivenSparams :: (Phantoms.TTerm Meta.Defn_Given -> Phantoms.TTerm [[Meta.Data_Param]])
defn_GivenSparams x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Given"),
    Core.projectionField = (Core.Name "sparams")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

defn_GivenTempl :: (Phantoms.TTerm Meta.Defn_Given -> Phantoms.TTerm Meta.Template)
defn_GivenTempl x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Given"),
    Core.projectionField = (Core.Name "templ")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

defn_GivenWithMods :: (Phantoms.TTerm Meta.Defn_Given -> Phantoms.TTerm [Meta.Mod] -> Phantoms.TTerm Meta.Defn_Given)
defn_GivenWithMods original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Given"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "mods"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Given"),
          Core.projectionField = (Core.Name "name")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "tparams"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Given"),
          Core.projectionField = (Core.Name "tparams")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "sparams"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Given"),
          Core.projectionField = (Core.Name "sparams")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "templ"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Given"),
          Core.projectionField = (Core.Name "templ")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

defn_GivenWithName :: (Phantoms.TTerm Meta.Defn_Given -> Phantoms.TTerm Meta.Name -> Phantoms.TTerm Meta.Defn_Given)
defn_GivenWithName original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Given"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "mods"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Given"),
          Core.projectionField = (Core.Name "mods")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "tparams"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Given"),
          Core.projectionField = (Core.Name "tparams")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "sparams"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Given"),
          Core.projectionField = (Core.Name "sparams")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "templ"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Given"),
          Core.projectionField = (Core.Name "templ")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

defn_GivenWithTparams :: (Phantoms.TTerm Meta.Defn_Given -> Phantoms.TTerm [[Meta.Type_Param]] -> Phantoms.TTerm Meta.Defn_Given)
defn_GivenWithTparams original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Given"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "mods"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Given"),
          Core.projectionField = (Core.Name "mods")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Given"),
          Core.projectionField = (Core.Name "name")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "tparams"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "sparams"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Given"),
          Core.projectionField = (Core.Name "sparams")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "templ"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Given"),
          Core.projectionField = (Core.Name "templ")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

defn_GivenWithSparams :: (Phantoms.TTerm Meta.Defn_Given -> Phantoms.TTerm [[Meta.Data_Param]] -> Phantoms.TTerm Meta.Defn_Given)
defn_GivenWithSparams original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Given"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "mods"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Given"),
          Core.projectionField = (Core.Name "mods")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Given"),
          Core.projectionField = (Core.Name "name")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "tparams"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Given"),
          Core.projectionField = (Core.Name "tparams")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "sparams"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "templ"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Given"),
          Core.projectionField = (Core.Name "templ")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

defn_GivenWithTempl :: (Phantoms.TTerm Meta.Defn_Given -> Phantoms.TTerm Meta.Template -> Phantoms.TTerm Meta.Defn_Given)
defn_GivenWithTempl original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Given"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "mods"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Given"),
          Core.projectionField = (Core.Name "mods")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Given"),
          Core.projectionField = (Core.Name "name")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "tparams"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Given"),
          Core.projectionField = (Core.Name "tparams")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "sparams"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Given"),
          Core.projectionField = (Core.Name "sparams")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "templ"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

defn_Enum :: (Phantoms.TTerm [Meta.Mod] -> Phantoms.TTerm Meta.Type_Name -> Phantoms.TTerm [Meta.Type_Param] -> Phantoms.TTerm Meta.Ctor_Primary -> Phantoms.TTerm Meta.Template -> Phantoms.TTerm Meta.Defn_Enum)
defn_Enum mods name tparams ctor template = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Enum"),
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
      Core.fieldTerm = (Phantoms.unTTerm template)}]})))

defn_EnumMods :: (Phantoms.TTerm Meta.Defn_Enum -> Phantoms.TTerm [Meta.Mod])
defn_EnumMods x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Enum"),
    Core.projectionField = (Core.Name "mods")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

defn_EnumName :: (Phantoms.TTerm Meta.Defn_Enum -> Phantoms.TTerm Meta.Type_Name)
defn_EnumName x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Enum"),
    Core.projectionField = (Core.Name "name")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

defn_EnumTparams :: (Phantoms.TTerm Meta.Defn_Enum -> Phantoms.TTerm [Meta.Type_Param])
defn_EnumTparams x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Enum"),
    Core.projectionField = (Core.Name "tparams")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

defn_EnumCtor :: (Phantoms.TTerm Meta.Defn_Enum -> Phantoms.TTerm Meta.Ctor_Primary)
defn_EnumCtor x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Enum"),
    Core.projectionField = (Core.Name "ctor")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

defn_EnumTemplate :: (Phantoms.TTerm Meta.Defn_Enum -> Phantoms.TTerm Meta.Template)
defn_EnumTemplate x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Enum"),
    Core.projectionField = (Core.Name "template")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

defn_EnumWithMods :: (Phantoms.TTerm Meta.Defn_Enum -> Phantoms.TTerm [Meta.Mod] -> Phantoms.TTerm Meta.Defn_Enum)
defn_EnumWithMods original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Enum"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "mods"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Enum"),
          Core.projectionField = (Core.Name "name")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "tparams"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Enum"),
          Core.projectionField = (Core.Name "tparams")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "ctor"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Enum"),
          Core.projectionField = (Core.Name "ctor")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "template"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Enum"),
          Core.projectionField = (Core.Name "template")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

defn_EnumWithName :: (Phantoms.TTerm Meta.Defn_Enum -> Phantoms.TTerm Meta.Type_Name -> Phantoms.TTerm Meta.Defn_Enum)
defn_EnumWithName original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Enum"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "mods"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Enum"),
          Core.projectionField = (Core.Name "mods")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "tparams"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Enum"),
          Core.projectionField = (Core.Name "tparams")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "ctor"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Enum"),
          Core.projectionField = (Core.Name "ctor")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "template"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Enum"),
          Core.projectionField = (Core.Name "template")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

defn_EnumWithTparams :: (Phantoms.TTerm Meta.Defn_Enum -> Phantoms.TTerm [Meta.Type_Param] -> Phantoms.TTerm Meta.Defn_Enum)
defn_EnumWithTparams original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Enum"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "mods"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Enum"),
          Core.projectionField = (Core.Name "mods")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Enum"),
          Core.projectionField = (Core.Name "name")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "tparams"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "ctor"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Enum"),
          Core.projectionField = (Core.Name "ctor")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "template"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Enum"),
          Core.projectionField = (Core.Name "template")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

defn_EnumWithCtor :: (Phantoms.TTerm Meta.Defn_Enum -> Phantoms.TTerm Meta.Ctor_Primary -> Phantoms.TTerm Meta.Defn_Enum)
defn_EnumWithCtor original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Enum"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "mods"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Enum"),
          Core.projectionField = (Core.Name "mods")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Enum"),
          Core.projectionField = (Core.Name "name")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "tparams"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Enum"),
          Core.projectionField = (Core.Name "tparams")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "ctor"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "template"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Enum"),
          Core.projectionField = (Core.Name "template")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

defn_EnumWithTemplate :: (Phantoms.TTerm Meta.Defn_Enum -> Phantoms.TTerm Meta.Template -> Phantoms.TTerm Meta.Defn_Enum)
defn_EnumWithTemplate original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Enum"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "mods"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Enum"),
          Core.projectionField = (Core.Name "mods")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Enum"),
          Core.projectionField = (Core.Name "name")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "tparams"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Enum"),
          Core.projectionField = (Core.Name "tparams")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "ctor"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Enum"),
          Core.projectionField = (Core.Name "ctor")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "template"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

defn_EnumCase :: (Phantoms.TTerm [Meta.Mod] -> Phantoms.TTerm Meta.Data_Name -> Phantoms.TTerm [Meta.Type_Param] -> Phantoms.TTerm Meta.Ctor_Primary -> Phantoms.TTerm [Meta.Init] -> Phantoms.TTerm Meta.Defn_EnumCase)
defn_EnumCase mods name tparams ctor inits = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Defn_EnumCase"),
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
      Core.fieldTerm = (Phantoms.unTTerm inits)}]})))

defn_EnumCaseMods :: (Phantoms.TTerm Meta.Defn_EnumCase -> Phantoms.TTerm [Meta.Mod])
defn_EnumCaseMods x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_EnumCase"),
    Core.projectionField = (Core.Name "mods")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

defn_EnumCaseName :: (Phantoms.TTerm Meta.Defn_EnumCase -> Phantoms.TTerm Meta.Data_Name)
defn_EnumCaseName x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_EnumCase"),
    Core.projectionField = (Core.Name "name")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

defn_EnumCaseTparams :: (Phantoms.TTerm Meta.Defn_EnumCase -> Phantoms.TTerm [Meta.Type_Param])
defn_EnumCaseTparams x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_EnumCase"),
    Core.projectionField = (Core.Name "tparams")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

defn_EnumCaseCtor :: (Phantoms.TTerm Meta.Defn_EnumCase -> Phantoms.TTerm Meta.Ctor_Primary)
defn_EnumCaseCtor x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_EnumCase"),
    Core.projectionField = (Core.Name "ctor")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

defn_EnumCaseInits :: (Phantoms.TTerm Meta.Defn_EnumCase -> Phantoms.TTerm [Meta.Init])
defn_EnumCaseInits x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_EnumCase"),
    Core.projectionField = (Core.Name "inits")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

defn_EnumCaseWithMods :: (Phantoms.TTerm Meta.Defn_EnumCase -> Phantoms.TTerm [Meta.Mod] -> Phantoms.TTerm Meta.Defn_EnumCase)
defn_EnumCaseWithMods original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Defn_EnumCase"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "mods"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_EnumCase"),
          Core.projectionField = (Core.Name "name")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "tparams"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_EnumCase"),
          Core.projectionField = (Core.Name "tparams")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "ctor"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_EnumCase"),
          Core.projectionField = (Core.Name "ctor")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "inits"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_EnumCase"),
          Core.projectionField = (Core.Name "inits")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

defn_EnumCaseWithName :: (Phantoms.TTerm Meta.Defn_EnumCase -> Phantoms.TTerm Meta.Data_Name -> Phantoms.TTerm Meta.Defn_EnumCase)
defn_EnumCaseWithName original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Defn_EnumCase"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "mods"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_EnumCase"),
          Core.projectionField = (Core.Name "mods")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "tparams"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_EnumCase"),
          Core.projectionField = (Core.Name "tparams")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "ctor"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_EnumCase"),
          Core.projectionField = (Core.Name "ctor")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "inits"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_EnumCase"),
          Core.projectionField = (Core.Name "inits")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

defn_EnumCaseWithTparams :: (Phantoms.TTerm Meta.Defn_EnumCase -> Phantoms.TTerm [Meta.Type_Param] -> Phantoms.TTerm Meta.Defn_EnumCase)
defn_EnumCaseWithTparams original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Defn_EnumCase"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "mods"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_EnumCase"),
          Core.projectionField = (Core.Name "mods")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_EnumCase"),
          Core.projectionField = (Core.Name "name")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "tparams"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "ctor"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_EnumCase"),
          Core.projectionField = (Core.Name "ctor")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "inits"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_EnumCase"),
          Core.projectionField = (Core.Name "inits")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

defn_EnumCaseWithCtor :: (Phantoms.TTerm Meta.Defn_EnumCase -> Phantoms.TTerm Meta.Ctor_Primary -> Phantoms.TTerm Meta.Defn_EnumCase)
defn_EnumCaseWithCtor original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Defn_EnumCase"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "mods"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_EnumCase"),
          Core.projectionField = (Core.Name "mods")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_EnumCase"),
          Core.projectionField = (Core.Name "name")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "tparams"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_EnumCase"),
          Core.projectionField = (Core.Name "tparams")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "ctor"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "inits"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_EnumCase"),
          Core.projectionField = (Core.Name "inits")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

defn_EnumCaseWithInits :: (Phantoms.TTerm Meta.Defn_EnumCase -> Phantoms.TTerm [Meta.Init] -> Phantoms.TTerm Meta.Defn_EnumCase)
defn_EnumCaseWithInits original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Defn_EnumCase"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "mods"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_EnumCase"),
          Core.projectionField = (Core.Name "mods")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_EnumCase"),
          Core.projectionField = (Core.Name "name")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "tparams"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_EnumCase"),
          Core.projectionField = (Core.Name "tparams")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "ctor"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_EnumCase"),
          Core.projectionField = (Core.Name "ctor")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "inits"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

defn_RepeatedEnumCase :: (Phantoms.TTerm [Meta.Mod] -> Phantoms.TTerm [Meta.Data_Name] -> Phantoms.TTerm Meta.Defn_RepeatedEnumCase)
defn_RepeatedEnumCase mods cases = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Defn_RepeatedEnumCase"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "mods"),
      Core.fieldTerm = (Phantoms.unTTerm mods)},
    Core.Field {
      Core.fieldName = (Core.Name "cases"),
      Core.fieldTerm = (Phantoms.unTTerm cases)}]})))

defn_RepeatedEnumCaseMods :: (Phantoms.TTerm Meta.Defn_RepeatedEnumCase -> Phantoms.TTerm [Meta.Mod])
defn_RepeatedEnumCaseMods x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_RepeatedEnumCase"),
    Core.projectionField = (Core.Name "mods")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

defn_RepeatedEnumCaseCases :: (Phantoms.TTerm Meta.Defn_RepeatedEnumCase -> Phantoms.TTerm [Meta.Data_Name])
defn_RepeatedEnumCaseCases x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_RepeatedEnumCase"),
    Core.projectionField = (Core.Name "cases")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

defn_RepeatedEnumCaseWithMods :: (Phantoms.TTerm Meta.Defn_RepeatedEnumCase -> Phantoms.TTerm [Meta.Mod] -> Phantoms.TTerm Meta.Defn_RepeatedEnumCase)
defn_RepeatedEnumCaseWithMods original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Defn_RepeatedEnumCase"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "mods"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "cases"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_RepeatedEnumCase"),
          Core.projectionField = (Core.Name "cases")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

defn_RepeatedEnumCaseWithCases :: (Phantoms.TTerm Meta.Defn_RepeatedEnumCase -> Phantoms.TTerm [Meta.Data_Name] -> Phantoms.TTerm Meta.Defn_RepeatedEnumCase)
defn_RepeatedEnumCaseWithCases original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Defn_RepeatedEnumCase"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "mods"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_RepeatedEnumCase"),
          Core.projectionField = (Core.Name "mods")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "cases"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

defn_GivenAlias :: (Phantoms.TTerm [Meta.Mod] -> Phantoms.TTerm Meta.Name -> Phantoms.TTerm [[Meta.Type_Param]] -> Phantoms.TTerm [[Meta.Data_Param]] -> Phantoms.TTerm Meta.Type -> Phantoms.TTerm Meta.Data -> Phantoms.TTerm Meta.Defn_GivenAlias)
defn_GivenAlias mods name tparams sparams decltpe body = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Defn_GivenAlias"),
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
      Core.fieldTerm = (Phantoms.unTTerm body)}]})))

defn_GivenAliasMods :: (Phantoms.TTerm Meta.Defn_GivenAlias -> Phantoms.TTerm [Meta.Mod])
defn_GivenAliasMods x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_GivenAlias"),
    Core.projectionField = (Core.Name "mods")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

defn_GivenAliasName :: (Phantoms.TTerm Meta.Defn_GivenAlias -> Phantoms.TTerm Meta.Name)
defn_GivenAliasName x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_GivenAlias"),
    Core.projectionField = (Core.Name "name")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

defn_GivenAliasTparams :: (Phantoms.TTerm Meta.Defn_GivenAlias -> Phantoms.TTerm [[Meta.Type_Param]])
defn_GivenAliasTparams x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_GivenAlias"),
    Core.projectionField = (Core.Name "tparams")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

defn_GivenAliasSparams :: (Phantoms.TTerm Meta.Defn_GivenAlias -> Phantoms.TTerm [[Meta.Data_Param]])
defn_GivenAliasSparams x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_GivenAlias"),
    Core.projectionField = (Core.Name "sparams")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

defn_GivenAliasDecltpe :: (Phantoms.TTerm Meta.Defn_GivenAlias -> Phantoms.TTerm Meta.Type)
defn_GivenAliasDecltpe x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_GivenAlias"),
    Core.projectionField = (Core.Name "decltpe")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

defn_GivenAliasBody :: (Phantoms.TTerm Meta.Defn_GivenAlias -> Phantoms.TTerm Meta.Data)
defn_GivenAliasBody x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_GivenAlias"),
    Core.projectionField = (Core.Name "body")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

defn_GivenAliasWithMods :: (Phantoms.TTerm Meta.Defn_GivenAlias -> Phantoms.TTerm [Meta.Mod] -> Phantoms.TTerm Meta.Defn_GivenAlias)
defn_GivenAliasWithMods original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Defn_GivenAlias"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "mods"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_GivenAlias"),
          Core.projectionField = (Core.Name "name")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "tparams"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_GivenAlias"),
          Core.projectionField = (Core.Name "tparams")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "sparams"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_GivenAlias"),
          Core.projectionField = (Core.Name "sparams")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "decltpe"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_GivenAlias"),
          Core.projectionField = (Core.Name "decltpe")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "body"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_GivenAlias"),
          Core.projectionField = (Core.Name "body")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

defn_GivenAliasWithName :: (Phantoms.TTerm Meta.Defn_GivenAlias -> Phantoms.TTerm Meta.Name -> Phantoms.TTerm Meta.Defn_GivenAlias)
defn_GivenAliasWithName original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Defn_GivenAlias"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "mods"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_GivenAlias"),
          Core.projectionField = (Core.Name "mods")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "tparams"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_GivenAlias"),
          Core.projectionField = (Core.Name "tparams")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "sparams"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_GivenAlias"),
          Core.projectionField = (Core.Name "sparams")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "decltpe"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_GivenAlias"),
          Core.projectionField = (Core.Name "decltpe")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "body"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_GivenAlias"),
          Core.projectionField = (Core.Name "body")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

defn_GivenAliasWithTparams :: (Phantoms.TTerm Meta.Defn_GivenAlias -> Phantoms.TTerm [[Meta.Type_Param]] -> Phantoms.TTerm Meta.Defn_GivenAlias)
defn_GivenAliasWithTparams original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Defn_GivenAlias"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "mods"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_GivenAlias"),
          Core.projectionField = (Core.Name "mods")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_GivenAlias"),
          Core.projectionField = (Core.Name "name")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "tparams"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "sparams"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_GivenAlias"),
          Core.projectionField = (Core.Name "sparams")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "decltpe"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_GivenAlias"),
          Core.projectionField = (Core.Name "decltpe")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "body"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_GivenAlias"),
          Core.projectionField = (Core.Name "body")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

defn_GivenAliasWithSparams :: (Phantoms.TTerm Meta.Defn_GivenAlias -> Phantoms.TTerm [[Meta.Data_Param]] -> Phantoms.TTerm Meta.Defn_GivenAlias)
defn_GivenAliasWithSparams original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Defn_GivenAlias"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "mods"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_GivenAlias"),
          Core.projectionField = (Core.Name "mods")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_GivenAlias"),
          Core.projectionField = (Core.Name "name")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "tparams"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_GivenAlias"),
          Core.projectionField = (Core.Name "tparams")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "sparams"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "decltpe"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_GivenAlias"),
          Core.projectionField = (Core.Name "decltpe")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "body"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_GivenAlias"),
          Core.projectionField = (Core.Name "body")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

defn_GivenAliasWithDecltpe :: (Phantoms.TTerm Meta.Defn_GivenAlias -> Phantoms.TTerm Meta.Type -> Phantoms.TTerm Meta.Defn_GivenAlias)
defn_GivenAliasWithDecltpe original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Defn_GivenAlias"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "mods"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_GivenAlias"),
          Core.projectionField = (Core.Name "mods")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_GivenAlias"),
          Core.projectionField = (Core.Name "name")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "tparams"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_GivenAlias"),
          Core.projectionField = (Core.Name "tparams")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "sparams"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_GivenAlias"),
          Core.projectionField = (Core.Name "sparams")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "decltpe"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "body"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_GivenAlias"),
          Core.projectionField = (Core.Name "body")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

defn_GivenAliasWithBody :: (Phantoms.TTerm Meta.Defn_GivenAlias -> Phantoms.TTerm Meta.Data -> Phantoms.TTerm Meta.Defn_GivenAlias)
defn_GivenAliasWithBody original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Defn_GivenAlias"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "mods"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_GivenAlias"),
          Core.projectionField = (Core.Name "mods")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_GivenAlias"),
          Core.projectionField = (Core.Name "name")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "tparams"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_GivenAlias"),
          Core.projectionField = (Core.Name "tparams")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "sparams"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_GivenAlias"),
          Core.projectionField = (Core.Name "sparams")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "decltpe"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_GivenAlias"),
          Core.projectionField = (Core.Name "decltpe")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "body"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

defn_ExtensionGroup :: (Phantoms.TTerm [Meta.Type_Param] -> Phantoms.TTerm [[Meta.Data_Param]] -> Phantoms.TTerm Meta.Stat -> Phantoms.TTerm Meta.Defn_ExtensionGroup)
defn_ExtensionGroup tparams parmss body = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Defn_ExtensionGroup"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "tparams"),
      Core.fieldTerm = (Phantoms.unTTerm tparams)},
    Core.Field {
      Core.fieldName = (Core.Name "parmss"),
      Core.fieldTerm = (Phantoms.unTTerm parmss)},
    Core.Field {
      Core.fieldName = (Core.Name "body"),
      Core.fieldTerm = (Phantoms.unTTerm body)}]})))

defn_ExtensionGroupTparams :: (Phantoms.TTerm Meta.Defn_ExtensionGroup -> Phantoms.TTerm [Meta.Type_Param])
defn_ExtensionGroupTparams x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_ExtensionGroup"),
    Core.projectionField = (Core.Name "tparams")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

defn_ExtensionGroupParmss :: (Phantoms.TTerm Meta.Defn_ExtensionGroup -> Phantoms.TTerm [[Meta.Data_Param]])
defn_ExtensionGroupParmss x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_ExtensionGroup"),
    Core.projectionField = (Core.Name "parmss")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

defn_ExtensionGroupBody :: (Phantoms.TTerm Meta.Defn_ExtensionGroup -> Phantoms.TTerm Meta.Stat)
defn_ExtensionGroupBody x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_ExtensionGroup"),
    Core.projectionField = (Core.Name "body")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

defn_ExtensionGroupWithTparams :: (Phantoms.TTerm Meta.Defn_ExtensionGroup -> Phantoms.TTerm [Meta.Type_Param] -> Phantoms.TTerm Meta.Defn_ExtensionGroup)
defn_ExtensionGroupWithTparams original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Defn_ExtensionGroup"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "tparams"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "parmss"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_ExtensionGroup"),
          Core.projectionField = (Core.Name "parmss")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "body"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_ExtensionGroup"),
          Core.projectionField = (Core.Name "body")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

defn_ExtensionGroupWithParmss :: (Phantoms.TTerm Meta.Defn_ExtensionGroup -> Phantoms.TTerm [[Meta.Data_Param]] -> Phantoms.TTerm Meta.Defn_ExtensionGroup)
defn_ExtensionGroupWithParmss original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Defn_ExtensionGroup"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "tparams"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_ExtensionGroup"),
          Core.projectionField = (Core.Name "tparams")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "parmss"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "body"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_ExtensionGroup"),
          Core.projectionField = (Core.Name "body")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

defn_ExtensionGroupWithBody :: (Phantoms.TTerm Meta.Defn_ExtensionGroup -> Phantoms.TTerm Meta.Stat -> Phantoms.TTerm Meta.Defn_ExtensionGroup)
defn_ExtensionGroupWithBody original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Defn_ExtensionGroup"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "tparams"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_ExtensionGroup"),
          Core.projectionField = (Core.Name "tparams")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "parmss"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_ExtensionGroup"),
          Core.projectionField = (Core.Name "parmss")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "body"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

defn_Def :: (Phantoms.TTerm [Meta.Mod] -> Phantoms.TTerm Meta.Data_Name -> Phantoms.TTerm [Meta.Type_Param] -> Phantoms.TTerm [[Meta.Data_Param]] -> Phantoms.TTerm (Maybe Meta.Type) -> Phantoms.TTerm Meta.Data -> Phantoms.TTerm Meta.Defn_Def)
defn_Def mods name tparams paramss decltpe body = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Def"),
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
      Core.fieldTerm = (Phantoms.unTTerm body)}]})))

defn_DefMods :: (Phantoms.TTerm Meta.Defn_Def -> Phantoms.TTerm [Meta.Mod])
defn_DefMods x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Def"),
    Core.projectionField = (Core.Name "mods")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

defn_DefName :: (Phantoms.TTerm Meta.Defn_Def -> Phantoms.TTerm Meta.Data_Name)
defn_DefName x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Def"),
    Core.projectionField = (Core.Name "name")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

defn_DefTparams :: (Phantoms.TTerm Meta.Defn_Def -> Phantoms.TTerm [Meta.Type_Param])
defn_DefTparams x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Def"),
    Core.projectionField = (Core.Name "tparams")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

defn_DefParamss :: (Phantoms.TTerm Meta.Defn_Def -> Phantoms.TTerm [[Meta.Data_Param]])
defn_DefParamss x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Def"),
    Core.projectionField = (Core.Name "paramss")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

defn_DefDecltpe :: (Phantoms.TTerm Meta.Defn_Def -> Phantoms.TTerm (Maybe Meta.Type))
defn_DefDecltpe x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Def"),
    Core.projectionField = (Core.Name "decltpe")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

defn_DefBody :: (Phantoms.TTerm Meta.Defn_Def -> Phantoms.TTerm Meta.Data)
defn_DefBody x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Def"),
    Core.projectionField = (Core.Name "body")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

defn_DefWithMods :: (Phantoms.TTerm Meta.Defn_Def -> Phantoms.TTerm [Meta.Mod] -> Phantoms.TTerm Meta.Defn_Def)
defn_DefWithMods original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Def"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "mods"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Def"),
          Core.projectionField = (Core.Name "name")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "tparams"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Def"),
          Core.projectionField = (Core.Name "tparams")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "paramss"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Def"),
          Core.projectionField = (Core.Name "paramss")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "decltpe"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Def"),
          Core.projectionField = (Core.Name "decltpe")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "body"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Def"),
          Core.projectionField = (Core.Name "body")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

defn_DefWithName :: (Phantoms.TTerm Meta.Defn_Def -> Phantoms.TTerm Meta.Data_Name -> Phantoms.TTerm Meta.Defn_Def)
defn_DefWithName original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Def"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "mods"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Def"),
          Core.projectionField = (Core.Name "mods")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "tparams"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Def"),
          Core.projectionField = (Core.Name "tparams")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "paramss"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Def"),
          Core.projectionField = (Core.Name "paramss")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "decltpe"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Def"),
          Core.projectionField = (Core.Name "decltpe")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "body"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Def"),
          Core.projectionField = (Core.Name "body")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

defn_DefWithTparams :: (Phantoms.TTerm Meta.Defn_Def -> Phantoms.TTerm [Meta.Type_Param] -> Phantoms.TTerm Meta.Defn_Def)
defn_DefWithTparams original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Def"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "mods"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Def"),
          Core.projectionField = (Core.Name "mods")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Def"),
          Core.projectionField = (Core.Name "name")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "tparams"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "paramss"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Def"),
          Core.projectionField = (Core.Name "paramss")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "decltpe"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Def"),
          Core.projectionField = (Core.Name "decltpe")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "body"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Def"),
          Core.projectionField = (Core.Name "body")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

defn_DefWithParamss :: (Phantoms.TTerm Meta.Defn_Def -> Phantoms.TTerm [[Meta.Data_Param]] -> Phantoms.TTerm Meta.Defn_Def)
defn_DefWithParamss original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Def"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "mods"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Def"),
          Core.projectionField = (Core.Name "mods")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Def"),
          Core.projectionField = (Core.Name "name")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "tparams"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Def"),
          Core.projectionField = (Core.Name "tparams")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "paramss"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "decltpe"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Def"),
          Core.projectionField = (Core.Name "decltpe")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "body"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Def"),
          Core.projectionField = (Core.Name "body")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

defn_DefWithDecltpe :: (Phantoms.TTerm Meta.Defn_Def -> Phantoms.TTerm (Maybe Meta.Type) -> Phantoms.TTerm Meta.Defn_Def)
defn_DefWithDecltpe original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Def"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "mods"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Def"),
          Core.projectionField = (Core.Name "mods")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Def"),
          Core.projectionField = (Core.Name "name")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "tparams"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Def"),
          Core.projectionField = (Core.Name "tparams")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "paramss"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Def"),
          Core.projectionField = (Core.Name "paramss")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "decltpe"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "body"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Def"),
          Core.projectionField = (Core.Name "body")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

defn_DefWithBody :: (Phantoms.TTerm Meta.Defn_Def -> Phantoms.TTerm Meta.Data -> Phantoms.TTerm Meta.Defn_Def)
defn_DefWithBody original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Def"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "mods"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Def"),
          Core.projectionField = (Core.Name "mods")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Def"),
          Core.projectionField = (Core.Name "name")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "tparams"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Def"),
          Core.projectionField = (Core.Name "tparams")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "paramss"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Def"),
          Core.projectionField = (Core.Name "paramss")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "decltpe"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Def"),
          Core.projectionField = (Core.Name "decltpe")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "body"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

defn_Macro :: (Phantoms.TTerm [Meta.Mod] -> Phantoms.TTerm Meta.Data_Name -> Phantoms.TTerm [Meta.Type_Param] -> Phantoms.TTerm [[Meta.Data_Param]] -> Phantoms.TTerm (Maybe Meta.Type) -> Phantoms.TTerm Meta.Data -> Phantoms.TTerm Meta.Defn_Macro)
defn_Macro mods name tparams paramss decltpe body = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Macro"),
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
      Core.fieldTerm = (Phantoms.unTTerm body)}]})))

defn_MacroMods :: (Phantoms.TTerm Meta.Defn_Macro -> Phantoms.TTerm [Meta.Mod])
defn_MacroMods x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Macro"),
    Core.projectionField = (Core.Name "mods")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

defn_MacroName :: (Phantoms.TTerm Meta.Defn_Macro -> Phantoms.TTerm Meta.Data_Name)
defn_MacroName x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Macro"),
    Core.projectionField = (Core.Name "name")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

defn_MacroTparams :: (Phantoms.TTerm Meta.Defn_Macro -> Phantoms.TTerm [Meta.Type_Param])
defn_MacroTparams x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Macro"),
    Core.projectionField = (Core.Name "tparams")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

defn_MacroParamss :: (Phantoms.TTerm Meta.Defn_Macro -> Phantoms.TTerm [[Meta.Data_Param]])
defn_MacroParamss x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Macro"),
    Core.projectionField = (Core.Name "paramss")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

defn_MacroDecltpe :: (Phantoms.TTerm Meta.Defn_Macro -> Phantoms.TTerm (Maybe Meta.Type))
defn_MacroDecltpe x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Macro"),
    Core.projectionField = (Core.Name "decltpe")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

defn_MacroBody :: (Phantoms.TTerm Meta.Defn_Macro -> Phantoms.TTerm Meta.Data)
defn_MacroBody x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Macro"),
    Core.projectionField = (Core.Name "body")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

defn_MacroWithMods :: (Phantoms.TTerm Meta.Defn_Macro -> Phantoms.TTerm [Meta.Mod] -> Phantoms.TTerm Meta.Defn_Macro)
defn_MacroWithMods original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Macro"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "mods"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Macro"),
          Core.projectionField = (Core.Name "name")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "tparams"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Macro"),
          Core.projectionField = (Core.Name "tparams")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "paramss"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Macro"),
          Core.projectionField = (Core.Name "paramss")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "decltpe"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Macro"),
          Core.projectionField = (Core.Name "decltpe")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "body"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Macro"),
          Core.projectionField = (Core.Name "body")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

defn_MacroWithName :: (Phantoms.TTerm Meta.Defn_Macro -> Phantoms.TTerm Meta.Data_Name -> Phantoms.TTerm Meta.Defn_Macro)
defn_MacroWithName original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Macro"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "mods"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Macro"),
          Core.projectionField = (Core.Name "mods")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "tparams"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Macro"),
          Core.projectionField = (Core.Name "tparams")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "paramss"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Macro"),
          Core.projectionField = (Core.Name "paramss")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "decltpe"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Macro"),
          Core.projectionField = (Core.Name "decltpe")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "body"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Macro"),
          Core.projectionField = (Core.Name "body")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

defn_MacroWithTparams :: (Phantoms.TTerm Meta.Defn_Macro -> Phantoms.TTerm [Meta.Type_Param] -> Phantoms.TTerm Meta.Defn_Macro)
defn_MacroWithTparams original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Macro"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "mods"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Macro"),
          Core.projectionField = (Core.Name "mods")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Macro"),
          Core.projectionField = (Core.Name "name")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "tparams"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "paramss"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Macro"),
          Core.projectionField = (Core.Name "paramss")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "decltpe"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Macro"),
          Core.projectionField = (Core.Name "decltpe")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "body"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Macro"),
          Core.projectionField = (Core.Name "body")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

defn_MacroWithParamss :: (Phantoms.TTerm Meta.Defn_Macro -> Phantoms.TTerm [[Meta.Data_Param]] -> Phantoms.TTerm Meta.Defn_Macro)
defn_MacroWithParamss original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Macro"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "mods"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Macro"),
          Core.projectionField = (Core.Name "mods")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Macro"),
          Core.projectionField = (Core.Name "name")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "tparams"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Macro"),
          Core.projectionField = (Core.Name "tparams")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "paramss"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "decltpe"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Macro"),
          Core.projectionField = (Core.Name "decltpe")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "body"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Macro"),
          Core.projectionField = (Core.Name "body")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

defn_MacroWithDecltpe :: (Phantoms.TTerm Meta.Defn_Macro -> Phantoms.TTerm (Maybe Meta.Type) -> Phantoms.TTerm Meta.Defn_Macro)
defn_MacroWithDecltpe original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Macro"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "mods"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Macro"),
          Core.projectionField = (Core.Name "mods")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Macro"),
          Core.projectionField = (Core.Name "name")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "tparams"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Macro"),
          Core.projectionField = (Core.Name "tparams")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "paramss"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Macro"),
          Core.projectionField = (Core.Name "paramss")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "decltpe"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "body"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Macro"),
          Core.projectionField = (Core.Name "body")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

defn_MacroWithBody :: (Phantoms.TTerm Meta.Defn_Macro -> Phantoms.TTerm Meta.Data -> Phantoms.TTerm Meta.Defn_Macro)
defn_MacroWithBody original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Macro"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "mods"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Macro"),
          Core.projectionField = (Core.Name "mods")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Macro"),
          Core.projectionField = (Core.Name "name")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "tparams"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Macro"),
          Core.projectionField = (Core.Name "tparams")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "paramss"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Macro"),
          Core.projectionField = (Core.Name "paramss")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "decltpe"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Macro"),
          Core.projectionField = (Core.Name "decltpe")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "body"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

defn_Type :: (Phantoms.TTerm [Meta.Mod] -> Phantoms.TTerm Meta.Type_Name -> Phantoms.TTerm [Meta.Type_Param] -> Phantoms.TTerm Meta.Type -> Phantoms.TTerm Meta.Defn_Type)
defn_Type mods name tparams body = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Type"),
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
      Core.fieldTerm = (Phantoms.unTTerm body)}]})))

defn_TypeMods :: (Phantoms.TTerm Meta.Defn_Type -> Phantoms.TTerm [Meta.Mod])
defn_TypeMods x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Type"),
    Core.projectionField = (Core.Name "mods")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

defn_TypeName :: (Phantoms.TTerm Meta.Defn_Type -> Phantoms.TTerm Meta.Type_Name)
defn_TypeName x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Type"),
    Core.projectionField = (Core.Name "name")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

defn_TypeTparams :: (Phantoms.TTerm Meta.Defn_Type -> Phantoms.TTerm [Meta.Type_Param])
defn_TypeTparams x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Type"),
    Core.projectionField = (Core.Name "tparams")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

defn_TypeBody :: (Phantoms.TTerm Meta.Defn_Type -> Phantoms.TTerm Meta.Type)
defn_TypeBody x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Type"),
    Core.projectionField = (Core.Name "body")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

defn_TypeWithMods :: (Phantoms.TTerm Meta.Defn_Type -> Phantoms.TTerm [Meta.Mod] -> Phantoms.TTerm Meta.Defn_Type)
defn_TypeWithMods original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Type"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "mods"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Type"),
          Core.projectionField = (Core.Name "name")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "tparams"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Type"),
          Core.projectionField = (Core.Name "tparams")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "body"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Type"),
          Core.projectionField = (Core.Name "body")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

defn_TypeWithName :: (Phantoms.TTerm Meta.Defn_Type -> Phantoms.TTerm Meta.Type_Name -> Phantoms.TTerm Meta.Defn_Type)
defn_TypeWithName original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Type"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "mods"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Type"),
          Core.projectionField = (Core.Name "mods")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "tparams"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Type"),
          Core.projectionField = (Core.Name "tparams")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "body"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Type"),
          Core.projectionField = (Core.Name "body")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

defn_TypeWithTparams :: (Phantoms.TTerm Meta.Defn_Type -> Phantoms.TTerm [Meta.Type_Param] -> Phantoms.TTerm Meta.Defn_Type)
defn_TypeWithTparams original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Type"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "mods"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Type"),
          Core.projectionField = (Core.Name "mods")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Type"),
          Core.projectionField = (Core.Name "name")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "tparams"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "body"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Type"),
          Core.projectionField = (Core.Name "body")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

defn_TypeWithBody :: (Phantoms.TTerm Meta.Defn_Type -> Phantoms.TTerm Meta.Type -> Phantoms.TTerm Meta.Defn_Type)
defn_TypeWithBody original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Type"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "mods"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Type"),
          Core.projectionField = (Core.Name "mods")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Type"),
          Core.projectionField = (Core.Name "name")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "tparams"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Type"),
          Core.projectionField = (Core.Name "tparams")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "body"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

defn_Class :: (Phantoms.TTerm [Meta.Mod] -> Phantoms.TTerm Meta.Type_Name -> Phantoms.TTerm [Meta.Type_Param] -> Phantoms.TTerm Meta.Ctor_Primary -> Phantoms.TTerm Meta.Template -> Phantoms.TTerm Meta.Defn_Class)
defn_Class mods name tparams ctor template = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Class"),
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
      Core.fieldTerm = (Phantoms.unTTerm template)}]})))

defn_ClassMods :: (Phantoms.TTerm Meta.Defn_Class -> Phantoms.TTerm [Meta.Mod])
defn_ClassMods x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Class"),
    Core.projectionField = (Core.Name "mods")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

defn_ClassName :: (Phantoms.TTerm Meta.Defn_Class -> Phantoms.TTerm Meta.Type_Name)
defn_ClassName x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Class"),
    Core.projectionField = (Core.Name "name")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

defn_ClassTparams :: (Phantoms.TTerm Meta.Defn_Class -> Phantoms.TTerm [Meta.Type_Param])
defn_ClassTparams x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Class"),
    Core.projectionField = (Core.Name "tparams")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

defn_ClassCtor :: (Phantoms.TTerm Meta.Defn_Class -> Phantoms.TTerm Meta.Ctor_Primary)
defn_ClassCtor x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Class"),
    Core.projectionField = (Core.Name "ctor")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

defn_ClassTemplate :: (Phantoms.TTerm Meta.Defn_Class -> Phantoms.TTerm Meta.Template)
defn_ClassTemplate x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Class"),
    Core.projectionField = (Core.Name "template")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

defn_ClassWithMods :: (Phantoms.TTerm Meta.Defn_Class -> Phantoms.TTerm [Meta.Mod] -> Phantoms.TTerm Meta.Defn_Class)
defn_ClassWithMods original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Class"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "mods"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Class"),
          Core.projectionField = (Core.Name "name")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "tparams"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Class"),
          Core.projectionField = (Core.Name "tparams")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "ctor"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Class"),
          Core.projectionField = (Core.Name "ctor")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "template"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Class"),
          Core.projectionField = (Core.Name "template")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

defn_ClassWithName :: (Phantoms.TTerm Meta.Defn_Class -> Phantoms.TTerm Meta.Type_Name -> Phantoms.TTerm Meta.Defn_Class)
defn_ClassWithName original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Class"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "mods"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Class"),
          Core.projectionField = (Core.Name "mods")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "tparams"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Class"),
          Core.projectionField = (Core.Name "tparams")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "ctor"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Class"),
          Core.projectionField = (Core.Name "ctor")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "template"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Class"),
          Core.projectionField = (Core.Name "template")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

defn_ClassWithTparams :: (Phantoms.TTerm Meta.Defn_Class -> Phantoms.TTerm [Meta.Type_Param] -> Phantoms.TTerm Meta.Defn_Class)
defn_ClassWithTparams original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Class"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "mods"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Class"),
          Core.projectionField = (Core.Name "mods")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Class"),
          Core.projectionField = (Core.Name "name")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "tparams"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "ctor"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Class"),
          Core.projectionField = (Core.Name "ctor")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "template"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Class"),
          Core.projectionField = (Core.Name "template")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

defn_ClassWithCtor :: (Phantoms.TTerm Meta.Defn_Class -> Phantoms.TTerm Meta.Ctor_Primary -> Phantoms.TTerm Meta.Defn_Class)
defn_ClassWithCtor original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Class"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "mods"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Class"),
          Core.projectionField = (Core.Name "mods")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Class"),
          Core.projectionField = (Core.Name "name")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "tparams"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Class"),
          Core.projectionField = (Core.Name "tparams")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "ctor"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "template"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Class"),
          Core.projectionField = (Core.Name "template")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

defn_ClassWithTemplate :: (Phantoms.TTerm Meta.Defn_Class -> Phantoms.TTerm Meta.Template -> Phantoms.TTerm Meta.Defn_Class)
defn_ClassWithTemplate original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Class"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "mods"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Class"),
          Core.projectionField = (Core.Name "mods")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Class"),
          Core.projectionField = (Core.Name "name")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "tparams"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Class"),
          Core.projectionField = (Core.Name "tparams")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "ctor"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Class"),
          Core.projectionField = (Core.Name "ctor")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "template"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

defn_Trait :: (Phantoms.TTerm [Meta.Mod] -> Phantoms.TTerm Meta.Type_Name -> Phantoms.TTerm [Meta.Type_Param] -> Phantoms.TTerm Meta.Ctor_Primary -> Phantoms.TTerm Meta.Template -> Phantoms.TTerm Meta.Defn_Trait)
defn_Trait mods name tparams ctor template = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Trait"),
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
      Core.fieldTerm = (Phantoms.unTTerm template)}]})))

defn_TraitMods :: (Phantoms.TTerm Meta.Defn_Trait -> Phantoms.TTerm [Meta.Mod])
defn_TraitMods x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Trait"),
    Core.projectionField = (Core.Name "mods")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

defn_TraitName :: (Phantoms.TTerm Meta.Defn_Trait -> Phantoms.TTerm Meta.Type_Name)
defn_TraitName x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Trait"),
    Core.projectionField = (Core.Name "name")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

defn_TraitTparams :: (Phantoms.TTerm Meta.Defn_Trait -> Phantoms.TTerm [Meta.Type_Param])
defn_TraitTparams x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Trait"),
    Core.projectionField = (Core.Name "tparams")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

defn_TraitCtor :: (Phantoms.TTerm Meta.Defn_Trait -> Phantoms.TTerm Meta.Ctor_Primary)
defn_TraitCtor x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Trait"),
    Core.projectionField = (Core.Name "ctor")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

defn_TraitTemplate :: (Phantoms.TTerm Meta.Defn_Trait -> Phantoms.TTerm Meta.Template)
defn_TraitTemplate x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Trait"),
    Core.projectionField = (Core.Name "template")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

defn_TraitWithMods :: (Phantoms.TTerm Meta.Defn_Trait -> Phantoms.TTerm [Meta.Mod] -> Phantoms.TTerm Meta.Defn_Trait)
defn_TraitWithMods original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Trait"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "mods"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Trait"),
          Core.projectionField = (Core.Name "name")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "tparams"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Trait"),
          Core.projectionField = (Core.Name "tparams")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "ctor"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Trait"),
          Core.projectionField = (Core.Name "ctor")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "template"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Trait"),
          Core.projectionField = (Core.Name "template")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

defn_TraitWithName :: (Phantoms.TTerm Meta.Defn_Trait -> Phantoms.TTerm Meta.Type_Name -> Phantoms.TTerm Meta.Defn_Trait)
defn_TraitWithName original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Trait"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "mods"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Trait"),
          Core.projectionField = (Core.Name "mods")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "tparams"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Trait"),
          Core.projectionField = (Core.Name "tparams")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "ctor"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Trait"),
          Core.projectionField = (Core.Name "ctor")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "template"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Trait"),
          Core.projectionField = (Core.Name "template")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

defn_TraitWithTparams :: (Phantoms.TTerm Meta.Defn_Trait -> Phantoms.TTerm [Meta.Type_Param] -> Phantoms.TTerm Meta.Defn_Trait)
defn_TraitWithTparams original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Trait"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "mods"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Trait"),
          Core.projectionField = (Core.Name "mods")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Trait"),
          Core.projectionField = (Core.Name "name")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "tparams"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "ctor"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Trait"),
          Core.projectionField = (Core.Name "ctor")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "template"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Trait"),
          Core.projectionField = (Core.Name "template")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

defn_TraitWithCtor :: (Phantoms.TTerm Meta.Defn_Trait -> Phantoms.TTerm Meta.Ctor_Primary -> Phantoms.TTerm Meta.Defn_Trait)
defn_TraitWithCtor original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Trait"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "mods"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Trait"),
          Core.projectionField = (Core.Name "mods")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Trait"),
          Core.projectionField = (Core.Name "name")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "tparams"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Trait"),
          Core.projectionField = (Core.Name "tparams")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "ctor"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "template"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Trait"),
          Core.projectionField = (Core.Name "template")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

defn_TraitWithTemplate :: (Phantoms.TTerm Meta.Defn_Trait -> Phantoms.TTerm Meta.Template -> Phantoms.TTerm Meta.Defn_Trait)
defn_TraitWithTemplate original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Trait"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "mods"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Trait"),
          Core.projectionField = (Core.Name "mods")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Trait"),
          Core.projectionField = (Core.Name "name")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "tparams"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Trait"),
          Core.projectionField = (Core.Name "tparams")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "ctor"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Trait"),
          Core.projectionField = (Core.Name "ctor")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "template"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

defn_Object :: (Phantoms.TTerm Meta.Data_Name -> Phantoms.TTerm Meta.Defn_Object)
defn_Object name = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Object"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Phantoms.unTTerm name)}]})))

defn_ObjectName :: (Phantoms.TTerm Meta.Defn_Object -> Phantoms.TTerm Meta.Data_Name)
defn_ObjectName x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Object"),
    Core.projectionField = (Core.Name "name")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

defn_ObjectWithName :: (Phantoms.TTerm Meta.Defn_Object -> Phantoms.TTerm Meta.Data_Name -> Phantoms.TTerm Meta.Defn_Object)
defn_ObjectWithName original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Defn_Object"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

pkg :: (Phantoms.TTerm Meta.Data_Name -> Phantoms.TTerm Meta.Data_Ref -> Phantoms.TTerm [Meta.Stat] -> Phantoms.TTerm Meta.Pkg)
pkg name ref stats = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Pkg"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Phantoms.unTTerm name)},
    Core.Field {
      Core.fieldName = (Core.Name "ref"),
      Core.fieldTerm = (Phantoms.unTTerm ref)},
    Core.Field {
      Core.fieldName = (Core.Name "stats"),
      Core.fieldTerm = (Phantoms.unTTerm stats)}]})))

pkgName :: (Phantoms.TTerm Meta.Pkg -> Phantoms.TTerm Meta.Data_Name)
pkgName x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Pkg"),
    Core.projectionField = (Core.Name "name")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

pkgRef :: (Phantoms.TTerm Meta.Pkg -> Phantoms.TTerm Meta.Data_Ref)
pkgRef x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Pkg"),
    Core.projectionField = (Core.Name "ref")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

pkgStats :: (Phantoms.TTerm Meta.Pkg -> Phantoms.TTerm [Meta.Stat])
pkgStats x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Pkg"),
    Core.projectionField = (Core.Name "stats")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

pkgWithName :: (Phantoms.TTerm Meta.Pkg -> Phantoms.TTerm Meta.Data_Name -> Phantoms.TTerm Meta.Pkg)
pkgWithName original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Pkg"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "ref"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Pkg"),
          Core.projectionField = (Core.Name "ref")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "stats"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Pkg"),
          Core.projectionField = (Core.Name "stats")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

pkgWithRef :: (Phantoms.TTerm Meta.Pkg -> Phantoms.TTerm Meta.Data_Ref -> Phantoms.TTerm Meta.Pkg)
pkgWithRef original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Pkg"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Pkg"),
          Core.projectionField = (Core.Name "name")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "ref"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "stats"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Pkg"),
          Core.projectionField = (Core.Name "stats")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

pkgWithStats :: (Phantoms.TTerm Meta.Pkg -> Phantoms.TTerm [Meta.Stat] -> Phantoms.TTerm Meta.Pkg)
pkgWithStats original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Pkg"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Pkg"),
          Core.projectionField = (Core.Name "name")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "ref"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Pkg"),
          Core.projectionField = (Core.Name "ref")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "stats"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

pkg_Object :: (Phantoms.TTerm [Meta.Mod] -> Phantoms.TTerm Meta.Data_Name -> Phantoms.TTerm Meta.Template -> Phantoms.TTerm Meta.Pkg_Object)
pkg_Object mods name template = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Pkg_Object"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "mods"),
      Core.fieldTerm = (Phantoms.unTTerm mods)},
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Phantoms.unTTerm name)},
    Core.Field {
      Core.fieldName = (Core.Name "template"),
      Core.fieldTerm = (Phantoms.unTTerm template)}]})))

pkg_ObjectMods :: (Phantoms.TTerm Meta.Pkg_Object -> Phantoms.TTerm [Meta.Mod])
pkg_ObjectMods x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Pkg_Object"),
    Core.projectionField = (Core.Name "mods")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

pkg_ObjectName :: (Phantoms.TTerm Meta.Pkg_Object -> Phantoms.TTerm Meta.Data_Name)
pkg_ObjectName x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Pkg_Object"),
    Core.projectionField = (Core.Name "name")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

pkg_ObjectTemplate :: (Phantoms.TTerm Meta.Pkg_Object -> Phantoms.TTerm Meta.Template)
pkg_ObjectTemplate x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Pkg_Object"),
    Core.projectionField = (Core.Name "template")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

pkg_ObjectWithMods :: (Phantoms.TTerm Meta.Pkg_Object -> Phantoms.TTerm [Meta.Mod] -> Phantoms.TTerm Meta.Pkg_Object)
pkg_ObjectWithMods original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Pkg_Object"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "mods"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Pkg_Object"),
          Core.projectionField = (Core.Name "name")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "template"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Pkg_Object"),
          Core.projectionField = (Core.Name "template")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

pkg_ObjectWithName :: (Phantoms.TTerm Meta.Pkg_Object -> Phantoms.TTerm Meta.Data_Name -> Phantoms.TTerm Meta.Pkg_Object)
pkg_ObjectWithName original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Pkg_Object"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "mods"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Pkg_Object"),
          Core.projectionField = (Core.Name "mods")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "template"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Pkg_Object"),
          Core.projectionField = (Core.Name "template")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

pkg_ObjectWithTemplate :: (Phantoms.TTerm Meta.Pkg_Object -> Phantoms.TTerm Meta.Template -> Phantoms.TTerm Meta.Pkg_Object)
pkg_ObjectWithTemplate original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Pkg_Object"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "mods"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Pkg_Object"),
          Core.projectionField = (Core.Name "mods")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Pkg_Object"),
          Core.projectionField = (Core.Name "name")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "template"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

ctorPrimary :: (Phantoms.TTerm Meta.Ctor_Primary -> Phantoms.TTerm Meta.Ctor)
ctorPrimary x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Ctor"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "primary"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

ctorSecondary :: (Phantoms.TTerm Meta.Ctor_Secondary -> Phantoms.TTerm Meta.Ctor)
ctorSecondary x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Ctor"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "secondary"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

ctor_Primary :: (Phantoms.TTerm [Meta.Mod] -> Phantoms.TTerm Meta.Name -> Phantoms.TTerm [[Meta.Data_Param]] -> Phantoms.TTerm Meta.Ctor_Primary)
ctor_Primary mods name paramss = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Ctor_Primary"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "mods"),
      Core.fieldTerm = (Phantoms.unTTerm mods)},
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Phantoms.unTTerm name)},
    Core.Field {
      Core.fieldName = (Core.Name "paramss"),
      Core.fieldTerm = (Phantoms.unTTerm paramss)}]})))

ctor_PrimaryMods :: (Phantoms.TTerm Meta.Ctor_Primary -> Phantoms.TTerm [Meta.Mod])
ctor_PrimaryMods x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Ctor_Primary"),
    Core.projectionField = (Core.Name "mods")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

ctor_PrimaryName :: (Phantoms.TTerm Meta.Ctor_Primary -> Phantoms.TTerm Meta.Name)
ctor_PrimaryName x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Ctor_Primary"),
    Core.projectionField = (Core.Name "name")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

ctor_PrimaryParamss :: (Phantoms.TTerm Meta.Ctor_Primary -> Phantoms.TTerm [[Meta.Data_Param]])
ctor_PrimaryParamss x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Ctor_Primary"),
    Core.projectionField = (Core.Name "paramss")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

ctor_PrimaryWithMods :: (Phantoms.TTerm Meta.Ctor_Primary -> Phantoms.TTerm [Meta.Mod] -> Phantoms.TTerm Meta.Ctor_Primary)
ctor_PrimaryWithMods original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Ctor_Primary"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "mods"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Ctor_Primary"),
          Core.projectionField = (Core.Name "name")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "paramss"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Ctor_Primary"),
          Core.projectionField = (Core.Name "paramss")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

ctor_PrimaryWithName :: (Phantoms.TTerm Meta.Ctor_Primary -> Phantoms.TTerm Meta.Name -> Phantoms.TTerm Meta.Ctor_Primary)
ctor_PrimaryWithName original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Ctor_Primary"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "mods"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Ctor_Primary"),
          Core.projectionField = (Core.Name "mods")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "paramss"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Ctor_Primary"),
          Core.projectionField = (Core.Name "paramss")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

ctor_PrimaryWithParamss :: (Phantoms.TTerm Meta.Ctor_Primary -> Phantoms.TTerm [[Meta.Data_Param]] -> Phantoms.TTerm Meta.Ctor_Primary)
ctor_PrimaryWithParamss original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Ctor_Primary"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "mods"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Ctor_Primary"),
          Core.projectionField = (Core.Name "mods")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Ctor_Primary"),
          Core.projectionField = (Core.Name "name")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "paramss"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

ctor_Secondary :: (Phantoms.TTerm [Meta.Mod] -> Phantoms.TTerm Meta.Name -> Phantoms.TTerm [[Meta.Data_Param]] -> Phantoms.TTerm Meta.Init -> Phantoms.TTerm [Meta.Stat] -> Phantoms.TTerm Meta.Ctor_Secondary)
ctor_Secondary mods name paramss init stats = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Ctor_Secondary"),
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
      Core.fieldTerm = (Phantoms.unTTerm stats)}]})))

ctor_SecondaryMods :: (Phantoms.TTerm Meta.Ctor_Secondary -> Phantoms.TTerm [Meta.Mod])
ctor_SecondaryMods x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Ctor_Secondary"),
    Core.projectionField = (Core.Name "mods")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

ctor_SecondaryName :: (Phantoms.TTerm Meta.Ctor_Secondary -> Phantoms.TTerm Meta.Name)
ctor_SecondaryName x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Ctor_Secondary"),
    Core.projectionField = (Core.Name "name")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

ctor_SecondaryParamss :: (Phantoms.TTerm Meta.Ctor_Secondary -> Phantoms.TTerm [[Meta.Data_Param]])
ctor_SecondaryParamss x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Ctor_Secondary"),
    Core.projectionField = (Core.Name "paramss")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

ctor_SecondaryInit :: (Phantoms.TTerm Meta.Ctor_Secondary -> Phantoms.TTerm Meta.Init)
ctor_SecondaryInit x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Ctor_Secondary"),
    Core.projectionField = (Core.Name "init")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

ctor_SecondaryStats :: (Phantoms.TTerm Meta.Ctor_Secondary -> Phantoms.TTerm [Meta.Stat])
ctor_SecondaryStats x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Ctor_Secondary"),
    Core.projectionField = (Core.Name "stats")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

ctor_SecondaryWithMods :: (Phantoms.TTerm Meta.Ctor_Secondary -> Phantoms.TTerm [Meta.Mod] -> Phantoms.TTerm Meta.Ctor_Secondary)
ctor_SecondaryWithMods original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Ctor_Secondary"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "mods"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Ctor_Secondary"),
          Core.projectionField = (Core.Name "name")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "paramss"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Ctor_Secondary"),
          Core.projectionField = (Core.Name "paramss")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "init"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Ctor_Secondary"),
          Core.projectionField = (Core.Name "init")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "stats"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Ctor_Secondary"),
          Core.projectionField = (Core.Name "stats")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

ctor_SecondaryWithName :: (Phantoms.TTerm Meta.Ctor_Secondary -> Phantoms.TTerm Meta.Name -> Phantoms.TTerm Meta.Ctor_Secondary)
ctor_SecondaryWithName original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Ctor_Secondary"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "mods"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Ctor_Secondary"),
          Core.projectionField = (Core.Name "mods")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "paramss"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Ctor_Secondary"),
          Core.projectionField = (Core.Name "paramss")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "init"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Ctor_Secondary"),
          Core.projectionField = (Core.Name "init")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "stats"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Ctor_Secondary"),
          Core.projectionField = (Core.Name "stats")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

ctor_SecondaryWithParamss :: (Phantoms.TTerm Meta.Ctor_Secondary -> Phantoms.TTerm [[Meta.Data_Param]] -> Phantoms.TTerm Meta.Ctor_Secondary)
ctor_SecondaryWithParamss original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Ctor_Secondary"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "mods"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Ctor_Secondary"),
          Core.projectionField = (Core.Name "mods")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Ctor_Secondary"),
          Core.projectionField = (Core.Name "name")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "paramss"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "init"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Ctor_Secondary"),
          Core.projectionField = (Core.Name "init")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "stats"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Ctor_Secondary"),
          Core.projectionField = (Core.Name "stats")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

ctor_SecondaryWithInit :: (Phantoms.TTerm Meta.Ctor_Secondary -> Phantoms.TTerm Meta.Init -> Phantoms.TTerm Meta.Ctor_Secondary)
ctor_SecondaryWithInit original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Ctor_Secondary"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "mods"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Ctor_Secondary"),
          Core.projectionField = (Core.Name "mods")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Ctor_Secondary"),
          Core.projectionField = (Core.Name "name")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "paramss"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Ctor_Secondary"),
          Core.projectionField = (Core.Name "paramss")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "init"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "stats"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Ctor_Secondary"),
          Core.projectionField = (Core.Name "stats")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

ctor_SecondaryWithStats :: (Phantoms.TTerm Meta.Ctor_Secondary -> Phantoms.TTerm [Meta.Stat] -> Phantoms.TTerm Meta.Ctor_Secondary)
ctor_SecondaryWithStats original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Ctor_Secondary"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "mods"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Ctor_Secondary"),
          Core.projectionField = (Core.Name "mods")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Ctor_Secondary"),
          Core.projectionField = (Core.Name "name")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "paramss"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Ctor_Secondary"),
          Core.projectionField = (Core.Name "paramss")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "init"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Ctor_Secondary"),
          Core.projectionField = (Core.Name "init")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "stats"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

init :: (Phantoms.TTerm Meta.Type -> Phantoms.TTerm Meta.Name -> Phantoms.TTerm [[Meta.Data]] -> Phantoms.TTerm Meta.Init)
init tpe name argss = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Init"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "tpe"),
      Core.fieldTerm = (Phantoms.unTTerm tpe)},
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Phantoms.unTTerm name)},
    Core.Field {
      Core.fieldName = (Core.Name "argss"),
      Core.fieldTerm = (Phantoms.unTTerm argss)}]})))

initTpe :: (Phantoms.TTerm Meta.Init -> Phantoms.TTerm Meta.Type)
initTpe x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Init"),
    Core.projectionField = (Core.Name "tpe")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

initName :: (Phantoms.TTerm Meta.Init -> Phantoms.TTerm Meta.Name)
initName x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Init"),
    Core.projectionField = (Core.Name "name")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

initArgss :: (Phantoms.TTerm Meta.Init -> Phantoms.TTerm [[Meta.Data]])
initArgss x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Init"),
    Core.projectionField = (Core.Name "argss")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

initWithTpe :: (Phantoms.TTerm Meta.Init -> Phantoms.TTerm Meta.Type -> Phantoms.TTerm Meta.Init)
initWithTpe original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Init"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "tpe"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Init"),
          Core.projectionField = (Core.Name "name")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "argss"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Init"),
          Core.projectionField = (Core.Name "argss")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

initWithName :: (Phantoms.TTerm Meta.Init -> Phantoms.TTerm Meta.Name -> Phantoms.TTerm Meta.Init)
initWithName original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Init"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "tpe"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Init"),
          Core.projectionField = (Core.Name "tpe")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "argss"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Init"),
          Core.projectionField = (Core.Name "argss")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

initWithArgss :: (Phantoms.TTerm Meta.Init -> Phantoms.TTerm [[Meta.Data]] -> Phantoms.TTerm Meta.Init)
initWithArgss original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Init"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "tpe"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Init"),
          Core.projectionField = (Core.Name "tpe")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Init"),
          Core.projectionField = (Core.Name "name")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "argss"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

self :: (Phantoms.TTerm () -> Phantoms.TTerm Meta.Self)
self x = (Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
  Core.wrappedTermTypeName = (Core.Name "hydra.ext.scala.meta.Self"),
  Core.wrappedTermBody = (Phantoms.unTTerm x)})))

unSelf :: (Phantoms.TTerm Meta.Self -> Phantoms.TTerm ())
unSelf x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.scala.meta.Self")))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

template :: (Phantoms.TTerm [Meta.Stat] -> Phantoms.TTerm [Meta.Init] -> Phantoms.TTerm Meta.Self -> Phantoms.TTerm [Meta.Stat] -> Phantoms.TTerm Meta.Template)
template early inits self stats = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Template"),
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
      Core.fieldTerm = (Phantoms.unTTerm stats)}]})))

templateEarly :: (Phantoms.TTerm Meta.Template -> Phantoms.TTerm [Meta.Stat])
templateEarly x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Template"),
    Core.projectionField = (Core.Name "early")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

templateInits :: (Phantoms.TTerm Meta.Template -> Phantoms.TTerm [Meta.Init])
templateInits x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Template"),
    Core.projectionField = (Core.Name "inits")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

templateSelf :: (Phantoms.TTerm Meta.Template -> Phantoms.TTerm Meta.Self)
templateSelf x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Template"),
    Core.projectionField = (Core.Name "self")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

templateStats :: (Phantoms.TTerm Meta.Template -> Phantoms.TTerm [Meta.Stat])
templateStats x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Template"),
    Core.projectionField = (Core.Name "stats")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

templateWithEarly :: (Phantoms.TTerm Meta.Template -> Phantoms.TTerm [Meta.Stat] -> Phantoms.TTerm Meta.Template)
templateWithEarly original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Template"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "early"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "inits"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Template"),
          Core.projectionField = (Core.Name "inits")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "self"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Template"),
          Core.projectionField = (Core.Name "self")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "stats"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Template"),
          Core.projectionField = (Core.Name "stats")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

templateWithInits :: (Phantoms.TTerm Meta.Template -> Phantoms.TTerm [Meta.Init] -> Phantoms.TTerm Meta.Template)
templateWithInits original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Template"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "early"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Template"),
          Core.projectionField = (Core.Name "early")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "inits"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "self"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Template"),
          Core.projectionField = (Core.Name "self")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "stats"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Template"),
          Core.projectionField = (Core.Name "stats")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

templateWithSelf :: (Phantoms.TTerm Meta.Template -> Phantoms.TTerm Meta.Self -> Phantoms.TTerm Meta.Template)
templateWithSelf original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Template"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "early"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Template"),
          Core.projectionField = (Core.Name "early")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "inits"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Template"),
          Core.projectionField = (Core.Name "inits")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "self"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "stats"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Template"),
          Core.projectionField = (Core.Name "stats")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

templateWithStats :: (Phantoms.TTerm Meta.Template -> Phantoms.TTerm [Meta.Stat] -> Phantoms.TTerm Meta.Template)
templateWithStats original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Template"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "early"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Template"),
          Core.projectionField = (Core.Name "early")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "inits"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Template"),
          Core.projectionField = (Core.Name "inits")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "self"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Template"),
          Core.projectionField = (Core.Name "self")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "stats"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

modAnnot :: (Phantoms.TTerm Meta.Mod_Annot -> Phantoms.TTerm Meta.Mod)
modAnnot x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Mod"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "annot"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

modPrivate :: (Phantoms.TTerm Meta.Mod_Private -> Phantoms.TTerm Meta.Mod)
modPrivate x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Mod"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "private"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

modProtected :: (Phantoms.TTerm Meta.Mod_Protected -> Phantoms.TTerm Meta.Mod)
modProtected x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Mod"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "protected"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

modImplicit :: (Phantoms.TTerm Meta.Mod)
modImplicit = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Mod"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "implicit"),
    Core.fieldTerm = Core.TermUnit}})))

modFinal :: (Phantoms.TTerm Meta.Mod)
modFinal = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Mod"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "final"),
    Core.fieldTerm = Core.TermUnit}})))

modSealed :: (Phantoms.TTerm Meta.Mod)
modSealed = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Mod"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "sealed"),
    Core.fieldTerm = Core.TermUnit}})))

modOpen :: (Phantoms.TTerm Meta.Mod)
modOpen = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Mod"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "open"),
    Core.fieldTerm = Core.TermUnit}})))

modSuper :: (Phantoms.TTerm Meta.Mod)
modSuper = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Mod"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "super"),
    Core.fieldTerm = Core.TermUnit}})))

modOverride :: (Phantoms.TTerm Meta.Mod)
modOverride = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Mod"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "override"),
    Core.fieldTerm = Core.TermUnit}})))

modCase :: (Phantoms.TTerm Meta.Mod)
modCase = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Mod"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "case"),
    Core.fieldTerm = Core.TermUnit}})))

modAbstract :: (Phantoms.TTerm Meta.Mod)
modAbstract = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Mod"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "abstract"),
    Core.fieldTerm = Core.TermUnit}})))

modCovariant :: (Phantoms.TTerm Meta.Mod)
modCovariant = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Mod"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "covariant"),
    Core.fieldTerm = Core.TermUnit}})))

modContravariant :: (Phantoms.TTerm Meta.Mod)
modContravariant = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Mod"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "contravariant"),
    Core.fieldTerm = Core.TermUnit}})))

modLazy :: (Phantoms.TTerm Meta.Mod)
modLazy = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Mod"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "lazy"),
    Core.fieldTerm = Core.TermUnit}})))

modValParam :: (Phantoms.TTerm Meta.Mod)
modValParam = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Mod"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "valParam"),
    Core.fieldTerm = Core.TermUnit}})))

modVarParam :: (Phantoms.TTerm Meta.Mod)
modVarParam = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Mod"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "varParam"),
    Core.fieldTerm = Core.TermUnit}})))

modInfix :: (Phantoms.TTerm Meta.Mod)
modInfix = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Mod"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "infix"),
    Core.fieldTerm = Core.TermUnit}})))

modInline :: (Phantoms.TTerm Meta.Mod)
modInline = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Mod"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "inline"),
    Core.fieldTerm = Core.TermUnit}})))

modUsing :: (Phantoms.TTerm Meta.Mod)
modUsing = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Mod"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "using"),
    Core.fieldTerm = Core.TermUnit}})))

modOpaque :: (Phantoms.TTerm Meta.Mod)
modOpaque = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Mod"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "opaque"),
    Core.fieldTerm = Core.TermUnit}})))

modTransparent :: (Phantoms.TTerm Meta.Mod)
modTransparent = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Mod"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "transparent"),
    Core.fieldTerm = Core.TermUnit}})))

mod_Annot :: (Phantoms.TTerm Meta.Init -> Phantoms.TTerm Meta.Mod_Annot)
mod_Annot init = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Mod_Annot"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "init"),
      Core.fieldTerm = (Phantoms.unTTerm init)}]})))

mod_AnnotInit :: (Phantoms.TTerm Meta.Mod_Annot -> Phantoms.TTerm Meta.Init)
mod_AnnotInit x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Mod_Annot"),
    Core.projectionField = (Core.Name "init")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

mod_AnnotWithInit :: (Phantoms.TTerm Meta.Mod_Annot -> Phantoms.TTerm Meta.Init -> Phantoms.TTerm Meta.Mod_Annot)
mod_AnnotWithInit original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Mod_Annot"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "init"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

mod_Private :: (Phantoms.TTerm Meta.Ref -> Phantoms.TTerm Meta.Mod_Private)
mod_Private within = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Mod_Private"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "within"),
      Core.fieldTerm = (Phantoms.unTTerm within)}]})))

mod_PrivateWithin :: (Phantoms.TTerm Meta.Mod_Private -> Phantoms.TTerm Meta.Ref)
mod_PrivateWithin x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Mod_Private"),
    Core.projectionField = (Core.Name "within")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

mod_PrivateWithWithin :: (Phantoms.TTerm Meta.Mod_Private -> Phantoms.TTerm Meta.Ref -> Phantoms.TTerm Meta.Mod_Private)
mod_PrivateWithWithin original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Mod_Private"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "within"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

mod_Protected :: (Phantoms.TTerm Meta.Ref -> Phantoms.TTerm Meta.Mod_Protected)
mod_Protected within = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Mod_Protected"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "within"),
      Core.fieldTerm = (Phantoms.unTTerm within)}]})))

mod_ProtectedWithin :: (Phantoms.TTerm Meta.Mod_Protected -> Phantoms.TTerm Meta.Ref)
mod_ProtectedWithin x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Mod_Protected"),
    Core.projectionField = (Core.Name "within")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

mod_ProtectedWithWithin :: (Phantoms.TTerm Meta.Mod_Protected -> Phantoms.TTerm Meta.Ref -> Phantoms.TTerm Meta.Mod_Protected)
mod_ProtectedWithWithin original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Mod_Protected"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "within"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

enumeratorGenerator :: (Phantoms.TTerm Meta.Enumerator_Generator -> Phantoms.TTerm Meta.Enumerator)
enumeratorGenerator x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Enumerator"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "generator"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

enumeratorCaseGenerator :: (Phantoms.TTerm Meta.Enumerator_CaseGenerator -> Phantoms.TTerm Meta.Enumerator)
enumeratorCaseGenerator x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Enumerator"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "caseGenerator"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

enumeratorVal :: (Phantoms.TTerm Meta.Enumerator_Val -> Phantoms.TTerm Meta.Enumerator)
enumeratorVal x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Enumerator"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "val"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

enumeratorGuard :: (Phantoms.TTerm Meta.Enumerator_Guard -> Phantoms.TTerm Meta.Enumerator)
enumeratorGuard x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Enumerator"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "guard"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

enumerator_Generator :: (Phantoms.TTerm Meta.Pat -> Phantoms.TTerm Meta.Data -> Phantoms.TTerm Meta.Enumerator_Generator)
enumerator_Generator pat rhs = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Enumerator_Generator"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "pat"),
      Core.fieldTerm = (Phantoms.unTTerm pat)},
    Core.Field {
      Core.fieldName = (Core.Name "rhs"),
      Core.fieldTerm = (Phantoms.unTTerm rhs)}]})))

enumerator_GeneratorPat :: (Phantoms.TTerm Meta.Enumerator_Generator -> Phantoms.TTerm Meta.Pat)
enumerator_GeneratorPat x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Enumerator_Generator"),
    Core.projectionField = (Core.Name "pat")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

enumerator_GeneratorRhs :: (Phantoms.TTerm Meta.Enumerator_Generator -> Phantoms.TTerm Meta.Data)
enumerator_GeneratorRhs x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Enumerator_Generator"),
    Core.projectionField = (Core.Name "rhs")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

enumerator_GeneratorWithPat :: (Phantoms.TTerm Meta.Enumerator_Generator -> Phantoms.TTerm Meta.Pat -> Phantoms.TTerm Meta.Enumerator_Generator)
enumerator_GeneratorWithPat original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Enumerator_Generator"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "pat"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "rhs"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Enumerator_Generator"),
          Core.projectionField = (Core.Name "rhs")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

enumerator_GeneratorWithRhs :: (Phantoms.TTerm Meta.Enumerator_Generator -> Phantoms.TTerm Meta.Data -> Phantoms.TTerm Meta.Enumerator_Generator)
enumerator_GeneratorWithRhs original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Enumerator_Generator"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "pat"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Enumerator_Generator"),
          Core.projectionField = (Core.Name "pat")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "rhs"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

enumerator_CaseGenerator :: (Phantoms.TTerm Meta.Pat -> Phantoms.TTerm Meta.Data -> Phantoms.TTerm Meta.Enumerator_CaseGenerator)
enumerator_CaseGenerator pat rhs = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Enumerator_CaseGenerator"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "pat"),
      Core.fieldTerm = (Phantoms.unTTerm pat)},
    Core.Field {
      Core.fieldName = (Core.Name "rhs"),
      Core.fieldTerm = (Phantoms.unTTerm rhs)}]})))

enumerator_CaseGeneratorPat :: (Phantoms.TTerm Meta.Enumerator_CaseGenerator -> Phantoms.TTerm Meta.Pat)
enumerator_CaseGeneratorPat x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Enumerator_CaseGenerator"),
    Core.projectionField = (Core.Name "pat")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

enumerator_CaseGeneratorRhs :: (Phantoms.TTerm Meta.Enumerator_CaseGenerator -> Phantoms.TTerm Meta.Data)
enumerator_CaseGeneratorRhs x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Enumerator_CaseGenerator"),
    Core.projectionField = (Core.Name "rhs")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

enumerator_CaseGeneratorWithPat :: (Phantoms.TTerm Meta.Enumerator_CaseGenerator -> Phantoms.TTerm Meta.Pat -> Phantoms.TTerm Meta.Enumerator_CaseGenerator)
enumerator_CaseGeneratorWithPat original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Enumerator_CaseGenerator"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "pat"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "rhs"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Enumerator_CaseGenerator"),
          Core.projectionField = (Core.Name "rhs")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

enumerator_CaseGeneratorWithRhs :: (Phantoms.TTerm Meta.Enumerator_CaseGenerator -> Phantoms.TTerm Meta.Data -> Phantoms.TTerm Meta.Enumerator_CaseGenerator)
enumerator_CaseGeneratorWithRhs original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Enumerator_CaseGenerator"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "pat"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Enumerator_CaseGenerator"),
          Core.projectionField = (Core.Name "pat")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "rhs"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

enumerator_Val :: (Phantoms.TTerm Meta.Pat -> Phantoms.TTerm Meta.Data -> Phantoms.TTerm Meta.Enumerator_Val)
enumerator_Val pat rhs = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Enumerator_Val"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "pat"),
      Core.fieldTerm = (Phantoms.unTTerm pat)},
    Core.Field {
      Core.fieldName = (Core.Name "rhs"),
      Core.fieldTerm = (Phantoms.unTTerm rhs)}]})))

enumerator_ValPat :: (Phantoms.TTerm Meta.Enumerator_Val -> Phantoms.TTerm Meta.Pat)
enumerator_ValPat x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Enumerator_Val"),
    Core.projectionField = (Core.Name "pat")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

enumerator_ValRhs :: (Phantoms.TTerm Meta.Enumerator_Val -> Phantoms.TTerm Meta.Data)
enumerator_ValRhs x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Enumerator_Val"),
    Core.projectionField = (Core.Name "rhs")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

enumerator_ValWithPat :: (Phantoms.TTerm Meta.Enumerator_Val -> Phantoms.TTerm Meta.Pat -> Phantoms.TTerm Meta.Enumerator_Val)
enumerator_ValWithPat original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Enumerator_Val"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "pat"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "rhs"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Enumerator_Val"),
          Core.projectionField = (Core.Name "rhs")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

enumerator_ValWithRhs :: (Phantoms.TTerm Meta.Enumerator_Val -> Phantoms.TTerm Meta.Data -> Phantoms.TTerm Meta.Enumerator_Val)
enumerator_ValWithRhs original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Enumerator_Val"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "pat"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Enumerator_Val"),
          Core.projectionField = (Core.Name "pat")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "rhs"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

enumerator_Guard :: (Phantoms.TTerm Meta.Data -> Phantoms.TTerm Meta.Enumerator_Guard)
enumerator_Guard cond = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Enumerator_Guard"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "cond"),
      Core.fieldTerm = (Phantoms.unTTerm cond)}]})))

enumerator_GuardCond :: (Phantoms.TTerm Meta.Enumerator_Guard -> Phantoms.TTerm Meta.Data)
enumerator_GuardCond x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Enumerator_Guard"),
    Core.projectionField = (Core.Name "cond")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

enumerator_GuardWithCond :: (Phantoms.TTerm Meta.Enumerator_Guard -> Phantoms.TTerm Meta.Data -> Phantoms.TTerm Meta.Enumerator_Guard)
enumerator_GuardWithCond original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Enumerator_Guard"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "cond"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

importExportStatImport :: (Phantoms.TTerm Meta.Import -> Phantoms.TTerm Meta.ImportExportStat)
importExportStatImport x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.ImportExportStat"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "import"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

importExportStatExport :: (Phantoms.TTerm Meta.Export -> Phantoms.TTerm Meta.ImportExportStat)
importExportStatExport x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.ImportExportStat"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "export"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

import_ :: (Phantoms.TTerm [Meta.Importer] -> Phantoms.TTerm Meta.Import)
import_ importers = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Import"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "importers"),
      Core.fieldTerm = (Phantoms.unTTerm importers)}]})))

importImporters :: (Phantoms.TTerm Meta.Import -> Phantoms.TTerm [Meta.Importer])
importImporters x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Import"),
    Core.projectionField = (Core.Name "importers")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

importWithImporters :: (Phantoms.TTerm Meta.Import -> Phantoms.TTerm [Meta.Importer] -> Phantoms.TTerm Meta.Import)
importWithImporters original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Import"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "importers"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

export :: (Phantoms.TTerm [Meta.Importer] -> Phantoms.TTerm Meta.Export)
export importers = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Export"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "importers"),
      Core.fieldTerm = (Phantoms.unTTerm importers)}]})))

exportImporters :: (Phantoms.TTerm Meta.Export -> Phantoms.TTerm [Meta.Importer])
exportImporters x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Export"),
    Core.projectionField = (Core.Name "importers")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

exportWithImporters :: (Phantoms.TTerm Meta.Export -> Phantoms.TTerm [Meta.Importer] -> Phantoms.TTerm Meta.Export)
exportWithImporters original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Export"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "importers"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

importer :: (Phantoms.TTerm Meta.Data_Ref -> Phantoms.TTerm [Meta.Importee] -> Phantoms.TTerm Meta.Importer)
importer ref importees = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Importer"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "ref"),
      Core.fieldTerm = (Phantoms.unTTerm ref)},
    Core.Field {
      Core.fieldName = (Core.Name "importees"),
      Core.fieldTerm = (Phantoms.unTTerm importees)}]})))

importerRef :: (Phantoms.TTerm Meta.Importer -> Phantoms.TTerm Meta.Data_Ref)
importerRef x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Importer"),
    Core.projectionField = (Core.Name "ref")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

importerImportees :: (Phantoms.TTerm Meta.Importer -> Phantoms.TTerm [Meta.Importee])
importerImportees x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Importer"),
    Core.projectionField = (Core.Name "importees")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

importerWithRef :: (Phantoms.TTerm Meta.Importer -> Phantoms.TTerm Meta.Data_Ref -> Phantoms.TTerm Meta.Importer)
importerWithRef original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Importer"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "ref"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "importees"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Importer"),
          Core.projectionField = (Core.Name "importees")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

importerWithImportees :: (Phantoms.TTerm Meta.Importer -> Phantoms.TTerm [Meta.Importee] -> Phantoms.TTerm Meta.Importer)
importerWithImportees original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Importer"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "ref"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Importer"),
          Core.projectionField = (Core.Name "ref")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "importees"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

importeeWildcard :: (Phantoms.TTerm Meta.Importee)
importeeWildcard = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Importee"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "wildcard"),
    Core.fieldTerm = Core.TermUnit}})))

importeeGiven :: (Phantoms.TTerm Meta.Importee_Given -> Phantoms.TTerm Meta.Importee)
importeeGiven x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Importee"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "given"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

importeeGivenAll :: (Phantoms.TTerm Meta.Importee)
importeeGivenAll = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Importee"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "givenAll"),
    Core.fieldTerm = Core.TermUnit}})))

importeeName :: (Phantoms.TTerm Meta.Importee_Name -> Phantoms.TTerm Meta.Importee)
importeeName x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Importee"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "name"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

importeeRename :: (Phantoms.TTerm Meta.Importee_Rename -> Phantoms.TTerm Meta.Importee)
importeeRename x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Importee"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "rename"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

importeeUnimport :: (Phantoms.TTerm Meta.Importee_Unimport -> Phantoms.TTerm Meta.Importee)
importeeUnimport x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.Importee"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "unimport"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

importee_Given :: (Phantoms.TTerm Meta.Type -> Phantoms.TTerm Meta.Importee_Given)
importee_Given tpe = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Importee_Given"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "tpe"),
      Core.fieldTerm = (Phantoms.unTTerm tpe)}]})))

importee_GivenTpe :: (Phantoms.TTerm Meta.Importee_Given -> Phantoms.TTerm Meta.Type)
importee_GivenTpe x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Importee_Given"),
    Core.projectionField = (Core.Name "tpe")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

importee_GivenWithTpe :: (Phantoms.TTerm Meta.Importee_Given -> Phantoms.TTerm Meta.Type -> Phantoms.TTerm Meta.Importee_Given)
importee_GivenWithTpe original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Importee_Given"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "tpe"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

importee_Name :: (Phantoms.TTerm Meta.Name -> Phantoms.TTerm Meta.Importee_Name)
importee_Name name = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Importee_Name"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Phantoms.unTTerm name)}]})))

importee_NameName :: (Phantoms.TTerm Meta.Importee_Name -> Phantoms.TTerm Meta.Name)
importee_NameName x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Importee_Name"),
    Core.projectionField = (Core.Name "name")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

importee_NameWithName :: (Phantoms.TTerm Meta.Importee_Name -> Phantoms.TTerm Meta.Name -> Phantoms.TTerm Meta.Importee_Name)
importee_NameWithName original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Importee_Name"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

importee_Rename :: (Phantoms.TTerm Meta.Name -> Phantoms.TTerm Meta.Name -> Phantoms.TTerm Meta.Importee_Rename)
importee_Rename name rename = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Importee_Rename"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Phantoms.unTTerm name)},
    Core.Field {
      Core.fieldName = (Core.Name "rename"),
      Core.fieldTerm = (Phantoms.unTTerm rename)}]})))

importee_RenameName :: (Phantoms.TTerm Meta.Importee_Rename -> Phantoms.TTerm Meta.Name)
importee_RenameName x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Importee_Rename"),
    Core.projectionField = (Core.Name "name")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

importee_RenameRename :: (Phantoms.TTerm Meta.Importee_Rename -> Phantoms.TTerm Meta.Name)
importee_RenameRename x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Importee_Rename"),
    Core.projectionField = (Core.Name "rename")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

importee_RenameWithName :: (Phantoms.TTerm Meta.Importee_Rename -> Phantoms.TTerm Meta.Name -> Phantoms.TTerm Meta.Importee_Rename)
importee_RenameWithName original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Importee_Rename"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "rename"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Importee_Rename"),
          Core.projectionField = (Core.Name "rename")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

importee_RenameWithRename :: (Phantoms.TTerm Meta.Importee_Rename -> Phantoms.TTerm Meta.Name -> Phantoms.TTerm Meta.Importee_Rename)
importee_RenameWithRename original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Importee_Rename"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Importee_Rename"),
          Core.projectionField = (Core.Name "name")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "rename"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

importee_Unimport :: (Phantoms.TTerm Meta.Name -> Phantoms.TTerm Meta.Importee_Unimport)
importee_Unimport name = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Importee_Unimport"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Phantoms.unTTerm name)}]})))

importee_UnimportName :: (Phantoms.TTerm Meta.Importee_Unimport -> Phantoms.TTerm Meta.Name)
importee_UnimportName x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Importee_Unimport"),
    Core.projectionField = (Core.Name "name")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

importee_UnimportWithName :: (Phantoms.TTerm Meta.Importee_Unimport -> Phantoms.TTerm Meta.Name -> Phantoms.TTerm Meta.Importee_Unimport)
importee_UnimportWithName original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Importee_Unimport"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

caseTreeCase :: (Phantoms.TTerm Meta.Case -> Phantoms.TTerm Meta.CaseTree)
caseTreeCase x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.CaseTree"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "case"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

caseTreeTypeCase :: (Phantoms.TTerm Meta.TypeCase -> Phantoms.TTerm Meta.CaseTree)
caseTreeTypeCase x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.scala.meta.CaseTree"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "typeCase"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

case_ :: (Phantoms.TTerm Meta.Pat -> Phantoms.TTerm (Maybe Meta.Data) -> Phantoms.TTerm Meta.Data -> Phantoms.TTerm Meta.Case)
case_ pat cond body = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Case"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "pat"),
      Core.fieldTerm = (Phantoms.unTTerm pat)},
    Core.Field {
      Core.fieldName = (Core.Name "cond"),
      Core.fieldTerm = (Phantoms.unTTerm cond)},
    Core.Field {
      Core.fieldName = (Core.Name "body"),
      Core.fieldTerm = (Phantoms.unTTerm body)}]})))

casePat :: (Phantoms.TTerm Meta.Case -> Phantoms.TTerm Meta.Pat)
casePat x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Case"),
    Core.projectionField = (Core.Name "pat")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

caseCond :: (Phantoms.TTerm Meta.Case -> Phantoms.TTerm (Maybe Meta.Data))
caseCond x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Case"),
    Core.projectionField = (Core.Name "cond")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

caseBody :: (Phantoms.TTerm Meta.Case -> Phantoms.TTerm Meta.Data)
caseBody x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Case"),
    Core.projectionField = (Core.Name "body")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

caseWithPat :: (Phantoms.TTerm Meta.Case -> Phantoms.TTerm Meta.Pat -> Phantoms.TTerm Meta.Case)
caseWithPat original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Case"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "pat"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "cond"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Case"),
          Core.projectionField = (Core.Name "cond")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "body"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Case"),
          Core.projectionField = (Core.Name "body")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

caseWithCond :: (Phantoms.TTerm Meta.Case -> Phantoms.TTerm (Maybe Meta.Data) -> Phantoms.TTerm Meta.Case)
caseWithCond original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Case"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "pat"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Case"),
          Core.projectionField = (Core.Name "pat")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "cond"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "body"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Case"),
          Core.projectionField = (Core.Name "body")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

caseWithBody :: (Phantoms.TTerm Meta.Case -> Phantoms.TTerm Meta.Data -> Phantoms.TTerm Meta.Case)
caseWithBody original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Case"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "pat"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Case"),
          Core.projectionField = (Core.Name "pat")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "cond"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Case"),
          Core.projectionField = (Core.Name "cond")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "body"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

typeCase :: (Phantoms.TTerm Meta.Type -> Phantoms.TTerm Meta.Type -> Phantoms.TTerm Meta.TypeCase)
typeCase pat body = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.TypeCase"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "pat"),
      Core.fieldTerm = (Phantoms.unTTerm pat)},
    Core.Field {
      Core.fieldName = (Core.Name "body"),
      Core.fieldTerm = (Phantoms.unTTerm body)}]})))

typeCasePat :: (Phantoms.TTerm Meta.TypeCase -> Phantoms.TTerm Meta.Type)
typeCasePat x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.TypeCase"),
    Core.projectionField = (Core.Name "pat")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

typeCaseBody :: (Phantoms.TTerm Meta.TypeCase -> Phantoms.TTerm Meta.Type)
typeCaseBody x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.TypeCase"),
    Core.projectionField = (Core.Name "body")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

typeCaseWithPat :: (Phantoms.TTerm Meta.TypeCase -> Phantoms.TTerm Meta.Type -> Phantoms.TTerm Meta.TypeCase)
typeCaseWithPat original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.TypeCase"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "pat"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "body"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.TypeCase"),
          Core.projectionField = (Core.Name "body")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

typeCaseWithBody :: (Phantoms.TTerm Meta.TypeCase -> Phantoms.TTerm Meta.Type -> Phantoms.TTerm Meta.TypeCase)
typeCaseWithBody original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.TypeCase"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "pat"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.TypeCase"),
          Core.projectionField = (Core.Name "pat")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "body"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

source :: (Phantoms.TTerm [Meta.Stat] -> Phantoms.TTerm Meta.Source)
source stats = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Source"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "stats"),
      Core.fieldTerm = (Phantoms.unTTerm stats)}]})))

sourceStats :: (Phantoms.TTerm Meta.Source -> Phantoms.TTerm [Meta.Stat])
sourceStats x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.scala.meta.Source"),
    Core.projectionField = (Core.Name "stats")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

sourceWithStats :: (Phantoms.TTerm Meta.Source -> Phantoms.TTerm [Meta.Stat] -> Phantoms.TTerm Meta.Source)
sourceWithStats original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.scala.meta.Source"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "stats"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

quasi :: (Phantoms.TTerm () -> Phantoms.TTerm Meta.Quasi)
quasi x = (Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
  Core.wrappedTermTypeName = (Core.Name "hydra.ext.scala.meta.Quasi"),
  Core.wrappedTermBody = (Phantoms.unTTerm x)})))

unQuasi :: (Phantoms.TTerm Meta.Quasi -> Phantoms.TTerm ())
unQuasi x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.scala.meta.Quasi")))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))
