module Hydra.Ext.Staging.Scala.Serde where

import Hydra.Kernel
import Hydra.Ast
import Hydra.Serialization
import qualified Hydra.Lib.Literals as Literals
import qualified Hydra.Ext.Scala.Meta as Scala

import qualified Data.List as L
import qualified Data.Maybe as Y


dotOp :: Op
dotOp = Op (Symbol ".") (Padding WsNone WsNone) (Precedence 0) AssociativityLeft

functionArrowOp :: Op
functionArrowOp = op "=>" (negate 1) AssociativityRight

matchOp :: Op
matchOp = Op (Symbol "match") (Padding WsSpace $ WsBreakAndIndent "  ") (Precedence 0) AssociativityNone

writeCase :: Scala.Case -> Expr
writeCase (Scala.Case pat _ term) = spaceSep [cst "case", writePat pat, cst "=>", writeTerm term]

writeDefn :: Scala.Defn -> Expr
writeDefn def = case def of
  Scala.DefnDef (Scala.Defn_Def _ name tparams [params] scod body) -> spaceSep [
      cst "def", nameAndParams, cst "=", writeTerm body]
    where
      nameAndParams = noSep $ Y.catMaybes [
        Just $ writeData_Name name,
        if L.null tparams then Nothing else Just $ bracketList inlineStyle (writeType_Param <$> tparams),
        Just $ parenList False (writeData_Param <$> params),
        fmap (\t -> spaceSep [cst ":", writeType t]) scod]
  Scala.DefnVal (Scala.Defn_Val _ [Scala.PatVar (Scala.Pat_Var (Scala.Data_Name (Scala.PredefString name)))] typ term) -> spaceSep [
      cst "val", nameAndType, cst "=", writeTerm term]
    where
      nameAndType = Y.maybe (cst name) (\t -> spaceSep [cst $ name ++ ":", writeType t]) typ

writeImportExportStat :: Scala.ImportExportStat -> Expr
writeImportExportStat ie = case ie of
  Scala.ImportExportStatImport (Scala.Import importers) -> newlineSep (writeImporter <$> importers)
--  Scala.ImportExportStatExport exp ->

writeImporter :: Scala.Importer -> Expr
writeImporter (Scala.Importer (Scala.Data_RefName (Scala.Data_Name (Scala.PredefString ref))) importees) = spaceSep [
    cst "import", noSep [cst ref, forImportees importees]]
  where
    forImportee it = cst $ case it of
      Scala.ImporteeWildcard -> "*"
      Scala.ImporteeName (Scala.Importee_Name (Scala.NameValue name)) -> name
    forImportees its = if L.null its
      then cst ""
      else if L.length its == 1
      then noSep [cst ".", forImportee $ L.head its]
      else noSep [cst ".", curlyBracesList Nothing inlineStyle (forImportee <$> its)]
writeLit :: Scala.Lit -> Expr
writeLit lit = case lit of
--  Scala.LitNull
  Scala.LitInt i -> cst $ Literals.showInt32 i
--  Scala.LitDouble Double
--  Scala.LitFloat Float
--  Scala.LitByte Integer
--  Scala.LitShort Integer
--  Scala.LitChar Integer
--  Scala.LitLong Int64
  Scala.LitBoolean b -> cst $ if b then "true" else "false"
  Scala.LitUnit -> cst "()"
  Scala.LitString s -> cst $ Literals.showString s
--  Scala.LitSymbol sym ->
  _ -> cst $ Literals.showString $ "TODO:literal:" ++ show lit

writeName :: Scala.Name -> Expr
writeName name = case name of
  Scala.NameValue s -> cst s

writePat :: Scala.Pat -> Expr
writePat pat = case pat of
  Scala.PatExtract (Scala.Pat_Extract fun args) -> noSep [writeTerm fun, parenList False (writePat <$> args)]
  Scala.PatVar (Scala.Pat_Var tname) -> writeData_Name tname

writePkg :: Scala.Pkg -> Expr
writePkg (Scala.Pkg name _ stats) = doubleNewlineSep $ package:(writeStat <$> stats)
  where
    package = spaceSep [cst "package", writeData_Name name]

writeStat :: Scala.Stat -> Expr
writeStat stat = case stat of
--  Scala.StatTerm Term ->
--  Scala.StatDecl Decl ->
  Scala.StatDefn def -> writeDefn def
  Scala.StatImportExport ie -> writeImportExportStat ie

writeTerm :: Scala.Data -> Expr
writeTerm term = case term of
  Scala.DataLit lit -> writeLit lit
  Scala.DataRef ref -> writeData_Ref ref
  Scala.DataApply (Scala.Data_Apply fun args) -> noSep [writeTerm fun, parenList False (writeTerm <$> args)]
  Scala.DataAssign assign -> cst ">ASSIGN"
  Scala.DataTuple (Scala.Data_Tuple args) -> parenList False (writeTerm <$> args)
  Scala.DataMatch (Scala.Data_Match expr cases) -> ifx matchOp (writeTerm expr) $ newlineSep (writeCase <$> cases)
  Scala.DataFunctionData ft -> writeData_FunctionData ft

writeData_FunctionData :: Scala.Data_FunctionData -> Expr
writeData_FunctionData ft = case ft of
  Scala.Data_FunctionDataFunction (Scala.Data_Function params body) ->
    spaceSep [parenList False (writeData_Param <$> params), cst "=>", writeTerm body]

writeData_Name :: Scala.Data_Name -> Expr
writeData_Name (Scala.Data_Name (Scala.PredefString name)) = cst name

writeData_Param :: Scala.Data_Param -> Expr
writeData_Param (Scala.Data_Param _ name stype _) = noSep $ Y.catMaybes [
  Just $ writeName name,
  fmap (\t -> spaceSep [cst ":", writeType t]) stype]

writeData_Ref :: Scala.Data_Ref -> Expr
writeData_Ref ref = case ref of
  Scala.Data_RefName name -> writeData_Name name
  Scala.Data_RefSelect sel -> writeData_Select sel

writeData_Select :: Scala.Data_Select -> Expr
writeData_Select (Scala.Data_Select arg name) = ifx dotOp (writeTerm arg) (writeTerm proj)
  where
    proj = Scala.DataRef $ Scala.Data_RefName name

writeType :: Scala.Type -> Expr
writeType typ = case typ of
  Scala.TypeRef (Scala.Type_RefName name) -> writeType_Name name
  Scala.TypeApply (Scala.Type_Apply fun args) -> noSep [writeType fun, bracketList inlineStyle (writeType <$> args)]
  Scala.TypeFunctionType (Scala.Type_FunctionTypeFunction (Scala.Type_Function [dom] cod)) -> ifx functionArrowOp (writeType dom) (writeType cod)
  Scala.TypeLambda (Scala.Type_Lambda params body) -> noSep [writeType body, bracketList inlineStyle (writeType_Param <$> params)]
  Scala.TypeVar (Scala.Type_Var name) -> writeType_Name name
  _ -> cst $ "UNKNOWN TYPE: " ++ show typ

writeType_Name :: Scala.Type_Name -> Expr
writeType_Name (Scala.Type_Name name) = cst name

writeType_Param :: Scala.Type_Param -> Expr
writeType_Param (Scala.Type_Param [] n [] [] [] []) = writeName n
