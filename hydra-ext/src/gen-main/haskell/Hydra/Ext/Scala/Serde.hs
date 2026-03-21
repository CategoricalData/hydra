-- Note: this is an automatically generated file. Do not edit.

-- | Serialization functions for converting Scala AST to abstract expressions

module Hydra.Ext.Scala.Serde where

import qualified Hydra.Ast as Ast
import qualified Hydra.Ext.Scala.Meta as Meta
import qualified Hydra.Lib.Equality as Equality
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Literals as Literals
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Lib.Math as Math
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Serialization as Serialization
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | The dot operator for member access
dotOp :: Ast.Op
dotOp =
    Ast.Op {
      Ast.opSymbol = (Ast.Symbol "."),
      Ast.opPadding = Ast.Padding {
        Ast.paddingLeft = Ast.WsNone,
        Ast.paddingRight = Ast.WsNone},
      Ast.opPrecedence = (Ast.Precedence 0),
      Ast.opAssociativity = Ast.AssociativityLeft}

-- | The function arrow operator (=>)
functionArrowOp :: Ast.Op
functionArrowOp = Serialization.op "=>" (Math.negate 1) Ast.AssociativityRight

-- | The match operator
matchOp :: Ast.Op
matchOp =
    Ast.Op {
      Ast.opSymbol = (Ast.Symbol "match"),
      Ast.opPadding = Ast.Padding {
        Ast.paddingLeft = Ast.WsSpace,
        Ast.paddingRight = (Ast.WsBreakAndIndent "  ")},
      Ast.opPrecedence = (Ast.Precedence 0),
      Ast.opAssociativity = Ast.AssociativityNone}

-- | Convert a case clause to an expression
writeCase :: Meta.Case -> Ast.Expr
writeCase c =

      let pat = Meta.casePat c
          term = Meta.caseBody c
      in (Serialization.spaceSep [
        Serialization.cst "case",
        (writePat pat),
        (Serialization.cst "=>"),
        (writeTerm term)])

-- | Convert a definition to an expression
writeDefn :: Meta.Defn -> Ast.Expr
writeDefn def =
    case def of
      Meta.DefnDef v0 ->
        let name = Meta.defn_DefName v0
            tparams = Meta.defn_DefTparams v0
            paramss = Meta.defn_DefParamss v0
            scod = Meta.defn_DefDecltpe v0
            body = Meta.defn_DefBody v0
            tparamsExpr =
                    Logic.ifElse (Lists.null tparams) Nothing (Maybes.pure (Serialization.bracketList Serialization.inlineStyle (Lists.map writeType_Param tparams)))
            scodExpr =
                    Maybes.map (\t -> Serialization.spaceSep [
                      Serialization.cst ":",
                      (writeType t)]) scod
            paramssExprs = Lists.map (\ps -> Serialization.parenList False (Lists.map writeData_Param ps)) paramss
            nameAndParams =
                    Serialization.noSep (Maybes.cat (Lists.concat [
                      [
                        Maybes.pure (writeData_Name name)],
                      [
                        tparamsExpr],
                      (Lists.map (\pe -> Maybes.pure pe) paramssExprs),
                      [
                        scodExpr]]))
            bodyExpr = writeTerm body
            defSig =
                    Serialization.spaceSep [
                      Serialization.cst "def",
                      nameAndParams,
                      (Serialization.cst "=")]
            bodyLen = Serialization.expressionLength bodyExpr
        in (Logic.ifElse (Equality.gt bodyLen 80) (Serialization.noSep [
          defSig,
          (Serialization.cst "\n  "),
          bodyExpr]) (Serialization.spaceSep [
          defSig,
          bodyExpr]))
      Meta.DefnType v0 ->
        let name = Meta.defn_TypeName v0
            tparams = Meta.defn_TypeTparams v0
            body = Meta.defn_TypeBody v0
        in (Serialization.spaceSep (Maybes.cat [
          Maybes.pure (Serialization.cst "type"),
          (Maybes.pure (writeType_Name name)),
          (Logic.ifElse (Lists.null tparams) Nothing (Maybes.pure (Serialization.bracketList Serialization.inlineStyle (Lists.map writeType_Param tparams)))),
          (Maybes.pure (Serialization.cst "=")),
          (Maybes.pure (writeType body))]))
      Meta.DefnVal v0 ->
        let pats = Meta.defn_ValPats v0
            typ = Meta.defn_ValDecltpe v0
            rhs = Meta.defn_ValRhs v0
            firstPat = Lists.head pats
            patName =
                    case firstPat of
                      Meta.PatVar v1 -> Meta.pat_VarName v1
            nameStr = Meta.unPredefString (Meta.data_NameValue patName)
            nameAndType =
                    Maybes.maybe (Serialization.cst nameStr) (\t -> Serialization.spaceSep [
                      Serialization.cst (Strings.cat2 nameStr ":"),
                      (writeType t)]) typ
        in (Serialization.spaceSep [
          Serialization.cst "val",
          nameAndType,
          (Serialization.cst "="),
          (writeTerm rhs)])
      Meta.DefnClass v0 ->
        let mods = Meta.defn_ClassMods v0
            name = Meta.defn_ClassName v0
            tparams = Meta.defn_ClassTparams v0
            ctor = Meta.defn_ClassCtor v0
            paramss = Meta.ctor_PrimaryParamss ctor
            tparamsExpr =
                    Logic.ifElse (Lists.null tparams) Nothing (Maybes.pure (Serialization.bracketList Serialization.inlineStyle (Lists.map writeType_Param tparams)))
            paramsExpr =
                    Logic.ifElse (Lists.null paramss) Nothing (Maybes.pure (Serialization.parenList False (Lists.map writeData_Param (Lists.concat paramss))))
            nameAndParams =
                    Serialization.noSep (Maybes.cat [
                      Maybes.pure (writeType_Name name),
                      tparamsExpr,
                      paramsExpr])
        in (Serialization.spaceSep (Lists.concat [
          Lists.map writeMod mods,
          [
            Serialization.cst "class",
            nameAndParams]]))
      Meta.DefnEnum v0 ->
        let name = Meta.defn_EnumName v0
            tparams = Meta.defn_EnumTparams v0
            template = Meta.defn_EnumTemplate v0
            stats = Meta.templateStats template
            enumHeader =
                    Serialization.spaceSep [
                      Serialization.cst "enum",
                      (Serialization.noSep (Maybes.cat [
                        Maybes.pure (writeType_Name name),
                        (Logic.ifElse (Lists.null tparams) Nothing (Maybes.pure (Serialization.bracketList Serialization.inlineStyle (Lists.map writeType_Param tparams))))])),
                      (Serialization.cst ":")]
            enumCases =
                    Lists.map (\s -> Serialization.spaceSep [
                      Serialization.cst "  ",
                      (writeStat s)]) stats
        in (Serialization.newlineSep (Lists.concat [
          [
            enumHeader],
          enumCases]))
      Meta.DefnEnumCase v0 ->
        let name = Meta.defn_EnumCaseName v0
            ctor = Meta.defn_EnumCaseCtor v0
            inits = Meta.defn_EnumCaseInits v0
            paramss = Meta.ctor_PrimaryParamss ctor
            allParams = Lists.concat paramss
            params =
                    Logic.ifElse (Lists.null allParams) (Serialization.cst "") (Serialization.parenList False (Lists.map writeData_Param allParams))
            extendsClause =
                    Logic.ifElse (Lists.null inits) (Serialization.cst "") (Serialization.spaceSep [
                      Serialization.cst "extends",
                      (Serialization.commaSep Serialization.inlineStyle (Lists.map writeInit inits))])
        in (Serialization.spaceSep [
          Serialization.cst "case",
          (Serialization.noSep [
            writeData_Name name,
            params]),
          extendsClause])

-- | Convert an import/export statement to an expression
writeImportExportStat :: Meta.ImportExportStat -> Ast.Expr
writeImportExportStat ie =
    case ie of
      Meta.ImportExportStatImport v0 ->
        let importers = Meta.importImporters v0
        in (Serialization.newlineSep (Lists.map writeImporter importers))

-- | Convert an importer to an expression
writeImporter :: Meta.Importer -> Ast.Expr
writeImporter imp =

      let ref = Meta.importerRef imp
          importees = Meta.importerImportees imp
          refName =
                  case ref of
                    Meta.Data_RefName v0 -> Meta.unPredefString (Meta.data_NameValue v0)
          forImportees =
                  Logic.ifElse (Lists.null importees) (Serialization.cst "") (Logic.ifElse (Equality.equal (Lists.length importees) 1) (Serialization.noSep [
                    Serialization.cst ".",
                    case (Lists.head importees) of
                      Meta.ImporteeWildcard -> Serialization.cst "*"
                      Meta.ImporteeName v0 -> Serialization.cst (case (Meta.importee_NameName v0) of
                        Meta.NameValue v1 -> v1)]) (Serialization.noSep [
                    Serialization.cst ".",
                    (Serialization.curlyBracesList Nothing Serialization.inlineStyle (Lists.map (\it -> case it of
                      Meta.ImporteeWildcard -> Serialization.cst "*"
                      Meta.ImporteeName v0 -> Serialization.cst (case (Meta.importee_NameName v0) of
                        Meta.NameValue v1 -> v1)) importees))]))
      in (Serialization.spaceSep [
        Serialization.cst "import",
        (Serialization.noSep [
          Serialization.cst refName,
          forImportees])])

-- | Convert a literal to an expression
writeLit :: Meta.Lit -> Ast.Expr
writeLit lit =
    case lit of
      Meta.LitBoolean v0 -> Serialization.cst (Logic.ifElse v0 "true" "false")
      Meta.LitByte v0 -> Serialization.cst (Strings.cat2 (Literals.showInt8 v0) ".toByte")
      Meta.LitShort v0 -> Serialization.cst (Strings.cat2 (Literals.showInt16 v0) ".toShort")
      Meta.LitInt v0 -> Serialization.cst (Literals.showInt32 v0)
      Meta.LitLong v0 -> Serialization.cst (Strings.cat2 (Literals.showInt64 v0) "L")
      Meta.LitFloat v0 -> Serialization.cst (Strings.cat2 (Literals.showFloat32 v0) "f")
      Meta.LitDouble v0 -> Serialization.cst (Literals.showFloat64 v0)
      Meta.LitUnit -> Serialization.cst "()"
      Meta.LitString v0 -> Serialization.cst (Literals.showString v0)
      _ -> Serialization.cst "TODO:literal"

-- | Convert a name to an expression
writeName :: Meta.Name -> Ast.Expr
writeName name =
    case name of
      Meta.NameValue v0 -> Serialization.cst v0

-- | Convert a pattern to an expression
writePat :: Meta.Pat -> Ast.Expr
writePat pat =
    case pat of
      Meta.PatExtract v0 ->
        let fun = Meta.pat_ExtractFun v0
            args = Meta.pat_ExtractArgs v0
        in (Logic.ifElse (Lists.null args) (writeTerm fun) (Serialization.noSep [
          writeTerm fun,
          (Serialization.parenList False (Lists.map writePat args))]))
      Meta.PatVar v0 -> writeData_Name (Meta.pat_VarName v0)
      Meta.PatWildcard -> Serialization.cst "_"

-- | Convert a package to an expression
writePkg :: Meta.Pkg -> Ast.Expr
writePkg pkg =

      let name = Meta.pkgName pkg
          stats = Meta.pkgStats pkg
          package =
                  Serialization.spaceSep [
                    Serialization.cst "package",
                    (writeData_Name name)]
      in (Serialization.doubleNewlineSep (Lists.concat [
        [
          package],
        (Lists.map writeStat stats)]))

-- | Convert a statement to an expression
writeStat :: Meta.Stat -> Ast.Expr
writeStat stat =
    case stat of
      Meta.StatTerm v0 -> writeTerm v0
      Meta.StatDefn v0 -> writeDefn v0
      Meta.StatImportExport v0 -> writeImportExportStat v0

-- | Convert a term to an expression
writeTerm :: Meta.Data -> Ast.Expr
writeTerm term =
    case term of
      Meta.DataLit v0 -> writeLit v0
      Meta.DataRef v0 -> writeData_Ref v0
      Meta.DataApply v0 ->
        let fun = Meta.data_ApplyFun v0
            args = Meta.data_ApplyArgs v0
        in (Serialization.noSep [
          writeTerm fun,
          (Serialization.parenList False (Lists.map writeTerm args))])
      Meta.DataAssign v0 ->
        let lhs = Meta.data_AssignLhs v0
            rhs = Meta.data_AssignRhs v0
        in (Serialization.spaceSep [
          writeTerm lhs,
          (Serialization.cst "->"),
          (writeTerm rhs)])
      Meta.DataTuple v0 -> Serialization.parenList False (Lists.map writeTerm (Meta.data_TupleArgs v0))
      Meta.DataMatch v0 ->
        let expr = Meta.data_MatchExpr v0
            mCases = Meta.data_MatchCases v0
        in (Serialization.ifx matchOp (writeTerm expr) (Serialization.newlineSep (Lists.map writeCase mCases)))
      Meta.DataFunctionData v0 -> writeData_FunctionData v0
      Meta.DataBlock v0 ->
        let stats = Meta.data_BlockStats v0
        in (Serialization.curlyBlock Serialization.fullBlockStyle (Serialization.newlineSep (Lists.map writeStat stats)))

-- | Convert function data to an expression
writeData_FunctionData :: Meta.Data_FunctionData -> Ast.Expr
writeData_FunctionData ft =
    case ft of
      Meta.Data_FunctionDataFunction v0 ->
        let params = Meta.data_FunctionParams v0
            body = Meta.data_FunctionBody v0
            bodyExpr = writeTerm body
            bodyLen = Serialization.expressionLength bodyExpr
        in (Logic.ifElse (Equality.gt bodyLen 60) (Serialization.noSep [
          Serialization.parenList False (Lists.map writeData_Param params),
          (Serialization.cst " =>\n  "),
          bodyExpr]) (Serialization.spaceSep [
          Serialization.parenList False (Lists.map writeData_Param params),
          (Serialization.cst "=>"),
          bodyExpr]))

-- | Convert a data name to an expression
writeData_Name :: Meta.Data_Name -> Ast.Expr
writeData_Name dn = Serialization.cst (Meta.unPredefString (Meta.data_NameValue dn))

-- | Convert a data parameter to an expression
writeData_Param :: Meta.Data_Param -> Ast.Expr
writeData_Param dp =

      let name = Meta.data_ParamName dp
          stype = Meta.data_ParamDecltpe dp
      in (Serialization.noSep (Maybes.cat [
        Maybes.pure (writeName name),
        (Maybes.map (\t -> Serialization.spaceSep [
          Serialization.cst ":",
          (writeType t)]) stype)]))

-- | Convert a data reference to an expression
writeData_Ref :: Meta.Data_Ref -> Ast.Expr
writeData_Ref ref =
    case ref of
      Meta.Data_RefName v0 -> writeData_Name v0
      Meta.Data_RefSelect v0 -> writeData_Select v0

-- | Convert a data select to an expression
writeData_Select :: Meta.Data_Select -> Ast.Expr
writeData_Select sel =

      let arg = Meta.data_SelectQual sel
          name = Meta.data_SelectName sel
      in (Serialization.ifx dotOp (writeTerm arg) (writeTerm (Meta.DataRef (Meta.Data_RefName name))))

-- | Convert a type to an expression
writeType :: Meta.Type -> Ast.Expr
writeType typ =
    case typ of
      Meta.TypeRef v0 -> case v0 of
        Meta.Type_RefName v1 -> writeType_Name v1
      Meta.TypeApply v0 ->
        let fun = Meta.type_ApplyTpe v0
            args = Meta.type_ApplyArgs v0
        in (Serialization.noSep [
          writeType fun,
          (Serialization.bracketList Serialization.inlineStyle (Lists.map writeType args))])
      Meta.TypeFunctionType v0 -> case v0 of
        Meta.Type_FunctionTypeFunction v1 ->
          let dom = Lists.head (Meta.type_FunctionParams v1)
              cod = Meta.type_FunctionRes v1
          in (Serialization.ifx functionArrowOp (writeType dom) (writeType cod))
      Meta.TypeLambda v0 ->
        let params = Meta.type_LambdaTparams v0
            body = Meta.type_LambdaTpe v0
        in (Serialization.noSep [
          writeType body,
          (Serialization.bracketList Serialization.inlineStyle (Lists.map writeType_Param params))])
      Meta.TypeVar v0 -> writeType_Name (Meta.type_VarName v0)

-- | Convert a type name to an expression
writeType_Name :: Meta.Type_Name -> Ast.Expr
writeType_Name tn = Serialization.cst (Meta.type_NameValue tn)

-- | Convert a type parameter to an expression
writeType_Param :: Meta.Type_Param -> Ast.Expr
writeType_Param tp = writeName (Meta.type_ParamName tp)

-- | Convert an init to an expression
writeInit :: Meta.Init -> Ast.Expr
writeInit init = writeType (Meta.initTpe init)

-- | Convert a modifier to an expression
writeMod :: Meta.Mod -> Ast.Expr
writeMod m =
    case m of
      Meta.ModCase -> Serialization.cst "case"
      Meta.ModSealed -> Serialization.cst "sealed"
      Meta.ModAbstract -> Serialization.cst "abstract"
      Meta.ModFinal -> Serialization.cst "final"
      Meta.ModOverride -> Serialization.cst "override"
      Meta.ModImplicit -> Serialization.cst "implicit"
      Meta.ModLazy -> Serialization.cst "lazy"
      Meta.ModPrivate _ -> Serialization.cst "private"
      Meta.ModProtected _ -> Serialization.cst "protected"
