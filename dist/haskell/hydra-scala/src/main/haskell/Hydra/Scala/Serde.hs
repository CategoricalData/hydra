-- Note: this is an automatically generated file. Do not edit.
-- | Serialization functions for converting Scala AST to abstract expressions

module Hydra.Scala.Serde where
import qualified Hydra.Ast as Ast
import qualified Hydra.Coders as Coders
import qualified Hydra.Core as Core
import qualified Hydra.Error.Checking as Checking
import qualified Hydra.Error.Core as ErrorCore
import qualified Hydra.Error.Packaging as ErrorPackaging
import qualified Hydra.Errors as Errors
import qualified Hydra.Graph as Graph
import qualified Hydra.Java.Serde as Serde
import qualified Hydra.Json.Model as Model
import qualified Hydra.Haskell.Lib.Equality as Equality
import qualified Hydra.Haskell.Lib.Lists as Lists
import qualified Hydra.Haskell.Lib.Literals as Literals
import qualified Hydra.Haskell.Lib.Logic as Logic
import qualified Hydra.Haskell.Lib.Math as Math
import qualified Hydra.Haskell.Lib.Maybes as Maybes
import qualified Hydra.Haskell.Lib.Strings as Strings
import qualified Hydra.Packaging as Packaging
import qualified Hydra.Parsing as Parsing
import qualified Hydra.Paths as Paths
import qualified Hydra.Query as Query
import qualified Hydra.Relational as Relational
import qualified Hydra.Scala.Syntax as Syntax
import qualified Hydra.Serialization as Serialization
import qualified Hydra.Tabular as Tabular
import qualified Hydra.Testing as Testing
import qualified Hydra.Topology as Topology
import qualified Hydra.Typed as Typed
import qualified Hydra.Typing as Typing
import qualified Hydra.Util as Util
import qualified Hydra.Validation as Validation
import qualified Hydra.Variants as Variants
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
-- | Convert a case clause to an expression
caseToExpr :: Syntax.Case -> Ast.Expr
caseToExpr c =

      let pat = Syntax.casePat c
          term = Syntax.caseBody c
      in (Serialization.spaceSep [
        Serialization.cst "case",
        (patToExpr pat),
        (Serialization.cst "=>"),
        (termToExpr term)])
-- | Convert a function-data lambda to an expression
dataFunctionToExpr :: Syntax.FunctionData -> Ast.Expr
dataFunctionToExpr f =

      let params = Syntax.functionDataParams f
          body = Syntax.functionDataBody f
          bodyExpr = termToExpr body
          bodyLen = Serialization.expressionLength bodyExpr
      in (Logic.ifElse (Equality.gt bodyLen 60) (Serialization.noSep [
        Serialization.parenListAdaptive (Lists.map dataParamToExpr params),
        (Serialization.cst " =>\n  "),
        bodyExpr]) (Serialization.spaceSep [
        Serialization.parenListAdaptive (Lists.map dataParamToExpr params),
        (Serialization.cst "=>"),
        bodyExpr]))
-- | Convert a data name to an expression
dataNameToExpr :: Syntax.NameData -> Ast.Expr
dataNameToExpr dn = Serialization.cst (Syntax.unPredefString (Syntax.nameDataValue dn))
-- | Convert a data parameter to an expression
dataParamToExpr :: Syntax.ParamData -> Ast.Expr
dataParamToExpr dp =

      let name = Syntax.paramDataName dp
          stype = Syntax.paramDataDecltpe dp
      in (Serialization.noSep (Maybes.cat [
        Maybes.pure (nameToExpr name),
        (Maybes.map (\t -> Serialization.spaceSep [
          Serialization.cst ":",
          (typeToExpr t)]) stype)]))
-- | Convert a data reference to an expression
dataRefToExpr :: Syntax.RefData -> Ast.Expr
dataRefToExpr ref =
    case ref of
      Syntax.RefDataName v0 -> dataNameToExpr v0
      Syntax.RefDataSelect v0 -> dataSelectToExpr v0
-- | Convert a data select to an expression
dataSelectToExpr :: Syntax.SelectData -> Ast.Expr
dataSelectToExpr sel =

      let arg = Syntax.selectDataQual sel
          name = Syntax.selectDataName sel
      in (Serialization.ifx dotOp (termToExpr arg) (termToExpr (Syntax.DataRef (Syntax.RefDataName name))))
-- | Convert a definition to an expression
defnToExpr :: Syntax.Defn -> Ast.Expr
defnToExpr def =
    case def of
      Syntax.DefnDef v0 ->
        let name = Syntax.defDefnName v0
            tparams = Syntax.defDefnTparams v0
            paramss = Syntax.defDefnParamss v0
            scod = Syntax.defDefnDecltpe v0
            body = Syntax.defDefnBody v0
            tparamsExpr =
                    Logic.ifElse (Lists.null tparams) Nothing (Maybes.pure (Serialization.bracketList Serialization.inlineStyle (Lists.map typeParamToExpr tparams)))
            scodExpr =
                    Maybes.map (\t -> Serialization.spaceSep [
                      Serialization.cst ":",
                      (typeToExpr t)]) scod
            paramssExprs = Lists.map (\ps -> Serialization.parenListAdaptive (Lists.map dataParamToExpr ps)) paramss
            nameAndParams =
                    Serialization.noSep (Maybes.cat (Lists.concat [
                      [
                        Maybes.pure (dataNameToExpr name)],
                      [
                        tparamsExpr],
                      (Lists.map (\pe -> Maybes.pure pe) paramssExprs),
                      [
                        scodExpr]]))
            bodyExpr = termToExpr body
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
      Syntax.DefnType v0 ->
        let name = Syntax.typeDefnName v0
            tparams = Syntax.typeDefnTparams v0
            body = Syntax.typeDefnBody v0
        in (Serialization.spaceSep (Maybes.cat [
          Maybes.pure (Serialization.cst "type"),
          (Maybes.pure (typeNameToExpr name)),
          (Logic.ifElse (Lists.null tparams) Nothing (Maybes.pure (Serialization.bracketList Serialization.inlineStyle (Lists.map typeParamToExpr tparams)))),
          (Maybes.pure (Serialization.cst "=")),
          (Maybes.pure (typeToExpr body))]))
      Syntax.DefnVal v0 ->
        let mods = Syntax.valDefnMods v0
            pats = Syntax.valDefnPats v0
            typ = Syntax.valDefnDecltpe v0
            rhs = Syntax.valDefnRhs v0
            nameStr =
                    Maybes.fromMaybe "" (Maybes.map (\firstPat ->
                      let patName =
                              case firstPat of
                                Syntax.PatVar v1 -> Syntax.varPatName v1
                      in (Syntax.unPredefString (Syntax.nameDataValue patName))) (Lists.maybeHead pats))
            nameAndType =
                    Maybes.maybe (Serialization.cst nameStr) (\t -> Serialization.spaceSep [
                      Serialization.cst (Strings.cat2 nameStr ":"),
                      (typeToExpr t)]) typ
            valKeyword = Logic.ifElse (Lists.null mods) "val" "lazy val"
        in (Serialization.spaceSep [
          Serialization.cst valKeyword,
          nameAndType,
          (Serialization.cst "="),
          (termToExpr rhs)])
      Syntax.DefnClass v0 ->
        let mods = Syntax.classDefnMods v0
            name = Syntax.classDefnName v0
            tparams = Syntax.classDefnTparams v0
            ctor = Syntax.classDefnCtor v0
            paramss = Syntax.primaryCtorParamss ctor
            tparamsExpr =
                    Logic.ifElse (Lists.null tparams) Nothing (Maybes.pure (Serialization.bracketList Serialization.inlineStyle (Lists.map typeParamToExpr tparams)))
            paramsExpr =
                    Logic.ifElse (Lists.null paramss) Nothing (Maybes.pure (Serialization.parenListAdaptive (Lists.map dataParamToExpr (Lists.concat paramss))))
            nameAndParams =
                    Serialization.noSep (Maybes.cat [
                      Maybes.pure (typeNameToExpr name),
                      tparamsExpr,
                      paramsExpr])
        in (Serialization.spaceSep (Lists.concat [
          Lists.map modToExpr mods,
          [
            Serialization.cst "class",
            nameAndParams]]))
      Syntax.DefnEnum v0 ->
        let name = Syntax.enumDefnName v0
            tparams = Syntax.enumDefnTparams v0
            template = Syntax.enumDefnTemplate v0
            stats = Syntax.templateStats template
            enumHeader =
                    Serialization.spaceSep [
                      Serialization.cst "enum",
                      (Serialization.noSep (Maybes.cat [
                        Maybes.pure (typeNameToExpr name),
                        (Logic.ifElse (Lists.null tparams) Nothing (Maybes.pure (Serialization.bracketList Serialization.inlineStyle (Lists.map typeParamToExpr tparams))))])),
                      (Serialization.cst ":")]
            enumCases =
                    Lists.map (\s -> Serialization.spaceSep [
                      Serialization.cst "  ",
                      (statToExpr s)]) stats
        in (Serialization.newlineSep (Lists.concat [
          [
            enumHeader],
          enumCases]))
      Syntax.DefnEnumCase v0 ->
        let name = Syntax.enumCaseDefnName v0
            ctor = Syntax.enumCaseDefnCtor v0
            inits = Syntax.enumCaseDefnInits v0
            paramss = Syntax.primaryCtorParamss ctor
            allParams = Lists.concat paramss
            params =
                    Logic.ifElse (Lists.null allParams) (Serialization.cst "") (Serialization.parenListAdaptive (Lists.map dataParamToExpr allParams))
            extendsClause =
                    Logic.ifElse (Lists.null inits) (Serialization.cst "") (Serialization.spaceSep [
                      Serialization.cst "extends",
                      (Serialization.commaSep Serialization.inlineStyle (Lists.map initToExpr inits))])
        in (Serialization.spaceSep [
          Serialization.cst "case",
          (Serialization.noSep [
            dataNameToExpr name,
            params]),
          extendsClause])
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
-- | Convert an import/export statement to an expression
importExportStatToExpr :: Syntax.ImportExportStat -> Ast.Expr
importExportStatToExpr ie =
    case ie of
      Syntax.ImportExportStatImport v0 ->
        let importers = Syntax.importImporters v0
        in (Serialization.newlineSep (Lists.map importerToExpr importers))
-- | Convert an importer to an expression
importerToExpr :: Syntax.Importer -> Ast.Expr
importerToExpr imp =

      let ref = Syntax.importerRef imp
          importees = Syntax.importerImportees imp
          refName =
                  case ref of
                    Syntax.RefDataName v0 -> Syntax.unPredefString (Syntax.nameDataValue v0)
          forImportees =
                  Logic.ifElse (Lists.null importees) (Serialization.cst "") (Logic.ifElse (Equality.equal (Lists.length importees) 1) (Maybes.fromMaybe (Serialization.cst "") (Maybes.map (\firstImp -> Serialization.noSep [
                    Serialization.cst ".",
                    case firstImp of
                      Syntax.ImporteeWildcard -> Serialization.cst "*"
                      Syntax.ImporteeName v0 -> Serialization.cst (case (Syntax.nameImporteeName v0) of
                        Syntax.NameValue v1 -> v1)]) (Lists.maybeHead importees))) (Serialization.noSep [
                    Serialization.cst ".",
                    (Serialization.curlyBracesList Nothing Serialization.inlineStyle (Lists.map (\it -> case it of
                      Syntax.ImporteeWildcard -> Serialization.cst "*"
                      Syntax.ImporteeName v0 -> Serialization.cst (case (Syntax.nameImporteeName v0) of
                        Syntax.NameValue v1 -> v1)) importees))]))
      in (Serialization.spaceSep [
        Serialization.cst "import",
        (Serialization.noSep [
          Serialization.cst refName,
          forImportees])])
-- | Convert an init to an expression
initToExpr :: Syntax.Init -> Ast.Expr
initToExpr init = typeToExpr (Syntax.initTpe init)
-- | Convert a literal to an expression
litToExpr :: Syntax.Lit -> Ast.Expr
litToExpr lit =
    case lit of
      Syntax.LitBoolean v0 -> Serialization.cst (Logic.ifElse v0 "true" "false")
      Syntax.LitByte v0 -> Serialization.cst (Strings.cat2 (Literals.showInt8 v0) ".toByte")
      Syntax.LitShort v0 -> Serialization.cst (Strings.cat2 (Literals.showInt16 v0) ".toShort")
      Syntax.LitInt v0 -> Serialization.cst (Literals.showInt32 v0)
      Syntax.LitLong v0 -> Serialization.cst (Strings.cat2 (Literals.showInt64 v0) "L")
      Syntax.LitFloat v0 -> Serialization.cst (scalaFloatLiteralText "Float" "f" (Literals.showFloat32 v0))
      Syntax.LitDouble v0 -> Serialization.cst (scalaFloatLiteralText "Double" "" (Literals.showFloat64 v0))
      Syntax.LitUnit -> Serialization.cst "()"
      Syntax.LitString v0 -> Serialization.cst (Strings.cat2 "\"" (Strings.cat2 (Serde.escapeJavaString v0) "\""))
      Syntax.LitBytes v0 -> Serialization.cst (Strings.cat2 "Array[Byte](" (Strings.cat2 (Strings.intercalate ", " (Lists.map (\b -> Strings.cat2 (Literals.showInt32 b) ".toByte") v0)) ")"))
      _ -> Serialization.cst "TODO:literal"
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
-- | Convert a modifier to an expression
modToExpr :: Syntax.Mod -> Ast.Expr
modToExpr m =
    case m of
      Syntax.ModCase -> Serialization.cst "case"
      Syntax.ModSealed -> Serialization.cst "sealed"
      Syntax.ModAbstract -> Serialization.cst "abstract"
      Syntax.ModFinal -> Serialization.cst "final"
      Syntax.ModOverride -> Serialization.cst "override"
      Syntax.ModImplicit -> Serialization.cst "implicit"
      Syntax.ModLazy -> Serialization.cst "lazy"
      Syntax.ModPrivate _ -> Serialization.cst "private"
      Syntax.ModProtected _ -> Serialization.cst "protected"
-- | Convert a name to an expression
nameToExpr :: Syntax.Name -> Ast.Expr
nameToExpr name =
    case name of
      Syntax.NameValue v0 -> Serialization.cst v0
-- | Convert a pattern to an expression
patToExpr :: Syntax.Pat -> Ast.Expr
patToExpr pat =
    case pat of
      Syntax.PatExtract v0 ->
        let fun = Syntax.extractPatFun v0
            args = Syntax.extractPatArgs v0
        in (Logic.ifElse (Lists.null args) (termToExpr fun) (Serialization.noSep [
          termToExpr fun,
          (Serialization.parenListAdaptive (Lists.map patToExpr args))]))
      Syntax.PatVar v0 -> dataNameToExpr (Syntax.varPatName v0)
      Syntax.PatWildcard -> Serialization.cst "_"
-- | Convert a package to an expression
pkgToExpr :: Syntax.Pkg -> Ast.Expr
pkgToExpr pkg =

      let name = Syntax.pkgName pkg
          stats = Syntax.pkgStats pkg
          package =
                  Serialization.spaceSep [
                    Serialization.cst "package",
                    (dataNameToExpr name)]
      in (Serialization.doubleNewlineSep (Lists.concat [
        [
          package],
        (Lists.map statToExpr stats)]))
scalaFloatLiteralText :: String -> String -> String -> String
scalaFloatLiteralText prefix suffix s =
    Logic.ifElse (Equality.equal s "NaN") (Strings.cat2 prefix ".NaN") (Logic.ifElse (Equality.equal s "Infinity") (Strings.cat2 prefix ".PositiveInfinity") (Logic.ifElse (Equality.equal s "-Infinity") (Strings.cat2 prefix ".NegativeInfinity") (Strings.cat2 s suffix)))
-- | Convert a statement to an expression
statToExpr :: Syntax.Stat -> Ast.Expr
statToExpr stat =
    case stat of
      Syntax.StatTerm v0 -> termToExpr v0
      Syntax.StatDefn v0 -> defnToExpr v0
      Syntax.StatImportExport v0 -> importExportStatToExpr v0
-- | Convert a term to an expression
termToExpr :: Syntax.Data -> Ast.Expr
termToExpr term =
    case term of
      Syntax.DataLit v0 -> litToExpr v0
      Syntax.DataRef v0 -> dataRefToExpr v0
      Syntax.DataApply v0 ->
        let fun = Syntax.applyDataFun v0
            args = Syntax.applyDataArgs v0
        in (Serialization.noSep [
          termToExpr fun,
          (Serialization.parenListAdaptive (Lists.map termToExpr args))])
      Syntax.DataAssign v0 ->
        let lhs = Syntax.assignDataLhs v0
            rhs = Syntax.assignDataRhs v0
        in (Serialization.spaceSep [
          termToExpr lhs,
          (Serialization.cst "->"),
          (termToExpr rhs)])
      Syntax.DataTuple v0 -> Serialization.parenListAdaptive (Lists.map termToExpr (Syntax.tupleDataArgs v0))
      Syntax.DataMatch v0 ->
        let expr = Syntax.matchDataExpr v0
            mCases = Syntax.matchDataCases v0
        in (Serialization.ifx matchOp (termToExpr expr) (Serialization.newlineSep (Lists.map caseToExpr mCases)))
      Syntax.DataFunction v0 -> dataFunctionToExpr v0
      Syntax.DataBlock v0 ->
        let stats = Syntax.blockDataStats v0
        in (Serialization.curlyBlock Serialization.fullBlockStyle (Serialization.newlineSep (Lists.map statToExpr stats)))
-- | Convert a type name to an expression
typeNameToExpr :: Syntax.NameType -> Ast.Expr
typeNameToExpr tn = Serialization.cst (Syntax.nameTypeValue tn)
-- | Convert a type parameter to an expression
typeParamToExpr :: Syntax.ParamType -> Ast.Expr
typeParamToExpr tp = nameToExpr (Syntax.paramTypeName tp)
-- | Convert a type to an expression
typeToExpr :: Syntax.Type -> Ast.Expr
typeToExpr typ =
    case typ of
      Syntax.TypeRef v0 -> case v0 of
        Syntax.RefTypeName v1 -> typeNameToExpr v1
      Syntax.TypeApply v0 ->
        let fun = Syntax.applyTypeTpe v0
            args = Syntax.applyTypeArgs v0
        in (Serialization.noSep [
          typeToExpr fun,
          (Serialization.bracketList Serialization.inlineStyle (Lists.map typeToExpr args))])
      Syntax.TypeFunction v0 ->
        let cod = Syntax.functionTypeRes v0
            dom = Maybes.fromMaybe cod (Lists.maybeHead (Syntax.functionTypeParams v0))
        in (Serialization.ifx functionArrowOp (typeToExpr dom) (typeToExpr cod))
      Syntax.TypeLambda v0 ->
        let params = Syntax.lambdaTypeTparams v0
            body = Syntax.lambdaTypeTpe v0
        in (Serialization.noSep [
          typeToExpr body,
          (Serialization.bracketList Serialization.inlineStyle (Lists.map typeParamToExpr params))])
      Syntax.TypeVar v0 -> typeNameToExpr (Syntax.varTypeName v0)
