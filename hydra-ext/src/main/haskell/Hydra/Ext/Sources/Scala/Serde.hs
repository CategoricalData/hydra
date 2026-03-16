module Hydra.Ext.Sources.Scala.Serde where

-- Standard imports for term-level sources outside of the kernel
import Hydra.Kernel
import Hydra.Sources.Libraries
import           Hydra.Dsl.Meta.Lib.Strings                as Strings
import           Hydra.Dsl.Meta.Phantoms                   as Phantoms
import qualified Hydra.Dsl.Annotations                     as Annotations
import qualified Hydra.Dsl.Bootstrap                       as Bootstrap
import qualified Hydra.Dsl.Grammars                        as Grammars
import qualified Hydra.Dsl.LiteralTypes                    as LiteralTypes
import qualified Hydra.Dsl.Literals                        as Literals
import qualified Hydra.Dsl.Meta.Accessors                  as Accessors
import qualified Hydra.Dsl.Ast                        as Ast
import qualified Hydra.Dsl.Meta.Base                       as MetaBase
import qualified Hydra.Dsl.Meta.Coders                     as Coders
import qualified Hydra.Dsl.Meta.Compute                    as Compute
import qualified Hydra.Dsl.Meta.Core                       as Core
import qualified Hydra.Dsl.Grammar                    as Grammar
import qualified Hydra.Dsl.Meta.Graph                      as Graph
import qualified Hydra.Dsl.Meta.Json                       as Json
import qualified Hydra.Dsl.Meta.Lib.Chars                  as Chars
import qualified Hydra.Dsl.Meta.Lib.Eithers                as Eithers
import qualified Hydra.Dsl.Meta.Lib.Equality               as Equality
import qualified Hydra.Dsl.Meta.Lib.Lists                  as Lists
import qualified Hydra.Dsl.Meta.Lib.Literals               as Literals
import qualified Hydra.Dsl.Meta.Lib.Logic                  as Logic
import qualified Hydra.Dsl.Meta.Lib.Maps                   as Maps
import qualified Hydra.Dsl.Meta.Lib.Math                   as Math
import qualified Hydra.Dsl.Meta.Lib.Maybes                 as Maybes
import qualified Hydra.Dsl.Meta.Lib.Pairs                  as Pairs
import qualified Hydra.Dsl.Meta.Lib.Sets                   as Sets
import qualified Hydra.Dsl.Module                     as Module
import qualified Hydra.Dsl.Meta.Terms                      as MetaTerms
import qualified Hydra.Dsl.Meta.Testing                    as Testing
import qualified Hydra.Dsl.Topology                   as Topology
import qualified Hydra.Dsl.Meta.Types                      as MetaTypes
import qualified Hydra.Dsl.Typing                     as Typing
import qualified Hydra.Dsl.Util                       as Util
import qualified Hydra.Dsl.Meta.Variants                   as Variants
import qualified Hydra.Dsl.Prims                           as Prims
import qualified Hydra.Dsl.Meta.Tabular                         as Tabular
import qualified Hydra.Dsl.Terms                           as Terms
import qualified Hydra.Dsl.Tests                           as Tests
import qualified Hydra.Dsl.Types                           as Types
import qualified Hydra.Sources.Decode.Core                 as DecodeCore
import qualified Hydra.Sources.Encode.Core                 as EncodeCore
import qualified Hydra.Sources.Kernel.Terms.Adapt           as Adapt
import qualified Hydra.Sources.Kernel.Terms.All            as KernelTerms
import qualified Hydra.Sources.Kernel.Terms.Annotations    as Annotations
import qualified Hydra.Sources.Kernel.Terms.Arity          as Arity
import qualified Hydra.Sources.Kernel.Terms.Checking       as Checking
import qualified Hydra.Sources.Kernel.Terms.Constants      as Constants
import qualified Hydra.Sources.Kernel.Terms.Extract.Core   as ExtractCore
import qualified Hydra.Sources.Kernel.Terms.Extract.Util   as ExtractUtil
import qualified Hydra.Sources.Kernel.Terms.Formatting     as Formatting
import qualified Hydra.Sources.Kernel.Terms.Grammars       as Grammars
import qualified Hydra.Sources.Kernel.Terms.Inference      as Inference
import qualified Hydra.Sources.Kernel.Terms.Languages      as Languages
import qualified Hydra.Sources.Kernel.Terms.Lexical        as Lexical
import qualified Hydra.Sources.Kernel.Terms.Literals       as Literals
import qualified Hydra.Sources.Kernel.Terms.Names          as Names
import qualified Hydra.Sources.Kernel.Terms.Reduction      as Reduction
import qualified Hydra.Sources.Kernel.Terms.Reflect        as Reflect
import qualified Hydra.Sources.Kernel.Terms.Rewriting      as Rewriting
import qualified Hydra.Sources.Kernel.Terms.Schemas        as Schemas
import qualified Hydra.Sources.Kernel.Terms.Serialization  as Serialization
import qualified Hydra.Sources.Kernel.Terms.Show.Accessors as ShowAccessors
import qualified Hydra.Sources.Kernel.Terms.Show.Core      as ShowCore
import qualified Hydra.Sources.Kernel.Terms.Show.Graph     as ShowGraph
import qualified Hydra.Sources.Kernel.Terms.Show.Meta      as ShowMeta
import qualified Hydra.Sources.Kernel.Terms.Show.Typing    as ShowTyping
import qualified Hydra.Sources.Kernel.Terms.Sorting        as Sorting
import qualified Hydra.Sources.Kernel.Terms.Substitution   as Substitution
import qualified Hydra.Sources.Kernel.Terms.Tarjan         as Tarjan
import qualified Hydra.Sources.Kernel.Terms.Templates      as Templates
import qualified Hydra.Sources.Kernel.Terms.Unification    as Unification
import qualified Hydra.Sources.Kernel.Types.All            as KernelTypes
import           Prelude hiding ((++))
import qualified Data.Int                                  as I
import qualified Data.List                                 as L
import qualified Data.Map                                  as M
import qualified Data.Set                                  as S
import qualified Data.Maybe                                as Y

-- Additional imports
import Hydra.Ast
import qualified Hydra.Ext.Scala.Meta as Scala
import qualified Hydra.Ext.Sources.Scala.Meta as ScalaMeta


define :: String -> TTerm a -> TBinding a
define = definitionInModule module_

ns :: Namespace
ns = Namespace "hydra.ext.scala.serde"

module_ :: Module
module_ = Module ns elements
    [Serialization.ns]
    (ScalaMeta.ns:KernelTypes.kernelTypesNamespaces) $
    Just "Serialization functions for converting Scala AST to abstract expressions"
  where
    elements = [
      toBinding dotOp,
      toBinding functionArrowOp,
      toBinding matchOp,
      toBinding writeCase,
      toBinding writeDefn,
      toBinding writeImportExportStat,
      toBinding writeImporter,
      toBinding writeLit,
      toBinding writeName,
      toBinding writePat,
      toBinding writePkg,
      toBinding writeStat,
      toBinding writeTerm,
      toBinding writeData_FunctionData,
      toBinding writeData_Name,
      toBinding writeData_Param,
      toBinding writeData_Ref,
      toBinding writeData_Select,
      toBinding writeType,
      toBinding writeType_Name,
      toBinding writeType_Param,
      toBinding writeInit,
      toBinding writeMod]


dotOp :: TBinding Op
dotOp = define "dotOp" $
  doc "The dot operator for member access" $
  Ast.op (Ast.symbol (string ".")) (Ast.padding Ast.wsNone Ast.wsNone) (Ast.precedence (int32 0)) Ast.associativityLeft

functionArrowOp :: TBinding Op
functionArrowOp = define "functionArrowOp" $
  doc "The function arrow operator (=>)" $
  Serialization.op @@ string "=>" @@ Math.negate (int32 1) @@ Ast.associativityRight

matchOp :: TBinding Op
matchOp = define "matchOp" $
  doc "The match operator" $
  Ast.op (Ast.symbol (string "match")) (Ast.padding Ast.wsSpace (Ast.wsBreakAndIndent (string "  "))) (Ast.precedence (int32 0)) Ast.associativityNone

writeCase :: TBinding (Scala.Case -> Expr)
writeCase = define "writeCase" $
  doc "Convert a case clause to an expression" $
  lambda "c" $ lets [
    "pat">: project Scala._Case Scala._Case_pat @@ var "c",
    "term">: project Scala._Case Scala._Case_body @@ var "c"] $
    Serialization.spaceSep @@ list [
      Serialization.cst @@ string "case",
      writePat @@ var "pat",
      Serialization.cst @@ string "=>",
      writeTerm @@ var "term"]

writeDefn :: TBinding (Scala.Defn -> Expr)
writeDefn = define "writeDefn" $
  doc "Convert a definition to an expression" $
  lambda "def" $
    cases Scala._Defn (var "def") Nothing [
      Scala._Defn_def>>: lambda "dd" $ lets [
        "name">: project Scala._Defn_Def Scala._Defn_Def_name @@ var "dd",
        "tparams">: project Scala._Defn_Def Scala._Defn_Def_tparams @@ var "dd",
        "paramss">: project Scala._Defn_Def Scala._Defn_Def_paramss @@ var "dd",
        "scod">: project Scala._Defn_Def Scala._Defn_Def_decltpe @@ var "dd",
        "body">: project Scala._Defn_Def Scala._Defn_Def_body @@ var "dd",
        "params">: Lists.head (var "paramss"),
        "tparamsExpr">: Logic.ifElse (Lists.null (var "tparams"))
          nothing
          (Maybes.pure (Serialization.bracketList @@ Serialization.inlineStyle @@ (Lists.map writeType_Param (var "tparams")))),
        "scodExpr">: Maybes.map
          (lambda "t" $ Serialization.spaceSep @@ list [Serialization.cst @@ string ":", writeType @@ var "t"])
          (var "scod"),
        "nameAndParams">: Serialization.noSep @@ (Maybes.cat $ list [
          Maybes.pure (writeData_Name @@ var "name"),
          var "tparamsExpr",
          Maybes.pure (Serialization.parenList @@ false @@ (Lists.map writeData_Param (var "params"))),
          var "scodExpr"])] $
        Serialization.spaceSep @@ list [
          Serialization.cst @@ string "def",
          var "nameAndParams",
          Serialization.cst @@ string "=",
          writeTerm @@ var "body"],

      Scala._Defn_type>>: lambda "dt" $ lets [
        "name">: project Scala._Defn_Type Scala._Defn_Type_name @@ var "dt",
        "tparams">: project Scala._Defn_Type Scala._Defn_Type_tparams @@ var "dt",
        "body">: project Scala._Defn_Type Scala._Defn_Type_body @@ var "dt"] $
        Serialization.spaceSep @@ (Maybes.cat $ list [
          Maybes.pure (Serialization.cst @@ string "type"),
          Maybes.pure (writeType_Name @@ var "name"),
          Logic.ifElse (Lists.null (var "tparams"))
            nothing
            (Maybes.pure (Serialization.bracketList @@ Serialization.inlineStyle @@ (Lists.map writeType_Param (var "tparams")))),
          Maybes.pure (Serialization.cst @@ string "="),
          Maybes.pure (writeType @@ var "body")]),

      Scala._Defn_val>>: lambda "dv" $ lets [
        "pats">: project Scala._Defn_Val Scala._Defn_Val_pats @@ var "dv",
        "typ">: project Scala._Defn_Val Scala._Defn_Val_decltpe @@ var "dv",
        "rhs">: project Scala._Defn_Val Scala._Defn_Val_rhs @@ var "dv",
        "firstPat">: Lists.head (var "pats"),
        "patName">: cases Scala._Pat (var "firstPat") Nothing [
          Scala._Pat_var>>: lambda "pv" $ project Scala._Pat_Var Scala._Pat_Var_name @@ var "pv"],
        "nameStr">: unwrap Scala._PredefString @@ (project Scala._Data_Name Scala._Data_Name_value @@ var "patName"),
        "nameAndType">: Maybes.maybe
          (Serialization.cst @@ var "nameStr")
          (lambda "t" $ Serialization.spaceSep @@ list [
            Serialization.cst @@ (Strings.cat2 (var "nameStr") (string ":")),
            writeType @@ var "t"])
          (var "typ")] $
        Serialization.spaceSep @@ list [
          Serialization.cst @@ string "val",
          var "nameAndType",
          Serialization.cst @@ string "=",
          writeTerm @@ var "rhs"],

      Scala._Defn_class>>: lambda "dc" $ lets [
        "mods">: project Scala._Defn_Class Scala._Defn_Class_mods @@ var "dc",
        "name">: project Scala._Defn_Class Scala._Defn_Class_name @@ var "dc",
        "tparams">: project Scala._Defn_Class Scala._Defn_Class_tparams @@ var "dc",
        "ctor">: project Scala._Defn_Class Scala._Defn_Class_ctor @@ var "dc",
        "paramss">: project Scala._Ctor_Primary Scala._Ctor_Primary_paramss @@ var "ctor",
        "tparamsExpr">: Logic.ifElse (Lists.null (var "tparams"))
          nothing
          (Maybes.pure (Serialization.bracketList @@ Serialization.inlineStyle @@ (Lists.map writeType_Param (var "tparams")))),
        "paramsExpr">: Logic.ifElse (Lists.null (var "paramss"))
          nothing
          (Maybes.pure (Serialization.parenList @@ false @@ (Lists.map writeData_Param (Lists.concat (var "paramss"))))),
        "nameAndParams">: Serialization.noSep @@ (Maybes.cat $ list [
          Maybes.pure (writeType_Name @@ var "name"),
          var "tparamsExpr",
          var "paramsExpr"])] $
        Serialization.spaceSep @@ (Lists.concat $ list [
          Lists.map writeMod (var "mods"),
          list [Serialization.cst @@ string "class", var "nameAndParams"]]),

      Scala._Defn_enum>>: lambda "de" $ lets [
        "name">: project Scala._Defn_Enum Scala._Defn_Enum_name @@ var "de",
        "tparams">: project Scala._Defn_Enum Scala._Defn_Enum_tparams @@ var "de",
        "template">: project Scala._Defn_Enum Scala._Defn_Enum_template @@ var "de",
        "stats">: project Scala._Template Scala._Template_stats @@ var "template",
        "enumHeader">: Serialization.spaceSep @@ list [
          Serialization.cst @@ string "enum",
          Serialization.noSep @@ (Maybes.cat $ list [
            Maybes.pure (writeType_Name @@ var "name"),
            Logic.ifElse (Lists.null (var "tparams"))
              nothing
              (Maybes.pure (Serialization.bracketList @@ Serialization.inlineStyle @@ (Lists.map writeType_Param (var "tparams"))))]),
          Serialization.cst @@ string ":"],
        "enumCases">: Lists.map
          (lambda "s" $ Serialization.spaceSep @@ list [Serialization.cst @@ string "  ", writeStat @@ var "s"])
          (var "stats")] $
        Serialization.newlineSep @@ (Lists.concat $ list [list [var "enumHeader"], var "enumCases"]),

      Scala._Defn_enumCase>>: lambda "dec" $ lets [
        "name">: project Scala._Defn_EnumCase Scala._Defn_EnumCase_name @@ var "dec",
        "ctor">: project Scala._Defn_EnumCase Scala._Defn_EnumCase_ctor @@ var "dec",
        "inits">: project Scala._Defn_EnumCase Scala._Defn_EnumCase_inits @@ var "dec",
        "paramss">: project Scala._Ctor_Primary Scala._Ctor_Primary_paramss @@ var "ctor",
        "allParams">: Lists.concat (var "paramss"),
        "params">: Logic.ifElse (Lists.null (var "allParams"))
          (Serialization.cst @@ string "")
          (Serialization.parenList @@ false @@ (Lists.map writeData_Param (var "allParams"))),
        "extendsClause">: Logic.ifElse (Lists.null (var "inits"))
          (Serialization.cst @@ string "")
          (Serialization.spaceSep @@ list [
            Serialization.cst @@ string "extends",
            Serialization.commaSep @@ Serialization.inlineStyle @@ (Lists.map writeInit (var "inits"))])] $
        Serialization.spaceSep @@ list [
          Serialization.cst @@ string "case",
          Serialization.noSep @@ list [writeData_Name @@ var "name", var "params"],
          var "extendsClause"]]

writeImportExportStat :: TBinding (Scala.ImportExportStat -> Expr)
writeImportExportStat = define "writeImportExportStat" $
  doc "Convert an import/export statement to an expression" $
  lambda "ie" $
    cases Scala._ImportExportStat (var "ie") Nothing [
      Scala._ImportExportStat_import>>: lambda "imp" $ lets [
        "importers">: project Scala._Import Scala._Import_importers @@ var "imp"] $
        Serialization.newlineSep @@ (Lists.map writeImporter (var "importers"))]

writeImporter :: TBinding (Scala.Importer -> Expr)
writeImporter = define "writeImporter" $
  doc "Convert an importer to an expression" $
  lambda "imp" $ lets [
    "ref">: project Scala._Importer Scala._Importer_ref @@ var "imp",
    "importees">: project Scala._Importer Scala._Importer_importees @@ var "imp",
    "refName">: cases Scala._Data_Ref (var "ref") Nothing [
      Scala._Data_Ref_name>>: lambda "dn" $
        unwrap Scala._PredefString @@ (project Scala._Data_Name Scala._Data_Name_value @@ var "dn")],
    "forImportees">: Logic.ifElse (Lists.null (var "importees"))
      (Serialization.cst @@ string "")
      (Logic.ifElse (Equality.equal (Lists.length (var "importees")) (int32 1))
        (Serialization.noSep @@ list [
          Serialization.cst @@ string ".",
          cases Scala._Importee (Lists.head (var "importees")) Nothing [
            Scala._Importee_wildcard>>: constant (Serialization.cst @@ string "*"),
            Scala._Importee_name>>: lambda "in" $
              Serialization.cst @@ (cases Scala._Name (project Scala._Importee_Name Scala._Importee_Name_name @@ var "in") Nothing [
                Scala._Name_value>>: lambda "s" $ var "s"])]])
        (Serialization.noSep @@ list [
          Serialization.cst @@ string ".",
          Serialization.curlyBracesList @@ nothing @@ Serialization.inlineStyle @@
            (Lists.map
              (lambda "it" $ cases Scala._Importee (var "it") Nothing [
                Scala._Importee_wildcard>>: constant (Serialization.cst @@ string "*"),
                Scala._Importee_name>>: lambda "in" $
                  Serialization.cst @@ (cases Scala._Name (project Scala._Importee_Name Scala._Importee_Name_name @@ var "in") Nothing [
                    Scala._Name_value>>: lambda "s" $ var "s"])])
              (var "importees"))]))] $
    Serialization.spaceSep @@ list [
      Serialization.cst @@ string "import",
      Serialization.noSep @@ list [Serialization.cst @@ var "refName", var "forImportees"]]

writeLit :: TBinding (Scala.Lit -> Expr)
writeLit = define "writeLit" $
  doc "Convert a literal to an expression" $
  lambda "lit" $
    cases Scala._Lit (var "lit") (Just $ Serialization.cst @@ string "TODO:literal") [
      Scala._Lit_int>>: lambda "i" $ Serialization.cst @@ (Literals.showInt32 (var "i")),
      Scala._Lit_boolean>>: lambda "b" $ Serialization.cst @@ (Logic.ifElse (var "b") (string "true") (string "false")),
      Scala._Lit_unit>>: constant $ Serialization.cst @@ string "()",
      Scala._Lit_string>>: lambda "s" $ Serialization.cst @@ (Literals.showString (var "s"))]

writeName :: TBinding (Scala.Name -> Expr)
writeName = define "writeName" $
  doc "Convert a name to an expression" $
  lambda "name" $
    cases Scala._Name (var "name") Nothing [
      Scala._Name_value>>: lambda "s" $ Serialization.cst @@ var "s"]

writePat :: TBinding (Scala.Pat -> Expr)
writePat = define "writePat" $
  doc "Convert a pattern to an expression" $
  lambda "pat" $
    cases Scala._Pat (var "pat") Nothing [
      Scala._Pat_extract>>: lambda "pe" $ lets [
        "fun">: project Scala._Pat_Extract Scala._Pat_Extract_fun @@ var "pe",
        "args">: project Scala._Pat_Extract Scala._Pat_Extract_args @@ var "pe"] $
        Serialization.noSep @@ list [
          writeTerm @@ var "fun",
          Serialization.parenList @@ false @@ (Lists.map writePat (var "args"))],
      Scala._Pat_var>>: lambda "pv" $
        writeData_Name @@ (project Scala._Pat_Var Scala._Pat_Var_name @@ var "pv")]

writePkg :: TBinding (Scala.Pkg -> Expr)
writePkg = define "writePkg" $
  doc "Convert a package to an expression" $
  lambda "pkg" $ lets [
    "name">: project Scala._Pkg Scala._Pkg_name @@ var "pkg",
    "stats">: project Scala._Pkg Scala._Pkg_stats @@ var "pkg",
    "package">: Serialization.spaceSep @@ list [Serialization.cst @@ string "package", writeData_Name @@ var "name"]] $
    Serialization.doubleNewlineSep @@ (Lists.concat $ list [
      list [var "package"],
      Lists.map writeStat (var "stats")])

writeStat :: TBinding (Scala.Stat -> Expr)
writeStat = define "writeStat" $
  doc "Convert a statement to an expression" $
  lambda "stat" $
    cases Scala._Stat (var "stat") Nothing [
      Scala._Stat_defn>>: lambda "def" $ writeDefn @@ var "def",
      Scala._Stat_importExport>>: lambda "ie" $ writeImportExportStat @@ var "ie"]

writeTerm :: TBinding (Scala.Data -> Expr)
writeTerm = define "writeTerm" $
  doc "Convert a term to an expression" $
  lambda "term" $
    cases Scala._Data (var "term") Nothing [
      Scala._Data_lit>>: lambda "lit" $ writeLit @@ var "lit",
      Scala._Data_ref>>: lambda "ref" $ writeData_Ref @@ var "ref",
      Scala._Data_apply>>: lambda "app" $ lets [
        "fun">: project Scala._Data_Apply Scala._Data_Apply_fun @@ var "app",
        "args">: project Scala._Data_Apply Scala._Data_Apply_args @@ var "app"] $
        Serialization.noSep @@ list [
          writeTerm @@ var "fun",
          Serialization.parenList @@ false @@ (Lists.map writeTerm (var "args"))],
      Scala._Data_assign>>: constant $ Serialization.cst @@ string ">ASSIGN",
      Scala._Data_tuple>>: lambda "tup" $
        Serialization.parenList @@ false @@ (Lists.map writeTerm (project Scala._Data_Tuple Scala._Data_Tuple_args @@ var "tup")),
      Scala._Data_match>>: lambda "m" $ lets [
        "expr">: project Scala._Data_Match Scala._Data_Match_expr @@ var "m",
        "mCases">: project Scala._Data_Match Scala._Data_Match_cases @@ var "m"] $
        Serialization.ifx @@ matchOp @@ (writeTerm @@ var "expr") @@
          (Serialization.newlineSep @@ (Lists.map writeCase (var "mCases"))),
      Scala._Data_functionData>>: lambda "ft" $ writeData_FunctionData @@ var "ft"]

writeData_FunctionData :: TBinding (Scala.Data_FunctionData -> Expr)
writeData_FunctionData = define "writeData_FunctionData" $
  doc "Convert function data to an expression" $
  lambda "ft" $
    cases Scala._Data_FunctionData (var "ft") Nothing [
      Scala._Data_FunctionData_function>>: lambda "f" $ lets [
        "params">: project Scala._Data_Function Scala._Data_Function_params @@ var "f",
        "body">: project Scala._Data_Function Scala._Data_Function_body @@ var "f"] $
        Serialization.spaceSep @@ list [
          Serialization.parenList @@ false @@ (Lists.map writeData_Param (var "params")),
          Serialization.cst @@ string "=>",
          writeTerm @@ var "body"]]

writeData_Name :: TBinding (Scala.Data_Name -> Expr)
writeData_Name = define "writeData_Name" $
  doc "Convert a data name to an expression" $
  lambda "dn" $
    Serialization.cst @@ (unwrap Scala._PredefString @@ (project Scala._Data_Name Scala._Data_Name_value @@ var "dn"))

writeData_Param :: TBinding (Scala.Data_Param -> Expr)
writeData_Param = define "writeData_Param" $
  doc "Convert a data parameter to an expression" $
  lambda "dp" $ lets [
    "name">: project Scala._Data_Param Scala._Data_Param_name @@ var "dp",
    "stype">: project Scala._Data_Param Scala._Data_Param_decltpe @@ var "dp"] $
    Serialization.noSep @@ (Maybes.cat $ list [
      Maybes.pure (writeName @@ var "name"),
      Maybes.map
        (lambda "t" $ Serialization.spaceSep @@ list [Serialization.cst @@ string ":", writeType @@ var "t"])
        (var "stype")])

writeData_Ref :: TBinding (Scala.Data_Ref -> Expr)
writeData_Ref = define "writeData_Ref" $
  doc "Convert a data reference to an expression" $
  lambda "ref" $
    cases Scala._Data_Ref (var "ref") Nothing [
      Scala._Data_Ref_name>>: lambda "name" $ writeData_Name @@ var "name",
      Scala._Data_Ref_select>>: lambda "sel" $ writeData_Select @@ var "sel"]

writeData_Select :: TBinding (Scala.Data_Select -> Expr)
writeData_Select = define "writeData_Select" $
  doc "Convert a data select to an expression" $
  lambda "sel" $ lets [
    "arg">: project Scala._Data_Select Scala._Data_Select_qual @@ var "sel",
    "name">: project Scala._Data_Select Scala._Data_Select_name @@ var "sel"] $
    Serialization.ifx @@ dotOp @@ (writeTerm @@ var "arg") @@
      (writeTerm @@ (inject Scala._Data Scala._Data_ref (inject Scala._Data_Ref Scala._Data_Ref_name (var "name"))))

writeType :: TBinding (Scala.Type -> Expr)
writeType = define "writeType" $
  doc "Convert a type to an expression" $
  lambda "typ" $
    cases Scala._Type (var "typ") Nothing [
      Scala._Type_ref>>: lambda "tr" $
        cases Scala._Type_Ref (var "tr") Nothing [
          Scala._Type_Ref_name>>: lambda "name" $ writeType_Name @@ var "name"],
      Scala._Type_apply>>: lambda "ta" $ lets [
        "fun">: project Scala._Type_Apply Scala._Type_Apply_tpe @@ var "ta",
        "args">: project Scala._Type_Apply Scala._Type_Apply_args @@ var "ta"] $
        Serialization.noSep @@ list [
          writeType @@ var "fun",
          Serialization.bracketList @@ Serialization.inlineStyle @@ (Lists.map writeType (var "args"))],
      Scala._Type_functionType>>: lambda "ft" $
        cases Scala._Type_FunctionType (var "ft") Nothing [
          Scala._Type_FunctionType_function>>: lambda "tf" $ lets [
            "dom">: Lists.head (project Scala._Type_Function Scala._Type_Function_params @@ var "tf"),
            "cod">: project Scala._Type_Function Scala._Type_Function_res @@ var "tf"] $
            Serialization.ifx @@ functionArrowOp @@ (writeType @@ var "dom") @@ (writeType @@ var "cod")],
      Scala._Type_lambda>>: lambda "tl" $ lets [
        "params">: project Scala._Type_Lambda Scala._Type_Lambda_tparams @@ var "tl",
        "body">: project Scala._Type_Lambda Scala._Type_Lambda_tpe @@ var "tl"] $
        Serialization.noSep @@ list [
          writeType @@ var "body",
          Serialization.bracketList @@ Serialization.inlineStyle @@ (Lists.map writeType_Param (var "params"))],
      Scala._Type_var>>: lambda "tv" $
        writeType_Name @@ (project Scala._Type_Var Scala._Type_Var_name @@ var "tv")]

writeType_Name :: TBinding (Scala.Type_Name -> Expr)
writeType_Name = define "writeType_Name" $
  doc "Convert a type name to an expression" $
  lambda "tn" $
    Serialization.cst @@ (project Scala._Type_Name Scala._Type_Name_value @@ var "tn")

writeType_Param :: TBinding (Scala.Type_Param -> Expr)
writeType_Param = define "writeType_Param" $
  doc "Convert a type parameter to an expression" $
  lambda "tp" $
    writeName @@ (project Scala._Type_Param Scala._Type_Param_name @@ var "tp")

writeInit :: TBinding (Scala.Init -> Expr)
writeInit = define "writeInit" $
  doc "Convert an init to an expression" $
  lambda "init" $
    writeType @@ (project Scala._Init Scala._Init_tpe @@ var "init")

writeMod :: TBinding (Scala.Mod -> Expr)
writeMod = define "writeMod" $
  doc "Convert a modifier to an expression" $
  lambda "m" $
    cases Scala._Mod (var "m") Nothing [
      Scala._Mod_case>>: constant $ Serialization.cst @@ string "case",
      Scala._Mod_sealed>>: constant $ Serialization.cst @@ string "sealed",
      Scala._Mod_abstract>>: constant $ Serialization.cst @@ string "abstract",
      Scala._Mod_final>>: constant $ Serialization.cst @@ string "final",
      Scala._Mod_override>>: constant $ Serialization.cst @@ string "override",
      Scala._Mod_implicit>>: constant $ Serialization.cst @@ string "implicit",
      Scala._Mod_lazy>>: constant $ Serialization.cst @@ string "lazy",
      Scala._Mod_private>>: lambda "_" $ Serialization.cst @@ string "private",
      Scala._Mod_protected>>: lambda "_" $ Serialization.cst @@ string "protected"]
