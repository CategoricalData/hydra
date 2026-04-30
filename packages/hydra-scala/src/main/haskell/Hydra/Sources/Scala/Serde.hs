module Hydra.Sources.Scala.Serde where

-- Standard imports for term-level sources outside of the kernel
import Hydra.Kernel
import Hydra.Sources.Libraries
import           Hydra.Dsl.Meta.Lib.Strings                as Strings
import           Hydra.Dsl.Meta.Phantoms                   as Phantoms
import qualified Hydra.Dsl.Annotations                     as Annotations
import qualified Hydra.Dsl.Bootstrap                       as Bootstrap
import qualified Hydra.Dsl.LiteralTypes                    as LiteralTypes
import qualified Hydra.Dsl.Literals                        as Literals
import qualified Hydra.Dsl.Paths                      as Paths
import qualified Hydra.Dsl.Ast                        as Ast
import qualified Hydra.Dsl.Meta.Base                       as MetaBase
import qualified Hydra.Dsl.Coders                     as Coders
import qualified Hydra.Dsl.Util                    as Util
import qualified Hydra.Dsl.Meta.Core                       as Core
import qualified Hydra.Dsl.Meta.Graph                      as Graph
import qualified Hydra.Dsl.Json.Model                       as Json
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
import qualified Hydra.Dsl.Packaging                     as Packaging
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
import qualified Hydra.Sources.Kernel.Terms.Inference      as Inference
import qualified Hydra.Sources.Kernel.Terms.Languages      as Languages
import qualified Hydra.Sources.Kernel.Terms.Lexical        as Lexical
import qualified Hydra.Sources.Kernel.Terms.Literals       as Literals
import qualified Hydra.Sources.Kernel.Terms.Names          as Names
import qualified Hydra.Sources.Kernel.Terms.Reduction      as Reduction
import qualified Hydra.Sources.Kernel.Terms.Reflect        as Reflect
import qualified Hydra.Sources.Kernel.Terms.Serialization  as Serialization
import qualified Hydra.Sources.Kernel.Terms.Show.Paths as ShowPaths
import qualified Hydra.Sources.Kernel.Terms.Show.Core      as ShowCore
import qualified Hydra.Sources.Kernel.Terms.Show.Graph     as ShowGraph
import qualified Hydra.Sources.Kernel.Terms.Show.Variants  as ShowVariants
import qualified Hydra.Sources.Kernel.Terms.Show.Typing    as ShowTyping
import qualified Hydra.Sources.Kernel.Terms.Sorting        as Sorting
import qualified Hydra.Sources.Kernel.Terms.Substitution   as Substitution
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
import qualified Hydra.Scala.Syntax as Scala
import qualified Hydra.Sources.Java.Serde as JavaSerdeSource
import qualified Hydra.Sources.Scala.Syntax as ScalaSyntax


define :: String -> TTerm a -> TTermDefinition a
define = definitionInModule module_

ns :: Namespace
ns = Namespace "hydra.scala.serde"

module_ :: Module
module_ = Module {
            moduleNamespace = ns,
            moduleDefinitions = definitions,
            moduleTermDependencies = [Serialization.ns, JavaSerdeSource.ns],
            moduleTypeDependencies = (ScalaSyntax.ns:KernelTypes.kernelTypesNamespaces),
            moduleDescription = Just "Serialization functions for converting Scala AST to abstract expressions"}
  where
    definitions = [
      toDefinition dotOp,
      toDefinition functionArrowOp,
      toDefinition matchOp,
      toDefinition scalaFloatLiteralText,
      toDefinition caseToExpr,
      toDefinition dataFunctionDataToExpr,
      toDefinition dataNameToExpr,
      toDefinition dataParamToExpr,
      toDefinition dataRefToExpr,
      toDefinition dataSelectToExpr,
      toDefinition defnToExpr,
      toDefinition importExportStatToExpr,
      toDefinition importerToExpr,
      toDefinition initToExpr,
      toDefinition litToExpr,
      toDefinition modToExpr,
      toDefinition nameToExpr,
      toDefinition patToExpr,
      toDefinition pkgToExpr,
      toDefinition statToExpr,
      toDefinition termToExpr,
      toDefinition typeToExpr,
      toDefinition typeNameToExpr,
      toDefinition typeParamToExpr]


dotOp :: TTermDefinition Op
dotOp = define "dotOp" $
  doc "The dot operator for member access" $
  Ast.op (Ast.symbol (string ".")) (Ast.padding Ast.wsNone Ast.wsNone) (Ast.precedence (int32 0)) Ast.associativityLeft

functionArrowOp :: TTermDefinition Op
functionArrowOp = define "functionArrowOp" $
  doc "The function arrow operator (=>)" $
  Serialization.op @@ string "=>" @@ Math.negate (int32 1) @@ Ast.associativityRight

matchOp :: TTermDefinition Op
matchOp = define "matchOp" $
  doc "The match operator" $
  Ast.op (Ast.symbol (string "match")) (Ast.padding Ast.wsSpace (Ast.wsBreakAndIndent (string "  "))) (Ast.precedence (int32 0)) Ast.associativityNone

caseToExpr :: TTermDefinition (Scala.Case -> Expr)
caseToExpr = define "caseToExpr" $
  doc "Convert a case clause to an expression" $
  lambda "c" $ lets [
    "pat">: project Scala._Case Scala._Case_pat @@ var "c",
    "term">: project Scala._Case Scala._Case_body @@ var "c"] $
    Serialization.spaceSep @@ list [
      Serialization.cst @@ string "case",
      patToExpr @@ var "pat",
      Serialization.cst @@ string "=>",
      termToExpr @@ var "term"]

dataFunctionDataToExpr :: TTermDefinition (Scala.Data_FunctionData -> Expr)
dataFunctionDataToExpr = define "dataFunctionDataToExpr" $
  doc "Convert function data to an expression" $
  lambda "ft" $
    cases Scala._Data_FunctionData (var "ft") Nothing [
      Scala._Data_FunctionData_function>>: lambda "f" $ lets [
        "params">: project Scala._Data_Function Scala._Data_Function_params @@ var "f",
        "body">: project Scala._Data_Function Scala._Data_Function_body @@ var "f",
        "bodyExpr">: termToExpr @@ var "body",
        "bodyLen">: Serialization.expressionLength @@ var "bodyExpr"] $
        -- For long lambda bodies (>60 chars), put body on indented new line
        Logic.ifElse (Equality.gt (var "bodyLen") (int32 60))
          (Serialization.noSep @@ list [
            Serialization.parenListAdaptive @@ (Lists.map dataParamToExpr (var "params")),
            Serialization.cst @@ string " =>\n  ",
            var "bodyExpr"])
          (Serialization.spaceSep @@ list [
            Serialization.parenListAdaptive @@ (Lists.map dataParamToExpr (var "params")),
            Serialization.cst @@ string "=>",
            var "bodyExpr"])]

dataNameToExpr :: TTermDefinition (Scala.Data_Name -> Expr)
dataNameToExpr = define "dataNameToExpr" $
  doc "Convert a data name to an expression" $
  lambda "dn" $
    Serialization.cst @@ (unwrap Scala._PredefString @@ (project Scala._Data_Name Scala._Data_Name_value @@ var "dn"))

dataParamToExpr :: TTermDefinition (Scala.Data_Param -> Expr)
dataParamToExpr = define "dataParamToExpr" $
  doc "Convert a data parameter to an expression" $
  lambda "dp" $ lets [
    "name">: project Scala._Data_Param Scala._Data_Param_name @@ var "dp",
    "stype">: project Scala._Data_Param Scala._Data_Param_decltpe @@ var "dp"] $
    Serialization.noSep @@ (Maybes.cat $ list [
      Maybes.pure (nameToExpr @@ var "name"),
      Maybes.map
        (lambda "t" $ Serialization.spaceSep @@ list [Serialization.cst @@ string ":", typeToExpr @@ var "t"])
        (var "stype")])

dataRefToExpr :: TTermDefinition (Scala.Data_Ref -> Expr)
dataRefToExpr = define "dataRefToExpr" $
  doc "Convert a data reference to an expression" $
  lambda "ref" $
    cases Scala._Data_Ref (var "ref") Nothing [
      Scala._Data_Ref_name>>: lambda "name" $ dataNameToExpr @@ var "name",
      Scala._Data_Ref_select>>: lambda "sel" $ dataSelectToExpr @@ var "sel"]

dataSelectToExpr :: TTermDefinition (Scala.Data_Select -> Expr)
dataSelectToExpr = define "dataSelectToExpr" $
  doc "Convert a data select to an expression" $
  lambda "sel" $ lets [
    "arg">: project Scala._Data_Select Scala._Data_Select_qual @@ var "sel",
    "name">: project Scala._Data_Select Scala._Data_Select_name @@ var "sel"] $
    Serialization.ifx @@ dotOp @@ (termToExpr @@ var "arg") @@
      (termToExpr @@ (inject Scala._Data Scala._Data_ref (inject Scala._Data_Ref Scala._Data_Ref_name (var "name"))))

defnToExpr :: TTermDefinition (Scala.Defn -> Expr)
defnToExpr = define "defnToExpr" $
  doc "Convert a definition to an expression" $
  lambda "def" $
    cases Scala._Defn (var "def") Nothing [
      Scala._Defn_def>>: lambda "dd" $ lets [
        "name">: project Scala._Defn_Def Scala._Defn_Def_name @@ var "dd",
        "tparams">: project Scala._Defn_Def Scala._Defn_Def_tparams @@ var "dd",
        "paramss">: project Scala._Defn_Def Scala._Defn_Def_paramss @@ var "dd",
        "scod">: project Scala._Defn_Def Scala._Defn_Def_decltpe @@ var "dd",
        "body">: project Scala._Defn_Def Scala._Defn_Def_body @@ var "dd",
        "tparamsExpr">: Logic.ifElse (Lists.null (var "tparams"))
          nothing
          (Maybes.pure (Serialization.bracketList @@ Serialization.inlineStyle @@ (Lists.map typeParamToExpr (var "tparams")))),
        "scodExpr">: Maybes.map
          (lambda "t" $ Serialization.spaceSep @@ list [Serialization.cst @@ string ":", typeToExpr @@ var "t"])
          (var "scod"),
        -- Render each parameter list in its own parens (for curried defs)
        "paramssExprs">: Lists.map
          ("ps" ~> Serialization.parenListAdaptive @@ (Lists.map dataParamToExpr (var "ps")))
          (var "paramss"),
        "nameAndParams">: Serialization.noSep @@ (Maybes.cat $ Lists.concat (list [
          list [Maybes.pure (dataNameToExpr @@ var "name")],
          list [var "tparamsExpr"],
          Lists.map ("pe" ~> Maybes.pure (var "pe")) (var "paramssExprs"),
          list [var "scodExpr"]]))] $
        "bodyExpr" <~ (termToExpr @@ var "body") $
        "defSig" <~ (Serialization.spaceSep @@ list [
          Serialization.cst @@ string "def",
          var "nameAndParams",
          Serialization.cst @@ string "="]) $
        "bodyLen" <~ (Serialization.expressionLength @@ var "bodyExpr") $
        -- For long def bodies, put body on indented new line
        Logic.ifElse (Equality.gt (var "bodyLen") (int32 80))
          (Serialization.noSep @@ list [var "defSig", Serialization.cst @@ string "\n  ", var "bodyExpr"])
          (Serialization.spaceSep @@ list [var "defSig", var "bodyExpr"]),

      Scala._Defn_type>>: lambda "dt" $ lets [
        "name">: project Scala._Defn_Type Scala._Defn_Type_name @@ var "dt",
        "tparams">: project Scala._Defn_Type Scala._Defn_Type_tparams @@ var "dt",
        "body">: project Scala._Defn_Type Scala._Defn_Type_body @@ var "dt"] $
        Serialization.spaceSep @@ (Maybes.cat $ list [
          Maybes.pure (Serialization.cst @@ string "type"),
          Maybes.pure (typeNameToExpr @@ var "name"),
          Logic.ifElse (Lists.null (var "tparams"))
            nothing
            (Maybes.pure (Serialization.bracketList @@ Serialization.inlineStyle @@ (Lists.map typeParamToExpr (var "tparams")))),
          Maybes.pure (Serialization.cst @@ string "="),
          Maybes.pure (typeToExpr @@ var "body")]),

      Scala._Defn_val>>: lambda "dv" $ lets [
        "mods">: project Scala._Defn_Val Scala._Defn_Val_mods @@ var "dv",
        "pats">: project Scala._Defn_Val Scala._Defn_Val_pats @@ var "dv",
        "typ">: project Scala._Defn_Val Scala._Defn_Val_decltpe @@ var "dv",
        "rhs">: project Scala._Defn_Val Scala._Defn_Val_rhs @@ var "dv",
        "nameStr">: Maybes.fromMaybe (string "") (Maybes.map
          (lambda "firstPat" $
            "patName" <~ (cases Scala._Pat (var "firstPat") Nothing [
              Scala._Pat_var>>: lambda "pv" $ project Scala._Pat_Var Scala._Pat_Var_name @@ var "pv"]) $
            unwrap Scala._PredefString @@ (project Scala._Data_Name Scala._Data_Name_value @@ var "patName"))
          (Lists.maybeHead (var "pats"))),
        "nameAndType">: Maybes.maybe
          (Serialization.cst @@ var "nameStr")
          (lambda "t" $ Serialization.spaceSep @@ list [
            Serialization.cst @@ (Strings.cat2 (var "nameStr") (string ":")),
            typeToExpr @@ var "t"])
          (var "typ"),
        "valKeyword">: Logic.ifElse (Lists.null (var "mods")) (string "val") (string "lazy val")] $
        Serialization.spaceSep @@ list [
          Serialization.cst @@ var "valKeyword",
          var "nameAndType",
          Serialization.cst @@ string "=",
          termToExpr @@ var "rhs"],

      Scala._Defn_class>>: lambda "dc" $ lets [
        "mods">: project Scala._Defn_Class Scala._Defn_Class_mods @@ var "dc",
        "name">: project Scala._Defn_Class Scala._Defn_Class_name @@ var "dc",
        "tparams">: project Scala._Defn_Class Scala._Defn_Class_tparams @@ var "dc",
        "ctor">: project Scala._Defn_Class Scala._Defn_Class_ctor @@ var "dc",
        "paramss">: project Scala._Ctor_Primary Scala._Ctor_Primary_paramss @@ var "ctor",
        "tparamsExpr">: Logic.ifElse (Lists.null (var "tparams"))
          nothing
          (Maybes.pure (Serialization.bracketList @@ Serialization.inlineStyle @@ (Lists.map typeParamToExpr (var "tparams")))),
        "paramsExpr">: Logic.ifElse (Lists.null (var "paramss"))
          nothing
          (Maybes.pure (Serialization.parenListAdaptive @@ (Lists.map dataParamToExpr (Lists.concat (var "paramss"))))),
        "nameAndParams">: Serialization.noSep @@ (Maybes.cat $ list [
          Maybes.pure (typeNameToExpr @@ var "name"),
          var "tparamsExpr",
          var "paramsExpr"])] $
        Serialization.spaceSep @@ (Lists.concat $ list [
          Lists.map modToExpr (var "mods"),
          list [Serialization.cst @@ string "class", var "nameAndParams"]]),

      Scala._Defn_enum>>: lambda "de" $ lets [
        "name">: project Scala._Defn_Enum Scala._Defn_Enum_name @@ var "de",
        "tparams">: project Scala._Defn_Enum Scala._Defn_Enum_tparams @@ var "de",
        "template">: project Scala._Defn_Enum Scala._Defn_Enum_template @@ var "de",
        "stats">: project Scala._Template Scala._Template_stats @@ var "template",
        "enumHeader">: Serialization.spaceSep @@ list [
          Serialization.cst @@ string "enum",
          Serialization.noSep @@ (Maybes.cat $ list [
            Maybes.pure (typeNameToExpr @@ var "name"),
            Logic.ifElse (Lists.null (var "tparams"))
              nothing
              (Maybes.pure (Serialization.bracketList @@ Serialization.inlineStyle @@ (Lists.map typeParamToExpr (var "tparams"))))]),
          Serialization.cst @@ string ":"],
        "enumCases">: Lists.map
          (lambda "s" $ Serialization.spaceSep @@ list [Serialization.cst @@ string "  ", statToExpr @@ var "s"])
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
          (Serialization.parenListAdaptive @@ (Lists.map dataParamToExpr (var "allParams"))),
        "extendsClause">: Logic.ifElse (Lists.null (var "inits"))
          (Serialization.cst @@ string "")
          (Serialization.spaceSep @@ list [
            Serialization.cst @@ string "extends",
            Serialization.commaSep @@ Serialization.inlineStyle @@ (Lists.map initToExpr (var "inits"))])] $
        Serialization.spaceSep @@ list [
          Serialization.cst @@ string "case",
          Serialization.noSep @@ list [dataNameToExpr @@ var "name", var "params"],
          var "extendsClause"]]

importExportStatToExpr :: TTermDefinition (Scala.ImportExportStat -> Expr)
importExportStatToExpr = define "importExportStatToExpr" $
  doc "Convert an import/export statement to an expression" $
  lambda "ie" $
    cases Scala._ImportExportStat (var "ie") Nothing [
      Scala._ImportExportStat_import>>: lambda "imp" $ lets [
        "importers">: project Scala._Import Scala._Import_importers @@ var "imp"] $
        Serialization.newlineSep @@ (Lists.map importerToExpr (var "importers"))]

importerToExpr :: TTermDefinition (Scala.Importer -> Expr)
importerToExpr = define "importerToExpr" $
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
        (Maybes.fromMaybe (Serialization.cst @@ string "") (Maybes.map
          (lambda "firstImp" $ Serialization.noSep @@ list [
            Serialization.cst @@ string ".",
            cases Scala._Importee (var "firstImp") Nothing [
              Scala._Importee_wildcard>>: constant (Serialization.cst @@ string "*"),
              Scala._Importee_name>>: lambda "in" $
                Serialization.cst @@ (cases Scala._Name (project Scala._Importee_Name Scala._Importee_Name_name @@ var "in") Nothing [
                  Scala._Name_value>>: lambda "s" $ var "s"])]])
          (Lists.maybeHead (var "importees"))))
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

initToExpr :: TTermDefinition (Scala.Init -> Expr)
initToExpr = define "initToExpr" $
  doc "Convert an init to an expression" $
  lambda "init" $
    typeToExpr @@ (project Scala._Init Scala._Init_tpe @@ var "init")

-- | Convert a showFloat32/showFloat64 result into valid Scala source syntax,
-- mapping NaN and ±Infinity to {Float,Double}.{NaN,PositiveInfinity,NegativeInfinity}.
-- The 'prefix' is "Float" or "Double"; 'suffix' is "f" (Float) or "" (Double).
scalaFloatLiteralText :: TTermDefinition (String -> String -> String -> String)
scalaFloatLiteralText = define "scalaFloatLiteralText" $
  lambda "prefix" $ lambda "suffix" $ lambda "s" $
    Logic.ifElse (Equality.equal (var "s") (string "NaN"))
      (Strings.cat2 (var "prefix") (string ".NaN")) $
    Logic.ifElse (Equality.equal (var "s") (string "Infinity"))
      (Strings.cat2 (var "prefix") (string ".PositiveInfinity")) $
    Logic.ifElse (Equality.equal (var "s") (string "-Infinity"))
      (Strings.cat2 (var "prefix") (string ".NegativeInfinity"))
      (Strings.cat2 (var "s") (var "suffix"))

litToExpr :: TTermDefinition (Scala.Lit -> Expr)
litToExpr = define "litToExpr" $
  doc "Convert a literal to an expression" $
  lambda "lit" $
    cases Scala._Lit (var "lit") (Just $ Serialization.cst @@ string "TODO:literal") [
      Scala._Lit_boolean>>: lambda "b" $ Serialization.cst @@ (Logic.ifElse (var "b") (string "true") (string "false")),
      Scala._Lit_byte>>: lambda "i" $ Serialization.cst @@ (Strings.cat2 (Literals.showInt8 (var "i")) (string ".toByte")),
      Scala._Lit_short>>: lambda "i" $ Serialization.cst @@ (Strings.cat2 (Literals.showInt16 (var "i")) (string ".toShort")),
      Scala._Lit_int>>: lambda "i" $ Serialization.cst @@ (Literals.showInt32 (var "i")),
      Scala._Lit_long>>: lambda "i" $ Serialization.cst @@ (Strings.cat2 (Literals.showInt64 (var "i")) (string "L")),
      Scala._Lit_float>>: lambda "f" $
        Serialization.cst @@ (scalaFloatLiteralText @@ string "Float" @@ string "f" @@ Literals.showFloat32 (var "f")),
      Scala._Lit_double>>: lambda "f" $
        Serialization.cst @@ (scalaFloatLiteralText @@ string "Double" @@ string "" @@ Literals.showFloat64 (var "f")),
      Scala._Lit_unit>>: constant $ Serialization.cst @@ string "()",
      Scala._Lit_string>>: lambda "s" $ Serialization.cst @@ Strings.cat2 (string "\"") (Strings.cat2 (JavaSerdeSource.escapeJavaString @@ var "s") (string "\"")),
      Name "bytes">>: lambda "bs" $
        Serialization.cst @@ Strings.cat2 (string "Array[Byte](")
          (Strings.cat2
            (Strings.intercalate (string ", ") (Lists.map (lambda "b" $ Strings.cat2 (Literals.showInt32 (var "b")) (string ".toByte")) (var "bs")))
            (string ")"))]

modToExpr :: TTermDefinition (Scala.Mod -> Expr)
modToExpr = define "modToExpr" $
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

nameToExpr :: TTermDefinition (Scala.Name -> Expr)
nameToExpr = define "nameToExpr" $
  doc "Convert a name to an expression" $
  lambda "name" $
    cases Scala._Name (var "name") Nothing [
      Scala._Name_value>>: lambda "s" $ Serialization.cst @@ var "s"]

patToExpr :: TTermDefinition (Scala.Pat -> Expr)
patToExpr = define "patToExpr" $
  doc "Convert a pattern to an expression" $
  lambda "pat" $
    cases Scala._Pat (var "pat") Nothing [
      Scala._Pat_extract>>: lambda "pe" $ lets [
        "fun">: project Scala._Pat_Extract Scala._Pat_Extract_fun @@ var "pe",
        "args">: project Scala._Pat_Extract Scala._Pat_Extract_args @@ var "pe"] $
        -- Omit parens for truly parameterless enum cases; include for parameterized
        Logic.ifElse (Lists.null (var "args"))
          (termToExpr @@ var "fun")
          (Serialization.noSep @@ list [
            termToExpr @@ var "fun",
            Serialization.parenListAdaptive @@ (Lists.map patToExpr (var "args"))]),
      Scala._Pat_var>>: lambda "pv" $
        dataNameToExpr @@ (project Scala._Pat_Var Scala._Pat_Var_name @@ var "pv"),
      Scala._Pat_wildcard>>: constant (Serialization.cst @@ string "_")]

pkgToExpr :: TTermDefinition (Scala.Pkg -> Expr)
pkgToExpr = define "pkgToExpr" $
  doc "Convert a package to an expression" $
  lambda "pkg" $ lets [
    "name">: project Scala._Pkg Scala._Pkg_name @@ var "pkg",
    "stats">: project Scala._Pkg Scala._Pkg_stats @@ var "pkg",
    "package">: Serialization.spaceSep @@ list [Serialization.cst @@ string "package", dataNameToExpr @@ var "name"]] $
    Serialization.doubleNewlineSep @@ (Lists.concat $ list [
      list [var "package"],
      Lists.map statToExpr (var "stats")])

statToExpr :: TTermDefinition (Scala.Stat -> Expr)
statToExpr = define "statToExpr" $
  doc "Convert a statement to an expression" $
  lambda "stat" $
    cases Scala._Stat (var "stat") Nothing [
      Scala._Stat_term>>: lambda "t" $ termToExpr @@ var "t",
      Scala._Stat_defn>>: lambda "def" $ defnToExpr @@ var "def",
      Scala._Stat_importExport>>: lambda "ie" $ importExportStatToExpr @@ var "ie"]

termToExpr :: TTermDefinition (Scala.Data -> Expr)
termToExpr = define "termToExpr" $
  doc "Convert a term to an expression" $
  lambda "term" $
    cases Scala._Data (var "term") Nothing [
      Scala._Data_lit>>: lambda "lit" $ litToExpr @@ var "lit",
      Scala._Data_ref>>: lambda "ref" $ dataRefToExpr @@ var "ref",
      Scala._Data_apply>>: lambda "app" $ lets [
        "fun">: project Scala._Data_Apply Scala._Data_Apply_fun @@ var "app",
        "args">: project Scala._Data_Apply Scala._Data_Apply_args @@ var "app"] $
        Serialization.noSep @@ list [
          termToExpr @@ var "fun",
          Serialization.parenListAdaptive @@ (Lists.map termToExpr (var "args"))],
      Scala._Data_assign>>: lambda "a" $ lets [
        "lhs">: project Scala._Data_Assign Scala._Data_Assign_lhs @@ var "a",
        "rhs">: project Scala._Data_Assign Scala._Data_Assign_rhs @@ var "a"] $
        Serialization.spaceSep @@ list [termToExpr @@ var "lhs", Serialization.cst @@ string "->", termToExpr @@ var "rhs"],
      Scala._Data_tuple>>: lambda "tup" $
        Serialization.parenListAdaptive @@ (Lists.map termToExpr (project Scala._Data_Tuple Scala._Data_Tuple_args @@ var "tup")),
      Scala._Data_match>>: lambda "m" $ lets [
        "expr">: project Scala._Data_Match Scala._Data_Match_expr @@ var "m",
        "mCases">: project Scala._Data_Match Scala._Data_Match_cases @@ var "m"] $
        Serialization.ifx @@ matchOp @@ (termToExpr @@ var "expr") @@
          (Serialization.newlineSep @@ (Lists.map caseToExpr (var "mCases"))),
      Scala._Data_functionData>>: lambda "ft" $ dataFunctionDataToExpr @@ var "ft",
      Scala._Data_block>>: lambda "blk" $ lets [
        "stats">: project Scala._Data_Block Scala._Data_Block_stats @@ var "blk"] $
        Serialization.curlyBlock @@ Serialization.fullBlockStyle @@ (Serialization.newlineSep @@ (Lists.map statToExpr (var "stats")))]

typeToExpr :: TTermDefinition (Scala.Type -> Expr)
typeToExpr = define "typeToExpr" $
  doc "Convert a type to an expression" $
  lambda "typ" $
    cases Scala._Type (var "typ") Nothing [
      Scala._Type_ref>>: lambda "tr" $
        cases Scala._Type_Ref (var "tr") Nothing [
          Scala._Type_Ref_name>>: lambda "name" $ typeNameToExpr @@ var "name"],
      Scala._Type_apply>>: lambda "ta" $ lets [
        "fun">: project Scala._Type_Apply Scala._Type_Apply_tpe @@ var "ta",
        "args">: project Scala._Type_Apply Scala._Type_Apply_args @@ var "ta"] $
        Serialization.noSep @@ list [
          typeToExpr @@ var "fun",
          Serialization.bracketList @@ Serialization.inlineStyle @@ (Lists.map typeToExpr (var "args"))],
      Scala._Type_functionType>>: lambda "ft" $
        cases Scala._Type_FunctionType (var "ft") Nothing [
          Scala._Type_FunctionType_function>>: lambda "tf" $ lets [
            "cod">: project Scala._Type_Function Scala._Type_Function_res @@ var "tf",
            "dom">: Maybes.fromMaybe (var "cod") (Lists.maybeHead (project Scala._Type_Function Scala._Type_Function_params @@ var "tf"))] $
            Serialization.ifx @@ functionArrowOp @@ (typeToExpr @@ var "dom") @@ (typeToExpr @@ var "cod")],
      Scala._Type_lambda>>: lambda "tl" $ lets [
        "params">: project Scala._Type_Lambda Scala._Type_Lambda_tparams @@ var "tl",
        "body">: project Scala._Type_Lambda Scala._Type_Lambda_tpe @@ var "tl"] $
        Serialization.noSep @@ list [
          typeToExpr @@ var "body",
          Serialization.bracketList @@ Serialization.inlineStyle @@ (Lists.map typeParamToExpr (var "params"))],
      Scala._Type_var>>: lambda "tv" $
        typeNameToExpr @@ (project Scala._Type_Var Scala._Type_Var_name @@ var "tv")]

typeNameToExpr :: TTermDefinition (Scala.Type_Name -> Expr)
typeNameToExpr = define "typeNameToExpr" $
  doc "Convert a type name to an expression" $
  lambda "tn" $
    Serialization.cst @@ (project Scala._Type_Name Scala._Type_Name_value @@ var "tn")

typeParamToExpr :: TTermDefinition (Scala.Type_Param -> Expr)
typeParamToExpr = define "typeParamToExpr" $
  doc "Convert a type parameter to an expression" $
  lambda "tp" $
    nameToExpr @@ (project Scala._Type_Param Scala._Type_Param_name @@ var "tp")
