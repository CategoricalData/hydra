module Hydra.Ext.Sources.Scala.Meta where

-- Standard imports for type-level sources outside of the kernel
import Hydra.Kernel
import Hydra.Dsl.Annotations
import Hydra.Dsl.Bootstrap
import           Hydra.Dsl.Types ((>:))
import qualified Hydra.Dsl.Types as T
import qualified Hydra.Sources.Kernel.Types.Core as Core


ns :: Namespace
ns = Namespace "hydra.ext.scala.meta"

def :: String -> Type -> Binding
def = datatype ns

meta :: String -> Type
meta = typeref ns

module_ :: Module
module_ = Module ns elements [Core.ns] [Core.ns] $
    Just "A Scala syntax model based on Scalameta (https://scalameta.org)"
  where
    elements = [
      predefString,
      scalaSymbol,
      tree,
      ref,
      stat,
      name,
      lit,
      data_,
      data_Ref,
      data_This,
      data_Super,
      data_Name,
      data_Anonymous,
      data_Select,
      data_Interpolate,
      data_Xml,
      data_Apply,
      data_ApplyUsing,
      data_ApplyType,
      data_ApplyInfix,
      data_ApplyUnary,
      data_Assign,
      data_Return,
      data_Throw,
      data_Ascribe,
      data_Annotate,
      data_Tuple,
      data_Block,
      data_EndMarker,
      data_If,
      data_QuotedMacroExpr,
      data_QuotedMacroType,
      data_SplicedMacroExpr,
      data_Match,
      data_Try,
      data_TryWithHandler,
      data_FunctionData,
      data_ContextFunction,
      data_Function,
      data_PolyFunction,
      data_PartialFunction,
      data_While,
      data_Do,
      data_For,
      data_ForYield,
      data_New,
      data_NewAnonymous,
      data_Placeholder,
      data_Eta,
      data_Repeated,
      data_Param,
      type_,
      type_Ref,
      type_Name,
      type_AnonymousName,
      type_Select,
      type_Project,
      type_Singleton,
      type_Apply,
      type_ApplyInfix,
      type_FunctionType,
      type_Function,
      type_PolyFunction,
      type_ContextFunction,
      type_ImplicitFunction,
      type_Tuple,
      type_With,
      type_And,
      type_Or,
      type_Refine,
      type_Existential,
      type_Annotate,
      type_Lambda,
      type_Macro,
      type_Method,
      type_Placeholder,
      typeBounds,
      type_ByName,
      type_Repeated,
      type_Var,
      type_TypedParam,
      type_Param,
      type_Match,
      pat,
      pat_Var,
      pat_Bind,
      pat_Alternative,
      pat_Tuple,
      pat_Repeated,
      pat_Extract,
      pat_ExtractInfix,
      pat_Interpolate,
      pat_Xml,
      pat_Typed,
      pat_Macro,
      pat_Given,
      member,
      member_Data,
      member_Type,
      decl,
      decl_Val,
      decl_Var,
      decl_,
      decl_Type,
      decl_Given,
      defn,
      defn_Val,
      defn_Var,
      defn_Given,
      defn_Enum,
      defn_EnumCase,
      defn_RepeatedEnumCase,
      defn_GivenAlias,
      defn_ExtensionGroup,
      defn_,
      defn_Macro,
      defn_Type,
      defn_Class,
      defn_Trait,
      defn_Object,
      pkg,
      pkg_Object,
      ctor,
      ctor_Primary,
      ctor_Secondary,
      init_,
      self,
      template,
      mod_,
      mod_Annot,
      mod_Private,
      mod_Protected,
      enumerator,
      enumerator_Generator,
      enumerator_CaseGenerator,
      enumerator_Val,
      enumerator_Guard,
      importExportStat,
      import_,
      export_,
      importer,
      importee,
      importee_Given,
      importee_Name,
      importee_Rename,
      importee_Unimport,
      caseTree,
      case_,
      typeCase,
      source,
      quasi]

predefString :: Binding
predefString = def "PredefString" $ --  See scala/Predef.scala
  T.wrap T.string

scalaSymbol :: Binding
scalaSymbol = def "ScalaSymbol" $ --  See scala/Symbol.scala
  T.record [
    "name">: T.string]

tree :: Binding
tree = def "Tree" $ --  Note: ignoring fields of Tree and InternalTree for now
  T.union [
    "ref">: meta "Ref",
    "stat">: meta "Stat",
    "type">: meta "Type",
    "bounds">: meta "TypeBounds",
    "pat">: meta "Pat",
    "member">: meta "Member",
    "ctor">: meta "Ctor",
    "template">: meta "Template",
    "mod">: meta "Mod",
    "enumerator">: meta "Enumerator",
    "importer">: meta "Importer",
    "importee">: meta "Importee",
    "caseTree">: meta "CaseTree",
    "source">: meta "Source",
    "quasi">: meta "Quasi"]

ref :: Binding
ref = def "Ref" $
  T.union [
    "name">: meta "Name",
    "init">: meta "Init"]

stat :: Binding
stat = def "Stat" $
  T.union [
    "term">: meta "Data",
    "decl">: meta "Decl",
    "defn">: meta "Defn",
    "importExport">: meta "ImportExportStat"]

name :: Binding
name = def "Name" $
  T.union [
    "value">: T.string,
    "anonymous">: T.unit,
    "indeterminate">: meta "PredefString"]

lit :: Binding
lit = def "Lit" $
  T.union [
    "null">: T.unit,
    "int">: T.int32,
    "double">: T.float64,
    "float">: T.float32,
    "byte">: T.int8,
    "short">: T.int16,
    "char">: T.uint16,
    "long">: T.int64,
    "boolean">: T.boolean,
    "unit">: T.unit,
    "string">: T.string,
    "symbol">: meta "ScalaSymbol"]

data_ :: Binding
data_ = def "Data" $
  T.union [
    "lit">: meta "Lit",
    "ref">: meta "Data_Ref",
    "interpolate">: meta "Data_Interpolate",
    "xml">: meta "Data_Xml",
    "apply">: meta "Data_Apply",
    "applyUsing">: meta "Data_ApplyUsing",
    "applyType">: meta "Data_ApplyType",
    "assign">: meta "Data_Assign",
    "return">: meta "Data_Return",
    "throw">: meta "Data_Throw",
    "ascribe">: meta "Data_Ascribe",
    "annotate">: meta "Data_Annotate",
    "tuple">: meta "Data_Tuple",
    "block">: meta "Data_Block",
    "endMarker">: meta "Data_EndMarker",
    "if">: meta "Data_If",
    "quotedMacroExpr">: meta "Data_QuotedMacroExpr",
    "quotedMacroType">: meta "Data_QuotedMacroType",
    "splicedMacroExpr">: meta "Data_SplicedMacroExpr",
    "match">: meta "Data_Match",
    "try">: meta "Data_Try",
    "tryWithHandler">: meta "Data_TryWithHandler",
    "functionData">: meta "Data_FunctionData",
    "polyFunction">: meta "Data_PolyFunction",
    "partialFunction">: meta "Data_PartialFunction",
    "while">: meta "Data_While",
    "do">: meta "Data_Do",
    "for">: meta "Data_For",
    "forYield">: meta "Data_ForYield",
    "new">: meta "Data_New",
    "newAnonymous">: meta "Data_NewAnonymous",
    "placeholder">: meta "Data_Placeholder",
    "eta">: meta "Data_Eta",
    "repeated">: meta "Data_Repeated",
    "param">: meta "Data_Param"]

data_Ref :: Binding
data_Ref = def "Data_Ref" $
  T.union [
    "this">: meta "Data_This",
    "super">: meta "Data_Super",
    "name">: meta "Data_Name",
    "anonymous">: meta "Data_Anonymous",
    "select">: meta "Data_Select",
    "applyUnary">: meta "Data_ApplyUnary"]

data_This :: Binding
data_This = def "Data_This" $
  T.wrap T.unit

data_Super :: Binding
data_Super = def "Data_Super" $
  T.record [
    "thisp">: meta "Name",
    "superp">: meta "Name"]

data_Name :: Binding
data_Name = def "Data_Name" $
  T.record [
    "value">: meta "PredefString"]

data_Anonymous :: Binding
data_Anonymous = def "Data_Anonymous" $
  T.wrap T.unit

data_Select :: Binding
data_Select = def "Data_Select" $
  T.record [
    "qual">: meta "Data",
    "name">: meta "Data_Name"]

data_Interpolate :: Binding
data_Interpolate = def "Data_Interpolate" $
  T.record [
    "prefix">: meta "Data_Name",
    "parts">: T.list $ meta "Lit",
    "args">: T.list $ meta "Data"]

data_Xml :: Binding
data_Xml = def "Data_Xml" $
  T.record [
    "parts">: T.list $ meta "Lit",
    "args">: T.list $ meta "Data"]

data_Apply :: Binding
data_Apply = def "Data_Apply" $
  T.record [
    "fun">: meta "Data",
    "args">: T.list $ meta "Data"]

data_ApplyUsing :: Binding
data_ApplyUsing = def "Data_ApplyUsing" $
  T.record [
    "fun">: meta "Data",
    "targs">: T.list $ meta "Data"]

data_ApplyType :: Binding
data_ApplyType = def "Data_ApplyType" $
  T.record [
    "lhs">: meta "Data",
    "op">: meta "Data_Name",
    "targs">: T.list $ meta "Type",
    "args">: T.list $ meta "Data"]

data_ApplyInfix :: Binding
data_ApplyInfix = def "Data_ApplyInfix" $
  T.record [
    "lhs">: meta "Data",
    "op">: meta "Data_Name",
    "targs">: T.list $ meta "Type",
    "args">: T.list $ meta "Data"]

data_ApplyUnary :: Binding
data_ApplyUnary = def "Data_ApplyUnary" $
  T.record [
    "op">: meta "Data_Name",
    "arg">: meta "Data"]

data_Assign :: Binding
data_Assign = def "Data_Assign" $
  T.record [
    "lhs">: meta "Data",
    "rhs">: meta "Data"]

data_Return :: Binding
data_Return = def "Data_Return" $
  T.record [
    "expr">: meta "Data"]

data_Throw :: Binding
data_Throw = def "Data_Throw" $
  T.record [
    "expr">: meta "Data"]

data_Ascribe :: Binding
data_Ascribe = def "Data_Ascribe" $
  T.record [
    "expr">: meta "Data",
    "tpe">: meta "Type"]

data_Annotate :: Binding
data_Annotate = def "Data_Annotate" $
  T.record [
    "expr">: meta "Data",
    "annots">: T.list $ meta "Mod_Annot"]

data_Tuple :: Binding
data_Tuple = def "Data_Tuple" $
  T.record [
    "args">: T.list $ meta "Data"]

data_Block :: Binding
data_Block = def "Data_Block" $
  T.record [
    "stats">: T.list $ meta "Stat"]

data_EndMarker :: Binding
data_EndMarker = def "Data_EndMarker" $
  T.record [
    "name">: meta "Data_Name"]

data_If :: Binding
data_If = def "Data_If" $
  T.record [
    "cond">: meta "Data",
    "thenp">: meta "Data",
    "elsep">: meta "Data"]

data_QuotedMacroExpr :: Binding
data_QuotedMacroExpr = def "Data_QuotedMacroExpr" $
  T.record [
    "body">: meta "Data"]

data_QuotedMacroType :: Binding
data_QuotedMacroType = def "Data_QuotedMacroType" $
  T.record [
    "tpe">: meta "Type"]

data_SplicedMacroExpr :: Binding
data_SplicedMacroExpr = def "Data_SplicedMacroExpr" $
  T.record [
    "body">: meta "Data"]

data_Match :: Binding
data_Match = def "Data_Match" $
  T.record [
    "expr">: meta "Data",
    "cases">: T.list $ meta "Case"]

data_Try :: Binding
data_Try = def "Data_Try" $
  T.record [
    "expr">: meta "Data",
    "catchp">: T.list $ meta "Case",
    "finallyp">: T.maybe $ meta "Data"]

data_TryWithHandler :: Binding
data_TryWithHandler = def "Data_TryWithHandler" $
  T.record [
    "expr">: meta "Data",
    "catchp">: meta "Data",
    "finallyp">: T.maybe $ meta "Data"]

data_FunctionData :: Binding
data_FunctionData = def "Data_FunctionData" $
  T.union [
    "contextFunction">: meta "Data_ContextFunction",
    "function">: meta "Data_Function"]

data_ContextFunction :: Binding
data_ContextFunction = def "Data_ContextFunction" $
  T.record [
    "params">: T.list $ meta "Data_Param",
    "body">: meta "Data"]

data_Function :: Binding
data_Function = def "Data_Function" $
  T.record [
    "params">: T.list $ meta "Data_Param",
    "body">: meta "Data"]

data_PolyFunction :: Binding
data_PolyFunction = def "Data_PolyFunction" $
  T.record [
    "tparams">: T.list $ meta "Type_Param",
    "body">: meta "Data"]

data_PartialFunction :: Binding
data_PartialFunction = def "Data_PartialFunction" $
  T.record [
    "cases">: T.list $ meta "Case"]

data_While :: Binding
data_While = def "Data_While" $
  T.record [
    "expr">: meta "Data",
    "body">: meta "Data"]

data_Do :: Binding
data_Do = def "Data_Do" $
  T.record [
    "body">: meta "Data",
    "expr">: meta "Data"]

data_For :: Binding
data_For = def "Data_For" $
  T.record [
    "enums">: T.list $ meta "Enumerator"]

data_ForYield :: Binding
data_ForYield = def "Data_ForYield" $
  T.record [
    "enums">: T.list $ meta "Enumerator"]

data_New :: Binding
data_New = def "Data_New" $
  T.record [
    "init">: meta "Init"]

data_NewAnonymous :: Binding
data_NewAnonymous = def "Data_NewAnonymous" $
  T.record [
    "templ">: meta "Template"]

data_Placeholder :: Binding
data_Placeholder = def "Data_Placeholder"
  T.unit

data_Eta :: Binding
data_Eta = def "Data_Eta" $
  T.record [
    "expr">: meta "Data"]

data_Repeated :: Binding
data_Repeated = def "Data_Repeated" $
  T.record [
    "expr">: meta "Data"]

data_Param :: Binding
data_Param = def "Data_Param" $
  T.record [
    "mods">: T.list $ meta "Mod",
    "name">: meta "Name",
    "decltpe">: T.maybe $ meta "Type",
    "default">: T.maybe $ meta "Data"]

type_ :: Binding
type_ = def "Type" $
  T.union [
    "ref">: meta "Type_Ref",
    "anonymousName">: meta "Type_AnonymousName",
    "apply">: meta "Type_Apply",
    "applyInfix">: meta "Type_ApplyInfix",
    "functionType">: meta "Type_FunctionType",
    "polyFunction">: meta "Type_PolyFunction",
    "implicitFunction">: meta "Type_ImplicitFunction",
    "tuple">: meta "Type_Tuple",
    "with">: meta "Type_With",
    "and">: meta "Type_And",
    "or">: meta "Type_Or",
    "refine">: meta "Type_Refine",
    "existential">: meta "Type_Existential",
    "annotate">: meta "Type_Annotate",
    "lambda">: meta "Type_Lambda",
    "macro">: meta "Type_Macro",
    "method">: meta "Type_Method",
    "placeholder">: meta "Type_Placeholder",
    "byName">: meta "Type_ByName",
    "repeated">: meta "Type_Repeated",
    "var">: meta "Type_Var",
    "typedParam">: meta "Type_TypedParam",
    "match">: meta "Type_Match"]

type_Ref :: Binding
type_Ref = def "Type_Ref" $
  T.union [
    "name">: meta "Type_Name",
    "select">: meta "Type_Select",
    "project">: meta "Type_Project",
    "singleton">: meta "Type_Singleton"]

type_Name :: Binding
type_Name = def "Type_Name" $
  T.record [
    "value">: T.string]

type_AnonymousName :: Binding
type_AnonymousName = def "Type_AnonymousName" $
  T.wrap T.unit

type_Select :: Binding
type_Select = def "Type_Select" $
  T.record [
    "qual">: meta "Data_Ref",
    "name">: meta "Type_Name"]

type_Project :: Binding
type_Project = def "Type_Project" $
  T.record [
    "qual">: meta "Type",
    "name">: meta "Type_Name"]

type_Singleton :: Binding
type_Singleton = def "Type_Singleton" $
  T.record [
    "ref">: meta "Data_Ref"]

type_Apply :: Binding
type_Apply = def "Type_Apply" $
  T.record [
    "tpe">: meta "Type",
    "args">: T.list $ meta "Type"]

type_ApplyInfix :: Binding
type_ApplyInfix = def "Type_ApplyInfix" $
  T.record [
    "lhs">: meta "Type",
    "op">: meta "Type_Name",
    "rhs">: meta "Type"]

type_FunctionType :: Binding
type_FunctionType = def "Type_FunctionType" $
  T.union [
    "function">: meta "Type_Function",
    "contextFunction">: meta "Type_ContextFunction"]

type_Function :: Binding
type_Function = def "Type_Function" $
  T.record [
    "params">: T.list $ meta "Type",
    "res">: meta "Type"]

type_PolyFunction :: Binding
type_PolyFunction = def "Type_PolyFunction" $
  T.record [
    "tparams">: T.list $ meta "Type_Param",
    "tpe">: meta "Type"]

type_ContextFunction :: Binding
type_ContextFunction = def "Type_ContextFunction" $
  T.record [
    "params">: T.list $ meta "Type",
    "res">: meta "Type"]

type_ImplicitFunction :: Binding
type_ImplicitFunction = def "Type_ImplicitFunction" $
  T.record [
    "params">: T.list $ meta "Type",
    "res">: meta "Type"]

type_Tuple :: Binding
type_Tuple = def "Type_Tuple" $
  T.record [
    "args">: T.list $ meta "Type"]

type_With :: Binding
type_With = def "Type_With" $
  T.record [
    "lhs">: meta "Type",
    "rhs">: meta "Type"]

type_And :: Binding
type_And = def "Type_And" $
  T.record [
    "lhs">: meta "Type",
    "rhs">: meta "Type"]

type_Or :: Binding
type_Or = def "Type_Or" $
  T.record [
    "lhs">: meta "Type",
    "rhs">: meta "Type"]

type_Refine :: Binding
type_Refine = def "Type_Refine" $
  T.record [
    "tpe">: T.maybe $ meta "Type",
    "stats">: T.list $ meta "Stat"]

type_Existential :: Binding
type_Existential = def "Type_Existential" $
  T.record [
    "tpe">: meta "Type",
    "stats">: T.list $ meta "Stat"]

type_Annotate :: Binding
type_Annotate = def "Type_Annotate" $
  T.record [
    "tpe">: meta "Type",
    "annots">: T.list $ meta "Mod_Annot"]

type_Lambda :: Binding
type_Lambda = def "Type_Lambda" $
  T.record [
    "tparams">: T.list $ meta "Type_Param",
    "tpe">: meta "Type"]

type_Macro :: Binding
type_Macro = def "Type_Macro" $
  T.record [
    "body">: meta "Data"]

type_Method :: Binding
type_Method = def "Type_Method" $
  T.record [
    "paramss">: T.list $ T.list $ meta "Data_Param",
    "tpe">: meta "Type"]

type_Placeholder :: Binding
type_Placeholder = def "Type_Placeholder" $
  T.record [
    "bounds">: meta "TypeBounds"]

typeBounds :: Binding
typeBounds = def "TypeBounds" $
  T.record [
    "lo">: T.maybe $ meta "Type",
    "hi">: T.maybe $ meta "Type"]

type_ByName :: Binding
type_ByName = def "Type_ByName" $
  T.record [
    "tpe">: meta "Type"]

type_Repeated :: Binding
type_Repeated = def "Type_Repeated" $
  T.record [
    "tpe">: meta "Type"]

type_Var :: Binding
type_Var = def "Type_Var" $
  T.record [
    "name">: meta "Type_Name"]

type_TypedParam :: Binding
type_TypedParam = def "Type_TypedParam" $
  T.record [
    "name">: meta "Name",
    "typ">: meta "Type"]

type_Param :: Binding
type_Param = def "Type_Param" $
  T.record [
    "mods">: T.list $ meta "Mod",
    "name">: meta "Name",
    "tparams">: T.list $ meta "Type_Param",
    "tbounds">: T.list $ meta "TypeBounds",
    "vbounds">: T.list $ meta "Type",
    "cbounds">: T.list $ meta "Type"]

type_Match :: Binding
type_Match = def "Type_Match" $
  T.record [
    "tpe">: meta "Type",
    "cases">: T.list $ meta "TypeCase"]

pat :: Binding
pat = def "Pat" $
  T.union [
    "var">: meta "Pat_Var",
    "wildcard">: T.unit,
    "seqWildcard">: T.unit,
    "bind">: meta "Pat_Bind",
    "alternative">: meta "Pat_Alternative",
    "tuple">: meta "Pat_Tuple",
    "repeated">: meta "Pat_Repeated",
    "extract">: meta "Pat_Extract",
    "extractInfix">: meta "Pat_ExtractInfix",
    "interpolate">: meta "Pat_Interpolate",
    "xml">: meta "Pat_Xml",
    "typed">: meta "Pat_Typed",
    "macro">: meta "Pat_Macro",
    "given">: meta "Pat_Given"]

pat_Var :: Binding
pat_Var = def "Pat_Var" $
  T.record [
    "name">: meta "Data_Name"]

pat_Bind :: Binding
pat_Bind = def "Pat_Bind" $
  T.record [
    "lhs">: meta "Pat",
    "rhs">: meta "Pat"]

pat_Alternative :: Binding
pat_Alternative = def "Pat_Alternative" $
  T.record [
    "lhs">: meta "Pat",
    "rhs">: meta "Pat"]

pat_Tuple :: Binding
pat_Tuple = def "Pat_Tuple" $
  T.record [
    "args">: T.list $ meta "Pat"]

pat_Repeated :: Binding
pat_Repeated = def "Pat_Repeated" $
  T.record [
    "name">: meta "Data_Name"]

pat_Extract :: Binding
pat_Extract = def "Pat_Extract" $
  T.record [
    "fun">: meta "Data",
    "args">: T.list $ meta "Pat"]

pat_ExtractInfix :: Binding
pat_ExtractInfix = def "Pat_ExtractInfix" $
  T.record [
    "lhs">: meta "Pat",
    "op">: meta "Data_Name",
    "rhs">: T.list $ meta "Pat"]

pat_Interpolate :: Binding
pat_Interpolate = def "Pat_Interpolate" $
  T.record [
    "prefix">: meta "Data_Name",
    "parts">: T.list $ meta "Lit"]

pat_Xml :: Binding
pat_Xml = def "Pat_Xml" $
  T.record [
    "parts">: T.list $ meta "Lit",
    "args">: T.list $ meta "Pat"]

pat_Typed :: Binding
pat_Typed = def "Pat_Typed" $
  T.record [
    "lhs">: meta "Pat",
    "rhs">: meta "Type"]

pat_Macro :: Binding
pat_Macro = def "Pat_Macro" $
  T.record [
    "body">: meta "Data"]

pat_Given :: Binding
pat_Given = def "Pat_Given" $
  T.record [
    "tpe">: meta "Type"]

member :: Binding
member = def "Member" $
  T.union [
    "term">: meta "Member_Data",
    "type">: meta "Member_Type",
    "termParam">: meta "Data_Param",
    "typeParam">: meta "Type_Param",
    "self">: meta "Self"]

member_Data :: Binding
member_Data = def "Member_Data" $
  T.union [
    "pkg">: meta "Pkg",
    "object">: meta "Pkg_Object"]

member_Type :: Binding
member_Type = def "Member_Type" $
  T.record [
    "name">: meta "Type_Name"]

decl :: Binding
decl = def "Decl" $
  T.union [
    "val">: meta "Decl_Val",
    "var">: meta "Decl_Var",
    "def">: meta "Decl_Def",
    "type">: meta "Decl_Type",
    "given">: meta "Decl_Given"]

decl_Val :: Binding
decl_Val = def "Decl_Val" $
  T.record [
    "mods">: T.list $ meta "Mod",
    "pats">: T.list $ meta "Pat",
    "decltpe">: meta "Type"]

decl_Var :: Binding
decl_Var = def "Decl_Var" $
  T.record [
    "mods">: T.list $ meta "Mod",
    "pats">: T.list $ meta "Pat",
    "decltpe">: meta "Type"]

decl_ :: Binding
decl_ = def "Decl_Def" $
  T.record [
    "mods">: T.list $ meta "Mod",
    "name">: meta "Data_Name",
    "tparams">: T.list $ meta "Type_Param",
    "paramss">: T.list $ T.list $ meta "Data_Param",
    "decltpe">: meta "Type"]

decl_Type :: Binding
decl_Type = def "Decl_Type" $
  T.record [
    "mods">: T.list $ meta "Mod",
    "name">: meta "Type_Name",
    "tparams">: T.list $ meta "Type_Param",
    "bounds">: meta "TypeBounds"]

decl_Given :: Binding
decl_Given = def "Decl_Given" $
  T.record [
    "mods">: T.list $ meta "Mod",
    "name">: meta "Data_Name",
    "tparams">: T.list $ meta "Type_Param",
    "sparams">: T.list $ T.list $ meta "Data_Param",
    "decltpe">: meta "Type"]

defn :: Binding
defn = def "Defn" $
  T.union [
    "val">: meta "Defn_Val",
    "var">: meta "Defn_Var",
    "given">: meta "Defn_Given",
    "enum">: meta "Defn_Enum",
    "enumCase">: meta "Defn_EnumCase",
    "repeatedEnumCase">: meta "Defn_RepeatedEnumCase",
    "givenAlias">: meta "Defn_GivenAlias",
    "extensionGroup">: meta "Defn_ExtensionGroup",
    "def">: meta "Defn_Def",
    "macro">: meta "Defn_Macro",
    "type">: meta "Defn_Type",
    "class">: meta "Defn_Class",
    "trait">: meta "Defn_Trait",
    "object">: meta "Defn_Object"]

defn_Val :: Binding
defn_Val = def "Defn_Val" $
  T.record [
    "mods">: T.list $ meta "Mod",
    "pats">: T.list $ meta "Pat",
    "decltpe">: T.maybe $ meta "Type",
    "rhs">: meta "Data"]

defn_Var :: Binding
defn_Var = def "Defn_Var" $
  T.record [
    "mods">: T.list $ meta "Mod",
    "pats">: T.list $ meta "Pat",
    "decltpe">: meta "Type",
    "rhs">: T.maybe $ meta "Data"]

defn_Given :: Binding
defn_Given = def "Defn_Given" $
  T.record [
    "mods">: T.list $ meta "Mod",
    "name">: meta "Name",
    "tparams">: T.list $ T.list $ meta "Type_Param",
    "sparams">: T.list $ T.list $ meta "Data_Param",
    "templ">: meta "Template"]

defn_Enum :: Binding
defn_Enum = def "Defn_Enum" $
  T.record [
    "mods">: T.list $ meta "Mod",
    "name">: meta "Type_Name",
    "tparams">: T.list $ meta "Type_Param",
    "ctor">: meta "Ctor_Primary",
    "template">: meta "Template"]

defn_EnumCase :: Binding
defn_EnumCase = def "Defn_EnumCase" $
  T.record [
    "mods">: T.list $ meta "Mod",
    "name">: meta "Data_Name",
    "tparams">: T.list $ meta "Type_Param",
    "ctor">: meta "Ctor_Primary",
    "inits">: T.list $ meta "Init"]

defn_RepeatedEnumCase :: Binding
defn_RepeatedEnumCase = def "Defn_RepeatedEnumCase" $
  T.record [
    "mods">: T.list $ meta "Mod",
    "cases">: T.list $ meta "Data_Name"]

defn_GivenAlias :: Binding
defn_GivenAlias = def "Defn_GivenAlias" $
  T.record [
    "mods">: T.list $ meta "Mod",
    "name">: meta "Name",
    "tparams">: T.list $ T.list $ meta "Type_Param",
    "sparams">: T.list $ T.list $ meta "Data_Param",
    "decltpe">: meta "Type",
    "body">: meta "Data"]

defn_ExtensionGroup :: Binding
defn_ExtensionGroup = def "Defn_ExtensionGroup" $
  T.record [
    "tparams">: T.list $ meta "Type_Param",
    "parmss">: T.list $ T.list $ meta "Data_Param",
    "body">: meta "Stat"]

defn_ :: Binding
defn_ = def "Defn_Def" $
  T.record [
    "mods">: T.list $ meta "Mod",
    "name">: meta "Data_Name",
    "tparams">: T.list $ meta "Type_Param",
    "paramss">: T.list $ T.list $ meta "Data_Param",
    "decltpe">: T.maybe $ meta "Type",
    "body">: meta "Data"]

defn_Macro :: Binding
defn_Macro = def "Defn_Macro" $
  T.record [
    "mods">: T.list $ meta "Mod",
    "name">: meta "Data_Name",
    "tparams">: T.list $ meta "Type_Param",
    "paramss">: T.list $ T.list $ meta "Data_Param",
    "decltpe">: T.maybe $ meta "Type",
    "body">: meta "Data"]

defn_Type :: Binding
defn_Type = def "Defn_Type" $
  T.record [
    "mods">: T.list $ meta "Mod",
    "name">: meta "Type_Name",
    "tparams">: T.list $ meta "Type_Param",
    "body">: meta "Type"]

defn_Class :: Binding
defn_Class = def "Defn_Class" $
  T.record [
    "mods">: T.list $ meta "Mod",
    "name">: meta "Type_Name",
    "tparams">: T.list $ meta "Type_Param",
    "ctor">: meta "Ctor_Primary",
    "template">: meta "Template"]

defn_Trait :: Binding
defn_Trait = def "Defn_Trait" $
  T.record [
    "mods">: T.list $ meta "Mod",
    "name">: meta "Type_Name",
    "tparams">: T.list $ meta "Type_Param",
    "ctor">: meta "Ctor_Primary",
    "template">: meta "Template"]

defn_Object :: Binding
defn_Object = def "Defn_Object" $
  T.record [
    "name">: meta "Data_Name"]

pkg :: Binding
pkg = def "Pkg" $
  T.record [
    "name">: meta "Data_Name",
    "ref">: meta "Data_Ref",
    "stats">: T.list $ meta "Stat"]

pkg_Object :: Binding
pkg_Object = def "Pkg_Object" $
  T.record [
    "mods">: T.list $ meta "Mod",
    "name">: meta "Data_Name",
    "template">: meta "Template"]

ctor :: Binding
ctor = def "Ctor" $
  T.union [
    "primary">: meta "Ctor_Primary",
    "secondary">: meta "Ctor_Secondary"]

ctor_Primary :: Binding
ctor_Primary = def "Ctor_Primary" $
  T.record [
    "mods">: T.list $ meta "Mod",
    "name">: meta "Name",
    "paramss">: T.list $ T.list $ meta "Data_Param"]

ctor_Secondary :: Binding
ctor_Secondary = def "Ctor_Secondary" $
  T.record [
    "mods">: T.list $ meta "Mod",
    "name">: meta "Name",
    "paramss">: T.list $ T.list $ meta "Data_Param",
    "init">: meta "Init",
    "stats">: T.list $ meta "Stat"]

init_ :: Binding
init_ = def "Init" $
  T.record [
    "tpe">: meta "Type",
    "name">: meta "Name",
    "argss">: T.list $ T.list $ meta "Data"]

self :: Binding
self = def "Self" $
  T.wrap T.unit

template :: Binding
template = def "Template" $
  T.record [
    "early">: T.list $ meta "Stat",
    "inits">: T.list $ meta "Init",
    "self">: meta "Self",
    "stats">: T.list $ meta "Stat"]

mod_ :: Binding
mod_ = def "Mod" $
  T.union [
    "annot">: meta "Mod_Annot",
    "private">: meta "Mod_Private",
    "protected">: meta "Mod_Protected",
    "implicit">: T.unit,
    "final">: T.unit,
    "sealed">: T.unit,
    "open">: T.unit,
    "super">: T.unit,
    "override">: T.unit,
    "case">: T.unit,
    "abstract">: T.unit,
    "covariant">: T.unit,
    "contravariant">: T.unit,
    "lazy">: T.unit,
    "valParam">: T.unit,
    "varParam">: T.unit,
    "infix">: T.unit,
    "inline">: T.unit,
    "using">: T.unit,
    "opaque">: T.unit,
    "transparent">: T.unit]

mod_Annot :: Binding
mod_Annot = def "Mod_Annot" $
  T.record [
    "init">: meta "Init"]

mod_Private :: Binding
mod_Private = def "Mod_Private" $
  T.record [
    "within">: meta "Ref"]

mod_Protected :: Binding
mod_Protected = def "Mod_Protected" $
  T.record [
    "within">: meta "Ref"]

enumerator :: Binding
enumerator = def "Enumerator" $
  T.union [
    "generator">: meta "Enumerator_Generator",
    "caseGenerator">: meta "Enumerator_CaseGenerator",
    "val">: meta "Enumerator_Val",
    "guard">: meta "Enumerator_Guard"]

enumerator_Generator :: Binding
enumerator_Generator = def "Enumerator_Generator" $
  T.record [
    "pat">: meta "Pat",
    "rhs">: meta "Data"]

enumerator_CaseGenerator :: Binding
enumerator_CaseGenerator = def "Enumerator_CaseGenerator" $
  T.record [
    "pat">: meta "Pat",
    "rhs">: meta "Data"]

enumerator_Val :: Binding
enumerator_Val = def "Enumerator_Val" $
  T.record [
    "pat">: meta "Pat",
    "rhs">: meta "Data"]

enumerator_Guard :: Binding
enumerator_Guard = def "Enumerator_Guard" $
  T.record [
    "cond">: meta "Data"]

importExportStat :: Binding
importExportStat = def "ImportExportStat" $
  T.union [
    "import">: meta "Import",
    "export">: meta "Export"]

import_ :: Binding
import_ = def "Import" $
  T.record [
    "importers">: T.list $ meta "Importer"]

export_ :: Binding
export_ = def "Export" $
  T.record [
    "importers">: T.list $ meta "Importer"]

importer :: Binding
importer = def "Importer" $
  T.record [
    "ref">: meta "Data_Ref",
    "importees">: T.list $ meta "Importee"]

importee :: Binding
importee = def "Importee" $
  T.union [
    "wildcard">: T.unit,
    "given">: meta "Importee_Given",
    "givenAll">: T.unit,
    "name">: meta "Importee_Name",
    "rename">: meta "Importee_Rename",
    "unimport">: meta "Importee_Unimport"]

importee_Given :: Binding
importee_Given = def "Importee_Given" $
  T.record [
    "tpe">: meta "Type"]

importee_Name :: Binding
importee_Name = def "Importee_Name" $
  T.record [
    "name">: meta "Name"]

importee_Rename :: Binding
importee_Rename = def "Importee_Rename" $
  T.record [
    "name">: meta "Name",
    "rename">: meta "Name"]

importee_Unimport :: Binding
importee_Unimport = def "Importee_Unimport" $
  T.record [
    "name">: meta "Name"]

caseTree :: Binding
caseTree = def "CaseTree" $
  T.union [
    "case">: meta "Case",
    "typeCase">: meta "TypeCase"]

case_ :: Binding
case_ = def "Case" $
  T.record [
    "pat">: meta "Pat",
    "cond">: T.maybe $ meta "Data",
    "body">: meta "Data"]

typeCase :: Binding
typeCase = def "TypeCase" $
  T.record [
    "pat">: meta "Type",
    "body">: meta "Type"]

source :: Binding
source = def "Source" $
  T.record [
    "stats">: T.list $ meta "Stat"]

quasi :: Binding
quasi = def "Quasi" $ --  TODO
  T.wrap T.unit
